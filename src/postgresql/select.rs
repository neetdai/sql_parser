use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::common::{Column, Table, TableType, Limit, InnerJoin};
use crate::postgresql::{Keyword, Lexer, Token};
use alloc::vec::Vec;
use core::iter::Peekable;
use core::mem::replace;

enum PrefixAliasStatus {
    None,
    Prefix,
    Alias,
}

#[derive(Debug, PartialEq)]
pub struct Select<'a> {
    pub columns: Vec<Column<'a>>,
    pub tables: Vec<TableType<'a>>,
    pub limit: Option<Limit<'a>>,
}

impl<'a> Select<'a> {
    pub(crate) fn new(mut lexer: Lexer<'a>) -> Option<Result<Self, ParserError>> {
        let mut lexer_peekable = lexer.peekable();
        let columns = {
            match Self::consume_columns(&mut lexer_peekable) {
                Ok(columns) => columns,
                Err(e) => return Some(Err(e)),
            }
        };

        match lexer_peekable.next()? {
            Ok(Token::Keyword(Keyword::From)) => {},
            Ok(_) => {
                return Some(Err(ParserError::Syntax));
            },
            Err(e) => return Some(Err(e)),
        }

        let tables = {
            match Self::consume_tables(&mut lexer_peekable) {
                Ok(tables) => tables,
                Err(e) => return Some(Err(e)),
            }
        };

        let mut select = Select {
            columns,
            tables,
            limit: None,
        };

        match Self::consume_limit(&mut lexer_peekable) {
            Some(Ok(limit)) => select.limit = Some(limit),
            Some(Err(e)) => return Some(Err(e)),
            None => {},
        }

        Some(Ok(select))
    }

    fn consume_columns(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<Column<'a>>, ParserError> {
        let mut columns = Vec::<Column<'a>>::new();
        let mut column_status = PrefixAliasStatus::None;
        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(name))) if matches!(column_status, PrefixAliasStatus::Prefix) => {
                    column_status = PrefixAliasStatus::None;

                    if let Some(column) = columns.last_mut() {
                        let prefix = replace(&mut column.name, Token::Ident(name));
                        column.prefix = Some(prefix);
                    }
                    lexer.next();
                }
                Some(Ok(Token::Ident(name))) if matches!(column_status, PrefixAliasStatus::None) => {
                    columns.push(Column {
                        prefix: None,
                        name: Token::Ident(name),
                        alias: None,
                    });

                    lexer.next();
                    if let Some(Ok(Token::Period)) = lexer.peek() {
                        column_status = PrefixAliasStatus::Prefix;
                        lexer.next();
                    } else {
                        column_status = PrefixAliasStatus::None;
                    }
                }
                Some(Ok(Token::Mul)) => {
                    column_status = PrefixAliasStatus::None;

                    if let Some(column) = columns.last_mut() {
                        let prefix = replace(&mut column.name, Token::Mul);
                        column.prefix = Some(prefix);
                    } else {
                        columns.push(Column {
                            prefix: None,
                            name: Token::Mul,
                            alias: None,
                        });
                    }

                    lexer.next();
                }
                Some(Ok(Token::Ident(alias))) if matches!(column_status, PrefixAliasStatus::Alias) => {
                    column_status = PrefixAliasStatus::None;
                    if let Some(column) = columns.last_mut() {
                        column.alias = Some(Token::Ident(alias));
                    }
                    column_status = PrefixAliasStatus::None;
                    lexer.next();
                }
                Some(Ok(Token::Keyword(Keyword::As))) => {
                    column_status = PrefixAliasStatus::Alias;
                    lexer.next();
                }
                Some(Ok(Token::Keyword(Keyword::From))) | None => {
                    break;
                }
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(_) => {
                    return Err(ParserError::Syntax);
                }
                Some(Err(_)) => {
                    let err = lexer.next().expect("expect consume columns error");
                    err?;
                }
            }
        }

        Ok(columns)
    }

    fn consume_tables(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<TableType<'a>>, ParserError> {
        let mut tables = Vec::new();

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(_))) => {
                    match Self::consume_single_table(lexer) {
                        Some(Ok(table)) => tables.push(table),
                        Some(Err(e)) => return Err(e),
                        None => {},
                    }
                }
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(Ok(Token::Keyword(Keyword::Where))) |
                Some(Ok(Token::Keyword(Keyword::Having))) |
                Some(Ok(Token::Keyword(Keyword::Limit))) |
                Some(Ok(Token::Keyword(Keyword::Group))) | None => break,
                Some(Err(_)) => {
                    let err = lexer.next().expect("expect consume tables error");
                    err?;
                }
                Some(_) => {
                    return Err(ParserError::Syntax);
                }
            }
        }

        Ok(tables)
    }

    fn consume_single_table(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<TableType<'a>, ParserError>> {
        let mut table_status = PrefixAliasStatus::None;
        let mut table_result = Option::<Table<'a>>::None;

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(name))) if matches!(table_status, PrefixAliasStatus::Prefix) => {
                    table_status = PrefixAliasStatus::None;
                    if let Some(table) = table_result.as_mut() {
                        let prefix = replace(&mut table.name, Token::Ident(name));
                        table.prefix = Some(prefix);
                    }
                    lexer.next();
                }
                Some(Ok(Token::Ident(name))) if matches!(table_status, PrefixAliasStatus::None) => {
                    table_result = Some(Table {
                        prefix: None,
                        name: Token::Ident(name),
                        alias: None,
                    });
                    lexer.next();
                    if let Some(Ok(Token::Period)) = lexer.peek() {
                        table_status = PrefixAliasStatus::Prefix;
                        lexer.next();
                    } else {
                        table_status = PrefixAliasStatus::None;
                    }
                }
                Some(Ok(Token::Ident(alias))) if matches!(table_status, PrefixAliasStatus::Alias) => {
                    table_status = PrefixAliasStatus::None;
                    if let Some(table) = table_result.as_mut() {
                        table.alias = Some(Token::Ident(alias));
                    }
                    lexer.next();
                }
                Some(Ok(Token::Keyword(Keyword::As))) => {
                    lexer.next();
                    table_status = PrefixAliasStatus::Alias;
                }
                Some(Err(_)) => {
                    let err = lexer.next().expect("expect consume tables error");
                    if let Err(e) = err {
                        return Some(Err(e));
                    } else {
                        return Some(Err(ParserError::Syntax));
                    }
                }
                Some(_) | None => break,
            }
        }

        table_result.map(|table| Ok(TableType::Table(table)))
    }

    fn consume_limit(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<Limit<'a>, ParserError>> {
        match lexer.next_if_eq(&Ok(Token::Keyword(Keyword::Limit)))? {
            Ok(Token::Keyword(Keyword::Limit)) => {},
            Ok(_) => return Some(Err(ParserError::Syntax)),
            Err(e) => return Some(Err(e)),
        }

        let mut limit = {
            match lexer.next() {
                Some(Ok(Token::Number(number))) => Limit { from: None, limit: Token::Number(number) },
                Some(Ok(Token::Integer(number))) => Limit { from: None, limit: Token::Integer(number) },
                Some(Ok(Token::BigInteger(number))) => Limit { from: None, limit: Token::BigInteger(number) },
                Some(Ok(Token::Float(number))) => Limit { from: None, limit: Token::Float(number) },
                Some(Ok(_)) | None => return Some(Err(ParserError::Syntax)),
                Some(Err(e)) => return Some(Err(e)),
            }
        };
        
        if let Some(Ok(Token::Comma)) = lexer.next() {
            match lexer.next() {
                Some(Ok(Token::Number(number))) => {
                    let prefix = replace(&mut limit.limit, Token::Number(number));
                    limit.from = Some(prefix);
                },
                Some(Ok(Token::Integer(number))) => {
                    let prefix = replace(&mut limit.limit, Token::Integer(number));
                    limit.from = Some(prefix);
                },
                Some(Ok(Token::BigInteger(number))) => {
                    let prefix = replace(&mut limit.limit, Token::BigInteger(number));
                    limit.from = Some(prefix);
                },
                Some(Ok(Token::Float(number))) => {
                    let prefix = replace(&mut limit.limit, Token::Float(number));
                    limit.from = Some(prefix);
                },
                Some(Ok(_)) | None => return Some(Err(ParserError::Syntax)),
                Some(Err(e)) => return Some(Err(e)),
            }
        }

        Some(Ok(limit))
    }
}
