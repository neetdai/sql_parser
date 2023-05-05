use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::common::{Column, Expr, Field, InnerJoin, Limit, Table, TableType};
use crate::postgresql::{Keyword, Lexer, Token};
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::iter::Peekable;
use core::mem::replace;
use core::ops::{Fn, FnOnce};

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
            Ok(Token::Keyword(Keyword::From)) => {}
            Ok(_) => {
                return Some(Err(ParserError::Syntax));
            }
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
            None => {}
        }

        Some(Ok(select))
    }

    fn consume_columns(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<Column<'a>>, ParserError> {
        let mut columns = Vec::<Column<'a>>::new();
        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(_))) | Some(Ok(Token::Mul)) => {
                    match Self::consume_single_column(lexer) {
                        Some(Ok(column)) => columns.push(column),
                        Some(Err(e)) => return Err(e),
                        None => {}
                    }
                }
                Some(Ok(Token::Keyword(Keyword::From))) | None => {
                    break;
                }
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(Err(_)) => {
                    let err = lexer.next().expect("expect consume columns error");
                    err?;
                }
                Some(_) => {
                    return Err(ParserError::Syntax);
                }
            }
        }

        Ok(columns)
    }

    fn consume_single_column(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Column<'a>, ParserError>> {
        let mut column_status = PrefixAliasStatus::None;
        let mut column_result = Option::<Column<'_>>::None;

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(name)))
                    if matches!(column_status, PrefixAliasStatus::Prefix) =>
                {
                    column_status = PrefixAliasStatus::None;

                    if let Some(column) = column_result.as_mut() {
                        let prefix = replace(&mut column.name, Token::Ident(name));
                        column.prefix = Some(prefix);
                    }
                    lexer.next();
                }
                Some(Ok(Token::Ident(name)))
                    if matches!(column_status, PrefixAliasStatus::None) =>
                {
                    column_result = Some(Column {
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

                    if let Some(column) = column_result.as_mut() {
                        let prefix = replace(&mut column.name, Token::Mul);
                        column.prefix = Some(prefix);
                    } else {
                        column_result = Some(Column {
                            prefix: None,
                            name: Token::Mul,
                            alias: None,
                        });
                    }

                    lexer.next();
                }
                Some(Ok(Token::Ident(alias)))
                    if matches!(column_status, PrefixAliasStatus::Alias) =>
                {
                    column_status = PrefixAliasStatus::None;
                    if let Some(column) = column_result.as_mut() {
                        column.alias = Some(Token::Ident(alias));
                    }
                    column_status = PrefixAliasStatus::None;
                    lexer.next();
                }
                Some(Ok(Token::Keyword(Keyword::As))) => {
                    column_status = PrefixAliasStatus::Alias;
                    lexer.next();
                }
                Some(_) if !matches!(column_status, PrefixAliasStatus::None) => {
                    return Some(Err(ParserError::Syntax))
                }
                Some(_) | None => break,
            }
        }

        column_result.map(Ok)
    }

    fn consume_single_field(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Field<'a>, ParserError>> {
        let mut field_status = PrefixAliasStatus::None;
        let mut field_result = Option::<Field<'_>>::None;

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(name)))
                    if matches!(field_status, PrefixAliasStatus::Prefix) =>
                {
                    field_status = PrefixAliasStatus::None;

                    if let Some(field) = field_result.as_mut() {
                        let prefix = replace(&mut field.name, Token::Ident(name));
                        field.prefix = Some(prefix);
                    }
                    lexer.next();
                }
                Some(Ok(Token::Ident(name))) if matches!(field_status, PrefixAliasStatus::None) => {
                    field_result = Some(Field {
                        prefix: None,
                        name: Token::Ident(name),
                    });

                    lexer.next();
                    if let Some(Ok(Token::Period)) = lexer.peek() {
                        field_status = PrefixAliasStatus::Prefix;
                        lexer.next();
                    } else {
                        field_status = PrefixAliasStatus::None;
                    }
                }
                Some(Ok(_)) if !matches!(field_status, PrefixAliasStatus::None) => {
                    return Some(Err(ParserError::Syntax))
                }
                Some(Err(e)) => return Some(Err(*e)),
                Some(Ok(_)) | None => break,
            }
        }

        field_result.map(Ok)
    }

    fn consume_tables(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<TableType<'a>>, ParserError> {
        let mut tables = Vec::new();

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(_))) => match Self::consume_single_table(lexer) {
                    Some(Ok(table)) => match lexer.peek() {
                        Some(Ok(Token::Keyword(Keyword::Inner)))
                        | Some(Ok(Token::Keyword(Keyword::Join))) => {
                            match Self::consume_inner_join(lexer, table) {
                                Some(Ok(inner_join)) => tables.push(inner_join),
                                Some(Err(e)) => return Err(e),
                                None => {}
                            }
                        }
                        Some(Ok(Token::Keyword(Keyword::Left))) => todo!(),
                        Some(Ok(Token::Keyword(Keyword::Right))) => todo!(),
                        _ => tables.push(TableType::Table(table)),
                    },
                    Some(Err(e)) => return Err(e),
                    None => {}
                },
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(Ok(Token::Keyword(Keyword::Where)))
                | Some(Ok(Token::Keyword(Keyword::Having)))
                | Some(Ok(Token::Keyword(Keyword::Limit)))
                | Some(Ok(Token::Keyword(Keyword::Group)))
                | None => break,
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

    fn consume_single_table(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Table<'a>, ParserError>> {
        let mut table_status = PrefixAliasStatus::None;
        let mut table_result = Option::<Table<'a>>::None;

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(name)))
                    if matches!(table_status, PrefixAliasStatus::Prefix) =>
                {
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
                Some(Ok(Token::Ident(alias)))
                    if matches!(table_status, PrefixAliasStatus::Alias) =>
                {
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

        table_result.map(Ok)
    }

    fn consume_inner_join(
        lexer: &mut Peekable<Lexer<'a>>,
        left_table: Table<'a>,
    ) -> Option<Result<TableType<'a>, ParserError>> {
        match lexer.next() {
            Some(Ok(Token::Keyword(Keyword::Inner))) => {
                if lexer
                    .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                    .is_none()
                {
                    return Some(Err(ParserError::Syntax));
                }
            }
            Some(Ok(Token::Keyword(Keyword::Join))) => {}
            Some(Ok(_)) => return Some(Err(ParserError::Syntax)),
            Some(Err(e)) => return Some(Err(e)),
            None => return None,
        }

        let right_table = {
            match Self::consume_single_table(lexer) {
                Some(Ok(table)) => table,
                Some(Err(e)) => return Some(Err(e)),
                None => return Some(Err(ParserError::Syntax)),
            }
        };

        match lexer.next() {
            Some(Ok(Token::Keyword(Keyword::On))) => {
                todo!()
            }
            Some(Ok(Token::Keyword(Keyword::Using))) => {
                let result = Self::consume_paren(
                    lexer,
                    |lexer| -> Option<Result<Vec<Field<'a>>, ParserError>> {
                        let mut collect = Vec::new();
                        loop {
                            match lexer.peek() {
                                Some(Ok(Token::Ident(_))) => {
                                    match Self::consume_single_field(lexer) {
                                        Some(Ok(field)) => collect.push(field),
                                        Some(Err(e)) => return Some(Err(e)),
                                        None => {}
                                    }
                                }
                                Some(Ok(Token::Comma)) => {
                                    lexer.next();
                                }
                                Some(Ok(Token::RParen)) => break,
                                Some(_) => {
                                    return lexer.next().map(|result| match result {
                                        Ok(_) => Err(ParserError::Syntax),
                                        Err(e) => Err(e),
                                    })
                                }
                                None => return Some(Err(ParserError::Syntax)),
                            }
                        }

                        Some(Ok(collect))
                    },
                );

                result.map(|result| {
                    result.map(|collect| {
                        TableType::InnerJoin(Box::new(InnerJoin::Using {
                            left: left_table,
                            right: right_table,
                            columns: collect,
                        }))
                    })
                })
            }
            Some(Ok(Token::Keyword(Keyword::Natural))) => {
                Some(Ok(TableType::InnerJoin(Box::new(InnerJoin::Natural {
                    left: left_table,
                    right: right_table,
                }))))
            }
            Some(Ok(_)) | None => return Some(Err(ParserError::Syntax)),
            Some(Err(e)) => return Some(Err(e)),
        }
    }

    fn consume_limit(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<Limit<'a>, ParserError>> {
        match lexer.next_if_eq(&Ok(Token::Keyword(Keyword::Limit)))? {
            Ok(Token::Keyword(Keyword::Limit)) => {}
            Ok(_) => return Some(Err(ParserError::Syntax)),
            Err(e) => return Some(Err(e)),
        }

        let mut limit = {
            match lexer.next() {
                Some(Ok(Token::Number(number))) => Limit {
                    from: None,
                    limit: Token::Number(number),
                },
                Some(Ok(Token::Integer(number))) => Limit {
                    from: None,
                    limit: Token::Integer(number),
                },
                Some(Ok(Token::BigInteger(number))) => Limit {
                    from: None,
                    limit: Token::BigInteger(number),
                },
                Some(Ok(Token::Float(number))) => Limit {
                    from: None,
                    limit: Token::Float(number),
                },
                Some(Ok(_)) | None => return Some(Err(ParserError::Syntax)),
                Some(Err(e)) => return Some(Err(e)),
            }
        };

        if let Some(Ok(Token::Comma)) = lexer.next() {
            match lexer.next() {
                Some(Ok(Token::Number(number))) => {
                    let prefix = replace(&mut limit.limit, Token::Number(number));
                    limit.from = Some(prefix);
                }
                Some(Ok(Token::Integer(number))) => {
                    let prefix = replace(&mut limit.limit, Token::Integer(number));
                    limit.from = Some(prefix);
                }
                Some(Ok(Token::BigInteger(number))) => {
                    let prefix = replace(&mut limit.limit, Token::BigInteger(number));
                    limit.from = Some(prefix);
                }
                Some(Ok(Token::Float(number))) => {
                    let prefix = replace(&mut limit.limit, Token::Float(number));
                    limit.from = Some(prefix);
                }
                Some(Ok(_)) | None => return Some(Err(ParserError::Syntax)),
                Some(Err(e)) => return Some(Err(e)),
            }
        }

        Some(Ok(limit))
    }

    // // 生成条件语句
    // fn consume_condition(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<Expr<'a>, ParserError>> {
    //     let result = Self::consume_paren(lexer, |lexer| {

    //     });

    // }

    fn consume_condition_value(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Expr<'a>, ParserError>> {
        match lexer.peek() {
            Some(Ok(Token::Ident(_))) => match Self::consume_single_field(lexer) {
                Some(Ok(left_field)) => {
                    lexer.next();
                    Some(Ok(Expr::Field(left_field)))
                }
                Some(Err(e)) => return Some(Err(e)),
                None => return Some(Err(ParserError::Syntax)),
            },
            Some(Ok(Token::Number(_))) => lexer.next().map(|result| result.map(Expr::Number)),
            Some(Ok(Token::Integer(_))) => lexer.next().map(|result| result.map(Expr::Integer)),
            Some(Ok(Token::BigInteger(_))) => {
                lexer.next().map(|result| result.map(Expr::BigInteger))
            }
            Some(Ok(Token::Float(_))) => lexer.next().map(|result| result.map(Expr::Float)),
            Some(Ok(Token::String(_))) => lexer.next().map(|result| result.map(Expr::String)),
            Some(Ok(Token::Unicode(_))) => lexer.next().map(|result| result.map(Expr::Unicode)),
            Some(Ok(Token::Params(_))) => lexer.next().map(|result| result.map(Expr::Params)),
            Some(Err(_)) => lexer.next().map(|result| {
                if let Err(e) = result {
                    Err(e)
                } else {
                    Err(ParserError::Syntax)
                }
            }),
            Some(Ok(_)) | None => return None,
        }
    }

    // 消耗括号
    // 如果一开始匹配不到左括号, 返回None
    // 如果最后匹配不到右括号, 返回Some(Err(ParserError::Syntax))
    // 中间调用func去获取结果, 处理成功会将结果返回
    fn consume_paren<F, T>(
        lexer: &mut Peekable<Lexer<'a>>,
        func: F,
    ) -> Option<Result<T, ParserError>>
    where
        F: Fn(&mut Peekable<Lexer<'a>>) -> Option<Result<T, ParserError>>,
    {
        lexer.next_if_eq(&Ok(Token::LParen))?;

        let result = func(lexer);

        if lexer.next_if_eq(&Ok(Token::RParen)).is_none() {
            return Some(Err(ParserError::Syntax));
        }

        result
    }

    // fn consume_operator(
    //     lexer: &mut Peekable<Lexer<'a>>,
    //     left_expr: Expr<'a>,
    // ) -> Result<Expr<'a>, ParserError> {
    // }
}
