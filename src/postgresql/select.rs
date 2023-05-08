use crate::error::{Error as ParserError, ErrorType, SyntaxType};
use crate::postgresql::common::{Column, Expr, Field, Join, JoinType, Limit, Table, TableType};
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
    pub(crate) fn new(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParserError<'a>> {
        Self::expect_keyword(lexer, Keyword::Select)?;

        let columns = Self::parse_columns(lexer)?;

        Self::expect_keyword(lexer, Keyword::From)?;

        let tables = {
            match Self::parse_tables(lexer) {
                Ok(tables) => tables,
                Err(e) => return Err(e),
            }
        };

        let mut select = Select {
            columns,
            tables,
            limit: None,
        };

        match Self::parse_limit(lexer) {
            Some(Ok(limit)) => select.limit = Some(limit),
            Some(Err(e)) => return Err(e),
            None => {}
        }

        Ok(select)
    }

    // 期望指定关键字
    fn expect_keyword(
        lexer: &mut Peekable<Lexer<'a>>,
        expected_keyword: Keyword,
    ) -> Result<(), ParserError<'a>> {
        match lexer.next() {
            Some(Ok(Token::Keyword(keyword))) if expected_keyword == keyword => Ok(()),
            Some(Ok(token)) => Err(ParserError::Syntax {
                expected: SyntaxType::Keyword(expected_keyword),
                found: SyntaxType::Token(token),
            }),
            Some(Err(e)) => Err(e),
            None => Err(ParserError::Syntax {
                expected: SyntaxType::Keyword(expected_keyword),
                found: SyntaxType::Eof,
            }),
        }
    }

    fn parse_columns(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<Column<'a>>, ParserError<'a>> {
        let mut columns = Vec::<Column<'a>>::new();
        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(_))) | Some(Ok(Token::Mul)) => {
                    match Self::parse_single_column(lexer) {
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
                Some(_) => match lexer.next().unwrap() {
                    Ok(token) => {
                        return Err(ParserError::Syntax {
                            expected: SyntaxType::Column,
                            found: SyntaxType::Token(token),
                        })
                    }
                    Err(e) => return Err(e),
                },
            }
        }

        Ok(columns)
    }

    // 解析单一列, 包括前缀table和后缀alias
    fn parse_single_column(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Column<'a>, ParserError<'a>>> {
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
                    let result = lexer.next().unwrap();
                    match result {
                        Ok(token) => {
                            return Some(Err(ParserError::Syntax {
                                expected: SyntaxType::Column,
                                found: SyntaxType::Token(token),
                            }))
                        }
                        Err(e) => return Some(Err(e)),
                    }
                }
                Some(_) | None => break,
            }
        }

        column_result.map(Ok)
    }

    fn parse_single_field(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Field<'a>, ParserError<'a>>> {
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
                    let token = lexer.next().unwrap().unwrap();
                    return Some(Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Token(token) } ))
                }
                Some(Err(_)) => {
                    match lexer.next() {
                        Some(Ok(token)) => return Some(Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Token(token) } )),
                        Some(Err(e)) => return Some(Err(e)),
                        None => return Some(Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Eof } )),
                    }
                },
                Some(Ok(_)) | None => break,
            }
        }

        field_result.map(Ok)
    }

    fn parse_tables(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Result<Vec<TableType<'a>>, ParserError<'a>> {
        let mut tables = Vec::new();

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(_))) => match Self::parse_single_table(lexer) {
                    Some(Ok(table)) => match lexer.peek() {
                        Some(Ok(Token::Keyword(Keyword::Inner)))
                        | Some(Ok(Token::Keyword(Keyword::Left)))
                        | Some(Ok(Token::Keyword(Keyword::Right)))
                        | Some(Ok(Token::Keyword(Keyword::Join))) => {
                            match Self::parse_join(lexer, table) {
                                Ok(inner_join) => tables.push(inner_join),
                                Err(e) => return Err(e),
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
                Some(Ok(_)) | None => break,
                Some(Err(_)) => {
                    let err = lexer.next().expect("expect consume tables error");
                    err?;
                }
            }
        }

        Ok(tables)
    }

    fn parse_single_table(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Option<Result<Table<'a>, ParserError<'a>>> {
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
                        return Some(Err(ParserError::Invalid));
                    }
                }
                Some(_) | None => break,
            }
        }

        table_result.map(Ok)
    }

    // 解析Join类型
    fn parse_join_type(lexer: &mut Peekable<Lexer<'a>>) -> Result<JoinType, ParserError<'a>> {
        match lexer.next().ok_or(ParserError::Syntax {
            expected: SyntaxType::Join,
            found: SyntaxType::Eof,
        })? {
            Ok(Token::Keyword(Keyword::Inner)) => {
                if lexer
                    .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                    .is_none()
                {
                    Err(ParserError::Syntax {
                        expected: SyntaxType::Join,
                        found: SyntaxType::Eof,
                    })
                } else {
                    Ok(JoinType::InnerJoin)
                }
            }
            Ok(Token::Keyword(Keyword::Left)) => {
                match lexer.next().ok_or(ParserError::Syntax {
                    expected: SyntaxType::Join,
                    found: SyntaxType::Eof,
                })? {
                    Ok(Token::Keyword(Keyword::Inner)) => Ok(JoinType::LeftJoin),
                    Ok(Token::Keyword(Keyword::Outer))
                        if lexer
                            .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                            .is_some() =>
                    {
                        Ok(JoinType::LeftOuterJoin)
                    }
                    Ok(token) => Err(ParserError::Syntax {
                        expected: SyntaxType::Join,
                        found: SyntaxType::Token(token),
                    }),
                    Err(e) => Err(e),
                }
            }
            Ok(Token::Keyword(Keyword::Right)) => {
                match lexer.next().ok_or(ParserError::Syntax {
                    expected: SyntaxType::Join,
                    found: SyntaxType::Eof,
                })? {
                    Ok(Token::Keyword(Keyword::Inner)) => Ok(JoinType::RightJoin),
                    Ok(Token::Keyword(Keyword::Outer))
                        if lexer
                            .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                            .is_some() =>
                    {
                        Ok(JoinType::RightOuterJoin)
                    }
                    Ok(token) => Err(ParserError::Syntax {
                        expected: SyntaxType::Join,
                        found: SyntaxType::Token(token),
                    }),
                    Err(e) => Err(e),
                }
            }
            Ok(Token::Keyword(Keyword::Join)) => Ok(JoinType::InnerJoin),
            Ok(token) => Err(ParserError::Syntax {
                expected: SyntaxType::Join,
                found: SyntaxType::Token(token),
            }),
            Err(e) => Err(e),
        }
    }

    fn parse_join(
        lexer: &mut Peekable<Lexer<'a>>,
        left_table: Table<'a>,
    ) -> Result<TableType<'a>, ParserError<'a>> {
        let join_type = Self::parse_join_type(lexer)?;

        let right_table = {
            match Self::parse_single_table(lexer) {
                Some(Ok(table)) => table,
                Some(Err(e)) => return Err(e),
                None => return Err(ParserError::Invalid),
            }
        };

        match lexer.next() {
            Some(Ok(Token::Keyword(Keyword::On))) => {
                todo!()
            }
            Some(Ok(Token::Keyword(Keyword::Using))) => {
                let result = Self::parse_paren(
                    lexer,
                    |lexer| -> Option<Result<Vec<Field<'a>>, ParserError<'a>>> {
                        let mut collect = Vec::new();
                        loop {
                            let token = lexer.peek();
                            if token.is_none() {
                                return Some(Err(ParserError::Syntax {expected: SyntaxType::Column, found: SyntaxType::Eof }));
                            } else {
                                match token.unwrap() {
                                    Ok(Token::Ident(_)) => {
                                        match Self::parse_single_field(lexer) {
                                            Some(Ok(field)) => collect.push(field),
                                            Some(Err(e)) => return Some(Err(e)),
                                            None => {}
                                        }
                                    }
                                    Ok(Token::Comma) => {
                                        lexer.next();
                                    }
                                    Ok(Token::RParen) => break,
                                    Ok(token) => {
                                        return lexer.next().map(|result| match result {
                                            Ok(token) => Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Token(token) }),
                                            Err(e) => Err(e),
                                        })
                                    }
                                    Err(e) => {
                                        match lexer.next() {
                                            Some(Ok(token)) => return Some(Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Token(token) } )),
                                            Some(Err(e)) => return Some(Err(e)),
                                            None => return Some(Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Eof } )),
                                        }
                                    },
                                }
                            }
                        }

                        Some(Ok(collect))
                    },
                );

                match result {
                    Some(Ok(collect)) => {
                        Ok(TableType::Join {
                            join_type,
                            oper: Box::new(Join::Using {
                                left: left_table,
                                right: right_table,
                                columns: collect,
                            })
                        })
                    }
                    Some(Err(e)) => Err(e),
                    None => Err(ParserError::Invalid),
                }
            }
            Some(Ok(Token::Keyword(Keyword::Natural))) => Ok(TableType::Join {
                join_type,
                oper: Box::new(Join::Natural {
                    left: left_table,
                    right: right_table,
                }),
            }),
            Some(Ok(token)) => {
                return Err(ParserError::Syntax {
                    expected: SyntaxType::Join,
                    found: SyntaxType::Token(token),
                })
            }
            Some(Err(e)) => return Err(e),
            None => {
                return Err(ParserError::Syntax {
                    expected: SyntaxType::Join,
                    found: SyntaxType::Eof,
                })
            }
        }
    }

    fn parse_limit(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<Limit<'a>, ParserError<'a>>> {
        match lexer.next_if_eq(&Ok(Token::Keyword(Keyword::Limit)))? {
            Ok(Token::Keyword(Keyword::Limit)) => {}
            Ok(_) => return Some(Err(ParserError::Invalid)),
            Err(e) => return Some(Err(e)),
        }

        let mut limit = {
            let limit = match lexer.next() {
                Some(Ok(Token::Number(number))) => Token::Number(number),
                Some(Ok(Token::Integer(number))) => Token::Integer(number),
                Some(Ok(Token::BigInteger(number))) => Token::BigInteger(number),
                Some(Ok(Token::Float(number))) => Token::Float(number),
                Some(Ok(_)) | None => return Some(Err(ParserError::Invalid)),
                Some(Err(e)) => return Some(Err(e)),
            };

            Limit {
                from: None,
                limit,
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
                Some(Ok(_)) | None => return Some(Err(ParserError::Invalid)),
                Some(Err(e)) => return Some(Err(e)),
            }
        }

        Some(Ok(limit))
    }

    // // // 生成条件语句
    // // fn consume_condition(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<Expr<'a>, ParserError>> {
    // //     let result = Self::parse_paren(lexer, |lexer| {

    // //     });

    // // }

    // fn consume_condition_value(
    //     lexer: &mut Peekable<Lexer<'a>>,
    // ) -> Option<Result<Expr<'a>, ParserError>> {
    //     match lexer.peek() {
    //         Some(Ok(Token::Ident(_))) => match Self::parse_single_field(lexer) {
    //             Some(Ok(left_field)) => {
    //                 lexer.next();
    //                 Some(Ok(Expr::Field(left_field)))
    //             }
    //             Some(Err(e)) => return Some(Err(e)),
    //             None => return Some(Err(ParserError::Syntax)),
    //         },
    //         Some(Ok(Token::Number(_))) => lexer.next().map(|result| result.map(Expr::Number)),
    //         Some(Ok(Token::Integer(_))) => lexer.next().map(|result| result.map(Expr::Integer)),
    //         Some(Ok(Token::BigInteger(_))) => lexer.next().map(|result| result.map(Expr::BigInteger)),
    //         Some(Ok(Token::Float(_))) => lexer.next().map(|result| result.map(Expr::Float)),
    //         Some(Ok(Token::String(_))) => lexer.next().map(|result| result.map(Expr::String)),
    //         Some(Ok(Token::Unicode(_))) => lexer.next().map(|result| result.map(Expr::Unicode)),
    //         Some(Ok(Token::Params(_))) => lexer.next().map(|result| result.map(Expr::Params)),
    //         Some(Err(_)) => lexer.next().map(|result| {
    //             if let Err(e) = result {
    //                 Err(e)
    //             } else {
    //                 Err(ParserError::Syntax)
    //             }
    //         }),
    //         Some(Ok(_)) | None => return None,
    //     }
    // }

    // 消耗括号
    // 如果一开始匹配不到左括号, 返回None
    // 如果最后匹配不到右括号, 返回Some(Err(ParserError::Syntax))
    // 中间调用func去获取结果, 处理成功会将结果返回
    fn parse_paren<F, T>(
        lexer: &mut Peekable<Lexer<'a>>,
        func: F,
    ) -> Option<Result<T, ParserError<'a>>>
    where
        F: Fn(&mut Peekable<Lexer<'a>>) -> Option<Result<T, ParserError<'a>>>,
    {
        lexer.next_if_eq(&Ok(Token::LParen))?;

        let result = func(lexer);

        match lexer.next() {
            Some(Ok(Token::RParen)) => {},
            Some(Ok(token)) => return Some(Err(ParserError::Syntax { expected: SyntaxType::Token(Token::RParen), found: SyntaxType::Token(token) })),
            Some(Err(e)) => return Some(Err(e)),
            None => return Some(Err(ParserError::Syntax { expected: SyntaxType::Token(Token::RParen), found: SyntaxType::Eof })),
        }

        result
    }

    // // fn consume_operator(
    // //     lexer: &mut Peekable<Lexer<'a>>,
    // //     left_expr: Expr<'a>,
    // // ) -> Result<Expr<'a>, ParserError> {
    // // }
}
