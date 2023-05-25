use crate::error::{Error as ParserError, ErrorType, SyntaxType};
use crate::postgresql::common::{
    Column, Expr, Field, FourFundamentalOperation, Join, JoinType, Limit, LinkOperator, Table,
    TableType, BinaryOperation, ColumnType, Function,
};
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
    pub columns: Vec<ColumnType<'a>>,
    pub tables: Vec<TableType<'a>>,
    pub r#where: Option<Expr<'a>>,
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
            r#where: None,
            limit: None,
        };

        match Self::parse_where(lexer) {
            Some(Ok(r#where)) => select.r#where = Some(r#where),
            Some(Err(e)) => return Err(e),
            None => {}
        }

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

    fn expect_token(
        lexer: &mut Peekable<Lexer<'a>>,
        expected_token: Token<'a>,
    ) -> Result<(), ParserError<'a>> {
        match lexer.next() {
            Some(Ok(token)) if token == expected_token => Ok(()),
            Some(Ok(token)) => Err(ParserError::Syntax {
                expected: SyntaxType::Token(expected_token),
                found: SyntaxType::Token(token),
            }),
            Some(Err(e)) => Err(e),
            None => Err(ParserError::Syntax {
                expected: SyntaxType::Token(expected_token),
                found: SyntaxType::Eof,
            }),
        }
    }

    fn parse_columns(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<ColumnType<'a>>, ParserError<'a>> {
        let mut columns = Vec::<ColumnType<'a>>::new();
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
    ) -> Option<Result<ColumnType<'a>, ParserError<'a>>> {
        let mut column_status = PrefixAliasStatus::None;
        let mut column_result = Option::<ColumnType<'_>>::None;

        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(name)))
                    if matches!(column_status, PrefixAliasStatus::Prefix) =>
                {
                    column_status = PrefixAliasStatus::None;

                    if let Some(ColumnType::Column(column)) = column_result.as_mut() {
                        let prefix = replace(&mut column.name, Token::Ident(name));
                        column.prefix = Some(prefix);
                    }
                    lexer.next();
                }
                Some(Ok(Token::Ident(name)))
                    if matches!(column_status, PrefixAliasStatus::None) =>
                {
                    let name = lexer.next().expect("unreach").unwrap();

                    match lexer.peek() {
                        Some(Ok(Token::Period)) => {
                            column_status = PrefixAliasStatus::Prefix;
                            lexer.next();

                            column_result = Some(ColumnType::Column(Column {
                                prefix: None,
                                name,
                                alias: None,
                            }));
                        }
                        Some(Ok(Token::LParen)) => {
                            column_status = PrefixAliasStatus::None;
                            match Self::parse_function(lexer, name) {
                                Ok(func) => {
                                    column_result = Some(ColumnType::Function(Box::new(func)));
                                }
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        Some(_) => {
                            column_status = PrefixAliasStatus::None;
                            column_result = Some(ColumnType::Column(Column {
                                prefix: None,
                                name,
                                alias: None,
                            }));
                        }
                        None => return None,
                    }
                }
                Some(Ok(Token::Mul)) => {
                    column_status = PrefixAliasStatus::None;

                    if let Some(ColumnType::Column(column)) = column_result.as_mut() {
                        let prefix = replace(&mut column.name, Token::Mul);
                        column.prefix = Some(prefix);
                    } else {
                        column_result = Some(ColumnType::Column(Column {
                            prefix: None,
                            name: Token::Mul,
                            alias: None,
                        }));
                    }

                    lexer.next();
                }
                Some(Ok(Token::Ident(alias)))
                    if matches!(column_status, PrefixAliasStatus::Alias) =>
                {
                    column_status = PrefixAliasStatus::None;
                    match column_result.as_mut() {
                        Some(ColumnType::Column(column)) => column.alias = Some(Token::Ident(alias)),
                        Some(ColumnType::Function(func)) => func.alias = Some(Token::Ident(alias)),
                        None => {},
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
                    return Some(Err(ParserError::Syntax {
                        expected: SyntaxType::Column,
                        found: SyntaxType::Token(token),
                    }));
                }
                Some(Err(_)) => match lexer.next() {
                    Some(Ok(token)) => {
                        return Some(Err(ParserError::Syntax {
                            expected: SyntaxType::Column,
                            found: SyntaxType::Token(token),
                        }))
                    }
                    Some(Err(e)) => return Some(Err(e)),
                    None => {
                        return Some(Err(ParserError::Syntax {
                            expected: SyntaxType::Column,
                            found: SyntaxType::Eof,
                        }))
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
                        | Some(Ok(Token::Keyword(Keyword::Join)))
                        | Some(Ok(Token::Keyword(Keyword::Full))) => {
                            match Self::parse_join(lexer, table) {
                                Ok(inner_join) => tables.push(inner_join),
                                Err(e) => return Err(e),
                            }
                        }
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
                    expected: SyntaxType::LeftJoin,
                    found: SyntaxType::Eof,
                })? {
                    Ok(Token::Keyword(Keyword::Join)) => Ok(JoinType::LeftJoin),
                    Ok(Token::Keyword(Keyword::Outer))
                        if lexer
                            .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                            .is_some() =>
                    {
                        Ok(JoinType::LeftOuterJoin)
                    }
                    Ok(token) => Err(ParserError::Syntax {
                        expected: SyntaxType::LeftOuterJoin,
                        found: SyntaxType::Token(token),
                    }),
                    Err(e) => Err(e),
                }
            }
            Ok(Token::Keyword(Keyword::Right)) => {
                match lexer.next().ok_or(ParserError::Syntax {
                    expected: SyntaxType::RightJoin,
                    found: SyntaxType::Eof,
                })? {
                    Ok(Token::Keyword(Keyword::Join)) => Ok(JoinType::RightJoin),
                    Ok(Token::Keyword(Keyword::Outer))
                        if lexer
                            .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                            .is_some() =>
                    {
                        Ok(JoinType::RightOuterJoin)
                    }
                    Ok(token) => Err(ParserError::Syntax {
                        expected: SyntaxType::RightOutJoin,
                        found: SyntaxType::Token(token),
                    }),
                    Err(e) => Err(e),
                }
            }
            Ok(Token::Keyword(Keyword::Full)) => {
                match lexer.next().ok_or(ParserError::Syntax {
                    expected: SyntaxType::FullJoin,
                    found: SyntaxType::Eof,
                })? {
                    Ok(Token::Keyword(Keyword::Join)) => Ok(JoinType::FullJoin),
                    Ok(Token::Keyword(Keyword::Outer))
                        if lexer
                            .next_if_eq(&Ok(Token::Keyword(Keyword::Join)))
                            .is_some() =>
                    {
                        Ok(JoinType::FullOuterJoin)
                    }
                    Ok(token) => Err(ParserError::Syntax {
                        expected: SyntaxType::FullOuterJoin,
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
                let collect = Self::parse_fields_range(lexer)?;

                Ok(TableType::Join {
                    join_type,
                    oper: Box::new(Join::Using {
                        left: left_table,
                        right: right_table,
                        columns: collect,
                    }),
                })
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
            Some(Err(e)) => Err(e),
            None => Err(ParserError::Syntax {
                expected: SyntaxType::Join,
                found: SyntaxType::Eof,
            }),
        }
    }

    // 解析limit
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

            Limit { from: None, limit }
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

    // 收集括号中的字段
    fn parse_fields_range(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Result<Vec<Field<'a>>, ParserError<'a>> {
        Self::expect_token(lexer, Token::LParen)?;

        let mut fields = Vec::new();
        loop {
            match lexer.peek() {
                Some(Ok(Token::Ident(_))) => match Self::parse_single_field(lexer) {
                    Some(Ok(field)) => {
                        fields.push(field);
                    }
                    Some(Err(e)) => return Err(e),
                    None => {
                        return Err(ParserError::Syntax {
                            expected: SyntaxType::Column,
                            found: SyntaxType::Eof,
                        })
                    }
                },
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(Ok(Token::RParen)) => {
                    break;
                }
                Some(_) => match lexer.next() {
                    Some(Ok(token)) => {
                        return Err(ParserError::Syntax {
                            expected: SyntaxType::Column,
                            found: SyntaxType::Token(token),
                        })
                    }
                    Some(Err(e)) => return Err(e),
                    None => {
                        return Err(ParserError::Syntax {
                            expected: SyntaxType::Column,
                            found: SyntaxType::Eof,
                        })
                    }
                },
                None => {
                    return Err(ParserError::Syntax {
                        expected: SyntaxType::Column,
                        found: SyntaxType::Eof,
                    })
                }
            }
        }

        Self::expect_token(lexer, Token::RParen)?;

        Ok(fields)
    }

    fn parse_where(lexer: &mut Peekable<Lexer<'a>>) -> Option<Result<Expr<'a>, ParserError<'a>>> {
        lexer
            .next_if_eq(&Ok(Token::Keyword(Keyword::Where)))
            .map(|_| Self::parse_expr(lexer))
    }

    fn parse_expr_value(lexer: &mut Peekable<Lexer<'a>>) -> Result<Expr<'a>, ParserError<'a>> {
        match lexer.peek() {
            Some(Ok(Token::Ident(_))) => match Self::parse_single_field(lexer) {
                Some(Ok(left_field)) => Ok(Expr::Field(left_field)),
                Some(Err(e)) => return Err(e),
                None => {
                    return Err(ParserError::Syntax {
                        expected: SyntaxType::Expr,
                        found: SyntaxType::Eof,
                    });
                }
            },
            Some(Ok(Token::Number(_))) => lexer.next().unwrap().map(|token| {
                let Token::Number(content) = token else { unreachable!() };
                Expr::Number(content)
            }),
            Some(Ok(Token::Integer(_))) => lexer.next().unwrap().map(|token| {
                let Token::Integer(content) = token else { unreachable!() };
                Expr::Integer(content)
            }),
            Some(Ok(Token::BigInteger(_))) => lexer.next().unwrap().map(|token| {
                let Token::BigInteger(content) = token else { unreachable!() };
                Expr::BigInteger(content)
            }),
            Some(Ok(Token::Float(_))) => lexer.next().unwrap().map(|token| {
                let Token::Float(content) = token else { unreachable!() };
                Expr::Float(content)
            }),
            Some(Ok(Token::String(_))) => lexer.next().unwrap().map(|token| {
                let Token::String(content) = token else { unreachable!() };
                Expr::String(content)
            }),
            Some(Ok(Token::Unicode(_))) => lexer.next().unwrap().map(|token| {
                let Token::Unicode(content) = token else { unreachable!() };
                Expr::Unicode(content)
            }),
            Some(Ok(Token::Params(_))) => lexer.next().unwrap().map(|token| {
                let Token::Params(content) = token else { unreachable!() };
                Expr::Params(content)
            }),
            Some(_) => match lexer.next().unwrap() {
                Ok(token) => Err(ParserError::Syntax {
                    expected: SyntaxType::Expr,
                    found: SyntaxType::Token(token),
                }),
                Err(e) => Err(e),
            },
            None => Err(ParserError::Syntax {
                expected: SyntaxType::Expr,
                found: SyntaxType::Eof,
            }),
        }
    }

    fn parse_expr(lexer: &mut Peekable<Lexer<'a>>) -> Result<Expr<'a>, ParserError<'a>> {
        let mut paren_count = 0isize;

        if lexer.next_if_eq(&Ok(Token::LParen)).is_some() {
            paren_count += 1;
        }

        let expr = match lexer.peek() {
            Some(Ok(Token::Ident(_)))
            | Some(Ok(Token::Float(_)))
            | Some(Ok(Token::Integer(_)))
            | Some(Ok(Token::BigInteger(_)))
            | Some(Ok(Token::Number(_)))
            | Some(Ok(Token::String(_)))
            | Some(Ok(Token::Unicode(_))) => {
                let mut expr = Self::parse_expr_value(lexer)?;

                loop {
                    match lexer.peek() {
                        Some(Ok(Token::Equal))
                        | Some(Ok(Token::Keyword(Keyword::And)))
                        | Some(Ok(Token::Keyword(Keyword::Or)))
                        | Some(Ok(Token::Keyword(Keyword::Between)))
                        | Some(Ok(Token::Keyword(Keyword::In))) => {
                            expr = Self::parse_operation(lexer, expr)?;
                        }
                        Some(Ok(Token::Plus)) | Some(Ok(Token::Sub)) => {
                            expr = Self::parse_addtion_and_subtraction(lexer, expr)?;
                        }
                        Some(Ok(Token::Mul)) | Some(Ok(Token::Div)) | Some(Ok(Token::Mod)) => {
                            expr = Self::parse_multiplication_and_division(lexer, expr)?;
                        }
                        Some(Ok(Token::DoubleEqual))
                        | Some(Ok(Token::Less))
                        | Some(Ok(Token::LessOrEqual))
                        | Some(Ok(Token::Greater))
                        | Some(Ok(Token::GreaterOrEqual))
                        | Some(Ok(Token::Keyword(Keyword::Is)))
                        | Some(Ok(Token::Keyword(Keyword::Not)))
                        | Some(Ok(Token::Negative))
                        | Some(Ok(Token::NotEqual))
                        | Some(Ok(Token::LessOrGreater)) => {
                            expr = Self::parse_binary_operation(lexer, expr)?;
                        }
                        Some(Ok(Token::Caret)) => {
                            expr = Self::parse_exponent(lexer, expr)?;
                        }
                        Some(_) | None => break expr,
                    }
                }
            }
            Some(_) => {
                let result = lexer.next().unwrap();
                match result {
                    Ok(token) => {
                        return Err(ParserError::Syntax {
                            expected: SyntaxType::Expr,
                            found: SyntaxType::Token(token),
                        });
                    }
                    Err(e) => return Err(e),
                }
            }
            None => {
                return Err(ParserError::Syntax {
                    expected: SyntaxType::Column,
                    found: SyntaxType::Eof,
                });
            }
        };

        if lexer.next_if_eq(&Ok(Token::RParen)).is_some() {
            paren_count -= 1;
        }

        if paren_count != 0 {
            Err(ParserError::Invalid)
        } else {
            Ok(expr)
        }
    }

    // 解析加法和减法
    fn parse_addtion_and_subtraction(
        lexer: &mut Peekable<Lexer<'a>>,
        left_expr: Expr<'a>,
    ) -> Result<Expr<'a>, ParserError<'a>> {
        let left_expr = Box::new(left_expr);
        let fundament_operation = match lexer.next() {
            Some(Ok(Token::Plus)) => Ok(FourFundamentalOperation::Plus),
            Some(Ok(Token::Sub)) => Ok(FourFundamentalOperation::Sub),
            Some(result) => match result {
                Ok(token) => Err(ParserError::Syntax {
                    expected: SyntaxType::Token(Token::Plus),
                    found: SyntaxType::Token(token),
                }),
                Err(e) => Err(e),
            },
            None => Err(ParserError::Invalid),
        }?;

        // 因为有 括号 和 乘除 取模 的优先计算
        match lexer.peek() {
            Some(Ok(Token::LParen)) => Ok(Expr::FourFundamental {
                operation_type: fundament_operation,
                left_expr,
                right_expr: Box::new(Self::parse_expr(lexer)?),
            }),
            _ => {
                let right_expr = Self::parse_expr_value(lexer)?;

                match lexer.peek() {
                    Some(Ok(Token::Mul)) | Some(Ok(Token::Div)) | Some(Ok(Token::Mod)) => {
                        Ok(Expr::FourFundamental {
                            operation_type: fundament_operation,
                            left_expr,
                            right_expr: Box::new(Self::parse_multiplication_and_division(
                                lexer, right_expr,
                            )?),
                        })
                    }
                    // 指数
                    Some(Ok(Token::Caret)) => Ok(Expr::FourFundamental {
                        operation_type: fundament_operation,
                        left_expr,
                        right_expr: Box::new(Self::parse_exponent(lexer, right_expr)?),
                    }),
                    _ => Ok(Expr::FourFundamental {
                        operation_type: fundament_operation,
                        left_expr,
                        right_expr: Box::new(right_expr),
                    }),
                }
            }
        }
    }

    // 解析乘法和除法
    fn parse_multiplication_and_division(
        lexer: &mut Peekable<Lexer<'a>>,
        left_expr: Expr<'a>,
    ) -> Result<Expr<'a>, ParserError<'a>> {
        let left_expr = Box::new(left_expr);
        let fundament_operation = match lexer.next() {
            Some(Ok(Token::Mul)) => Ok(FourFundamentalOperation::Multiply),
            Some(Ok(Token::Div)) => Ok(FourFundamentalOperation::Divide),
            Some(Ok(Token::Mod)) => Ok(FourFundamentalOperation::Modulo),
            Some(result) => match result {
                Ok(token) => Err(ParserError::Syntax {
                    expected: SyntaxType::Token(Token::Mul),
                    found: SyntaxType::Token(token),
                }),
                Err(e) => Err(e),
            },
            None => Err(ParserError::Invalid),
        }?;

        match lexer.peek() {
            Some(Ok(Token::LParen)) => Ok(Expr::FourFundamental {
                operation_type: fundament_operation,
                left_expr,
                right_expr: Box::new(Self::parse_expr(lexer)?),
            }),
            _ => {
                let right_expr = Self::parse_expr_value(lexer)?;

                match lexer.peek() {
                    // 指数
                    Some(Ok(Token::Caret)) => Ok(Expr::FourFundamental {
                        operation_type: fundament_operation,
                        left_expr,
                        right_expr: Box::new(Self::parse_exponent(lexer, right_expr)?),
                    }),
                    _ => Ok(Expr::FourFundamental {
                        operation_type: fundament_operation,
                        left_expr,
                        right_expr: Box::new(right_expr),
                    })
                }
            },
        }
    }

    // 解析指数
    fn parse_exponent(lexer: &mut Peekable<Lexer<'a>>, left_expr: Expr<'a>) -> Result<Expr<'a>, ParserError<'a>> {
        Self::expect_token(lexer, Token::Caret)?;

        let left_expr = Box::new(left_expr);

        match lexer.peek() {
            Some(Ok(Token::LParen)) => Ok(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Exponent,
                left_expr,
                right_expr: Box::new(Self::parse_expr(lexer)?),
            }),
            _ => {
                let right_expr = Box::new(Self::parse_expr_value(lexer)?);

                Ok(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Exponent,
                    left_expr,
                    right_expr,
                })
            }
        }
    }

    // 解析二元操作
    fn parse_binary_operation(
        lexer: &mut Peekable<Lexer<'a>>,
        left_expr: Expr<'a>,
    ) -> Result<Expr<'a>, ParserError<'a>> {
        let binary_operator = match lexer.next() {
            Some(Ok(Token::DoubleEqual)) => BinaryOperation::DoubleEqual,
            Some(Ok(Token::LessOrEqual)) => BinaryOperation::LessOrEqual,
            Some(Ok(Token::Less)) => BinaryOperation::Less,
            Some(Ok(Token::Greater)) => BinaryOperation::Greater,
            Some(Ok(Token::GreaterOrEqual)) => BinaryOperation::GreaterOrEqual,
            Some(Ok(Token::Keyword(Keyword::Not))) => BinaryOperation::Not,
            Some(Ok(Token::Keyword(Keyword::Is))) => BinaryOperation::Is,
            Some(Ok(Token::Negative)) => BinaryOperation::Negative,
            Some(Ok(Token::NotEqual)) => BinaryOperation::NotEqual,
            Some(Ok(Token::LessOrGreater)) => BinaryOperation::LessOrGreater,
            Some(_) | None => return Err(ParserError::Invalid),
        };

        Ok(Expr::Binary {
            operator: binary_operator,
            left_expr: Box::new(left_expr),
            right_expr: Box::new(Self::parse_expr_value(lexer)?),
        })
    }

    // 解析操作
    fn parse_operation(
        lexer: &mut Peekable<Lexer<'a>>,
        left_expr: Expr<'a>,
    ) -> Result<Expr<'a>, ParserError<'a>> {
        let left_expr = Box::new(left_expr);

        match lexer.peek() {
            Some(Ok(Token::Equal)) => {
                lexer.next();
                let right_expr = Box::new(Self::parse_expr(lexer)?);
                Ok(Expr::Equal {
                    left_expr,
                    right_expr,
                })
            }
            Some(Ok(Token::Keyword(Keyword::And))) => {
                lexer.next();
                let right_expr = Box::new(Self::parse_expr(lexer)?);
                Ok(Expr::AndOr {
                    operator: LinkOperator::And,
                    left_expr,
                    right_expr,
                })
            }
            Some(Ok(Token::Keyword(Keyword::Or))) => {
                lexer.next();
                let right_expr = Box::new(Self::parse_expr(lexer)?);
                Ok(Expr::AndOr {
                    operator: LinkOperator::Or,
                    left_expr,
                    right_expr,
                })
            }
            Some(Ok(Token::Keyword(Keyword::In))) => {
                lexer.next();
                let expr_collection = Self::parse_expr_range(lexer)?;
                Ok(Expr::In {
                    target: left_expr,
                    expr_collection,
                })
            }
            Some(Ok(Token::Keyword(Keyword::Between))) => {
                let (start, end) = Self::parse_between(lexer)?;
                let start = Box::new(start);
                let end = Box::new(end);

                Ok(Expr::Between {
                    target: left_expr,
                    start,
                    end,
                })
            }
            _ => Self::parse_expr(lexer),
        }
    }

    fn parse_expr_range(lexer: &mut Peekable<Lexer<'a>>) -> Result<Vec<Expr<'a>>, ParserError<'a>> {
        Self::expect_token(lexer, Token::LParen)?;

        let mut collections = Vec::new();

        loop {
            match lexer.peek() {
                Some(Ok(Token::RParen)) | None => break,
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(Ok(_)) => collections.push(Self::parse_expr_value(lexer)?),
                Some(Err(_)) => {
                    let err = lexer.next().expect("unreachable");
                    err?;
                }
            }
        }

        Self::expect_token(lexer, Token::RParen)?;

        Ok(collections)
    }

    // 解析between的范围
    fn parse_between(
        lexer: &mut Peekable<Lexer<'a>>,
    ) -> Result<(Expr<'a>, Expr<'a>), ParserError<'a>> {
        Self::expect_keyword(lexer, Keyword::Between)?;

        let left_expr = Self::parse_expr_value(lexer)?;

        Self::expect_keyword(lexer, Keyword::And)?;

        let right_expr = Self::parse_expr_value(lexer)?;

        Ok((left_expr, right_expr))
    }

    // 解析函数
    fn parse_function(lexer: &mut Peekable<Lexer<'a>>, function_name: Token<'a>) -> Result<Function<'a>, ParserError<'a>> {
        let mut params = Vec::new();

        Self::expect_token(lexer, Token::LParen)?;

        loop {
            match lexer.peek() {
                Some(Ok(Token::RParen)) | None => break,
                Some(Ok(Token::Ident(_))) => {
                    match Self::parse_single_column(lexer) {
                        Some(Ok(column)) => params.push(column),
                        Some(Err(e)) => return Err(e),
                        None => {}
                    }
                }
                Some(Ok(Token::Comma)) => {
                    lexer.next();
                }
                Some(_) => {
                    match lexer.next() {
                        Some(Ok(token)) => return Err(ParserError::Syntax { expected: SyntaxType::Column, found: SyntaxType::Token(token) } ),
                        Some(Err(e)) => return Err(e),
                        None => break,
                    }
                }
            }
        }

        Self::expect_token(lexer, Token::RParen)?;

        Ok(Function {
            name: function_name,
            params,
            alias: None,
        })
    }
}
