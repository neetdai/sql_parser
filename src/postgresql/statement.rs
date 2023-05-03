use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Lexer, Select, Token};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Select(Select<'a>),
    // Insert(Insert<'a>),
    // Update(Update<'a>),
}

impl<'a> Statement<'a> {
    pub(crate) fn new(mut lexer: Lexer<'a>) -> Option<Result<Self, ParserError>> {
        match lexer.next()? {
            Ok(Token::Keyword(keyword)) => match keyword {
                Keyword::Select => match Select::new(lexer)? {
                    Ok(select) => Some(Ok(Self::Select(select))),
                    Err(e) => Some(Err(e)),
                },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

#[test]
fn select_statement() {
    use crate::postgresql::common::*;
    use alloc::vec;

    let mut lexer = Lexer::new("select * from a");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Some(Ok(Statement::Select(Select {
            columns: vec![Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            }],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            limit: None,
        })))
    );

    let mut lexer = Lexer::new("select a.b as b, c.d as d from w.q as a, e.r as c");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Some(Ok(Statement::Select(Select {
            columns: vec![
                Column {
                    prefix: Some(Token::Ident("a")),
                    name: Token::Ident("b"),
                    alias: Some(Token::Ident("b")),
                },
                Column {
                    prefix: Some(Token::Ident("c")),
                    name: Token::Ident("d"),
                    alias: Some(Token::Ident("d")),
                }
            ],
            tables: vec![
                TableType::Table(Table {
                    prefix: Some(Token::Ident("w")),
                    name: Token::Ident("q"),
                    alias: Some(Token::Ident("a")),
                }),
                TableType::Table(Table {
                    prefix: Some(Token::Ident("e")),
                    name: Token::Ident("r"),
                    alias: Some(Token::Ident("c")),
                })
            ],
            limit: None,
        })))
    );
}

#[test]
fn test_select_limit() {
    use crate::postgresql::common::*;
    use alloc::vec;
    
    let mut lexer = Lexer::new("select * from a limit 10");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Some(Ok(Statement::Select(Select {
            columns: vec![
                Column {
                    prefix: None,
                    name: Token::Mul,
                    alias: None,
                }
            ],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            limit: Some(Limit {
                from: None,
                limit: Token::Integer("10"),
            }),
        })))
    );

    let mut lexer = Lexer::new("select * from a limit 10, 50");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Some(Ok(Statement::Select(Select {
            columns: vec![
                Column {
                    prefix: None,
                    name: Token::Mul,
                    alias: None,
                }
            ],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            limit: Some(Limit {
                from: Some(Token::Integer("10")),
                limit: Token::Integer("50"),
            }),
        })))
    );
}