use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Lexer, Select, Token};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Select(Select<'a>),
    // Insert(Insert<'a>),
    // Update(Update<'a>),
}

impl<'a> Statement<'a> {
    pub(crate) fn new(mut lexer: Lexer<'a>) -> Result<Self, ParserError<'a>> {
        let mut lexer = lexer.peekable();
        match lexer.peek() {
            Some(Ok(Token::Keyword(keyword))) => match keyword {
                Keyword::Select => Select::new(&mut lexer).map(Self::Select),
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
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select a.b as b, c.d as d from w.q as a, e.r as c");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![
                ColumnType::Column(Column {
                    prefix: Some(Token::Ident("a")),
                    name: Token::Ident("b"),
                    alias: Some(Token::Ident("b")),
                }),
                ColumnType::Column(Column {
                    prefix: Some(Token::Ident("c")),
                    name: Token::Ident("d"),
                    alias: Some(Token::Ident("d")),
                })
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
            r#where: None,
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select a.* from a");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: Some(Token::Ident("a")),
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select a from a");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: None,
        }))
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
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: Some(Limit {
                from: None,
                limit: Token::Integer("10"),
            }),
        }))
    );

    let mut lexer = Lexer::new("select * from a limit 10, 50");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: Some(Limit {
                from: Some(Token::Integer("10")),
                limit: Token::Integer("50"),
            }),
        }))
    );
}

#[test]
fn test_select_inner_join() {
    use crate::postgresql::common::*;
    use alloc::boxed::Box;
    use alloc::vec;

    let join_query = [
        (
            "select * from a inner join b using (q, w, e)",
            JoinType::InnerJoin,
        ),
        (
            "select * from a left outer join b using (q, w, e)",
            JoinType::LeftOuterJoin,
        ),
        (
            "select * from a right outer join b using (q, w, e)",
            JoinType::RightOuterJoin,
        ),
        (
            "select * from a left join b using (q, w, e)",
            JoinType::LeftJoin,
        ),
        (
            "select * from a right join b using (q, w, e)",
            JoinType::RightJoin,
        ),
        (
            "select * from a full join b using (q, w, e)",
            JoinType::FullJoin,
        ),
        (
            "select * from a full outer join b using (q, w, e)",
            JoinType::FullOuterJoin,
        ),
    ];
    for (query_text, join_type) in join_query {
        let mut lexer = Lexer::new(query_text);
        let statement = Statement::new(lexer);
        assert_eq!(
            statement,
            Ok(Statement::Select(Select {
                columns: vec![ColumnType::Column(Column {
                    prefix: None,
                    name: Token::Mul,
                    alias: None,
                })],
                tables: vec![TableType::Join {
                    join_type: join_type,
                    oper: Box::new(Join::Using {
                        left: Table {
                            prefix: None,
                            name: Token::Ident("a"),
                            alias: None,
                        },
                        right: Table {
                            prefix: None,
                            name: Token::Ident("b"),
                            alias: None,
                        },
                        columns: vec![
                            Field {
                                prefix: None,
                                name: Token::Ident("q")
                            },
                            Field {
                                prefix: None,
                                name: Token::Ident("w")
                            },
                            Field {
                                prefix: None,
                                name: Token::Ident("e")
                            },
                        ]
                    })
                }],
                r#where: None,
                limit: None,
            }))
        );
    }
}

#[test]
fn test_select_where() {
    use crate::postgresql::common::*;
    use alloc::boxed::Box;
    use alloc::vec;

    let mut lexer = Lexer::new("select * from a where 1 + 1 * 2");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Plus,
                left_expr: Box::new(Expr::Integer("1")),
                right_expr: Box::new(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Multiply,
                    left_expr: Box::new(Expr::Integer("1")),
                    right_expr: Box::new(Expr::Integer("2")),
                }),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where 1 * 2 + 1");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Plus,
                left_expr: Box::new(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Multiply,
                    left_expr: Box::new(Expr::Integer("1")),
                    right_expr: Box::new(Expr::Integer("2")),
                }),
                right_expr: Box::new(Expr::Integer("1")),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where 1 + 2 ^ 1");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Plus,
                left_expr: Box::new(Expr::Integer("1")),
                right_expr: Box::new(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Exponent,
                    left_expr: Box::new(Expr::Integer("2")),
                    right_expr: Box::new(Expr::Integer("1")),
                }),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where 1 * 2 ^ 1");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Multiply,
                left_expr: Box::new(Expr::Integer("1")),
                right_expr: Box::new(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Exponent,
                    left_expr: Box::new(Expr::Integer("2")),
                    right_expr: Box::new(Expr::Integer("1")),
                }),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where 2 ^ ( 1 + 1 )");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Exponent,
                left_expr: Box::new(Expr::Integer("2")),
                right_expr: Box::new(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Plus,
                    left_expr: Box::new(Expr::Integer("1")),
                    right_expr: Box::new(Expr::Integer("1")),
                }),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where 1 * ( 2 + 1 )");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::FourFundamental {
                operation_type: FourFundamentalOperation::Multiply,
                left_expr: Box::new(Expr::Integer("1")),
                right_expr: Box::new(Expr::FourFundamental {
                    operation_type: FourFundamentalOperation::Plus,
                    left_expr: Box::new(Expr::Integer("2")),
                    right_expr: Box::new(Expr::Integer("1")),
                }),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where a in (1,2,3)");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::In {
                target: Box::new(Expr::Field(Field {
                    prefix: None,
                    name: Token::Ident("a")
                })),
                expr_collection: vec![Expr::Integer("1"), Expr::Integer("2"), Expr::Integer("3")]
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where a in (1,2,3) and b = 1 * (1 + 2)");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::AndOr {
                operator: LinkOperator::And,
                left_expr: Box::new(Expr::In {
                    target: Box::new(Expr::Field(Field {
                        prefix: None,
                        name: Token::Ident("a")
                    })),
                    expr_collection: vec![
                        Expr::Integer("1"),
                        Expr::Integer("2"),
                        Expr::Integer("3")
                    ]
                }),
                right_expr: Box::new(Expr::Equal {
                    left_expr: Box::new(Expr::Field(Field {
                        prefix: None,
                        name: Token::Ident("b")
                    })),
                    right_expr: Box::new(Expr::FourFundamental {
                        operation_type: FourFundamentalOperation::Multiply,
                        left_expr: Box::new(Expr::Integer("1")),
                        right_expr: Box::new(Expr::FourFundamental {
                            operation_type: FourFundamentalOperation::Plus,
                            left_expr: Box::new(Expr::Integer("1")),
                            right_expr: Box::new(Expr::Integer("2")),
                        }),
                    }),
                }),
            }),
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select * from a where a between 1 and 10");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Column(Column {
                prefix: None,
                name: Token::Mul,
                alias: None,
            })],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: Some(Expr::Between {
                target: Box::new(Expr::Field(Field { prefix: None,  name: Token::Ident("a") })),
                start: Box::new(Expr::Integer("1")),
                end: Box::new(Expr::Integer("10")),
            }),
            limit: None,
        }))
    );

}


#[test]
fn test_select_binary_operator() {
    use crate::postgresql::common::*;
    use alloc::boxed::Box;
    use alloc::vec;

    let func = |sql_text: &str, binary_operation: BinaryOperation| {
        let mut lexer = Lexer::new(sql_text);
        let statement = Statement::new(lexer);
        assert_eq!(
            statement,
            Ok(Statement::Select(Select {
                columns: vec![ColumnType::Column(Column {
                    prefix: None,
                    name: Token::Mul,
                    alias: None,
                })],
                tables: vec![TableType::Table(Table {
                    prefix: None,
                    name: Token::Ident("a"),
                    alias: None,
                })],
                r#where: Some(Expr::Binary {
                    operator: binary_operation,
                    left_expr: Box::new(Expr::Integer("2")),
                    right_expr: Box::new(Expr::Integer("1")),
                }),
                limit: None,
            }))
        );
    };
    
    let list = vec![
        ("select * from a where 2 == 1", BinaryOperation::DoubleEqual),
        ("select * from a where 2 < 1", BinaryOperation::Less),
        ("select * from a where 2 > 1", BinaryOperation::Greater),
        ("select * from a where 2 >= 1", BinaryOperation::GreaterOrEqual),
        ("select * from a where 2 <= 1", BinaryOperation::LessOrEqual),
        ("select * from a where 2 <> 1", BinaryOperation::LessOrGreater),
        ("select * from a where 2 >= 1", BinaryOperation::GreaterOrEqual),
        ("select * from a where 2 != 1", BinaryOperation::NotEqual),
        ("select * from a where 2 is 1", BinaryOperation::Is),
        ("select * from a where 2 not 1", BinaryOperation::Not),
    ];
}

#[test]
fn test_select_function() {
    use crate::postgresql::common::*;
    use alloc::boxed::Box;
    use alloc::vec;

    let mut lexer = Lexer::new("select count(a.b) as a_q from a");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Function(Box::new(Function {
                name: Token::Ident("count"),
                params: vec![
                    ColumnType::Column(Column {
                        prefix: Some(Token::Ident("a")),
                        name: Token::Ident("b"),
                        alias: None,
                    })
                ],
                alias: Some(Token::Ident("a_q")),
            }))],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: None,
        }))
    );

    let mut lexer = Lexer::new("select count(func(a.b)) as a_q from a");
    let statement = Statement::new(lexer);
    assert_eq!(
        statement,
        Ok(Statement::Select(Select {
            columns: vec![ColumnType::Function(Box::new(Function {
                name: Token::Ident("count"),
                params: vec![
                    ColumnType::Function(Box::new(Function {
                        name: Token::Ident("func"),
                        params: vec![
                            ColumnType::Column(Column {
                                prefix: Some(Token::Ident("a")),
                                name: Token::Ident("b"),
                                alias: None,
                            })
                        ],
                        alias: None
                    }))
                ],
                alias: Some(Token::Ident("a_q")),
            }))],
            tables: vec![TableType::Table(Table {
                prefix: None,
                name: Token::Ident("a"),
                alias: None,
            })],
            r#where: None,
            limit: None,
        }))
    );
}