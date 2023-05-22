use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Token};
use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

#[derive(Debug, PartialEq)]
pub enum TableType<'a> {
    Table(Table<'a>),
    Join {
        join_type: JoinType,
        oper: Box<Join<'a>>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub prefix: Option<Token<'a>>,
    pub name: Token<'a>,
    pub alias: Option<Token<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum JoinType {
    InnerJoin,
    LeftOuterJoin,
    RightOuterJoin,
    FullJoin,
    FullOuterJoin,
    LeftJoin,
    RightJoin,
}

#[derive(Debug, PartialEq)]
pub enum Join<'a> {
    Using {
        left: Table<'a>,
        right: Table<'a>,
        columns: Vec<Field<'a>>,
    },
    Condition {
        left: Table<'a>,
        right: Table<'a>,
        condition: Expr<'a>,
    },
    Natural {
        left: Table<'a>,
        right: Table<'a>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Column<'a> {
    pub prefix: Option<Token<'a>>,
    pub name: Token<'a>,
    pub alias: Option<Token<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Field<'a> {
    pub prefix: Option<Token<'a>>,
    pub name: Token<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Limit<'a> {
    pub from: Option<Token<'a>>,
    pub limit: Token<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Number(&'a str),
    Integer(&'a str),
    BigInteger(&'a str),
    Float(&'a str),
    String(String),
    Unicode(String),
    Params(&'a str),
    Field(Field<'a>),
    AndOr {
        operator: LinkOperator,
        left_expr: Box<Expr<'a>>,
        right_expr: Box<Expr<'a>>,
    },
    Equal {
        left_expr: Box<Expr<'a>>,
        right_expr: Box<Expr<'a>>,
    },
    FourFundamental {
        operation_type: FourFundamentalOperation,
        left_expr: Box<Expr<'a>>,
        right_expr: Box<Expr<'a>>,
    },
    In {
        target: Box<Expr<'a>>,
        expr_collection: Vec<Expr<'a>>,
    },
    Between {
        target: Box<Expr<'a>>,
        start: Box<Expr<'a>>,
        end: Box<Expr<'a>>,
    },
    Binary {
        operator: BinaryOperation,
        left_expr: Box<Expr<'a>>,
        right_expr: Box<Expr<'a>>,
    }
}

#[derive(Debug, PartialEq)]
pub enum LinkOperator {
    And,
    Or,
}

// 四则运算符
#[derive(Debug, PartialEq)]
pub enum FourFundamentalOperation {
    Plus,
    Sub,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperation {
    DoubleEqual,
    LessOrEqual,
    Less,
    Greater,
    GreaterOrEqual,
    Not,
    Is,

}
