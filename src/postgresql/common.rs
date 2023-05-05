use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Token};
use alloc::boxed::Box;
use alloc::vec::Vec;

#[derive(Debug, PartialEq)]
pub enum TableType<'a> {
    Table(Table<'a>),
    InnerJoin(Box<InnerJoin<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub prefix: Option<Token<'a>>,
    pub name: Token<'a>,
    pub alias: Option<Token<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum InnerJoin<'a> {
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
    Number(Token<'a>),
    Integer(Token<'a>),
    BigInteger(Token<'a>),
    Float(Token<'a>),
    String(Token<'a>),
    Unicode(Token<'a>),
    Params(Token<'a>),
    Field(Field<'a>),
    And(Box<And<'a>>),
    Eq(Box<Eq<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct And<'a> {
    left: Box<Expr<'a>>,
    right: Box<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Eq<'a> {
    left: Box<Expr<'a>>,
    right: Box<Expr<'a>>,
}
