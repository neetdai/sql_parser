use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Token};
use alloc::vec::Vec;
use alloc::boxed::Box;

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
        left: TableType<'a>,
        right: TableType<'a>,
        columns: Vec<Token<'a>>
    },
    Condition {
        left: TableType<'a>,
        right: TableType<'a>,
        operator: Operator<'a>
    },
}

#[derive(Debug, PartialEq)]
pub struct Column<'a> {
    pub prefix: Option<Token<'a>>,
    pub name: Token<'a>,
    pub alias: Option<Token<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Limit<'a> {
    pub from: Option<Token<'a>>,
    pub limit: Token<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Operator<'a> {
    Token(Token<'a>),
    And(Box<And<'a>>),
    Eq(Box<Eq<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct And<'a> {
    left: Box<Operator<'a>>,
    right: Box<Operator<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Eq<'a> {
    left: Box<Operator<'a>>,
    right: Box<Operator<'a>>,
}