use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Token};
use alloc::vec::Vec;

#[derive(Debug, PartialEq)]
pub enum TableType<'a> {
    Table(Table<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub prefix: Option<Token<'a>>,
    pub name: Token<'a>,
    pub alias: Option<Token<'a>>,
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