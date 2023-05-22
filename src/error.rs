#[cfg(feature = "postgresql")]
use crate::postgresql::{Keyword, Token};

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    Unexpected(ErrorType),
    Invalid,
    Syntax {
        expected: SyntaxType<'a>,
        found: SyntaxType<'a>,
    },
}

#[derive(Debug, PartialEq)]
pub enum ErrorType {
    String((usize, usize)),
    Integer((usize, usize)),
    Float((usize, usize)),
    Number((usize, usize)),
    Unicode((usize, usize)),
    Symbol((usize, usize)),
    Ident((usize, usize)),
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum SyntaxType<'a> {
    Token(Token<'a>),
    Keyword(Keyword),
    Column,
    Join,
    LeftJoin,
    LeftOuterJoin,
    RightJoin,
    RightOutJoin,
    FullJoin,
    FullOuterJoin,
    Eof,
    Expr,
}
