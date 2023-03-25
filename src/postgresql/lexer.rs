use core::iter::{
    Peekable,
    Iterator,
};
use core::str::CharIndices;
use crate::error::Error;
use crate::postgresql::Token;

pub(crate) struct Lexer<'a> {
    src: &'a str,
    scanner: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        let scanner = src.char_indices().peekable();
        Self {
            src,
            scanner,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}