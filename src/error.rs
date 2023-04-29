use core::ops::RangeInclusive;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Error {
    Unexpected(ErrorType),
    Invalid,
    Syntax,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ErrorType {
    String((usize, usize)),
    Integer((usize, usize)),
    Float((usize, usize)),
    Number((usize, usize)),
    Unicode((usize, usize)),
    Symbol((usize, usize)),
    Ident((usize, usize)),
}
