use core::ops::RangeInclusive;

#[derive(Debug, PartialEq)]
pub enum Error {
    Unexpected(ErrorType),
    Invalid,
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
}