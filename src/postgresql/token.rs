use super::keyword::Keyword;

#[derive(Debug, PartialEq)]
pub(crate) enum Token<'a> {
    String(&'a str),
    Number(&'a str),
    Ident(&'a str),
    Keyword(Keyword),
}
