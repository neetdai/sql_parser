use super::keyword::Keyword;
use alloc::string::String;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    String(String),
    Unicode(String),
    Number(&'a str),
    BigInteger(&'a str),
    Integer(&'a str),
    Float(&'a str),
    Ident(&'a str),
    Keyword(Keyword),
    Params(&'a str),

    // 点号
    Period,

    // 逗号
    Comma,

    // 除号
    Div,

    // 等于号
    Equal,

    // 双等于号
    DoubleEqual,

    // 大于
    Greater,

    // 大于等于
    GreaterOrEqual,

    // 小于
    Less,

    // 小于等于
    LessOrEqual,

    // 小于大于
    LessOrGreater,

    // 加号
    Plus,

    // 减号
    Sub,

    // 乘号
    Mul,

    // 波浪线
    Tilde,

    // 
    At,

    // 井号
    Sharp,

    // 取模
    Mod,

    // 异或
    Caret,

    // 并
    Ampersand,

    // 或
    Pipe,

    // 问号
    QuestionMark,

    // 左括号
    LParen,

    // 右括号
    RParen,

    // 左中括号
    LBracket,

    // 右中括号
    RBracket,

    // 结束符
    Eof,

    // 冒号
    Colon,

    // 非
    Negative,

    // 不等于
    NotEqual,
}
