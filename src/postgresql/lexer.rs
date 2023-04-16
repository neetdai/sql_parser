use crate::error::Error as ParserError;
use crate::postgresql::{Keyword, Token};
use core::iter::{Iterator, Peekable};
use core::ops::{FnOnce, FnMut};
use core::str::CharIndices;
use alloc::string::String;

pub(crate) struct Lexer<'a> {
    src: &'a str,
    scanner: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        let scanner = src.char_indices().peekable();
        Self { src, scanner }
    }

    // 消耗空格
    fn consume_whitespace(&mut self) {
        self.while_next_if(|(_, c)| c.is_whitespace());
    }

    // 通过条件获取字符串所在的区间, 并且消耗迭代器
    fn while_next_if<F>(&mut self, condition: F) -> Option<(usize, usize)>
    where
        F: FnOnce(&(usize, char)) -> bool + Copy,
    {
        let begin = self.scanner.next_if(condition).map(|(index, _)| index)?;

        let mut end = begin;

        while let Some((index, _)) = self.scanner.next_if(condition) {
            end = index;
        }

        Some((begin, end))
    }

    fn peek_if<F>(&mut self, condition: F) -> Option<&(usize, char)>
    where
        F: FnOnce(&&(usize, char)) -> bool + Copy
    {
        self.scanner.peek().filter(condition)
    }

    fn next_if<F>(&mut self, condition: F) -> Option<(usize, char)>
    where
        F: FnOnce(&(usize, char)) -> bool + Copy
    {
        self.scanner.next_if(condition)
    }

    // 扫描关键字或标识符, 消费迭代器
    // SQL标识符和关键词必须以一个字母（a-z，也可以是带变音符的字母和非拉丁字母）或一个下划线（_）开始。后续字符可以是字母、下划线（_）、数字（0-9）或美元符号（$）
    // http://www.postgres.cn/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
    fn scan_ident_quato(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        let (begin, end) = self.while_next_if(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')?;

        let ident = self.src.get(begin..=end)?;
        let mut ident_chars = ident.chars();
        let first = ident_chars.next()?;

        if !first.is_ascii_alphabetic() || first != '_' {
            return Some(Err(ParserError::Unexpected(String::from("Unexpected ident"))));
        }
        
        let token = Keyword::compare_str(ident)
            .map(|keyword|Ok(Token::Keyword(keyword)))
            .or(Some(Ok(Token::Ident(ident))));

        token
    }

    // 扫描数字, 消费迭代器
    fn scan_number(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        let (begin, mut end) = self.while_next_if(|(_, c)| c.is_ascii_digit())?;

        if let Some((tmp_end, _)) = self.next_if(|(_, c)| *c == '.') {
            end = tmp_end;
            if self.peek_if(|(_, c)| c.is_ascii_digit()).is_some() {
                let (_, tmp_end) = self.while_next_if(|(_, c)| c.is_ascii_digit())?;
                end = tmp_end;
            }
        }

        match self.scan_scientific_notation() {
            Some(Ok((_, tmp_end))) => end = tmp_end,
            Some(Err(e)) => return Some(Err(e)),
            None => {}
        }

        let number = self.src.get(begin..=end)?;

        Some(Ok(Token::Number(number)))
    }

    // 扫描科学计数法
    fn scan_scientific_notation(&mut self) -> Option<Result<(usize, usize), ParserError>> {
        let (begin, _) = self.next_if(|(_, c)| *c == 'e' || *c == 'E')?;
        let mut end = begin;

        if self.peek_if(|(_, c)| *c == '-').is_some() {
            match self.scan_negative_number() {
                Some(Ok((_, tmp_end))) => {
                    end = tmp_end;
                    return Some(Ok((begin, end)));
                }
                Some(Err(e)) => return Some(Err(e)),
                None => {},
            }
        }
        if self.peek_if(|(_, c)| c.is_ascii_digit()).is_some() {
            let (_, tmp_end) = self.while_next_if(|(_, c)| c.is_ascii_digit())?;
            end = tmp_end;
        } else {
            return Some(Err(ParserError::Unexpected(String::from("Unexpected number"))));
        }

        Some(Ok((begin, end)))
    }

    // 扫描负整数
    fn scan_negative_number(&mut self) -> Option<Result<(usize, usize), ParserError>> {
        let (begin, _) = self.next_if(|(_, c)| *c == '-')?;

        Some(
            self.while_next_if(|(_, c)| c.is_ascii_digit())
                .ok_or(ParserError::Unexpected(String::from("Unexpected negative number")))
                .map(|(_, end)| {
                    (begin, end)
                })
        )
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.scanner.peek() {
            Some((_, c)) if c.is_ascii_digit() => self.scan_number(),
            Some(_) => self.scan_ident_quato(),
            None => None,
        }
    }
}

#[test]
fn scan_number1() {
    let mut lexer = Lexer::new("123");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123"))));

    let mut lexer = Lexer::new("123.456");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123.456"))));

    let mut lexer = Lexer::new("123.asdf");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123."))));

    let mut lexer = Lexer::new("123e3");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123e3"))));

    let mut lexer = Lexer::new("123ea");
    assert_eq!(lexer.next(), Some(Err(ParserError::Unexpected(String::from("Unexpected number")))));

    let mut lexer = Lexer::new("123.456e12");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123.456e12"))));

    let mut lexer = Lexer::new("123e-3");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123e-3"))));

    let mut lexer = Lexer::new("123.e3");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123.e3"))));

    // let mut lexer = Lexer::new(".123");
    // assert_eq!(lexer.next(), Some(Ok(Token::Number(".123"))));
}