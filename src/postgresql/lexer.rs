use crate::error::{Error as ParserError, ErrorType};
use crate::postgresql::{Keyword, Token};
use alloc::string::String;
use alloc::vec::Vec;
use core::char;
use core::iter::{Extend, Iterator, Peekable};
use core::ops::FnOnce;
use core::str::CharIndices;

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
        F: FnOnce(&&(usize, char)) -> bool + Copy,
    {
        self.scanner.peek().filter(condition)
    }

    fn next_if<F>(&mut self, condition: F) -> Option<(usize, char)>
    where
        F: FnOnce(&(usize, char)) -> bool + Copy,
    {
        self.scanner.next_if(condition)
    }

    // 扫描关键字或标识符, 消费迭代器
    // SQL标识符和关键词必须以一个字母（a-z，也可以是带变音符的字母和非拉丁字母）或一个下划线（_）开始。后续字符可以是字母、下划线（_）、数字（0-9）或美元符号（$）
    // http://www.postgres.cn/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
    fn scan_ident_quato(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        let (begin, end) =
            self.while_next_if(|(_, c)| c.is_ascii_alphanumeric() || *c == '_' || *c == '$')?;

        let ident = self.src.get(begin..=end)?;
        let mut ident_chars = ident.chars();
        let first = ident_chars.next()?;

        if first.is_ascii_digit() || first == '$' {
            return Some(Err(ParserError::Unexpected(ErrorType::Ident((begin, end)))));
        }

        let token = Keyword::compare_str(ident)
            .map(|keyword| Ok(Token::Keyword(keyword)))
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
                None => {}
            }
        }
        if self.peek_if(|(_, c)| c.is_ascii_digit()).is_some() {
            let (_, tmp_end) = self.while_next_if(|(_, c)| c.is_ascii_digit())?;
            end = tmp_end;
        } else {
            return Some(Err(ParserError::Unexpected(ErrorType::Number((
                begin, end,
            )))));
        }

        Some(Ok((begin, end)))
    }

    // 扫描负整数
    fn scan_negative_number(&mut self) -> Option<Result<(usize, usize), ParserError>> {
        let (begin, end) = self.next_if(|(_, c)| *c == '-')?;

        Some(
            self.while_next_if(|(_, c)| c.is_ascii_digit())
                .ok_or(ParserError::Unexpected(ErrorType::Number((
                    begin,
                    self.scanner
                        .peek()
                        .map(|(index, _)| *index)
                        .unwrap_or(self.src.len() - 1),
                ))))
                .map(|(_, end)| (begin, end)),
        )
    }

    // 扫描以小数点为前缀的数字
    fn scan_decimal_prefix(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        let (begin, _) = self.next_if(|(_, c)| *c == '.')?;
        let mut end = begin;

        match self.while_next_if(|(_, c)| c.is_ascii_digit()) {
            Some((_, tmp_end)) => end = tmp_end,
            None => return None,
        }

        match self.scan_scientific_notation() {
            Some(Ok((_, tmp_end))) => end = tmp_end,
            Some(Err(e)) => return Some(Err(e)),
            None => {}
        }

        let number = self.src.get(begin..=end)?;

        Some(Ok(Token::Number(number)))
    }

    // 扫描符号
    fn scan_symbol(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        match self.scanner.peek() {
            Some((_, ',')) => {
                self.scanner.next();
                Some(Ok(Token::Comma))
            }
            Some((_, '=')) => {
                self.scanner.next();
                Some(Ok(Token::Equal))
            }
            Some((_, '>')) => {
                self.scanner.next();
                self.next_if(|(_, c)| *c == '=')
                    .map(|_| Ok(Token::GreaterOrEqual))
                    .or(Some(Ok(Token::Greater)))
            }
            Some((_, '<')) => {
                self.scanner.next();
                self.next_if(|(_, c)| *c == '=')
                    .map(|_| Ok(Token::LessOrEqual))
                    .or(Some(Ok(Token::Less)))
            }
            Some((_, '+')) => {
                self.scanner.next();
                Some(Ok(Token::Plus))
            }
            Some((_, '-')) => {
                self.scanner.next();
                // 一段注释是以双横杠开始并且延伸到行结尾的一个字符序列
                match self.next_if(|(_, c)| *c == '-') {
                    None => Some(Ok(Token::Sub)),
                    Some(_) => {
                        self.scan_annotation();
                        self.next()
                    }
                }
            }
            Some((_, '/')) => {
                self.scanner.next();
                match self.next_if(|(_, c)| *c == '*') {
                    None => Some(Ok(Token::Div)),
                    Some(_) => {
                        self.scan_c_like_annotation();
                        self.next()
                    }
                }
            }
            Some((_, '*')) => {
                self.scanner.next();
                Some(Ok(Token::Mul))
            }
            Some((_, '~')) => {
                self.scanner.next();
                Some(Ok(Token::Tilde))
            }
            Some((_, '@')) => {
                self.scanner.next();
                Some(Ok(Token::At))
            }
            Some((_, '#')) => {
                self.scanner.next();
                Some(Ok(Token::Sharp))
            }
            Some((_, '%')) => {
                self.scanner.next();
                Some(Ok(Token::Mod))
            }
            Some((_, '^')) => {
                self.scanner.next();
                Some(Ok(Token::Caret))
            }
            Some((_, '&')) => {
                self.scanner.next();
                Some(Ok(Token::Ampersand))
            }
            Some((_, '|')) => {
                self.scanner.next();
                Some(Ok(Token::Pipe))
            }
            Some((_, '?')) => {
                self.scanner.next();
                Some(Ok(Token::QuestionMark))
            }
            Some((_, '(')) => {
                self.scanner.next();
                Some(Ok(Token::LParen))
            }
            Some((_, ')')) => {
                self.scanner.next();
                Some(Ok(Token::RParen))
            }
            Some((_, '[')) => {
                self.scanner.next();
                Some(Ok(Token::LBracket))
            }
            Some((_, ']')) => {
                self.scanner.next();
                Some(Ok(Token::RBracket))
            }
            Some((_, ';')) => {
                self.scanner.next();
                Some(Ok(Token::Eof))
            }
            Some((_, ':')) => {
                self.scanner.next();
                Some(Ok(Token::Colon))
            }
            Some(_) => self.scan_ident_quato(),
            None => None,
        }
    }

    // 扫描注释
    fn scan_annotation(&mut self) {
        self.while_next_if(|(_, c)| *c != '\n');
        self.scanner.next();
    }

    // 扫描C风格注释块
    fn scan_c_like_annotation(&mut self) -> Option<()> {
        loop {
            match self.scanner.next() {
                None => return None,
                Some((_, '*')) => {
                    if self.next_if(|(_, c)| *c == '/').is_some() {
                        return Some(());
                    }
                }
                Some((_, '/')) => {
                    if self.next_if(|(_, c)| *c == '*').is_some() {
                        self.scan_c_like_annotation();
                    }
                }
                Some(_) => {}
            }
        }
    }

    // 扫描字符串
    fn scan_string(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        self.scan_str().map(|result| result.map(Token::String))
    }

    //在SQL中，一个字符串常量是一个由单引号（'）包围的任意字符序列，例如'This is a string'。为了在一个字符串中包括一个单引号，可以写两个相连的单引号，例如'Dianne''s horse'。注意这和一个双引号（"）不同。
    // 两个只由空白及至少一个新行分隔的字符串常量会被连接在一起，并且将作为一个写在一起的字符串常量来对待。例如：
    //
    // SELECT 'foo'
    // 'bar';
    //
    // 等同于：
    //
    // SELECT 'foobar';
    //
    // 但是：
    //
    // SELECT 'foo'      'bar';
    //
    // 则不是合法的语法（这种有些奇怪的行为是SQL指定的，PostgreSQL遵循了该标准）。
    fn scan_str(&mut self) -> Option<Result<String, ParserError>> {
        let mut s = String::new();

        let begin = self.next_if(|(_, c)| *c == '\'').map(|(index, _)| index)? + 1;
        let mut end = begin;

        loop {
            match self.scanner.next() {
                Some((end_index, '\'')) => match self.scanner.peek() {
                    Some((index, '\'')) => {
                        end = *index;
                        self.scanner.next();
                    }
                    Some((_, '\n')) => {
                        s.push_str(self.src.get(begin..=end)?);
                        self.while_next_if(|(_, c)| *c == '\n');
                        match self.scan_str() {
                            Some(Ok(tmp_s)) => {
                                s.push_str(tmp_s.as_str());
                                return Some(Ok(s));
                            }
                            Some(Err(e)) => return Some(Err(e)),
                            None => return Some(Ok(s)),
                        }
                    }
                    _ => {
                        end = end_index;
                        s.push_str(self.src.get(begin..end)?);
                        return Some(Ok(s));
                    }
                },
                Some((index, _)) => {
                    end = index;
                }
                None => {
                    return Some(Err(ParserError::Unexpected(ErrorType::String((
                        begin, end,
                    )))));
                }
            }
        }
    }

    // 扫描utf16
    // 在引号内，Unicode字符可以以转义的形式指定：反斜线接上4位16进制代码点号码或者反斜线和加号接上6位16进制代码点号码。
    fn scan_unicode(&mut self) -> Option<Result<Token<'a>, ParserError>> {
        let begin = self
            .next_if(|(_, c)| *c == 'U' || *c == 'u')
            .map(|(index, _)| index)?;
        if self.next_if(|(_, c)| *c == '&').is_none() {
            return Some(Err(ParserError::Unexpected(ErrorType::Unicode((
                begin, begin,
            )))));
        }

        let begin = match self.next_if(|(_, c)| *c == '\'') {
            Some((index, _)) => index + 1,
            None => {
                return Some(Err(ParserError::Unexpected(ErrorType::Unicode((
                    begin,
                    begin + 1,
                )))))
            }
        };

        let mut unicode = Vec::new();
        let mut end = begin;
        loop {
            match self.scanner.next() {
                Some((_, '\\')) => {
                    let range = {
                        if self.next_if(|(_, c)| *c == '+').is_some() {
                            0..6
                        } else {
                            0..4
                        }
                    };

                    let mut n = 0;
                    for _ in range {
                        match self.scanner.next() {
                            Some((index, c)) if c.is_ascii_digit() => {
                                end = index;
                                let tmp = u32::from(c) - 48;
                                n <<= 4;
                                n += tmp;
                            }
                            Some((index, _)) => {
                                end = index;
                                return Some(Err(ParserError::Unexpected(ErrorType::Unicode((
                                    begin, end,
                                )))));
                            }
                            None => {
                                return Some(Err(ParserError::Unexpected(ErrorType::Unicode((
                                    begin,
                                    end,
                                )))))
                            }
                        }
                    }

                    unicode.push(n);
                }
                Some((_, c)) if c == '\'' => {
                    return Some(
                        char::decode_utf16(unicode.into_iter().map(|c| c as u16))
                            .collect::<Result<String, char::DecodeUtf16Error>>()
                            .map(Token::Unicode)
                            .map_err(|_| ParserError::Unexpected(ErrorType::Unicode((begin, end)))),
                    );
                }
                Some((_, c)) => {
                    unicode.push(u32::from(c));
                }
                None => {
                    return Some(Err(ParserError::Unexpected(ErrorType::Unicode((
                        begin,
                        end + 1,
                    )))));
                }
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();

        match self.scanner.peek() {
            Some((index, c)) if *c == 'u' || *c == 'U' => {
                if let Some(prefix) = self.src.get(*index..=*index + 1) {
                    let prefix_upper = prefix.to_uppercase();
                    if prefix_upper.as_str() == "U&" {
                        self.scan_unicode()
                    } else {
                        self.scan_ident_quato()
                    }
                } else {
                    self.scan_ident_quato()
                }
            }
            Some((_, '\'')) => self.scan_string(),
            Some((_, '.')) => self.scan_decimal_prefix().or(Some(Ok(Token::Period))),
            Some((_, c)) if c.is_ascii_digit() => self.scan_number(),
            Some((_, c)) if !c.is_ascii_alphabetic() && !c.is_ascii_digit() => self.scan_symbol(),
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
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Number((3, 3)))))
    );

    let mut lexer = Lexer::new("123.456e12");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123.456e12"))));

    let mut lexer = Lexer::new("123e-3");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123e-3"))));

    let mut lexer = Lexer::new("123.e3");
    assert_eq!(lexer.next(), Some(Ok(Token::Number("123.e3"))));

    let mut lexer = Lexer::new("123e-a");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Number((4, 5)))))
    );

    let mut lexer = Lexer::new(".123");
    assert_eq!(lexer.next(), Some(Ok(Token::Number(".123"))));

    let mut lexer = Lexer::new(".123e3");
    assert_eq!(lexer.next(), Some(Ok(Token::Number(".123e3"))));

    let mut lexer = Lexer::new(".asd");
    assert_eq!(lexer.next(), Some(Ok(Token::Period)));

    let mut lexer = Lexer::new("123e-");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Number((4, 4)))))
    );
}

#[test]
fn scan_symbol() {
    use alloc::vec;
    use alloc::vec::Vec;
    let compare = |params: Vec<(&str, Token<'static>)>| {
        for (a, token) in params.into_iter() {
            let mut lexer = Lexer::new(a);
            assert_eq!(lexer.next(), Some(Ok(token)));
        }
    };

    compare(vec![
        (",", Token::Comma),
        ("=", Token::Equal),
        (">", Token::Greater),
        (">=", Token::GreaterOrEqual),
        ("<", Token::Less),
        ("<=", Token::LessOrEqual),
        ("@", Token::At),
        ("~", Token::Tilde),
        ("+", Token::Plus),
        ("-", Token::Sub),
        ("/", Token::Div),
        ("*", Token::Mul),
        ("?", Token::QuestionMark),
        ("%", Token::Mod),
        ("#", Token::Sharp),
        ("^", Token::Caret),
        ("&", Token::Ampersand),
        ("|", Token::Pipe),
        ("(", Token::LParen),
        (")", Token::RParen),
        ("[", Token::LBracket),
        ("]", Token::RBracket),
        (";", Token::Eof),
        (":", Token::Colon),
        ("--sdfsdf\n#", Token::Sharp),
        (
            r#"/* multiline comment
            * with nesting: /* nested block comment */
            */="#,
            Token::Equal,
        ),
    ]);
}

#[test]
fn scan_string() {
    let mut lexer = Lexer::new("\'asdfa\'");
    assert_eq!(lexer.next(), Some(Ok(Token::String(String::from("asdfa")))));

    let mut lexer = Lexer::new("\'asd");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::String((1, 3)))))
    );

    let mut lexer = Lexer::new("\'\'");
    assert_eq!(lexer.next(), Some(Ok(Token::String(String::from("")))));

    let mut lexer = Lexer::new("\'");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::String((1, 1)))))
    );

    let mut lexer = Lexer::new("\'asd\'\n\n\'asd\'");
    assert_eq!(
        lexer.next(),
        Some(Ok(Token::String(String::from("asdasd"))))
    );

    let mut lexer = Lexer::new("\'asd\'\n\n");
    assert_eq!(lexer.next(), Some(Ok(Token::String(String::from("asd")))));

    let mut lexer = Lexer::new("\'foo\'  \'bar\'");
    assert_eq!(lexer.next(), Some(Ok(Token::String(String::from("foo")))));

    let mut lexer = Lexer::new("\'asda\'\'sdfs\'");
    assert_eq!(
        lexer.next(),
        Some(Ok(Token::String(String::from("asda\'\'sdfs"))))
    );

    let mut lexer = Lexer::new("\'\'\'asdasdfs\'");
    assert_eq!(
        lexer.next(),
        Some(Ok(Token::String(String::from("\'\'asdasdfs"))))
    );
}

#[test]
fn scan_unicode() {
    let mut lexer = Lexer::new(r"U&'d\0061t\+000061'");
    assert_eq!(lexer.next(), Some(Ok(Token::Unicode(String::from("data")))));

    let mut lexer = Lexer::new(r"U&'d\0061t");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Unicode((3, 9)))))
    );

    let mut lexer = Lexer::new(r"U&'d\061t'");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Unicode((3, 8)))))
    );

    let mut lexer = Lexer::new(r"U&'d\0061t\+00061'");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Unicode((3, 17)))))
    );

    let mut lexer = Lexer::new(r"");
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new(r"U");
    assert_eq!(lexer.next(), Some(Ok(Token::Ident("U"))));

    let mut lexer = Lexer::new(r"U&");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Unicode((0, 1)))))
    );

    let mut lexer = Lexer::new(r"U&'\006");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Unicode((
            3,
            6,
        )))))
    )
}

#[test]
fn scan_keyword() {
    use alloc::vec;
    let func = |list: Vec<(&str, Keyword)>| {
        for (text, keyword) in list.into_iter() {
            let mut lexer = Lexer::new(text);
            assert_eq!(lexer.next(), Some(Ok(Token::Keyword(keyword))));
        }
    };

    func(vec![
        ("Abs", Keyword::Abs),
        ("All", Keyword::All),
        ("Analyse", Keyword::Analyse),
        ("Analyze", Keyword::Analyze),
        ("And", Keyword::And),
        ("Any", Keyword::Any),
        ("Array", Keyword::Array),
        ("As),", Keyword::As),
        ("Asc", Keyword::Asc),
        ("Asymmetric", Keyword::Asymmetric),
        ("Authorization", Keyword::Authorization),
        ("Binary", Keyword::Binary),
        ("Both", Keyword::Both),
        ("Case", Keyword::Case),
        ("Cast", Keyword::Cast),
        ("Check", Keyword::Check),
        ("Collate", Keyword::Collate),
        ("Collation", Keyword::Collation),
        ("Column", Keyword::Column),
        ("Concurrently", Keyword::Concurrently),
        ("Constraint", Keyword::Constraint),
        ("Create", Keyword::Create),
        ("Cross", Keyword::Cross),
        ("Current_Catalog", Keyword::Current_Catalog),
        ("Current_Date", Keyword::Current_Date),
        ("Current_Role", Keyword::Current_Role),
        ("Current_Schema", Keyword::Current_Schema),
        ("Current_Time", Keyword::Current_Time),
        ("Current_Timestamp", Keyword::Current_Timestamp),
        ("Current_User", Keyword::Current_User),
        ("Default", Keyword::Default),
        ("Deferrable", Keyword::Deferrable),
        ("Desc", Keyword::Desc),
        ("Distinct", Keyword::Distinct),
        ("Do),", Keyword::Do),
        ("Else", Keyword::Else),
        ("Except", Keyword::Except),
        ("False", Keyword::False),
        ("Fetch", Keyword::Fetch),
        ("For", Keyword::For),
        ("Foreign", Keyword::Foreign),
        ("Freeze", Keyword::Freeze),
        ("From", Keyword::From),
        ("Full", Keyword::Full),
        ("Grant", Keyword::Grant),
        ("Group", Keyword::Group),
        ("Having", Keyword::Having),
        ("Ilike", Keyword::Ilike),
        ("In),", Keyword::In),
        ("Initially", Keyword::Initially),
        ("Inner", Keyword::Inner),
        ("Intersect", Keyword::Intersect),
        ("Into", Keyword::Into),
        ("Is),", Keyword::Is),
        ("Isnull", Keyword::Isnull),
        ("Join", Keyword::Join),
        ("Lateral", Keyword::Lateral),
        ("Leading", Keyword::Leading),
        ("Left", Keyword::Left),
        ("Like", Keyword::Like),
        ("Limit", Keyword::Limit),
        ("Localtime", Keyword::Localtime),
        ("Localtimestamp", Keyword::Localtimestamp),
        ("Natural", Keyword::Natural),
        ("Not", Keyword::Not),
        ("Notnull", Keyword::Notnull),
        ("Null", Keyword::Null),
        ("On),", Keyword::On),
        ("Only", Keyword::Only),
        ("Or),", Keyword::Or),
        ("Order", Keyword::Order),
        ("Overlaps", Keyword::Overlaps),
        ("Placing", Keyword::Placing),
        ("Primary", Keyword::Primary),
        ("References", Keyword::References),
        ("Returning", Keyword::Returning),
        ("Right", Keyword::Right),
        ("Select", Keyword::Select),
        ("Session_User", Keyword::Session_User),
        ("Similar", Keyword::Similar),
        ("Some", Keyword::Some),
        ("Symmetric", Keyword::Symmetric),
        ("Table", Keyword::Table),
        ("Tablesample", Keyword::Tablesample),
        ("Then", Keyword::Then),
        ("To),", Keyword::To),
        ("Trailing", Keyword::Trailing),
        ("True", Keyword::True),
        ("Union", Keyword::Union),
        ("Unique", Keyword::Unique),
        ("User", Keyword::User),
        ("Using", Keyword::Using),
        ("Variadic", Keyword::Variadic),
        ("Verbose", Keyword::Verbose),
        ("When", Keyword::When),
        ("Where", Keyword::Where),
        ("Window", Keyword::Window),
        ("With", Keyword::With),
    ]);
}

#[test]
fn scan_ident() {
    let mut lexer = Lexer::new("_qwe");
    assert_eq!(lexer.next(), Some(Ok(Token::Ident("_qwe"))));

    let mut lexer = Lexer::new("$qwe");
    assert_eq!(
        lexer.next(),
        Some(Err(ParserError::Unexpected(ErrorType::Ident((0, 3)))))
    );
}

#[test]
fn scan_sql_text() {
    use alloc::vec;
    let mut lexer = Lexer::new("select * from a");
    assert_eq!(
        lexer.collect::<Vec<Result<Token<'_>, ParserError>>>(),
        vec![
            Ok(Token::Keyword(Keyword::Select)),
            Ok(Token::Mul),
            Ok(Token::Keyword(Keyword::From)),
            Ok(Token::Ident("a")),
        ]
    );
}