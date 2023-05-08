#[cfg(feature = "postgresql")]
use crate::postgresql::*;

use crate::error::{Error as ParserError, ErrorType};

pub fn parse(sql_text: &str) -> Result<Statement<'_>, ParserError<'_>> {
    let mut lexer = Lexer::new(sql_text); // Lexer for the given SQL text.
    let statement = Statement::new(lexer);

    statement
}
