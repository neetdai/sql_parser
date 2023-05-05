#![no_std]
#![forbid(unsafe_code)]
extern crate alloc;

mod error;
mod parser;

#[cfg(feature = "postgresql")]
mod postgresql;

#[cfg(feature = "postgresql")]
pub use postgresql::*;

pub use error::{Error as ParserError, ErrorType};
pub use parser::parse;
