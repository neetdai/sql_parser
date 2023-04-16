use alloc::string::String;

#[derive(Debug, PartialEq)]
pub enum Error {
    Unexpected(String),
}
