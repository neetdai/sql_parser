#![no_std]
#![forbid(unsafe_code)]
extern crate alloc;

mod error;
mod parser;

#[cfg(feature = "postgresql")]
mod postgresql;
