use std::io;
use std::num;
use std::str;
use std::string;

use anyhow::Error;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum MachError {
    #[error("fail to interpret a sequence of u8 as a string, {}.", _0)]
    Utf8Error(#[from] str::Utf8Error),
    #[error("fail to convert a String from a UTF-8 byte vector, {}.", _0)]
    FromUtf8Error(#[from] string::FromUtf8Error),
    #[error("fail to parse UUID, {}.", _0)]
    UuidError(::uuid::Error),
    #[error("fail to do I/O operations, {}.", _0)]
    IoError(#[from] io::Error),
    #[error("fail to parse time, {}.", _0)]
    TimeParseError(#[from] time::ParseError),
    #[error("fail to parse integer, {}.", _0)]
    ParseIntError(#[from] num::ParseIntError),
    #[error("fail to parse octal, {}.", _0)]
    ParseOctalError(String),
    #[error("unknown file format, magic 0x{:08x}.", _0)]
    UnknownMagic(u32),
    #[error("unknown symbol type, {}.", _0)]
    UnknownSymType(u8),
    #[error("offset {} out of range: [0..{}).", _0, _1)]
    OutOfRange(usize, usize),
    #[error("number overflowing.")]
    NumberOverflow,
    #[error("buffer overflowing, {}.", _0)]
    BufferOverflow(usize),
}


impl From<uuid::Error> for MachError {
    fn from(err: uuid::Error) -> Self {
        MachError::UuidError(err)
    }
}


pub type Result<T> = ::std::result::Result<T, Error>;
