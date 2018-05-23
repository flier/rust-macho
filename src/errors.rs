use std::io;
use std::num;
use std::str;
use std::string;

use failure::Error;
use time;
use uuid;

#[derive(Debug, Fail)]
pub enum MachError {
    #[fail(display = "fail to interpret a sequence of u8 as a string, {}.", _0)]
    Utf8Error(#[cause] str::Utf8Error),
    #[fail(display = "fail to convert a String from a UTF-8 byte vector, {}.", _0)]
    FromUtf8Error(#[cause] string::FromUtf8Error),
    #[fail(display = "fail to parse UUID, {}.", _0)]
    UuidParseError(::uuid::ParseError),
    #[fail(display = "fail to do I/O operations, {}.", _0)]
    IoError(#[cause] io::Error),
    #[fail(display = "fail to parse time, {}.", _0)]
    TimeParseError(#[cause] time::ParseError),
    #[fail(display = "fail to parse integer, {}.", _0)]
    ParseIntError(#[cause] num::ParseIntError),
    #[fail(display = "fail to parse octal, {}.", _0)]
    ParseOctalError(String),
    #[fail(display = "unknown file format, magic 0x{:08x}.", _0)]
    UnknownMagic(u32),
    #[fail(display = "unknown symbol type, {}.", _0)]
    UnknownSymType(u8),
    #[fail(display = "offset {} out of range: [0..{}).", _0, _1)]
    OutOfRange(usize, usize),
    #[fail(display = "number overflowing.")]
    NumberOverflow,
    #[fail(display = "buffer overflowing, {}.", _0)]
    BufferOverflow(usize),
}

impl From<str::Utf8Error> for MachError {
    fn from(err: str::Utf8Error) -> Self {
        MachError::Utf8Error(err)
    }
}

impl From<string::FromUtf8Error> for MachError {
    fn from(err: string::FromUtf8Error) -> Self {
        MachError::FromUtf8Error(err)
    }
}

impl From<uuid::ParseError> for MachError {
    fn from(err: uuid::ParseError) -> Self {
        MachError::UuidParseError(err)
    }
}

impl From<io::Error> for MachError {
    fn from(err: io::Error) -> Self {
        MachError::IoError(err)
    }
}

impl From<time::ParseError> for MachError {
    fn from(err: time::ParseError) -> Self {
        MachError::TimeParseError(err)
    }
}

impl From<num::ParseIntError> for MachError {
    fn from(err: num::ParseIntError) -> Self {
        MachError::ParseIntError(err)
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;
