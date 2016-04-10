use std::str;
use std::string;
use std::io;
use std::result;

use uuid;
use time;

#[derive(Debug)]
pub enum Error {
    Utf8Error(str::Utf8Error),
    FromUtf8Error(string::FromUtf8Error),
    UuidParseError(::uuid::ParseError),
    IoError(io::Error),
    TimeParseError(::time::ParseError),
    LoadError(String),
}

impl From<str::Utf8Error> for Error {
    fn from(err: str::Utf8Error) -> Self {
        Error::Utf8Error(err)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self {
        Error::FromUtf8Error(err)
    }
}

impl From<uuid::ParseError> for Error {
    fn from(err: uuid::ParseError) -> Self {
        Error::UuidParseError(err)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IoError(err)
    }
}

impl From<time::ParseError> for Error {
    fn from(err: time::ParseError) -> Self {
        Error::TimeParseError(err)
    }
}

pub type Result<T> = result::Result<T, Error>;
