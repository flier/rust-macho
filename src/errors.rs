use std::error;
use std::fmt;
use std::io;
use std::num;
use std::result;
use std::str;
use std::string;

use uuid;
use time;

#[derive(Debug)]
pub enum Error {
    Utf8Error(str::Utf8Error),
    FromUtf8Error(string::FromUtf8Error),
    UuidParseError(::uuid::ParseError),
    IoError(io::Error),
    TimeParseError(time::ParseError),
    ParseIntError(num::ParseIntError),
    ParseOctalError(String),
    LoadError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Utf8Error(ref err) => write!(f, "utf-8 error: {}", err),
            Error::FromUtf8Error(ref err) => write!(f, "utf-8 error: {}", err),
            Error::UuidParseError(ref err) => write!(f, "uuid parse error: {}", err),
            Error::IoError(ref err) => write!(f, "io error: {}", err),
            Error::TimeParseError(ref err) => write!(f, "parse time error: {}", err),
            Error::ParseIntError(ref err) => write!(f, "parse int error: {}", err),
            Error::ParseOctalError(ref s) => write!(f, "parse octal error: {}", s),
            Error::LoadError(ref reason) => write!(f, "load error: {}", reason),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Utf8Error(ref err) => err.description(),
            Error::FromUtf8Error(ref err) => err.description(),
            Error::UuidParseError(_) => "parse uuid failed",
            Error::IoError(ref err) => err.description(),
            Error::TimeParseError(ref err) => err.description(),
            Error::ParseIntError(ref err) => err.description(),
            Error::ParseOctalError(_) => "parse octal error",
            Error::LoadError(_) => "load mach-o file failed",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Utf8Error(ref err) => Some(err),
            Error::FromUtf8Error(ref err) => Some(err),
            Error::IoError(ref err) => Some(err),
            Error::TimeParseError(ref err) => Some(err),
            Error::ParseIntError(ref err) => Some(err),
            _ => None,
        }
    }
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

impl From<num::ParseIntError> for Error {
    fn from(err: num::ParseIntError) -> Self {
        Error::ParseIntError(err)
    }
}

pub type Result<T> = result::Result<T, Error>;
