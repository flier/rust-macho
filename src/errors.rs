use std::io;
use std::num;
use std::ops::Range;
use std::str;
use std::string;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Utf8Error(#[from] str::Utf8Error),
    #[error(transparent)]
    FromUtf8Error(#[from] string::FromUtf8Error),
    #[error(transparent)]
    UuidError(#[from] uuid::Error),
    #[error(transparent)]
    IoError(#[from] io::Error),
    #[cfg(feature = "display")]
    #[error(transparent)]
    TimeParseError(#[from] time::error::ComponentRange),
    #[error(transparent)]
    ParseIntError(#[from] num::ParseIntError),
    #[error("fail to parse octal.")]
    ParseOctalError(String),
    #[error("unknown file format, magic 0x{0}.")]
    UnknownMagic(u32),
    #[error("unknown symbol type, {0}.")]
    UnknownSymType(u8),
    #[error("offset {0} out of range: {1:?}.")]
    OutOfRange(usize, Range<usize>),
    #[error("number overflowing.")]
    NumberOverflow,
    #[error("buffer overflowing, {0}.")]
    BufferOverflow(usize),
    #[error("recursive export trie at offset {0}.")]
    RecursiveExportTrie(u64),
}

pub type Result<T> = ::std::result::Result<T, Error>;
