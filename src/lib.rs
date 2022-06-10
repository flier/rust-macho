//! Mach-O File Format Parser for Rust
//!
//! # Examples
//!
//! ```
//! use std::io::{Read, Cursor};
//! use std::fs::File;
//! use mach_object::{OFile, CPU_TYPE_X86_64, MachCommand, LoadCommand};
//!
//! let mut f = File::open("tests/helloworld").unwrap();
//! let mut buf = Vec::new();
//! let size = f.read_to_end(&mut buf).unwrap();
//! let mut cur = Cursor::new(&buf[..size]);
//! if let OFile::MachFile { ref header, ref commands } = OFile::parse(&mut cur).unwrap() {
//!     assert_eq!(header.cputype, CPU_TYPE_X86_64);
//!     assert_eq!(header.ncmds as usize, commands.len());
//!     for &MachCommand(ref cmd, cmdsize) in commands {
//!         if let &LoadCommand::Segment64 { ref segname, ref sections, .. } = cmd {
//!             println!("segment: {}", segname);
//!
//!             for ref sect in sections {
//!                 println!("  section: {}", sect.sectname);
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! For more detail, please check the unit tests
//! and the [otool](https://github.com/flier/rust-macho/blob/master/examples/otool.rs) example.
//!
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

mod commands;
mod consts;
#[cfg(feature = "display")]
mod display;
mod errors;
mod export;
mod loader;
mod opcode;
mod symbol;

pub use crate::commands::*;
pub use crate::consts::*;
pub use crate::errors::Error as MachError;
pub use crate::export::{ExportKind, ExportSymbol, ExportTrie, ExportType};
pub use crate::loader::{ArHeader, CheckedSlice, FatArch, FatHeader, MachCommand, MachHeader, OFile, RanLib};
pub use crate::opcode::{
    Bind, BindOpCode, BindOpCodes, BindSymbol, BindSymbolFlags, BindSymbolType, LazyBind, LazyBindSymbol, Rebase,
    RebaseOpCode, RebaseOpCodes, RebaseSymbol, WeakBind, WeakBindSymbol,
};
pub use crate::symbol::{Symbol, SymbolIter, SymbolReference};
