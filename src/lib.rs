//! Mach-O File Format Parser for Rust
//!
//! # Examples
//!
//! ```
//! use std::io::{Read, Cursor};
//! use std::fs::File;
//! use mach_object::{OFile, CPU_TYPE_X86_64, MachCommand, LoadCommand};
//!
//! let mut f = File::open("test/helloworld").unwrap();
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
extern crate byteorder;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate libc;
#[macro_use]
extern crate log;
extern crate time;
extern crate uuid;

#[cfg(test)]
extern crate diff;
#[cfg(test)]
extern crate pretty_env_logger;

mod consts;
mod errors;
mod commands;
mod opcode;
mod loader;
mod symbol;

pub use consts::*;
pub use commands::*;
pub use loader::{ArHeader, FatArch, FatHeader, MachCommand, MachHeader, OFile, RanLib};
pub use symbol::{Symbol, SymbolIter, SymbolReader, SymbolReference};
pub use opcode::{Bind, BindOpCode, BindOpCodes, BindSymbol, BindSymbolFlags, LazyBind, LazyBindSymbol, Rebase,
                 RebaseOpCode, RebaseOpCodes, RebaseSymbol, SymbolType, WeakBind, WeakBindSymbol};
