#[macro_use]
extern crate log;
extern crate byteorder;

mod consts;
mod loader;

pub use consts::*;
pub use loader::{Error, UniversalFile};
