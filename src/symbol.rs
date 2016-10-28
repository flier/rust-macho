use std::fmt;
use std::io::{Cursor, Seek, SeekFrom};

use byteorder::{ByteOrder, ReadBytesExt, LittleEndian, BigEndian};

use errors::*;
use commands::*;
use loader::*;

#[derive(Debug)]
pub enum Symbol {
    Undefined { external: bool },
    Absolute { external: bool, entry: usize },
    Defined {
        external: bool,
        section: u8,
        entry: usize,
    },
    Prebound { external: bool },
    Indirect { external: bool, name: usize },
}

pub struct SymbolIter<'a> {
    cur: &'a mut Cursor<&'a [u8]>,
    nsyms: u32,
    is_bigend: bool,
    is_64bit: bool,
}

impl<'a> SymbolIter<'a> {
    fn parse(&mut self) -> Result<Symbol> {
        if self.is_bigend {
            Symbol::parse::<BigEndian>(self.cur, self.is_64bit)
        } else {
            Symbol::parse::<LittleEndian>(self.cur, self.is_64bit)
        }
    }
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = Symbol;

    fn next(&mut self) -> Option<Symbol> {
        if self.nsyms > 0 {
            if let Ok(symbol) = self.parse() {
                self.nsyms -= 1;

                return Some(symbol);
            }
        }

        None
    }
}

pub trait SymbolProvider {
    fn symbols<'a>(&self, cur: &'a mut Cursor<&'a [u8]>) -> Option<SymbolIter<'a>>;
}

impl SymbolProvider for OFile {
    fn symbols<'a>(&self, cur: &'a mut Cursor<&'a [u8]>) -> Option<SymbolIter<'a>> {
        if let &OFile::MachFile { ref header, ref commands } = self {
            for cmd in commands {
                let &MachCommand(ref cmd, _) = cmd;

                if let &LoadCommand::SymTab { symoff, nsyms, stroff, strsize } = cmd {
                    if let Ok(_) = cur.seek(SeekFrom::Start(symoff as u64)) {
                        return Some(SymbolIter {
                            cur: cur,
                            nsyms: nsyms,
                            is_bigend: header.is_bigend(),
                            is_64bit: header.is_64bit(),
                        });
                    }
                }
            }
        }

        None
    }
}

impl Symbol {
    pub fn parse<O: ByteOrder>(cur: &mut Cursor<&[u8]>, is_64bit: bool) -> Result<Symbol> {
        let strx = cur.read_u32::<O>();
        let flags = try!(cur.read_u8());
        let sect = try!(cur.read_u8());
        let desc = cur.read_u16::<O>();
        let value = if is_64bit {
            try!(cur.read_u64::<O>()) as usize
        } else {
            try!(cur.read_u32::<O>()) as usize
        };

        let external = (flags & N_EXT) == N_EXT;

        let typ = flags & N_TYPE;

        match typ {
            N_UNDF => Ok(Symbol::Undefined { external: external }),
            N_ABS => {
                Ok(Symbol::Absolute {
                    external: external,
                    entry: value,
                })
            }
            N_SECT => {
                Ok(Symbol::Defined {
                    external: external,
                    section: sect,
                    entry: value,
                })
            }
            N_PBUD => Ok(Symbol::Prebound { external: external }),
            N_INDR => {
                Ok(Symbol::Indirect {
                    external: external,
                    name: value,
                })
            }
            _ => Err(Error::LoadError(format!("unknown symbol type 0x{:x}", typ))),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Symbol::Undefined { external } => {
                write!(f, "                 {}", if external { "U" } else { "u" })
            }
            &Symbol::Absolute { external, entry } => {
                write!(f, "{:016x} {}", entry, if external { "A" } else { "a" })
            }
            &Symbol::Defined { external, section, entry } => {
                write!(f, "{:016x} {}", entry, if external { "D" } else { "d" })
            }
            &Symbol::Prebound { external } => {
                write!(f, "                 {}", if external { "U" } else { "u" })
            }
            &Symbol::Indirect { external, ref name } => {
                write!(f, "                 {}", if external { "I" } else { "i" })
            }
        }
    }
}

// The n_type field really contains four fields:
//  unsigned char N_STAB:3,
//            N_PEXT:1,
//            N_TYPE:3,
//            N_EXT:1;
// which are used via the following masks.
//
const N_STAB: u8 = 0xe0;  /* if any of these bits set, a symbolic debugging entry */
const N_PEXT: u8 = 0x10;  /* private external symbol bit */
const N_TYPE: u8 = 0x0e;  /* mask for the type bits */
const N_EXT: u8 = 0x01;  /* external symbol bit, set for external symbols */



// Values for N_TYPE bits of the n_type field.
//
const N_UNDF: u8 = 0x0;    /* undefined, n_sect == NO_SECT */
const N_ABS: u8 = 0x2;    /* absolute, n_sect == NO_SECT */
const N_SECT: u8 = 0xe;    /* defined in section number n_sect */
const N_PBUD: u8 = 0xc;    /* prebound undefined (defined in a dylib) */
const N_INDR: u8 = 0xa;    /* indirect */
