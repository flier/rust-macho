use std::fmt;
use std::ffi::CStr;
use std::io::{BufRead, Cursor, Seek, SeekFrom};

use byteorder::{ByteOrder, ReadBytesExt, LittleEndian, BigEndian};

use errors::*;
use commands::*;
use loader::*;

#[derive(Debug)]
pub enum Symbol {
    Undefined { name: String, external: bool },
    Absolute {
        name: String,
        external: bool,
        entry: usize,
    },
    Defined {
        name: String,
        external: bool,
        section: u8,
        entry: usize,
    },
    Prebound { name: String, external: bool },
    Indirect {
        name: String,
        external: bool,
        refsym: String,
    },
}

pub struct SymbolIter<'a> {
    cur: &'a mut Cursor<&'a [u8]>,
    nsyms: u32,
    stroff: u32,
    strsize: u32,
    is_bigend: bool,
    is_64bit: bool,
}

impl<'a> SymbolIter<'a> {
    fn parse(&mut self) -> Result<Symbol> {
        if self.is_bigend {
            self.parse_symbol::<BigEndian>()
        } else {
            self.parse_symbol::<LittleEndian>()
        }
    }

    pub fn parse_symbol<O: ByteOrder>(&mut self) -> Result<Symbol> {
        let strx = try!(self.cur.read_u32::<O>()) as usize;
        let flags = try!(self.cur.read_u8());
        let sect = try!(self.cur.read_u8());
        let desc = try!(self.cur.read_u16::<O>());
        let value = if self.is_64bit {
            try!(self.cur.read_u64::<O>()) as usize
        } else {
            try!(self.cur.read_u32::<O>()) as usize
        };

        let external = (flags & N_EXT) == N_EXT;

        let typ = flags & N_TYPE;

        match typ {
            N_UNDF => {
                Ok(Symbol::Undefined {
                    name: try!(self.read_string(strx)),
                    external: external,
                })
            }
            N_ABS => {
                Ok(Symbol::Absolute {
                    name: try!(self.read_string(strx)),
                    external: external,
                    entry: value,
                })
            }
            N_SECT => {
                Ok(Symbol::Defined {
                    name: try!(self.read_string(strx)),
                    external: external,
                    section: sect,
                    entry: value,
                })
            }
            N_PBUD => {
                Ok(Symbol::Prebound {
                    name: try!(self.read_string(strx)),
                    external: external,
                })
            }
            N_INDR => {
                Ok(Symbol::Indirect {
                    name: try!(self.read_string(strx)),
                    external: external,
                    refsym: try!(self.read_string(value)),
                })
            }
            _ => Err(Error::LoadError(format!("unknown symbol type 0x{:x}", typ))),
        }
    }

    fn read_string(&mut self, off: usize) -> Result<String> {
        if off == 0 {
            Ok(String::new())
        } else if off >= self.strsize as usize {
            Err(Error::LoadError(format!("string offset out of range [..{})", self.strsize)))
        } else {
            let mut cur = self.cur.clone();

            try!(cur.seek(SeekFrom::Start(self.stroff as u64 + off as u64)));

            let mut buf = Vec::new();

            try!(cur.read_until(0, &mut buf));

            let s = unsafe { try!(CStr::from_ptr(buf.as_ptr() as *const i8).to_str()) };

            Ok(String::from(s))
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
                            stroff: stroff,
                            strsize: strsize,
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

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Symbol::Undefined { ref name, external } => {
                write!(f,
                       "                 {} {}",
                       if external { "U" } else { "u" },
                       name)
            }
            &Symbol::Absolute { ref name, external, entry } => {
                write!(f,
                       "{:016x} {} {}",
                       entry,
                       if external { "A" } else { "a" },
                       name)
            }
            &Symbol::Defined { ref name, external, section, entry } => {
                write!(f,
                       "{:016x} {} {}",
                       entry,
                       if external { "D" } else { "d" },
                       name)
            }
            &Symbol::Prebound { ref name, external } => {
                write!(f,
                       "                 {} {}",
                       if external { "U" } else { "u" },
                       name)
            }
            &Symbol::Indirect { ref name, external, ref refsym } => {
                write!(f,
                       "                 {} {}",
                       if external { "I" } else { "i" },
                       name)
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
