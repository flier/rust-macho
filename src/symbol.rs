use std::str;
use std::fmt;
use std::io::{BufRead, Cursor, Seek, SeekFrom};

use byteorder::{ByteOrder, ReadBytesExt, LittleEndian, BigEndian};

use errors::*;
use commands::LoadCommand;
use loader::{OFile, MachCommand};

#[derive(Debug)]
pub enum Symbol<'a> {
    Undefined {
        name: Option<&'a str>,
        external: bool,
    },
    Absolute {
        name: Option<&'a str>,
        external: bool,
        entry: usize,
    },
    Defined {
        name: Option<&'a str>,
        external: bool,
        section: u8,
        entry: usize,
    },
    Prebound {
        name: Option<&'a str>,
        external: bool,
    },
    Indirect {
        name: Option<&'a str>,
        external: bool,
        refsym: Option<&'a str>,
    },
}

impl<'a> fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Symbol::Undefined { ref name, external } => {
                write!(f,
                       "                 {} {}",
                       if external { "U" } else { "u" },
                       name.unwrap_or(""))
            }
            &Symbol::Absolute { ref name, external, entry } => {
                write!(f,
                       "{:016x} {} {}",
                       entry,
                       if external { "A" } else { "a" },
                       name.unwrap_or(""))
            }
            &Symbol::Defined { ref name, external, section, entry } => {
                write!(f,
                       "{:016x} {} {}",
                       entry,
                       if external { "D" } else { "d" },
                       name.unwrap_or(""))
            }
            &Symbol::Prebound { ref name, external } => {
                write!(f,
                       "                 {} {}",
                       if external { "U" } else { "u" },
                       name.unwrap_or(""))
            }
            &Symbol::Indirect { ref name, external, ref refsym } => {
                write!(f,
                       "                 {} {}",
                       if external { "I" } else { "i" },
                       name.unwrap_or(""))
            }
        }
    }
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
    fn parse(&mut self) -> Result<Symbol<'a>> {
        if self.is_bigend {
            self.parse_symbol::<BigEndian>()
        } else {
            self.parse_symbol::<LittleEndian>()
        }
    }

    pub fn parse_symbol<O: ByteOrder>(&mut self) -> Result<Symbol<'a>> {
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
                    name: try!(self.load_str(strx)),
                    external: external,
                })
            }
            N_ABS => {
                Ok(Symbol::Absolute {
                    name: try!(self.load_str(strx)),
                    external: external,
                    entry: value,
                })
            }
            N_SECT => {
                Ok(Symbol::Defined {
                    name: try!(self.load_str(strx)),
                    external: external,
                    section: sect,
                    entry: value,
                })
            }
            N_PBUD => {
                Ok(Symbol::Prebound {
                    name: try!(self.load_str(strx)),
                    external: external,
                })
            }
            N_INDR => {
                Ok(Symbol::Indirect {
                    name: try!(self.load_str(strx)),
                    external: external,
                    refsym: try!(self.load_str(value)),
                })
            }
            _ => Err(Error::LoadError(format!("unknown symbol type 0x{:x}", typ))),
        }
    }

    fn load_str(&mut self, off: usize) -> Result<Option<&'a str>> {
        if off == 0 {
            Ok(None)
        } else if off >= self.strsize as usize {
            Err(Error::LoadError(format!("string offset out of range [..{})", self.strsize)))
        } else {
            let buf = *self.cur.get_ref();
            let s = *&buf[self.stroff as usize + off as usize..]
                .split(|x| *x == 0)
                .next()
                .unwrap();

            Ok(Some(try!(str::from_utf8(s))))
        }
    }
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = Symbol<'a>;

    fn next(&mut self) -> Option<Symbol<'a>> {
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
