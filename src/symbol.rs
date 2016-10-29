use std::str;
use std::fmt;
use std::rc::Rc;
use std::io::{Cursor, Seek, SeekFrom};

use byteorder::{ByteOrder, ReadBytesExt, LittleEndian, BigEndian};

use errors::*;
use consts::*;
use commands::{LoadCommand, Section};
use loader::{OFile, MachCommand};

#[derive(Debug)]
pub enum Symbol<'a> {
    Undefined {
        name: Option<&'a str>,
        external: bool,
        desc: u16,
    },
    Absolute {
        name: Option<&'a str>,
        external: bool,
        desc: u16,
        entry: usize,
    },
    Defined {
        name: Option<&'a str>,
        external: bool,
        section: Option<Rc<Section>>,
        desc: u16,
        entry: usize,
    },
    Prebound {
        name: Option<&'a str>,
        external: bool,
        desc: u16,
    },
    Indirect {
        name: Option<&'a str>,
        external: bool,
        desc: u16,
        symbol: Option<&'a str>,
    },
    Debug {
        name: Option<&'a str>,
        section: Option<Rc<Section>>,
        desc: u16,
        addr: usize,
    },
}

impl<'a> fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Symbol::Undefined { ref name, external, .. } => {
                write!(f,
                       "                 {} {}",
                       if external { "U" } else { "u" },
                       name.unwrap_or(""))
            }
            &Symbol::Absolute { ref name, external, entry, .. } => {
                write!(f,
                       "{:016x} {} {}",
                       entry,
                       if external { "A" } else { "a" },
                       name.unwrap_or(""))
            }
            &Symbol::Defined { ref name, external, ref section, entry, .. } => {
                let mut symtype = "s";

                if let &Some(ref section) = section {
                    let Section { ref sectname, ref segname, .. } = **section;

                    if segname == SEG_TEXT && sectname == SECT_TEXT {
                        symtype = "t"
                    } else if segname == SEG_DATA {
                        if sectname == SECT_DATA {
                            symtype = "d"
                        } else if sectname == SECT_BSS {
                            symtype = "b"
                        } else if sectname == SECT_COMMON {
                            symtype = "c"
                        }
                    }
                }

                write!(f,
                       "{:016x} {} {}",
                       entry,
                       if external {
                           symtype.to_uppercase()
                       } else {
                           symtype.to_lowercase()
                       },
                       name.unwrap_or(""))
            }
            &Symbol::Prebound { ref name, external, .. } => {
                write!(f,
                       "                 {} {}",
                       if external { "P" } else { "p" },
                       name.unwrap_or(""))
            }
            &Symbol::Indirect { ref name, external, .. } => {
                write!(f,
                       "                 {} {}",
                       if external { "I" } else { "i" },
                       name.unwrap_or(""))
            }
            &Symbol::Debug { ref name, addr, .. } => {
                if addr == 0 {
                    write!(f, "                 d {}", name.unwrap_or(""))
                } else {
                    write!(f, "{:016x} d {}", addr, name.unwrap_or(""))
                }
            }
        }
    }
}

pub struct SymbolIter<'a> {
    cur: &'a mut Cursor<&'a [u8]>,
    sections: Vec<Rc<Section>>,
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

        if (flags & N_STAB) != 0 {
            Ok(Symbol::Debug {
                name: try!(self.load_str(strx)),
                section: if sect == NO_SECT {
                    None
                } else {
                    Some(self.sections[(sect - 1) as usize].clone())
                },
                desc: desc,
                addr: value,
            })
        } else {
            let external = (flags & N_EXT) == N_EXT;

            let typ = flags & N_TYPE;

            match typ {
                N_UNDF => {
                    Ok(Symbol::Undefined {
                        name: try!(self.load_str(strx)),
                        external: external,
                        desc: desc,
                    })
                }
                N_ABS => {
                    Ok(Symbol::Absolute {
                        name: try!(self.load_str(strx)),
                        external: external,
                        desc: desc,
                        entry: value,
                    })
                }
                N_SECT => {
                    Ok(Symbol::Defined {
                        name: try!(self.load_str(strx)),
                        external: external,
                        section: if sect == NO_SECT {
                            None
                        } else {
                            Some(self.sections[(sect - 1) as usize].clone())
                        },
                        desc: desc,
                        entry: value,
                    })
                }
                N_PBUD => {
                    Ok(Symbol::Prebound {
                        name: try!(self.load_str(strx)),
                        external: external,
                        desc: desc,
                    })
                }
                N_INDR => {
                    Ok(Symbol::Indirect {
                        name: try!(self.load_str(strx)),
                        external: external,
                        desc: desc,
                        symbol: try!(self.load_str(value)),
                    })
                }
                _ => Err(Error::LoadError(format!("unknown symbol type 0x{:x}", typ))),
            }
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
            let sections = commands.iter()
                .filter_map(|cmd| match cmd.0 {
                    LoadCommand::Segment { ref sections, .. } |
                    LoadCommand::Segment64 { ref sections, .. } => Some(sections),
                    _ => None,
                })
                .flat_map(|sections| sections.clone())
                .collect();

            for cmd in commands {
                let &MachCommand(ref cmd, _) = cmd;

                if let &LoadCommand::SymTab { symoff, nsyms, stroff, strsize } = cmd {
                    if let Ok(_) = cur.seek(SeekFrom::Start(symoff as u64)) {
                        return Some(SymbolIter {
                            cur: cur,
                            sections: sections,
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
#[allow(dead_code)]
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

const NO_SECT: u8 = 0;   /* symbol is not in any section */
