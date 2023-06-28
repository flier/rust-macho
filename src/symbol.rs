use std::io::Cursor;
use std::rc::Rc;
use std::str;

use byteorder::{BigEndian, ByteOrder, LittleEndian, ReadBytesExt};

use crate::{
    commands::Section,
    consts::*,
    errors::{Error::*, Result},
};

/// the link-edit 4.3BSD "stab" style symbol
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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

impl<'a> Symbol<'a> {
    pub fn name(&self) -> Option<&str> {
        match *self {
            Symbol::Undefined { name, .. }
            | Symbol::Absolute { name, .. }
            | Symbol::Defined { name, .. }
            | Symbol::Prebound { name, .. }
            | Symbol::Indirect { name, .. }
            | Symbol::Debug { name, .. } => name,
        }
    }

    pub fn is_external(&self) -> bool {
        match *self {
            Symbol::Undefined { external, .. }
            | Symbol::Absolute { external, .. }
            | Symbol::Defined { external, .. }
            | Symbol::Prebound { external, .. }
            | Symbol::Indirect { external, .. } => external,
            _ => false,
        }
    }
}

/// Reference type and flags of symbol
pub trait SymbolReference {
    /// raw `desc` value
    fn desc(&self) -> u16;

    /// types of references
    fn ref_type(&self) -> u8 {
        self.desc() as u8 & REFERENCE_TYPE
    }

    /// To simplify stripping of objects that use are used with the dynamic link
    /// editor, the static link editor marks the symbols defined an object that are
    /// referenced by a dynamicly bound object (dynamic shared libraries, bundles).
    /// With this marking strip knows not to strip these symbols.
    fn is_ref_dyn(&self) -> bool {
        (self.desc() & REFERENCED_DYNAMICALLY) == REFERENCED_DYNAMICALLY
    }

    /// The ordinal recorded references the libraries listed in the Mach-O file
    fn lib_ordinal(&self) -> u8 {
        ((self.desc() >> 8) & 0xff) as u8
    }

    /// symbol is not to be dead stripped
    fn is_no_dead_strip(&self) -> bool {
        (self.desc() & N_NO_DEAD_STRIP) == N_NO_DEAD_STRIP
    }

    /// symbol is discarded
    fn is_discarded(&self) -> bool {
        (self.desc() & N_DESC_DISCARDED) == N_DESC_DISCARDED
    }

    /// symbol is weak referenced
    fn is_weak_ref(&self) -> bool {
        (self.desc() & N_WEAK_REF) == N_WEAK_REF
    }

    /// coalesed symbol is a weak definition
    fn is_weak_def(&self) -> bool {
        (self.desc() & N_WEAK_DEF) == N_WEAK_DEF
    }

    /// reference to a weak symbol
    fn is_ref_to_weak(&self) -> bool {
        (self.desc() & N_REF_TO_WEAK) == N_REF_TO_WEAK
    }

    /// symbol is a Thumb function (ARM)
    fn is_arm_thumb_def(&self) -> bool {
        (self.desc() & N_ARM_THUMB_DEF) == N_ARM_THUMB_DEF
    }

    /// the function is actually a resolver function and
    /// should be called to get the address of the real function to use.
    fn is_resolver(&self) -> bool {
        (self.desc() & N_SYMBOL_RESOLVER) == N_SYMBOL_RESOLVER
    }

    /// symbol is pinned to the previous content.
    fn is_alt_entry(&self) -> bool {
        (self.desc() & N_ALT_ENTRY) == N_ALT_ENTRY
    }
}

impl<'a> SymbolReference for Symbol<'a> {
    fn desc(&self) -> u16 {
        match *self {
            Symbol::Undefined { desc, .. }
            | Symbol::Absolute { desc, .. }
            | Symbol::Defined { desc, .. }
            | Symbol::Prebound { desc, .. }
            | Symbol::Indirect { desc, .. }
            | Symbol::Debug { desc, .. } => desc,
        }
    }
}

/// `Symbol` Iter
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
    pub fn new(
        cur: &'a mut Cursor<&'a [u8]>,
        sections: Vec<Rc<Section>>,
        nsyms: u32,
        stroff: u32,
        strsize: u32,
        is_bigend: bool,
        is_64bit: bool,
    ) -> Self {
        Self {
            cur,
            sections,
            nsyms,
            stroff,
            strsize,
            is_bigend,
            is_64bit,
        }
    }

    fn parse(&mut self) -> Result<Symbol<'a>> {
        if self.is_bigend {
            self.parse_symbol::<BigEndian>()
        } else {
            self.parse_symbol::<LittleEndian>()
        }
    }

    pub fn parse_symbol<O: ByteOrder>(&mut self) -> Result<Symbol<'a>> {
        let strx = self.cur.read_u32::<O>()? as usize;
        let flags = self.cur.read_u8()?;
        let sect = self.cur.read_u8()?;
        let desc = self.cur.read_u16::<O>()?;
        let value = if self.is_64bit {
            self.cur.read_u64::<O>()? as usize
        } else {
            self.cur.read_u32::<O>()? as usize
        };

        if (flags & N_STAB) != 0 {
            Ok(Symbol::Debug {
                name: self.load_str(strx)?,
                section: if sect == NO_SECT {
                    None
                } else {
                    self.sections.get((sect - 1) as usize).cloned()
                },
                desc,
                addr: value,
            })
        } else {
            let external = (flags & N_EXT) == N_EXT;

            let typ = flags & N_TYPE;

            match typ {
                N_UNDF => Ok(Symbol::Undefined {
                    name: self.load_str(strx)?,
                    external,
                    desc,
                }),
                N_ABS => Ok(Symbol::Absolute {
                    name: self.load_str(strx)?,
                    external,
                    desc,
                    entry: value,
                }),
                N_SECT => Ok(Symbol::Defined {
                    name: self.load_str(strx)?,
                    external,
                    section: if sect == NO_SECT {
                        None
                    } else {
                        self.sections.get((sect - 1) as usize).cloned()
                    },
                    desc,
                    entry: value,
                }),
                N_PBUD => Ok(Symbol::Prebound {
                    name: self.load_str(strx)?,
                    external,
                    desc,
                }),
                N_INDR => Ok(Symbol::Indirect {
                    name: self.load_str(strx)?,
                    external,
                    desc,
                    symbol: self.load_str(value)?,
                }),
                _ => Err(UnknownSymType(typ)),
            }
        }
    }

    fn load_str(&mut self, off: usize) -> Result<Option<&'a str>> {
        if off == 0 {
            Ok(None)
        } else if off >= self.strsize as usize {
            Err(OutOfRange(off, 0..self.strsize as usize))
        } else {
            let buf = *self.cur.get_ref();
            if let Some(s) = buf[self.stroff as usize + off as usize..].split(|x| *x == 0).next() {
                Ok(Some(str::from_utf8(s)?))
            } else {
                Ok(None)
            }
        }
    }
}

#[cfg(feature = "serde")]
impl<'a> serde::Serialize for SymbolIter<'a> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut cursor = self.cur.clone();
        let mut this = SymbolIter {
            cur: &mut cursor,
            sections: self.sections.clone(),
            nsyms: self.nsyms.clone(),
            stroff: self.stroff.clone(),
            strsize: self.stroff.clone(),
            is_bigend: self.is_bigend,
            is_64bit: self.is_64bit,
        };
        let res = serializer.collect_seq(this);

        res
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

// The n_type field really contains four fields:
//  unsigned char N_STAB:3,
//            N_PEXT:1,
//            N_TYPE:3,
//            N_EXT:1;
// which are used via the following masks.
//
const N_STAB: u8 = 0xe0; /* if any of these bits set, a symbolic debugging entry */
#[allow(dead_code)]
const N_PEXT: u8 = 0x10; /* private external symbol bit */
const N_TYPE: u8 = 0x0e; /* mask for the type bits */
const N_EXT: u8 = 0x01; /* external symbol bit, set for external symbols */

// Values for N_TYPE bits of the n_type field.
//
const N_UNDF: u8 = 0x0; /* undefined, n_sect == NO_SECT */
const N_ABS: u8 = 0x2; /* absolute, n_sect == NO_SECT */
const N_SECT: u8 = 0xe; /* defined in section number n_sect */
const N_PBUD: u8 = 0xc; /* prebound undefined (defined in a dylib) */
const N_INDR: u8 = 0xa; /* indirect */

const NO_SECT: u8 = 0; /* symbol is not in any section */
