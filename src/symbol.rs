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
    },
    Absolute {
        name: Option<&'a str>,
        external: bool,
        entry: usize,
    },
    Defined {
        name: Option<&'a str>,
        external: bool,
        section: Option<Rc<Section>>,
        entry: usize,
    },
    Prebound {
        name: Option<&'a str>,
        external: bool,
    },
    Indirect {
        name: Option<&'a str>,
        external: bool,
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
            &Symbol::Defined { ref name, external, ref section, entry } => {
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
            &Symbol::Prebound { ref name, external } => {
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
                        section: if sect == NO_SECT {
                            None
                        } else {
                            Some(self.sections[(sect - 1) as usize].clone())
                        },
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
const MAX_SECT: u8 = 255; /* 1 thru 255 inclusive */


// To support the lazy binding of undefined symbols in the dynamic link-editor,
// the undefined symbols in the symbol table (the nlist structures) are marked
// with the indication if the undefined reference is a lazy reference or
// non-lazy reference.  If both a non-lazy reference and a lazy reference is
// made to the same symbol the non-lazy reference takes precedence.  A reference
// is lazy only when all references to that symbol are made through a symbol
// pointer in a lazy symbol pointer section.
//
// The implementation of marking nlist structures in the symbol table for
// undefined symbols will be to use some of the bits of the n_desc field as a
// reference type.  The mask REFERENCE_TYPE will be applied to the n_desc field
// of an nlist structure for an undefined symbol to determine the type of
// undefined reference (lazy or non-lazy).
//
// The constants for the REFERENCE FLAGS are propagated to the reference table
// in a shared library file.  In that case the constant for a defined symbol,
// REFERENCE_FLAG_DEFINED, is also used.
//
// Reference type bits of the n_desc field of undefined symbols
const REFERENCE_TYPE: u8 = 0x7;
// types of references
const REFERENCE_FLAG_UNDEFINED_NON_LAZY: u8 = 0;
const REFERENCE_FLAG_UNDEFINED_LAZY: u8 = 1;
const REFERENCE_FLAG_DEFINED: u8 = 2;
const REFERENCE_FLAG_PRIVATE_DEFINED: u8 = 3;
const REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY: u8 = 4;
const REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY: u8 = 5;

// To simplify stripping of objects that use are used with the dynamic link
// editor, the static link editor marks the symbols defined an object that are
// referenced by a dynamicly bound object (dynamic shared libraries, bundles).
// With this marking strip knows not to strip these symbols.
//
const REFERENCED_DYNAMICALLY: u8 = 0x0010;

// For images created by the static link editor with the -twolevel_namespace
// option in effect the flags field of the mach header is marked with
// MH_TWOLEVEL.  And the binding of the undefined references of the image are
// determined by the static link editor.  Which library an undefined symbol is
// bound to is recorded by the static linker in the high 8 bits of the n_desc
// field using the SET_LIBRARY_ORDINAL macro below.  The ordinal recorded
// references the libraries listed in the Mach-O's LC_LOAD_DYLIB,
// LC_LOAD_WEAK_DYLIB, LC_REEXPORT_DYLIB, LC_LOAD_UPWARD_DYLIB, and
// LC_LAZY_LOAD_DYLIB, etc. load commands in the order they appear in the
// headers.   The library ordinals start from 1.
// For a dynamic library that is built as a two-level namespace image the
// undefined references from module defined in another use the same nlist struct
// an in that case SELF_LIBRARY_ORDINAL is used as the library ordinal.  For
// defined symbols in all images they also must have the library ordinal set to
// SELF_LIBRARY_ORDINAL.  The EXECUTABLE_ORDINAL refers to the executable
// image for references from plugins that refer to the executable that loads
// them.
//
// The DYNAMIC_LOOKUP_ORDINAL is for undefined symbols in a two-level namespace
// image that are looked up by the dynamic linker with flat namespace semantics.
// This ordinal was added as a feature in Mac OS X 10.3 by reducing the
// value of MAX_LIBRARY_ORDINAL by one.  So it is legal for existing binaries
// or binaries built with older tools to have 0xfe (254) dynamic libraries.  In
// this case the ordinal value 0xfe (254) must be treated as a library ordinal
// for compatibility.
//
fn GET_LIBRARY_ORDINAL(desc: u16) -> u8 {
    ((desc >> 8) & 0xff) as u8
}
fn SET_LIBRARY_ORDINAL(desc: u16, ordinal: u8) -> u16 {
    ((desc & 0x00ff) | (((ordinal as u16) & 0xff) << 8))
}

const SELF_LIBRARY_ORDINAL: u8 = 0x0;
const MAX_LIBRARY_ORDINAL: u8 = 0xfd;
const DYNAMIC_LOOKUP_ORDINAL: u8 = 0xfe;
const EXECUTABLE_ORDINAL: u8 = 0xff;

// The bit 0x0020 of the n_desc field is used for two non-overlapping purposes
// and has two different symbolic names, N_NO_DEAD_STRIP and N_DESC_DISCARDED.
//

// The N_NO_DEAD_STRIP bit of the n_desc field only ever appears in a
// relocatable .o file (MH_OBJECT filetype). And is used to indicate to the
// static link editor it is never to dead strip the symbol.
//
const N_NO_DEAD_STRIP: u16 = 0x0020; /* symbol is not to be dead stripped */

// The N_DESC_DISCARDED bit of the n_desc field never appears in linked image.
// But is used in very rare cases by the dynamic link editor to mark an in
// memory symbol as discared and longer used for linking.
//
const N_DESC_DISCARDED: u16 = 0x0020; /* symbol is discarded */

// The N_WEAK_REF bit of the n_desc field indicates to the dynamic linker that
// the undefined symbol is allowed to be missing and is to have the address of
// zero when missing.
//
const N_WEAK_REF: u16 = 0x0040; /* symbol is weak referenced */

// The N_WEAK_DEF bit of the n_desc field indicates to the static and dynamic
// linkers that the symbol definition is weak, allowing a non-weak symbol to
// also be used which causes the weak definition to be discared.  Currently this
// is only supported for symbols in coalesed sections.
//
const N_WEAK_DEF: u16 = 0x0080; /* coalesed symbol is a weak definition */

// The N_REF_TO_WEAK bit of the n_desc field indicates to the dynamic linker
// that the undefined symbol should be resolved using flat namespace searching.
//
const N_REF_TO_WEAK: u16 = 0x0080; /* reference to a weak symbol */

// The N_ARM_THUMB_DEF bit of the n_desc field indicates that the symbol is
// a defintion of a Thumb function.
//
const N_ARM_THUMB_DEF: u16 = 0x0008; /* symbol is a Thumb function (ARM) */

// The N_SYMBOL_RESOLVER bit of the n_desc field indicates that the
// that the function is actually a resolver function and should
// be called to get the address of the real function to use.
// This bit is only available in .o files (MH_OBJECT filetype)
//
const N_SYMBOL_RESOLVER: u16 = 0x0100;

// The N_ALT_ENTRY bit of the n_desc field indicates that the
// symbol is pinned to the previous content.
//
const N_ALT_ENTRY: u16 = 0x0200;
