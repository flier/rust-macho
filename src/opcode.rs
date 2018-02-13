use std::fmt;
use std::slice;
use std::path::Path;
use std::collections::VecDeque;
use std::ops::Range;
use std::rc::Rc;

use consts::*;
use commands::{DyLib, LoadCommand, Section};
use errors::{MachError, Result};

/// Bind or rebase symbol type
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SymbolType {
    Pointer,
    TextAbsolute32,
    TextRelative32,
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                SymbolType::Pointer => "pointer",
                SymbolType::TextAbsolute32 => "text abs32",
                SymbolType::TextRelative32 => "text rel32",
            }
        )
    }
}

bitflags! {
    /// Flags for bind symbol
    pub struct BindSymbolFlags: u8 {
        const WEAK_IMPORT = BIND_SYMBOL_FLAGS_WEAK_IMPORT;
        const NON_WEAK_DEFINITION = BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION;
    }
}

/// OpCode for the binding symbol
#[derive(Clone, Debug, PartialEq)]
pub enum BindOpCode {
    Done,
    SetDyLibrary(isize),
    SetSymbol {
        name: String,
        flags: BindSymbolFlags,
    },
    SetSymbolType(SymbolType),
    SetAddend(usize),
    SetSegmentOffset {
        segment_index: u8,
        segment_offset: usize,
    },
    AddAddress {
        offset: isize,
    },
    Bind,
    BindAndAddAddress {
        offset: isize,
    },
    BindAndSkipping {
        times: usize,
        skip: usize,
    },
}

/// An iterator over the `BindOpCode`
pub struct BindOpCodes<'a> {
    iter: slice::Iter<'a, u8>,
    ptr_size: usize,
}

impl<'a> Iterator for BindOpCodes<'a> {
    type Item = BindOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .and_then(|b| match (b & BIND_OPCODE_MASK, b & BIND_IMMEDIATE_MASK) {
                (BIND_OPCODE_DONE, _) => Some(BindOpCode::Done),
                (BIND_OPCODE_SET_DYLIB_ORDINAL_IMM, library_ordinal) => {
                    Some(BindOpCode::SetDyLibrary(library_ordinal as isize))
                }
                (BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB, _) => self.iter.read_uleb128().ok().map(|library_ordinal| {
                    BindOpCode::SetDyLibrary(library_ordinal as isize)
                }),
                (BIND_OPCODE_SET_DYLIB_SPECIAL_IMM, library_type) => match library_type {
                    0 => Some(BindOpCode::SetDyLibrary(BIND_SPECIAL_DYLIB_SELF)),
                    0x0f => Some(BindOpCode::SetDyLibrary(BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE)),
                    0x0e => Some(BindOpCode::SetDyLibrary(BIND_SPECIAL_DYLIB_FLAT_LOOKUP)),
                    _ => {
                        warn!("unknown library type: 0x{:x}", library_type);

                        None
                    }
                },
                (BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM, flags) => {
                    let mut v = vec![];

                    while let Some(&b) = self.iter.next() {
                        if b == 0 {
                            break;
                        } else {
                            v.push(b);
                        }
                    }

                    String::from_utf8(v).ok().map(|name| {
                        BindOpCode::SetSymbol {
                            name,
                            flags: BindSymbolFlags::from_bits_truncate(flags),
                        }
                    })
                }
                (BIND_OPCODE_SET_TYPE_IMM, bind_type) => match bind_type {
                    BIND_TYPE_POINTER => Some(BindOpCode::SetSymbolType(SymbolType::Pointer)),
                    BIND_TYPE_TEXT_ABSOLUTE32 => Some(BindOpCode::SetSymbolType(SymbolType::TextAbsolute32)),
                    BIND_TYPE_TEXT_PCREL32 => Some(BindOpCode::SetSymbolType(SymbolType::TextRelative32)),
                    _ => {
                        warn!("unknown bind type, {}", bind_type);

                        None
                    }
                },
                (BIND_OPCODE_SET_ADDEND_SLEB, _) => self.iter
                    .read_uleb128()
                    .ok()
                    .map(|addend| BindOpCode::SetAddend(addend)),
                (BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => {
                    self.iter.read_uleb128().ok().map(|segment_offset| {
                        BindOpCode::SetSegmentOffset {
                            segment_index,
                            segment_offset,
                        }
                    })
                }
                (BIND_OPCODE_ADD_ADDR_ULEB, _) => self.iter.read_uleb128().ok().map(|offset| {
                    BindOpCode::AddAddress {
                        offset: offset as isize,
                    }
                }),
                (BIND_OPCODE_DO_BIND, _) => Some(BindOpCode::Bind),
                (BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB, _) => self.iter.read_uleb128().ok().map(|offset| {
                    BindOpCode::AddAddress {
                        offset: offset as isize,
                    }
                }),
                (BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED, count) => Some(BindOpCode::AddAddress {
                    offset: self.ptr_size as isize * count as isize,
                }),
                (BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB, _) => {
                    if let (Ok(times), Ok(skip)) = (self.iter.read_uleb128(), self.iter.read_uleb128()) {
                        Some(BindOpCode::BindAndSkipping { times, skip })
                    } else {
                        warn!("fail to read times and skip");

                        None
                    }
                }
                (opcode, immediate) => {
                    warn!(
                        "unknown bind opcode: {:x}, immediate = {}",
                        opcode,
                        immediate
                    );

                    None
                }
            })
    }
}

/// A stream of BIND opcodes to bind all binding symbols.
pub struct Bind<'a> {
    opcodes: BindOpCodes<'a>,
    symbol_builder: BindSymbolBuilder<'a>,
    symbols: VecDeque<BindSymbol<'a>>,
}

impl<'a> Bind<'a> {
    pub fn parse(payload: &'a [u8], commands: &'a [LoadCommand], ptr_size: usize) -> Self {
        Bind {
            opcodes: BindOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol_builder: BindSymbolBuilder::new(commands),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> BindOpCodes<'a> {
        self.opcodes
    }

    fn push_current_symbol(&mut self) {
        match self.symbol_builder.build_bind_symbol() {
            Ok(symbol) => {
                self.symbols.push_back(symbol);
            }
            Err(err) => {
                warn!("fail to create symbol, {}", err);
            }
        }
    }
}

impl<'a> Iterator for Bind<'a> {
    type Item = BindSymbol<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Bind OpCode: {:?}", opcode);

                match opcode {
                    BindOpCode::Done => {
                        break;
                    }
                    BindOpCode::SetDyLibrary(ordinal) => if let Err(err) = self.symbol_builder.set_dylib(ordinal) {
                        warn!("fail to set dylib to {}, {}", ordinal, err);

                        break;
                    },
                    BindOpCode::SetSymbol { name, flags } => {
                        self.symbol_builder.symbol_name = Some(name);
                        self.symbol_builder.symbol_flags = Some(flags);
                    }
                    BindOpCode::SetSymbolType(symbol_type) => {
                        self.symbol_builder.symbol_type = Some(symbol_type);
                    }
                    BindOpCode::SetAddend(addend) => {
                        self.symbol_builder.symbol_addend = addend;
                    }
                    BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol_builder.set_segment(segment_index as usize);
                        self.symbol_builder.symbol_offset = segment_offset as isize;
                    }
                    BindOpCode::AddAddress { offset } => {
                        self.symbol_builder.symbol_offset += offset;
                    }
                    BindOpCode::Bind => {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndAddAddress { offset } => {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += offset + self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndSkipping { times, skip } => for _ in 0..times {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += (skip + self.opcodes.ptr_size) as isize;
                    },
                }

                if !self.symbols.is_empty() {
                    break;
                }
            }

            self.symbols.pop_front()
        })
    }
}

/// The mach binding symbol information
#[derive(Clone, Debug, PartialEq)]
pub struct BindSymbol<'a> {
    pub dylib_name: &'a str,
    pub segment_name: &'a str,
    pub section_name: &'a str,
    pub name: String,
    pub flags: BindSymbolFlags,
    pub symbol_type: SymbolType,
    pub address: usize,
    pub addend: usize,
}

/// A stream of BIND opcodes to bind all weak binding symbols.
pub struct WeakBind<'a> {
    opcodes: BindOpCodes<'a>,
    symbol_builder: BindSymbolBuilder<'a>,
    symbols: VecDeque<WeakBindSymbol<'a>>,
}

impl<'a> WeakBind<'a> {
    pub fn parse(payload: &'a [u8], commands: &'a [LoadCommand], ptr_size: usize) -> Self {
        WeakBind {
            opcodes: BindOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol_builder: BindSymbolBuilder::new(commands),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> BindOpCodes<'a> {
        self.opcodes
    }

    fn push_current_symbol(&mut self) {
        match self.symbol_builder.build_weak_bind_symbol() {
            Ok(symbol) => {
                self.symbols.push_back(symbol);
            }
            Err(err) => {
                warn!("fail to create symbol, {}", err);
            }
        }
    }
}

impl<'a> Iterator for WeakBind<'a> {
    type Item = WeakBindSymbol<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Weak Bind OpCode: {:?}", opcode);

                match opcode {
                    BindOpCode::Done => {
                        break;
                    }
                    BindOpCode::SetSymbol { name, flags } => {
                        self.symbol_builder.symbol_name = Some(name);
                        self.symbol_builder.symbol_flags = Some(flags);
                    }
                    BindOpCode::SetSymbolType(symbol_type) => {
                        self.symbol_builder.symbol_type = Some(symbol_type);
                    }
                    BindOpCode::SetAddend(addend) => {
                        self.symbol_builder.symbol_addend = addend;
                    }
                    BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol_builder.set_segment(segment_index as usize);
                        self.symbol_builder.symbol_offset = segment_offset as isize;
                    }
                    BindOpCode::AddAddress { offset } => {
                        self.symbol_builder.symbol_offset += offset;
                    }
                    BindOpCode::Bind => {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndAddAddress { offset } => {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += offset + self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndSkipping { times, skip } => for _ in 0..times {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += (skip + self.opcodes.ptr_size) as isize;
                    },
                    _ => {
                        warn!("unexpected weak bind opcode: {:?}", opcode);

                        break;
                    }
                }

                if !self.symbols.is_empty() {
                    break;
                }
            }

            self.symbols.pop_front()
        })
    }
}

/// The mach weak binding symbol information
#[derive(Clone, Debug, PartialEq)]
pub struct WeakBindSymbol<'a> {
    pub segment_name: &'a str,
    pub section_name: &'a str,
    pub name: String,
    pub flags: BindSymbolFlags,
    pub symbol_type: SymbolType,
    pub address: usize,
    pub addend: usize,
}

/// A stream of BIND opcodes to bind all lazy symbols.
pub struct LazyBind<'a> {
    opcodes: BindOpCodes<'a>,
    symbol_builder: BindSymbolBuilder<'a>,
    symbols: VecDeque<LazyBindSymbol<'a>>,
}

impl<'a> LazyBind<'a> {
    pub fn parse(payload: &'a [u8], commands: &'a [LoadCommand], ptr_size: usize) -> Self {
        LazyBind {
            opcodes: BindOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol_builder: BindSymbolBuilder::new(commands),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> BindOpCodes<'a> {
        self.opcodes
    }

    fn push_current_symbol(&mut self) {
        match self.symbol_builder.build_lazy_bind_symbol() {
            Ok(symbol) => {
                self.symbols.push_back(symbol);
            }
            Err(err) => {
                warn!("fail to create symbol, {}", err);
            }
        }
    }
}

impl<'a> Iterator for LazyBind<'a> {
    type Item = LazyBindSymbol<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Lazy Bind OpCode: {:?}", opcode);

                match opcode {
                    BindOpCode::Done => {}
                    BindOpCode::SetDyLibrary(ordinal) => if let Err(err) = self.symbol_builder.set_dylib(ordinal) {
                        warn!("fail to set dylib to {}, {}", ordinal, err);

                        break;
                    },
                    BindOpCode::SetSymbol { name, flags } => {
                        self.symbol_builder.symbol_name = Some(name);
                        self.symbol_builder.symbol_flags = Some(flags);
                    }
                    BindOpCode::SetSymbolType(symbol_type) => {
                        self.symbol_builder.symbol_type = Some(symbol_type);
                    }
                    BindOpCode::SetAddend(addend) => {
                        self.symbol_builder.symbol_addend = addend;
                    }
                    BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol_builder.set_segment(segment_index as usize);
                        self.symbol_builder.symbol_offset = segment_offset as isize;
                    }
                    BindOpCode::AddAddress { offset } => {
                        self.symbol_builder.symbol_offset += offset;
                    }
                    BindOpCode::Bind => {
                        self.push_current_symbol();
                        self.symbol_builder.symbol_offset += self.opcodes.ptr_size as isize;
                    }
                    _ => {
                        warn!("unexpected lazy bind opcode: {:?}", opcode);

                        break;
                    }
                }

                if !self.symbols.is_empty() {
                    break;
                }
            }

            self.symbols.pop_front()
        })
    }
}

/// The mach lazy binding symbol information
#[derive(Clone, Debug, PartialEq)]
pub struct LazyBindSymbol<'a> {
    pub dylib_name: &'a str,
    pub segment_name: &'a str,
    pub section_name: &'a str,
    pub name: String,
    pub flags: BindSymbolFlags,
    pub address: usize,
}

#[derive(Clone, Debug, Default)]
struct BindSymbolBuilder<'a> {
    commands: &'a [LoadCommand],
    dylibs: Vec<&'a DyLib>,

    pub dylib_name: Option<&'a str>,
    pub segment: Option<(&'a str, Range<usize>, &'a [Rc<Section>])>,
    pub symbol_name: Option<String>,
    pub symbol_flags: Option<BindSymbolFlags>,
    pub symbol_type: Option<SymbolType>,
    pub symbol_addend: usize,
    pub symbol_offset: isize,
}

impl<'a> BindSymbolBuilder<'a> {
    pub fn new(commands: &'a [LoadCommand]) -> Self {
        let dylibs = commands
            .iter()
            .flat_map(|cmd| match cmd {
                &LoadCommand::IdDyLib(ref dylib)
                | &LoadCommand::LoadDyLib(ref dylib)
                | &LoadCommand::LoadWeakDyLib(ref dylib)
                | &LoadCommand::ReexportDyLib(ref dylib)
                | &LoadCommand::LoadUpwardDylib(ref dylib)
                | &LoadCommand::LazyLoadDylib(ref dylib) => Some(dylib),
                _ => None,
            })
            .collect::<Vec<&DyLib>>();

        BindSymbolBuilder {
            commands,
            dylibs,
            dylib_name: None,
            segment: None,
            symbol_name: None,
            symbol_flags: None,
            symbol_type: None,
            symbol_addend: 0,
            symbol_offset: 0,
        }
    }

    pub fn set_dylib(&mut self, ordinal: isize) -> Result<()> {
        self.dylib_name = match ordinal {
            BIND_SPECIAL_DYLIB_SELF => Some("this-image"),
            BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE => Some("main-executable"),
            BIND_SPECIAL_DYLIB_FLAT_LOOKUP => Some("flat-namespace"),
            index if index > self.dylibs.len() as isize => {
                bail!("libraryOrdinal out of range");
            }
            index if index > 0 => self.dylibs
                .get((index - 1) as usize)
                .and_then(|dylib| Path::new(dylib.name.as_str()).file_name())
                .and_then(|filename| filename.to_str())
                .and_then(|filename| filename.split('.').next()),
            _ => {
                bail!("unknown special ordinal");
            }
        };

        Ok(())
    }

    pub fn set_segment(&mut self, segment_index: usize) {
        self.segment = self.commands.get(segment_index).and_then(|cmd| match cmd {
            &LoadCommand::Segment {
                ref segname,
                vmaddr,
                vmsize,
                ref sections,
                ..
            }
            | &LoadCommand::Segment64 {
                ref segname,
                vmaddr,
                vmsize,
                ref sections,
                ..
            } => Some((
                segname.as_str(),
                (vmaddr..vmaddr + vmsize),
                sections.as_slice(),
            )),
            _ => None,
        });
    }

    pub fn build_bind_symbol(&self) -> Result<BindSymbol<'a>> {
        let dylib_name = self.dylib_name
            .as_ref()
            .ok_or_else(|| format_err!("dylib missed"))?;
        let &(ref segment_name, ref segment_range, ref sections) = self.segment
            .as_ref()
            .ok_or_else(|| format_err!("segment missed"))?;
        let name = self.symbol_name
            .as_ref()
            .cloned()
            .ok_or_else(|| format_err!("symbol name missed"))?;
        let symbol_type = self.symbol_type
            .ok_or_else(|| format_err!("symbol type missed"))?;
        let address = (segment_range.start as isize + self.symbol_offset) as usize;
        let section_name = section_name(sections, address).ok_or_else(|| format_err!("section missed"))?;
        let flags = self.symbol_flags
            .ok_or_else(|| format_err!("symbol flags missed"))?;

        if address >= segment_range.end {
            bail!(
                "address 0x{:016x} out of range: {:?}",
                address,
                segment_range
            );
        }

        Ok(BindSymbol {
            dylib_name,
            segment_name,
            section_name,
            name,
            symbol_type,
            address,
            addend: self.symbol_addend,
            flags,
        })
    }

    pub fn build_weak_bind_symbol(&self) -> Result<WeakBindSymbol<'a>> {
        let &(ref segment_name, ref segment_range, ref sections) = self.segment
            .as_ref()
            .ok_or_else(|| format_err!("segment missed"))?;
        let name = self.symbol_name
            .as_ref()
            .cloned()
            .ok_or_else(|| format_err!("symbol name missed"))?;
        let symbol_type = self.symbol_type
            .ok_or_else(|| format_err!("symbol type missed"))?;
        let address = (segment_range.start as isize + self.symbol_offset) as usize;
        let section_name = section_name(sections, address).ok_or_else(|| format_err!("section missed"))?;
        let flags = self.symbol_flags
            .ok_or_else(|| format_err!("symbol flags missed"))?;

        if address >= segment_range.end {
            bail!(
                "address 0x{:016x} out of range: {:?}",
                address,
                segment_range
            );
        }

        Ok(WeakBindSymbol {
            segment_name,
            section_name,
            name,
            symbol_type,
            address,
            addend: self.symbol_addend,
            flags,
        })
    }

    pub fn build_lazy_bind_symbol(&self) -> Result<LazyBindSymbol<'a>> {
        let dylib_name = self.dylib_name
            .as_ref()
            .ok_or_else(|| format_err!("dylib missed"))?;
        let &(ref segment_name, ref segment_range, ref sections) = self.segment
            .as_ref()
            .ok_or_else(|| format_err!("segment missed"))?;
        let name = self.symbol_name
            .as_ref()
            .cloned()
            .ok_or_else(|| format_err!("symbol name missed"))?;
        let address = (segment_range.start as isize + self.symbol_offset) as usize;
        let section_name = section_name(sections, address).ok_or_else(|| format_err!("section missed"))?;
        let flags = self.symbol_flags
            .ok_or_else(|| format_err!("symbol flags missed"))?;

        if address >= segment_range.end {
            bail!(
                "address 0x{:016x} out of range: {:?}",
                address,
                segment_range
            );
        }

        Ok(LazyBindSymbol {
            dylib_name,
            segment_name,
            section_name,
            name,
            address,
            flags,
        })
    }
}

fn section_name(sections: &[Rc<Section>], addr: usize) -> Option<&str> {
    sections
        .iter()
        .map(|section| section.as_ref())
        .find(|section| {
            section.addr <= addr && section.addr + section.size > addr
        })
        .map(|section| section.sectname.as_str())
}

/// OpCode for the rebasing symbol
#[derive(Clone, Debug, PartialEq)]
pub enum RebaseOpCode {
    Done,
    SetSymbolType(SymbolType),
    SetSegmentOffset {
        segment_index: u8,
        segment_offset: usize,
    },
    AddAddress {
        offset: isize,
    },
    Rebase {
        times: usize,
    },
    RebaseAndAddAddress {
        offset: isize,
    },
    RebaseAndSkipping {
        times: usize,
        skip: usize,
    },
}

/// An iterator over the `RebaseOpCode` of a rebase infomation block.
pub struct RebaseOpCodes<'a> {
    iter: slice::Iter<'a, u8>,
    ptr_size: usize,
}

impl<'a> Iterator for RebaseOpCodes<'a> {
    type Item = RebaseOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|b| {
            match (b & REBASE_OPCODE_MASK, b & REBASE_IMMEDIATE_MASK) {
                (REBASE_OPCODE_DONE, _) => Some(RebaseOpCode::Done),
                (REBASE_OPCODE_SET_TYPE_IMM, rebase_type) => match rebase_type {
                    REBASE_TYPE_POINTER => Some(RebaseOpCode::SetSymbolType(SymbolType::Pointer)),
                    REBASE_TYPE_TEXT_ABSOLUTE32 => Some(RebaseOpCode::SetSymbolType(SymbolType::TextAbsolute32)),
                    REBASE_TYPE_TEXT_PCREL32 => Some(RebaseOpCode::SetSymbolType(SymbolType::TextRelative32)),
                    _ => {
                        warn!("unknown rebase type, {}", rebase_type);

                        None
                    }
                },
                (REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => {
                    self.iter.read_uleb128().ok().map(|segment_offset| {
                        RebaseOpCode::SetSegmentOffset {
                            segment_index,
                            segment_offset,
                        }
                    })
                }
                (REBASE_OPCODE_ADD_ADDR_ULEB, _) => self.iter.read_uleb128().ok().map(|offset| {
                    RebaseOpCode::AddAddress {
                        offset: offset as isize,
                    }
                }),
                (REBASE_OPCODE_ADD_ADDR_IMM_SCALED, count) => Some(RebaseOpCode::AddAddress {
                    offset: self.ptr_size as isize * count as isize,
                }),
                (REBASE_OPCODE_DO_REBASE_IMM_TIMES, times) => Some(RebaseOpCode::Rebase {
                    times: times as usize,
                }),
                (REBASE_OPCODE_DO_REBASE_ULEB_TIMES, _) => self.iter
                    .read_uleb128()
                    .ok()
                    .map(|times| RebaseOpCode::Rebase { times }),
                (REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB, _) => self.iter.read_uleb128().ok().map(|offset| {
                    RebaseOpCode::RebaseAndAddAddress {
                        offset: offset as isize,
                    }
                }),
                (REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB, _) => {
                    if let (Ok(times), Ok(skip)) = (self.iter.read_uleb128(), self.iter.read_uleb128()) {
                        Some(RebaseOpCode::RebaseAndSkipping { times, skip })
                    } else {
                        warn!("fail to read times and skip");

                        None
                    }
                }
                (opcode, immediate) => {
                    warn!(
                        "unknown rebase opcode: 0x{:02x}, immediate: {}",
                        opcode,
                        immediate
                    );

                    None
                }
            }
        })
    }
}

/// A stream of REBASE opcodes
pub struct Rebase<'a> {
    opcodes: RebaseOpCodes<'a>,
    symbol_builder: RebaseSymbolBuilder<'a>,
    symbols: VecDeque<RebaseSymbol<'a>>,
}

impl<'a> Rebase<'a> {
    pub fn parse(payload: &'a [u8], commands: &'a [LoadCommand], ptr_size: usize) -> Self {
        Rebase {
            opcodes: RebaseOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol_builder: RebaseSymbolBuilder::new(commands),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> RebaseOpCodes<'a> {
        self.opcodes
    }

    fn push_current_symbol(&mut self) {
        match self.symbol_builder.build() {
            Ok(symbol) => {
                self.symbols.push_back(symbol);
            }
            Err(err) => {
                warn!("fail to create symbol, {}", err);
            }
        }
    }
}

impl<'a> Iterator for Rebase<'a> {
    type Item = RebaseSymbol<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Rebase OpCode: {:?}", opcode);

                match opcode {
                    RebaseOpCode::Done => {
                        break;
                    }
                    RebaseOpCode::SetSymbolType(symbol_type) => {
                        self.symbol_builder.symbol_type = Some(symbol_type);
                    }
                    RebaseOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol_builder.set_segment(segment_index as usize);
                        self.symbol_builder.symbol_offset = segment_offset as isize;
                    }
                    RebaseOpCode::AddAddress { offset } => {
                        self.symbol_builder.symbol_offset += offset;
                    }
                    RebaseOpCode::Rebase { times } => for _ in 0..times {
                        self.push_current_symbol();

                        self.symbol_builder.symbol_offset += self.opcodes.ptr_size as isize;
                    },
                    RebaseOpCode::RebaseAndAddAddress { offset } => {
                        self.push_current_symbol();

                        self.symbol_builder.symbol_offset += offset + self.opcodes.ptr_size as isize;
                    }
                    RebaseOpCode::RebaseAndSkipping { times, skip } => for _ in 0..times {
                        self.push_current_symbol();

                        self.symbol_builder.symbol_offset += (skip + self.opcodes.ptr_size) as isize;
                    },
                }

                if !self.symbols.is_empty() {
                    break;
                }
            }

            self.symbols.pop_front()
        })
    }
}

/// The rebase symbol information
#[derive(Clone, Debug, PartialEq)]
pub struct RebaseSymbol<'a> {
    pub segment_name: &'a str,
    pub section_name: &'a str,
    pub address: usize,
    pub symbol_type: SymbolType,
}

#[derive(Clone, Debug, Default)]
struct RebaseSymbolBuilder<'a> {
    commands: &'a [LoadCommand],

    pub segment: Option<(&'a str, Range<usize>, &'a [Rc<Section>])>,
    pub symbol_type: Option<SymbolType>,
    pub symbol_offset: isize,
}

impl<'a> RebaseSymbolBuilder<'a> {
    pub fn new(commands: &'a [LoadCommand]) -> Self {
        RebaseSymbolBuilder {
            commands,
            segment: None,
            symbol_type: None,
            symbol_offset: 0,
        }
    }

    pub fn set_segment(&mut self, segment_index: usize) {
        self.segment = self.commands.get(segment_index).and_then(|cmd| match cmd {
            &LoadCommand::Segment {
                ref segname,
                vmaddr,
                vmsize,
                ref sections,
                ..
            }
            | &LoadCommand::Segment64 {
                ref segname,
                vmaddr,
                vmsize,
                ref sections,
                ..
            } => Some((
                segname.as_str(),
                (vmaddr..vmaddr + vmsize),
                sections.as_slice(),
            )),
            _ => None,
        });
    }

    pub fn build(&self) -> Result<RebaseSymbol<'a>> {
        let &(ref segment_name, ref segment_range, ref sections) = self.segment
            .as_ref()
            .ok_or_else(|| format_err!("segment missed"))?;
        let symbol_type = self.symbol_type
            .ok_or_else(|| format_err!("symbol type missed"))?;
        let address = (segment_range.start as isize + self.symbol_offset) as usize;
        let section_name = section_name(sections, address).ok_or_else(|| format_err!("section missed"))?;

        if address >= segment_range.end {
            bail!(
                "address 0x{:016x} out of range: {:?}",
                address,
                segment_range
            );
        }

        Ok(RebaseSymbol {
            segment_name,
            section_name,
            symbol_type,
            address,
        })
    }
}

trait BufExt<'a>: Iterator<Item = &'a u8> {
    fn read_uleb128(&mut self) -> Result<usize> {
        let mut v = 0;
        let mut bits = 0;

        while let Some(b) = self.next() {
            let n = usize::from(b & 0x7F);

            if bits > 63 {
                bail!(MachError::NumberOverflow)
            }

            v |= n << bits;
            bits += 7;

            if (b & 0x80) == 0 {
                break;
            }
        }

        Ok(v)
    }
}

impl<'a, T> BufExt<'a> for T
where
    T: Iterator<Item = &'a u8>,
{
}
