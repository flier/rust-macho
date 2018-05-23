use std::collections::VecDeque;
use std::fmt;
use std::slice;

use consts::*;
use errors::{MachError, Result};

/// Bind or rebase symbol type
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BindSymbolType {
    Pointer,
    TextAbsolute32,
    TextRelative32,
}

impl Default for BindSymbolType {
    fn default() -> Self {
        BindSymbolType::Pointer
    }
}

impl fmt::Display for BindSymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                BindSymbolType::Pointer => "pointer",
                BindSymbolType::TextAbsolute32 => "text abs32",
                BindSymbolType::TextRelative32 => "text rel32",
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

impl Default for BindSymbolFlags {
    fn default() -> Self {
        BindSymbolFlags::empty()
    }
}

/// `OpCode` for the binding symbol
#[derive(Clone, Debug, PartialEq)]
pub enum BindOpCode {
    Done,
    SetDyLibrary(isize),
    SetSymbol { name: String, flags: BindSymbolFlags },
    SetSymbolType(BindSymbolType),
    SetAddend(usize),
    SetSegmentOffset { segment_index: u8, segment_offset: usize },
    AddAddress { offset: isize },
    Bind,
    BindAndAddAddress { offset: isize },
    BindAndSkipping { times: usize, skip: usize },
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
                (BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB, _) => self.iter
                    .read_uleb128()
                    .ok()
                    .map(|library_ordinal| BindOpCode::SetDyLibrary(library_ordinal as isize)),
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
                    self.iter.read_cstr().ok().map(|name| BindOpCode::SetSymbol {
                        name,
                        flags: BindSymbolFlags::from_bits_truncate(flags),
                    })
                }
                (BIND_OPCODE_SET_TYPE_IMM, bind_type) => match bind_type {
                    BIND_TYPE_POINTER => Some(BindOpCode::SetSymbolType(BindSymbolType::Pointer)),
                    BIND_TYPE_TEXT_ABSOLUTE32 => Some(BindOpCode::SetSymbolType(BindSymbolType::TextAbsolute32)),
                    BIND_TYPE_TEXT_PCREL32 => Some(BindOpCode::SetSymbolType(BindSymbolType::TextRelative32)),
                    _ => {
                        warn!("unknown bind type, {}", bind_type);

                        None
                    }
                },
                (BIND_OPCODE_SET_ADDEND_SLEB, _) => self.iter.read_uleb128().ok().map(BindOpCode::SetAddend),
                (BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => self.iter.read_uleb128().ok().map(
                    |segment_offset| BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    },
                ),
                (BIND_OPCODE_ADD_ADDR_ULEB, _) => self.iter.read_uleb128().ok().map(|offset| BindOpCode::AddAddress {
                    offset: offset as isize,
                }),
                (BIND_OPCODE_DO_BIND, _) => Some(BindOpCode::Bind),
                (BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB, _) => {
                    self.iter.read_uleb128().ok().map(|offset| BindOpCode::AddAddress {
                        offset: offset as isize,
                    })
                }
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
                    warn!("unknown bind opcode: {:x}, immediate = {}", opcode, immediate);

                    None
                }
            })
    }
}

/// The mach binding symbol information
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BindSymbol {
    pub dylib_ordinal: usize,
    pub segment_index: usize,
    pub name: String,
    pub flags: BindSymbolFlags,
    pub symbol_type: BindSymbolType,
    pub symbol_offset: isize,
    pub addend: usize,
}

/// A stream of BIND opcodes to bind all binding symbols.
pub struct Bind<'a> {
    opcodes: BindOpCodes<'a>,
    symbol: BindSymbol,
    symbols: VecDeque<BindSymbol>,
}

impl<'a> Bind<'a> {
    pub fn parse(payload: &'a [u8], ptr_size: usize) -> Self {
        Bind {
            opcodes: BindOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol: Default::default(),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> BindOpCodes<'a> {
        self.opcodes
    }
}

impl<'a> Iterator for Bind<'a> {
    type Item = BindSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Bind OpCode: {:?}", opcode);

                match opcode {
                    BindOpCode::Done => {
                        break;
                    }
                    BindOpCode::SetDyLibrary(ordinal) => {
                        self.symbol.dylib_ordinal = ordinal as usize;
                    }
                    BindOpCode::SetSymbol { name, flags } => {
                        self.symbol.name = name;
                        self.symbol.flags = flags;
                    }
                    BindOpCode::SetSymbolType(symbol_type) => {
                        self.symbol.symbol_type = symbol_type;
                    }
                    BindOpCode::SetAddend(addend) => {
                        self.symbol.addend = addend;
                    }
                    BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol.segment_index = segment_index as usize;
                        self.symbol.symbol_offset = segment_offset as isize;
                    }
                    BindOpCode::AddAddress { offset } => {
                        self.symbol.symbol_offset += offset;
                    }
                    BindOpCode::Bind => {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndAddAddress { offset } => {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += offset + self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndSkipping { times, skip } => for _ in 0..times {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += (skip + self.opcodes.ptr_size) as isize;
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

/// The mach weak binding symbol information
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WeakBindSymbol {
    pub segment_index: usize,
    pub name: String,
    pub flags: BindSymbolFlags,
    pub symbol_type: BindSymbolType,
    pub symbol_offset: isize,
    pub addend: usize,
}

/// A stream of BIND opcodes to bind all weak binding symbols.
pub struct WeakBind<'a> {
    opcodes: BindOpCodes<'a>,
    symbol: WeakBindSymbol,
    symbols: VecDeque<WeakBindSymbol>,
}

impl<'a> WeakBind<'a> {
    pub fn parse(payload: &'a [u8], ptr_size: usize) -> Self {
        WeakBind {
            opcodes: BindOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol: Default::default(),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> BindOpCodes<'a> {
        self.opcodes
    }
}

impl<'a> Iterator for WeakBind<'a> {
    type Item = WeakBindSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Weak Bind OpCode: {:?}", opcode);

                match opcode {
                    BindOpCode::Done => {
                        break;
                    }
                    BindOpCode::SetSymbol { name, flags } => {
                        self.symbol.name = name;
                        self.symbol.flags = flags;
                    }
                    BindOpCode::SetSymbolType(symbol_type) => {
                        self.symbol.symbol_type = symbol_type;
                    }
                    BindOpCode::SetAddend(addend) => {
                        self.symbol.addend = addend;
                    }
                    BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol.segment_index = segment_index as usize;
                        self.symbol.symbol_offset = segment_offset as isize;
                    }
                    BindOpCode::AddAddress { offset } => {
                        self.symbol.symbol_offset += offset;
                    }
                    BindOpCode::Bind => {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndAddAddress { offset } => {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += offset + self.opcodes.ptr_size as isize;
                    }
                    BindOpCode::BindAndSkipping { times, skip } => for _ in 0..times {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += (skip + self.opcodes.ptr_size) as isize;
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

/// The mach lazy binding symbol information
#[derive(Clone, Debug, Default, PartialEq)]
pub struct LazyBindSymbol {
    pub dylib_ordinal: usize,
    pub segment_index: usize,
    pub name: String,
    pub flags: BindSymbolFlags,
    pub symbol_offset: isize,
}

/// A stream of BIND opcodes to bind all lazy symbols.
pub struct LazyBind<'a> {
    opcodes: BindOpCodes<'a>,
    symbol: LazyBindSymbol,
    symbols: VecDeque<LazyBindSymbol>,
}

impl<'a> LazyBind<'a> {
    pub fn parse(payload: &'a [u8], ptr_size: usize) -> Self {
        LazyBind {
            opcodes: BindOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol: Default::default(),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> BindOpCodes<'a> {
        self.opcodes
    }
}

impl<'a> Iterator for LazyBind<'a> {
    type Item = LazyBindSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Lazy Bind OpCode: {:?}", opcode);

                match opcode {
                    BindOpCode::Done => {}
                    BindOpCode::SetDyLibrary(ordinal) => {
                        self.symbol.dylib_ordinal = ordinal as usize;
                    }
                    BindOpCode::SetSymbol { name, flags } => {
                        self.symbol.name = name;
                        self.symbol.flags = flags;
                    }
                    BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol.segment_index = segment_index as usize;
                        self.symbol.symbol_offset = segment_offset as isize;
                    }
                    BindOpCode::AddAddress { offset } => {
                        self.symbol.symbol_offset += offset;
                    }
                    BindOpCode::Bind => {
                        self.symbols.push_back(self.symbol.clone());
                        self.symbol.symbol_offset += self.opcodes.ptr_size as isize;
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

/// `OpCode` for the rebasing symbol
#[derive(Clone, Debug, PartialEq)]
pub enum RebaseOpCode {
    Done,
    SetSymbolType(BindSymbolType),
    SetSegmentOffset { segment_index: u8, segment_offset: usize },
    AddAddress { offset: isize },
    Rebase { times: usize },
    RebaseAndAddAddress { offset: isize },
    RebaseAndSkipping { times: usize, skip: usize },
}

/// An iterator over the `RebaseOpCode` of a rebase infomation block.
pub struct RebaseOpCodes<'a> {
    iter: slice::Iter<'a, u8>,
    ptr_size: usize,
}

impl<'a> Iterator for RebaseOpCodes<'a> {
    type Item = RebaseOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .and_then(|b| match (b & REBASE_OPCODE_MASK, b & REBASE_IMMEDIATE_MASK) {
                (REBASE_OPCODE_DONE, _) => Some(RebaseOpCode::Done),
                (REBASE_OPCODE_SET_TYPE_IMM, rebase_type) => match rebase_type {
                    REBASE_TYPE_POINTER => Some(RebaseOpCode::SetSymbolType(BindSymbolType::Pointer)),
                    REBASE_TYPE_TEXT_ABSOLUTE32 => Some(RebaseOpCode::SetSymbolType(BindSymbolType::TextAbsolute32)),
                    REBASE_TYPE_TEXT_PCREL32 => Some(RebaseOpCode::SetSymbolType(BindSymbolType::TextRelative32)),
                    _ => {
                        warn!("unknown rebase type, {}", rebase_type);

                        None
                    }
                },
                (REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => self.iter.read_uleb128().ok().map(
                    |segment_offset| RebaseOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    },
                ),
                (REBASE_OPCODE_ADD_ADDR_ULEB, _) => {
                    self.iter.read_uleb128().ok().map(|offset| RebaseOpCode::AddAddress {
                        offset: offset as isize,
                    })
                }
                (REBASE_OPCODE_ADD_ADDR_IMM_SCALED, count) => Some(RebaseOpCode::AddAddress {
                    offset: self.ptr_size as isize * count as isize,
                }),
                (REBASE_OPCODE_DO_REBASE_IMM_TIMES, times) => Some(RebaseOpCode::Rebase { times: times as usize }),
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
                    warn!("unknown rebase opcode: 0x{:02x}, immediate: {}", opcode, immediate);

                    None
                }
            })
    }
}

/// A stream of REBASE opcodes
pub struct Rebase<'a> {
    opcodes: RebaseOpCodes<'a>,
    symbol: RebaseSymbol,
    symbols: VecDeque<RebaseSymbol>,
}

impl<'a> Rebase<'a> {
    pub fn parse(payload: &'a [u8], ptr_size: usize) -> Self {
        Rebase {
            opcodes: RebaseOpCodes {
                iter: payload.iter(),
                ptr_size,
            },
            symbol: Default::default(),
            symbols: Default::default(),
        }
    }

    pub fn opcodes(self) -> RebaseOpCodes<'a> {
        self.opcodes
    }
}

impl<'a> Iterator for Rebase<'a> {
    type Item = RebaseSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.pop_front().or_else(|| {
            while let Some(opcode) = self.opcodes.next() {
                trace!("Rebase OpCode: {:?}", opcode);

                match opcode {
                    RebaseOpCode::Done => {
                        break;
                    }
                    RebaseOpCode::SetSymbolType(symbol_type) => {
                        self.symbol.symbol_type = symbol_type;
                    }
                    RebaseOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    } => {
                        self.symbol.segment_index = segment_index as usize;
                        self.symbol.symbol_offset = segment_offset as isize;
                    }
                    RebaseOpCode::AddAddress { offset } => {
                        self.symbol.symbol_offset += offset;
                    }
                    RebaseOpCode::Rebase { times } => for _ in 0..times {
                        self.symbols.push_back(self.symbol.clone());

                        self.symbol.symbol_offset += self.opcodes.ptr_size as isize;
                    },
                    RebaseOpCode::RebaseAndAddAddress { offset } => {
                        self.symbols.push_back(self.symbol.clone());

                        self.symbol.symbol_offset += offset + self.opcodes.ptr_size as isize;
                    }
                    RebaseOpCode::RebaseAndSkipping { times, skip } => for _ in 0..times {
                        self.symbols.push_back(self.symbol.clone());

                        self.symbol.symbol_offset += (skip + self.opcodes.ptr_size) as isize;
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
#[derive(Clone, Debug, Default, PartialEq)]
pub struct RebaseSymbol {
    pub segment_index: usize,
    pub symbol_offset: isize,
    pub symbol_type: BindSymbolType,
}

pub trait IteratorExt<'a>: Iterator<Item = &'a u8> {
    fn read_uleb128(&mut self) -> Result<usize> {
        let mut v = 0;
        let mut bits = 0;

        while let Some(b) = self.next() {
            let n = usize::from(b & 0x7F);

            if bits > 63 {
                return Err(MachError::NumberOverflow.into());
            }

            v |= n << bits;
            bits += 7;

            if (b & 0x80) == 0 {
                break;
            }
        }

        Ok(v)
    }

    fn read_cstr(&mut self) -> Result<String> {
        let mut v = vec![];

        while let Some(&b) = self.next() {
            if b == 0 {
                break;
            } else {
                v.push(b);
            }
        }

        Ok(String::from_utf8(v)?)
    }
}

impl<'a, T> IteratorExt<'a> for T
where
    T: Iterator<Item = &'a u8>,
{
}
