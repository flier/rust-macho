use std::fmt;
use std::slice;

use consts::*;
use errors::{MachError, Result};

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum RebaseOpCode {
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

impl RebaseOpCode {
    pub fn parse<'a>(payload: &'a [u8]) -> RebaseOpCodes<'a> {
        RebaseOpCodes(payload.iter())
    }
}

pub struct RebaseOpCodes<'a>(slice::Iter<'a, u8>);

impl<'a> Iterator for RebaseOpCodes<'a> {
    type Item = RebaseOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().and_then(
            |b| match (b & REBASE_OPCODE_MASK, b & REBASE_IMMEDIATE_MASK) {
                (REBASE_OPCODE_DONE, _) => None,
                (REBASE_OPCODE_SET_TYPE_IMM, rebase_type) => match rebase_type {
                    REBASE_TYPE_POINTER => Some(RebaseOpCode::SetSymbolType(SymbolType::Pointer)),
                    REBASE_TYPE_TEXT_ABSOLUTE32 => Some(RebaseOpCode::SetSymbolType(SymbolType::TextAbsolute32)),
                    REBASE_TYPE_TEXT_PCREL32 => Some(RebaseOpCode::SetSymbolType(SymbolType::TextRelative32)),
                    _ => {
                        warn!("unknown rebase type, {}", rebase_type);

                        None
                    }
                },
                (REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => self.0.read_uleb128().ok().map(
                    |segment_offset| RebaseOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    },
                ),
                (REBASE_OPCODE_ADD_ADDR_ULEB, _) => self.0
                    .read_uleb128()
                    .ok()
                    .map(|offset| RebaseOpCode::AddAddress {
                        offset: offset as isize,
                    }),
                (REBASE_OPCODE_ADD_ADDR_IMM_SCALED, count) => Some(RebaseOpCode::AddAddress {
                    offset: POINTER_BYTES as isize * count as isize,
                }),
                (REBASE_OPCODE_DO_REBASE_IMM_TIMES, times) => Some(RebaseOpCode::Rebase {
                    times: times as usize,
                }),
                (REBASE_OPCODE_DO_REBASE_ULEB_TIMES, _) => self.0
                    .read_uleb128()
                    .ok()
                    .map(|times| RebaseOpCode::Rebase { times }),
                (REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB, _) => self.0.read_uleb128().ok().map(|offset| {
                    RebaseOpCode::RebaseAndAddAddress {
                        offset: offset as isize,
                    }
                }),
                (REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB, _) => {
                    if let (Ok(times), Ok(skip)) = (self.0.read_uleb128(), self.0.read_uleb128()) {
                        Some(RebaseOpCode::RebaseAndSkipping { times, skip })
                    } else {
                        warn!("fail to read times and skip");

                        None
                    }
                }
                (opcode, immediate) => {
                    warn!(
                        "unknown rebase opcode: 0x{:02x}, immediate: {}",
                        opcode, immediate
                    );

                    None
                }
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DyLibrary {
    Ordinal(usize),
    Itself,
    Executable,
    FlatLookup,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BindOpCode {
    SetDyLibrary(DyLibrary),
    SetSymbol {
        name: String,
        flags: u8,
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

impl BindOpCode {
    pub fn parse<'a>(payload: &'a [u8]) -> BindOpCodes<'a> {
        BindOpCodes(payload.iter())
    }
}

pub struct BindOpCodes<'a>(slice::Iter<'a, u8>);

impl<'a> Iterator for BindOpCodes<'a> {
    type Item = BindOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .and_then(|b| match (b & BIND_OPCODE_MASK, b & BIND_IMMEDIATE_MASK) {
                (BIND_OPCODE_DONE, _) => {
                    trace!("BIND_OPCODE_DONE");

                    None
                }
                (BIND_OPCODE_SET_DYLIB_ORDINAL_IMM, library_ordinal) => Some(BindOpCode::SetDyLibrary(
                    DyLibrary::Ordinal(library_ordinal as usize),
                )),
                (BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB, _) => self.0
                    .read_uleb128()
                    .ok()
                    .map(|library_ordinal| BindOpCode::SetDyLibrary(DyLibrary::Ordinal(library_ordinal))),
                (BIND_OPCODE_SET_DYLIB_SPECIAL_IMM, library_type) => match library_type {
                    0 => Some(BindOpCode::SetDyLibrary(DyLibrary::Itself)),
                    0x0f => Some(BindOpCode::SetDyLibrary(DyLibrary::Executable)),
                    0x0e => Some(BindOpCode::SetDyLibrary(DyLibrary::FlatLookup)),
                    _ => {
                        warn!("unknown library type: 0x{:x}", library_type);

                        None
                    }
                },
                (BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM, flags) => {
                    let mut v = vec![];

                    while let Some(&b) = self.0.next() {
                        if b == 0 {
                            break;
                        } else {
                            v.push(b);
                        }
                    }

                    String::from_utf8(v)
                        .ok()
                        .map(|name| BindOpCode::SetSymbol { name, flags })
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
                (BIND_OPCODE_SET_ADDEND_SLEB, _) => self.0
                    .read_uleb128()
                    .ok()
                    .map(|addend| BindOpCode::SetAddend(addend)),
                (BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => self.0.read_uleb128().ok().map(
                    |segment_offset| BindOpCode::SetSegmentOffset {
                        segment_index,
                        segment_offset,
                    },
                ),
                (BIND_OPCODE_ADD_ADDR_ULEB, _) => self.0
                    .read_uleb128()
                    .ok()
                    .map(|offset| BindOpCode::AddAddress {
                        offset: offset as isize,
                    }),
                (BIND_OPCODE_DO_BIND, _) => Some(BindOpCode::Bind),
                (BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB, _) => self.0.read_uleb128().ok().map(|offset| {
                    BindOpCode::AddAddress {
                        offset: offset as isize,
                    }
                }),
                (BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED, count) => Some(BindOpCode::AddAddress {
                    offset: POINTER_BYTES as isize * count as isize,
                }),
                (BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB, _) => {
                    if let (Ok(times), Ok(skip)) = (self.0.read_uleb128(), self.0.read_uleb128()) {
                        Some(BindOpCode::BindAndSkipping { times, skip })
                    } else {
                        warn!("fail to read times and skip");

                        None
                    }
                }
                (opcode, immediate) => {
                    warn!(
                        "unknown bind opcode: {:x}, immediate = {}",
                        opcode, immediate
                    );

                    None
                }
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
