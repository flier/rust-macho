use std::io::Cursor;
use std::mem;

use bytes::Buf;

use consts::*;
use errors::{MachError, Result};

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum SymbolType {
    Pointer,
    TextAbsolute32,
    TextRelative32,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RebaseOpCode {
    SetSymbolType(SymbolType),
    SetSegmentOffset {
        segment_index: u8,
        segment_offset: usize,
    },
    AddAddress {
        offset: usize,
    },
    Rebase {
        times: usize,
    },
    RebaseAndAddAddress {
        offset: usize,
    },
    RebaseAndSkipping {
        times: usize,
        skip: usize,
    },
}

impl RebaseOpCode {
    pub fn parse<T: AsRef<[u8]>>(payload: T) -> RebaseOpCodes<T> {
        RebaseOpCodes(Cursor::new(payload))
    }
}

pub struct RebaseOpCodes<T>(Cursor<T>);

impl<T> Iterator for RebaseOpCodes<T>
where
    T: AsRef<[u8]>,
{
    type Item = RebaseOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        let b = self.0.get_u8();

        match (b & REBASE_OPCODE_MASK, b & REBASE_IMMEDIATE_MASK) {
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
            (REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB, segment_index) => self.0.get_uleb128().ok().map(
                |segment_offset| RebaseOpCode::SetSegmentOffset {
                    segment_index,
                    segment_offset,
                },
            ),
            (REBASE_OPCODE_ADD_ADDR_ULEB, _) => self.0.get_uleb128().ok().map(|address_offset| {
                RebaseOpCode::AddAddress {
                    offset: address_offset,
                }
            }),
            (REBASE_OPCODE_ADD_ADDR_IMM_SCALED, count) => Some(RebaseOpCode::AddAddress {
                offset: mem::size_of::<u32>() * count as usize,
            }),
            (REBASE_OPCODE_DO_REBASE_IMM_TIMES, times) => Some(RebaseOpCode::Rebase {
                times: times as usize,
            }),
            (REBASE_OPCODE_DO_REBASE_ULEB_TIMES, _) => self.0.get_uleb128().ok().map(|times| RebaseOpCode::Rebase {
                times: times as usize,
            }),
            (REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB, _) => self.0.get_uleb128().ok().map(|offset| {
                RebaseOpCode::RebaseAndAddAddress {
                    offset: offset + mem::size_of::<u32>(),
                }
            }),
            (REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB, _) => {
                if let (Ok(times), Ok(skip)) = (self.0.get_uleb128(), self.0.get_uleb128()) {
                    Some(RebaseOpCode::RebaseAndSkipping { times, skip })
                } else {
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
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BindOpCode {}

impl BindOpCode {
    pub fn parse<T: AsRef<[u8]>>(payload: T) -> BindOpCodes<T> {
        BindOpCodes(Cursor::new(payload))
    }
}

pub struct BindOpCodes<T>(Cursor<T>);

impl<T> Iterator for BindOpCodes<T>
where
    T: AsRef<[u8]>,
{
    type Item = BindOpCode;

    fn next(&mut self) -> Option<Self::Item> {
        let b = self.0.get_u8();

        match (b & BIND_OPCODE_MASK, b & BIND_IMMEDIATE_MASK) {
            (BIND_OPCODE_DONE, _) => {
                trace!("BIND_OPCODE_DONE");

                None
            }
            (opcode, immediate) => {
                warn!("unknown opcode: {:x}, immediate = {}", opcode, immediate);

                None
            }
        }
    }
}

trait BufExt: Buf {
    fn get_uleb128(&mut self) -> Result<usize> {
        let mut v = 0;
        let mut bits = 0;

        for b in self.iter() {
            v |= usize::from(b & 0x7F) << bits;
            bits += 7;

            if bits > 64 {
                bail!(MachError::NumberOverflow)
            }

            if (b & 0x80) == 0 {
                break;
            }
        }

        Ok(v)
    }
}

impl<T> BufExt for T
where
    T: Buf,
{
}
