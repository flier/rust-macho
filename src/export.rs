use std::io::Cursor;

use byteorder::ReadBytesExt;

use errors::{MachError, Result};
use commands::CursorExt;
use consts::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExportSymbolType {
    Regular,
    ThreadLocal,
    Absolute,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExportSymbol {
    Regular { address: usize },
    Weak { address: usize },
    Reexport { ordinal: usize, name: String },
    Stub { offset: usize },
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExportNode {
    Symbol {
        symbol_type: ExportSymbolType,
        symbol: ExportSymbol,
    },
    Stem {
        edges: Vec<(String, ExportNode)>,
    },
}

impl ExportNode {
    pub fn parse<'a, T>(cur: &mut Cursor<T>) -> Result<Self>
    where
        T: AsRef<[u8]>,
    {
        let terminal_size = cur.read_uleb128()?;

        if terminal_size != 0 {
            let flags = cur.read_uleb128()?;

            let symbol_type = match flags as u8 & EXPORT_SYMBOL_FLAGS_KIND_MASK {
                EXPORT_SYMBOL_FLAGS_KIND_REGULAR => ExportSymbolType::Regular,
                EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL => ExportSymbolType::ThreadLocal,
                EXPORT_SYMBOL_FLAGS_KIND_ABSOLUTE => ExportSymbolType::Absolute,
                _ => unreachable!(),
            };

            let flags = ExportSymbolFlags::from_bits_truncate(flags as u32);

            let symbol = if flags.contains(ExportSymbolFlags::EXPORT_SYMBOL_FLAGS_REEXPORT) {
                let ordinal = cur.read_uleb128()?;
                let name = cur.read_cstr()?;

                ExportSymbol::Reexport { ordinal, name }
            } else {
                let address = cur.read_uleb128()?;

                if flags.contains(ExportSymbolFlags::EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION) {
                    ExportSymbol::Weak { address }
                } else if flags.contains(ExportSymbolFlags::EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER) {
                    ExportSymbol::Stub { offset: address }
                } else {
                    ExportSymbol::Regular { address }
                }
            };

            Ok(ExportNode::Symbol {
                symbol_type,
                symbol,
            })
        } else {
            let edges = (0..cur.read_u8()? as usize)
                .map(|_| {
                    let name = cur.read_cstr()?;
                    let offset = cur.read_uleb128()?;

                    Ok((name, offset))
                })
                .collect::<Result<Vec<(String, usize)>>>()?;

            let payload = cur.get_ref().as_ref();

            let edges = edges
                .into_iter()
                .map(|(name, offset)| {
                    if offset > payload.len() {
                        bail!(MachError::BufferOverflow(offset))
                    }

                    let mut cur = Cursor::new(payload);

                    cur.set_position(offset as u64);

                    Ok((name, ExportNode::parse(&mut cur)?))
                })
                .collect::<Result<Vec<(String, ExportNode)>>>()?;

            Ok(ExportNode::Stem { edges })
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExportTrie {
    root: ExportNode,
}

impl ExportTrie {
    pub fn parse<'a, T>(cur: &mut Cursor<T>) -> Result<Self>
    where
        T: AsRef<[u8]>,
    {
        Ok(ExportTrie {
            root: ExportNode::parse(cur)?,
        })
    }
}
