use std::io::Cursor;

use byteorder::ReadBytesExt;

use crate::commands::CursorExt;
use crate::consts::*;
use crate::errors::{Error::*, Result};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExportKind {
    Regular,
    ThreadLocal,
    Absolute,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExportType {
    Regular { address: usize },
    Weak { address: usize },
    Reexport { ordinal: usize, name: String },
    Stub { offset: usize, resolver: usize },
}

#[derive(Clone, Debug, PartialEq)]
struct Exported {
    symbol: Option<(ExportKind, ExportType)>,
    edges: Vec<(String, Exported)>,
}

impl Exported {
    pub fn parse<T>(cur: &mut Cursor<T>) -> Result<Exported>
    where
        T: AsRef<[u8]>,
    {
        Self::parse_inner(cur, &[])
    }

    fn parse_inner<T>(cur: &mut Cursor<T>, visited_offsets: &[u64]) -> Result<Exported>
    where
        T: AsRef<[u8]>,
    {
        // add this node to the list of visited offsets that will be passed
        // to child nodes
        let mut visited_offsets = visited_offsets.to_owned();
        visited_offsets.push(cur.position());

        let terminal_size = cur.read_uleb128()?;

        let symbol = if terminal_size != 0 {
            let flags = cur.read_uleb128()?;

            let kind = match flags as u8 & EXPORT_SYMBOL_FLAGS_KIND_MASK {
                EXPORT_SYMBOL_FLAGS_KIND_REGULAR => ExportKind::Regular,
                EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL => ExportKind::ThreadLocal,
                EXPORT_SYMBOL_FLAGS_KIND_ABSOLUTE => ExportKind::Absolute,
                _ => unreachable!(),
            };

            let flags = ExportSymbolFlags::from_bits_truncate(flags as u32);

            let symbol = if flags.contains(ExportSymbolFlags::EXPORT_SYMBOL_FLAGS_REEXPORT) {
                let ordinal = cur.read_uleb128()?;
                let name = cur.read_cstr()?;

                ExportType::Reexport { ordinal, name }
            } else if flags.contains(ExportSymbolFlags::EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER) {
                let offset = cur.read_uleb128()?;
                let resolver = cur.read_uleb128()?;

                ExportType::Stub { offset, resolver }
            } else {
                let address = cur.read_uleb128()?;

                if flags.contains(ExportSymbolFlags::EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION) {
                    ExportType::Weak { address }
                } else {
                    ExportType::Regular { address }
                }
            };

            Some((kind, symbol))
        } else {
            None
        };

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
                    return Err(BufferOverflow(offset));
                }
                // prevent infinite recursions if the trie is malformed by
                // checking we don't revisit any of our parents
                if visited_offsets.contains(&(offset as u64)) {
                    return Err(RecursiveExportTrie(offset as u64));
                }

                let mut cur = Cursor::new(payload);

                cur.set_position(offset as u64);

                Ok((name, Exported::parse_inner(&mut cur, &visited_offsets)?))
            })
            .collect::<Result<Vec<(String, Exported)>>>()?;

        Ok(Exported { symbol, edges })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExportTrie<'a> {
    payload: &'a [u8],
    root: Exported,
}

impl<'a> ExportTrie<'a> {
    pub fn parse(payload: &'a [u8]) -> Result<ExportTrie<'a>> {
        let mut cur = Cursor::new(payload);
        let root = Exported::parse(&mut cur)?;

        Ok(ExportTrie { payload, root })
    }

    pub fn symbols(&'a self) -> ExportSymbols<'a> {
        ExportSymbols {
            nodes: vec![(Default::default(), &self.root)],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExportSymbol {
    pub name: String,
    pub kind: ExportKind,
    pub symbol: ExportType,
}

impl ExportSymbol {
    pub fn address(&self) -> Option<usize> {
        match self.symbol {
            ExportType::Reexport { .. } => None,
            ExportType::Regular { address }
            | ExportType::Weak { address }
            | ExportType::Stub { offset: address, .. } => Some(address),
        }
    }
}

pub struct ExportSymbols<'a> {
    nodes: Vec<(String, &'a Exported)>,
}

impl<'a> Iterator for ExportSymbols<'a> {
    type Item = ExportSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((prefix, node)) = self.nodes.pop() {
            for &(ref s, ref node) in &node.edges {
                self.nodes.push((prefix.clone() + s, node))
            }

            if let Some((kind, ref symbol)) = node.symbol {
                return Some(ExportSymbol {
                    name: prefix,
                    kind,
                    symbol: symbol.clone(),
                });
            }
        }

        None
    }
}
