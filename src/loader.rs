#![allow(non_camel_case_types)]
use std::convert::From;
use std::io::{BufRead, Cursor, ErrorKind, Read, Seek, SeekFrom};
use std::mem::size_of;

use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian, ReadBytesExt};

use crate::commands::{LoadCommand, ReadStringExt};
use crate::consts::*;
use crate::errors::{Error::*, Result};

#[cfg(windows)]
type uid_t = libc::uint32_t;
#[cfg(windows)]
type gid_t = libc::uint32_t;
#[cfg(windows)]
type mode_t = libc::uint32_t;
#[cfg(not(windows))]
type uid_t = libc::uid_t;
#[cfg(not(windows))]
type gid_t = libc::gid_t;
#[cfg(not(windows))]
type mode_t = libc::mode_t;

/// The 32-bit mach/fat header
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Arch32 {}
/// The 64-bit mach/fat header
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Arch64 {}

/// The architecture of mach header
///
pub trait MachHeaderParser {
    /// parse mach header
    fn parse_mach_header<T: BufRead, O: ByteOrder>(magic: u32, buf: &mut T) -> Result<MachHeader>;
}

impl MachHeaderParser for Arch32 {
    fn parse_mach_header<T: BufRead, O: ByteOrder>(magic: u32, buf: &mut T) -> Result<MachHeader> {
        let header = MachHeader {
            magic,
            cputype: buf.read_i32::<O>()?,
            cpusubtype: buf.read_i32::<O>()?,
            filetype: buf.read_u32::<O>()?,
            ncmds: buf.read_u32::<O>()?,
            sizeofcmds: buf.read_u32::<O>()?,
            flags: buf.read_u32::<O>()?,
        };

        Ok(header)
    }
}

impl MachHeaderParser for Arch64 {
    fn parse_mach_header<T: BufRead, O: ByteOrder>(magic: u32, buf: &mut T) -> Result<MachHeader> {
        let header = MachHeader {
            magic,
            cputype: buf.read_i32::<O>()?,
            cpusubtype: buf.read_i32::<O>()?,
            filetype: buf.read_u32::<O>()?,
            ncmds: buf.read_u32::<O>()?,
            sizeofcmds: buf.read_u32::<O>()?,
            flags: buf.read_u32::<O>()?,
        };

        buf.consume(4);

        Ok(header)
    }
}

/// The mach header appears at the very beginning of the object file
///
#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct MachHeader {
    /// mach magic number identifier
    pub magic: u32,
    /// cpu specifier
    pub cputype: cpu_type_t,
    /// machine specifier
    pub cpusubtype: cpu_subtype_t,
    /// type of file
    pub filetype: u32,
    /// number of load commands
    pub ncmds: u32,
    /// the size of all the load commands
    pub sizeofcmds: u32,
    /// flags
    pub flags: u32,
}

impl MachHeader {
    pub fn is_64bit(&self) -> bool {
        (self.cputype & CPU_ARCH_MASK) == CPU_ARCH_ABI64
    }

    pub fn is_bigend(&self) -> bool {
        self.magic == MH_CIGAM || self.magic == MH_CIGAM_64
    }
}

/// Wrap load command with size in the Mach-O file
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct MachCommand(pub LoadCommand, pub usize);

impl MachCommand {
    pub fn command(&self) -> &LoadCommand {
        &self.0
    }

    pub fn size(&self) -> usize {
        self.1
    }
}

/// The structures of the file format for "fat" architecture specific file (wrapper design).
/// At the begining of the file there is one `FatHeader` structure followed by a number of `FatArch`
/// structures.
///
#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct FatHeader {
    /// fat magic number identifier
    pub magic: u32,
    /// number of structs that follow
    pub archs: Vec<FatArch>,
}

/// The architecture of mach header
///
pub trait FatArchParser {
    /// parse fat arch
    fn parse_fat_arch<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<FatArch>;
}

impl FatArchParser for Arch32 {
    fn parse_fat_arch<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<FatArch> {
        Ok(FatArch {
            cputype: buf.read_u32::<O>()? as cpu_type_t,
            cpusubtype: buf.read_u32::<O>()? as cpu_subtype_t,
            offset: buf.read_u32::<O>()? as u64,
            size: buf.read_u32::<O>()? as u64,
            align: buf.read_u32::<O>()?,
        })
    }
}

impl FatArchParser for Arch64 {
    fn parse_fat_arch<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<FatArch> {
        Ok(FatArch {
            cputype: buf.read_u32::<O>()? as cpu_type_t,
            cpusubtype: buf.read_u32::<O>()? as cpu_subtype_t,
            offset: buf.read_u64::<O>()?,
            size: buf.read_u64::<O>()?,
            align: {
                let align = buf.read_u32::<O>()?;
                let _reserved = buf.read_u32::<O>()?;
                align
            },
        })
    }
}

/// For each architecture in the file, specified by a pair of cputype and cpusubtype,
/// the `FatArch` describes the file offset, file size and alignment
/// in the file of the architecture specific member.
///
#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct FatArch {
    /// cpu specifier (int)
    pub cputype: cpu_type_t,
    /// machine specifier (int)
    pub cpusubtype: cpu_subtype_t,
    /// file offset to this object file
    pub offset: u64,
    /// size of this object file
    pub size: u64,
    /// alignment as a power of 2
    pub align: u32,
}

impl FatArch {
    pub fn name(&self) -> Option<&str> {
        get_arch_name_from_types(self.cputype, self.cpusubtype)
    }
}

/// the archive file header
#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct ArHeader {
    pub ar_name: String,
    /// modification time
    pub ar_date: libc::time_t,
    /// user id
    pub ar_uid: uid_t,
    /// group id
    pub ar_gid: gid_t,
    /// octal file permissions
    pub ar_mode: mode_t,
    /// size in bytes
    pub ar_size: usize,
    /// consistency check
    pub ar_fmag: u16,
    /// extended format #1
    pub ar_member_name: Option<String>,
}

impl ArHeader {
    fn parse<T: AsRef<[u8]>>(buf: &mut Cursor<T>) -> Result<ArHeader> {
        let mut header = ArHeader {
            ar_name: String::from(buf.read_fixed_size_string(16)?.trim()),
            ar_date: buf.read_fixed_size_string(12)?.trim().parse()?,
            ar_uid: buf.read_fixed_size_string(6)?.trim().parse()?,
            ar_gid: buf.read_fixed_size_string(6)?.trim().parse()?,
            ar_mode: Self::parse_octal(buf.read_fixed_size_string(8)?.trim())? as mode_t,
            ar_size: buf.read_fixed_size_string(10)?.trim().parse()?,
            ar_fmag: buf.read_u16::<NativeEndian>()?,
            ar_member_name: None,
        };

        if let Some(size) = header.extended_format_size() {
            header.ar_member_name = Some(buf.read_fixed_size_string(size)?);
        }

        debug!("{:08x}\tparsed ar header: {:?}", buf.position(), header);

        Ok(header)
    }

    fn extended_format_size(&self) -> Option<usize> {
        if self.ar_name.starts_with(AR_EFMT1) {
            if let Ok(size) = self.ar_name[AR_EFMT1.len()..].parse() {
                return Some(size);
            }
        }

        None
    }

    fn parse_octal(s: &str) -> Result<usize> {
        let mut v: usize = 0;

        for c in s.as_bytes() {
            if *c < b'0' || b'7' < *c {
                return Err(ParseOctalError(String::from(s)));
            }

            v = v * 8 + (c - b'0') as usize;
        }

        Ok(v)
    }

    pub fn name(&self) -> &str {
        self.ar_member_name.as_ref().unwrap_or(&self.ar_name).as_str()
    }
}

/// Structure of the __.SYMDEF table of contents for an archive.
///
/// __.SYMDEF begins with a long giving the size in bytes of the ranlib
/// structures which immediately follow, and then continues with a string
/// table consisting of a long giving the number of bytes of strings which
/// follow and then the strings themselves.  The `ran_strx` fields index the
/// string table whose first byte is numbered 0.
///
#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct RanLib {
    // string table index of
    pub ran_strx: off_t,
    pub ran_off: off_t,
}

/// The abstract file block, including mach-o file, fat/universal file,
/// archive file and symdef block
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum OFile {
    MachFile {
        header: MachHeader,
        commands: Vec<MachCommand>,
    },
    FatFile {
        magic: u32,
        files: Vec<(FatArch, OFile)>,
    },
    ArFile {
        files: Vec<(ArHeader, OFile)>,
    },
    SymDef {
        ranlibs: Vec<RanLib>,
    },
}

impl OFile {
    /// Parse a file base on its magic number
    pub fn parse<T: AsRef<[u8]>>(buf: &mut Cursor<T>) -> Result<OFile> {
        let magic = buf.read_u32::<NativeEndian>()?;

        debug!(
            "0x{:08x}\tparsing ofile header with magic: 0x{:x}",
            buf.position(),
            magic
        );

        match magic {
            MH_MAGIC => Self::parse_mach_file::<Arch32, LittleEndian, T>(magic, buf),
            MH_CIGAM => Self::parse_mach_file::<Arch32, BigEndian, T>(magic, buf),
            MH_MAGIC_64 => Self::parse_mach_file::<Arch64, LittleEndian, T>(magic, buf),
            MH_CIGAM_64 => Self::parse_mach_file::<Arch64, BigEndian, T>(magic, buf),
            FAT_MAGIC | FAT_CIGAM => Self::parse_fat_file::<Arch32, BigEndian, T>(magic, buf),
            FAT_MAGIC64 | FAT_CIGAM64 => Self::parse_fat_file::<Arch64, BigEndian, T>(magic, buf),
            _ => {
                let mut ar_magic = [0; 8];

                buf.seek(SeekFrom::Current(-4))?;

                if buf.get_ref().as_ref().len() < ar_magic.len() {
                    return Err(UnknownMagic(magic));
                }

                buf.read_exact(&mut ar_magic)?;

                if ar_magic == ARMAG {
                    Self::parse_ar_file::<NativeEndian, T>(buf)
                } else {
                    Err(UnknownMagic(magic))
                }
            }
        }
    }

    fn parse_mach_file<P: MachHeaderParser, O: ByteOrder, T: AsRef<[u8]>>(
        magic: u32,
        buf: &mut Cursor<T>,
    ) -> Result<OFile> {
        debug!("0x{:08x}\tparsing macho-o file header", buf.position());

        let header = P::parse_mach_header::<_, O>(magic, buf)?;

        debug!("parsed mach-o file header: {:?}", header);

        let mut commands = Vec::new();

        for _ in 0..header.ncmds as usize {
            let (cmd, cmdsize) = LoadCommand::parse::<O, T>(&header, buf)?;

            commands.push(MachCommand(cmd, cmdsize));
        }

        debug!("parsed {} load commands", commands.len());

        Ok(OFile::MachFile { header, commands })
    }

    fn parse_fat_file<P: FatArchParser, O: ByteOrder, T: AsRef<[u8]>>(
        magic: u32,
        buf: &mut Cursor<T>,
    ) -> Result<OFile> {
        debug!("0x{:08x}\tparsing fat file header", buf.position());

        let nfat_arch = buf.read_u32::<O>()?;

        debug!(
            "0x{:08}\tparsed fat header with {} archs, magic=0x{:x}",
            buf.position(),
            nfat_arch,
            magic
        );

        let archs = (0..nfat_arch)
            .map(|i| {
                let arch = P::parse_fat_arch::<_, O>(buf)?;

                debug!("0x{:08}\tfat header arch#{}, arch={:?}", buf.position(), i, arch);

                Ok(arch)
            })
            .collect::<Result<Vec<_>>>()?;

        let start = buf.position();
        let payload = buf.get_ref().as_ref();
        let files = archs
            .into_iter()
            .map(|arch| {
                if u64::from(arch.offset) < start {
                    Err(BufferOverflow(arch.offset as usize))
                } else {
                    let content = payload.checked_slice(arch.offset as usize, arch.size as usize)?;

                    debug!("0x{:08}\tparsing mach-o file, arch={:?}", arch.offset, arch);

                    let mut cur = Cursor::new(content);

                    let file = OFile::parse(&mut cur)?;

                    Ok((arch, file))
                }
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(OFile::FatFile { magic, files })
    }

    fn parse_ar_file<O: ByteOrder, T: AsRef<[u8]>>(buf: &mut Cursor<T>) -> Result<OFile> {
        let mut files = Vec::new();

        loop {
            debug!("0x{:08x}\tparsing ar header", buf.position());

            match ArHeader::parse(buf) {
                Ok(ref mut header) => {
                    let mut end = buf
                        .position()
                        .checked_add(header.ar_size as u64)
                        .ok_or(BufferOverflow(header.ar_size as usize))?;

                    if let Some(size) = header.extended_format_size() {
                        end = end.checked_sub(size as u64).ok_or(BufferOverflow(size))?;
                    }

                    if header.name() == SYMDEF || header.name() == SYMDEF_SORTED {
                        let ranlib_len = buf.read_u32::<O>()? as usize;
                        let ranlib_num = ranlib_len / size_of::<RanLib>();
                        let mut ranlibs = Vec::with_capacity(ranlib_num);

                        for _ in 0..ranlib_num {
                            ranlibs.push(RanLib {
                                ran_strx: buf.read_u32::<O>()?,
                                ran_off: buf.read_u32::<O>()?,
                            })
                        }

                        let toc_strsize = buf.read_u32::<O>()?;

                        debug!(
                            "parsed {} with {} ranlibs and {} bytes string",
                            header.name(),
                            ranlibs.len(),
                            toc_strsize
                        );

                        files.push((header.clone(), OFile::SymDef { ranlibs }))
                    } else {
                        let file = Self::parse(buf)?;

                        debug!(
                            "0x{:08x}\tseek to 0x{:08x}, skip {} bytes",
                            buf.position(),
                            end,
                            end - buf.position()
                        );

                        files.push((header.clone(), file));
                    }

                    buf.seek(SeekFrom::Start(end))?;
                }
                Err(IoError(err)) if err.kind() == ErrorKind::UnexpectedEof => break,
                Err(err) => {
                    warn!("parse ar file failed, {:?}", err);

                    return Err(err);
                }
            }
        }

        debug!("found {} ar header/files", files.len());

        Ok(OFile::ArFile { files })
    }
}

pub trait CheckedSlice<T>: AsRef<[T]> {
    fn checked_slice(&self, off: usize, len: usize) -> Result<&[T]> {
        let s = self.as_ref();
        let start = off as usize;
        let end = off.checked_add(len).ok_or(BufferOverflow(len))? as usize;

        if start >= s.len() || start >= end {
            Err(BufferOverflow(start))
        } else if end > s.len() {
            Err(BufferOverflow(end))
        } else {
            Ok(&s[start..end])
        }
    }
}

impl<T, S> CheckedSlice<T> for S where S: AsRef<[T]> {}

#[cfg(test)]
pub mod tests {
    use std::io::Cursor;

    use byteorder::{BigEndian, LittleEndian};

    use super::super::*;
    use super::{Arch64, MachHeaderParser};

    /**
    Mach header
          magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      flags
     0xcefaedfe      18         10  0x00           2    14       1600 0x00000085
    **/
    const MACH_HEADER_32_DATA: &[u8] = &[
        // magic
        0xFE, 0xED, 0xFA, 0xCE, // cputype
        0x00, 0x00, 0x00, 0x12, // cpusubtype
        0x00, 0x00, 0x00, 0x0A, // filetype
        0x00, 0x00, 0x00, 0x02, // ncmds
        0x00, 0x00, 0x00, 0x0E, // sizeofcmds
        0x00, 0x00, 0x06, 0x40, // flags
        0x00, 0x00, 0x00, 0x85,
    ];

    /**
    Mach header
          magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      flags
     0xfeedfacf 16777223          3  0x80           2    15       2080 0x00a18085
    **/
    const MACH_HEADER_64_DATA: &[u8] = &[
        // magic
        0xcf, 0xfa, 0xed, 0xfe, // cputype
        0x7, 0x0, 0x0, 0x1, // cpusubtype
        0x3, 0x0, 0x0, 0x80, // filetype
        0x2, 0x0, 0x0, 0x0, // ncmds
        0xf, 0x0, 0x0, 0x0, // sizeofcmds
        0x20, 0x8, 0x0, 0x0, // flags
        0x85, 0x80, 0xa1, 0x0, // reserved
        0x0, 0x0, 0x0, 0x0,
    ];

    #[test]
    fn test_parse_mach_32_header() {
        let mut cur = Cursor::new(&MACH_HEADER_32_DATA[4..]);

        let header = Arch64::parse_mach_header::<Cursor<&[u8]>, BigEndian>(MH_CIGAM, &mut cur).unwrap();

        assert_eq!(header.magic, MH_CIGAM);
        assert_eq!(header.cputype, CPU_TYPE_POWERPC);
        assert_eq!(header.cpusubtype, CPU_SUBTYPE_POWERPC_7400);
        assert_eq!(header.filetype, MH_EXECUTE);
        assert_eq!(header.ncmds, 14);
        assert_eq!(header.sizeofcmds, 1600);
        assert_eq!(header.flags, 0x00000085);

        assert!(!header.is_64bit());
        assert!(header.is_bigend());
    }

    #[test]
    fn test_parse_mach_64_header() {
        let mut cur = Cursor::new(&MACH_HEADER_64_DATA[4..]);

        let header = Arch64::parse_mach_header::<Cursor<&[u8]>, LittleEndian>(MH_MAGIC_64, &mut cur).unwrap();

        assert_eq!(header.magic, MH_MAGIC_64);
        assert_eq!(header.cputype, CPU_TYPE_X86_64);
        assert_eq!(header.cpusubtype, CPU_SUBTYPE_LIB64 | CPU_SUBTYPE_386);
        assert_eq!(header.filetype, MH_EXECUTE);
        assert_eq!(header.ncmds, 15);
        assert_eq!(header.sizeofcmds, 2080);
        assert_eq!(header.flags, 0x00a18085);

        assert!(header.is_64bit());
        assert!(!header.is_bigend());
    }
}
