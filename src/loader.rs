#![allow(non_camel_case_types)]
use std::fmt;
use std::convert::From;
use std::mem::size_of;
use std::io::{BufRead, Cursor, ErrorKind, Read, Seek, SeekFrom};

use libc;
use time;
use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian, ReadBytesExt};

use consts::*;
use errors::*;
use commands::{LcString, LoadCommand, ReadStringExt};

/// The architecture of mach header
///
pub trait MachArch {
    /// parse mach header
    fn parse_mach_header<T: BufRead, O: ByteOrder>(magic: u32, buf: &mut T) -> Result<MachHeader>;
}

/// The 32-bit mach header
pub enum Arch32 {}
/// The 64-bit mach header
pub enum Arch64 {}

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

impl MachArch for Arch32 {
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

impl MachArch for Arch64 {
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

impl fmt::Display for MachHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Mach header\n")?;
        write!(
            f,
            "      magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      \
             flags\n"
        )?;
        write!(
            f,
            " 0x{:08x} {:7} {:10}  0x{:02x}  {:10} {:5} {:10} 0x{:08x}\n",
            self.magic,
            self.cputype,
            get_cpu_subtype_type(self.cpusubtype),
            get_cpu_subtype_feature(self.cpusubtype),
            self.filetype,
            self.ncmds,
            self.sizeofcmds,
            self.flags
        )
    }
}

/// Wrap load command with size in the Mach-O file
#[derive(Debug, Clone)]
pub struct MachCommand(pub LoadCommand, pub usize);

impl fmt::Display for MachCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            LoadCommand::Segment { .. } | LoadCommand::Segment64 { .. } => self.print_segment_command(f),
            LoadCommand::DyldInfo { .. } => self.print_dyld_info_command(f),
            LoadCommand::SymTab { .. } => self.print_symtab_command(f),
            LoadCommand::DySymTab { .. } => self.print_dysymtab_command(f),
            LoadCommand::IdDyLinker(_) | LoadCommand::LoadDyLinker(_) | LoadCommand::DyLdEnv(_) => {
                self.print_dylinker_command(f)
            }
            LoadCommand::IdFvmLib(_) | LoadCommand::LoadFvmLib(_) => self.print_fvmlib_command(f),
            LoadCommand::IdDyLib(_)
            | LoadCommand::LoadDyLib(_)
            | LoadCommand::LoadWeakDyLib(_)
            | LoadCommand::ReexportDyLib(_)
            | LoadCommand::LoadUpwardDylib(_)
            | LoadCommand::LazyLoadDylib(_) => self.print_dylib_command(f),
            LoadCommand::VersionMin { .. } => self.print_version_min_command(f),
            LoadCommand::SourceVersion(_) => self.print_source_version_command(f),
            LoadCommand::Uuid(_) => self.print_uuid_command(f),
            LoadCommand::EntryPoint { .. } => self.print_entry_point_command(f),
            LoadCommand::CodeSignature(_)
            | LoadCommand::SegmentSplitInfo(_)
            | LoadCommand::FunctionStarts(_)
            | LoadCommand::DataInCode(_)
            | LoadCommand::DylibCodeSignDrs(_)
            | LoadCommand::LinkerOptimizationHint(_) => self.print_linkedit_data_command(f),

            _ => Ok(()),
        }
    }
}

impl MachCommand {
    pub fn command(&self) -> &LoadCommand {
        &self.0
    }

    pub fn size(&self) -> usize {
        self.1
    }

    fn print_segment_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::Segment {
                ref segname,
                vmaddr,
                vmsize,
                fileoff,
                filesize,
                maxprot,
                initprot,
                flags,
                ref sections,
            }
            | &LoadCommand::Segment64 {
                ref segname,
                vmaddr,
                vmsize,
                fileoff,
                filesize,
                maxprot,
                initprot,
                flags,
                ref sections,
            } => {
                let is_64bit = if cmd.cmd() == LC_SEGMENT_64 {
                    true
                } else {
                    false
                };

                write!(f, "      cmd {}\n", cmd.name())?;
                write!(f, "  cmdsize {}\n", cmdsize)?;
                write!(f, "  segname {}\n", segname)?;
                if is_64bit {
                    write!(f, "   vmaddr 0x{:016x}\n", vmaddr)?;
                    write!(f, "   vmsize 0x{:016x}\n", vmsize)?;
                } else {
                    write!(f, "   vmaddr 0x{:08x}\n", vmaddr)?;
                    write!(f, "   vmsize 0x{:08x}\n", vmsize)?;
                }
                write!(f, "  fileoff {}\n", fileoff)?;
                write!(f, " filesize {}\n", filesize)?;
                write!(f, "  maxprot 0x{:08x}\n", maxprot)?;
                write!(f, " initprot 0x{:08x}\n", initprot)?;
                write!(f, "   nsects {}\n", sections.len())?;
                write!(f, "    flags 0x{:x}\n", flags.bits())?;

                for ref section in sections {
                    write!(f, "Section\n")?;
                    write!(f, "  sectname {}\n", section.sectname)?;
                    write!(
                        f,
                        "   segname {}{}",
                        section.segname,
                        if *segname != section.segname {
                            " (does not match segment)\n"
                        } else {
                            "\n"
                        }
                    )?;
                    if is_64bit {
                        write!(f, "      addr 0x{:016x}\n", section.addr)?;
                        write!(f, "      size 0x{:016x}\n", section.size)?;
                    } else {
                        write!(f, "      addr 0x{:08x}\n", section.addr)?;
                        write!(f, "      size 0x{:08x}\n", section.size)?;
                    }
                    write!(f, "    offset {}\n", section.offset)?;
                    write!(
                        f,
                        "     align 2^{} ({})\n",
                        section.align,
                        1 << section.align
                    )?;
                    write!(f, "    reloff {}\n", section.reloff)?;
                    write!(f, "    nreloc {}\n", section.nreloc)?;
                    let flags: u32 = section.flags.into();
                    write!(f, "     flags 0x{:08x}\n", flags)?;
                    write!(
                        f,
                        " reserved1 {}{}",
                        section.reserved1,
                        match section.flags.sect_type() {
                            S_SYMBOL_STUBS
                            | S_LAZY_SYMBOL_POINTERS
                            | S_LAZY_DYLIB_SYMBOL_POINTERS
                            | S_NON_LAZY_SYMBOL_POINTERS => " (index into indirect symbol table)\n",
                            _ => "\n",
                        }
                    )?;
                    write!(
                        f,
                        " reserved2 {}{}",
                        section.reserved2,
                        if section.flags.sect_type() == S_SYMBOL_STUBS {
                            " (size of stubs)\n"
                        } else {
                            "\n"
                        }
                    )?;
                }

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_dyld_info_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::DyldInfo {
            rebase_off,
            rebase_size,
            bind_off,
            bind_size,
            weak_bind_off,
            weak_bind_size,
            lazy_bind_off,
            lazy_bind_size,
            export_off,
            export_size,
        } = cmd
        {
            write!(f, "            cmd {}\n", cmd.name())?;
            write!(f, "        cmdsize {}\n", cmdsize)?;
            write!(f, "     rebase_off 0x{:08x}\n", rebase_off)?;
            write!(f, "    rebase_size {}\n", rebase_size)?;
            write!(f, "       bind_off 0x{:08x}\n", bind_off)?;
            write!(f, "      bind_size {}\n", bind_size)?;
            write!(f, "  weak_bind_off 0x{:08x}\n", weak_bind_off)?;
            write!(f, " weak_bind_size {}\n", weak_bind_size)?;
            write!(f, "  lazy_bind_off 0x{:08x}\n", lazy_bind_off)?;
            write!(f, " lazy_bind_size {}\n", lazy_bind_size)?;
            write!(f, "     export_off 0x{:08x}\n", export_off)?;
            write!(f, "    export_size {}\n", export_size)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_symtab_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::SymTab {
            symoff,
            nsyms,
            stroff,
            strsize,
        } = cmd
        {
            write!(f, "     cmd {}\n", cmd.name())?;
            write!(f, " cmdsize {}\n", cmdsize)?;
            write!(f, "  symoff {}\n", symoff)?;
            write!(f, "   nsyms {}\n", nsyms)?;
            write!(f, "  stroff {}\n", stroff)?;
            write!(f, " strsize {}\n", strsize)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_dysymtab_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::DySymTab {
            ilocalsym,
            nlocalsym,
            iextdefsym,
            nextdefsym,
            iundefsym,
            nundefsym,
            tocoff,
            ntoc,
            modtaboff,
            nmodtab,
            extrefsymoff,
            nextrefsyms,
            indirectsymoff,
            nindirectsyms,
            extreloff,
            nextrel,
            locreloff,
            nlocrel,
        } = cmd
        {
            write!(f, "            cmd {}\n", cmd.name())?;
            write!(f, "        cmdsize {}\n", cmdsize)?;
            write!(f, "      ilocalsym {}\n", ilocalsym)?;
            write!(f, "      nlocalsym {}\n", nlocalsym)?;
            write!(f, "     iextdefsym {}\n", iextdefsym)?;
            write!(f, "     nextdefsym {}\n", nextdefsym)?;
            write!(f, "      iundefsym {}\n", iundefsym)?;
            write!(f, "      nundefsym {}\n", nundefsym)?;
            write!(f, "         tocoff {}\n", tocoff)?;
            write!(f, "           ntoc {}\n", ntoc)?;
            write!(f, "      modtaboff {}\n", modtaboff)?;
            write!(f, "        nmodtab {}\n", nmodtab)?;
            write!(f, "   extrefsymoff {}\n", extrefsymoff)?;
            write!(f, "    nextrefsyms {}\n", nextrefsyms)?;
            write!(f, " indirectsymoff {}\n", indirectsymoff)?;
            write!(f, "  nindirectsyms {}\n", nindirectsyms)?;
            write!(f, "      extreloff {}\n", extreloff)?;
            write!(f, "        nextrel {}\n", nextrel)?;
            write!(f, "      locreloff {}\n", locreloff)?;
            write!(f, "        nlocrel {}\n", nlocrel)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_dylinker_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::IdDyLinker(LcString(off, ref name))
            | &LoadCommand::LoadDyLinker(LcString(off, ref name))
            | &LoadCommand::DyLdEnv(LcString(off, ref name)) => {
                write!(f, "          cmd {}\n", cmd.name())?;
                write!(f, "      cmdsize {}\n", cmdsize)?;
                write!(f, "         name {} (offset {})\n", name, off)?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_fvmlib_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::IdFvmLib(ref fvmlib) | &LoadCommand::LoadFvmLib(ref fvmlib) => {
                write!(f, "           cmd {}\n", cmd.name())?;
                write!(f, "       cmdsize {}\n", cmdsize)?;
                write!(f, " minor version {}\n", fvmlib.minor_version)?;
                write!(f, "   header addr 0x{:08x}\n", fvmlib.header_addr)?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_dylib_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::IdDyLib(ref dylib)
            | &LoadCommand::LoadDyLib(ref dylib)
            | &LoadCommand::LoadWeakDyLib(ref dylib)
            | &LoadCommand::ReexportDyLib(ref dylib)
            | &LoadCommand::LoadUpwardDylib(ref dylib)
            | &LoadCommand::LazyLoadDylib(ref dylib) => {
                write!(f, "          cmd {}\n", cmd.name())?;
                write!(f, "      cmdsize {}\n", cmdsize)?;
                write!(
                    f,
                    "         name {} (offset {})\n",
                    dylib.name,
                    dylib.name.0
                )?;
                let ts = time::at_utc(time::Timespec::new(dylib.timestamp as i64, 0));
                write!(
                    f,
                    "   time stamp {} {}\n",
                    dylib.timestamp,
                    time::strftime("%a %b %e %T %Y %Z", &ts).map_err(|_| fmt::Error)?
                )?;
                write!(
                    f,
                    "      current version {}.{}.{}\n",
                    dylib.current_version.major(),
                    dylib.current_version.minor(),
                    dylib.current_version.release()
                )?;
                write!(
                    f,
                    "compatibility version {}.{}.{}\n",
                    dylib.compatibility_version.major(),
                    dylib.compatibility_version.minor(),
                    dylib.compatibility_version.release()
                )?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_version_min_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::VersionMin { version, sdk, .. } = cmd {
            write!(f, "      cmd {}\n", cmd.name())?;
            write!(f, "  cmdsize {}\n", cmdsize)?;
            write!(f, "  version {}\n", version)?;
            write!(f, "      sdk {}\n", sdk)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_source_version_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::SourceVersion(version) = cmd {
            write!(f, "      cmd {}\n", cmd.name())?;
            write!(f, "  cmdsize {}\n", cmdsize)?;
            write!(f, "  version {}\n", version)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_uuid_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::Uuid(ref uuid) = cmd {
            write!(f, "     cmd {}\n", cmd.name())?;
            write!(f, " cmdsize {}\n", cmdsize)?;
            write!(
                f,
                "    uuid {}\n",
                uuid.hyphenated().to_string().to_uppercase()
            )?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_entry_point_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::EntryPoint {
            entryoff,
            stacksize,
        } = cmd
        {
            write!(f, "       cmd {}\n", cmd.name())?;
            write!(f, "   cmdsize {}\n", cmdsize)?;
            write!(f, "  entryoff {}\n", entryoff)?;
            write!(f, " stacksize {}\n", stacksize)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_linkedit_data_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::CodeSignature(ref data)
            | &LoadCommand::SegmentSplitInfo(ref data)
            | &LoadCommand::FunctionStarts(ref data)
            | &LoadCommand::DataInCode(ref data)
            | &LoadCommand::DylibCodeSignDrs(ref data)
            | &LoadCommand::LinkerOptimizationHint(ref data) => {
                write!(f, "      cmd {}\n", cmd.name())?;
                write!(f, "  cmdsize {}\n", cmdsize)?;
                write!(f, "  dataoff {}\n", data.off)?;
                write!(f, " datasize {}\n", data.size)?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }
}

/// The structures of the file format for "fat" architecture specific file (wrapper design).
/// At the begining of the file there is one FatHeader structure followed by a number of FatArch
/// structures.
///
#[derive(Debug, Default, Clone)]
pub struct FatHeader {
    /// fat magic number identifier
    pub magic: u32,
    /// number of structs that follow
    pub archs: Vec<FatArch>,
}

impl fmt::Display for FatHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Fat headers\n")?;
        write!(f, "fat_magic 0x{:08x}\n", self.magic)?;
        write!(f, "nfat_arch {}\n", self.archs.len())?;

        for (i, arch) in self.archs.iter().enumerate() {
            write!(f, "architecture {}\n", i)?;
            write!(f, "    cputype {}\n", arch.cputype)?;
            write!(
                f,
                "    cpusubtype {}\n",
                get_cpu_subtype_type(arch.cpusubtype)
            )?;
            write!(
                f,
                "    capabilities 0x{:x}\n",
                get_cpu_subtype_feature(arch.cpusubtype)
            )?;
            write!(f, "    offset {}\n", arch.offset)?;
            write!(f, "    size {}\n", arch.size)?;
            write!(f, "    align 2^{} ({})\n", arch.align, 1 << arch.align)?;
        }

        Ok(())
    }
}

/// For each architecture in the file, specified by a pair of cputype and cpusubtype,
/// the FatArch describes the file offset, file size and alignment
/// in the file of the architecture specific member.
///
#[derive(Debug, Default, Clone)]
pub struct FatArch {
    /// cpu specifier (int)
    pub cputype: cpu_type_t,
    /// machine specifier (int)
    pub cpusubtype: cpu_subtype_t,
    /// file offset to this object file
    pub offset: u32,
    /// size of this object file
    pub size: u32,
    /// alignment as a power of 2
    pub align: u32,
}

/// the archive file header
#[derive(Debug, Default, Clone)]
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
                bail!(MachError::ParseOctalError(String::from(s)));
            }

            v = v * 8 + (c - b'0') as usize;
        }

        Ok(v)
    }
}

impl fmt::Display for ArHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "0{:o} {:3}/{:<3} {:5} {} {}\n",
            self.ar_mode,
            self.ar_uid,
            self.ar_gid,
            self.ar_size,
            self.ar_date,
            self.ar_name
        )
    }
}

/// Structure of the __.SYMDEF table of contents for an archive.
///
/// __.SYMDEF begins with a long giving the size in bytes of the ranlib
/// structures which immediately follow, and then continues with a string
/// table consisting of a long giving the number of bytes of strings which
/// follow and then the strings themselves.  The ran_strx fields index the
/// string table whose first byte is numbered 0.
///
#[derive(Debug, Default, Clone)]
pub struct RanLib {
    // string table index of
    pub ran_strx: off_t,
    pub ran_off: off_t,
}

/// The abstract file block, including mach-o file, fat/universal file,
/// archive file and symdef block
#[derive(Debug, Clone)]
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
            FAT_MAGIC => Self::parse_fat_file::<LittleEndian, T>(magic, buf),
            FAT_CIGAM => Self::parse_fat_file::<BigEndian, T>(magic, buf),
            _ => {
                let mut ar_magic = [0; 8];

                buf.seek(SeekFrom::Current(-4))?;
                buf.read_exact(&mut ar_magic)?;

                if ar_magic == ARMAG {
                    Self::parse_ar_file::<NativeEndian, T>(buf)
                } else {
                    bail!(MachError::LoadError(
                        format!("unknown file format 0x{:x}", magic),
                    ))
                }
            }
        }
    }

    fn parse_mach_file<A: MachArch, O: ByteOrder, T: AsRef<[u8]>>(magic: u32, buf: &mut Cursor<T>) -> Result<OFile> {
        debug!("0x{:08x}\tparsing macho-o file header", buf.position());

        let header = A::parse_mach_header::<Cursor<T>, O>(magic, buf)?;

        debug!("parsed mach-o file header: {:?}", header);

        let mut commands = Vec::new();

        for _ in 0..header.ncmds as usize {
            let (cmd, cmdsize) = LoadCommand::parse::<O, T>(buf)?;

            commands.push(MachCommand(cmd, cmdsize));
        }

        debug!("parsed {} load commands", commands.len());

        Ok(OFile::MachFile {
            header: header,
            commands: commands,
        })
    }

    fn parse_fat_file<O: ByteOrder, T: AsRef<[u8]>>(magic: u32, buf: &mut Cursor<T>) -> Result<OFile> {
        debug!("0x{:08x}\tparsing fat file header", buf.position());

        let nfat_arch = buf.read_u32::<O>()?;

        debug!(
            "parsed fat header @ 0x{:08} with {} archs, magic=0x{:x}",
            buf.position(),
            nfat_arch,
            magic
        );

        let mut archs = Vec::new();

        for i in 0..nfat_arch {
            let arch = FatArch {
                cputype: buf.read_u32::<O>()? as cpu_type_t,
                cpusubtype: buf.read_u32::<O>()? as cpu_subtype_t,
                offset: buf.read_u32::<O>()?,
                size: buf.read_u32::<O>()?,
                align: buf.read_u32::<O>()?,
            };

            debug!("fat header arch#{}, arch={:?}", i, arch);

            archs.push(arch);
        }

        let payload = buf.get_ref().as_ref();
        let mut files = Vec::new();

        for arch in archs {
            debug!(
                "parsing mach-o file at 0x{:x}, arch={:?}",
                arch.offset,
                arch
            );

            let start = arch.offset as usize;
            let end = (arch.offset + arch.size) as usize;

            if start >= payload.len() || start >= end {
                bail!(MachError::BufferOverflow(start))
            }

            if end > payload.len() {
                bail!(MachError::BufferOverflow(start))
            }

            let mut cur = Cursor::new(&payload[start..end]);

            let file = OFile::parse(&mut cur)?;

            files.push((arch, file));
        }

        Ok(OFile::FatFile {
            magic: magic,
            files: files,
        })
    }

    fn parse_ar_file<O: ByteOrder, T: AsRef<[u8]>>(buf: &mut Cursor<T>) -> Result<OFile> {
        let mut files = Vec::new();

        loop {
            debug!("0x{:08x}\tparsing ar header", buf.position());

            match ArHeader::parse(buf) {
                Ok(ref mut header) => if let Some(ref member_name) = header.ar_member_name {
                    if member_name == SYMDEF || member_name == SYMDEF_SORTED {
                        let ranlib_len = buf.read_u32::<O>()? as usize;
                        let mut ranlibs = Vec::new();

                        for _ in 0..(ranlib_len / size_of::<RanLib>()) {
                            ranlibs.push(RanLib {
                                ran_strx: buf.read_u32::<O>()?,
                                ran_off: buf.read_u32::<O>()?,
                            })
                        }

                        let toc_strsize = buf.read_u32::<O>()?;

                        let end = buf.position() + toc_strsize as u64;

                        buf.seek(SeekFrom::Start(end))?;

                        debug!(
                            "parsed {} with {} ranlibs and {} bytes string",
                            member_name,
                            ranlibs.len(),
                            toc_strsize
                        );

                        files.push((header.clone(), OFile::SymDef { ranlibs: ranlibs }))
                    } else {
                        let mut end = buf.position() + header.ar_size as u64;

                        if let Some(size) = header.extended_format_size() {
                            end -= size as u64;
                        }

                        let file = Self::parse(buf)?;

                        debug!(
                            "0x{:08x}\tseek to 0x{:08x}, skip {} bytes",
                            buf.position(),
                            end,
                            end - buf.position()
                        );

                        buf.seek(SeekFrom::Start(end))?;

                        files.push((header.clone(), file));
                    }
                },
                Err(err) => {
                    match err.downcast_ref::<::std::io::Error>() {
                        Some(err) if err.kind() == ErrorKind::UnexpectedEof => {
                            break;
                        }
                        _ => {
                            warn!("parse ar file failed, {:?}", err);
                        }
                    }

                    bail!(err)
                }
            }
        }

        debug!("found {} ar header/files", files.len());

        Ok(OFile::ArFile { files: files })
    }
}

#[cfg(test)]
pub mod tests {
    use std::str;
    use std::io::{Cursor, Write};

    use byteorder::{BigEndian, LittleEndian};

    use super::super::*;
    use super::{Arch64, MachArch};

    /**
    Mach header
          magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      flags
     0xcefaedfe      18         10  0x00           2    14       1600 0x00000085
    **/
    const MACH_HEADER_32_DATA: &[u8] = &[
        // magic
        0xFE,
        0xED,
        0xFA,
        0xCE,
        // cputype
        0x00,
        0x00,
        0x00,
        0x12,
        // cpusubtype
        0x00,
        0x00,
        0x00,
        0x0A,
        // filetype
        0x00,
        0x00,
        0x00,
        0x02,
        // ncmds
        0x00,
        0x00,
        0x00,
        0x0E,
        // sizeofcmds
        0x00,
        0x00,
        0x06,
        0x40,
        // flags
        0x00,
        0x00,
        0x00,
        0x85,
    ];

    /**
    Mach header
          magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      flags
     0xfeedfacf 16777223          3  0x80           2    15       2080 0x00a18085
    **/
    const MACH_HEADER_64_DATA: &[u8] = &[
        // magic
        0xcf,
        0xfa,
        0xed,
        0xfe,
        // cputype
        0x7,
        0x0,
        0x0,
        0x1,
        // cpusubtype
        0x3,
        0x0,
        0x0,
        0x80,
        // filetype
        0x2,
        0x0,
        0x0,
        0x0,
        // ncmds
        0xf,
        0x0,
        0x0,
        0x0,
        // sizeofcmds
        0x20,
        0x8,
        0x0,
        0x0,
        // flags
        0x85,
        0x80,
        0xa1,
        0x0,
        // reserved
        0x0,
        0x0,
        0x0,
        0x0,
    ];

    static HELLO_WORLD_BIN: &'static [u8] = include_bytes!("../test/helloworld");
    static HELLO_WORLD_LC: &'static str = include_str!("../test/helloworld.lc");
    static HELLO_UNIVERSAL_BIN: &'static [u8] = include_bytes!("../test/helloworld.universal");
    static HELLO_UNIVERSAL_I386_LC: &'static str = include_str!(
        "../test/helloworld.universal.\
         i386.lc"
    );
    static HELLO_UNIVERSAL_X86_64_LC: &'static str = include_str!(
        "../test/helloworld.universal.\
         x86_64.lc"
    );
    static HELLO_OBJC_BIN: &'static [u8] = include_bytes!("../test/helloobjc");
    static HELLO_OBJC_LC: &'static str = include_str!("../test/helloobjc.lc");
    static HELLO_RUST_BIN: &'static [u8] = include_bytes!("../test/hellorust");
    static HELLO_RUST_LC: &'static str = include_str!("../test/hellorust.lc");

    macro_rules! parse_test_file {
        ($buf: expr) => ({
            let _ = pretty_env_logger::try_init();

            let mut cursor = Cursor::new($buf);

            OFile::parse(&mut cursor).unwrap()
        })
    }

    macro_rules! assert_nodiff {
        ($left:expr, $right:expr) => ({
            let mut w = Vec::new();
            let mut diffs = 0;
            let left = $left.replace("\r\n", "\n");
            let right = $right.replace("\r\n", "\n");

            for diff in diff::lines(&left, &right) {
                match diff {
                    diff::Result::Left(l) => {
                        diffs += 1;
                        write!(w, "-{}\n", l).unwrap()
                    },
                    diff::Result::Both(_, _) => {}
                    diff::Result::Right(r) => {
                        diffs += 1;
                        write!(w, "+{}\n", r).unwrap()
                    },
                }
            }

            if diffs > 0 {
                info!("found {} diffs:\n{}", diffs, String::from_utf8(w).unwrap());
            }

            assert_eq!(&left, &right);
        })
    }

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

    #[test]
    fn test_parse_hello_bin() {
        if let OFile::MachFile { commands, .. } = parse_test_file!(HELLO_WORLD_BIN) {
            let mut w = Vec::<u8>::new();

            write!(w, "helloworld:\n").unwrap();

            for (i, ref cmd) in commands.iter().enumerate() {
                write!(w, "Load command {}\n", i).unwrap();
                write!(w, "{}", cmd).unwrap();
            }

            let dump = str::from_utf8(w.as_slice()).unwrap();

            assert_nodiff!(dump, HELLO_WORLD_LC);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_parse_hello_objc() {
        if let OFile::MachFile { commands, .. } = parse_test_file!(HELLO_OBJC_BIN) {
            let mut w = Vec::<u8>::new();

            write!(w, "helloobjc:\n").unwrap();

            for (i, ref cmd) in commands.iter().enumerate() {
                write!(w, "Load command {}\n", i).unwrap();
                write!(w, "{}", cmd).unwrap();
            }

            let dump = str::from_utf8(w.as_slice()).unwrap();

            assert_nodiff!(dump, HELLO_OBJC_LC);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_parse_hello_rust() {
        if let OFile::MachFile { commands, .. } = parse_test_file!(HELLO_RUST_BIN) {
            let mut w = Vec::<u8>::new();

            write!(w, "hellorust:\n").unwrap();

            for (i, ref cmd) in commands.iter().enumerate() {
                write!(w, "Load command {}\n", i).unwrap();
                write!(w, "{}", cmd).unwrap();
            }

            let dump = str::from_utf8(w.as_slice()).unwrap();

            assert_nodiff!(dump, HELLO_RUST_LC);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_parse_hello_universal() {
        if let OFile::FatFile { ref files, .. } = parse_test_file!(HELLO_UNIVERSAL_BIN) {
            assert_eq!(files.len(), 2);

            for (i, arch_dump) in [HELLO_UNIVERSAL_I386_LC, HELLO_UNIVERSAL_X86_64_LC]
                .iter()
                .enumerate()
            {
                let mut w = Vec::<u8>::new();

                write!(w, "helloworld.universal:\n").unwrap();

                if let (_, OFile::MachFile { ref commands, .. }) = files[i] {
                    for (i, ref cmd) in commands.iter().enumerate() {
                        write!(w, "Load command {}\n", i).unwrap();
                        write!(w, "{}", cmd).unwrap();
                    }

                    let dump = str::from_utf8(w.as_slice()).unwrap();

                    assert_nodiff!(dump, *arch_dump);
                } else {
                    panic!();
                }
            }
        } else {
            panic!();
        }
    }
}
