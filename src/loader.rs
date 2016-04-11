use std::fmt;
use std::io::{BufRead, Seek, SeekFrom, Cursor};

use time;
use byteorder::{ByteOrder, BigEndian, LittleEndian, ReadBytesExt, NativeEndian};

use consts::*;
use errors::*;
use commands::{LoadCommand, LcString};

pub trait MachArch {
    fn parse_mach_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader>;
}

pub enum Arch32 {}
pub enum Arch64 {}

impl MachArch for Arch32 {
    fn parse_mach_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader> {
        let header = MachHeader {
            magic: try!(buf.read_u32::<O>()),
            cputype: try!(buf.read_i32::<O>()),
            cpusubtype: try!(buf.read_i32::<O>()),
            filetype: try!(buf.read_u32::<O>()),
            ncmds: try!(buf.read_u32::<O>()),
            sizeofcmds: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        };

        Ok(header)
    }
}

impl MachArch for Arch64 {
    fn parse_mach_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader> {
        let header = MachHeader {
            magic: try!(buf.read_u32::<O>()),
            cputype: try!(buf.read_i32::<O>()),
            cpusubtype: try!(buf.read_i32::<O>()),
            filetype: try!(buf.read_u32::<O>()),
            ncmds: try!(buf.read_u32::<O>()),
            sizeofcmds: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        };

        buf.consume(4);

        Ok(header)
    }
}

#[derive(Debug, Default, Clone)]
pub struct MachHeader {
    pub magic: u32,
    pub cputype: cpu_type_t,
    pub cpusubtype: cpu_subtype_t,
    pub filetype: u32,
    pub ncmds: u32,
    pub sizeofcmds: u32,
    pub flags: u32,
}

impl fmt::Display for MachHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Mach header\n"));
        try!(write!(f,
                    "      magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      \
                     flags\n"));
        write!(f,
               " 0x{:08x} {:7} {:10}  0x{:02x}  {:10} {:5} {:10} 0x{:08x}\n",
               self.magic,
               self.cputype,
               get_cpu_subtype_type(self.cpusubtype),
               get_cpu_subtype_feature(self.cpusubtype),
               self.filetype,
               self.ncmds,
               self.sizeofcmds,
               self.flags)
    }
}

#[derive(Debug, Clone)]
pub struct MachCommand(pub LoadCommand, pub usize);

impl fmt::Display for MachCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            LoadCommand::Segment {..} |
            LoadCommand::Segment64 {..} => self.print_segment_command(f),
            LoadCommand::DyldInfo { .. } => self.print_dyld_info_command(f),
            LoadCommand::SymTab {..} => self.print_symtab_command(f),
            LoadCommand::DySymTab {..} => self.print_dysymtab_command(f),
            LoadCommand::IdDyLinker(_) |
            LoadCommand::LoadDyLinker(_) |
            LoadCommand::DyLdEnv(_) => self.print_dylinker_command(f),
            LoadCommand::IdFvmLib(_) |
            LoadCommand::LoadFvmLib(_) => self.print_fvmlib_command(f),
            LoadCommand::IdDyLib(_) |
            LoadCommand::LoadDyLib(_) |
            LoadCommand::LoadWeakDyLib(_) |
            LoadCommand::ReexportDyLib(_) |
            LoadCommand::LoadUpwardDylib(_) |
            LoadCommand::LazyLoadDylib(_) => self.print_dylib_command(f),
            LoadCommand::VersionMin {..} => self.print_version_min_command(f),
            LoadCommand::SourceVersion(_) => self.print_source_version_command(f),
            LoadCommand::Uuid(_) => self.print_uuid_command(f),
            LoadCommand::EntryPoint {..} => self.print_entry_point_command(f),
            LoadCommand::CodeSignature(_) |
            LoadCommand::SegmentSplitInfo(_) |
            LoadCommand::FunctionStarts(_) |
            LoadCommand::DataInCode(_) |
            LoadCommand::DylibCodeSignDrs(_) |
            LoadCommand::LinkerOptimizationHint(_) => self.print_linkedit_data_command(f),

            _ => Ok(()),
        }
    }
}

impl MachCommand {
    fn print_segment_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::Segment {ref segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, flags, ref sections} |
            &LoadCommand::Segment64 {ref segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, flags, ref sections} => {
                let is_64bit = if cmd.cmd() == LC_SEGMENT_64 { true } else { false };

                try!(write!(f, "      cmd {}\n", cmd.name()));
                try!(write!(f, "  cmdsize {}\n", cmdsize));
                try!(write!(f, "  segname {}\n", segname));
                if is_64bit {
                    try!(write!(f, "   vmaddr 0x{:016x}\n", vmaddr));
                    try!(write!(f, "   vmsize 0x{:016x}\n", vmsize));
                } else {
                    try!(write!(f, "   vmaddr 0x{:08x}\n", vmaddr));
                    try!(write!(f, "   vmsize 0x{:08x}\n", vmsize));
                }
                try!(write!(f, "  fileoff {}\n", fileoff));
                try!(write!(f, " filesize {}\n", filesize));
                try!(write!(f, "  maxprot 0x{:08x}\n", maxprot));
                try!(write!(f, " initprot 0x{:08x}\n", initprot));
                try!(write!(f, "   nsects {}\n", sections.len()));
                try!(write!(f, "    flags 0x{:x}\n", flags.bits()));

                for ref section in sections {
                    try!(write!(f, "Section\n"));
                    try!(write!(f, "  sectname {}\n", section.sectname));
                    try!(write!(f, "   segname {}{}", section.segname,
                        if *segname != section.segname { " (does not match segment)\n" } else { "\n" }));
                    if is_64bit {
                        try!(write!(f, "      addr 0x{:016x}\n", section.addr));
                        try!(write!(f, "      size 0x{:016x}\n", section.size));
                    } else {
                        try!(write!(f, "      addr 0x{:08x}\n", section.addr));
                        try!(write!(f, "      size 0x{:08x}\n", section.size));
                    }
                    try!(write!(f, "    offset {}\n", section.offset));
                    try!(write!(f, "     align 2^{} ({})\n", section.align, 1<<section.align));
                    try!(write!(f, "    reloff {}\n", section.reloff));
                    try!(write!(f, "    nreloc {}\n", section.nreloc));
                    let flags :u32 = section.flags.into();
                    try!(write!(f, "     flags 0x{:08x}\n", flags));
                    try!(write!(f, " reserved1 {}{}", section.reserved1,
                        match section.flags.secttype() {
                            S_SYMBOL_STUBS |
                            S_LAZY_SYMBOL_POINTERS |
                            S_LAZY_DYLIB_SYMBOL_POINTERS |
                            S_NON_LAZY_SYMBOL_POINTERS => {
                                " (index into indirect symbol table)\n"
                            }
                            _ =>{
                                "\n"
                            }
                        }));
                    try!(write!(f, " reserved2 {}{}", section.reserved2,
                        if section.flags.secttype() == S_SYMBOL_STUBS { " (size of stubs)\n" } else { "\n" }));
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

        if let &LoadCommand::DyldInfo { rebase_off, rebase_size, bind_off, bind_size, weak_bind_off, weak_bind_size,
            lazy_bind_off, lazy_bind_size, export_off, export_size} = cmd {
            try!(write!(f, "            cmd {}\n", cmd.name()));
            try!(write!(f, "        cmdsize {}\n", cmdsize));
            try!(write!(f, "     rebase_off {}\n", rebase_off));
            try!(write!(f, "    rebase_size {}\n", rebase_size));
            try!(write!(f, "       bind_off {}\n", bind_off));
            try!(write!(f, "      bind_size {}\n", bind_size));
            try!(write!(f, "  weak_bind_off {}\n", weak_bind_off));
            try!(write!(f, " weak_bind_size {}\n", weak_bind_size));
            try!(write!(f, "  lazy_bind_off {}\n", lazy_bind_off));
            try!(write!(f, " lazy_bind_size {}\n", lazy_bind_size));
            try!(write!(f, "     export_off {}\n", export_off));
            try!(write!(f, "    export_size {}\n", export_size));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_symtab_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::SymTab {symoff, nsyms, stroff, strsize} = cmd {
            try!(write!(f, "     cmd {}\n", cmd.name()));
            try!(write!(f, " cmdsize {}\n", cmdsize));
            try!(write!(f, "  symoff {}\n", symoff));
            try!(write!(f, "   nsyms {}\n", nsyms));
            try!(write!(f, "  stroff {}\n", stroff));
            try!(write!(f, " strsize {}\n", strsize));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_dysymtab_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::DySymTab {ilocalsym, nlocalsym, iextdefsym, nextdefsym, iundefsym, nundefsym, tocoff, ntoc, modtaboff, nmodtab,
                extrefsymoff, nextrefsyms, indirectsymoff, nindirectsyms, extreloff, nextrel, locreloff, nlocrel} = cmd {
            try!(write!(f, "            cmd {}\n", cmd.name()));
            try!(write!(f, "        cmdsize {}\n", cmdsize));
            try!(write!(f, "      ilocalsym {}\n", ilocalsym));
            try!(write!(f, "      nlocalsym {}\n", nlocalsym));
            try!(write!(f, "     iextdefsym {}\n", iextdefsym));
            try!(write!(f, "     nextdefsym {}\n", nextdefsym));
            try!(write!(f, "      iundefsym {}\n", iundefsym));
            try!(write!(f, "      nundefsym {}\n", nundefsym));
            try!(write!(f, "         tocoff {}\n", tocoff));
            try!(write!(f, "           ntoc {}\n", ntoc));
            try!(write!(f, "      modtaboff {}\n", modtaboff));
            try!(write!(f, "        nmodtab {}\n", nmodtab));
            try!(write!(f, "   extrefsymoff {}\n", extrefsymoff));
            try!(write!(f, "    nextrefsyms {}\n", nextrefsyms));
            try!(write!(f, " indirectsymoff {}\n", indirectsymoff));
            try!(write!(f, "  nindirectsyms {}\n", nindirectsyms));
            try!(write!(f, "      extreloff {}\n", extreloff));
            try!(write!(f, "        nextrel {}\n", nextrel));
            try!(write!(f, "      locreloff {}\n", locreloff));
            try!(write!(f, "        nlocrel {}\n", nlocrel));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_dylinker_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::IdDyLinker(LcString(off, ref name)) |
            &LoadCommand::LoadDyLinker(LcString(off, ref name)) |
            &LoadCommand::DyLdEnv(LcString(off, ref name)) => {
                try!(write!(f, "          cmd {}\n", cmd.name()));
                try!(write!(f, "      cmdsize {}\n", cmdsize));
                try!(write!(f, "         name {} (offset {})\n", name, off));

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
            &LoadCommand::IdFvmLib(ref fvmlib) |
            &LoadCommand::LoadFvmLib(ref fvmlib) => {
                try!(write!(f, "           cmd {}\n", cmd.name()));
                try!(write!(f, "       cmdsize {}\n", cmdsize));
                try!(write!(f, " minor version {}\n", fvmlib.minor_version));
                try!(write!(f, "   header addr 0x{:08x}\n", fvmlib.header_addr));

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
            &LoadCommand::IdDyLib(ref dylib) |
            &LoadCommand::LoadDyLib(ref dylib) |
            &LoadCommand::LoadWeakDyLib(ref dylib) |
            &LoadCommand::ReexportDyLib(ref dylib) |
            &LoadCommand::LoadUpwardDylib(ref dylib) |
            &LoadCommand::LazyLoadDylib(ref dylib) => {
                try!(write!(f, "          cmd {}\n", cmd.name()));
                try!(write!(f, "      cmdsize {}\n", cmdsize));
                try!(write!(f,
                            "         name {} (offset {})\n",
                            dylib.name,
                            dylib.name.0));
                let ts = time::at(time::Timespec::new(dylib.timestamp as i64, 0));
                try!(write!(f,
                            "   time stamp {} {}\n",
                            dylib.timestamp,
                            try!(time::strftime("%a %b %e %T %Y", &ts).map_err(|_| fmt::Error))));
                try!(write!(f,
                            "      current version {}.{}.{}\n",
                            dylib.current_version.major(),
                            dylib.current_version.minor(),
                            dylib.current_version.release()));
                try!(write!(f,
                            "compatibility version {}.{}.{}\n",
                            dylib.compatibility_version.major(),
                            dylib.compatibility_version.minor(),
                            dylib.compatibility_version.release()));

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_version_min_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::VersionMin{version, sdk, ..} = cmd {
            try!(write!(f, "      cmd {}\n", cmd.name()));
            try!(write!(f, "  cmdsize {}\n", cmdsize));
            try!(write!(f, "  version {}\n", version));
            try!(write!(f, "      sdk {}\n", sdk));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_source_version_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::SourceVersion(version) = cmd {
            try!(write!(f, "      cmd {}\n", cmd.name()));
            try!(write!(f, "  cmdsize {}\n", cmdsize));
            try!(write!(f, "  version {}\n", version));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_uuid_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::Uuid(ref uuid) = cmd {
            try!(write!(f, "     cmd {}\n", cmd.name()));
            try!(write!(f, " cmdsize {}\n", cmdsize));
            try!(write!(f,
                        "    uuid {}\n",
                        uuid.hyphenated().to_string().to_uppercase()));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_entry_point_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let &LoadCommand::EntryPoint {entryoff, stacksize} = cmd {
            try!(write!(f, "       cmd {}\n", cmd.name()));
            try!(write!(f, "   cmdsize {}\n", cmdsize));
            try!(write!(f, "  entryoff {}\n", entryoff));
            try!(write!(f, " stacksize {}\n", stacksize));

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_linkedit_data_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match cmd {
            &LoadCommand::CodeSignature(ref data) |
            &LoadCommand::SegmentSplitInfo(ref data) |
            &LoadCommand::FunctionStarts(ref data) |
            &LoadCommand::DataInCode(ref data) |
            &LoadCommand::DylibCodeSignDrs(ref data) |
            &LoadCommand::LinkerOptimizationHint(ref data) => {
                try!(write!(f, "      cmd {}\n", cmd.name()));
                try!(write!(f, "  cmdsize {}\n", cmdsize));
                try!(write!(f, "  dataoff {}\n", data.off));
                try!(write!(f, " datasize {}\n", data.size));

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct FatHeader {
    magic: u32,
    archs: Vec<FatArch>,
}

impl fmt::Display for FatHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Fat headers\n"));
        try!(write!(f, "fat_magic 0x{:08x}\n", self.magic));
        try!(write!(f, "nfat_arch {}\n", self.archs.len()));

        for (i, arch) in self.archs.iter().enumerate() {
            try!(write!(f, "architecture {}\n", i));
            try!(write!(f, "    cputype {}\n", arch.cputype));
            try!(write!(f,
                        "    cpusubtype {}\n",
                        get_cpu_subtype_type(arch.cpusubtype)));
            try!(write!(f,
                        "    capabilities 0x{:x}\n",
                        get_cpu_subtype_feature(arch.cpusubtype)));
            try!(write!(f, "    offset {}\n", arch.offset));
            try!(write!(f, "    size {}\n", arch.size));
            try!(write!(f, "    align 2^{} ({})\n", arch.align, 1 << arch.align));
        }

        Ok(())
    }
}

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

impl FatArch {
    fn parse<O: ByteOrder>(&self, buf: &mut Cursor<&[u8]>) -> Result<MachFile> {
        let magic = try!(buf.read_u32::<NativeEndian>());

        try!(buf.seek(SeekFrom::Current(-4)));

        debug!("parsing mach-o file with magic 0x{:x}", magic);

        match magic {
            MH_MAGIC => MachFile::parse::<Arch32, LittleEndian>(buf),
            MH_CIGAM => MachFile::parse::<Arch32, BigEndian>(buf),
            MH_MAGIC_64 => MachFile::parse::<Arch64, LittleEndian>(buf),
            MH_CIGAM_64 => MachFile::parse::<Arch64, BigEndian>(buf),
            _ => Err(Error::LoadError(format!("unknown file format 0x{:x}", magic))),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct MachFile {
    pub header: MachHeader,
    pub commands: Vec<MachCommand>,
}

impl MachFile {
    fn parse<A: MachArch, O: ByteOrder>(buf: &mut Cursor<&[u8]>) -> Result<MachFile> {
        let header = try!(A::parse_mach_header::<Cursor<&[u8]>, O>(buf));

        debug!("parsed file header: {:?}", header);

        let mut commands = Vec::new();

        for _ in 0..header.ncmds as usize {
            let (cmd, cmdsize) = try!(LoadCommand::parse::<O>(buf));

            commands.push(MachCommand(cmd, cmdsize));
        }

        debug!("parsed {} load commands", commands.len());

        Ok(MachFile {
            header: header,
            commands: commands,
        })
    }
}

#[derive(Debug, Default,  Clone)]
pub struct UniversalFile {
    pub header: Option<FatHeader>,
    pub files: Vec<MachFile>,
}

impl UniversalFile {
    pub fn load(buf: &mut Cursor<&[u8]>) -> Result<UniversalFile> {
        let magic = try!(buf.read_u32::<NativeEndian>());

        try!(buf.seek(SeekFrom::Current(-4)));

        debug!("parsing mach-o file with magic 0x{:x}", magic);

        match magic {
            MH_MAGIC => Self::load_mach_file::<Arch32, LittleEndian>(buf),
            MH_CIGAM => Self::load_mach_file::<Arch32, BigEndian>(buf),
            MH_MAGIC_64 => Self::load_mach_file::<Arch64, LittleEndian>(buf),
            MH_CIGAM_64 => Self::load_mach_file::<Arch64, BigEndian>(buf),
            FAT_MAGIC => Self::load_fat_file::<LittleEndian>(buf),
            FAT_CIGAM => Self::load_fat_file::<BigEndian>(buf),
            _ => Err(Error::LoadError(format!("unknown file format 0x{:x}", magic))),
        }
    }

    fn load_mach_file<A: MachArch, O: ByteOrder>(buf: &mut Cursor<&[u8]>) -> Result<UniversalFile> {
        Ok(UniversalFile {
            header: None,
            files: vec![try!(MachFile::parse::<A, O>(buf))],
        })
    }

    fn load_fat_file<O: ByteOrder>(buf: &mut Cursor<&[u8]>) -> Result<UniversalFile> {
        let magic = try!(buf.read_u32::<O>());
        let nfat_arch = try!(buf.read_u32::<O>());

        debug!("parsing fat header with {} archs, magic=0x{:x}",
               nfat_arch,
               magic);

        let mut archs = Vec::new();

        for i in 0..nfat_arch {
            let arch = FatArch {
                cputype: try!(buf.read_u32::<O>()) as cpu_type_t,
                cpusubtype: try!(buf.read_u32::<O>()) as cpu_subtype_t,
                offset: try!(buf.read_u32::<O>()),
                size: try!(buf.read_u32::<O>()),
                align: try!(buf.read_u32::<O>()),
            };

            debug!("fat header arch#{}, arch={:?}", i, arch);

            archs.push(arch);
        }

        let mut files = Vec::new();

        for arch in &archs {
            debug!("parsing mach-o file at 0x{:x}, arch={:?}",
                   arch.offset,
                   arch);

            try!(buf.seek(SeekFrom::Start(arch.offset as u64)));

            files.push(try!(arch.parse::<O>(buf)));
        }

        Ok(UniversalFile {
            header: Some(FatHeader {
                magic: magic,
                archs: archs,
            }),
            files: files,
        })
    }
}

#[cfg(test)]
pub mod tests {
    extern crate env_logger;
    extern crate diff;

    use std::str;
    use std::io::{Write, Cursor};

    use byteorder::LittleEndian;

    use super::super::*;
    use super::{MachArch, Arch64};

    /**
    Mach header
          magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      flags
     0xfeedfacf 16777223          3  0x80           2    15       2080 0x00a18085
    **/
    const MACH_HEADER_64_DATA: [u8; 32] = [0xcf, 0xfa, 0xed, 0xfe, 0x7, 0x0, 0x0, 0x1, 0x3, 0x0,
                                           0x0, 0x80, 0x2, 0x0, 0x0, 0x0, 0xf, 0x0, 0x0, 0x0,
                                           0x20, 0x8, 0x0, 0x0, 0x85, 0x80, 0xa1, 0x0, 0x0, 0x0,
                                           0x0, 0x0];

    static HELLO_WORLD_BIN: &'static [u8] = include_bytes!("../test/helloworld");
    static HELLO_WORLD_LC: &'static str = include_str!("../test/helloworld.lc");
    static HELLO_UNIVERSAL_BIN: &'static [u8] = include_bytes!("../test/helloworld.universal");
    static HELLO_UNIVERSAL_I386_LC: &'static str = include_str!("../test/helloworld.universal.\
                                                                 i386.lc");
    static HELLO_UNIVERSAL_X86_64_LC: &'static str = include_str!("../test/helloworld.universal.\
                                                                   x86_64.lc");
    static HELLO_OBJC_BIN: &'static [u8] = include_bytes!("../test/helloobjc");
    static HELLO_OBJC_LC: &'static str = include_str!("../test/helloobjc.lc");
    static HELLO_RUST_BIN: &'static [u8] = include_bytes!("../test/hellorust");
    static HELLO_RUST_LC: &'static str = include_str!("../test/hellorust.lc");

    macro_rules! setup_test_file {
        ($buf: expr) => ({
            let _ = env_logger::init();

            let mut cursor = Cursor::new($buf);

            UniversalFile::load(&mut cursor).unwrap()
        })
    }

    macro_rules! assert_nodiff {
        ($left:expr, $right:expr) => ({
            let mut w = Vec::new();
            let mut diffs = 0;

            for diff in diff::lines($left, $right) {
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

            assert_eq!($left, $right);
        })
    }

    #[test]
    fn test_parse_mach_header() {
        let mut buf = Vec::new();

        buf.extend_from_slice(&MACH_HEADER_64_DATA[..]);

        let mut cur = Cursor::new(buf.as_slice());

        let header = Arch64::parse_mach_header::<Cursor<&[u8]>, LittleEndian>(&mut cur).unwrap();

        assert_eq!(header.magic, MH_MAGIC_64);
        assert_eq!(header.cputype, CPU_TYPE_X86_64);
        assert_eq!(header.cpusubtype, 0x80000003u64 as i32);
        assert_eq!(header.filetype, MH_EXECUTE);
        assert_eq!(header.ncmds, 15);
        assert_eq!(header.sizeofcmds, 2080);
        assert_eq!(header.flags, 0x00a18085);
    }

    #[test]
    fn test_parse_hello_bin() {
        let file = setup_test_file!(HELLO_WORLD_BIN);

        assert_eq!(file.files.len(), 1);

        let mut w = Vec::<u8>::new();

        write!(w, "helloworld:\n").unwrap();

        let ref file = file.files[0];

        for (i, ref cmd) in file.commands.iter().enumerate() {
            write!(w, "Load command {}\n", i).unwrap();
            write!(w, "{}", cmd).unwrap();
        }

        let dump = str::from_utf8(w.as_slice()).unwrap();

        assert_nodiff!(dump, HELLO_WORLD_LC);
    }

    #[test]
    fn test_parse_hello_objc() {
        let file = setup_test_file!(HELLO_OBJC_BIN);

        assert_eq!(file.files.len(), 1);

        let mut w = Vec::<u8>::new();

        write!(w, "helloobjc:\n").unwrap();

        let ref file = file.files[0];

        for (i, ref cmd) in file.commands.iter().enumerate() {
            write!(w, "Load command {}\n", i).unwrap();
            write!(w, "{}", cmd).unwrap();
        }

        let dump = str::from_utf8(w.as_slice()).unwrap();

        assert_nodiff!(dump, HELLO_OBJC_LC);
    }

    #[test]
    fn test_parse_hello_rust() {
        let file = setup_test_file!(HELLO_RUST_BIN);

        assert_eq!(file.files.len(), 1);

        let mut w = Vec::<u8>::new();

        write!(w, "hellorust:\n").unwrap();

        let ref file = file.files[0];

        for (i, ref cmd) in file.commands.iter().enumerate() {
            write!(w, "Load command {}\n", i).unwrap();
            write!(w, "{}", cmd).unwrap();
        }

        let dump = str::from_utf8(w.as_slice()).unwrap();

        assert_nodiff!(dump, HELLO_RUST_LC);
    }

    #[test]
    fn test_parse_hello_universal() {
        let file = setup_test_file!(HELLO_UNIVERSAL_BIN);

        assert_eq!(file.files.len(), 2);

        for (i, arch_dump) in [HELLO_UNIVERSAL_I386_LC, HELLO_UNIVERSAL_X86_64_LC]
                                  .iter()
                                  .enumerate() {
            let ref file = file.files[i];

            let mut w = Vec::<u8>::new();

            write!(w, "helloworld.universal:\n").unwrap();

            for (i, ref cmd) in file.commands.iter().enumerate() {
                write!(w, "Load command {}\n", i).unwrap();
                write!(w, "{}", cmd).unwrap();
            }

            let dump = str::from_utf8(w.as_slice()).unwrap();

            assert_nodiff!(dump, *arch_dump);
        }
    }
}
