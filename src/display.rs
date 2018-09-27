use std::fmt;

use time;

use commands::{LcString, LoadCommand, Section};
use consts::*;
use loader::{ArHeader, FatHeader, MachCommand, MachHeader};
use symbol::Symbol;

impl fmt::Display for MachHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Mach header")?;
        writeln!(
            f,
            "      magic cputype cpusubtype  caps    filetype ncmds sizeofcmds      \
             flags"
        )?;
        writeln!(
            f,
            " 0x{:08x} {:7} {:10}  0x{:02x}  {:10} {:5} {:10} 0x{:08x}",
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

impl fmt::Display for FatHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Fat headers")?;
        writeln!(f, "fat_magic 0x{:08x}", self.magic)?;
        writeln!(f, "nfat_arch {}", self.archs.len())?;

        for (i, arch) in self.archs.iter().enumerate() {
            writeln!(f, "architecture {}", i)?;
            writeln!(f, "    cputype {}", arch.cputype)?;
            writeln!(f, "    cpusubtype {}", get_cpu_subtype_type(arch.cpusubtype))?;
            writeln!(f, "    capabilities 0x{:x}", get_cpu_subtype_feature(arch.cpusubtype))?;
            writeln!(f, "    offset {}", arch.offset)?;
            writeln!(f, "    size {}", arch.size)?;
            writeln!(f, "    align 2^{} ({})", arch.align, 1 << arch.align)?;
        }

        Ok(())
    }
}

impl fmt::Display for ArHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "0{:o} {:3}/{:<3} {:5} {} {}",
            self.ar_mode, self.ar_uid, self.ar_gid, self.ar_size, self.ar_date, self.ar_name
        )
    }
}

impl MachCommand {
    fn print_segment_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match *cmd {
            LoadCommand::Segment {
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
            | LoadCommand::Segment64 {
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
                let is_64bit = cmd.cmd() == LC_SEGMENT_64;

                writeln!(f, "      cmd {}", cmd.name())?;
                writeln!(f, "  cmdsize {}", cmdsize)?;
                writeln!(f, "  segname {}", segname)?;
                if is_64bit {
                    writeln!(f, "   vmaddr 0x{:016x}", vmaddr)?;
                    writeln!(f, "   vmsize 0x{:016x}", vmsize)?;
                } else {
                    writeln!(f, "   vmaddr 0x{:08x}", vmaddr)?;
                    writeln!(f, "   vmsize 0x{:08x}", vmsize)?;
                }
                writeln!(f, "  fileoff {}", fileoff)?;
                writeln!(f, " filesize {}", filesize)?;
                writeln!(f, "  maxprot 0x{:08x}", maxprot)?;
                writeln!(f, " initprot 0x{:08x}", initprot)?;
                writeln!(f, "   nsects {}", sections.len())?;
                writeln!(f, "    flags 0x{:x}", flags.bits())?;

                for section in sections {
                    writeln!(f, "Section")?;
                    writeln!(f, "  sectname {}", section.sectname)?;
                    writeln!(
                        f,
                        "   segname {}{}",
                        section.segname,
                        if *segname != section.segname {
                            " (does not match segment)"
                        } else {
                            ""
                        }
                    )?;
                    if is_64bit {
                        writeln!(f, "      addr 0x{:016x}", section.addr)?;
                        writeln!(f, "      size 0x{:016x}", section.size)?;
                    } else {
                        writeln!(f, "      addr 0x{:08x}", section.addr)?;
                        writeln!(f, "      size 0x{:08x}", section.size)?;
                    }
                    writeln!(f, "    offset {}", section.offset)?;
                    writeln!(f, "     align 2^{} ({})", section.align, 1 << section.align)?;
                    writeln!(f, "    reloff {}", section.reloff)?;
                    writeln!(f, "    nreloc {}", section.nreloc)?;
                    let flags: u32 = section.flags.into();
                    writeln!(f, "     flags 0x{:08x}", flags)?;
                    writeln!(
                        f,
                        " reserved1 {}{}",
                        section.reserved1,
                        match section.flags.sect_type() {
                            S_SYMBOL_STUBS
                            | S_LAZY_SYMBOL_POINTERS
                            | S_LAZY_DYLIB_SYMBOL_POINTERS
                            | S_NON_LAZY_SYMBOL_POINTERS => " (index into indirect symbol table)",
                            _ => "",
                        }
                    )?;
                    writeln!(
                        f,
                        " reserved2 {}{}",
                        section.reserved2,
                        if section.flags.sect_type() == S_SYMBOL_STUBS {
                            " (size of stubs)"
                        } else {
                            ""
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

        if let LoadCommand::DyldInfo {
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
        } = *cmd
        {
            writeln!(f, "            cmd {}", cmd.name())?;
            writeln!(f, "        cmdsize {}", cmdsize)?;
            writeln!(f, "     rebase_off 0x{:08x}", rebase_off)?;
            writeln!(f, "    rebase_size {}", rebase_size)?;
            writeln!(f, "       bind_off 0x{:08x}", bind_off)?;
            writeln!(f, "      bind_size {}", bind_size)?;
            writeln!(f, "  weak_bind_off 0x{:08x}", weak_bind_off)?;
            writeln!(f, " weak_bind_size {}", weak_bind_size)?;
            writeln!(f, "  lazy_bind_off 0x{:08x}", lazy_bind_off)?;
            writeln!(f, " lazy_bind_size {}", lazy_bind_size)?;
            writeln!(f, "     export_off 0x{:08x}", export_off)?;
            writeln!(f, "    export_size {}", export_size)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_symtab_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let LoadCommand::SymTab {
            symoff,
            nsyms,
            stroff,
            strsize,
        } = *cmd
        {
            writeln!(f, "     cmd {}", cmd.name())?;
            writeln!(f, " cmdsize {}", cmdsize)?;
            writeln!(f, "  symoff {}", symoff)?;
            writeln!(f, "   nsyms {}", nsyms)?;
            writeln!(f, "  stroff {}", stroff)?;
            writeln!(f, " strsize {}", strsize)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_dysymtab_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let LoadCommand::DySymTab {
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
        } = *cmd
        {
            writeln!(f, "            cmd {}", cmd.name())?;
            writeln!(f, "        cmdsize {}", cmdsize)?;
            writeln!(f, "      ilocalsym {}", ilocalsym)?;
            writeln!(f, "      nlocalsym {}", nlocalsym)?;
            writeln!(f, "     iextdefsym {}", iextdefsym)?;
            writeln!(f, "     nextdefsym {}", nextdefsym)?;
            writeln!(f, "      iundefsym {}", iundefsym)?;
            writeln!(f, "      nundefsym {}", nundefsym)?;
            writeln!(f, "         tocoff {}", tocoff)?;
            writeln!(f, "           ntoc {}", ntoc)?;
            writeln!(f, "      modtaboff {}", modtaboff)?;
            writeln!(f, "        nmodtab {}", nmodtab)?;
            writeln!(f, "   extrefsymoff {}", extrefsymoff)?;
            writeln!(f, "    nextrefsyms {}", nextrefsyms)?;
            writeln!(f, " indirectsymoff {}", indirectsymoff)?;
            writeln!(f, "  nindirectsyms {}", nindirectsyms)?;
            writeln!(f, "      extreloff {}", extreloff)?;
            writeln!(f, "        nextrel {}", nextrel)?;
            writeln!(f, "      locreloff {}", locreloff)?;
            writeln!(f, "        nlocrel {}", nlocrel)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_dylinker_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match *cmd {
            LoadCommand::IdDyLinker(LcString(off, ref name))
            | LoadCommand::LoadDyLinker(LcString(off, ref name))
            | LoadCommand::DyLdEnv(LcString(off, ref name)) => {
                writeln!(f, "          cmd {}", cmd.name())?;
                writeln!(f, "      cmdsize {}", cmdsize)?;
                writeln!(f, "         name {} (offset {})", name, off)?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_fvmlib_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match *cmd {
            LoadCommand::IdFvmLib(ref fvmlib) | LoadCommand::LoadFvmLib(ref fvmlib) => {
                writeln!(f, "           cmd {}", cmd.name())?;
                writeln!(f, "       cmdsize {}", cmdsize)?;
                writeln!(f, " minor version {}", fvmlib.minor_version)?;
                writeln!(f, "   header addr 0x{:08x}", fvmlib.header_addr)?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn print_dylib_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match *cmd {
            LoadCommand::IdDyLib(ref dylib)
            | LoadCommand::LoadDyLib(ref dylib)
            | LoadCommand::LoadWeakDyLib(ref dylib)
            | LoadCommand::ReexportDyLib(ref dylib)
            | LoadCommand::LoadUpwardDylib(ref dylib)
            | LoadCommand::LazyLoadDylib(ref dylib) => {
                writeln!(f, "          cmd {}", cmd.name())?;
                writeln!(f, "      cmdsize {}", cmdsize)?;
                writeln!(f, "         name {} (offset {})", dylib.name, dylib.name.0)?;
                let ts = time::at_utc(time::Timespec::new(i64::from(dylib.timestamp), 0));
                writeln!(
                    f,
                    "   time stamp {} {}",
                    dylib.timestamp,
                    time::strftime("%a %b %e %T %Y %Z", &ts).map_err(|_| fmt::Error)?
                )?;
                writeln!(
                    f,
                    "      current version {}.{}.{}",
                    dylib.current_version.major(),
                    dylib.current_version.minor(),
                    dylib.current_version.release()
                )?;
                writeln!(
                    f,
                    "compatibility version {}.{}.{}",
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

        if let LoadCommand::VersionMin { version, sdk, .. } = *cmd {
            writeln!(f, "      cmd {}", cmd.name())?;
            writeln!(f, "  cmdsize {}", cmdsize)?;
            writeln!(f, "  version {}", version)?;
            writeln!(f, "      sdk {}", sdk)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_source_version_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let LoadCommand::SourceVersion(version) = *cmd {
            writeln!(f, "      cmd {}", cmd.name())?;
            writeln!(f, "  cmdsize {}", cmdsize)?;
            writeln!(f, "  version {}", version)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_uuid_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let LoadCommand::Uuid(ref uuid) = *cmd {
            writeln!(f, "     cmd {}", cmd.name())?;
            writeln!(f, " cmdsize {}", cmdsize)?;
            writeln!(f, "    uuid {}", uuid.to_hyphenated_ref().to_string().to_uppercase())?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_entry_point_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        if let LoadCommand::EntryPoint { entryoff, stacksize } = *cmd {
            writeln!(f, "       cmd {}", cmd.name())?;
            writeln!(f, "   cmdsize {}", cmdsize)?;
            writeln!(f, "  entryoff {}", entryoff)?;
            writeln!(f, " stacksize {}", stacksize)?;

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn print_linkedit_data_command(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MachCommand(ref cmd, cmdsize) = *self;

        match *cmd {
            LoadCommand::CodeSignature(ref data)
            | LoadCommand::SegmentSplitInfo(ref data)
            | LoadCommand::FunctionStarts(ref data)
            | LoadCommand::DataInCode(ref data)
            | LoadCommand::DylibCodeSignDrs(ref data)
            | LoadCommand::LinkerOptimizationHint(ref data) => {
                writeln!(f, "      cmd {}", cmd.name())?;
                writeln!(f, "  cmdsize {}", cmdsize)?;
                writeln!(f, "  dataoff {}", data.off)?;
                writeln!(f, " datasize {}", data.size)?;

                Ok(())
            }
            _ => {
                unreachable!();
            }
        }
    }
}

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

impl<'a> fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Symbol::Undefined { ref name, external, .. } => write!(
                f,
                "                 {} {}",
                if external { "U" } else { "u" },
                name.unwrap_or("")
            ),
            Symbol::Absolute {
                ref name,
                external,
                entry,
                ..
            } => write!(
                f,
                "{:016x} {} {}",
                entry,
                if external { "A" } else { "a" },
                name.unwrap_or("")
            ),
            Symbol::Defined {
                ref name,
                external,
                ref section,
                entry,
                ..
            } => {
                let mut symtype = "s";

                if let Some(ref section) = *section {
                    let Section {
                        ref sectname,
                        ref segname,
                        ..
                    } = **section;

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

                write!(
                    f,
                    "{:016x} {} {}",
                    entry,
                    if external {
                        symtype.to_uppercase()
                    } else {
                        symtype.to_lowercase()
                    },
                    name.unwrap_or("")
                )
            }
            Symbol::Prebound { ref name, external, .. } => write!(
                f,
                "                 {} {}",
                if external { "P" } else { "p" },
                name.unwrap_or("")
            ),
            Symbol::Indirect { ref name, external, .. } => write!(
                f,
                "                 {} {}",
                if external { "I" } else { "i" },
                name.unwrap_or("")
            ),
            Symbol::Debug { ref name, addr, .. } => if addr == 0 {
                write!(f, "                 d {}", name.unwrap_or(""))
            } else {
                write!(f, "{:016x} d {}", addr, name.unwrap_or(""))
            },
        }
    }
}

#[cfg(test)]
pub mod tests {
    use std::io::Write;
    use std::str;

    use diff;

    use loader::OFile;

    static HELLO_WORLD_BIN: &'static [u8] = include_bytes!("../tests/helloworld");
    static HELLO_WORLD_LC: &'static str = include_str!("../tests/helloworld.lc");
    static HELLO_UNIVERSAL_BIN: &'static [u8] = include_bytes!("../tests/helloworld.universal");
    static HELLO_UNIVERSAL_I386_LC: &'static str = include_str!(
        "../tests/helloworld.universal.\
         i386.lc"
    );
    static HELLO_UNIVERSAL_X86_64_LC: &'static str = include_str!(
        "../tests/helloworld.universal.\
         x86_64.lc"
    );
    static HELLO_OBJC_BIN: &'static [u8] = include_bytes!("../tests/helloobjc");
    static HELLO_OBJC_LC: &'static str = include_str!("../tests/helloobjc.lc");
    static HELLO_RUST_BIN: &'static [u8] = include_bytes!("../tests/hellorust");
    static HELLO_RUST_LC: &'static str = include_str!("../tests/hellorust.lc");

    macro_rules! parse_test_file {
        ($buf:expr) => {{
            let _ = ::pretty_env_logger::try_init();

            let mut cursor = ::std::io::Cursor::new($buf);

            $crate::OFile::parse(&mut cursor).unwrap()
        }};
    }

    macro_rules! assert_nodiff {
        ($left:expr, $right:expr) => {{
            let mut w = Vec::new();
            let mut diffs = 0;
            let left = $left.replace("\r\n", "\n");
            let right = $right.replace("\r\n", "\n");

            for diff in diff::lines(&left, &right) {
                match diff {
                    diff::Result::Left(l) => {
                        diffs += 1;
                        writeln!(w, "-{}", l).unwrap()
                    }
                    diff::Result::Both(_, _) => {}
                    diff::Result::Right(r) => {
                        diffs += 1;
                        writeln!(w, "+{}", r).unwrap()
                    }
                }
            }

            if diffs > 0 {
                info!("found {} diffs:\n{}", diffs, String::from_utf8(w).unwrap());
            }

            assert_eq!(&left, &right);
        }};
    }

    #[test]
    fn test_parse_hello_bin() {
        if let OFile::MachFile { commands, .. } = parse_test_file!(HELLO_WORLD_BIN) {
            let mut w = Vec::<u8>::new();

            writeln!(w, "helloworld:").unwrap();

            for (i, ref cmd) in commands.iter().enumerate() {
                writeln!(w, "Load command {}", i).unwrap();
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

            writeln!(w, "helloobjc:").unwrap();

            for (i, ref cmd) in commands.iter().enumerate() {
                writeln!(w, "Load command {}", i).unwrap();
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

            writeln!(w, "hellorust:").unwrap();

            for (i, ref cmd) in commands.iter().enumerate() {
                writeln!(w, "Load command {}", i).unwrap();
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

            for (i, arch_dump) in [HELLO_UNIVERSAL_I386_LC, HELLO_UNIVERSAL_X86_64_LC].iter().enumerate() {
                let mut w = Vec::<u8>::new();

                writeln!(w, "helloworld.universal:").unwrap();

                if let (_, OFile::MachFile { ref commands, .. }) = files[i] {
                    for (i, ref cmd) in commands.iter().enumerate() {
                        writeln!(w, "Load command {}", i).unwrap();
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
