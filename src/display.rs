use std::fmt;

use time;

use loader::{ArHeader, FatHeader, MachCommand, MachHeader};
use commands::{LcString, LoadCommand, Section};
use symbol::Symbol;
use consts::*;

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

impl fmt::Display for ArHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "0{:o} {:3}/{:<3} {:5} {} {}\n",
            self.ar_mode, self.ar_uid, self.ar_gid, self.ar_size, self.ar_date, self.ar_name
        )
    }
}

impl MachCommand {
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
                    dylib.name, dylib.name.0
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
        match self {
            &Symbol::Undefined {
                ref name, external, ..
            } => write!(
                f,
                "                 {} {}",
                if external { "U" } else { "u" },
                name.unwrap_or("")
            ),
            &Symbol::Absolute {
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
            &Symbol::Defined {
                ref name,
                external,
                ref section,
                entry,
                ..
            } => {
                let mut symtype = "s";

                if let &Some(ref section) = section {
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
            &Symbol::Prebound {
                ref name, external, ..
            } => write!(
                f,
                "                 {} {}",
                if external { "P" } else { "p" },
                name.unwrap_or("")
            ),
            &Symbol::Indirect {
                ref name, external, ..
            } => write!(
                f,
                "                 {} {}",
                if external { "I" } else { "i" },
                name.unwrap_or("")
            ),
            &Symbol::Debug { ref name, addr, .. } => if addr == 0 {
                write!(f, "                 d {}", name.unwrap_or(""))
            } else {
                write!(f, "{:016x} d {}", addr, name.unwrap_or(""))
            },
        }
    }
}

#[cfg(test)]
pub mod tests {
    use std::str;
    use std::io::Write;

    use diff;

    use loader::OFile;

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
            let _ = ::pretty_env_logger::try_init();

            let mut cursor = ::std::io::Cursor::new($buf);

            $crate::OFile::parse(&mut cursor).unwrap()
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
