extern crate byteorder;
#[macro_use]
extern crate failure;
extern crate getopts;
#[macro_use]
extern crate log;
extern crate mach_object;
extern crate memmap;
extern crate pretty_env_logger;

use std::mem;
use std::ops::Range;
use std::env;
use std::io;
use std::borrow::Cow;
use std::rc::Rc;
use std::io::{stdout, Cursor, Seek, SeekFrom, Write};
use std::path::Path;
use std::fs::File;
use std::process::exit;

use failure::Error;
use getopts::Options;
use byteorder::ReadBytesExt;
use memmap::Mmap;

use mach_object::*;

const APP_VERSION: &'static str = "0.1.1";

fn print_usage(program: &str, opts: Options) {
    let brief = format!(
        "Usage: {} [-arch arch_type] [options] [--version] <object file> ...",
        program
    );

    print!("{}", opts.usage(&brief));
}

fn main() {
    pretty_env_logger::init();

    let args: Vec<String> = env::args().collect();
    let program = Path::new(args[0].as_str())
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();

    let mut opts = Options::new();

    opts.optopt("", "arch", "Specifies the architecture", "arch_type");
    opts.optflag("f", "", "print the fat headers");
    opts.optflag("a", "", "print the archive headers");
    opts.optflag("h", "", "print the mach header");
    opts.optflag("l", "", "print the load commands");
    opts.optflag("L", "", "print shared libraries used");
    opts.optflag("D", "", "print shared library id name");
    opts.optflag("t", "", "print the text section");
    opts.optflag("d", "", "print the data section");
    opts.optflag("n", "", "print the symbol table");
    opts.optopt("s", "", "print contents of section", "<segname>:<sectname>");
    opts.optflag("S", "", "print the table of contents of a library");
    opts.optflag("X", "", "print no leading addresses or headers");
    opts.optflag("", "bind", "print the mach-o binding info");
    opts.optflag("", "weak-bind", "print the mach-o weak binding info");
    opts.optflag("", "lazy-bind", "print the mach-o lazy binding info");
    opts.optflag("r", "rebase", "print the mach-o rebasing info");
    opts.optflag(
        "",
        "version",
        format!("print the version of {}", program).as_str(),
    );

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(_) => {
            print_usage(&program, opts);

            exit(-1);
        }
    };

    if matches.opt_present("version") {
        println!("{} version {}", program, APP_VERSION);

        exit(0);
    }

    if matches.free.is_empty() {
        println!("at least one file must be specified");

        print_usage(&program, opts);

        exit(-1);
    }

    let mut processor = FileProcessor {
        w: stdout(),
        cpu_type: 0,
        print_headers: !matches.opt_present("X"),
        print_fat_header: matches.opt_present("f"),
        print_archive_header: matches.opt_present("a"),
        print_mach_header: matches.opt_present("h"),
        print_load_commands: matches.opt_present("l"),
        print_shared_lib: matches.opt_present("L") || matches.opt_present("D"),
        print_shared_lib_just_id: matches.opt_present("D") && !matches.opt_present("L"),
        print_text_section: matches.opt_present("t"),
        print_data_section: matches.opt_present("d"),
        print_symbol_table: matches.opt_present("n"),
        print_section: matches.opt_str("s").map(|s| {
            let names: Vec<&str> = s.splitn(2, ':').collect();

            if names.len() == 2 {
                (String::from(names[0]), Some(String::from(names[1])))
            } else {
                (String::from(names[0]), None)
            }
        }),
        print_lib_toc: matches.opt_present("S"),
        print_bind_info: matches.opt_present("bind"),
        print_weak_bind_info: matches.opt_present("weak-bind"),
        print_lazy_bind_info: matches.opt_present("lazy-bind"),
        print_rebase_info: matches.opt_present("r"),
    };

    if let Some(flags) = matches.opt_str("arch") {
        if let Some(&(cpu_type, _)) = get_arch_from_flag(flags.as_str()) {
            processor.cpu_type = cpu_type;
        } else {
            eprintln!("unknown architecture specification flag: arch {}", flags);

            exit(-1);
        }
    }

    for filename in matches.free {
        if let Err(err) = processor.process(filename.as_str()) {
            eprintln!("fail to process file {}, {}", filename, err);

            exit(-1);
        }
    }
}

struct FileProcessor<T: Write> {
    w: T,
    cpu_type: cpu_type_t,
    print_headers: bool,
    print_fat_header: bool,
    print_archive_header: bool,
    print_mach_header: bool,
    print_load_commands: bool,
    print_shared_lib: bool,
    print_shared_lib_just_id: bool,
    print_text_section: bool,
    print_data_section: bool,
    print_symbol_table: bool,
    print_section: Option<(String, Option<String>)>,
    print_lib_toc: bool,
    print_bind_info: bool,
    print_weak_bind_info: bool,
    print_lazy_bind_info: bool,
    print_rebase_info: bool,
}

struct FileProcessContext<'a> {
    filename: Cow<'a, str>,
    content: &'a [u8],
    cur: Cursor<&'a [u8]>,
}

impl<'a> FileProcessContext<'a> {
    fn hexdump(&mut self, addr: usize, size: usize) -> Result<Vec<u8>, Error> {
        let mut w = Vec::new();

        for off in 0..size {
            if (off % 16) == 0 {
                if off > 0 {
                    writeln!(&mut w, "")?;
                }

                write!(&mut w, "{:016x}\t", addr + off)?;
            }

            write!(&mut w, "{:02x} ", self.cur.read_u8()?)?;
        }

        writeln!(&mut w, "")?;

        Ok(w)
    }
}

impl<T: Write> FileProcessor<T> {
    fn process(&mut self, filename: &str) -> Result<(), Error> {
        let file = File::open(filename)?;
        let mmap = unsafe { Mmap::map(&file) }?;
        let content = mmap.as_ref();
        let mut cur = Cursor::new(content);
        let file = OFile::parse(&mut cur)?;
        let mut ctxt = FileProcessContext {
            filename: filename.into(),
            content,
            cur,
        };

        debug!("process file {} with {} bytes", filename, mmap.len());

        self.process_ofile(&file, &mut ctxt)?;

        if self.print_symbol_table {
            debug!("dumping symbol table");

            if let Some(symbols) = file.symbols(&mut ctxt.cur) {
                for symbol in symbols {
                    writeln!(self.w, "{}", symbol)?;
                }
            }
        }

        Ok(())
    }

    fn process_ofile(&mut self, ofile: &OFile, ctxt: &mut FileProcessContext) -> Result<(), Error> {
        match ofile {
            &OFile::MachFile {
                ref header,
                ref commands,
            } => self.process_mach_file(&header, &commands, ctxt),
            &OFile::FatFile { magic, ref files } => self.process_fat_file(magic, files, ctxt),
            &OFile::ArFile { ref files } => self.process_ar_file(files, ctxt),
            &OFile::SymDef { ref ranlibs } => self.process_symdef(ranlibs, ctxt),
        }
    }

    fn print_mach_file(&self) -> bool {
        self.print_mach_header | self.print_load_commands | self.print_text_section | self.print_data_section
            | self.print_shared_lib
    }

    fn process_mach_file(
        &mut self,
        header: &MachHeader,
        commands: &[MachCommand],
        ctxt: &mut FileProcessContext,
    ) -> Result<(), Error> {
        if self.cpu_type != 0 && self.cpu_type != CPU_TYPE_ANY && self.cpu_type != header.cputype {
            return Ok(());
        }

        if self.print_headers && self.print_mach_file() {
            if self.cpu_type != 0 {
                writeln!(
                    self.w,
                    "{} (architecture {}):",
                    ctxt.filename,
                    get_arch_name_from_types(header.cputype, header.cpusubtype).unwrap_or(
                        format!(
                            "cputype {} cpusubtype {}",
                            header.cputype, header.cpusubtype
                        ).as_str()
                    )
                )?;
            } else {
                writeln!(self.w, "{}:", ctxt.filename)?;
            }
        }

        if self.print_mach_header {
            write!(self.w, "{}", header)?;
        }

        if self.print_load_commands {
            for (i, ref cmd) in commands.iter().enumerate() {
                writeln!(self.w, "Load command {}", i)?;
                write!(self.w, "{}", cmd)?;
            }
        }

        for cmd in commands {
            let &MachCommand(ref cmd, _) = cmd;

            match cmd {
                &LoadCommand::Segment { ref sections, .. } | &LoadCommand::Segment64 { ref sections, .. } => {
                    for ref sect in sections {
                        let name = Some((sect.segname.clone(), Some(sect.sectname.clone())));

                        if name == self.print_section || Some((sect.segname.clone(), None)) == self.print_section
                            || (self.print_text_section
                                && name == Some((String::from(SEG_TEXT), Some(String::from(SECT_TEXT)))))
                            || (self.print_data_section
                                && name == Some((String::from(SEG_DATA), Some(String::from(SECT_DATA)))))
                        {
                            if self.print_headers {
                                writeln!(
                                    self.w,
                                    "Contents of ({},{}) section",
                                    sect.segname, sect.sectname
                                )?;
                            }

                            ctxt.cur.seek(SeekFrom::Start(sect.offset as u64))?;

                            let dump = ctxt.hexdump(sect.addr, sect.size)?;

                            self.w.write(&dump[..])?;
                        }
                    }
                }

                &LoadCommand::IdFvmLib(ref fvmlib) | &LoadCommand::LoadFvmLib(ref fvmlib)
                    if self.print_shared_lib && !self.print_shared_lib_just_id =>
                {
                    writeln!(
                        self.w,
                        "\t{} (minor version {})",
                        fvmlib.name, fvmlib.minor_version
                    )?;
                }

                &LoadCommand::IdDyLib(ref dylib)
                | &LoadCommand::LoadDyLib(ref dylib)
                | &LoadCommand::LoadWeakDyLib(ref dylib)
                | &LoadCommand::ReexportDyLib(ref dylib)
                | &LoadCommand::LoadUpwardDylib(ref dylib)
                | &LoadCommand::LazyLoadDylib(ref dylib)
                    if self.print_shared_lib && (cmd.cmd() == LC_ID_DYLIB || !self.print_shared_lib_just_id) =>
                {
                    if self.print_shared_lib_just_id {
                        write!(self.w, "{}", dylib.name)?;
                    } else {
                        writeln!(
                            self.w,
                            "\t{} (compatibility version {}.{}.{}, current version {}.{}.{})",
                            dylib.name,
                            dylib.compatibility_version.major(),
                            dylib.compatibility_version.minor(),
                            dylib.compatibility_version.release(),
                            dylib.current_version.major(),
                            dylib.current_version.minor(),
                            dylib.current_version.release()
                        )?;
                    }
                }

                &LoadCommand::DyldInfo {
                    bind_off,
                    bind_size,
                    weak_bind_off,
                    weak_bind_size,
                    lazy_bind_off,
                    lazy_bind_size,
                    rebase_off,
                    rebase_size,
                    ..
                } => {
                    if self.print_bind_info {
                        writeln!(self.w, "Bind table:")?;

                        self.process_bind_info(BindType::Bind, bind_off, bind_size, ctxt.content, commands)?;
                    }

                    if self.print_weak_bind_info {
                        writeln!(self.w, "Weak bind table:")?;

                        self.process_bind_info(
                            BindType::Weak,
                            weak_bind_off,
                            weak_bind_size,
                            ctxt.content,
                            commands,
                        )?;
                    }

                    if self.print_lazy_bind_info {
                        writeln!(self.w, "Lazy bind table:")?;

                        self.process_bind_info(
                            BindType::Lazy,
                            lazy_bind_off,
                            lazy_bind_size,
                            ctxt.content,
                            commands,
                        )?;
                    }

                    if self.print_rebase_info {
                        writeln!(self.w, "Rebase table:")?;

                        self.process_rebase_info(rebase_off, rebase_size, ctxt.content, commands)?;
                    }
                }

                _ => {}
            }
        }

        Ok(())
    }

    fn process_fat_file(
        &mut self,
        magic: u32,
        files: &Vec<(FatArch, OFile)>,
        ctxt: &mut FileProcessContext,
    ) -> Result<(), Error> {
        if self.print_fat_header {
            let header = FatHeader {
                magic: magic,
                archs: files.iter().map(|&(ref arch, _)| arch.clone()).collect(),
            };

            write!(self.w, "{}", header)?;
        }

        for &(_, ref file) in files {
            self.process_ofile(file, ctxt)?;
        }

        Ok(())
    }

    fn process_ar_file(&mut self, files: &Vec<(ArHeader, OFile)>, ctxt: &mut FileProcessContext) -> Result<(), Error> {
        if self.print_headers && (self.print_lib_toc || self.print_mach_file()) {
            writeln!(self.w, "Archive :{}", ctxt.filename)?;
        }

        if self.print_archive_header {
            for &(ref header, _) in files {
                write!(self.w, "{}", header)?;
            }
        }

        for &(ref header, ref file) in files {
            self.process_ofile(
                file,
                &mut FileProcessContext {
                    filename: if let Some(ref name) = header.ar_member_name {
                        format!("{}({})", ctxt.filename, name).into()
                    } else {
                        ctxt.filename.clone()
                    },
                    content: ctxt.content,
                    cur: ctxt.cur.clone(),
                },
            )?;
        }

        Ok(())
    }

    fn process_symdef(&mut self, ranlibs: &Vec<RanLib>, ctxt: &mut FileProcessContext) -> Result<(), Error> {
        if self.print_lib_toc {
            writeln!(self.w, "Table of contents from: {}", ctxt.filename)?;
            writeln!(
                self.w,
                "size of ranlib structures: {} (number {})",
                ranlibs.len() * mem::size_of::<RanLib>(),
                ranlibs.len()
            )?;
            writeln!(self.w, "object offset  string index")?;

            for ref ranlib in ranlibs {
                writeln!(self.w, "{:<14} {}", ranlib.ran_off, ranlib.ran_strx)?;
            }
        }

        Ok(())
    }

    fn process_bind_info(
        &mut self,
        bind_type: BindType,
        offset: u32,
        size: u32,
        payload: &[u8],
        commands: &[MachCommand],
    ) -> Result<(), Error> {
        let start = offset as usize;
        let end = (offset + size) as usize;

        if start > payload.len() {
            bail!("bind_off in LC_DYLD_INFO load command pass end of file");
        }
        if end > payload.len() {
            bail!("bind_off plus bind_size in LC_DYLD_INFO load command past end of file");
        }

        let dylibs = commands
            .iter()
            .flat_map(|cmd| match cmd.command() {
                &LoadCommand::IdDyLib(ref dylib)
                | &LoadCommand::LoadDyLib(ref dylib)
                | &LoadCommand::LoadWeakDyLib(ref dylib)
                | &LoadCommand::ReexportDyLib(ref dylib)
                | &LoadCommand::LoadUpwardDylib(ref dylib)
                | &LoadCommand::LazyLoadDylib(ref dylib) => Some(dylib),
                _ => None,
            })
            .collect::<Vec<&DyLib>>();

        self.print_bind_header(bind_type)?;

        let mut dylib_name = None;
        let mut symbol = None;
        let mut symtype = SymbolType::Pointer;
        let mut symoff = 0;
        let mut sym_addend = 0;
        let mut segment = None;
        let mut lazyoff = 0;

        for opcode in BindOpCode::parse(&payload[start..end]) {
            trace!("Bind OpCode: {:?}", opcode);

            match opcode {
                BindOpCode::Done => match bind_type {
                    BindType::Bind | BindType::Weak => break,
                    _ => lazyoff = 0,
                },
                BindOpCode::SetDyLibrary(ordinal) => {
                    dylib_name = match ordinal {
                        BIND_SPECIAL_DYLIB_SELF => Some("this-image"),
                        BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE => Some("main-executable"),
                        BIND_SPECIAL_DYLIB_FLAT_LOOKUP => Some("flat-namespace"),
                        index if index > dylibs.len() as isize => {
                            bail!("libraryOrdinal out of range");
                        }
                        index if index > 0 => dylibs
                            .get((index - 1) as usize)
                            .and_then(|dylib| Path::new(dylib.name.as_str()).file_name())
                            .and_then(|filename| filename.to_str())
                            .and_then(|filename| filename.split('.').next()),
                        _ => {
                            bail!("unknown special ordinal");
                        }
                    };
                }
                BindOpCode::SetSymbol { name, flags } => {
                    symbol = Some((name, flags));
                }
                BindOpCode::SetSymbolType(symbol_type) => {
                    symtype = symbol_type;
                }
                BindOpCode::SetAddend(addend) => {
                    sym_addend = addend;
                }
                BindOpCode::SetSegmentOffset {
                    segment_index,
                    segment_offset,
                } => {
                    segment = self.get_section(commands, segment_index);

                    symoff = segment_offset as isize;
                }
                BindOpCode::AddAddress { offset } => {
                    symoff += offset;
                }
                BindOpCode::Bind => {
                    self.print_bind_symbol(
                        bind_type,
                        &segment,
                        symoff,
                        dylib_name,
                        &symbol,
                        symtype,
                        sym_addend,
                    )?;

                    symoff += POINTER_BYTES as isize;
                }
                BindOpCode::BindAndAddAddress { offset } => {
                    self.print_bind_symbol(
                        bind_type,
                        &segment,
                        symoff,
                        dylib_name,
                        &symbol,
                        symtype,
                        sym_addend,
                    )?;

                    symoff += offset + POINTER_BYTES as isize;
                }
                BindOpCode::BindAndSkipping { times, skip } => for _ in 0..times {
                    self.print_bind_symbol(
                        bind_type,
                        &segment,
                        symoff,
                        dylib_name,
                        &symbol,
                        symtype,
                        sym_addend,
                    )?;

                    symoff += (skip + POINTER_BYTES) as isize;
                },
            }
        }

        Ok(())
    }

    fn print_bind_header(&mut self, bind_type: BindType) -> io::Result<()> {
        match bind_type {
            BindType::Bind => writeln!(
                self.w,
                "segment  section            address    type       addend dylib            symbol"
            ),
            BindType::Weak => writeln!(
                self.w,
                "segment section          address       type     addend symbol"
            ),
            BindType::Lazy => writeln!(
                self.w,
                "segment section          address    index  dylib            symbol"
            ),
        }
    }

    fn print_bind_symbol(
        &mut self,
        bind_type: BindType,
        segment: &Option<(&str, Range<usize>, &[Rc<Section>])>,
        symoff: isize,
        dylib_name: Option<&str>,
        symbol: &Option<(String, BindSymbolFlags)>,
        symtype: SymbolType,
        sym_addend: usize,
    ) -> Result<(), Error> {
        let &(segname, ref vmrange, ref sections) = segment
            .as_ref()
            .ok_or_else(|| format_err!("segment missed"))?;
        let &(ref symname, symflags) = symbol.as_ref().ok_or_else(|| format_err!("symbol missed"))?;

        let addr = (vmrange.start as isize + symoff) as usize;

        if addr >= vmrange.end {
            bail!("address 0x{:016x} out of range", addr);
        }

        let secname = self.section_name(sections, addr);

        match bind_type {
            BindType::Bind => {
                let dylib_name = dylib_name.ok_or_else(|| format_err!("dylib name missed"))?;

                writeln!(
                    self.w,
                    "{:8} {:16} 0x{:08X} {:10}  {:5} {:<16} {}{}",
                    segname,
                    secname,
                    addr,
                    symtype,
                    sym_addend,
                    dylib_name,
                    symname,
                    if symflags.contains(BindSymbolFlags::WEAK_IMPORT) {
                        " (weak import)"
                    } else {
                        ""
                    }
                )?;
            }
            BindType::Weak => {
                writeln!(
                    self.w,
                    "{:8} {:16} 0x{:08X} {:10}  {:5} {}{}",
                    segname,
                    secname,
                    addr,
                    symtype,
                    sym_addend,
                    symname,
                    if symflags.contains(BindSymbolFlags::NON_WEAK_DEFINITION) {
                        " (strong)"
                    } else {
                        ""
                    }
                )?;
            }
            BindType::Lazy => {
                let dylib_name = dylib_name.ok_or_else(|| format_err!("dylib name missed"))?;

                writeln!(
                    self.w,
                    "{:8} {:16} 0x{:08X} {:<16} {}{}",
                    segname,
                    secname,
                    addr,
                    dylib_name,
                    symname,
                    if symflags.contains(BindSymbolFlags::WEAK_IMPORT) {
                        " (weak import)"
                    } else {
                        ""
                    }
                )?;
            }
        }

        Ok(())
    }

    fn process_rebase_info(
        &mut self,
        offset: u32,
        size: u32,
        payload: &[u8],
        commands: &[MachCommand],
    ) -> Result<(), Error> {
        debug!("process rebase info @ 0x{:08x} with {} bytes", offset, size);

        let start = offset as usize;
        let end = (offset + size) as usize;

        if start > payload.len() {
            bail!("rebase_off in LC_DYLD_INFO load command pass end of file");
        }
        if end > payload.len() {
            bail!("rebase_off plus bind_size in LC_DYLD_INFO load command past end of file");
        }

        writeln!(self.w, "segment  section            address     type")?;

        let mut segment = None;
        let mut symoff: isize = 0;
        let mut symtype = SymbolType::Pointer;

        for opcode in RebaseOpCode::parse(&payload[start..end]) {
            trace!("Rebase OpCode: {:?}", opcode);

            match opcode {
                RebaseOpCode::Done => {
                    break;
                }
                RebaseOpCode::SetSymbolType(symbol_type) => {
                    symtype = symbol_type;
                }
                RebaseOpCode::SetSegmentOffset {
                    segment_index,
                    segment_offset,
                } => {
                    segment = self.get_section(commands, segment_index);

                    symoff = segment_offset as isize;
                }
                RebaseOpCode::AddAddress { offset } => {
                    symoff += offset;
                }
                RebaseOpCode::Rebase { times } => for _ in 0..times {
                    self.print_rebase_symbol(&segment, symoff, symtype)?;

                    symoff += POINTER_BYTES as isize;
                },
                RebaseOpCode::RebaseAndAddAddress { offset } => {
                    self.print_rebase_symbol(&segment, symoff, symtype)?;

                    symoff += offset + POINTER_BYTES as isize;
                }
                RebaseOpCode::RebaseAndSkipping { times, skip } => for _ in 0..times {
                    self.print_rebase_symbol(&segment, symoff, symtype)?;

                    symoff += (skip + POINTER_BYTES) as isize;
                },
            }
        }

        Ok(())
    }

    fn print_rebase_symbol(
        &mut self,
        segment: &Option<(&str, Range<usize>, &[Rc<Section>])>,
        symoff: isize,
        symtype: SymbolType,
    ) -> Result<(), Error> {
        let &(segname, ref vmrange, sections) = segment
            .as_ref()
            .ok_or_else(|| format_err!("segment missed"))?;
        let addr = (vmrange.start as isize + symoff) as usize;

        if addr >= vmrange.end {
            bail!("address 0x{:016x} out of range", addr);
        }

        let secname = self.section_name(sections, addr);

        writeln!(
            self.w,
            "{:8} {:18} 0x{:08X}  {}",
            segname, secname, addr, symtype
        )?;

        Ok(())
    }

    fn get_section<'a>(
        &self,
        commands: &'a [MachCommand],
        segment_index: u8,
    ) -> Option<(&'a str, Range<usize>, &'a [Rc<Section>])> {
        commands
            .get(segment_index as usize)
            .and_then(|cmd| match cmd.command() {
                &LoadCommand::Segment {
                    ref segname,
                    vmaddr,
                    vmsize,
                    ref sections,
                    ..
                }
                | &LoadCommand::Segment64 {
                    ref segname,
                    vmaddr,
                    vmsize,
                    ref sections,
                    ..
                } => Some((
                    segname.as_str(),
                    (vmaddr..vmaddr + vmsize),
                    sections.as_slice(),
                )),
                _ => None,
            })
    }

    fn section_name(&self, sections: &[Rc<Section>], addr: usize) -> String {
        sections
            .iter()
            .find(|section| section.addr <= addr && section.addr + section.size > addr)
            .map(|section| section.sectname.clone())
            .unwrap_or_default()
    }
}

#[derive(Clone, Copy, Debug)]
enum BindType {
    Bind,
    Weak,
    Lazy,
}
