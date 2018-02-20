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
use std::env;
use std::borrow::Cow;
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
    opts.optflag("", "rebase", "print the mach-o rebasing info");
    opts.optflag("", "export", "print the mach-o exported symbols");
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
        print_rebase_info: matches.opt_present("rebase"),
        print_export_trie: matches.opt_present("export"),
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
    print_export_trie: bool,
}

struct FileProcessContext<'a> {
    filename: Cow<'a, str>,
    payload: &'a [u8],
    cur: Cursor<&'a [u8]>,
}

impl<'a> FileProcessContext<'a> {
    pub fn new(filename: &'a str, payload: &'a [u8]) -> FileProcessContext<'a> {
        FileProcessContext {
            filename: filename.into(),
            payload,
            cur: Cursor::new(payload),
        }
    }

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
        let payload = mmap.as_ref();
        let mut cur = Cursor::new(payload);
        let file = OFile::parse(&mut cur)?;
        let mut ctxt = FileProcessContext::new(filename, payload);

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

        let ptr_size = if header.is_64bit() {
            mem::size_of::<u64>()
        } else {
            mem::size_of::<u32>()
        };

        let commands = commands
            .iter()
            .map(|load| load.command())
            .cloned()
            .collect::<Vec<LoadCommand>>();

        for cmd in &commands {
            match *cmd {
                LoadCommand::Segment { ref sections, .. } | LoadCommand::Segment64 { ref sections, .. } => {
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
                                    "payloads of ({},{}) section",
                                    sect.segname, sect.sectname
                                )?;
                            }

                            ctxt.cur.seek(SeekFrom::Start(sect.offset as u64))?;

                            let dump = ctxt.hexdump(sect.addr, sect.size)?;

                            self.w.write(&dump[..])?;
                        }
                    }
                }

                LoadCommand::IdFvmLib(ref fvmlib) | LoadCommand::LoadFvmLib(ref fvmlib)
                    if self.print_shared_lib && !self.print_shared_lib_just_id =>
                {
                    writeln!(
                        self.w,
                        "\t{} (minor version {})",
                        fvmlib.name, fvmlib.minor_version
                    )?;
                }

                LoadCommand::IdDyLib(ref dylib)
                | LoadCommand::LoadDyLib(ref dylib)
                | LoadCommand::LoadWeakDyLib(ref dylib)
                | LoadCommand::ReexportDyLib(ref dylib)
                | LoadCommand::LoadUpwardDylib(ref dylib)
                | LoadCommand::LazyLoadDylib(ref dylib)
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

                LoadCommand::DyldInfo {
                    bind_off,
                    bind_size,
                    weak_bind_off,
                    weak_bind_size,
                    lazy_bind_off,
                    lazy_bind_size,
                    rebase_off,
                    rebase_size,
                    export_off,
                    export_size,
                } => {
                    if self.print_bind_info {
                        let payload = ctxt.payload
                            .checked_slice(bind_off as usize, bind_size as usize)?;

                        writeln!(self.w, "Bind table:")?;
                        writeln!(
                            self.w,
                            "segment  section            address    type       addend dylib            symbol"
                        )?;

                        for symbol in Bind::parse(payload, &commands, ptr_size) {
                            writeln!(self.w, "{}", symbol)?;
                        }
                    }

                    if self.print_weak_bind_info {
                        let payload = ctxt.payload
                            .checked_slice(weak_bind_off as usize, weak_bind_size as usize)?;

                        writeln!(self.w, "Weak bind table:")?;
                        writeln!(
                            self.w,
                            "segment section          address       type     addend symbol"
                        )?;

                        for symbol in WeakBind::parse(payload, &commands, ptr_size) {
                            writeln!(self.w, "{}", symbol)?;
                        }
                    }

                    if self.print_lazy_bind_info {
                        let payload = ctxt.payload
                            .checked_slice(lazy_bind_off as usize, lazy_bind_size as usize)?;

                        writeln!(self.w, "Lazy bind table:")?;

                        for symbol in LazyBind::parse(payload, &commands, ptr_size) {
                            writeln!(self.w, "{}", symbol)?;
                        }
                    }

                    if self.print_rebase_info {
                        let payload = ctxt.payload
                            .checked_slice(rebase_off as usize, rebase_size as usize)?;

                        writeln!(self.w, "Rebase table:")?;
                        writeln!(self.w, "segment  section            address     type")?;

                        for symbol in Rebase::parse(payload, &commands, ptr_size) {
                            writeln!(self.w, "{}", symbol)?;
                        }
                    }

                    if self.print_export_trie {
                        let start = export_off as usize;
                        let end = (export_off + export_size) as usize;

                        if start > ctxt.payload.len() {
                            bail!("export_off in LC_DYLD_INFO load command pass end of file");
                        }
                        if end > ctxt.payload.len() {
                            bail!("export_off plus export_size in LC_DYLD_INFO load command past end of file");
                        }

                        writeln!(self.w, "Exports trie:")?;

                        let mut cur = Cursor::new(&ctxt.payload[..end]);

                        cur.set_position(start as u64);

                        let export = ExportGraph::parse(&mut cur)?;

                        debug!("export trie: {:?}", export);
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

        for &(ref arch, ref file) in files {
            let start = arch.offset as usize;
            let end = arch.offset
                .checked_add(arch.size)
                .ok_or(MachError::NumberOverflow)? as usize;

            if start >= ctxt.payload.len() {
                bail!("file offset overflow, {}", start)
            }

            if end > ctxt.payload.len() {
                bail!("file offset overflow, {}", end)
            }

            let mut ctxt = FileProcessContext::new(&ctxt.filename, &ctxt.payload[start..end]);

            self.process_ofile(file, &mut ctxt)?;
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
                    payload: ctxt.payload,
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
}
