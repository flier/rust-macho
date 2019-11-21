#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use std::env;
use std::fmt;
use std::fs::File;
use std::io::{stdout, Cursor, Seek, SeekFrom, Write};
use std::mem;
use std::ops::Deref;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::rc::Rc;

use byteorder::ReadBytesExt;
use failure::Error;
use memmap::Mmap;
use structopt::StructOpt;

use mach_object::*;

const APP_VERSION: &'static str = "0.1.1";

#[derive(Debug, StructOpt)]
#[structopt(name = "otool", about = "object file displaying tool")]
struct Opt {
    /// Specifies the architecture
    #[structopt(long = "arch", parse(try_from_str = parse_cpu_type))]
    cpu_type: Option<cpu_type_t>,

    /// Print verbosely (symbolically) when possible
    #[structopt(short = "v")]
    print_verbose: bool,

    /// Print no leading addresses or headers
    #[structopt(short = "X")]
    print_headers: bool,

    /// Print the fat headers
    #[structopt(short = "f")]
    print_fat_header: bool,

    /// Print the archive headers
    #[structopt(short = "a")]
    print_archive_header: bool,

    /// Print the mach header
    #[structopt(short = "h")]
    print_mach_header: bool,

    /// Print the load commands
    #[structopt(short = "l")]
    print_load_commands: bool,

    /// Print shared libraries used
    #[structopt(short = "L")]
    print_shared_lib: bool,

    /// Print shared library id name
    #[structopt(short = "D")]
    print_shared_lib_just_id: bool,

    /// Print the text section
    #[structopt(short = "t")]
    print_text_section: bool,

    /// Print the data section
    #[structopt(short = "d")]
    print_data_section: bool,

    /// Print the symbol table
    #[structopt(short = "n")]
    print_symbol_table: bool,

    /// Print contents of section
    #[structopt(name = "segname[:sectname]", short = "s", parse(try_from_str = parse_section))]
    print_sections: Vec<(String, Option<String>)>,

    /// Print the table of contents of a library
    #[structopt(short = "S")]
    print_lib_toc: bool,

    /// Print the mach-o binding info
    #[structopt(long = "bind")]
    print_bind_info: bool,

    /// Print the mach-o weak binding info
    #[structopt(long = "weak")]
    print_weak_bind_info: bool,

    /// Print the mach-o lazy binding info
    #[structopt(long = "lazy")]
    print_lazy_bind_info: bool,

    /// Print the mach-o rebasing info
    #[structopt(long = "rebase")]
    print_rebase_info: bool,

    /// Print the mach-o exported symbols
    #[structopt(long = "export")]
    print_export_trie: bool,

    /// Print the version of program
    #[structopt(long = "version")]
    print_version: bool,

    /// The object files
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
}

fn parse_cpu_type(arch: &str) -> Result<cpu_type_t, Error> {
    get_arch_from_flag(arch)
        .map(|&(cpu_type, _)| cpu_type)
        .ok_or_else(|| format_err!("unknown architecture specification flag: arch {}", arch))
}

fn parse_section(s: &str) -> Result<(String, Option<String>), Error> {
    let mut names = s.splitn(2, ':');
    let segname = names.next().ok_or_else(|| format_err!("missing section name"))?;
    let sectname = names.next().map(|s| s.to_owned());

    Ok((segname.to_owned(), sectname))
}

fn main() {
    pretty_env_logger::init();

    let args: Vec<String> = env::args().collect();
    let program = Path::new(args[0].as_str()).file_name().unwrap().to_str().unwrap();

    let opt = Opt::from_args();

    if opt.print_version {
        println!("{} version {}", program, APP_VERSION);

        exit(0);
    }

    if opt.files.is_empty() {
        println!("at least one file must be specified");

        exit(-1);
    }

    let mut processor = FileProcessor { opt, w: stdout() };

    for filename in processor.files.clone() {
        if let Err(err) = processor.process(&filename) {
            eprintln!("fail to process file {:?}, {}", filename, err);

            exit(-1);
        }
    }
}

struct FileProcessor<T> {
    opt: Opt,
    w: T,
}

impl<T> Deref for FileProcessor<T> {
    type Target = Opt;

    fn deref(&self) -> &Self::Target {
        &self.opt
    }
}

struct FileProcessContext<'a> {
    filename: &'a Path,
    payload: &'a [u8],
    cur: Cursor<&'a [u8]>,
}

impl<'a> FileProcessContext<'a> {
    pub fn new(filename: &'a Path, payload: &'a [u8]) -> FileProcessContext<'a> {
        FileProcessContext {
            filename,
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
    fn process<P: AsRef<Path>>(&mut self, filename: P) -> Result<(), Error> {
        let filename = filename.as_ref();
        let file = File::open(filename)?;
        let mmap = unsafe { Mmap::map(&file) }?;
        let payload = mmap.as_ref();
        let mut cur = Cursor::new(payload);
        let file = OFile::parse(&mut cur)?;
        let mut ctxt = FileProcessContext::new(filename, payload);

        debug!("process file {:?} with {} bytes", filename, mmap.len());

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
        self.print_mach_header
            | self.print_load_commands
            | self.print_text_section
            | self.print_data_section
            | self.print_shared_lib
    }

    fn process_mach_file(
        &mut self,
        header: &MachHeader,
        commands: &[MachCommand],
        ctxt: &mut FileProcessContext,
    ) -> Result<(), Error> {
        if let Some(cpu_type) = self.cpu_type {
            if cpu_type != CPU_TYPE_ANY && cpu_type != header.cputype {
                return Ok(());
            }
        }

        if self.print_headers && self.print_mach_file() {
            if self.cpu_type.is_some() {
                writeln!(
                    self.w,
                    "{:?} (architecture {}):",
                    ctxt.filename,
                    get_arch_name_from_types(header.cputype, header.cpusubtype)
                        .unwrap_or(format!("cputype {} cpusubtype {}", header.cputype, header.cpusubtype).as_str())
                )?;
            } else {
                writeln!(self.w, "{:?}:", ctxt.filename)?;
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
                        let print_section = self.print_sections.iter().any(|(ref segname, ref sectname)| {
                            segname.as_str() == sect.segname
                                && (sectname
                                    .as_ref()
                                    .map_or(true, |sectname| sectname.as_str() == sect.sectname))
                        });
                        let print_text_section =
                            self.print_text_section && (sect.segname == SEG_TEXT && sect.sectname == SECT_TEXT);
                        let print_data_section =
                            self.print_data_section && (sect.segname == SEG_DATA && sect.sectname == SECT_DATA);

                        if print_section || print_text_section || print_data_section {
                            if self.print_headers {
                                writeln!(self.w, "payloads of ({},{}) section", sect.segname, sect.sectname)?;
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
                    writeln!(self.w, "\t{} (minor version {})", fvmlib.name, fvmlib.minor_version)?;
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
                        let payload = ctxt.payload.checked_slice(bind_off as usize, bind_size as usize)?;

                        writeln!(self.w, "Bind table:")?;
                        writeln!(
                            self.w,
                            "segment  section            address    type       addend dylib            symbol"
                        )?;

                        for symbol in Bind::parse(payload, ptr_size) {
                            writeln!(self.w, "{}", BindSymbolFmt(symbol, &commands))?;
                        }
                    }

                    if self.print_weak_bind_info {
                        let payload = ctxt
                            .payload
                            .checked_slice(weak_bind_off as usize, weak_bind_size as usize)?;

                        writeln!(self.w, "Weak bind table:")?;
                        writeln!(self.w, "segment section          address       type     addend symbol")?;

                        for symbol in WeakBind::parse(payload, ptr_size) {
                            writeln!(self.w, "{}", WeakBindSymbolFmt(symbol, &commands))?;
                        }
                    }

                    if self.print_lazy_bind_info {
                        let payload = ctxt
                            .payload
                            .checked_slice(lazy_bind_off as usize, lazy_bind_size as usize)?;

                        writeln!(self.w, "Lazy bind table:")?;

                        for symbol in LazyBind::parse(payload, ptr_size) {
                            writeln!(self.w, "{}", LazyBindSymbolFmt(symbol, &commands))?;
                        }
                    }

                    if self.print_rebase_info {
                        let payload = ctxt.payload.checked_slice(rebase_off as usize, rebase_size as usize)?;

                        writeln!(self.w, "Rebase table:")?;
                        writeln!(self.w, "segment  section            address     type")?;

                        for symbol in Rebase::parse(payload, ptr_size) {
                            writeln!(self.w, "{}", RebaseSymbolFmt(symbol, &commands))?;
                        }
                    }

                    if self.print_export_trie {
                        let payload = ctxt.payload.checked_slice(export_off as usize, export_size as usize)?;

                        writeln!(self.w, "Exports trie:")?;

                        let mut symbols = ExportTrie::parse(payload)?.symbols().collect::<Vec<ExportSymbol>>();

                        symbols.sort_by(|lhs, rhs| lhs.address().cmp(&rhs.address()));

                        for symbol in symbols {
                            writeln!(self.w, "{}", ExportSymbolFmt(symbol, &commands))?;
                        }
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
            let end = arch.offset.checked_add(arch.size).ok_or(MachError::NumberOverflow)? as usize;

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
            writeln!(self.w, "Archive :{:?}", ctxt.filename)?;
        }

        if self.print_archive_header {
            for &(ref header, _) in files {
                write!(self.w, "{}", header)?;
            }
        }

        for &(ref header, ref file) in files {
            let filename = if let Some(ref name) = header.ar_member_name {
                ctxt.filename.join(name)
            } else {
                ctxt.filename.to_owned()
            };

            self.process_ofile(
                file,
                &mut FileProcessContext {
                    filename: &filename,
                    payload: ctxt.payload,
                    cur: ctxt.cur.clone(),
                },
            )?;
        }

        Ok(())
    }

    fn process_symdef(&mut self, ranlibs: &Vec<RanLib>, ctxt: &mut FileProcessContext) -> Result<(), Error> {
        if self.print_lib_toc {
            writeln!(self.w, "Table of contents from: {:?}", ctxt.filename)?;
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

struct BindSymbolFmt<'a>(BindSymbol, &'a [LoadCommand]);

impl<'a> fmt::Display for BindSymbolFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let dylib_name = dylib_name(self.1, self.0.dylib_ordinal).ok_or(fmt::Error)?;
        let (segment_name, segment_range, sections) = segmnet_info(self.1, self.0.segment_index).ok_or(fmt::Error)?;
        let address = (segment_range.start as isize + self.0.symbol_offset) as usize;

        if address >= segment_range.end {
            return Err(fmt::Error);
        }

        let section_name = section_name(sections, address).ok_or(fmt::Error)?;

        write!(
            f,
            "{:8} {:16} 0x{:08X} {:10}  {:5} {:<16} {}{}",
            segment_name,
            section_name,
            address,
            self.0.symbol_type,
            self.0.addend,
            dylib_name,
            self.0.name,
            if self.0.flags.contains(BindSymbolFlags::WEAK_IMPORT) {
                " (weak import)"
            } else {
                ""
            }
        )
    }
}

struct WeakBindSymbolFmt<'a>(WeakBindSymbol, &'a [LoadCommand]);

impl<'a> fmt::Display for WeakBindSymbolFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (segment_name, segment_range, sections) = segmnet_info(self.1, self.0.segment_index).ok_or(fmt::Error)?;
        let address = (segment_range.start as isize + self.0.symbol_offset) as usize;

        if address >= segment_range.end {
            return Err(fmt::Error);
        }

        let section_name = section_name(sections, address).ok_or(fmt::Error)?;

        write!(
            f,
            "{:8} {:16} 0x{:08X} {:10}  {:5} {}{}",
            segment_name,
            section_name,
            address,
            self.0.symbol_type,
            self.0.addend,
            self.0.name,
            if self.0.flags.contains(BindSymbolFlags::NON_WEAK_DEFINITION) {
                " (strong)"
            } else {
                ""
            }
        )
    }
}

struct LazyBindSymbolFmt<'a>(LazyBindSymbol, &'a [LoadCommand]);

impl<'a> fmt::Display for LazyBindSymbolFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let dylib_name = dylib_name(self.1, self.0.dylib_ordinal).ok_or(fmt::Error)?;
        let (segment_name, segment_range, sections) = segmnet_info(self.1, self.0.segment_index).ok_or(fmt::Error)?;
        let address = (segment_range.start as isize + self.0.symbol_offset) as usize;

        if address >= segment_range.end {
            return Err(fmt::Error);
        }

        let section_name = section_name(sections, address).ok_or(fmt::Error)?;

        write!(
            f,
            "{:8} {:16} 0x{:08X} {:<16} {}{}",
            segment_name,
            section_name,
            address,
            dylib_name,
            self.0.name,
            if self.0.flags.contains(BindSymbolFlags::WEAK_IMPORT) {
                " (weak import)"
            } else {
                ""
            }
        )
    }
}

struct RebaseSymbolFmt<'a>(RebaseSymbol, &'a [LoadCommand]);

impl<'a> fmt::Display for RebaseSymbolFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (segment_name, segment_range, sections) = segmnet_info(self.1, self.0.segment_index).ok_or(fmt::Error)?;

        let address = (segment_range.start as isize + self.0.symbol_offset) as usize;

        if address >= segment_range.end {
            return Err(fmt::Error);
        }

        let section_name = section_name(sections, address).ok_or(fmt::Error)?;

        write!(
            f,
            "{:8} {:18} 0x{:08X}  {}",
            segment_name, section_name, address, self.0.symbol_type
        )
    }
}

struct ExportSymbolFmt<'a>(ExportSymbol, &'a [LoadCommand]);

impl<'a> fmt::Display for ExportSymbolFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.symbol {
            ExportType::Reexport { .. } => {
                write!(f, "[re-export] ")?;
            }
            ExportType::Regular { address }
            | ExportType::Weak { address }
            | ExportType::Stub { offset: address, .. } => {
                write!(f, "0x{:08X}  ", address)?;
            }
        }

        write!(f, "{}", self.0.name)?;

        let mut attrs = vec![];

        match self.0.kind {
            ExportKind::Absolute => {
                attrs.push("absolute".to_owned());
            }
            ExportKind::ThreadLocal => {
                attrs.push("per-thread".to_owned());
            }
            ExportKind::Regular => {}
        }

        match self.0.symbol {
            ExportType::Reexport { ordinal, ref name } => {
                let dylib_name = dylib_name(self.1, ordinal).ok_or(fmt::Error)?;

                if name.is_empty() {
                    attrs.push(format!("from {}", dylib_name));
                } else {
                    attrs.push(format!("{} from {}", name, dylib_name));
                }
            }
            ExportType::Weak { .. } => {
                attrs.push("weak".to_owned());
            }
            ExportType::Stub { resolver, .. } => {
                attrs.push(format!("resolver = 0x{:08X}", resolver));
            }
            ExportType::Regular { .. } => {}
        }

        if !attrs.is_empty() {
            write!(f, " [")?;

            while let Some(attr) = attrs.pop() {
                write!(f, "{}", attr)?;

                if !attrs.is_empty() {
                    write!(f, ", ")?;
                }
            }

            write!(f, "]")?;
        }

        Ok(())
    }
}

fn segmnet_info(commands: &[LoadCommand], segment_index: usize) -> Option<(&str, Range<usize>, &[Rc<Section>])> {
    commands.get(segment_index).and_then(|cmd| match cmd {
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
        } => Some((segname.as_str(), (vmaddr..vmaddr + vmsize), sections.as_slice())),
        _ => None,
    })
}

fn section_name(sections: &[Rc<Section>], addr: usize) -> Option<&str> {
    sections
        .iter()
        .map(|section| section.as_ref())
        .find(|section| section.addr <= addr && section.addr + section.size > addr)
        .map(|section| section.sectname.as_str())
}

fn dylib_name(commands: &[LoadCommand], ordinal: usize) -> Option<&str> {
    commands
        .iter()
        .flat_map(|cmd| match cmd {
            &LoadCommand::IdDyLib(ref dylib)
            | &LoadCommand::LoadDyLib(ref dylib)
            | &LoadCommand::LoadWeakDyLib(ref dylib)
            | &LoadCommand::ReexportDyLib(ref dylib)
            | &LoadCommand::LoadUpwardDylib(ref dylib)
            | &LoadCommand::LazyLoadDylib(ref dylib) => Some(dylib),
            _ => None,
        })
        .nth(ordinal - 1)
        .and_then(|dylib| Path::new(dylib.name.as_str()).file_name())
        .and_then(|filename| filename.to_str())
}
