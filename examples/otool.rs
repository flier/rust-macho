#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate getopts;
extern crate byteorder;
extern crate memmap;
extern crate regex;
extern crate gimli;
extern crate mach_object;

use std::env;
use std::mem::size_of;
use std::io::{Write, Cursor, Seek, SeekFrom, stdout, stderr};
use std::path::Path;
use std::process::exit;

use getopts::Options;
use byteorder::ReadBytesExt;
use memmap::{Mmap, Protection};
use gimli::CompilationUnitHeader;

use mach_object::*;

error_chain! {
    foreign_links {
        IoError(::std::io::Error);
        DwarfError(::gimli::Error);
        MachError(::mach_object::Error);
    }
}

const APP_VERSION: &'static str = "0.1.1";

static DW_LNS_opcode_names: &'static [&'static str] = &["DW_LNS_copy",
                                                        "DW_LNS_advance_pc",
                                                        "DW_LNS_advance_line",
                                                        "DW_LNS_set_file",
                                                        "DW_LNS_set_column",
                                                        "DW_LNS_negate_stmt",
                                                        "DW_LNS_set_basic_block",
                                                        "DW_LNS_const_add_pc",
                                                        "DW_LNS_fixed_advance_pc",
                                                        "DW_LNS_set_prologue_end",
                                                        "DW_LNS_set_epilogue_begin",
                                                        "DW_LNS_set_isa"];

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [-arch arch_type] [options] [--version] <object file> ...",
                        program);

    print!("{}", opts.usage(&brief));
}

fn main() {
    env_logger::init().unwrap();

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
    opts.optflag("i",
                 "ignore-case",
                 "ignore case distinctions in when finding by name using strings or regular expressions.");
    opts.optflag("x",
                 "regex",
                 "treat any <pattern> strings as regular expressions when searching instead of just as an exact string match.");
    opts.optflag("", "debug-all", "print all the DWARF sections");
    opts.optflag("", "debug-abbrev", "print the DWARF abbreviations");
    opts.optflag("",
                 "debug-aranges",
                 "print the DWARF lookup table for mapping addresses to compilation units");
    opts.optopt("",
                "debug-frame",
                "print the DWARF call frame information",
                "<offset>");
    opts.optopt("",
                "debug-info",
                "print the core DWARF information section",
                "<offset>");
    opts.optopt("",
                "debug-line",
                "print the DWARF line number information",
                "<offset>");
    opts.optopt("",
                "debug-pubnames",
                "print the DWARF lookup table for global objects and functions",
                "<pattern>");
    opts.optopt("",
                "debug-pubtypes",
                "print the DWARF lookup table for global types",
                "<pattern>");
    opts.optflag("", "debug-str", "print the DWARF string table");
    opts.optopt("",
                "debug-types",
                "print the DWARF type descriptions",
                "<offset>");
    opts.optflag("",
                 "version",
                 format!("print the version of {}", program).as_str());

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
        write!(stderr(), "at least one file must be specified\n\n").unwrap();

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
        print_section: matches
            .opt_str("s")
            .map(|s| {
                let names: Vec<&str> = s.splitn(2, ':').collect();

                if names.len() == 2 {
                    (String::from(names[0]), Some(String::from(names[1])))
                } else {
                    (String::from(names[0]), None)
                }
            }),
        print_lib_toc: matches.opt_present("S"),
        print_dwarf: DwarfProcessor {
            w: stdout(),
            ignore_case: matches.opt_present("i"),
            use_regex: matches.opt_present("x"),
            print_debug_all: matches.opt_present("debug-all"),
            print_debug_abbrev: matches.opt_present("debug-abbrev"),
            print_debug_aranges: matches.opt_present("debug-aranges"),
            print_debug_frame: matches
                .opt_strs("debug-frame")
                .iter()
                .map(|s| gimli::DebugFrameOffset(s.parse().unwrap()))
                .collect(),
            print_debug_info: matches
                .opt_strs("debug-info")
                .iter()
                .map(|s| gimli::DebugInfoOffset(s.parse().unwrap()))
                .collect(),
            print_debug_line: matches
                .opt_strs("debug-line")
                .iter()
                .map(|s| gimli::DebugLineOffset(s.parse().unwrap()))
                .collect(),
            print_debug_pubnames: matches.opt_strs("debug-pubnames"),
            print_debug_pubtypes: matches.opt_strs("debug-pubtypes"),
            print_debug_str: matches.opt_present("debug-str"),
            print_debug_types: matches
                .opt_strs("debug-types")
                .iter()
                .map(|s| s.parse().unwrap())
                .collect(),
        },
    };

    if let Some(flags) = matches.opt_str("arch") {
        if let Some(&(cpu_type, _)) = get_arch_from_flag(flags.as_str()) {
            processor.cpu_type = cpu_type;
        } else {
            write!(stderr(),
                   "unknown architecture specification flag: arch {}\n",
                   flags)
                    .unwrap();

            exit(-1);
        }
    }

    for filename in matches.free {
        if let Err(err) = processor.process(filename.as_str()) {
            write!(stderr(), "fail to process file {}, {}", filename, err).unwrap();

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
    print_dwarf: DwarfProcessor<T>,
}

struct FileProcessContext<'a> {
    filename: String,
    cur: &'a mut Cursor<&'a [u8]>,
}

impl<'a> FileProcessContext<'a> {
    fn hexdump(&mut self, addr: usize, size: usize) -> Result<Vec<u8>> {
        let mut w = Vec::new();

        for off in 0..size {
            if (off % 16) == 0 {
                if off > 0 {
                    try!(write!(&mut w, "\n"));
                }

                try!(write!(&mut w, "{:016x}\t", addr + off));
            }

            try!(write!(&mut w, "{:02x} ", try!(self.cur.read_u8())));
        }

        try!(write!(&mut w, "\n"));

        Ok(w)
    }
}

impl<T: Write> FileProcessor<T> {
    fn process(&mut self, filename: &str) -> Result<()> {
        let file_mmap = try!(Mmap::open_path(filename, Protection::Read));
        let mut cur = Cursor::new(unsafe { file_mmap.as_slice() });
        let file = try!(OFile::parse(&mut cur));
        let mut ctxt = FileProcessContext {
            filename: String::from(filename),
            cur: &mut cur,
        };

        debug!("process file {} with {} bytes", filename, file_mmap.len());

        try!(self.process_ofile(&file, &mut ctxt));

        if self.print_symbol_table {
            debug!("dumping symbol table");

            if let Some(symbols) = file.symbols(ctxt.cur) {
                for symbol in symbols {
                    try!(write!(self.w, "{}\n", symbol));
                }
            }
        }

        Ok(())
    }

    fn process_ofile(&mut self, ofile: &OFile, ctxt: &mut FileProcessContext) -> Result<()> {
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
        self.print_mach_header | self.print_load_commands | self.print_text_section | self.print_data_section |
        self.print_shared_lib
    }

    fn process_mach_file(&mut self,
                         header: &MachHeader,
                         commands: &Vec<MachCommand>,
                         ctxt: &mut FileProcessContext)
                         -> Result<()> {
        if self.cpu_type != 0 && self.cpu_type != CPU_TYPE_ANY && self.cpu_type != header.cputype {
            return Ok(());
        }

        if self.print_headers && self.print_mach_file() {
            if self.cpu_type != 0 {
                try!(write!(self.w,
                            "{} (architecture {}):\n",
                            ctxt.filename,
                            get_arch_name_from_types(header.cputype, header.cpusubtype)
                                .unwrap_or(format!("cputype {} cpusubtype {}",
                                                   header.cputype,
                                                   header.cpusubtype)
                                                   .as_str())));
            } else {
                try!(write!(self.w, "{}:\n", ctxt.filename));
            }
        }

        if self.print_mach_header {
            try!(write!(self.w, "{}", header));
        }

        if self.print_load_commands {
            for (i, ref cmd) in commands.iter().enumerate() {
                try!(write!(self.w, "Load command {}\n", i));
                try!(write!(self.w, "{}", cmd));
            }
        }

        for cmd in commands {
            let &MachCommand(ref cmd, _) = cmd;

            match cmd {
                &LoadCommand::Segment {
                     ref segname,
                     ref sections,
                     ..
                 } |
                &LoadCommand::Segment64 {
                     ref segname,
                     ref sections,
                     ..
                 } => {
                    for ref sect in sections {
                        let name = Some((sect.segname.clone(), Some(sect.sectname.clone())));

                        if name == self.print_section || Some((sect.segname.clone(), None)) == self.print_section ||
                           (self.print_text_section &&
                            name == Some((String::from(SEG_TEXT), Some(String::from(SECT_TEXT))))) ||
                           (self.print_data_section &&
                            name == Some((String::from(SEG_DATA), Some(String::from(SECT_DATA))))) {

                            if self.print_headers {
                                try!(write!(self.w,
                                            "Contents of ({},{}) section\n",
                                            sect.segname,
                                            sect.sectname));
                            }

                            try!(ctxt.cur.seek(SeekFrom::Start(sect.offset as u64)));

                            let dump = try!(ctxt.hexdump(sect.addr, sect.size));

                            try!(self.w.write(&dump[..]));
                        }
                    }

                    if segname == "__DWARF" {
                        if header.is_bigend() {
                            self.print_dwarf
                                .process(OFile::load_dwarf::<&[u8], gimli::BigEndian>(ctxt.cur, sections))?;
                        } else {
                            self.print_dwarf
                                .process(OFile::load_dwarf::<&[u8], gimli::LittleEndian>(ctxt.cur, sections))?;
                        }
                    }
                }

                &LoadCommand::IdFvmLib(ref fvmlib) |
                &LoadCommand::LoadFvmLib(ref fvmlib) if self.print_shared_lib && !self.print_shared_lib_just_id => {
                    try!(write!(self.w,
                                "\t{} (minor version {})\n",
                                fvmlib.name,
                                fvmlib.minor_version));
                }

                &LoadCommand::IdDyLib(ref dylib) |
                &LoadCommand::LoadDyLib(ref dylib) |
                &LoadCommand::LoadWeakDyLib(ref dylib) |
                &LoadCommand::ReexportDyLib(ref dylib) |
                &LoadCommand::LoadUpwardDylib(ref dylib) |
                &LoadCommand::LazyLoadDylib(ref dylib) if self.print_shared_lib &&
                                                          (cmd.cmd() == LC_ID_DYLIB ||
                                                           !self.print_shared_lib_just_id) => {
                    if self.print_shared_lib_just_id {
                        try!(write!(self.w, "{}", dylib.name));
                    } else {
                        try!(write!(self.w,
                                    "\t{} (compatibility version {}.{}.{}, current version \
                                     {}.{}.{})\n",
                                    dylib.name,
                                    dylib.compatibility_version.major(),
                                    dylib.compatibility_version.minor(),
                                    dylib.compatibility_version.release(),
                                    dylib.current_version.major(),
                                    dylib.current_version.minor(),
                                    dylib.current_version.release()));
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn process_fat_file(&mut self,
                        magic: u32,
                        files: &Vec<(FatArch, OFile)>,
                        ctxt: &mut FileProcessContext)
                        -> Result<()> {
        if self.print_fat_header {
            let header = FatHeader {
                magic: magic,
                archs: files
                    .iter()
                    .map(|&(ref arch, _)| arch.clone())
                    .collect(),
            };

            try!(write!(self.w, "{}", header));
        }

        for &(_, ref file) in files {
            try!(self.process_ofile(file, ctxt));
        }

        Ok(())
    }

    fn process_ar_file(&mut self, files: &Vec<(ArHeader, OFile)>, ctxt: &mut FileProcessContext) -> Result<()> {
        if self.print_headers && (self.print_lib_toc || self.print_mach_file()) {
            try!(write!(self.w, "Archive :{}\n", ctxt.filename));
        }

        if self.print_archive_header {
            for &(ref header, _) in files {
                try!(write!(self.w, "{}", header));
            }
        }

        for &(ref header, ref file) in files {
            try!(self.process_ofile(file,
                                    &mut FileProcessContext {
                                             filename: if let Some(ref name) = header.ar_member_name {
                                                 format!("{}({})", ctxt.filename, name)
                                             } else {
                                                 ctxt.filename.clone()
                                             },
                                             cur: &mut ctxt.cur.clone(),
                                         }));
        }

        Ok(())
    }

    fn process_symdef(&mut self, ranlibs: &Vec<RanLib>, ctxt: &mut FileProcessContext) -> Result<()> {
        if self.print_lib_toc {
            try!(write!(self.w, "Table of contents from: {}\n", ctxt.filename));
            try!(write!(self.w,
                        "size of ranlib structures: {} (number {})\n",
                        ranlibs.len() * size_of::<RanLib>(),
                        ranlibs.len()));
            try!(write!(self.w, "object offset  string index\n"));

            for ref ranlib in ranlibs {
                try!(write!(self.w, "{:<14} {}\n", ranlib.ran_off, ranlib.ran_strx));
            }
        }

        Ok(())
    }
}

struct DwarfProcessor<T: Write> {
    w: T,
    ignore_case: bool,
    use_regex: bool,
    print_debug_all: bool,
    print_debug_abbrev: bool,
    print_debug_aranges: bool,
    print_debug_frame: Vec<gimli::DebugFrameOffset>,
    print_debug_info: Vec<gimli::DebugInfoOffset>,
    print_debug_line: Vec<gimli::DebugLineOffset>,
    print_debug_pubnames: Vec<String>,
    print_debug_pubtypes: Vec<String>,
    print_debug_str: bool,
    print_debug_types: Vec<String>,
}

impl<T: Write> DwarfProcessor<T> {
    fn process<Endian: gimli::Endianity>(&mut self, dwarf: Dwarf<Endian>) -> Result<()> {
        if self.print_debug_all || self.print_debug_abbrev {
            self.process_debug_abbrev()
        }
        if self.print_debug_all || self.print_debug_aranges {
            self.process_debug_aranges()
        }

        for offset in self.print_debug_frame.iter() {}

        for offset in self.print_debug_info.iter() {}

        if self.print_debug_all || !self.print_debug_line.is_empty() {
            write!(self.w, ".debug_line contents:\n")?;

            if let Dwarf {
                       debug_abbrev: Some(ref debug_abbrev),
                       debug_line: Some(ref debug_line),
                       debug_info: Some(ref debug_info),
                       debug_str: Some(ref debug_str),
                       ..
                   } = dwarf {
                self.process_debug_line(debug_abbrev, debug_line, debug_info, debug_str);
            } else {
                write!(self.w, "< EMPTY >\n\n")?;
            }
        }

        for offset in self.print_debug_line.iter() {}

        for offset in self.print_debug_pubnames.iter() {}

        for offset in self.print_debug_pubtypes.iter() {}

        if self.print_debug_all || self.print_debug_str {
            self.process_debug_str()
        }

        for offset in self.print_debug_types.iter() {}

        Ok(())
    }

    fn process_debug_abbrev(&self) {}

    fn process_debug_aranges(&self) {}

    fn process_debug_line<Endian: gimli::Endianity>(&mut self,
                                                    debug_abbrev: &gimli::DebugAbbrev<Endian>,
                                                    debug_line: &gimli::DebugLine<Endian>,
                                                    debug_info: &gimli::DebugInfo<Endian>,
                                                    debug_str: &gimli::DebugStr<Endian>)
                                                    -> Result<()> {
        let mut iter = debug_info.units();

        while let Some(unit) = iter.next()? {
            let abbrevs = unit.abbreviations(debug_abbrev.clone())?;
            let mut cursor = unit.entries(&abbrevs);
            cursor.next_dfs()?;
            let root = cursor.current().unwrap();
            let offset = match root.attr_value(gimli::DW_AT_stmt_list)? {
                Some(gimli::AttributeValue::DebugLineRef(offset)) => offset,
                _ => continue,
            };

            if self.print_debug_all || self.print_debug_line.iter().any(|off| *off == offset) {
                write!(self.w,
                       "----------------------------------------------------------------------\n")?;
                write!(self.w, "debug_line[0x{:08x}]\n", offset.0)?;
                write!(self.w,
                       "----------------------------------------------------------------------\n")?;

                let comp_dir = root.attr(gimli::DW_AT_comp_dir)?
                    .and_then(|attr| attr.string_value(&debug_str));
                let comp_name = root.attr(gimli::DW_AT_name)?
                    .and_then(|attr| attr.string_value(&debug_str));

                if let Ok(program) = debug_line.program(offset, unit.address_size(), comp_dir, comp_name) {
                    {
                        let header = program.header();

                        write!(self.w, "Line table prologue:\n")?;
                        write!(self.w, "   total_length: 0x{:08x}\n", header.unit_length())?;
                        write!(self.w, "        version: 0x{:04x}\n", header.version())?;
                        write!(self.w,
                               "prologue_length: 0x{:08x}\n",
                               header.header_length())?;
                        write!(self.w,
                               "min_inst_length: 0x{:02x}\n",
                               header.minimum_instruction_length())?;
                        write!(self.w, "default_is_stmt: {}\n", header.default_is_stmt())?;
                        write!(self.w, "      line_base: {}\n", header.line_base())?;
                        write!(self.w, "     line_range: {}\n", header.line_range())?;
                        write!(self.w, "    opcode_base: 0x{:02x}\n", header.opcode_base())?;

                        for (i, length) in header.standard_opcode_lengths().iter().enumerate() {
                            write!(self.w,
                                   "standard_opcode_lengths[ {:26} ] = {}\n",
                                   DW_LNS_opcode_names[i],
                                   length)?;
                        }

                        for (i, dir) in header.include_directories().iter().enumerate() {
                            write!(self.w,
                                   "include_directories[{:>3}] = '{}'\n",
                                   i + 1,
                                   dir.to_string_lossy())?;
                        }

                        write!(self.w,
                               "                Dir  Mod Time   File Len   File Name\n")?;
                        write!(self.w,
                               "                ---- ---------- ---------- ---------------------------\n")?;
                        for (i, file) in header.file_names().iter().enumerate() {
                            write!(self.w,
                                   "file_names[{:>3}] {:>4} 0x{:08x} 0x{:08x} {}\n",
                                   i + 1,
                                   file.directory_index(),
                                   file.last_modification(),
                                   file.length(),
                                   file.path_name().to_string_lossy())?;
                        }

                        let mut opcodes = header.opcodes();
                        let mut op_index = 0;
                        let mut rows = program.clone().rows();

                        let mut dump_row = |w: &mut T| -> Result<()> {
                            if let Some((header, row)) = rows.next_row()? {
                                let line = row.line().unwrap_or(0);
                                let column = match row.column() {
                                    gimli::ColumnType::Column(column) => column,
                                    gimli::ColumnType::LeftEdge => 0,
                                };

                                write!(w,
                                       "0x{:016x}\t{}\t{}\t{}",
                                       row.address(),
                                       row.file_index(),
                                       line,
                                       column)?;

                                if row.is_stmt() {
                                    write!(w, " is_stmt")?;
                                }

                                if row.basic_block() {
                                    write!(w, " basic_block")?;
                                }

                                if row.end_sequence() {
                                    write!(w, " end_sequence")?;
                                }

                                if row.prologue_end() {
                                    write!(w, " prologue_end")?;
                                }

                                if row.epilogue_begin() {
                                    write!(w, " epilogue_begin")?;
                                }

                                write!(w, "\n")?;
                            };

                            Ok(())
                        };

                        while let Some(opcode) = opcodes.next_opcode(&header)? {
                            match opcode {
                                gimli::Opcode::Special(opcode) => {
                                    let adjusted_opcode = opcode - header.opcode_base();
                                    let op_advance = adjusted_opcode / header.line_range();
                                    let addr_advance = header.minimum_instruction_length() *
                                                       ((op_index + op_advance) /
                                                        header.maximum_operations_per_instruction());
                                    op_index = (op_index + op_advance) % header.maximum_operations_per_instruction();
                                    let line_advance = header.line_base() +
                                                       (adjusted_opcode % header.line_range()) as i8;

                                    write!(self.w,
                                           "address += 0x{:x},  line += {}\n",
                                           addr_advance,
                                           line_advance)?;
                                }
                                gimli::Opcode::AdvancePc(n) => {
                                    write!(self.w, "DW_LNS_advance_pc( {} ) \n", n)?;

                                    dump_row(&mut self.w)?;
                                }
                                gimli::Opcode::AdvanceLine(n) => {
                                    write!(self.w, "DW_LNS_advance_line( {} ) \n", n)?;
                                }
                                gimli::Opcode::SetFile(i) => {
                                    write!(self.w,
                                           "DW_LNS_set_file( {} ) // {}\n",
                                           i,
                                           header.file_names()[i as usize - 1]
                                               .path_name()
                                               .to_string_lossy())?;
                                }
                                gimli::Opcode::SetColumn(n) => {
                                    write!(self.w, "DW_LNS_set_column( {} ) \n", n)?;
                                }
                                gimli::Opcode::FixedAddPc(n) => {
                                    write!(self.w, "DW_LNS_fixed_advance_pc( {} ) \n", n)?;
                                }
                                gimli::Opcode::SetIsa(isa) => {
                                    write!(self.w, "DW_LNS_set_isa( {} ) \n", isa)?;
                                }
                                gimli::Opcode::SetAddress(addr) => {
                                    write!(self.w, "DW_LNE_set_address( 0x{:016x} ) \n", addr)?;
                                }
                                gimli::Opcode::DefineFile(ref file) => {
                                    write!(self.w,
                                           "DW_LNE_define_file( {} )\n",
                                           file.path_name().to_string_lossy())?;
                                }
                                gimli::Opcode::SetDiscriminator(discriminator) => {
                                    write!(self.w, "DW_LNE_set_discriminator( {} ) \n", discriminator)?;
                                }

                                gimli::Opcode::Copy |
                                gimli::Opcode::ConstAddPc => {
                                    write!(self.w, "{}\n", opcode)?;

                                    dump_row(&mut self.w)?;
                                }
                                gimli::Opcode::SetPrologueEnd |
                                gimli::Opcode::NegateStatement |
                                gimli::Opcode::SetBasicBlock |
                                gimli::Opcode::SetEpilogueBegin |
                                gimli::Opcode::EndSequence |
                                gimli::Opcode::UnknownStandard0(_) |
                                gimli::Opcode::UnknownStandard1(..) |
                                gimli::Opcode::UnknownStandardN(..) |
                                gimli::Opcode::UnknownExtended(..) => write!(self.w, "  {}\n", opcode)?,
                            }
                        }

                        write!(self.w, "\nLine Number Rows:\n")?;
                        write!(self.w, "<pc>        [lno,col]\n")?;
                    }

                    {
                        let mut rows = program.rows();
                        let mut file_index = 0;
                        while let Some((header, row)) = rows.next_row()? {
                            let line = row.line().unwrap_or(0);
                            let column = match row.column() {
                                gimli::ColumnType::Column(column) => column,
                                gimli::ColumnType::LeftEdge => 0,
                            };
                            write!(self.w, "0x{:08x}  [{:4},{:2}]", row.address(), line, column)?;
                            if row.is_stmt() {
                                write!(self.w, " NS")?;
                            }
                            if row.basic_block() {
                                write!(self.w, " BB")?;
                            }
                            if row.end_sequence() {
                                write!(self.w, " ET")?;
                            }
                            if row.prologue_end() {
                                write!(self.w, " PE")?;
                            }
                            if row.epilogue_begin() {
                                write!(self.w, " EB")?;
                            }
                            if row.isa() != 0 {
                                write!(self.w, " IS={}", row.isa())?;
                            }
                            if row.discriminator() != 0 {
                                write!(self.w, " DI={}", row.discriminator())?;
                            }
                            if file_index != row.file_index() {
                                file_index = row.file_index();
                                if let Some(file) = row.file(header) {
                                    if let Some(directory) = file.directory(header) {
                                        write!(self.w,
                                               " uri: \"{}/{}\"",
                                               directory.to_string_lossy(),
                                               file.path_name().to_string_lossy())?;
                                    } else {
                                        write!(self.w, " uri: \"{}\"", file.path_name().to_string_lossy())?;
                                    }
                                }
                            }
                            write!(self.w, "\n")?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn process_debug_str(&self) {}
}