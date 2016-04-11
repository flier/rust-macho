#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate byteorder;
extern crate memmap;
extern crate macho;

use std::env;
use std::io::{BufRead, Write, Cursor, Seek, SeekFrom, stdout, stderr};
use std::path::Path;
use std::process::exit;

use getopts::Options;
use byteorder::ReadBytesExt;
use memmap::{Mmap, Protection};

use macho::*;

const APP_VERSION: &'static str = "0.1";

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [-arch arch_type] [options] [--version] <object file> ...",
                        program);

    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = Path::new(args[0].as_str()).file_name().unwrap().to_str().unwrap();

    let mut opts = Options::new();

    opts.optopt("", "arch", "Specifies the architecture", "arch_type");
    opts.optflag("f", "", "print the fat headers");
    opts.optflag("h", "", "print the mach header");
    opts.optflag("l", "", "print the load commands");
    opts.optflag("L", "", "print shared libraries used");
    opts.optflag("D", "", "print shared library id name");
    opts.optflag("t", "", "print the text section");
    opts.optflag("d", "", "print the data section");
    opts.optopt("s", "", "print contents of section", "<segname>:<sectname>");
    opts.optflag("X", "", "print no leading addresses or headers");
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
        print_mach_header: matches.opt_present("h"),
        print_load_commands: matches.opt_present("l"),
        print_text_section: matches.opt_present("t"),
        print_data_section: matches.opt_present("d"),
        print_section: matches.opt_str("s").map(|s| {
            let names: Vec<&str> = s.splitn(2, ':').collect();

            if names.len() == 2 {
                (String::from(names[0]), Some(String::from(names[1])))
            } else {
                (String::from(names[0]), None)
            }
        }),
        print_shared_lib: matches.opt_present("L") || matches.opt_present("D"),
        print_shared_lib_just_id: matches.opt_present("D") && !matches.opt_present("L"),
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
    print_mach_header: bool,
    print_load_commands: bool,
    print_text_section: bool,
    print_data_section: bool,
    print_section: Option<(String, Option<String>)>,
    print_shared_lib: bool,
    print_shared_lib_just_id: bool,
}

impl<T: Write> FileProcessor<T> {
    fn process(&mut self, filename: &str) -> Result<(), Error> {
        let file_mmap = try!(Mmap::open_path(filename, Protection::Read));
        let mut cur = Cursor::new(unsafe { file_mmap.as_slice() });
        let ufile = try!(UniversalFile::load(&mut cur));

        if self.print_fat_header {
            if let Some(fat_header) = ufile.header {
                try!(write!(self.w, "{}", fat_header));
            }
        }

        for file in ufile.files {
            if self.cpu_type != 0 && self.cpu_type != CPU_TYPE_ANY &&
               self.cpu_type != file.header.cputype {
                continue;
            }

            if self.print_headers {
                if self.cpu_type != 0 {
                    try!(write!(self.w,
                                "{} (architecture {}):\n",
                                filename,
                                get_arch_name_from_types(file.header.cputype,
                                                         file.header.cpusubtype)
                                    .unwrap_or(format!("cputype {} cpusubtype {}",
                                                       file.header.cputype,
                                                       file.header.cpusubtype)
                                                   .as_str())));
                } else {
                    try!(write!(self.w, "{}:\n", filename));
                }
            }

            if self.print_mach_header {
                try!(write!(self.w, "{}", file.header));
            }

            if self.print_load_commands {
                for (i, ref cmd) in file.commands.iter().enumerate() {
                    try!(write!(self.w, "Load command {}\n", i));
                    try!(write!(self.w, "{}", cmd));
                }
            }

            for ref cmd in file.commands {
                let &MachCommand(ref cmd, _) = cmd;

                match cmd {
                    &LoadCommand::Segment{ref sections, ..} |
                    &LoadCommand::Segment64{ref sections, ..} => {
                        for ref sect in sections {
                            let name = Some((sect.segname.clone(), Some(sect.sectname.clone())));

                            if name == self.print_section ||
                               Some((sect.segname.clone(), None)) == self.print_section ||
                               (self.print_text_section &&
                                name ==
                                Some((String::from(SEG_TEXT), Some(String::from(SECT_TEXT))))) ||
                               (self.print_data_section &&
                                name ==
                                Some((String::from(SEG_DATA), Some(String::from(SECT_DATA))))) {

                                if self.print_headers {
                                    try!(write!(self.w,
                                                "Contents of ({},{}) section\n",
                                                sect.segname,
                                                sect.sectname));
                                }

                                try!(cur.seek(SeekFrom::Start(sect.offset as u64)));

                                let dump = try!(hexdump(sect.addr, &mut cur, sect.size));

                                try!(self.w.write(&dump[..]));
                            }
                        }
                    }

                    &LoadCommand::IdFvmLib(ref fvmlib) |
                    &LoadCommand::LoadFvmLib(ref fvmlib) if self.print_shared_lib &&
                                                            !self.print_shared_lib_just_id => {
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
        }

        Ok(())
    }
}

fn hexdump<T: BufRead>(addr: usize, buf: &mut T, size: usize) -> Result<Vec<u8>, Error> {
    let mut w = Vec::new();

    for off in 0..size {
        if (off % 16) == 0 {
            if off > 0 {
                try!(write!(&mut w, "\n"));
            }

            try!(write!(&mut w, "{:016x}\t", addr + off));
        }

        try!(write!(&mut w, "{:02x} ", try!(buf.read_u8())));
    }

    try!(write!(&mut w, "\n"));

    Ok(w)
}
