#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate memmap;
extern crate macho;

use std::env;
use std::io::{Write, Cursor, stdout, stderr};
use std::path::Path;
use std::process::exit;

use getopts::Options;
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

    opts.optflag("f", "", "print the fat headers");
    opts.optflag("h", "", "print the mach header");
    opts.optflag("l", "", "print the load commands");
    opts.optopt("", "arch", "Specifies the architecture", "arch_type");
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
        print_fat_header: matches.opt_present("f"),
        print_mach_header: matches.opt_present("h"),
        print_load_commands: matches.opt_present("l"),
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
    print_fat_header: bool,
    print_mach_header: bool,
    print_load_commands: bool,
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

            if self.cpu_type != 0 {
                try!(write!(self.w,
                            "{} (architecture {}):\n",
                            filename,
                            get_arch_name_from_types(file.header.cputype, file.header.cpusubtype)
                                .unwrap_or(format!("cputype {} cpusubtype {}",
                                                   file.header.cputype,
                                                   file.header.cpusubtype)
                                               .as_str())));
            } else {
                try!(write!(self.w, "{}:\n", filename));
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
        }

        Ok(())
    }
}
