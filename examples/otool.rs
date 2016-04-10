#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate memmap;
extern crate macho;

use std::env;
use std::io::{Write, Cursor, stderr};
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
        cpu_type: CPU_TYPE_ANY,
        print_fat_headers: matches.opt_present("f"),
    };

    if let Some(flags) = matches.opt_str("arch") {
        if let Ok((cpu_type, _)) = get_arch_from_flag(flags.as_str()) {
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

struct FileProcessor {
    cpu_type: cpu_type_t,
    print_fat_headers: bool,
}

impl FileProcessor {
    fn process(&self, filename: &str) -> Result<(), Error> {
        let file_mmap = try!(Mmap::open_path(filename, Protection::Read));
        let mut cur = Cursor::new(unsafe { file_mmap.as_slice() });
        let file = try!(UniversalFile::load(&mut cur));

        if self.print_fat_headers {
            if let Some(fat_header) = file.header {
                println!("{}", fat_header);
            }
        }

        Ok(())
    }
}
