#![cfg(all(target_os = "macos", feature = "integration_tests"))]

#[macro_use]
extern crate log;

use std::collections::HashSet;
use std::fs::File;
use std::io::{self, Cursor};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

use failure::Error;
use hexplay::HexViewBuilder;
use memmap::Mmap;
use pretty_env_logger;
use time::PreciseTime;
use walkdir::{DirEntry, WalkDir};

use mach_object::{LoadCommand, MachCommand, MachError, OFile};

const SYSTEM_PATH: &[&str] = &[
    // binary
    "/bin",
    "/sbin",
    "/usr/bin",
    "/usr/sbin",
    "/usr/libexec",
    "/usr/local/bin",
    "/usr/local/sbin",
    "/usr/local/libexec",
    // library
    "/usr/lib",
    "/usr/local/lib",
    // framework
    "/System/Library/Frameworks",
    "/Library/Frameworks",
    "/usr/local/Frameworks",
    // application
    "/Applications",
    "~/Applications",
];

fn load_mach_file(entry: &DirEntry) -> Result<(), Error> {
    if entry.metadata()?.len() == 0 {
        trace!("skip the empty file, {:?}", entry.path());

        return Ok(());
    }

    match File::open(entry.path()) {
        Ok(file) => {
            let start_time = PreciseTime::now();

            let mmap = unsafe { Mmap::map(&file) }?;
            let payload = mmap.as_ref();

            if payload.len() < 8 {
                trace!("skip too small file, {:?}", entry.path());
            } else if payload.starts_with(b"#") {
                trace!("skip the scripts, {:?}", entry.path());
            } else if payload.starts_with(b"<") {
                trace!("skip the XML/HTML file, {:?}", entry.path());
            } else if payload.starts_with(b"MZ") {
                trace!("skip the PE file, {:?}", entry.path());
            } else if payload.starts_with(b"PK") {
                trace!("skip the compressed file, {:?}", entry.path());
            } else {
                let mut cur = Cursor::new(payload);
                let ofile = match OFile::parse(&mut cur) {
                    Ok(ofile) => ofile,
                    Err(err) => {
                        if let Some(&MachError::UnknownMagic(magic)) = err.as_fail().downcast_ref::<MachError>() {
                            trace!("skip unknown file format: 0x{:08x}, {:?}", magic, entry.path());

                            return Ok(());
                        } else {
                            return Err(err);
                        }
                    }
                };

                verify_mach_file(entry.path(), &ofile);

                info!(
                    "loaded in {} ms, {:?}",
                    start_time.to(PreciseTime::now()).num_microseconds().unwrap_or_default() as f64 / 1000.0,
                    entry.path(),
                );
            }

            Ok(())
        }
        Err(ref err @ io::Error { .. }) if err.kind() == io::ErrorKind::PermissionDenied => {
            trace!("ignore the permission denied, {:?}", entry.path());

            Ok(())
        }
        Err(err) => Err(err.into()),
    }
}

fn verify_mach_file(path: &Path, ofile: &OFile) {
    match ofile {
        OFile::MachFile { ref commands, .. } => {
            for &MachCommand(ref cmd, cmdsize) in commands {
                if let LoadCommand::Command { cmd, ref payload } = cmd {
                    warn!(
                        "unsolved command #{} with {} bytes in {:?}:\n{}",
                        cmd,
                        cmdsize,
                        path,
                        HexViewBuilder::new(payload).finish()
                    );
                }
            }
        }
        OFile::FatFile { ref files, .. } => {
            for (arch, ofile) in files {
                verify_mach_file(&path.join(arch.name().unwrap_or_default()), ofile)
            }
        }
        OFile::ArFile { ref files } => {
            for (header, ofile) in files {
                verify_mach_file(&path.join(&header.ar_name), ofile)
            }
        }
        OFile::SymDef { .. } => {}
    }
}

#[test]
fn test_system_files() {
    let _ = pretty_env_logger::try_init();

    let mut solved = HashSet::new();

    for path in SYSTEM_PATH.iter() {
        trace!("walking directory: {:?}", path);

        for entry in WalkDir::new(path)
            .follow_links(true)
            .into_iter()
            .filter_map(|entry| entry.ok())
            .filter(|entry| !entry.file_type().is_dir())
        {
            if solved.contains(entry.path()) {
                continue;
            }

            let need_parse = if (entry.metadata().unwrap().permissions().mode() & 0o111) != 0 {
                true
            } else {
                match entry.path().extension().and_then(|ext| ext.to_str()) {
                    Some("dylib") | Some("so") | Some("a") | Some("o") => true,
                    _ => false,
                }
            };

            if need_parse {
                load_mach_file(&entry).expect(&format!("load binary file: {:?}", entry));
            }

            solved.insert(entry.path().to_owned());
        }
    }
}
