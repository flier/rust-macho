#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
extern crate hexplay;
extern crate memmap;
#[cfg(test)]
extern crate pretty_env_logger;
extern crate walkdir;

extern crate mach_object;

#[cfg(all(target_os = "macos", feature = "integration_tests"))]
mod integration {
    use std::collections::HashSet;
    use std::fs::File;
    use std::io::{self, Cursor};
    use std::os::unix::fs::PermissionsExt;

    use failure::Error;
    use hexplay::HexViewBuilder;
    use memmap::Mmap;
    use pretty_env_logger;
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
                let mmap = unsafe { Mmap::map(&file) }?;
                let payload = mmap.as_ref();

                if payload.starts_with(b"#") {
                    trace!("skip the scripts, {:?}", entry.path());
                } else {
                    let mut cur = Cursor::new(payload);
                    let ofile = match OFile::parse(&mut cur) {
                        Ok(ofile) => ofile,
                        Err(err) => {
                            if let Some(&MachError::UnknownMagic(magic)) = err.cause().downcast_ref::<MachError>() {
                                trace!("skip unknown file format: 0x{:08x}, {:?}", magic, entry.path());

                                return Ok(());
                            } else {
                                bail!(err);
                            }
                        }
                    };

                    verify_mach_file(&ofile);

                    trace!("loaded ofile, {:?}", entry.path());
                }

                Ok(())
            }
            Err(ref err @ io::Error { .. }) if err.kind() == io::ErrorKind::PermissionDenied => {
                trace!("ignore the permission denied, {:?}", entry.path());

                Ok(())
            }
            Err(err) => bail!(err),
        }
    }

    fn verify_mach_file(ofile: &OFile) {
        match ofile {
            OFile::MachFile { ref commands, .. } => for &MachCommand(ref cmd, cmdsize) in commands {
                if let LoadCommand::Command { cmd, ref payload } = cmd {
                    warn!(
                        "unsolved command #{} with {} bytes:\n{}",
                        cmd,
                        cmdsize,
                        HexViewBuilder::new(payload).finish()
                    );
                }
            },
            OFile::FatFile { ref files, .. } => for (_arch, ofile) in files {
                verify_mach_file(ofile)
            },
            OFile::ArFile { ref files } => for (_header, ofile) in files {
                verify_mach_file(ofile)
            },
            OFile::SymDef { .. } => trace!("skip symdef file"),
        }
    }

    #[test]
    fn test_system_binaries() {
        let _ = pretty_env_logger::try_init();

        let mut solved = HashSet::new();

        for path in SYSTEM_PATH.iter() {
            trace!("walk directory for binary: {:?}", path);

            for entry in WalkDir::new(path).follow_links(true).into_iter().filter_map(|e| e.ok()) {
                if solved.contains(entry.path()) {
                    trace!("skip duplicated file: {:?}", entry.path());
                } else {
                    let file_type = entry.file_type();
                    let metadata = entry.metadata().unwrap();

                    if (file_type.is_file() || file_type.is_symlink()) && (metadata.permissions().mode() & 0o111) != 0 {
                        load_mach_file(&entry).expect(&format!("load binary file: {:?}", entry));

                        solved.insert(entry.path().to_owned());
                    }
                }
            }
        }
    }

    #[test]
    fn test_system_libraries() {
        let _ = pretty_env_logger::try_init();

        let mut files = HashSet::new();

        for path in SYSTEM_PATH.iter() {
            trace!("walk directory for library: {:?}", path);

            for entry in WalkDir::new(path).follow_links(true).into_iter().filter_map(|e| e.ok()) {
                if files.contains(entry.path()) {
                    trace!("skip duplicated file: {:?}", entry.path());
                } else {
                    let file_type = entry.file_type();

                    if file_type.is_file() || file_type.is_symlink() {
                        match entry.path().extension().and_then(|ext| ext.to_str()) {
                            Some("dylib") | Some("so") | Some("a") | Some("o") => {
                                load_mach_file(&entry).expect(&format!("load library file: {:?}", entry));
                            }
                            _ => {}
                        }
                    }

                    files.insert(entry.path().to_owned());
                }
            }
        }
    }
}