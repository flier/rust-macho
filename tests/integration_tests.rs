#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
extern crate memmap;
#[cfg(test)]
extern crate pretty_env_logger;

extern crate mach_object;

#[cfg(all(target_os = "macos", feature = "integration_tests"))]
mod integration {
    use std::collections::HashSet;
    use std::fs::{self, DirEntry, File};
    use std::io::{self, Cursor};
    use std::os::unix::fs::PermissionsExt;
    use std::path::Path;

    use failure::Error;
    use memmap::Mmap;
    use pretty_env_logger;

    use mach_object::OFile;

    const SYSTEM_BINARY_PATH: &[&str] = &[
        "/bin",
        "/sbin",
        "/usr/bin",
        "/usr/sbin",
        "/usr/libexec",
        "/usr/local/bin",
        "/usr/local/sbin",
        "/usr/local/libexec",
    ];

    const SYSTEM_LIBRARY_PATH: &[&str] = &["/usr/lib", "/usr/local/lib"];

    const SYSTEM_FRAMEWORK_PATH: &[&str] = &[
        "/System/Library/Frameworks",
        "/Library/Frameworks",
        "/usr/local/Frameworks",
    ];

    const SYSTEM_APPLICATION_PATH: &[&str] = &["/Applications", "~/Applications"];

    fn load_mach_file(entry: &DirEntry) -> Result<(), Error> {
        let file_type = entry.file_type().unwrap();

        let path = if file_type.is_file() {
            entry.path()
        } else if file_type.is_symlink() {
            let symlink = entry.path();
            let dir = symlink.parent().unwrap();

            let path = dir.join(symlink.read_link().expect(&format!("read symlink: {:?}", entry)));

            if !path.exists() {
                trace!("skip the broken link: {:?} -> {:?}", entry, symlink);

                return Ok(());
            }

            path
        } else {
            warn!("unexpected file {:?}", entry);

            unreachable!()
        };

        match File::open(path.clone()) {
            Ok(file) => {
                let mmap = unsafe { Mmap::map(&file) }?;
                let payload = mmap.as_ref();

                if payload.starts_with(b"#") {
                    trace!("skip the scripts, {:?}", path);
                } else {
                    let mut cur = Cursor::new(payload);
                    let ofile = OFile::parse(&mut cur)?;

                    trace!("loaded ofile, {:?}", path);
                }

                Ok(())
            }
            Err(ref err @ io::Error { .. }) if err.kind() == io::ErrorKind::PermissionDenied => {
                trace!("ignore the permission denied, {:?}", path);

                Ok(())
            }
            Err(err) => bail!(err),
        }
    }

    #[test]
    fn test_system_binaries() {
        pretty_env_logger::try_init();

        let mut files = HashSet::new();

        for path in SYSTEM_BINARY_PATH {
            let path = Path::new(path);

            if path.exists() && path.is_dir() {
                trace!("walk directory: {:?}", path);

                for entry in fs::read_dir(path).unwrap() {
                    if let Ok(entry) = entry {
                        let path = entry.path();

                        if files.contains(&path) {
                            trace!("skip duplicated file: {:?}", path);
                        } else {
                            let file_type = entry.file_type().unwrap();
                            let metadata = entry.metadata().unwrap();

                            if (file_type.is_file() || file_type.is_symlink())
                                && (metadata.permissions().mode() & 0o111) != 0
                            {
                                load_mach_file(&entry).expect(&format!("load binary file: {:?}", entry));

                                files.insert(path);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_system_libraries() {
        pretty_env_logger::try_init();

        let mut files = HashSet::new();

        for path in SYSTEM_LIBRARY_PATH {
            let path = Path::new(path);

            if path.exists() && path.is_dir() {
                trace!("walk directory: {:?}", path);

                for entry in fs::read_dir(path).unwrap() {
                    if let Ok(entry) = entry {
                        let path = entry.path();

                        if files.contains(&path) {
                            trace!("skip duplicated file: {:?}", path);
                        } else {
                            match path.extension().and_then(|ext| ext.to_str()) {
                                Some("dylib") | Some("so") | Some("a") | Some("o") => {
                                    load_mach_file(&entry).expect(&format!("load library file: {:?}", entry));
                                }
                                ext => trace!("skip file with extention: {:?}", ext),
                            }

                            files.insert(path);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_system_frameworks() {
        pretty_env_logger::try_init();
    }

    #[test]
    fn test_system_applications() {
        pretty_env_logger::try_init();
    }
}
