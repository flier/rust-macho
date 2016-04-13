# rust-macho [![travis](https://travis-ci.org/flier/rust-macho.svg?branch=master)](https://travis-ci.org/flier/rust-macho)
Mach-O File Format Parser for Rust

## Examples

Use OFile::parse to read the mach-o file from a &[u8] slice.

```rust
use std::io::{Read, Cursor};
use std::fs::File;
use macho::{OFile, CPU_TYPE_X86_64, MachCommand, LoadCommand};

let mut f = File::open("test/helloworld").unwrap();
let mut buf = Vec::new();
let size = f.read_to_end(&mut buf).unwrap();
let mut cur = Cursor::new(&buf[..size]);
if let OFile::MachFile { ref header, ref commands } = OFile::parse(&mut cur).unwrap() {
    assert_eq!(header.cputype, CPU_TYPE_X86_64);
    assert_eq!(header.ncmds as usize, commands.len());
    for &MachCommand(ref cmd, cmdsize) in commands {
        if let &LoadCommand::Segment64 { ref segname, ref sections, .. } = cmd {
            println!("segment: {}", segname);

                for ref sect in sections {
                    println!("  section: {}", sect.sectname);
            }
        }
    }
}
```

For more detail, please check the unit tests and the [otool](examples/otool.rs) example.
