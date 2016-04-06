use std::str;
use std::io;
use std::io::{BufRead, Seek, SeekFrom};
use std::convert::From;
use std::iter::Iterator;
use std::marker::PhantomData;

use byteorder::{ByteOrder, BigEndian, LittleEndian, ReadBytesExt, NativeEndian};

use consts::*;

#[derive(Debug)]
pub enum Error {
    Utf8(str::Utf8Error),
    Read(io::Error),
    Load,
    Format,
}

impl From<str::Utf8Error> for Error {
    fn from(err: str::Utf8Error) -> Self {
        Error::Utf8(err)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Read(err)
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;

pub trait MachArch {
    fn parse_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader>;
}

pub enum Arch32 {}
pub enum Arch64 {}

impl MachArch for Arch32 {
    fn parse_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader> {
        Ok(MachHeader {
            magic: try!(buf.read_u32::<O>()),
            cputype: try!(buf.read_i32::<O>()),
            cpusubtype: try!(buf.read_i32::<O>()),
            filetype: try!(buf.read_u32::<O>()),
            ncmds: try!(buf.read_u32::<O>()),
            sizeofcmds: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
            reserved: 0,
        })
    }
}

impl MachArch for Arch64 {
    fn parse_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader> {
        Ok(MachHeader {
            magic: try!(buf.read_u32::<O>()),
            cputype: try!(buf.read_i32::<O>()),
            cpusubtype: try!(buf.read_i32::<O>()),
            filetype: try!(buf.read_u32::<O>()),
            ncmds: try!(buf.read_u32::<O>()),
            sizeofcmds: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
            reserved: try!(buf.read_u32::<O>()),
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct MachHeader {
    pub magic: u32,
    pub cputype: cpu_type_t,
    pub cpusubtype: cpu_subtype_t,
    pub filetype: u32,
    pub ncmds: u32,
    pub sizeofcmds: u32,
    pub flags: u32,
    pub reserved: u32,
}

#[derive(Debug, Default, Clone)]
pub struct LoadCommand {
    pub cmd: u32,
    pub payload: Vec<u8>,
}

impl LoadCommand {
    #[inline]
    fn name(&self) -> &'static str {
        load_cmd_name(self.cmd)
    }

    fn parse<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<LoadCommand> {
        let cmd = try!(buf.read_u32::<O>());
        let cmdsize = try!(buf.read_u32::<O>());
        let mut payload = Vec::new();

        payload.resize(cmdsize as usize - 8, 0);

        debug!("load {} command with {} bytes payload",
               load_cmd_name(cmd),
               payload.len());

        try!(buf.read_exact(payload.as_mut()));

        let cmd = LoadCommand {
            cmd: cmd,
            payload: payload,
        };

        Ok(cmd)
    }
}

#[derive(Debug, Default, Clone)]
pub struct MachSegment {
    pub cmd: u32,
    pub cmdsize: u32,
    pub segname: String,
    pub vmaddr: u32,
    pub vmsize: u32,
    pub fileoff: u32,
    pub filesize: u32,
    pub maxprot: vm_prot_t,
    pub initprot: vm_prot_t,
    pub nsects: u32,
    pub flags: u32,
}

impl MachSegment {
    fn parse<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachSegment> {
        Ok(MachSegment {
            cmd: try!(buf.read_u32::<O>()),
            cmdsize: try!(buf.read_u32::<O>()),
            segname: {
                let mut name = [0; 16];
                try!(buf.read_exact(&mut name));
                String::from(try!(str::from_utf8(&name[..])))
            },
            vmaddr: try!(buf.read_u32::<O>()),
            vmsize: try!(buf.read_u32::<O>()),
            fileoff: try!(buf.read_u32::<O>()),
            filesize: try!(buf.read_u32::<O>()),
            maxprot: try!(buf.read_i32::<O>()),
            initprot: try!(buf.read_i32::<O>()),
            nsects: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct MachFile {
    pub header: MachHeader,
    pub commands: Vec<LoadCommand>,
}

#[derive(Debug, Default,  Clone)]
pub struct UniversalFile {
    pub files: Vec<MachFile>,
}

impl UniversalFile {
    pub fn load<T: BufRead + Seek>(buf: &mut T) -> Result<UniversalFile> {
        let magic = try!(buf.read_u32::<NativeEndian>());

        try!(buf.seek(SeekFrom::Current(-4)));

        debug!("parsing mach-o file with magic 0x{:x}", magic);

        match magic {
            MH_MAGIC => MachLoader::<Arch32, LittleEndian>::parse(buf),
            MH_CIGAM => MachLoader::<Arch32, BigEndian>::parse(buf),
            MH_MAGIC_64 => MachLoader::<Arch64, LittleEndian>::parse(buf),
            MH_CIGAM_64 => MachLoader::<Arch64, BigEndian>::parse(buf),
            _ => Err(Error::Load),
        }
    }
}

pub struct MachLoader<A: MachArch, O: ByteOrder> {
    _arch: PhantomData<A>,
    _order: PhantomData<O>,
}

fn dump_commands(commands: &Vec<LoadCommand>) -> String {
    commands.iter()
            .fold(String::new(),
                  |s, c| s + &*format!("{}:{} ", c.cmd, c.payload.len()))
}

impl<A: MachArch, O: ByteOrder> MachLoader<A, O> {
    pub fn parse<T: BufRead>(buf: &mut T) -> Result<UniversalFile> {
        let header = try!(A::parse_header::<T, O>(buf));

        debug!("parsed file header: {:?}", header);

        let mut commands = Vec::new();

        for _ in 0..header.ncmds as usize {
            commands.push(try!(LoadCommand::parse::<T, O>(buf)));
        }

        debug!("parsed {} load commands: {}",
               commands.len(),
               dump_commands(&commands));

        Ok(UniversalFile {
            files: vec![MachFile {
                            header: header,
                            commands: commands,
                        }],
        })
    }
}

#[cfg(test)]
pub mod tests {
    extern crate env_logger;

    use std::io::Cursor;

    use super::UniversalFile;

    include!("testdata.rs");

    #[test]
    fn test_load() {
        let _ = env_logger::init();

        let mut header = mach_header_new();

        let mut cursor = Cursor::new(header);
        let file = UniversalFile::load(&mut cursor).unwrap();
    }
}
