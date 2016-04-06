use std::str;
use std::string;
use std::ffi::CStr;
use std::io;
use std::io::{BufRead, Seek, SeekFrom};
use std::convert::From;
use std::marker::PhantomData;

use byteorder::{ByteOrder, BigEndian, LittleEndian, ReadBytesExt, NativeEndian};

use consts::*;

#[derive(Debug)]
pub enum Error {
    StrFromUtf8(str::Utf8Error),
    StringFromUtf8(string::FromUtf8Error),
    Read(io::Error),
    Load,
    Format,
}

impl From<str::Utf8Error> for Error {
    fn from(err: str::Utf8Error) -> Self {
        Error::StrFromUtf8(err)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self {
        Error::StringFromUtf8(err)
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
        let header = MachHeader {
            magic: try!(buf.read_u32::<O>()),
            cputype: try!(buf.read_i32::<O>()),
            cpusubtype: try!(buf.read_i32::<O>()),
            filetype: try!(buf.read_u32::<O>()),
            ncmds: try!(buf.read_u32::<O>()),
            sizeofcmds: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        };

        Ok(header)
    }
}

impl MachArch for Arch64 {
    fn parse_header<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<MachHeader> {
        let header = MachHeader {
            magic: try!(buf.read_u32::<O>()),
            cputype: try!(buf.read_i32::<O>()),
            cpusubtype: try!(buf.read_i32::<O>()),
            filetype: try!(buf.read_u32::<O>()),
            ncmds: try!(buf.read_u32::<O>()),
            sizeofcmds: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        };

        buf.consume(4);

        Ok(header)
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
}


#[derive(Debug, Clone)]
pub enum LoadCommand {
    /// The segment load command indicates that a part of this file is to be
    /// mapped into the task's address space.  The size of this segment in memory,
    /// vmsize, maybe equal to or larger than the amount to map from this file,
    /// filesize.  The file is mapped starting at fileoff to the beginning of
    /// the segment in memory, vmaddr.  The rest of the memory of the segment,
    /// if any, is allocated zero fill on demand.  The segment's maximum virtual
    /// memory protection and initial virtual memory protection are specified
    /// by the maxprot and initprot fields.  If the segment has sections then the
    /// section structures directly follow the segment command and their size is
    /// reflected in cmdsize.
    ///
    Segment {
        /// segment name
        segname: String,
        /// memory address of this segment
        vmaddr: u32,
        /// memory size of this segment
        vmsize: u32,
        /// file offset of this segment
        fileoff: u32,
        /// amount to map from the file
        filesize: u32,
        /// maximum VM protection
        maxprot: vm_prot_t,
        /// initial VM protection
        initprot: vm_prot_t,
        /// flags
        flags: u32,
        /// sections
        sections: Vec<Section>,
    },
    /// The 64-bit segment load command indicates that a part of this file is to be
    /// mapped into a 64-bit task's address space.  If the 64-bit segment has
    /// sections then section_64 structures directly follow the 64-bit segment
    /// command and their size is reflected in cmdsize.
    ///
    Segment64 {
        /// segment name
        segname: String,
        /// memory address of this segment
        vmaddr: u64,
        /// memory size of this segment
        vmsize: u64,
        /// file offset of this segment
        fileoff: u64,
        /// amount to map from the file
        filesize: u64,
        /// maximum VM protection
        maxprot: vm_prot_t,
        /// initial VM protection
        initprot: vm_prot_t,
        /// flags
        flags: u32,
        /// sections
        sections: Vec<Section64>,
    },
    Command {
        /// type of load command 
        cmd: u32,
        /// command in bytes
        payload: Vec<u8>,
    },
}

trait ReadStringExt : io::Read {
    fn read_fixed_size_string(&mut self, len: usize) -> Result<String> {
        let mut buf = Vec::new();

        buf.resize(len, 0);

        try!(self.read_exact(buf.as_mut()));

        unsafe { Ok(String::from(try!(CStr::from_ptr(buf.as_ptr() as *const i8).to_str()))) }
    }
}

impl<R: io::Read + ?Sized> ReadStringExt for R {}

impl LoadCommand {
    fn parse<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<LoadCommand> {
        let cmd = try!(buf.read_u32::<O>());
        let cmdsize = try!(buf.read_u32::<O>());

        let cmd = match cmd {
            LC_SEGMENT => {
                let segname = try!(buf.read_fixed_size_string(16));
                let vmaddr = try!(buf.read_u32::<O>());
                let vmsize = try!(buf.read_u32::<O>());
                let fileoff = try!(buf.read_u32::<O>());
                let filesize = try!(buf.read_u32::<O>());
                let maxprot = try!(buf.read_i32::<O>());
                let initprot = try!(buf.read_i32::<O>());
                let nsects = try!(buf.read_u32::<O>());
                let flags = try!(buf.read_u32::<O>());
                let mut sections = Vec::new();

                for _ in 0..nsects {
                    sections.push(try!(Section::parse::<T, O>(buf)));
                }

                LoadCommand::Segment {
                    segname: segname,
                    vmaddr: vmaddr,
                    vmsize: vmsize,
                    fileoff: fileoff,
                    filesize: filesize,
                    maxprot: maxprot,
                    initprot: initprot,
                    flags: flags,
                    sections: sections,
                }
            }
            LC_SEGMENT_64 => {
                let segname = try!(buf.read_fixed_size_string(16));
                let vmaddr = try!(buf.read_u64::<O>());
                let vmsize = try!(buf.read_u64::<O>());
                let fileoff = try!(buf.read_u64::<O>());
                let filesize = try!(buf.read_u64::<O>());
                let maxprot = try!(buf.read_i32::<O>());
                let initprot = try!(buf.read_i32::<O>());
                let nsects = try!(buf.read_u32::<O>());
                let flags = try!(buf.read_u32::<O>());
                let mut sections = Vec::new();

                for _ in 0..nsects {
                    sections.push(try!(Section64::parse::<T, O>(buf)));
                }

                LoadCommand::Segment64 {
                    segname: segname,
                    vmaddr: vmaddr,
                    vmsize: vmsize,
                    fileoff: fileoff,
                    filesize: filesize,
                    maxprot: maxprot,
                    initprot: initprot,
                    flags: flags,
                    sections: sections,
                }
            }
            _ => {
                let mut payload = Vec::new();

                payload.resize(cmdsize as usize - 8, 0);

                debug!("load {} command with {} bytes payload",
                       LoadCommand::cmd_name(cmd),
                       payload.len());

                try!(buf.read_exact(payload.as_mut()));

                let cmd = LoadCommand::Command {
                    cmd: cmd,
                    payload: payload,
                };

                cmd
            }
        };

        debug!("parsed {} command: {:?}", cmd.name(), cmd);

        Ok(cmd)
    }

    fn cmd(&self) -> u32 {
        match self {
            &LoadCommand::Segment {..} => LC_SEGMENT,
            &LoadCommand::Segment64 {..} => LC_SEGMENT_64,
            &LoadCommand::Command {cmd, ..} => cmd,
        }
    }

    fn name(&self) -> &'static str {
        Self::cmd_name(self.cmd())
    }


    pub fn cmd_name(cmd: u32) -> &'static str {
        match cmd {
            LC_SEGMENT => "LC_SEGMENT",
            LC_SYMTAB => "LC_SYMTAB",
            LC_SYMSEG => "LC_SYMSEG",
            LC_THREAD => "LC_THREAD",
            LC_UNIXTHREAD => "LC_UNIXTHREAD",
            LC_LOADFVMLIB => "LC_LOADFVMLIB",
            LC_IDFVMLIB => "LC_IDFVMLIB",
            LC_IDENT => "LC_IDENT",
            LC_FVMFILE => "LC_FVMFILE",
            LC_PREPAGE => "LC_PREPAGE",
            LC_DYSYMTAB => "LC_DYSYMTAB",
            LC_LOAD_DYLIB => "LC_LOAD_DYLIB",
            LC_ID_DYLIB => "LC_ID_DYLIB",
            LC_LOAD_DYLINKER => "LC_LOAD_DYLINKER",
            LC_ID_DYLINKER => "LC_ID_DYLINKER",
            LC_PREBOUND_DYLIB => "LC_PREBOUND_DYLIB",
            LC_ROUTINES => "LC_ROUTINES",
            LC_SUB_FRAMEWORK => "LC_SUB_FRAMEWORK",
            LC_SUB_UMBRELLA => "LC_SUB_UMBRELLA",
            LC_SUB_CLIENT => "LC_SUB_CLIENT",
            LC_SUB_LIBRARY => "LC_SUB_LIBRARY",
            LC_TWOLEVEL_HINTS => "LC_TWOLEVEL_HINTS",
            LC_PREBIND_CKSUM => "LC_PREBIND_CKSUM",
            LC_LOAD_WEAK_DYLIB => "LC_LOAD_WEAK_DYLIB",
            LC_SEGMENT_64 => "LC_SEGMENT_64",
            LC_ROUTINES_64 => "LC_ROUTINES_64",
            LC_UUID => "LC_UUID",
            LC_RPATH => "LC_RPATH",
            LC_CODE_SIGNATURE => "LC_CODE_SIGNATURE",
            LC_SEGMENT_SPLIT_INFO => "LC_SEGMENT_SPLIT_INFO",
            LC_REEXPORT_DYLIB => "LC_REEXPORT_DYLIB",
            LC_LAZY_LOAD_DYLIB => "LC_LAZY_LOAD_DYLIB",
            LC_ENCRYPTION_INFO => "LC_ENCRYPTION_INFO",
            LC_DYLD_INFO => "LC_DYLD_INFO",
            LC_DYLD_INFO_ONLY => "LC_DYLD_INFO_ONLY",
            LC_LOAD_UPWARD_DYLIB => "LC_LOAD_UPWARD_DYLIB",
            LC_VERSION_MIN_MACOSX => "LC_VERSION_MIN_MACOSX",
            LC_VERSION_MIN_IPHONEOS => "LC_VERSION_MIN_IPHONEOS",
            LC_FUNCTION_STARTS => "LC_FUNCTION_STARTS",
            LC_DYLD_ENVIRONMENT => "LC_DYLD_ENVIRONMENT",
            LC_MAIN => "LC_MAIN",
            LC_DATA_IN_CODE => "LC_DATA_IN_CODE",
            LC_SOURCE_VERSION => "LC_SOURCE_VERSION",
            LC_DYLIB_CODE_SIGN_DRS => "LC_DYLIB_CODE_SIGN_DRS",
            LC_ENCRYPTION_INFO_64 => "LC_ENCRYPTION_INFO_64",
            LC_LINKER_OPTION => "LC_LINKER_OPTION",
            LC_LINKER_OPTIMIZATION_HINT => "LC_LINKER_OPTIMIZATION_HINT",
            _ => "LC_COMMAND",
        }
    }
}

// A segment is made up of zero or more sections.  Non-MH_OBJECT files have
// all of their segments with the proper sections in each, and padded to the
// specified segment alignment when produced by the link editor.  The first
// segment of a MH_EXECUTE and MH_FVMLIB format file contains the mach_header
// and load commands of the object file before its first section.  The zero
// fill sections are always last in their segment (in all formats).  This
// allows the zeroed segment padding to be mapped into memory where zero fill
// sections might be. The gigabyte zero fill sections, those with the section
// type S_GB_ZEROFILL, can only be in a segment with sections of this type.
// These segments are then placed after all other segments.
//
// The MH_OBJECT format has all of its sections in one segment for
// compactness.  There is no padding to a specified segment boundary and the
// mach_header and load commands are not part of the segment.
//
// Sections with the same section name, sectname, going into the same segment,
// segname, are combined by the link editor.  The resulting section is aligned
// to the maximum alignment of the combined sections and is the new section's
// alignment.  The combined sections are aligned to their original alignment in
// the combined section.  Any padded bytes to get the specified alignment are
// zeroed.
//
// The format of the relocation entries referenced by the reloff and nreloc
// fields of the section structure for mach object files is described in the
// header file <reloc.h>.
//

#[derive(Debug, Clone)]
pub struct Section {
    /// name of this section
    pub sectname: String,
    /// segment this section goes in
    pub segname: String,
    /// memory address of this section
    pub addr: u32,
    /// size in bytes of this section
    pub size: u32,
    /// file offset of this section
    pub offset: u32,
    /// section alignment (power of 2)
    pub align: u32,
    /// file offset of relocation entries
    pub reloff: u32,
    /// number of relocation entries
    pub nreloc: u32,
    /// flags (section type and attributes)
    pub flags: u32,
}

#[derive(Debug, Clone)]
pub struct Section64 {
    /// name of this section
    pub sectname: String,
    /// segment this section goes in
    pub segname: String,
    /// memory address of this section
    pub addr: u64,
    /// size in bytes of this section
    pub size: u64,
    /// file offset of this section
    pub offset: u32,
    /// section alignment (power of 2)
    pub align: u32,
    /// file offset of relocation entries
    pub reloff: u32,
    /// number of relocation entries
    pub nreloc: u32,
    /// flags (section type and attributes)
    pub flags: u32,
}

impl Section {
    fn parse<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<Section> {
        let section = Section {
            sectname: try!(buf.read_fixed_size_string(16)),
            segname: try!(buf.read_fixed_size_string(16)),
            addr: try!(buf.read_u32::<O>()),
            size: try!(buf.read_u32::<O>()),
            offset: try!(buf.read_u32::<O>()),
            align: try!(buf.read_u32::<O>()),
            reloff: try!(buf.read_u32::<O>()),
            nreloc: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        };

        buf.consume(8);

        Ok(section)
    }
}

impl Section64 {
    fn parse<T: BufRead, O: ByteOrder>(buf: &mut T) -> Result<Section64> {
        let section = Section64 {
            sectname: try!(buf.read_fixed_size_string(16)),
            segname: try!(buf.read_fixed_size_string(16)),
            addr: try!(buf.read_u64::<O>()),
            size: try!(buf.read_u64::<O>()),
            offset: try!(buf.read_u32::<O>()),
            align: try!(buf.read_u32::<O>()),
            reloff: try!(buf.read_u32::<O>()),
            nreloc: try!(buf.read_u32::<O>()),
            flags: try!(buf.read_u32::<O>()),
        };

        buf.consume(12);

        Ok(section)
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

impl<A: MachArch, O: ByteOrder> MachLoader<A, O> {
    pub fn parse<T: BufRead>(buf: &mut T) -> Result<UniversalFile> {
        let header = try!(A::parse_header::<T, O>(buf));

        debug!("parsed file header: {:?}", header);

        let mut commands = Vec::new();

        for _ in 0..header.ncmds as usize {
            commands.push(try!(LoadCommand::parse::<T, O>(buf)));
        }

        debug!("parsed {} load commands", commands.len());

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

    use super::super::*;

    include!("testdata.rs");

    #[test]
    fn test_parse_mach_header() {
        let _ = env_logger::init();

        let mut header = test_mach_header_new();

        let mut cursor = Cursor::new(header);

        let file = UniversalFile::load(&mut cursor).unwrap();

        assert_eq!(file.files.len(), 1);

        let file = &file.files[0];

        assert_eq!(file.header.magic, MH_MAGIC_64);
        assert_eq!(file.header.cputype, CPU_TYPE_X86_64);
        assert_eq!(file.header.cpusubtype, 0x80000003);
        assert_eq!(file.header.filetype, MH_EXECUTE);
        assert_eq!(file.header.ncmds, 15);
        assert_eq!(file.header.sizeofcmds, 2080);
        assert_eq!(file.header.flags, 0x00a18085);

        assert_eq!(file.commands.len(), 15);
        assert_eq!(file.commands.iter().map(|cmd| cmd.cmd()).collect::<Vec<u32>>(),
                   vec![LC_SEGMENT_64,
                        LC_SEGMENT_64,
                        LC_SEGMENT_64,
                        LC_SEGMENT_64,
                        LC_DYLD_INFO_ONLY,
                        LC_SYMTAB,
                        LC_DYSYMTAB,
                        LC_LOAD_DYLINKER,
                        LC_UUID,
                        LC_VERSION_MIN_MACOSX,
                        LC_SOURCE_VERSION,
                        LC_MAIN,
                        LC_LOAD_DYLIB,
                        LC_FUNCTION_STARTS,
                        LC_DATA_IN_CODE]);
    }
}
