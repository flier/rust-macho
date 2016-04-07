use std::ffi::CStr;
use std::io::{Read, BufRead, Seek, SeekFrom};
use std::convert::From;
use std::marker::PhantomData;

use byteorder::{ByteOrder, BigEndian, LittleEndian, ReadBytesExt, NativeEndian};
use uuid::Uuid;

use consts::*;

#[derive(Debug)]
pub enum Error {
    Utf8Error(::std::str::Utf8Error),
    FromUtf8Error(::std::string::FromUtf8Error),
    UuidParseError(::uuid::ParseError),
    IoError(::std::io::Error),
    LoadError,
}

impl From<::std::str::Utf8Error> for Error {
    fn from(err: ::std::str::Utf8Error) -> Self {
        Error::Utf8Error(err)
    }
}

impl From<::std::string::FromUtf8Error> for Error {
    fn from(err: ::std::string::FromUtf8Error) -> Self {
        Error::FromUtf8Error(err)
    }
}

impl From<::uuid::ParseError> for Error {
    fn from(err: ::uuid::ParseError) -> Self {
        Error::UuidParseError(err)
    }
}

impl From<::std::io::Error> for Error {
    fn from(err: ::std::io::Error) -> Self {
        Error::IoError(err)
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

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VersionTag(u32);

impl VersionTag {
    pub fn major(self) -> u32 {
        self.0 >> 16
    }

    pub fn minor(self) -> u32 {
        (self.0 >> 8) & 0xFF
    }

    pub fn release(self) -> u32 {
        self.0 & 0xFF
    }
}

impl ::std::fmt::Display for VersionTag {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.release() == 0 {
            write!(f, "{}.{}", self.major(), self.minor())
        } else {
            write!(f, "{}.{}.{}", self.major(), self.minor(), self.release())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuildTarget {
    MacOsX,
    IPhoneOs,
    WatchOs,
    TvOs,
}

impl From<u32> for BuildTarget {
    fn from(cmd: u32) -> Self {
        match cmd {
            LC_VERSION_MIN_MACOSX => BuildTarget::MacOsX,
            LC_VERSION_MIN_IPHONEOS => BuildTarget::IPhoneOs,
            LC_VERSION_MIN_WATCHOS => BuildTarget::WatchOs,
            LC_VERSION_MIN_TVOS => BuildTarget::TvOs,
            _ => unreachable!(),
        }
    }
}

impl Into<u32> for BuildTarget {
    fn into(self) -> u32 {
        match self {
            BuildTarget::MacOsX => LC_VERSION_MIN_MACOSX,
            BuildTarget::IPhoneOs => LC_VERSION_MIN_IPHONEOS,
            BuildTarget::WatchOs => LC_VERSION_MIN_WATCHOS,
            BuildTarget::TvOs => LC_VERSION_MIN_TVOS,
        }
    }
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
    // The uuid load command contains a single 128-bit unique random number that
    // identifies an object produced by the static link editor.
    //
    Uuid {
        /// the 128-bit uuid
        uuid: Uuid,
    },
    // The version_min_command contains the min OS version on which this
    // binary was built to run.
    //
    VersionMin {
        target: BuildTarget,
        version: VersionTag,
        sdk: VersionTag,
    },
    Command {
        /// type of load command
        cmd: u32,
        /// command in bytes
        payload: Vec<u8>,
    },
}

trait ReadStringExt : Read {
    fn read_fixed_size_string(&mut self, len: usize) -> Result<String> {
        let mut buf = Vec::new();

        buf.resize(len + 1, 0);
        buf.truncate(len);

        try!(self.read_exact(buf.as_mut()));

        unsafe { Ok(String::from(try!(CStr::from_ptr(buf.as_ptr() as *const i8).to_str()))) }
    }
}

impl<R: Read + ?Sized> ReadStringExt for R {}

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
            LC_UUID => {
                let mut uuid = [0; 16];

                try!(buf.read_exact(&mut uuid[..]));

                LoadCommand::Uuid { uuid: try!(Uuid::from_bytes(&uuid[..])) }
            }
            LC_VERSION_MIN_MACOSX |
            LC_VERSION_MIN_IPHONEOS |
            LC_VERSION_MIN_WATCHOS |
            LC_VERSION_MIN_TVOS => {
                LoadCommand::VersionMin {
                    target: BuildTarget::from(cmd),
                    version: VersionTag(try!(buf.read_u32::<O>())),
                    sdk: VersionTag(try!(buf.read_u32::<O>())),
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
            &LoadCommand::Uuid {..} => LC_UUID,
            &LoadCommand::VersionMin {target, ..} => BuildTarget::into(target),
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
    pub files: Vec<Box<MachFile>>,
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
            _ => Err(Error::LoadError),
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
            files: vec![Box::new(MachFile {
                            header: header,
                            commands: commands,
                        })],
        })
    }
}

#[cfg(test)]
pub mod tests {
    extern crate env_logger;

    use std::io::Cursor;

    use super::super::*;
    use super::Section64;

    include!("testdata.rs");

    macro_rules! setup_test_universal_file {
        () => ({
            let _ = env_logger::init();

            let mut cursor = Cursor::new(prepare_test_mach_header());

            let file = UniversalFile::load(&mut cursor).unwrap();

            assert_eq!(file.files.len(), 1);

            file
        })
    }

    #[test]
    fn test_parse_mach_header() {
        let file = setup_test_universal_file!();

        let file = file.files[0].as_ref();

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

    #[test]
    fn test_parse_segments() {
        let file = setup_test_universal_file!();

        let file = file.files[0].as_ref();

        if let LoadCommand::Segment64 {ref segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, flags, ref sections} = file.commands[0] {
           assert_eq!(segname, SEG_PAGEZERO);
           assert_eq!(vmaddr, 0);
           assert_eq!(vmsize, 0x0000000100000000);
           assert_eq!(fileoff, 0);
           assert_eq!(filesize, 0);
           assert_eq!(maxprot, 0);
           assert_eq!(initprot, 0);
           assert_eq!(flags, 0);
           assert!(sections.is_empty());
        } else {
            panic!();
        }

        if let LoadCommand::Segment64 {ref segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, flags, ref sections} = file.commands[1] {
           assert_eq!(segname, SEG_TEXT);
           assert_eq!(vmaddr, 0x0000000100000000);
           assert_eq!(vmsize, 0x00000000001e3000);
           assert_eq!(fileoff, 0);
           assert_eq!(filesize, 0x1e3000);
           assert_eq!(maxprot, 7);
           assert_eq!(initprot, 5);
           assert_eq!(flags, 0);
           assert_eq!(sections.len(), 8);

           assert_eq!(sections.iter().map(|sec: &Section64| sec.sectname.clone()).collect::<Vec<String>>(),
                      vec![SECT_TEXT, "__stubs", "__stub_helper", "__gcc_except_tab", "__const", "__cstring", "__unwind_info", "__eh_frame"]);
        } else {
            panic!();
        }


        if let LoadCommand::Segment64 {ref segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, flags, ref sections} = file.commands[2] {
           assert_eq!(segname, SEG_DATA);
           assert_eq!(vmaddr, 0x00000001001e3000);
           assert_eq!(vmsize, 0x0000000000013000);
           assert_eq!(fileoff, 0x1e3000);
           assert_eq!(filesize, 0x12000);
           assert_eq!(maxprot, 7);
           assert_eq!(initprot, 3);
           assert_eq!(flags, 0);
           assert_eq!(sections.len(),10);

           assert_eq!(sections.iter().map(|sec: &Section64| sec.sectname.clone()).collect::<Vec<String>>(),
                      vec!["__nl_symbol_ptr", "__got", "__la_symbol_ptr", "__mod_init_func", "__const",
                           SECT_DATA, "__thread_vars", "__thread_data", SECT_COMMON, SECT_BSS]);
        } else {
            panic!();
        }


        if let LoadCommand::Segment64 {ref segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, flags, ref sections} = file.commands[3] {
           assert_eq!(segname, SEG_LINKEDIT);
           assert_eq!(vmaddr, 0x00000001001f6000);
           assert_eq!(vmsize, 0x000000000017a000);
           assert_eq!(fileoff, 0x1f5000);
           assert_eq!(filesize, 0x1790b4);
           assert_eq!(maxprot, 7);
           assert_eq!(initprot, 1);
           assert_eq!(flags, 0);
           assert!(sections.is_empty());
        } else {
            panic!();
        }
    }

    #[test]
    fn test_parse_uuid_command() {
        let file = setup_test_universal_file!();

        let file = file.files[0].as_ref();

        if let LoadCommand::Uuid {ref uuid} = file.commands[8] {
            assert_eq!(uuid.hyphenated().to_string(),
                       "92e3cf1f-20da-3373-a98c-851366d353bf");
        } else {
            panic!();
        }
    }

    #[test]
    fn test_parse_min_version_command() {
        let file = setup_test_universal_file!();

        let file = file.files[0].as_ref();

        if let LoadCommand::VersionMin{ target, version, sdk} = file.commands[9] {
            assert_eq!(target, BuildTarget::MacOsX);
            assert_eq!(version.to_string(), "10.11");
            assert_eq!(sdk.to_string(), "10.11");
        } else {
            panic!();
        }
    }
}
