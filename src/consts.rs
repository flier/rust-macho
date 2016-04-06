#![allow(dead_code)]
#![allow(non_camel_case_types)]

pub type cpu_type_t = i32;
pub type cpu_subtype_t = i32;
pub type vm_prot_t = i32;

// Capability bits used in the definition of cpu_type.
//

/// mask for architecture bits
pub const CPU_ARCH_MASK: cpu_type_t = 0xff000000u64 as cpu_type_t;
/// 64 bit ABI
pub const CPU_ARCH_ABI64: cpu_type_t = 0x01000000u64 as cpu_type_t;

//  Machine types known by all.
//

pub const CPU_TYPE_ANY: cpu_type_t = -1;

pub const CPU_TYPE_VAX: cpu_type_t = 1;
pub const CPU_TYPE_MC680X0: cpu_type_t = 6;
pub const CPU_TYPE_X86: cpu_type_t = 7;
pub const CPU_TYPE_I386: cpu_type_t = CPU_TYPE_X86;
pub const CPU_TYPE_X86_64: cpu_type_t = (CPU_TYPE_X86 | CPU_ARCH_ABI64);
pub const CPU_TYPE_MIPS: cpu_type_t = 8;
pub const CPU_TYPE_MC98000: cpu_type_t = 10;
pub const CPU_TYPE_HPPA: cpu_type_t = 11;
pub const CPU_TYPE_ARM: cpu_type_t = 12;
pub const CPU_TYPE_ARM64: cpu_type_t = (CPU_TYPE_ARM | CPU_ARCH_ABI64);
pub const CPU_TYPE_MC88000: cpu_type_t = 13;
pub const CPU_TYPE_SPARC: cpu_type_t = 14;
pub const CPU_TYPE_I860: cpu_type_t = 15;
pub const CPU_TYPE_ALPHA: cpu_type_t = 16;
pub const CPU_TYPE_POWERPC: cpu_type_t = 18;
pub const CPU_TYPE_POWERPC64: cpu_type_t = (CPU_TYPE_POWERPC | CPU_ARCH_ABI64);

// Constant for the magic field of the mach_header (32-bit architectures)
//

/// the mach magic number
pub const MH_MAGIC: u32 = 0xfeedface;
/// NXSwapInt(MH_MAGIC)
pub const MH_CIGAM: u32 = 0xcefaedfe;

// Constant for the magic field of the mach_header_64 (64-bit architectures)
//

/// the 64-bit mach magic number
pub const MH_MAGIC_64: u32 = 0xfeedfacf;
/// NXSwapInt(MH_MAGIC_64)
pub const MH_CIGAM_64: u32 = 0xcffaedfe;


// After MacOS X 10.1 when a new load command is added that is required to be
// understood by the dynamic linker for the image to execute properly the
// LC_REQ_DYLD bit will be or'ed into the load command constant.  If the dynamic
// linker sees such a load command it it does not understand will issue a
// "unknown load command required for execution" error and refuse to use the
// image.  Other load commands without this bit that are not understood will
// simply be ignored.
//
pub const LC_REQ_DYLD: u32 = 0x80000000;

// Constants for the cmd field of all load commands, the type
pub const LC_SEGMENT: u32 = 0x1; /* segment of this file to be mapped */
pub const LC_SYMTAB: u32 = 0x2; /* link-edit stab symbol table info */
pub const LC_SYMSEG: u32 = 0x3; /* link-edit gdb symbol table info (obsolete) */
pub const LC_THREAD: u32 = 0x4; /* thread */
pub const LC_UNIXTHREAD: u32 = 0x5; /* unix thread (includes a stack) */
pub const LC_LOADFVMLIB: u32 = 0x6; /* load a specified fixed VM shared library */
pub const LC_IDFVMLIB: u32 = 0x7; /* fixed VM shared library identification */
pub const LC_IDENT: u32 = 0x8; /* object identification info (obsolete) */
pub const LC_FVMFILE: u32 = 0x9; /* fixed VM file inclusion (internal use) */
pub const LC_PREPAGE: u32 = 0xa;     /* prepage command (internal use) */
pub const LC_DYSYMTAB: u32 = 0xb; /* dynamic link-edit symbol table info */
pub const LC_LOAD_DYLIB: u32 = 0xc; /* load a dynamically linked shared library */
pub const LC_ID_DYLIB: u32 = 0xd; /* dynamically linked shared lib ident */
pub const LC_LOAD_DYLINKER: u32 = 0xe;    /* load a dynamic linker */
pub const LC_ID_DYLINKER: u32 = 0xf; /* dynamic linker identification */
pub const LC_PREBOUND_DYLIB: u32 = 0x10;  /* modules prebound for a dynamically */

// linked shared library
pub const LC_ROUTINES: u32 = 0x11;    /* image routines */
pub const LC_SUB_FRAMEWORK: u32 = 0x12;   /* sub framework */
pub const LC_SUB_UMBRELLA: u32 = 0x13;    /* sub umbrella */
pub const LC_SUB_CLIENT: u32 = 0x14;    /* sub client */
pub const LC_SUB_LIBRARY: u32 = 0x15;    /* sub library */
pub const LC_TWOLEVEL_HINTS: u32 = 0x16;  /* two-level namespace lookup hints */
pub const LC_PREBIND_CKSUM: u32 = 0x17;  /* prebind checksum */

// load a dynamically linked shared library that is allowed to be missing
// (all symbols are weak imported).
//
pub const LC_LOAD_WEAK_DYLIB: u32 = (0x18 | LC_REQ_DYLD);
pub const LC_SEGMENT_64: u32 = 0x19;    /* 64-bit segment of this file to be mapped */
pub const LC_ROUTINES_64: u32 = 0x1a;    /* 64-bit image routines */
pub const LC_UUID: u32 = 0x1b;    /* the uuid */
pub const LC_RPATH: u32 = (0x1c | LC_REQ_DYLD);    /* runpath additions */
pub const LC_CODE_SIGNATURE: u32 = 0x1d;  /* local of code signature */
pub const LC_SEGMENT_SPLIT_INFO: u32 = 0x1e; /* local of info to split segments */
pub const LC_REEXPORT_DYLIB: u32 = (0x1f | LC_REQ_DYLD); /* load and re-export dylib */
pub const LC_LAZY_LOAD_DYLIB: u32 = 0x20; /* delay load of dylib until first use */
pub const LC_ENCRYPTION_INFO: u32 = 0x21; /* encrypted segment information */
pub const LC_DYLD_INFO: u32 = 0x22;    /* compressed dyld information */
pub const LC_DYLD_INFO_ONLY: u32 = (0x22 | LC_REQ_DYLD);    /* compressed dyld information only */
pub const LC_LOAD_UPWARD_DYLIB: u32 = (0x23 | LC_REQ_DYLD); /* load upward dylib */
pub const LC_VERSION_MIN_MACOSX: u32 = 0x24;   /* build for MacOSX min OS version */
pub const LC_VERSION_MIN_IPHONEOS: u32 = 0x25; /* build for iPhoneOS min OS version */
pub const LC_FUNCTION_STARTS: u32 = 0x26; /* compressed table of function start addresses */
pub const LC_DYLD_ENVIRONMENT: u32 = 0x27; /* string for dyld to treat like environment variable */
pub const LC_MAIN: u32 = (0x28 | LC_REQ_DYLD); /* replacement for LC_UNIXTHREAD */
pub const LC_DATA_IN_CODE: u32 = 0x29; /* table of non-instructions in __text */
pub const LC_SOURCE_VERSION: u32 = 0x2A; /* source version used to build binary */
pub const LC_DYLIB_CODE_SIGN_DRS: u32 = 0x2B; /* Code signing DRs copied from linked dylibs */
pub const LC_ENCRYPTION_INFO_64: u32 = 0x2C; /* 64-bit encrypted segment information */
pub const LC_LINKER_OPTION: u32 = 0x2D; /* linker options in MH_OBJECT files */
pub const LC_LINKER_OPTIMIZATION_HINT: u32 = 0x2E; /* optimization hints in MH_OBJECT files */

pub fn load_cmd_name(cmd: u32) -> &'static str {
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
