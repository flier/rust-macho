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


// The layout of the file depends on the filetype.  For all but the MH_OBJECT
// file type the segments are padded out and aligned on a segment alignment
// boundary for efficient demand pageing.  The MH_EXECUTE, MH_FVMLIB, MH_DYLIB,
// MH_DYLINKER and MH_BUNDLE file types also have the headers included as part
// of their first segment.
//
// The file type MH_OBJECT is a compact format intended as output of the
// assembler and input (and possibly output) of the link editor (the .o
// format).  All sections are in one unnamed segment with no segment padding.
// This format is used as an executable format when the file is so small the
// segment padding greatly increases its size.
//
// The file type MH_PRELOAD is an executable format intended for things that
// are not executed under the kernel (proms, stand alones, kernels, etc).  The
// format can be executed under the kernel but may demand paged it and not
// preload it before execution.
//
// A core file is in MH_CORE format and can be any in an arbritray legal
// Mach-O file.
//
// Constants for the filetype field of the mach_header
//

/// relocatable object file
pub const MH_OBJECT: u32 = 0x1;
/// demand paged executable file
pub const MH_EXECUTE: u32 = 0x2;
/// fixed VM shared library file
pub const MH_FVMLIB: u32 = 0x3;
/// core file
pub const MH_CORE: u32 = 0x4;
/// preloaded executable file
pub const MH_PRELOAD: u32 = 0x5;
/// dynamically bound shared library
pub const MH_DYLIB: u32 = 0x6;
/// dynamic link editor
pub const MH_DYLINKER: u32 = 0x7;
/// dynamically bound bundle file
pub const MH_BUNDLE: u32 = 0x8;
/// shared library stub for static linking only, no section contents
pub const MH_DYLIB_STUB: u32 = 0x9;
/// companion file with only debug sections
pub const MH_DSYM: u32 = 0xa;
/// x86_64 kexts
pub const MH_KEXT_BUNDLE: u32 = 0xb;

// Constants for the flags field of the mach_header
//

/// the object file has no undefined references
pub const MH_NOUNDEFS: u32 = 0x1;
/// the object file is the output of an incremental link against a base file and can't be link edited again
pub const MH_INCRLINK: u32 = 0x2;
// the object file is input for the dynamic linker and can't be staticly link edited again
pub const MH_DYLDLINK: u32 = 0x4;
// the object file's undefined references are bound by the dynamic linker when loaded.
pub const MH_BINDATLOAD: u32 = 0x8;
/// the file has its dynamic undefined references prebound.
pub const MH_PREBOUND: u32 = 0x10;
/// the file has its read-only and read-write segments split
pub const MH_SPLIT_SEGS: u32 = 0x20;
/// the shared library init routine is to be run lazily via catching memory faults to its writeable segments (obsolete)
pub const MH_LAZY_INIT: u32 = 0x40;
/// the image is using two-level name space bindings
pub const MH_TWOLEVEL: u32 = 0x80;
/// the executable is forcing all images to use flat name space bindings
pub const MH_FORCE_FLAT: u32 = 0x100;
/// this umbrella guarantees no multiple defintions of symbols in its sub-images so the two-level namespace hints can always be used.
pub const MH_NOMULTIDEFS: u32 = 0x200;
/// do not have dyld notify the prebinding agent about this executable
pub const MH_NOFIXPREBINDING: u32 = 0x400;
/// the binary is not prebound but can have its prebinding redone. only used when MH_PREBOUND is not set.
pub const MH_PREBINDABLE: u32 = 0x800;
/// indicates that this binary binds to all two-level namespace modules of its dependent libraries.
/// only used when MH_PREBINDABLE and MH_TWOLEVEL are both set.
pub const MH_ALLMODSBOUND: u32 = 0x1000;
/// safe to divide up the sections into sub-sections via symbols for dead code stripping
pub const MH_SUBSECTIONS_VIA_SYMBOLS: u32 = 0x2000;
/// the binary has been canonicalized via the unprebind operation
pub const MH_CANONICAL: u32 = 0x4000;
/// the final linked image contains external weak symbols
pub const MH_WEAK_DEFINES: u32 = 0x8000;
/// the final linked image uses weak symbols
pub const MH_BINDS_TO_WEAK: u32 = 0x10000;
/// When this bit is set, all stacks in the task will be given stack execution privilege. 
/// Only used in MH_EXECUTE filetypes.
pub const MH_ALLOW_STACK_EXECUTION: u32 = 0x20000;
/// When this bit is set, the binary declares it is safe for use in processes with uid zero
pub const MH_ROOT_SAFE: u32 = 0x40000;
/// When this bit is set, the binary declares it is safe for use in processes when issetugid() is true
pub const MH_SETUID_SAFE: u32 = 0x80000;
/// When this bit is set on a dylib, the static linker does not need to examine dependent dylibs to see if any are re-exported
pub const MH_NO_REEXPORTED_DYLIBS: u32 = 0x100000;
/// When this bit is set, the OS will load the main executable at a random address.  Only used in MH_EXECUTE filetypes.
pub const MH_PIE: u32 = 0x200000;
/// Only for use on dylibs.  When linking against a dylib that has this bit set,
/// the static linker will automatically not create a LC_LOAD_DYLIB load command 
/// to the dylib if no symbols are being referenced from the dylib.
pub const MH_DEAD_STRIPPABLE_DYLIB: u32 = 0x400000;
/// Contains a section of type S_THREAD_LOCAL_VARIABLES
pub const MH_HAS_TLV_DESCRIPTORS: u32 = 0x800000;
/// When this bit is set, the OS will run the main executable with a non-executable heap even on platforms (e.g. i386)
/// that don't require it. Only used in MH_EXECUTE filetypes.
pub const MH_NO_HEAP_EXECUTION: u32 = 0x1000000;
/// The code was linked for use in an application extension.
pub const MH_APP_EXTENSION_SAFE: u32 = 0x02000000;


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

// Constants for the flags field of the segment_command

/// the file contents for this segment is for the high part of the VM space,
/// the low part is zero filled (for stacks in core files)
pub const SG_HIGHVM: u32 = 0x1;
/// this segment is the VM that is allocated by a fixed VM library,
/// for overlap checking in the link editor
pub const SG_FVMLIB: u32 = 0x2;
/// this segment has nothing that was relocated in it and nothing relocated to it,
/// that is it maybe safely replaced without relocation
pub const SG_NORELOC: u32 = 0x4;
/// This segment is protected.  If the segment starts at file offset 0,
/// the first page of the segment is not protected.
/// All other pages of the segment are protected.
pub const SG_PROTECTED_VERSION_1: u32 = 0x8;
