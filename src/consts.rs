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
// segment of this file to be mapped

pub const LC_SEGMENT: u32 = 0x1;
/// link-edit stab symbol table info
pub const LC_SYMTAB: u32 = 0x2;
/// link-edit gdb symbol table info (obsolete)
pub const LC_SYMSEG: u32 = 0x3;
/// thread
pub const LC_THREAD: u32 = 0x4;
/// unix thread (includes a stack)
pub const LC_UNIXTHREAD: u32 = 0x5;
/// load a specified fixed VM shared library
pub const LC_LOADFVMLIB: u32 = 0x6;
/// fixed VM shared library identification
pub const LC_IDFVMLIB: u32 = 0x7;
/// object identification info (obsolete)
pub const LC_IDENT: u32 = 0x8;
/// fixed VM file inclusion (internal use)
pub const LC_FVMFILE: u32 = 0x9;
/// prepage command (internal use)
pub const LC_PREPAGE: u32 = 0xa;
/// dynamic link-edit symbol table info
pub const LC_DYSYMTAB: u32 = 0xb;
/// load a dynamically linked shared library
pub const LC_LOAD_DYLIB: u32 = 0xc;
/// dynamically linked shared lib ident
pub const LC_ID_DYLIB: u32 = 0xd;
/// load a dynamic linker
pub const LC_LOAD_DYLINKER: u32 = 0xe;
/// dynamic linker identification
pub const LC_ID_DYLINKER: u32 = 0xf;
/// modules prebound for a dynamically
pub const LC_PREBOUND_DYLIB: u32 = 0x10;

// linked shared library
//
// image routines

pub const LC_ROUTINES: u32 = 0x11;
/// sub framework
pub const LC_SUB_FRAMEWORK: u32 = 0x12;
/// sub umbrella
pub const LC_SUB_UMBRELLA: u32 = 0x13;
/// sub client
pub const LC_SUB_CLIENT: u32 = 0x14;
/// sub library
pub const LC_SUB_LIBRARY: u32 = 0x15;
/// two-level namespace lookup hints
pub const LC_TWOLEVEL_HINTS: u32 = 0x16;
/// prebind checksum
pub const LC_PREBIND_CKSUM: u32 = 0x17;

// load a dynamically linked shared library that is allowed to be missing
// (all symbols are weak imported).
//
pub const LC_LOAD_WEAK_DYLIB: u32 = (0x18 | LC_REQ_DYLD);
/// 64-bit segment of this file to be mapped
pub const LC_SEGMENT_64: u32 = 0x19;
/// 64-bit image routines
pub const LC_ROUTINES_64: u32 = 0x1a;
/// the uuid
pub const LC_UUID: u32 = 0x1b;
/// runpath additions
pub const LC_RPATH: u32 = (0x1c | LC_REQ_DYLD);
/// local of code signature
pub const LC_CODE_SIGNATURE: u32 = 0x1d;
/// local of info to split segments
pub const LC_SEGMENT_SPLIT_INFO: u32 = 0x1e;
/// load and re-export dylib
pub const LC_REEXPORT_DYLIB: u32 = (0x1f | LC_REQ_DYLD);
/// delay load of dylib until first use
pub const LC_LAZY_LOAD_DYLIB: u32 = 0x20;
/// encrypted segment information
pub const LC_ENCRYPTION_INFO: u32 = 0x21;
/// compressed dyld information
pub const LC_DYLD_INFO: u32 = 0x22;
/// compressed dyld information only
pub const LC_DYLD_INFO_ONLY: u32 = (0x22 | LC_REQ_DYLD);
/// load upward dylib
pub const LC_LOAD_UPWARD_DYLIB: u32 = (0x23 | LC_REQ_DYLD);
/// build for MacOSX min OS version
pub const LC_VERSION_MIN_MACOSX: u32 = 0x24;
/// build for iPhoneOS min OS version
pub const LC_VERSION_MIN_IPHONEOS: u32 = 0x25;
/// compressed table of function start addresses
pub const LC_FUNCTION_STARTS: u32 = 0x26;
/// string for dyld to treat like environment variable
pub const LC_DYLD_ENVIRONMENT: u32 = 0x27;
/// replacement for LC_UNIXTHREAD
pub const LC_MAIN: u32 = (0x28 | LC_REQ_DYLD);
/// table of non-instructions in __text
pub const LC_DATA_IN_CODE: u32 = 0x29;
/// source version used to build binary
pub const LC_SOURCE_VERSION: u32 = 0x2A;
/// Code signing DRs copied from linked dylibs
pub const LC_DYLIB_CODE_SIGN_DRS: u32 = 0x2B;
/// 64-bit encrypted segment information
pub const LC_ENCRYPTION_INFO_64: u32 = 0x2C;
/// linker options in MH_OBJECT files
pub const LC_LINKER_OPTION: u32 = 0x2D;
/// optimization hints in MH_OBJECT files
pub const LC_LINKER_OPTIMIZATION_HINT: u32 = 0x2E;
/// build for AppleTV min OS version
pub const LC_VERSION_MIN_TVOS: u32 = 0x2F;
/// build for Watch min OS version
pub const LC_VERSION_MIN_WATCHOS: u32 = 0x30;

// Constants for the flags field of the segment_command
bitflags! { pub flags SegmentFlags: u32 {
    /// the file contents for this segment is for the high part of the VM space,
    /// the low part is zero filled (for stacks in core files)
    const SG_HIGHVM = 0x1,
    /// this segment is the VM that is allocated by a fixed VM library,
    /// for overlap checking in the link editor
    const SG_FVMLIB = 0x2,
    /// this segment has nothing that was relocated in it and nothing relocated to it,
    /// that is it maybe safely replaced without relocation
    const SG_NORELOC = 0x4,
    /// This segment is protected.  If the segment starts at file offset 0,
    /// the first page of the segment is not protected.
    /// All other pages of the segment are protected.
    const SG_PROTECTED_VERSION_1 = 0x8,
} }

// The flags field of a section structure is separated into two parts a section
// type and section attributes.  The section types are mutually exclusive (it
// can only have one type) but the section attributes are not (it may have more
// than one attribute).
//
pub const SECTION_TYPE: u32 = 0x000000ff; /* 256 section types */
pub const SECTION_ATTRIBUTES: u32 = 0xffffff00; /*  24 section attributes */

// Constants for the type of a section

/// regular section
pub const S_REGULAR: u32 = 0x0;
/// zero fill on demand section
pub const S_ZEROFILL: u32 = 0x1;
/// section with only literal C strings
pub const S_CSTRING_LITERALS: u32 = 0x2;
/// section with only 4 byte literals
pub const S_4BYTE_LITERALS: u32 = 0x3;
/// section with only 8 byte literals
pub const S_8BYTE_LITERALS: u32 = 0x4;
/// section with only pointers to literals
pub const S_LITERAL_POINTERS: u32 = 0x5;


// For the two types of symbol pointers sections and the symbol stubs section
// they have indirect symbol table entries.  For each of the entries in the
// section the indirect symbol table entries, in corresponding order in the
// indirect symbol table, start at the index stored in the reserved1 field
// of the section structure.  Since the indirect symbol table entries
// correspond to the entries in the section the number of indirect symbol table
// entries is inferred from the size of the section divided by the size of the
// entries in the section.  For symbol pointers sections the size of the entries
// in the section is 4 bytes and for symbol stubs sections the byte size of the
// stubs is stored in the reserved2 field of the section structure.
//

/// section with only non-lazy symbol pointers
pub const S_NON_LAZY_SYMBOL_POINTERS: u32 = 0x6;
/// section with only lazy symbol pointers
pub const S_LAZY_SYMBOL_POINTERS: u32 = 0x7;
/// section with only symbol stubs, byte size of stub in the reserved2 field
pub const S_SYMBOL_STUBS: u32 = 0x8;
/// section with only function pointers for initialization
pub const S_MOD_INIT_FUNC_POINTERS: u32 = 0x9;
/// section with only function pointers for termination
pub const S_MOD_TERM_FUNC_POINTERS: u32 = 0xa;
/// section contains symbols that are to be coalesced
pub const S_COALESCED: u32 = 0xb;
/// zero fill on demand section that can be larger than 4 gigabytes)
pub const S_GB_ZEROFILL: u32 = 0xc;
/// section with only pairs of function pointers for interposing

pub const S_INTERPOSING: u32 = 0xd;
/// section with only 16 byte literals
pub const S_16BYTE_LITERALS: u32 = 0xe;
/// section contains DTrace Object Format
pub const S_DTRACE_DOF: u32 = 0xf;
/// section with only lazy symbol pointers to lazy loaded dylibs
pub const S_LAZY_DYLIB_SYMBOL_POINTERS: u32 = 0x10;

// Section types to support thread local variables
//

/// template of initial values for TLVs
pub const S_THREAD_LOCAL_REGULAR: u32 = 0x11;
/// template of initial values for TLVs
pub const S_THREAD_LOCAL_ZEROFILL: u32 = 0x12;
/// TLV descriptors
pub const S_THREAD_LOCAL_VARIABLES: u32 = 0x13;
/// pointers to TLV descriptors
pub const S_THREAD_LOCAL_VARIABLE_POINTERS: u32 = 0x14;
/// functions to call to initialize TLV values
pub const S_THREAD_LOCAL_INIT_FUNCTION_POINTERS: u32 = 0x15;

// Constants for the section attributes part of the flags field of a section
// structure.
//
bitflags! { pub flags SectionAttributes: u32 {
    /// User setable attributes
    const SECTION_ATTRIBUTES_USR = 0xff000000,
    /// section contains only true machine instructions
    const S_ATTR_PURE_INSTRUCTIONS = 0x80000000,
    /// section contains coalesced symbols that are not to be in a ranlib table of contents
    const S_ATTR_NO_TOC = 0x40000000,
    /// ok to strip static symbols in this section in files with the MH_DYLDLINK flag
    const S_ATTR_STRIP_STATIC_SYMS = 0x20000000,
    /// no dead stripping
    const S_ATTR_NO_DEAD_STRIP = 0x10000000,
    /// blocks are live if they reference live blocks
    const S_ATTR_LIVE_SUPPORT = 0x08000000,
    /// Used with i386 code stubs written on by dyld
    const S_ATTR_SELF_MODIFYING_CODE = 0x04000000,

    // If a segment contains any sections marked with S_ATTR_DEBUG then all
    // sections in that segment must have this attribute.  No section other than
    // a section marked with this attribute may reference the contents of this
    // section.  A section with this attribute may contain no symbols and must have
    // a section type S_REGULAR.  The static linker will not copy section contents
    // from sections with this attribute into its output file.  These sections
    // generally contain DWARF debugging info.
    //

    /// a debug section
    const S_ATTR_DEBUG = 0x02000000,
    /// system setable attributes
    const SECTION_ATTRIBUTES_SYS = 0x00ffff00,
    /// section contains some machine instructions
    const S_ATTR_SOME_INSTRUCTIONS = 0x00000400,
    /// section has external relocation entries
    const S_ATTR_EXT_RELOC = 0x00000200,
    /// section has local relocation entries
    const S_ATTR_LOC_RELOC = 0x00000100,
} }

// The names of segments and sections in them are mostly meaningless to the
// link-editor.  But there are few things to support traditional UNIX
// executables that require the link-editor and assembler to use some names
// agreed upon by convention.
//
// The initial protection of the "__TEXT" segment has write protection turned
// off (not writeable).
//
// The link-editor will allocate common symbols at the end of the "__common"
// section in the "__DATA" segment.  It will create the section and segment
// if needed.
//

// The currently known segment names and the section names in those segments

/// the pagezero segment which has no protections and catches NULL references for MH_EXECUTE files
pub static SEG_PAGEZERO: &'static str = "__PAGEZERO";

/// the tradition UNIX text segment
pub static SEG_TEXT: &'static str = "__TEXT";
/// the real text part of the text
pub static SECT_TEXT: &'static str = "__text";

// section no headers, and no padding
//

/// the fvmlib initialization section
pub static SECT_FVMLIB_INIT0: &'static str = "__fvmlib_init0";
/// the section following the fvmlib initialization section
pub static SECT_FVMLIB_INIT1: &'static str = "__fvmlib_init1";

/// the tradition UNIX data segment
pub static SEG_DATA: &'static str = "__DATA";
/// the real initialized data section no padding, no bss overlap
pub static SECT_DATA: &'static str = "__data";
/// the real uninitialized data section no padding
pub static SECT_BSS: &'static str = "__bss";
/// the section common symbols are allocated in by the link editor
pub static SECT_COMMON: &'static str = "__common";

/// objective-C runtime segment
pub static SEG_OBJC: &'static str = "__OBJC";
/// symbol table
pub static SECT_OBJC_SYMBOLS: &'static str = "__symbol_table";
/// module information
pub static SECT_OBJC_MODULES: &'static str = "__module_info";
/// string table
pub static SECT_OBJC_STRINGS: &'static str = "__selector_strs";
/// string table
pub static SECT_OBJC_REFS: &'static str = "__selector_refs";

/// the icon segment
pub static SEG_ICON: &'static str = "__ICON";
/// the icon headers
pub static SECT_ICON_HEADER: &'static str = "__header";
/// the icons in tiff format
pub static SECT_ICON_TIFF: &'static str = "__tiff";

/// the segment containing all structs created and maintained by the link editor.
/// Created with -seglinkedit option to ld(1) for MH_EXECUTE and FVMLIB file types only
pub static SEG_LINKEDIT: &'static str = "__LINKEDIT";

/// the unix stack segment
pub static SEG_UNIXSTACK: &'static str = "__UNIXSTACK";

/// the segment for the self (dyld) modifing code stubs that has read, write and execute permissions
pub static SEG_IMPORT: &'static str = "__IMPORT";


// The following are used to encode rebasing information
//

pub const REBASE_TYPE_POINTER: u32 = 1;
pub const REBASE_TYPE_TEXT_ABSOLUTE32: u32 = 2;
pub const REBASE_TYPE_TEXT_PCREL32: u32 = 3;

pub const REBASE_OPCODE_MASK: u32 = 0xF0;
pub const REBASE_IMMEDIATE_MASK: u32 = 0x0F;

pub const REBASE_OPCODE_DONE: u32 = 0x00;
pub const REBASE_OPCODE_SET_TYPE_IMM: u32 = 0x10;
pub const REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: u32 = 0x20;
pub const REBASE_OPCODE_ADD_ADDR_ULEB: u32 = 0x30;
pub const REBASE_OPCODE_ADD_ADDR_IMM_SCALED: u32 = 0x40;
pub const REBASE_OPCODE_DO_REBASE_IMM_TIMES: u32 = 0x50;
pub const REBASE_OPCODE_DO_REBASE_ULEB_TIMES: u32 = 0x60;
pub const REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB: u32 = 0x70;
pub const REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB: u32 = 0x80;


// The following are used to encode binding information
//
pub const BIND_TYPE_POINTER: u32 = 1;
pub const BIND_TYPE_TEXT_ABSOLUTE32: u32 = 2;
pub const BIND_TYPE_TEXT_PCREL32: u32 = 3;

pub const BIND_SPECIAL_DYLIB_SELF: u32 = 0;
pub const BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE: u32 = -1;
pub const BIND_SPECIAL_DYLIB_FLAT_LOOKUP: u32 = -2;

pub const BIND_SYMBOL_FLAGS_WEAK_IMPORT: u32 = 0x1;
pub const BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION: u32 = 0x8;

pub const BIND_OPCODE_MASK: u32 = 0xF0;
pub const BIND_IMMEDIATE_MASK: u32 = 0x0F;
pub const BIND_OPCODE_DONE: u32 = 0x00;
pub const BIND_OPCODE_SET_DYLIB_ORDINAL_IMM: u32 = 0x10;
pub const BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB: u32 = 0x20;
pub const BIND_OPCODE_SET_DYLIB_SPECIAL_IMM: u32 = 0x30;
pub const BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM: u32 = 0x40;
pub const BIND_OPCODE_SET_TYPE_IMM: u32 = 0x50;
pub const BIND_OPCODE_SET_ADDEND_SLEB: u32 = 0x60;
pub const BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: u32 = 0x70;
pub const BIND_OPCODE_ADD_ADDR_ULEB: u32 = 0x80;
pub const BIND_OPCODE_DO_BIND: u32 = 0x90;
pub const BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB: u32 = 0xA0;
pub const BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED: u32 = 0xB0;
pub const BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB: u32 = 0xC0;


// The following are used on the flags byte of a terminal node
// in the export information.
//

bitflags! { pub flags ExportSymbolFlags: u32 {
    const EXPORT_SYMBOL_FLAGS_KIND_MASK              = 0x03,
    const EXPORT_SYMBOL_FLAGS_KIND_REGULAR           = 0x00,
    const EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL      = 0x01,
    const EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION        = 0x04,
    const EXPORT_SYMBOL_FLAGS_REEXPORT               = 0x08,
    const EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER      = 0x10,
}}

pub const DICE_KIND_DATA: u16 = 0x0001;
pub const DICE_KIND_JUMP_TABLE8: u16 = 0x0002;
pub const DICE_KIND_JUMP_TABLE16: u16 = 0x0003;
pub const DICE_KIND_JUMP_TABLE32: u16 = 0x0004;
pub const DICE_KIND_ABS_JUMP_TABLE32: u16 = 0x0005;
