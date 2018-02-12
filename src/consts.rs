#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::collections::HashMap;

pub type cpu_type_t = i32;
pub type cpu_subtype_t = i32;
pub type vm_prot_t = i32;
pub type off_t = u32;

#[cfg(target_pointer_width = "16")]
pub const POINTER_WIDTH: usize = 16;
#[cfg(target_pointer_width = "32")]
pub const POINTER_WIDTH: usize = 32;
#[cfg(target_pointer_width = "64")]
pub const POINTER_WIDTH: usize = 64;
#[cfg(target_pointer_width = "128")]
pub const POINTER_WIDTH: usize = 128;

pub const POINTER_BYTES: usize = POINTER_WIDTH / 8;

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

//  Machine subtypes (these are defined here, instead of in a machine
//  dependent directory, so that any program can get all definitions
//  regardless of where is it compiled).
//

// Capability bits used in the definition of cpu_subtype.
//

/// mask for feature flags
pub const CPU_SUBTYPE_MASK: cpu_subtype_t = 0xff000000u64 as cpu_subtype_t;
/// 64 bit libraries
pub const CPU_SUBTYPE_LIB64: cpu_subtype_t = 0x80000000u64 as cpu_subtype_t;

pub fn get_cpu_subtype_type(subtype: cpu_subtype_t) -> u32 {
    (subtype as u32) & !(CPU_SUBTYPE_MASK as u32)
}

pub fn get_cpu_subtype_feature(subtype: cpu_subtype_t) -> u32 {
    ((subtype as u32) & (CPU_SUBTYPE_MASK as u32)) >> 24
}

//  Object files that are hand-crafted to run on any
//  implementation of an architecture are tagged with
//  CPU_SUBTYPE_MULTIPLE.  This functions essentially the same as
//  the "ALL" subtype of an architecture except that it allows us
//  to easily find object files that may need to be modified
//  whenever a new implementation of an architecture comes out.
//
//  It is the responsibility of the implementor to make sure the
//  software handles unsupported implementations elegantly.
//
pub const CPU_SUBTYPE_MULTIPLE: cpu_subtype_t = -1;
pub const CPU_SUBTYPE_LITTLE_ENDIAN: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_BIG_ENDIAN: cpu_subtype_t = 1;

//  VAX subtypes (these do *not* necessary conform to the actual cpu
//  ID assigned by DEC available via the SID register).
//

pub const CPU_SUBTYPE_VAX_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_VAX780: cpu_subtype_t = 1;
pub const CPU_SUBTYPE_VAX785: cpu_subtype_t = 2;
pub const CPU_SUBTYPE_VAX750: cpu_subtype_t = 3;
pub const CPU_SUBTYPE_VAX730: cpu_subtype_t = 4;
pub const CPU_SUBTYPE_UVAXI: cpu_subtype_t = 5;
pub const CPU_SUBTYPE_UVAXII: cpu_subtype_t = 6;
pub const CPU_SUBTYPE_VAX8200: cpu_subtype_t = 7;
pub const CPU_SUBTYPE_VAX8500: cpu_subtype_t = 8;
pub const CPU_SUBTYPE_VAX8600: cpu_subtype_t = 9;
pub const CPU_SUBTYPE_VAX8650: cpu_subtype_t = 10;
pub const CPU_SUBTYPE_VAX8800: cpu_subtype_t = 11;
pub const CPU_SUBTYPE_UVAXIII: cpu_subtype_t = 12;

//  680x0 subtypes
//
// The subtype definitions here are unusual for historical reasons.
// NeXT used to consider 68030 code as generic 68000 code.  For
// backwards compatability:
//
//  CPU_SUBTYPE_MC68030 symbol has been preserved for source code
//  compatability.
//
//  CPU_SUBTYPE_MC680x0_ALL has been defined to be the same
//  subtype as CPU_SUBTYPE_MC68030 for binary comatability.
//
//  CPU_SUBTYPE_MC68030_ONLY has been added to allow new object
//  files to be tagged as containing 68030-specific instructions.
//

pub const CPU_SUBTYPE_MC680X0_ALL: cpu_subtype_t = 1;
pub const CPU_SUBTYPE_MC68030: cpu_subtype_t = 1; /* compat */
pub const CPU_SUBTYPE_MC68040: cpu_subtype_t = 2;
pub const CPU_SUBTYPE_MC68030_ONLY: cpu_subtype_t = 3;

//  I386 subtypes
//

macro_rules! CPU_SUBTYPE_INTEL {
    ($f:expr, $m:expr) => ({
        ($f) + (($m) << 4)
    })
}

pub const CPU_SUBTYPE_I386_ALL: cpu_subtype_t = CPU_SUBTYPE_INTEL!(3, 0);
pub const CPU_SUBTYPE_386: cpu_subtype_t = CPU_SUBTYPE_INTEL!(3, 0);
pub const CPU_SUBTYPE_486: cpu_subtype_t = CPU_SUBTYPE_INTEL!(4, 0);
pub const CPU_SUBTYPE_486SX: cpu_subtype_t = CPU_SUBTYPE_INTEL!(4, 8); // 8 << 4 = 128
pub const CPU_SUBTYPE_586: cpu_subtype_t = CPU_SUBTYPE_INTEL!(5, 0);
pub const CPU_SUBTYPE_PENT: cpu_subtype_t = CPU_SUBTYPE_INTEL!(5, 0);
pub const CPU_SUBTYPE_PENTPRO: cpu_subtype_t = CPU_SUBTYPE_INTEL!(6, 1);
pub const CPU_SUBTYPE_PENTII_M3: cpu_subtype_t = CPU_SUBTYPE_INTEL!(6, 3);
pub const CPU_SUBTYPE_PENTII_M5: cpu_subtype_t = CPU_SUBTYPE_INTEL!(6, 5);
pub const CPU_SUBTYPE_CELERON: cpu_subtype_t = CPU_SUBTYPE_INTEL!(7, 6);
pub const CPU_SUBTYPE_CELERON_MOBILE: cpu_subtype_t = CPU_SUBTYPE_INTEL!(7, 7);
pub const CPU_SUBTYPE_PENTIUM_3: cpu_subtype_t = CPU_SUBTYPE_INTEL!(8, 0);
pub const CPU_SUBTYPE_PENTIUM_3_M: cpu_subtype_t = CPU_SUBTYPE_INTEL!(8, 1);
pub const CPU_SUBTYPE_PENTIUM_3_XEON: cpu_subtype_t = CPU_SUBTYPE_INTEL!(8, 2);
pub const CPU_SUBTYPE_PENTIUM_M: cpu_subtype_t = CPU_SUBTYPE_INTEL!(9, 0);
pub const CPU_SUBTYPE_PENTIUM_4: cpu_subtype_t = CPU_SUBTYPE_INTEL!(10, 0);
pub const CPU_SUBTYPE_PENTIUM_4_M: cpu_subtype_t = CPU_SUBTYPE_INTEL!(10, 1);
pub const CPU_SUBTYPE_ITANIUM: cpu_subtype_t = CPU_SUBTYPE_INTEL!(11, 0);
pub const CPU_SUBTYPE_ITANIUM_2: cpu_subtype_t = CPU_SUBTYPE_INTEL!(11, 1);
pub const CPU_SUBTYPE_XEON: cpu_subtype_t = CPU_SUBTYPE_INTEL!(12, 0);
pub const CPU_SUBTYPE_XEON_MP: cpu_subtype_t = CPU_SUBTYPE_INTEL!(12, 1);

pub const CPU_SUBTYPE_INTEL_FAMILY_MAX: cpu_subtype_t = 15;
pub const CPU_SUBTYPE_INTEL_MODEL_ALL: cpu_subtype_t = 0;

//  X86 subtypes.
//

pub const CPU_SUBTYPE_X86_ALL: cpu_subtype_t = 3;
pub const CPU_SUBTYPE_X86_64_ALL: cpu_subtype_t = 3;
pub const CPU_SUBTYPE_X86_ARCH1: cpu_subtype_t = 4;
pub const CPU_SUBTYPE_X86_64_H: cpu_subtype_t = 8; /* Haswell feature subset */

//  Mips subtypes.
//

pub const CPU_SUBTYPE_MIPS_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_MIPS_R2300: cpu_subtype_t = 1;
pub const CPU_SUBTYPE_MIPS_R2600: cpu_subtype_t = 2;
pub const CPU_SUBTYPE_MIPS_R2800: cpu_subtype_t = 3;
pub const CPU_SUBTYPE_MIPS_R2000A: cpu_subtype_t = 4; /* pmax */
pub const CPU_SUBTYPE_MIPS_R2000: cpu_subtype_t = 5;
pub const CPU_SUBTYPE_MIPS_R3000A: cpu_subtype_t = 6; /* 3max */
pub const CPU_SUBTYPE_MIPS_R3000: cpu_subtype_t = 7;

//  MC98000 (PowerPC; subtypes
//
pub const CPU_SUBTYPE_MC98000_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_MC98601: cpu_subtype_t = 1;

//  HPPA subtypes for Hewlett-Packard HP-PA family of
//  risc processors. Port by NeXT to 700 series.
//

pub const CPU_SUBTYPE_HPPA_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_HPPA_7100: cpu_subtype_t = 0; /* compat */
pub const CPU_SUBTYPE_HPPA_7100LC: cpu_subtype_t = 1;

//  MC88000 subtypes.
//
pub const CPU_SUBTYPE_MC88000_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_MC88100: cpu_subtype_t = 1;
pub const CPU_SUBTYPE_MC88110: cpu_subtype_t = 2;

//  SPARC subtypes
//
pub const CPU_SUBTYPE_SPARC_ALL: cpu_subtype_t = 0;

//  I860 subtypes
//
pub const CPU_SUBTYPE_I860_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_I860_860: cpu_subtype_t = 1;

//  PowerPC subtypes
//
pub const CPU_SUBTYPE_POWERPC_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_POWERPC_601: cpu_subtype_t = 1;
pub const CPU_SUBTYPE_POWERPC_602: cpu_subtype_t = 2;
pub const CPU_SUBTYPE_POWERPC_603: cpu_subtype_t = 3;
pub const CPU_SUBTYPE_POWERPC_603E: cpu_subtype_t = 4;
pub const CPU_SUBTYPE_POWERPC_603EV: cpu_subtype_t = 5;
pub const CPU_SUBTYPE_POWERPC_604: cpu_subtype_t = 6;
pub const CPU_SUBTYPE_POWERPC_604E: cpu_subtype_t = 7;
pub const CPU_SUBTYPE_POWERPC_620: cpu_subtype_t = 8;
pub const CPU_SUBTYPE_POWERPC_750: cpu_subtype_t = 9;
pub const CPU_SUBTYPE_POWERPC_7400: cpu_subtype_t = 10;
pub const CPU_SUBTYPE_POWERPC_7450: cpu_subtype_t = 11;
pub const CPU_SUBTYPE_POWERPC_970: cpu_subtype_t = 100;

//  ARM subtypes
//
pub const CPU_SUBTYPE_ARM_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_ARM_V4T: cpu_subtype_t = 5;
pub const CPU_SUBTYPE_ARM_V6: cpu_subtype_t = 6;
pub const CPU_SUBTYPE_ARM_V5TEJ: cpu_subtype_t = 7;
pub const CPU_SUBTYPE_ARM_XSCALE: cpu_subtype_t = 8;
pub const CPU_SUBTYPE_ARM_V7: cpu_subtype_t = 9;
pub const CPU_SUBTYPE_ARM_V7F: cpu_subtype_t = 10; /* Cortex A9 */
pub const CPU_SUBTYPE_ARM_V7S: cpu_subtype_t = 11; /* Swift */
pub const CPU_SUBTYPE_ARM_V7K: cpu_subtype_t = 12;
pub const CPU_SUBTYPE_ARM_V6M: cpu_subtype_t = 14; /* Not meant to be run under xnu */
pub const CPU_SUBTYPE_ARM_V7M: cpu_subtype_t = 15; /* Not meant to be run under xnu */
pub const CPU_SUBTYPE_ARM_V7EM: cpu_subtype_t = 16; /* Not meant to be run under xnu */

pub const CPU_SUBTYPE_ARM_V8: cpu_subtype_t = 13;

//  ARM64 subtypes
//
pub const CPU_SUBTYPE_ARM64_ALL: cpu_subtype_t = 0;
pub const CPU_SUBTYPE_ARM64_V8: cpu_subtype_t = 1;

fn get_arch_flags() -> &'static HashMap<&'static str, (cpu_type_t, cpu_subtype_t)> {
    lazy_static! {
        static ref ARCH_FLAGS : HashMap<&'static str, (cpu_type_t, cpu_subtype_t)> = {
            let mut m = HashMap::new();

            m.insert("any",    (CPU_TYPE_ANY,     CPU_SUBTYPE_MULTIPLE ));
            m.insert("little", (CPU_TYPE_ANY,     CPU_SUBTYPE_LITTLE_ENDIAN ));
            m.insert("big",    (CPU_TYPE_ANY,     CPU_SUBTYPE_BIG_ENDIAN ));

        /* 64-bit Mach-O architectures */

            /* architecture families */
            m.insert("ppc64",     (CPU_TYPE_POWERPC64, CPU_SUBTYPE_POWERPC_ALL ));
            m.insert("x86_64",    (CPU_TYPE_X86_64, CPU_SUBTYPE_X86_64_ALL ));
            m.insert("x86_64h",   (CPU_TYPE_X86_64, CPU_SUBTYPE_X86_64_H ));
            m.insert("arm64",     (CPU_TYPE_ARM64,     CPU_SUBTYPE_ARM64_ALL ));
            /* specific architecture implementations */
            m.insert("ppc970-64", (CPU_TYPE_POWERPC64, CPU_SUBTYPE_POWERPC_970 ));

        /* 32-bit Mach-O architectures */

            /* architecture families */
            m.insert("ppc",    (CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_ALL ));
            m.insert("i386",   (CPU_TYPE_I386,    CPU_SUBTYPE_I386_ALL ));
            m.insert("m68k",   (CPU_TYPE_MC680X0, CPU_SUBTYPE_MC680X0_ALL ));
            m.insert("hppa",   (CPU_TYPE_HPPA,    CPU_SUBTYPE_HPPA_ALL ));
            m.insert("sparc",  (CPU_TYPE_SPARC,   CPU_SUBTYPE_SPARC_ALL ));
            m.insert("m88k",   (CPU_TYPE_MC88000, CPU_SUBTYPE_MC88000_ALL ));
            m.insert("i860",   (CPU_TYPE_I860,    CPU_SUBTYPE_I860_ALL ));
            m.insert("arm",    (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_ALL ));
            /* specific architecture implementations */
            m.insert("ppc601", (CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_601 ));
            m.insert("ppc603",(CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_603 ));
            m.insert("ppc603e",(CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_603E ));
            m.insert("ppc603ev",(CPU_TYPE_POWERPC,CPU_SUBTYPE_POWERPC_603EV ));
            m.insert("ppc604", (CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_604 ));
            m.insert("ppc604e",(CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_604E ));
            m.insert("ppc750", (CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_750 ));
            m.insert("ppc7400",(CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_7400 ));
            m.insert("ppc7450",(CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_7450 ));
            m.insert("ppc970", (CPU_TYPE_POWERPC, CPU_SUBTYPE_POWERPC_970 ));
            m.insert("i486",   (CPU_TYPE_I386,    CPU_SUBTYPE_486 ));
            m.insert("i486SX", (CPU_TYPE_I386,    CPU_SUBTYPE_486SX ));
            m.insert("pentium",(CPU_TYPE_I386,    CPU_SUBTYPE_PENT )); /* same as i586 */
            m.insert("i586",   (CPU_TYPE_I386,    CPU_SUBTYPE_586 ));
            m.insert("pentpro",( CPU_TYPE_I386, CPU_SUBTYPE_PENTPRO )); /* same as i686 */
            m.insert("i686",   (CPU_TYPE_I386, CPU_SUBTYPE_PENTPRO ));
            m.insert("pentIIm3",(CPU_TYPE_I386, CPU_SUBTYPE_PENTII_M3 ));
            m.insert("pentIIm5",(CPU_TYPE_I386, CPU_SUBTYPE_PENTII_M5 ));
            m.insert("pentium4",(CPU_TYPE_I386, CPU_SUBTYPE_PENTIUM_4 ));
            m.insert("m68030", (CPU_TYPE_MC680X0, CPU_SUBTYPE_MC68030_ONLY ));
            m.insert("m68040", (CPU_TYPE_MC680X0, CPU_SUBTYPE_MC68040 ));
            m.insert("hppa7100LC",( CPU_TYPE_HPPA,  CPU_SUBTYPE_HPPA_7100LC ));
            m.insert("armv4t", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V4T));
            m.insert("armv5",  (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V5TEJ));
            m.insert("xscale", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_XSCALE));
            m.insert("armv6",  (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V6 ));
            m.insert("armv6m", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V6M ));
            m.insert("armv7",  (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V7 ));
            m.insert("armv7f", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V7F ));
            m.insert("armv7s", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V7S ));
            m.insert("armv7k", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V7K ));
            m.insert("armv7m", (CPU_TYPE_ARM,     CPU_SUBTYPE_ARM_V7M ));
            m.insert("armv7em",( CPU_TYPE_ARM,    CPU_SUBTYPE_ARM_V7EM ));
            m.insert("arm64v8",(CPU_TYPE_ARM64,   CPU_SUBTYPE_ARM64_V8 ));

            m
        };
    }

    &ARCH_FLAGS
}

pub fn get_arch_from_flag(name: &str) -> Option<&(cpu_type_t, cpu_subtype_t)> {
    get_arch_flags().get(&name)
}

pub fn get_arch_name_from_types(cputype: cpu_type_t, subtype: cpu_subtype_t) -> Option<&'static str> {
    for (name, &(cpu_type, cpu_subtype)) in get_arch_flags() {
        if cpu_type == cputype && get_cpu_subtype_type(cpu_subtype) == get_cpu_subtype_type(subtype) {
            return Some(name);
        }
    }

    None
}

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

pub const FAT_MAGIC: u32 = 0xcafebabe;
pub const FAT_CIGAM: u32 = 0xbebafeca; /* NXSwapLong(FAT_MAGIC) */

pub const ARMAG: &'static [u8] = b"!<arch>\n";

pub const AR_EFMT1: &'static str = "#1/"; /* extended format #1 */

pub const SYMDEF: &'static str = "__.SYMDEF";
pub const SYMDEF_SORTED: &'static str = "__.SYMDEF SORTED";

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
/// the object file is the output of an incremental link
/// against a base file and can't be link edited again
pub const MH_INCRLINK: u32 = 0x2;
// the object file is input for the dynamic linker and can't be staticly link edited again
pub const MH_DYLDLINK: u32 = 0x4;
// the object file's undefined references are bound by the dynamic linker when loaded.
pub const MH_BINDATLOAD: u32 = 0x8;
/// the file has its dynamic undefined references prebound.
pub const MH_PREBOUND: u32 = 0x10;
/// the file has its read-only and read-write segments split
pub const MH_SPLIT_SEGS: u32 = 0x20;
/// the shared library init routine is to be run lazily
/// via catching memory faults to its writeable segments (obsolete)
pub const MH_LAZY_INIT: u32 = 0x40;
/// the image is using two-level name space bindings
pub const MH_TWOLEVEL: u32 = 0x80;
/// the executable is forcing all images to use flat name space bindings
pub const MH_FORCE_FLAT: u32 = 0x100;
/// this umbrella guarantees no multiple defintions of symbols
/// in its sub-images so the two-level namespace hints can always be used.
pub const MH_NOMULTIDEFS: u32 = 0x200;
/// do not have dyld notify the prebinding agent about this executable
pub const MH_NOFIXPREBINDING: u32 = 0x400;
/// the binary is not prebound but can have its prebinding redone.
/// only used when MH_PREBOUND is not set.
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
/// When this bit is set, the binary declares it is safe
/// for use in processes with uid zero
pub const MH_ROOT_SAFE: u32 = 0x40000;
/// When this bit is set, the binary declares it is safe
/// for use in processes when issetugid() is true
pub const MH_SETUID_SAFE: u32 = 0x80000;
/// When this bit is set on a dylib, the static linker does not need to examine dependent dylibs
/// to see if any are re-exported
pub const MH_NO_REEXPORTED_DYLIBS: u32 = 0x100000;
/// When this bit is set, the OS will load the main executable at a random address.
/// Only used in MH_EXECUTE filetypes.
pub const MH_PIE: u32 = 0x200000;
/// Only for use on dylibs.  When linking against a dylib that has this bit set,
/// the static linker will automatically not create a LC_LOAD_DYLIB load command
/// to the dylib if no symbols are being referenced from the dylib.
pub const MH_DEAD_STRIPPABLE_DYLIB: u32 = 0x400000;
/// Contains a section of type S_THREAD_LOCAL_VARIABLES
pub const MH_HAS_TLV_DESCRIPTORS: u32 = 0x800000;
/// When this bit is set, the OS will run the main executable
/// with a non-executable heap even on platforms (e.g. i386)
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

bitflags! {
    /// Constants for the flags field of the segment_command
    pub struct SegmentFlags: u32 {
        /// the file contents for this segment is for the high part of the VM space,
        /// the low part is zero filled (for stacks in core files)
        const SG_HIGHVM = 0x1;
        /// this segment is the VM that is allocated by a fixed VM library,
        /// for overlap checking in the link editor
        const SG_FVMLIB = 0x2;
        /// this segment has nothing that was relocated in it and nothing relocated to it,
        /// that is it maybe safely replaced without relocation
        const SG_NORELOC = 0x4;
        /// This segment is protected.  If the segment starts at file offset 0,
        /// the first page of the segment is not protected.
        /// All other pages of the segment are protected.
        const SG_PROTECTED_VERSION_1 = 0x8;
    }
}

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

bitflags! {
    /// Constants for the section attributes part of the flags field of a section structure.
    pub struct SectionAttributes: u32 {
        /// User setable attributes
        const SECTION_ATTRIBUTES_USR = 0xff000000;
        /// section contains only true machine instructions
        const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
        /// section contains coalesced symbols that are not to be in a ranlib table of contents
        const S_ATTR_NO_TOC = 0x40000000;
        /// ok to strip static symbols in this section in files with the MH_DYLDLINK flag
        const S_ATTR_STRIP_STATIC_SYMS = 0x20000000;
        /// no dead stripping
        const S_ATTR_NO_DEAD_STRIP = 0x10000000;
        /// blocks are live if they reference live blocks
        const S_ATTR_LIVE_SUPPORT = 0x08000000;
        /// Used with i386 code stubs written on by dyld
        const S_ATTR_SELF_MODIFYING_CODE = 0x04000000;

        // If a segment contains any sections marked with S_ATTR_DEBUG then all
        // sections in that segment must have this attribute.  No section other than
        // a section marked with this attribute may reference the contents of this
        // section.  A section with this attribute may contain no symbols and must have
        // a section type S_REGULAR.  The static linker will not copy section contents
        // from sections with this attribute into its output file.  These sections
        // generally contain DWARF debugging info.
        //

        /// a debug section
        const S_ATTR_DEBUG = 0x02000000;
        /// system setable attributes
        const SECTION_ATTRIBUTES_SYS = 0x00ffff00;
        /// section contains some machine instructions
        const S_ATTR_SOME_INSTRUCTIONS = 0x00000400;
        /// section has external relocation entries
        const S_ATTR_EXT_RELOC = 0x00000200;
        /// section has local relocation entries
        const S_ATTR_LOC_RELOC = 0x00000100;
    }
}

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

// An indirect symbol table entry is simply a 32bit index into the symbol table
// to the symbol that the pointer or stub is refering to.  Unless it is for a
// non-lazy symbol pointer section for a defined symbol which strip(1) as
// removed.  In which case it has the value INDIRECT_SYMBOL_LOCAL.  If the
// symbol was also absolute INDIRECT_SYMBOL_ABS is or'ed with that.
//
pub const INDIRECT_SYMBOL_LOCAL: u32 = 0x80000000;
pub const INDIRECT_SYMBOL_ABS: u32 = 0x40000000;

// The following are used to encode rebasing information
//

pub const REBASE_TYPE_POINTER: u8 = 1;
pub const REBASE_TYPE_TEXT_ABSOLUTE32: u8 = 2;
pub const REBASE_TYPE_TEXT_PCREL32: u8 = 3;

pub const REBASE_OPCODE_MASK: u8 = 0xF0;
pub const REBASE_IMMEDIATE_MASK: u8 = 0x0F;

pub const REBASE_OPCODE_DONE: u8 = 0x00;
pub const REBASE_OPCODE_SET_TYPE_IMM: u8 = 0x10;
pub const REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: u8 = 0x20;
pub const REBASE_OPCODE_ADD_ADDR_ULEB: u8 = 0x30;
pub const REBASE_OPCODE_ADD_ADDR_IMM_SCALED: u8 = 0x40;
pub const REBASE_OPCODE_DO_REBASE_IMM_TIMES: u8 = 0x50;
pub const REBASE_OPCODE_DO_REBASE_ULEB_TIMES: u8 = 0x60;
pub const REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB: u8 = 0x70;
pub const REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB: u8 = 0x80;

// The following are used to encode binding information
//
pub const BIND_TYPE_POINTER: u8 = 1;
pub const BIND_TYPE_TEXT_ABSOLUTE32: u8 = 2;
pub const BIND_TYPE_TEXT_PCREL32: u8 = 3;

pub const BIND_SPECIAL_DYLIB_SELF: isize = 0;
pub const BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE: isize = -1;
pub const BIND_SPECIAL_DYLIB_FLAT_LOOKUP: isize = -2;

pub const BIND_SYMBOL_FLAGS_WEAK_IMPORT: u8 = 0x1;
pub const BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION: u8 = 0x8;

pub const BIND_OPCODE_MASK: u8 = 0xF0;
pub const BIND_IMMEDIATE_MASK: u8 = 0x0F;
pub const BIND_OPCODE_DONE: u8 = 0x00;
pub const BIND_OPCODE_SET_DYLIB_ORDINAL_IMM: u8 = 0x10;
pub const BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB: u8 = 0x20;
pub const BIND_OPCODE_SET_DYLIB_SPECIAL_IMM: u8 = 0x30;
pub const BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM: u8 = 0x40;
pub const BIND_OPCODE_SET_TYPE_IMM: u8 = 0x50;
pub const BIND_OPCODE_SET_ADDEND_SLEB: u8 = 0x60;
pub const BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: u8 = 0x70;
pub const BIND_OPCODE_ADD_ADDR_ULEB: u8 = 0x80;
pub const BIND_OPCODE_DO_BIND: u8 = 0x90;
pub const BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB: u8 = 0xA0;
pub const BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED: u8 = 0xB0;
pub const BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB: u8 = 0xC0;

bitflags! {
    /// The following are used on the flags byte of a terminal node in the export information.
    pub struct ExportSymbolFlags: u32 {
        const EXPORT_SYMBOL_FLAGS_KIND_MASK              = 0x03;
        const EXPORT_SYMBOL_FLAGS_KIND_REGULAR           = 0x00;
        const EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL      = 0x01;
        const EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION        = 0x04;
        const EXPORT_SYMBOL_FLAGS_REEXPORT               = 0x08;
        const EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER      = 0x10;
    }
}

pub const DICE_KIND_DATA: u16 = 0x0001;
pub const DICE_KIND_JUMP_TABLE8: u16 = 0x0002;
pub const DICE_KIND_JUMP_TABLE16: u16 = 0x0003;
pub const DICE_KIND_JUMP_TABLE32: u16 = 0x0004;
pub const DICE_KIND_ABS_JUMP_TABLE32: u16 = 0x0005;

/// global symbol: name,,NO_SECT,type,0
pub const N_GSYM: u8 = 0x20;
/// procedure name (f77 kludge): name,,NO_SECT,0,0
pub const N_FNAME: u8 = 0x22;
/// procedure: name,,n_sect,linenumber,address
pub const N_FUN: u8 = 0x24;
/// static symbol: name,,n_sect,type,address
pub const N_STSYM: u8 = 0x26;
/// .lcomm symbol: name,,n_sect,type,address
pub const N_LCSYM: u8 = 0x28;
/// begin nsect sym: 0,,n_sect,0,address
pub const N_BNSYM: u8 = 0x2e;
/// AST file path: name,,NO_SECT,0,0
pub const N_AST: u8 = 0x32;
/// emitted with gcc2_compiled and in gcc source
pub const N_OPT: u8 = 0x3c;
/// register sym: name,,NO_SECT,type,register
pub const N_RSYM: u8 = 0x40;
/// src line: 0,,n_sect,linenumber,address
pub const N_SLINE: u8 = 0x44;
/// end nsect sym: 0,,n_sect,0,address
pub const N_ENSYM: u8 = 0x4e;
/// structure elt: name,,NO_SECT,type,struct_offset
pub const N_SSYM: u8 = 0x60;
/// source file name: name,,n_sect,0,address
pub const N_SO: u8 = 0x64;
/// object file name: name,,0,0,st_mtime
pub const N_OSO: u8 = 0x66;
/// local sym: name,,NO_SECT,type,offset
pub const N_LSYM: u8 = 0x80;
/// include file beginning: name,,NO_SECT,0,sum
pub const N_BINCL: u8 = 0x82;
/// #included file name: name,,n_sect,0,address
pub const N_SOL: u8 = 0x84;
/// compiler parameters: name,,NO_SECT,0,0
pub const N_PARAMS: u8 = 0x86;
/// compiler version: name,,NO_SECT,0,0
pub const N_VERSION: u8 = 0x88;
/// compiler -O level: name,,NO_SECT,0,0
pub const N_OLEVEL: u8 = 0x8A;
/// parameter: name,,NO_SECT,type,offset
pub const N_PSYM: u8 = 0xa0;
/// include file end: name,,NO_SECT,0,0
pub const N_EINCL: u8 = 0xa2;
/// alternate entry: name,,n_sect,linenumber,address
pub const N_ENTRY: u8 = 0xa4;
/// left bracket: 0,,NO_SECT,nesting level,address
pub const N_LBRAC: u8 = 0xc0;
/// deleted include file: name,,NO_SECT,0,sum
pub const N_EXCL: u8 = 0xc2;
/// right bracket: 0,,NO_SECT,nesting level,address
pub const N_RBRAC: u8 = 0xe0;
/// begin common: name,,NO_SECT,0,0
pub const N_BCOMM: u8 = 0xe2;
/// end common: name,,n_sect,0,0
pub const N_ECOMM: u8 = 0xe4;
/// end common (local name): 0,,n_sect,0,address
pub const N_ECOML: u8 = 0xe8;
/// second stab entry with length information
pub const N_LENG: u8 = 0xfe;
/// global pascal symbol: name,,NO_SECT,subtype,line
pub const N_PC: u8 = 0x30;

/// To support the lazy binding of undefined symbols in the dynamic link-editor,
/// the undefined symbols in the symbol table (the nlist structures) are marked
/// with the indication if the undefined reference is a lazy reference or
/// non-lazy reference.  If both a non-lazy reference and a lazy reference is
/// made to the same symbol the non-lazy reference takes precedence.  A reference
/// is lazy only when all references to that symbol are made through a symbol
/// pointer in a lazy symbol pointer section.
///
/// The implementation of marking nlist structures in the symbol table for
/// undefined symbols will be to use some of the bits of the n_desc field as a
/// reference type.  The mask REFERENCE_TYPE will be applied to the n_desc field
/// of an nlist structure for an undefined symbol to determine the type of
/// undefined reference (lazy or non-lazy).
///
/// The constants for the REFERENCE FLAGS are propagated to the reference table
/// in a shared library file.  In that case the constant for a defined symbol,
/// REFERENCE_FLAG_DEFINED, is also used.
///
/// Reference type bits of the n_desc field of undefined symbols
pub const REFERENCE_TYPE: u8 = 0x7;
// types of references
pub const REFERENCE_FLAG_UNDEFINED_NON_LAZY: u8 = 0;
pub const REFERENCE_FLAG_UNDEFINED_LAZY: u8 = 1;
pub const REFERENCE_FLAG_DEFINED: u8 = 2;
pub const REFERENCE_FLAG_PRIVATE_DEFINED: u8 = 3;
pub const REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY: u8 = 4;
pub const REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY: u8 = 5;

/// To simplify stripping of objects that use are used with the dynamic link
/// editor, the static link editor marks the symbols defined an object that are
/// referenced by a dynamicly bound object (dynamic shared libraries, bundles).
/// With this marking strip knows not to strip these symbols.
///
pub const REFERENCED_DYNAMICALLY: u16 = 0x0010;

// For images created by the static link editor with the -twolevel_namespace
// option in effect the flags field of the mach header is marked with
// MH_TWOLEVEL.  And the binding of the undefined references of the image are
// determined by the static link editor.  Which library an undefined symbol is
// bound to is recorded by the static linker in the high 8 bits of the n_desc
// field using the SET_LIBRARY_ORDINAL macro below.  The ordinal recorded
// references the libraries listed in the Mach-O's LC_LOAD_DYLIB,
// LC_LOAD_WEAK_DYLIB, LC_REEXPORT_DYLIB, LC_LOAD_UPWARD_DYLIB, and
// LC_LAZY_LOAD_DYLIB, etc. load commands in the order they appear in the
// headers.   The library ordinals start from 1.
// For a dynamic library that is built as a two-level namespace image the
// undefined references from module defined in another use the same nlist struct
// an in that case SELF_LIBRARY_ORDINAL is used as the library ordinal.  For
// defined symbols in all images they also must have the library ordinal set to
// SELF_LIBRARY_ORDINAL.  The EXECUTABLE_ORDINAL refers to the executable
// image for references from plugins that refer to the executable that loads
// them.
//
// The DYNAMIC_LOOKUP_ORDINAL is for undefined symbols in a two-level namespace
// image that are looked up by the dynamic linker with flat namespace semantics.
// This ordinal was added as a feature in Mac OS X 10.3 by reducing the
// value of MAX_LIBRARY_ORDINAL by one.  So it is legal for existing binaries
// or binaries built with older tools to have 0xfe (254) dynamic libraries.  In
// this case the ordinal value 0xfe (254) must be treated as a library ordinal
// for compatibility.
//
pub const SELF_LIBRARY_ORDINAL: u8 = 0x0;
pub const MAX_LIBRARY_ORDINAL: u8 = 0xfd;
pub const DYNAMIC_LOOKUP_ORDINAL: u8 = 0xfe;
pub const EXECUTABLE_ORDINAL: u8 = 0xff;

// The bit 0x0020 of the n_desc field is used for two non-overlapping purposes
// and has two different symbolic names, N_NO_DEAD_STRIP and N_DESC_DISCARDED.
//

/// The N_NO_DEAD_STRIP bit of the n_desc field only ever appears in a
/// relocatable .o file (MH_OBJECT filetype). And is used to indicate to the
/// static link editor it is never to dead strip the symbol.
///
pub const N_NO_DEAD_STRIP: u16 = 0x0020; /* symbol is not to be dead stripped */

/// The N_DESC_DISCARDED bit of the n_desc field never appears in linked image.
/// But is used in very rare cases by the dynamic link editor to mark an in
/// memory symbol as discared and longer used for linking.
///
pub const N_DESC_DISCARDED: u16 = 0x0020; /* symbol is discarded */

/// The N_WEAK_REF bit of the n_desc field indicates to the dynamic linker that
/// the undefined symbol is allowed to be missing and is to have the address of
/// zero when missing.
///
pub const N_WEAK_REF: u16 = 0x0040; /* symbol is weak referenced */

/// The N_WEAK_DEF bit of the n_desc field indicates to the static and dynamic
/// linkers that the symbol definition is weak, allowing a non-weak symbol to
/// also be used which causes the weak definition to be discared.  Currently this
/// is only supported for symbols in coalesed sections.
///
pub const N_WEAK_DEF: u16 = 0x0080; /* coalesed symbol is a weak definition */

/// The N_REF_TO_WEAK bit of the n_desc field indicates to the dynamic linker
/// that the undefined symbol should be resolved using flat namespace searching.
///
pub const N_REF_TO_WEAK: u16 = 0x0080; /* reference to a weak symbol */

/// The N_ARM_THUMB_DEF bit of the n_desc field indicates that the symbol is
/// a defintion of a Thumb function.
///
pub const N_ARM_THUMB_DEF: u16 = 0x0008; /* symbol is a Thumb function (ARM) */

/// The N_SYMBOL_RESOLVER bit of the n_desc field indicates that the
/// that the function is actually a resolver function and should
/// be called to get the address of the real function to use.
/// This bit is only available in .o files (MH_OBJECT filetype)
///
pub const N_SYMBOL_RESOLVER: u16 = 0x0100;

/// The N_ALT_ENTRY bit of the n_desc field indicates that the
/// symbol is pinned to the previous content.
///
pub const N_ALT_ENTRY: u16 = 0x0200;
