#![allow(non_camel_case_types)]

pub const BIND_OPCODE_MASK: u8 = 0xF0;
pub const BIND_IMMEDIATE_MASK: u8 = 0x0F;

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    /// Push current record onto the "import stack" and clear record state back to zeros
    BIND_OPCODE_DONE = 0x00,
    /// Set the current record's dynamic library ordinal field
    /// (an index into the dynamic libraries referenced in the load commands,
    /// in the order they appeared) to the value contained in the immediate
    BIND_OPCODE_SET_DYLIB_ORDINAL_IMM = 0x10,
    /// Set the current record's dynamic library ordinal field to Uleb128 byte stream following the opcode
    BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB = 0x20,
    /// Set the current record's dynamic library ordinal field to 1 of 3 special values,
    /// either self (0x0), main executable (0xf, or signed -1), or flat lookup (0xe, or signed -2)
    BIND_OPCODE_SET_DYLIB_SPECIAL_IMM = 0x30,
    /// Set the current record's symbol name to the null-terminated string following the opcode,
    /// and the flags to the value contained in the immediate
    BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM = 0x40,
    /// Set the current record's symbol type (pointer = 1, relative = 2, or absolute = 3)
    /// to the value contained in the immediate
    BIND_OPCODE_SET_TYPE_IMM = 0x50,
    /// Set the current record's addend to the Sleb128 byte stream following the opcode
    BIND_OPCODE_SET_ADDEND_SLEB = 0x60,
    /// Set the current record's segment index to the value contained in the immediate,
    /// and its offset in that segment to the Uleb128 byte stream following the opcode
    BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB = 0x70,
    /// Set the current record's segment offset to its current offset
    /// plus the value of Uleb128 byte stream following the opcode
    BIND_OPCODE_ADD_ADDR_ULEB = 0x80,
    /// Push the current record onto the "import stack", and then increment the current record's address offset
    /// by the size of the platform pointer (32 or 64 bit)
    BIND_OPCODE_DO_BIND = 0x90,
    /// Push the current record onto the "import stack", and then set the current record's address offset
    /// to its current value plus the size of a platform pointer plus the Uleb128 byte stream that follows the opcode
    BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB = 0xA0,
    /// Push the current record onto the "import stack", and then set the current record's address offset to:
    /// seg_offset = seg_offset + (scale * sizeofptr) + sizeofptr
    /// where scale is the value contained in the immediate and sizeofptr is the size of the pointer on the platform
    BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED = 0xB0,
    /// Push the current record, and consult the dyld source, too complicated to write
    BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB = 0xC0,
}
