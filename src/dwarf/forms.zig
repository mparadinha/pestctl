const std = @import("std");
const c = @import("../c.zig");
const Attrib = @import("../Dwarf.zig").Attrib;
const DW = @import("constants.zig");

pub const ReadInfo = struct {
    address_size: u8,
    is_64: bool,
    version: u16,

    debug_info_opt: ?[]const u8 = null,
    debug_str_opt: ?[]const u8 = null,
    debug_line_str_opt: ?[]const u8 = null,
    sup_debug_str_opt: ?[]const u8 = null,

    // TODO: refactor this with the new StrOffsetTable (and the other tables)
    // see Dwarf v5 spec, chapter 7.26, pg.240
    str_offsets_table_opt: ?[]const u8 = null, // this is just the table part, no header or anything
    str_offsets_table_is_64_opt: ?bool = null,
};

const ReaderType = std.io.FixedBufferStream([]const u8).Reader;

pub fn readAddress(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {
    switch (form) {
        DW.FORM.addr => switch (read_info.address_size) {
            4 => return @intCast(usize, try reader.readIntLittle(u32)),
            8 => return @intCast(usize, try reader.readIntLittle(u32)),
            else => std.debug.panic("DW_FORM_addr with address_size={}\n", .{read_info.address_size}),
        },
        DW.FORM.addrx, DW.FORM.addrx1, DW.FORM.addrx2, DW.FORM.addrx3, DW.FORM.addrx4 => {
            // see Dwarf v5 spec, chapter 7.26, pg.240
            std.debug.panic("TODO: Dwarf v5: {s}\n", .{DW.FORM.asStr(form)});
        },
        else => std.debug.panic("{s} is not a valid address form\n", .{DW.FORM.asStr(form)}),
    }
}

// TODO: pub fn readAddressPtr(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}

pub fn readBlock(read_info: ReadInfo, reader: ReaderType, form: u16) []const u8 {
    _ = read_info;
    const block_len = switch (form) {
        DW.FORM.block => try std.len.readULEB128(usize, reader),
        DW.FORM.block1 => @intCast(usize, try reader.readByte()),
        DW.FORM.block2 => @intCast(usize, try reader.readIntLittle(u16)),
        DW.FORM.block4 => @intCast(usize, try reader.readIntLittle(u32)),
        else => std.debug.panic("{s} is not a valid block form\n", .{DW.FORM.asStr(form)}),
    };
    const block_start = try reader.context.getPos();
    try reader.skipBytes(block_len, .{});
    return reader.context.buffer[block_start .. block_start + block_len];
}

const Constant = union(enum) {
    Signed: i128,
    Unsigned: u128,
};

// this one needs the pair instead of just the form because of DW_FORM_implicit_const
pub fn readConstant(read_info: ReadInfo, reader: ReaderType, attrib: Attrib) !Constant {
    _ = read_info;
    const form = attrib.form;
    return switch (form) {
        DW.FORM.data1 => Constant{ .Unsigned = try reader.readByte() },
        DW.FORM.data2 => Constant{ .Unsigned = try reader.readIntLittle(u16) },
        DW.FORM.data4 => Constant{ .Unsigned = try reader.readIntLittle(u32) },
        DW.FORM.data8 => Constant{ .Unsigned = try reader.readIntLittle(u64) },
        DW.FORM.data16 => Constant{ .Unsigned = try reader.readIntLittle(u128) },
        DW.FORM.udata => Constant{ .Unsigned = try std.leb.readULEB128(u128, reader) },
        DW.FORM.sdata => Constant{ .Signed = try std.leb.readILEB128(i128, reader) },
        DW.FORM.implicit_const => Constant{ .Signed = attrib.implicit_value },
        else => std.debug.panic("{s} is not a valid constant form\n", .{DW.FORM.asStr(form)}),
    };
}

pub fn readExprLoc(read_info: ReadInfo, reader: ReaderType, form: u16) ![]const u8 {
    _ = read_info;
    if (form != DW.FORM.exprloc) std.debug.panic("{s} is not a valid exprloc form\n", .{DW.FORM.asStr(form)});
    const len = try std.leb.readULEB128(usize, reader);
    const loc_start = try reader.context.getPos();
    try reader.skipBytes(len, .{});
    return reader.context.buffer[loc_start .. loc_start + len];
}

pub fn readFlag(read_info: ReadInfo, reader: ReaderType, form: u16) !bool {
    _ = read_info;
    return switch (form) {
        DW.FORM.flag => (try reader.readByte()) != 0,
        DW.FORM.flag_present => true,
        else => std.debug.panic("{s} is not a valid flag form\n", .{DW.FORM.asStr(form)}),
    };
}

pub fn readLinePtr(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {
    if (form != DW.FORM.sec_offset) std.debug.panic("{s} is not a valid lineptr form\n", .{DW.FORM.asStr(form)});
    return try readIs64(reader, read_info.is_64);
}

// TODO: pub fn readLocList(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}
// TODO: pub fn readLocListPtr(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}
// TODO: pub fn readMacroPtr(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}
// TODO: pub fn readRangeList(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}
// TODO: pub fn readRangeListPtr(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}

// some of these offsets are relative to the beginning of the compilation unit
// others are relative to the start of .debug_line. one has to check the form
// on how to interpret whatever is returned from this function
pub fn readReference(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {
    return switch (form) {
        DW.FORM.ref1 => @intCast(usize, try reader.readByte()),
        DW.FORM.ref2 => @intCast(usize, try reader.readIntLittle(u16)),
        DW.FORM.ref4 => @intCast(usize, try reader.readIntLittle(u32)),
        DW.FORM.ref8 => @intCast(usize, try reader.readIntLittle(u64)),
        DW.FORM.ref_udata => try std.leb.readULEB128(usize, reader),
        DW.FORM.ref_addr => try readIs64(reader, read_info.is_64),
        DW.FORM.ref_sig8 => @intCast(usize, try reader.readIntLittle(u64)),
        DW.FORM.ref_sup4 => @intCast(usize, try reader.readIntLittle(u32)),
        DW.FORM.ref_sup8 => @intCast(usize, try reader.readIntLittle(u64)),
        else => std.debug.panic("{s} is not a valid reference form\n", .{DW.FORM.asStr(form)}),
    };
}

pub fn readString(read_info: ReadInfo, reader: ReaderType, form: u16) ![]const u8 {
    //std.debug.print("reading string from {s}\n", .{DW.FORM.asStr(form)});
    switch (form) {
        DW.FORM.string => {
            const str_start = try reader.context.getPos();
            try reader.skipUntilDelimiterOrEof(0);
            const str_end = try reader.context.getPos();
            return reader.context.buffer[str_start..str_end];
        },
        DW.FORM.strp, DW.FORM.strp_sup, DW.FORM.line_strp => {
            const str_start = try readIs64(reader, read_info.is_64);
            const str_section = (switch (form) {
                DW.FORM.strp => read_info.debug_str_opt,
                DW.FORM.strp_sup => read_info.sup_debug_str_opt,
                DW.FORM.line_strp => read_info.debug_line_str_opt,
                else => unreachable,
            }) orelse std.debug.panic("{s}\n", .{DW.FORM.asStr(form)});
            //std.debug.print("str_start = 0x{x}\n", .{str_start});
            //std.debug.print("str_section = {s}\n", .{str_section[0..100]});
            const str_len = c.strlen(&str_section[str_start]);
            return str_section[str_start .. str_start + str_len];
        },
        // see Dwarf v5 spec, chapter 7.26, pg.240
        DW.FORM.strx, DW.FORM.strx1, DW.FORM.strx2, DW.FORM.strx3, DW.FORM.strx4 => {
            // TODO: refactor with the new StrOffsetTable
            const offset_idx = if (form == DW.FORM.strx)
                try std.leb.readULEB128(usize, reader)
            else switch (form) {
                DW.FORM.strx1 => @intCast(usize, try reader.readByte()),
                DW.FORM.strx2 => @intCast(usize, try reader.readIntLittle(u16)),
                DW.FORM.strx3 => @intCast(usize, try reader.readIntLittle(u24)),
                DW.FORM.strx4 => @intCast(usize, try reader.readIntLittle(u32)),
                else => unreachable,
            };
            const offset_section = read_info.str_offsets_table_opt orelse
                std.debug.panic("{s}\n", .{DW.FORM.asStr(form)});
            const offset_is_64 = read_info.str_offsets_table_is_64_opt orelse
                std.debug.panic("{s}\n", .{DW.FORM.asStr(form)});
            const size: usize = if (offset_is_64) 8 else 4;
            const offset_bytes = offset_section[size * offset_idx .. size * (offset_idx + 1)];
            var offset_stream = std.io.fixedBufferStream(offset_bytes);
            var offset_reader = offset_stream.reader();

            const offset = try readIs64(offset_reader, offset_is_64);

            // the spec does not specify which string section these offsets point to
            // so I'm assuming it's the "main" one, .debug_str (see Dwarf v5 spec, pg.218)
            const str_section = read_info.debug_str_opt orelse
                std.debug.panic("{s}\n", .{DW.FORM.asStr(form)});

            const str_start = offset;
            const str_len = c.strlen(&str_section[str_start]);
            return str_section[str_start .. str_start + str_len];
        },
        else => std.debug.panic("{s} is not a valid string form\n", .{DW.FORM.asStr(form)}),
    }
}

// TODO: pub fn readStrOffsetsPtr(read_info: ReadInfo, reader: ReaderType, form: u16) !usize {}

/// "In the 32-bit DWARF format [...] 4-byte [...] 64-bit DWARF format [...] 8-byte [...]"
pub fn readIs64(reader: anytype, is_64: bool) !usize {
    return if (is_64)
        @intCast(usize, try reader.readIntLittle(u64))
    else
        @intCast(usize, try reader.readIntLittle(u32));
}

const StrOffsetTable = struct {};

// for strx FORM types (also works for the addrx ones)
pub fn readIndirectOffset(reader: anytype, table: StrOffsetTable) usize {
    _ = reader;
    _ = table;
}

// TODO: do readIndirect address and for the range list and location list tables as well
// they each need to get an argument to a *parsed* table (because the info the header
// is needed to actually get the entry at a given index and interpret it)

// TODO: update this whole thing to dwarf 5 to use the new tables instead of the old ones
// TODO: also pubname and pubtypes got merged into a new .debug_names

pub const SkipInfo = struct {
    address_size: u8,
    is_64: bool,
};

pub fn skip(skip_info: SkipInfo, reader: anytype, form: u16) !void {
    switch (form) {
        // address
        DW.FORM.addr => try reader.skipBytes(skip_info.address_size, .{}),
        DW.FORM.addrx => _ = try std.leb.readULEB128(u64, reader),
        DW.FORM.addrx1 => try reader.skipBytes(1, .{}),
        DW.FORM.addrx2 => try reader.skipBytes(2, .{}),
        DW.FORM.addrx3 => try reader.skipBytes(3, .{}),
        DW.FORM.addrx4 => try reader.skipBytes(4, .{}),
        // block
        DW.FORM.block => {
            const skip_len = try std.leb.readULEB128(usize, reader);
            try reader.skipBytes(skip_len, .{});
        },
        DW.FORM.block1 => {
            const skip_len = try reader.readByte();
            try reader.skipBytes(skip_len, .{});
        },
        DW.FORM.block2 => {
            const skip_len = try reader.readIntLittle(u16);
            try reader.skipBytes(skip_len, .{});
        },
        DW.FORM.block4 => {
            const skip_len = try reader.readIntLittle(u32);
            try reader.skipBytes(skip_len, .{});
        },
        // constant
        DW.FORM.data1 => try reader.skipBytes(1, .{}),
        DW.FORM.data2 => try reader.skipBytes(2, .{}),
        DW.FORM.data4 => try reader.skipBytes(4, .{}),
        DW.FORM.data8 => try reader.skipBytes(8, .{}),
        DW.FORM.data16 => try reader.skipBytes(16, .{}),
        DW.FORM.udata => _ = try std.leb.readULEB128(u64, reader),
        DW.FORM.sdata => _ = try std.leb.readILEB128(i64, reader),
        DW.FORM.implicit_const => {},
        // exprloc
        DW.FORM.exprloc => {
            const instr_bytes_len = try std.leb.readULEB128(usize, reader);
            try reader.skipBytes(instr_bytes_len, .{});
        },
        // flag
        DW.FORM.flag => try reader.skipBytes(1, .{}),
        DW.FORM.flag_present => {},
        // loclist
        DW.FORM.loclistx => _ = try std.leb.readULEB128(u64, reader),
        // rnglist
        DW.FORM.rnglistx => _ = try std.leb.readULEB128(u64, reader),
        // reference
        DW.FORM.ref_addr => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        DW.FORM.ref1 => try reader.skipBytes(1, .{}),
        DW.FORM.ref2 => try reader.skipBytes(2, .{}),
        DW.FORM.ref4 => try reader.skipBytes(4, .{}),
        DW.FORM.ref8 => try reader.skipBytes(8, .{}),
        DW.FORM.ref_udata => _ = try std.leb.readULEB128(u64, reader),
        DW.FORM.ref_sup4 => try reader.skipBytes(4, .{}),
        DW.FORM.ref_sup8 => try reader.skipBytes(8, .{}),
        DW.FORM.ref_sig8 => try reader.skipBytes(8, .{}),
        // string
        DW.FORM.string => try reader.skipUntilDelimiterOrEof(0),
        DW.FORM.strp => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        DW.FORM.strp_sup => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        DW.FORM.line_strp => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        DW.FORM.strx => _ = try std.leb.readULEB128(u64, reader),
        DW.FORM.strx1 => try reader.skipBytes(1, .{}),
        DW.FORM.strx2 => try reader.skipBytes(2, .{}),
        DW.FORM.strx3 => try reader.skipBytes(3, .{}),
        DW.FORM.strx4 => try reader.skipBytes(4, .{}),
        // addrptr, lineptr, loclistptr, macptr, rnglistptr, stroffsetsptr
        DW.FORM.sec_offset => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        // other
        DW.FORM.indirect => {},

        else => std.debug.panic("unknown FORM=0x{x}. can't skip.\n", .{form}),
    }
}
