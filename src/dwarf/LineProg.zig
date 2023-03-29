const std = @import("std");
const Allocator = std.mem.Allocator;
const DW = @import("constants.zig");
const readLengthField = @import("../Dwarf.zig").readLengthField;
const stringFromTable = @import("../Elf.zig").stringFromTable;

const LineProg = @This();

raw_data: []const u8, // not owned by LineProg

allocator: Allocator,

version: u16,
address_size: u8,
segment_selector_size: u8,
min_instr_len: u8,
max_ops_per_instr: u8,
default_is_stmt: bool,
line_base: i8,
line_range: u8,
opcode_base: u8,
standard_opcode_lens: []u8,
include_dirs: [][]const u8,
files: []FileInfo,
ops: []const u8,

// some things filled during init to help with searching without having to actually run
// every single line program in the debug information
address_range: [2]u64,
file_line_range: [2]u32,

pub const FileInfo = struct {
    name: []const u8,
    dir: usize,
    mod_time: u64,
    len: usize,
};

pub const State = struct {
    address: u64 = 0,
    op_index: u32 = 0,
    file: u32 = 1,
    line: u32 = 1,
    column: u32 = 0,
    is_stmt: bool,
    basic_block: bool = false,
    end_sequence: bool = false,
    prologue_end: bool = false,
    epilogue_begin: bool = false,
    isa: u32 = 0,
    discriminator: u32 = 0,
};

/// this returns the amount of bytes we need to skip (starting from `offset`) to reach the
/// end of the .debug_line "section" that starts at `offset`
pub fn getSectionSize(debug_line: []const u8, offset: usize) !usize {
    var stream = std.io.fixedBufferStream(debug_line[offset..]);
    var reader = stream.reader();
    const length = try readLengthField(reader);
    return if (length.is_64) 4 + 8 + length.length else 4 + length.length;
}

/// `debug_line` and `debug_line_str` must remain valid memory while this LineProg is used
pub fn init(allocator: Allocator, debug_line: []const u8, offset: usize, debug_line_str: []const u8, comp_dir: []const u8) !LineProg {
    var self = @as(LineProg, undefined);
    self.allocator = allocator;

    var stream = std.io.fixedBufferStream(debug_line[offset..]);
    var reader = stream.reader();

    const length = try readLengthField(reader);
    const unit_len = length.length;
    const is_64 = length.is_64;
    const section_size = (try stream.getPos()) + unit_len;
    self.raw_data = debug_line[offset .. offset + section_size];

    self.version = try reader.readIntLittle(u16);
    self.address_size = if (self.version >= 5) try reader.readByte() else 8;
    self.segment_selector_size = if (self.version >= 5) try reader.readByte() else @as(u8, 0);
    if (is_64) {
        _ = try reader.readIntLittle(u64);
    } else _ = try reader.readIntLittle(u32);
    self.min_instr_len = try reader.readByte();
    self.max_ops_per_instr = try reader.readByte();
    self.default_is_stmt = (try reader.readByte()) != 0;
    self.line_base = try reader.readByteSigned();
    self.line_range = try reader.readByte();
    self.opcode_base = try reader.readByte();
    self.standard_opcode_lens = try self.allocator.alloc(u8, self.opcode_base - 1);
    std.debug.assert((try reader.read(self.standard_opcode_lens)) == self.opcode_base - 1);

    // the directory/file name entry formats specifiy the columns of a table. see:
    // Dwarf v5 spec, chapter D.5.1 pg.321 (fig.D.33) for a good example

    // from Dwarf v5 spec, chapter 6.2.4, pg.157:
    // Prior to DWARF Version 5, the current directory was not represented in the
    // directories field and a directory index of 0 implicitly referred to that directory as found
    // in the DW_AT_comp_dir attribute of the compilation unit debugging information
    // entry. In DWARF Version 5, the current directory is explicitly present in the
    // directories field. This is needed to support the common practice of stripping all but
    // the line number sections (.debug_line and .debug_line_str) from an executable.
    // Note that if a .debug_line_str section is present, both the compilation unit
    // debugging information entry and the line number header can share a single copy of
    // the current directory name string.
    // TODO: some things from v4 and v5 are the same we don't need this big if case for each
    if (self.version == 4) {
        var include_dirs = std.ArrayList([]const u8).init(allocator);
        try include_dirs.append(comp_dir);

        var byte = try reader.readByte();
        while (byte != 0) : (byte = try reader.readByte()) {
            const str_start = (try stream.getPos()) - 1;
            while (byte != 0) : (byte = try reader.readByte()) {}
            const str_end = (try stream.getPos()) - 1;
            try include_dirs.append(self.raw_data[str_start..str_end]);
        }
        self.include_dirs = try include_dirs.toOwnedSlice();

        // TODO
        // from Dwarf v4 spec, chapter 6.2.4, pg.115:
        // A compiler may generate a single null byte for the file names field and define file names
        // using the extended opcode DW_LNE_define_file

        var files = std.ArrayList(FileInfo).init(allocator);
        try files.append(@as(FileInfo, undefined)); // TODO: set this to the correct value
        byte = try reader.readByte();
        while (byte != 0) : (byte = try reader.readByte()) {
            const str_start = (try stream.getPos()) - 1;
            while (byte != 0) : (byte = try reader.readByte()) {}
            const str_end = (try stream.getPos()) - 1;
            try files.append(.{
                .name = self.raw_data[str_start..str_end],
                .dir = try std.leb.readULEB128(usize, reader),
                .mod_time = try std.leb.readULEB128(usize, reader),
                .len = try std.leb.readULEB128(usize, reader),
            });
        }
        self.files = try files.toOwnedSlice();
    } else if (self.version == 5) {
        const EntryFormat = struct { content_type: u16, form: u16 };

        const dir_entry_format_count = try reader.readByte();
        var dir_entry_formats = try self.allocator.alloc(EntryFormat, dir_entry_format_count);
        defer self.allocator.free(dir_entry_formats);
        for (dir_entry_formats) |*format| {
            format.content_type = try std.leb.readULEB128(u16, reader);
            format.form = try std.leb.readULEB128(u16, reader);
        }

        const dirs_count = try reader.readByte();
        self.include_dirs = try self.allocator.alloc([]const u8, dirs_count);
        for (self.include_dirs, 0..) |*dir, i| {
            for (dir_entry_formats) |format| {
                // TODO: use the readXXX for these forms
                if (format.content_type == DW.LNCT.path and format.form == DW.FORM.line_strp) {
                    const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
                    dir.* = stringFromTable(debug_line_str, strp);
                    break;
                }
            } else std.debug.panic("couldn't parse include dir {} (debug_line offfset = 0x{x})", .{
                i, offset,
            });
        }

        const file_entry_format_count = try reader.readByte();
        var file_entry_formats = try self.allocator.alloc(EntryFormat, file_entry_format_count);
        defer self.allocator.free(file_entry_formats);
        for (file_entry_formats) |*format| {
            format.content_type = try std.leb.readULEB128(u16, reader);
            format.form = try std.leb.readULEB128(u16, reader);
        }

        const file_count = try std.leb.readULEB128(usize, reader);
        self.files = try self.allocator.alloc(FileInfo, file_count);
        for (self.files) |*file| {
            file.dir = 0;
            file.mod_time = 0;
            file.len = 0;
            var found_path = false;
            for (file_entry_formats) |format| {
                if (format.content_type == DW.LNCT.path) {
                    file.name = switch (format.form) {
                        // TODO: use the readXXX for these forms
                        DW.FORM.string => blk: {
                            const str_start = try stream.getPos();
                            const strlen = stringFromTable(self.raw_data, str_start).len;
                            try reader.skipBytes(strlen + 1, .{});
                            break :blk self.raw_data[str_start .. str_start + strlen];
                        },
                        DW.FORM.line_strp => blk: {
                            const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
                            break :blk stringFromTable(debug_line_str, strp);
                        },
                        DW.FORM.strp => std.debug.panic("need to pass .debug_str in\n", .{}),
                        DW.FORM.strp_sup => unreachable, // ah hell no
                        else => std.debug.panic("invalid FORM 0x{x} for file name\n", .{format.form}),
                    };
                    found_path = true;
                } else if (format.content_type == DW.LNCT.directory_index) {
                    file.dir = switch (format.form) {
                        DW.FORM.data1 => @intCast(usize, try reader.readByte()),
                        DW.FORM.data2 => @intCast(usize, try reader.readIntLittle(u16)),
                        DW.FORM.udata => try std.leb.readULEB128(usize, reader),
                        else => std.debug.panic("invalid FORM 0x{x} for directory index\n", .{format.form}),
                    };
                } else if (format.content_type == DW.LNCT.timestamp) {
                    file.mod_time = switch (format.form) {
                        DW.FORM.udata => try std.leb.readULEB128(u64, reader),
                        DW.FORM.data4 => @intCast(u64, try reader.readIntLittle(u32)),
                        DW.FORM.data8 => try reader.readIntLittle(u64),
                        DW.FORM.block => std.debug.panic("TODO: implement block for timestamp\n", .{}),
                        else => std.debug.panic("invalid FORM 0x{x} for timestamp\n", .{format.form}),
                    };
                } else if (format.content_type == DW.LNCT.size) {
                    file.len = switch (format.form) {
                        DW.FORM.udata => try std.leb.readULEB128(u64, reader),
                        DW.FORM.data1 => @intCast(u64, try reader.readByte()),
                        DW.FORM.data2 => @intCast(u64, try reader.readIntLittle(u16)),
                        DW.FORM.data4 => @intCast(u64, try reader.readIntLittle(u32)),
                        DW.FORM.data8 => try reader.readIntLittle(u64),
                        else => std.debug.panic("invalid FORM 0x{x} for file len\n", .{format.form}),
                    };
                } else {
                    switch (format.form) {
                        DW.FORM.data1 => try reader.skipBytes(1, .{}),
                        DW.FORM.data2 => try reader.skipBytes(2, .{}),
                        DW.FORM.data4 => try reader.skipBytes(4, .{}),
                        DW.FORM.data8 => try reader.skipBytes(8, .{}),
                        DW.FORM.data16 => try reader.skipBytes(16, .{}),
                        DW.FORM.udata => _ = try std.leb.readULEB128(usize, reader),
                        DW.FORM.block => {
                            const block_len = try std.leb.readULEB128(usize, reader);
                            try reader.skipBytes(block_len, .{});
                        },
                        else => std.debug.panic("couldn't parse FORM=0x{x} to skip (debug_line offset = 0x{x})\n", .{ format.form, offset }),
                    }
                }
            }
            std.debug.assert(found_path);
        }
    }

    if ((try stream.getPos()) < self.raw_data.len) {
        self.ops = self.raw_data[try stream.getPos()..];
    } else {
        self.ops.len = 0;
        return self;
    }

    // run the whole program once to find the range of addresses/lines
    // this helps later with searching through multiple LineProg's
    self.address_range = .{ std.math.maxInt(u64), 0 };
    self.file_line_range = .{ std.math.maxInt(u32), 0 };
    var state = self.initialState();
    var op_stream = std.io.fixedBufferStream(self.ops);
    var op_reader = op_stream.reader();
    while ((try op_stream.getPos()) < self.ops.len) {
        const new_row = try self.updateState(&state, op_reader);
        if (new_row) |row| {
            self.address_range[0] = std.math.min(self.address_range[0], row.address);
            self.address_range[1] = std.math.max(self.address_range[1], row.address);
            self.file_line_range[0] = std.math.min(self.file_line_range[0], row.line);
            self.file_line_range[1] = std.math.max(self.file_line_range[1], row.line);
            if (row.end_sequence) break;
        }
    }

    return self;
}

pub fn deinit(self: LineProg) void {
    self.allocator.free(self.standard_opcode_lens);
    self.allocator.free(self.include_dirs);
    self.allocator.free(self.files);
}

pub fn initialState(self: LineProg) State {
    return .{ .is_stmt = self.default_is_stmt };
}

pub fn findAddrForSrc(self: LineProg, file: u32, line: u32) !?State {
    var state = self.initialState();
    var stream = std.io.fixedBufferStream(self.ops);
    var reader = stream.reader();
    while ((try stream.getPos()) < self.ops.len) {
        const new_row = try self.updateState(&state, reader);
        if (new_row) |row| {
            if (row.file == file and row.line == line) return row;
            if (row.end_sequence) break;
        }
    }
    return null;
}

pub fn findAddrRangeForSrc(self: LineProg, file: u32, line: u32) !?[2]u64 {
    var start_addr = @as(?u64, null);
    var state = self.initialState();
    var stream = std.io.fixedBufferStream(self.ops);
    var reader = stream.reader();
    while ((try stream.getPos()) < self.ops.len) {
        const new_row = try self.updateState(&state, reader);
        if (new_row) |row| {
            const is_src = row.file == file and row.line == line;
            if (start_addr == null and is_src and row.is_stmt) start_addr = row.address;
            if (start_addr != null and !is_src) return [2]u64{ start_addr.?, row.address };
            if (row.end_sequence) break;
        }
    }
    return null;
}

pub fn findAddr(self: LineProg, addr: usize, check_stmt: bool) !?State {
    var state = self.initialState();
    var stream = std.io.fixedBufferStream(self.ops);
    var reader = stream.reader();
    while ((try stream.getPos()) < self.ops.len) {
        const new_row = try self.updateState(&state, reader);
        if (new_row) |row| {
            if (check_stmt and !row.is_stmt) continue;
            if (row.address == addr) return row;
            if (row.end_sequence) break;
        }
    }
    return null;
}

/// not all opcodes produce a new row, and some need to alter the state *after* producing
/// the next one. so we return the new row when applicable, null when no new row was produced.
pub fn updateState(self: LineProg, state: *State, reader: anytype) !?State {
    var new_row: ?State = null;

    const op = try reader.readByte();
    if (op == 0) { // extended opcodes
        const op_size = try std.leb.readULEB128(u8, reader);
        _ = op_size;
        const extended_op = try reader.readByte();
        switch (extended_op) {
            DW.LNE.end_sequence => {
                state.end_sequence = true;
                new_row = state.*;
                state.* = State{ .is_stmt = self.default_is_stmt };
            },
            DW.LNE.set_address => {
                state.address = switch (self.address_size) {
                    4 => try reader.readIntLittle(u32),
                    8 => try reader.readIntLittle(u64),
                    else => std.debug.panic("invalid address size {}\n", .{self.address_size}),
                };
            },
            DW.LNE.set_discriminator => {
                state.discriminator = try std.leb.readULEB128(u32, reader);
            },
            else => std.debug.panic("unknown extended opcode 0x{x}\n", .{extended_op}),
        }
    } else if (op < self.opcode_base) { // standard opcodes
        switch (op) {
            DW.LNS.copy => {
                new_row = state.*;
                state.discriminator = 0;
                state.basic_block = false;
                state.prologue_end = false;
                state.epilogue_begin = false;
            },
            DW.LNS.advance_pc => {
                const advance = try std.leb.readULEB128(u32, reader);
                state.address += self.min_instr_len * ((state.op_index + advance) / self.max_ops_per_instr);
                state.op_index = (state.op_index + advance) % self.max_ops_per_instr;
            },
            DW.LNS.advance_line => {
                const advance = try std.leb.readILEB128(i32, reader);
                state.line = @intCast(u32, @intCast(i32, state.line) + advance);
            },
            DW.LNS.set_file => state.file = try std.leb.readULEB128(u32, reader),
            DW.LNS.set_column => state.column = try std.leb.readULEB128(u32, reader),
            DW.LNS.negate_stmt => state.is_stmt = !state.is_stmt,
            DW.LNS.set_basic_block => state.basic_block = true,
            DW.LNS.const_add_pc => {
                const adjusted_opcode = 255 - self.opcode_base;
                const operation_advance = adjusted_opcode / self.line_range;
                state.address += self.min_instr_len * ((state.op_index + operation_advance) / self.max_ops_per_instr);
                state.op_index = (state.op_index + operation_advance) % self.max_ops_per_instr;
            },
            DW.LNS.fixed_advance_pc => {
                state.address += try reader.readIntLittle(u16);
                state.op_index = 0;
            },
            DW.LNS.set_prologue_end => state.prologue_end = true,
            DW.LNS.set_epilogue_begin => state.epilogue_begin = true,
            DW.LNS.set_isa => state.isa = try std.leb.readULEB128(u32, reader),
            else => std.debug.panic("unknown standard opcode 0x{x}\n", .{op}),
        }
    } else { // special opcodes
        const adjusted_opcode = op - self.opcode_base;
        const operation_advance = adjusted_opcode / self.line_range;

        const line_increment = self.line_base + @intCast(i32, (adjusted_opcode % self.line_range));

        state.line = @intCast(u32, @intCast(i32, state.line) + line_increment);
        state.address += self.min_instr_len * ((state.op_index + operation_advance) / self.max_ops_per_instr);
        state.op_index = (state.op_index + operation_advance) % self.max_ops_per_instr;

        new_row = state.*;

        state.basic_block = false;
        state.prologue_end = false;
        state.epilogue_begin = false;
        state.discriminator = 0;
    }

    return new_row;
}

pub fn dump(self: LineProg) !void {
    std.debug.print("version: {}\n", .{self.version});
    std.debug.print("address_size: {}\n", .{self.address_size});
    std.debug.print("segment_selector_size: {}\n", .{self.segment_selector_size});
    std.debug.print("min_instr_len: {}\n", .{self.min_instr_len});
    std.debug.print("max_ops_per_instr: {}\n", .{self.max_ops_per_instr});
    std.debug.print("default_is_stmt: {}\n", .{self.default_is_stmt});
    std.debug.print("line_base: {}\n", .{self.line_base});
    std.debug.print("line_range: {}\n", .{self.line_range});
    std.debug.print("opcode_base: {}\n", .{self.opcode_base});
    std.debug.print("standard_opcode_lens: {d}\n", .{self.standard_opcode_lens});
    std.debug.print("include_dirs:\n", .{});
    for (self.include_dirs, 0..) |dir, i| std.debug.print("[{:0>2}] {s}\n", .{ i, dir });
    std.debug.print("files:\n", .{});
    for (self.files, 0..) |file, i| {
        std.debug.print("[{:0>3}] dir={:0>2} | {s}\n", .{ i, file.dir, file.name });
    }

    std.debug.print("post-init data:\n", .{});
    std.debug.print("  address range (inclusive): 0x{x:0>12} -> 0x{x:0>12}\n", .{
        self.address_range[0], self.address_range[1],
    });
    std.debug.print("  line range (inclusive): {} -> {}\n", .{
        self.file_line_range[0], self.file_line_range[1],
    });

    std.debug.print("full matrix:\n", .{});
    var state = State{ .is_stmt = self.default_is_stmt };
    var stream = std.io.fixedBufferStream(self.ops);
    var reader = stream.reader();
    var op_idx: usize = 0;
    while ((try stream.getPos()) < self.ops.len) : (op_idx += 1) {
        const new_row = try self.updateState(&state, reader);
        const row = new_row orelse continue;
        std.debug.print("[{d:0>6}]" ++
            " | addr=0x{x:0>12}" ++
            " | op_index={d: >2}" ++
            " | file={d: >3}" ++
            " | line={d: >4}" ++
            " | col={d: >3}" ++
            " | {s}" ++ // is_stmt
            " | {s}" ++ // basic_block
            " | {s}" ++ // end_sequence
            " | {s}" ++ // prologue_end
            " | {s}" ++ // epilogue_end
            " | isa={: >2}" ++
            " | discrim={}\n", .{
            op_idx,
            row.address,
            row.op_index,
            row.file,
            row.line,
            row.column,
            ([2][]const u8{ "       ", "is_stmt" })[if (row.is_stmt) 1 else 0],
            ([2][]const u8{ "           ", "basic_block" })[if (row.basic_block) 1 else 0],
            ([2][]const u8{ "            ", "end_sequence" })[if (row.end_sequence) 1 else 0],
            ([2][]const u8{ "            ", "prologue_end" })[if (row.prologue_end) 1 else 0],
            ([2][]const u8{ "              ", "epilogue_begin" })[if (row.epilogue_begin) 1 else 0],
            row.isa,
            row.discriminator,
        });
    }
}
