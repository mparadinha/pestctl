const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");

const Dwarf = @This();

/// user must init these slices (using this allocator) before calling `initTables`
allocator: Allocator,
debug_info: []const u8,
debug_abbrev: []const u8,
debug_str: []const u8,
debug_line: []const u8,
debug_line_str: []const u8,
debug_ranges: []const u8,

pub fn initTables(self: *Dwarf) !void {
    _ = self;
}

pub fn deinit(self: *Dwarf) void {
    self.allocator.free(self.debug_info);
    self.allocator.free(self.debug_abbrev);
    self.allocator.free(self.debug_str);
    self.allocator.free(self.debug_line);
    self.allocator.free(self.debug_line_str);
    self.allocator.free(self.debug_ranges);
}

pub const LineProg = struct {
    raw_data: []const u8, // not owned by  LineProg

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
    file_names: [][]const u8,

    // TODO: switch the include dirs and filenames to tables with more data like timestamps, md5

    ops: []const u8,

    const State = struct {
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

    /// `debug_line` and `debug_line_str` must remain valid memory while this LineProg is used
    pub fn init(allocator: Allocator, debug_line: []const u8, debug_line_str: []const u8, offset: usize) !LineProg {
        _ = debug_line_str;
        var self = @as(LineProg, undefined);
        self.allocator = allocator;

        var stream = std.io.fixedBufferStream(debug_line[offset..]);
        var reader = stream.reader();

        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntBig(u64);
        };
        const is_64 = (initial_len == 0xffff_ffff);
        self.raw_data = debug_line[offset .. offset + unit_len];

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
        if (self.version == 4) {
            var include_dirs = std.ArrayList([]const u8).init(allocator);
            try include_dirs.append(""); // TODO: set this to the correct value

            var byte = try reader.readByte();
            while (byte != 0) : (byte = try reader.readByte()) {
                const str_start = (try stream.getPos()) - 1;
                while (byte != 0) : (byte = try reader.readByte()) {}
                const str_end = (try stream.getPos()) - 1;
                try include_dirs.append(self.raw_data[str_start..str_end]);
            }
            self.include_dirs = include_dirs.toOwnedSlice();
            //for (self.include_dirs) |dir| std.debug.print("include dir: {s}\n", .{dir});

            // from Dwarf v4 spec, chapter 6.2.4, pg.115:
            // A compiler may generate a single null byte for the file names field and define file names
            // using the extended opcode DW_LNE_define_file

            var file_names = std.ArrayList([]const u8).init(allocator);
            try file_names.append(""); // TODO: set this to the correct value
            byte = try reader.readByte();
            while (byte != 0) : (byte = try reader.readByte()) {
                const str_start = (try stream.getPos()) - 1;
                while (byte != 0) : (byte = try reader.readByte()) {}
                const str_end = (try stream.getPos()) - 1;
                try file_names.append(self.raw_data[str_start..str_end]);
                _ = try std.leb.readULEB128(usize, reader); // dir idx
                _ = try std.leb.readULEB128(usize, reader); // file mod time
                _ = try std.leb.readULEB128(usize, reader); // file len
            }
            self.file_names = file_names.toOwnedSlice();
            //for (self.file_names) |file| std.debug.print("file name: {s}\n", .{file});
        } else if (self.version == 5) {
            // the directory/file name entry formats specifiy the columns of a table see:
            // Dwarf v5 spec, chapter D.5.1 pg.321 (fig.D.33) for a good example

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
            for (self.include_dirs) |*dir, i| {
                for (dir_entry_formats) |format| {
                    if (format.content_type == std.dwarf.LNCT.path and format.form == std.dwarf.FORM.line_strp) {
                        const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
                        const strlen = c.strlen(&debug_line_str[strp]);
                        dir.* = debug_line_str[strp .. strp + strlen];
                        break;
                    }
                } else std.debug.panic("couldn't parse include dir {} (debug_line offfset = 0x{x})", .{
                    i, offset,
                });
            }
            //for (self.include_dirs) |dir| std.debug.print("include dir: {s}\n", .{dir});

            const file_entry_format_count = try reader.readByte();
            var file_entry_formats = try self.allocator.alloc(EntryFormat, file_entry_format_count);
            defer self.allocator.free(file_entry_formats);
            for (file_entry_formats) |*format| {
                format.content_type = try std.leb.readULEB128(u16, reader);
                format.form = try std.leb.readULEB128(u16, reader);
            }

            const file_count = try std.leb.readULEB128(usize, reader);
            self.file_names = try self.allocator.alloc([]const u8, file_count);
            for (self.file_names) |*file| {
                var found_path = false;
                for (file_entry_formats) |format| {
                    if (format.content_type == std.dwarf.LNCT.path) {
                        std.debug.assert(format.form == std.dwarf.FORM.line_strp);
                        const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
                        const strlen = c.strlen(&debug_line_str[strp]);
                        file.* = debug_line_str[strp .. strp + strlen];
                        found_path = true;
                    } else {
                        switch (format.form) {
                            std.dwarf.FORM.data1 => try reader.skipBytes(1, .{}),
                            std.dwarf.FORM.data2 => try reader.skipBytes(2, .{}),
                            std.dwarf.FORM.data4 => try reader.skipBytes(4, .{}),
                            std.dwarf.FORM.data8 => try reader.skipBytes(8, .{}),
                            std.dwarf.FORM.data16 => try reader.skipBytes(16, .{}),
                            std.dwarf.FORM.udata => _ = try std.leb.readULEB128(usize, reader),
                            std.dwarf.FORM.block => {
                                const block_len = try std.leb.readULEB128(usize, reader);
                                try reader.skipBytes(block_len, .{});
                            },
                            else => std.debug.panic("couldn't parse FORM=0x{x} to skip (debug_line offset = 0x{x})\n", .{ format.form, offset }),
                        }
                    }
                }
                std.debug.assert(found_path);
            }
            //for (self.file_names) |file| std.debug.print("file name: {s}\n", .{file});
        }

        if ((try stream.getPos()) < self.raw_data.len) {
            self.ops = self.raw_data[try stream.getPos()..];
        } else {
            self.ops.len = 0;
        }

        return self;
    }

    pub fn deinit(self: LineProg) void {
        self.allocator.free(self.standard_opcode_lens);
        self.allocator.free(self.include_dirs);
        self.allocator.free(self.file_names);
    }

    // TODO: do this with a `anytype` like for UiContext and use inline for
    //pub fn findUntilMatch()

    pub fn findAddrForSrc(self: LineProg, file: u32, line: u32) !?State {
        var state = State{ .is_stmt = self.default_is_stmt };
        var stream = std.io.fixedBufferStream(self.ops);
        var reader = stream.reader();
        while ((try stream.getPos()) < self.ops.len) {
            //std.debug.print("next_byte=0x{x:0>2}, addr=0x{x:0>14}, line={d:0>4}, col={d:0>3}, file={d:0>2}\n", .{
            //    self.ops[try stream.getPos()], state.address, state.line, state.column, state.file,
            //});
            const new_row = try self.updateState(&state, reader);
            if (new_row) |row| {
                if (row.file == file and row.line >= line) return row;
            }
            if (state.end_sequence) break;
        }
        return null;
    }

    pub fn findAddr(self: LineProg, addr: usize) !?State {
        var state = State{ .is_stmt = self.default_is_stmt };
        var stream = std.io.fixedBufferStream(self.ops);
        var reader = stream.reader();
        while ((try stream.getPos()) < self.ops.len) {
            const new_row = try self.updateState(&state, reader);
            if (new_row) |row| {
                if (row.address > addr) return row;
            }
            if (state.end_sequence) break;
        }
        return null;
    }

    //pub fn find

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
                std.dwarf.LNE.end_sequence => {
                    state.end_sequence = true;
                    new_row = state.*;
                    state.* = State{ .is_stmt = self.default_is_stmt };
                },
                std.dwarf.LNE.set_address => {
                    state.address = switch (self.address_size) {
                        4 => try reader.readIntLittle(u32),
                        8 => try reader.readIntLittle(u64),
                        else => std.debug.panic("invalid address size {}\n", .{self.address_size}),
                    };
                },
                std.dwarf.LNE.set_discriminator => {
                    state.discriminator = try std.leb.readULEB128(u32, reader);
                },
                else => std.debug.panic("unknown extended opcode 0x{x}\n", .{extended_op}),
            }
        } else if (op < self.opcode_base) { // standard opcodes
            switch (op) {
                std.dwarf.LNS.copy => {
                    new_row = state.*;
                    state.discriminator = 0;
                    state.basic_block = false;
                    state.prologue_end = false;
                    state.epilogue_begin = false;
                },
                std.dwarf.LNS.advance_pc => {
                    const advance = try std.leb.readULEB128(u32, reader);
                    state.address += self.min_instr_len * ((state.op_index + advance) / self.max_ops_per_instr);
                    state.op_index = (state.op_index + advance) % self.max_ops_per_instr;
                },
                std.dwarf.LNS.advance_line => {
                    const advance = try std.leb.readILEB128(i32, reader);
                    state.line = @intCast(u32, @intCast(i32, state.line) + advance);
                },
                std.dwarf.LNS.set_file => state.file = try std.leb.readULEB128(u32, reader),
                std.dwarf.LNS.set_column => state.column = try std.leb.readULEB128(u32, reader),
                std.dwarf.LNS.negate_stmt => state.is_stmt = !state.is_stmt,
                std.dwarf.LNS.set_basic_block => state.basic_block = true,
                std.dwarf.LNS.const_add_pc => {
                    const adjusted_opcode = 255 - self.opcode_base;
                    const operation_advance = adjusted_opcode / self.line_range;
                    state.address += self.min_instr_len * ((state.op_index + operation_advance) / self.max_ops_per_instr);
                    state.op_index = (state.op_index + operation_advance) % self.max_ops_per_instr;
                },
                std.dwarf.LNS.fixed_advance_pc => {
                    state.address += try reader.readIntLittle(u16);
                    state.op_index = 0;
                },
                std.dwarf.LNS.set_prologue_end => state.prologue_end = true,
                std.dwarf.LNS.set_epilogue_begin => state.epilogue_begin = true,
                std.dwarf.LNS.set_isa => state.isa = try std.leb.readULEB128(u32, reader),
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
};

pub fn reachedHere(src: std.builtin.SourceLocation) void {
    printWait("reached {s}:{s}:{}:{}\n", src);
}

pub fn printWait(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.time.sleep(100);
}
