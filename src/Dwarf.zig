const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");

const Dwarf = @This();

/// user must init these slices (using this allocator) before using this type
/// (note that after calling `deinit` will free these)
allocator: Allocator,
debug_info: []const u8,
debug_abbrev: []const u8,
debug_str: []const u8,
debug_line: []const u8,
debug_line_str: []const u8,
debug_ranges: []const u8,

/// these are filled by `initTables`
line_progs: []LineProg,
units: []DebugUnit,

pub fn deinit(self: *Dwarf) void {
    self.allocator.free(self.debug_info);
    self.allocator.free(self.debug_abbrev);
    self.allocator.free(self.debug_str);
    self.allocator.free(self.debug_line);
    self.allocator.free(self.debug_line_str);
    self.allocator.free(self.debug_ranges);

    for (self.units) |*unit| unit.deinit();
    self.allocator.free(self.units);

    for (self.line_progs) |*prog| prog.deinit();
    self.allocator.free(self.line_progs);
}

pub fn initTables(self: *Dwarf) !void {
    var debug_units = std.ArrayList(DebugUnit).init(self.allocator);
    var debug_unit_offset: usize = 0;
    while (debug_unit_offset < self.debug_info.len) {
        try debug_units.append(try DebugUnit.init(
            self.allocator,
            self.debug_info,
            debug_unit_offset,
            self.debug_abbrev,
            self.debug_str,
        ));
        debug_unit_offset += try DebugUnit.getSectionSize(self.debug_info, debug_unit_offset);
    }
    self.units = debug_units.toOwnedSlice();

    // NOTE/TODO: each type in a compilation unit there could also be a separate
    // debug unit (type unit in this case) so our `units` array should really only
    // have the compilation unit ones, so we can maps it simply to line program info
    // see Dwarf v5 spec, pg.199

    var line_progs = std.ArrayList(LineProg).init(self.allocator);
    var debug_line_offset: usize = 0;
    while (debug_line_offset < self.debug_line.len) {
        try line_progs.append(try LineProg.init(
            self.allocator,
            self.debug_line,
            debug_line_offset,
            self.debug_line_str,
            self.units[line_progs.items.len].comp_dir,
        ));
        debug_line_offset += try LineProg.getSectionSize(self.debug_line, debug_line_offset);
    }
    self.line_progs = line_progs.toOwnedSlice();
}

pub const DebugUnit = struct {
    allocator: Allocator,
    version: u16,
    unit_type: u8,
    address_size: u8,
    is_64: bool,
    abbrev_offset: usize,
    entries_buf: []const u8, // not owned by this struct

    abbrevs: []Abbrev,

    comp_dir: []const u8,

    pub const Abbrev = struct {
        tag: u16,
        has_children: bool,
        attribs: []Attrib,

        pub const Attrib = struct { attrib: u16, form: u16, implicit_value: i64 };

        pub fn deinit(self: *Abbrev, allocator: Allocator) void {
            allocator.free(self.attribs);
        }
    };

    /// this returns the amount of bytes we need to skip (starting from `offset`) to reach the
    /// end of the debug unit that starts at `offset`
    pub fn getSectionSize(debug_info: []const u8, offset: usize) !usize {
        var reader = std.io.fixedBufferStream(debug_info[offset..]).reader();
        const initial_len = try reader.readIntLittle(u32);
        if (initial_len < 0xffff_fff0) return 4 + initial_len else {
            return 4 + 8 + (try reader.readIntLittle(u64));
        }
    }

    /// this struct holds no references to the `debug_abbrev` data.
    /// `debug_info` and `debug_str` however must remain valid while using this DebugUnit
    /// call `deinit` to free memory
    pub fn init(
        allocator: Allocator,
        debug_info: []const u8,
        offset: usize,
        debug_abbrev: []const u8,
        debug_str: []const u8,
    ) !DebugUnit {
        _ = debug_str;
        var self = @as(DebugUnit, undefined);
        self.allocator = allocator;

        var stream = std.io.fixedBufferStream(debug_info[offset..]);
        var reader = stream.reader();

        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntLittle(u64);
        };
        _ = unit_len;
        self.is_64 = (initial_len == 0xffff_ffff);

        self.version = try reader.readIntLittle(u16);
        if (self.version < 4) std.debug.panic("debug_info unit @ offset=0x{x} has version={}\n", .{ offset, self.version });

        self.unit_type = if (self.version >= 5) try reader.readByte() else DW.UT.compile;
        if (self.unit_type != DW.UT.compile) std.debug.panic("debug_info unit @ offset=0x{x} has unit_type=0x{x}\n", .{ offset, self.unit_type });

        // WHY WOULD YOU SWITCH THE ORDER OF THESE BETWEEN VERSION!!!!!????
        if (self.version >= 5) {
            self.address_size = try reader.readByte();
            self.abbrev_offset = if (self.is_64) try reader.readIntLittle(u64) else try reader.readIntLittle(u32);
        } else if (self.version == 4) {
            self.abbrev_offset = if (self.is_64) try reader.readIntLittle(u64) else try reader.readIntLittle(u32);
            self.address_size = try reader.readByte();
        } else unreachable;

        try self.readAbbrevTable(debug_abbrev);

        self.entries_buf = debug_info[offset .. offset + (try stream.getPos())];

        //try self.parseAllDebugInfo(debug_str);

        // we need to get at least the `DW_AT_comp_dir` because before Dwarf v5
        // the line programs refer to it (implicitly)
        var comp_dir_opt: ?[]const u8 = null;
        //std.debug.print("pos b4 abbrev code: 0x{x}, offset=0x{x}\n", .{ (try stream.getPos()), offset });
        const abbrev_code = try std.leb.readULEB128(usize, reader);
        //std.debug.print("abbrev_code=0x{x}, #abbrevs={}\n", .{ abbrev_code, self.abbrevs.len });
        const abbrev = self.abbrevs[abbrev_code];
        if (abbrev.tag != DW.TAG.compile_unit) std.debug.panic("not compile unit! :( (tag=0x{x}, stream_pos=0x{x}\n", .{ abbrev.tag, (try stream.getPos()) });
        for (abbrev.attribs) |pair| {
            if (pair.attrib == DW.AT.comp_dir) {
                comp_dir_opt = try readString(pair.form, &stream, self.is_64, debug_str);
            } else try skipFORM(pair.form, reader, .{ .address_size = self.address_size, .is_64 = self.is_64, .version = self.version });
        }
        self.comp_dir = if (comp_dir_opt) |comp_dir| comp_dir else std.debug.panic("no comp_dir found in DW_TAG_compile_unit\n", .{});

        return self;
    }

    pub fn deinit(self: *DebugUnit) void {
        for (self.abbrevs) |*abbrev| abbrev.deinit(self.allocator);
        self.allocator.free(self.abbrevs);
    }

    fn readAbbrevTable(self: *DebugUnit, debug_abbrev: []const u8) !void {
        var stream = std.io.fixedBufferStream(debug_abbrev[self.abbrev_offset..]);
        var reader = stream.reader();

        //std.debug.print("self.abbrev_offset=0x{x}\n", .{self.abbrev_offset});

        var valid_codes = std.ArrayList(usize).init(self.allocator);
        defer valid_codes.deinit();

        //std.debug.print("allocator used init ArrayList(Abbrev): {}\n", .{self.allocator});
        var abbrevs = std.ArrayList(Abbrev).init(self.allocator);
        try abbrevs.append(.{ // entry 0 is reserved
            .tag = undefined,
            .has_children = false,
            .attribs = &[0]Abbrev.Attrib{},
        });
        try valid_codes.append(0);

        var code = try std.leb.readULEB128(usize, reader);
        while (code != 0) : (code = try std.leb.readULEB128(u16, reader)) {
            var tag = try std.leb.readULEB128(u16, reader);
            var has_children = (try reader.readByte()) == DW.CHILDREN.yes;

            var attribs = std.ArrayList(Abbrev.Attrib).init(self.allocator);
            while (true) {
                const attrib = try std.leb.readULEB128(u16, reader);
                const form = try std.leb.readULEB128(u16, reader);
                if (attrib == 0 and form == 0) break;
                try attribs.append(.{
                    .attrib = attrib,
                    .form = form,
                    // see Dwarf v5 spec, pg.207
                    .implicit_value = if (form == DW.FORM.implicit_const) blk: {
                        break :blk try std.leb.readILEB128(i64, reader);
                    } else @as(i64, undefined),
                });
            }

            if (code >= abbrevs.items.len) try abbrevs.resize(code + 1);
            //std.debug.print("setting abbrev for code={}, tag=0x{x}, has_children={}, #attribs={}\n", .{ code, tag, has_children, attribs.items.len });
            abbrevs.items[code] = .{
                .tag = tag,
                .has_children = has_children,
                .attribs = attribs.toOwnedSlice(),
            };
            try valid_codes.append(code);
        }

        self.abbrevs = abbrevs.toOwnedSlice();

        // because we're placing the abbrevs into the array using their code (and because
        // these codes have no obligations as to size or order) some items in the abbrev
        // list will be garbage that we need to init to null entries
        for (self.abbrevs) |*abbrev, i| {
            var is_undef = std.mem.indexOfScalar(usize, valid_codes.items, i) == null;
            if (is_undef) abbrev.* = .{
                .tag = undefined,
                .has_children = false,
                .attribs = &[0]Abbrev.Attrib{},
            };
        }
    }

    fn parseAllDebugInfo(self: *DebugUnit, debug_str: []const u8) !void {
        var stream = std.io.fixedBufferStream(self.entries_buf);
        var reader = stream.reader();
        const skip_info = SkipInfo{ .address_size = self.address_size, .is_64 = self.is_64, .version = self.version };

        while ((try stream.getPos()) < self.entries_buf.len) {
            const abbrev_code = try std.leb.readULEB128(usize, reader);
            const abbrev = self.abbrevs[abbrev_code];

            switch (abbrev.tag) {
                DW.TAG.compile_unit => {
                    for (abbrev.attribs) |pair| {
                        switch (pair.attrib) {
                            DW.AT.comp_dir => self.comp_dir = try readString(pair.form, &stream, self.is_64, debug_str),
                            else => try skipFORM(pair.form, reader, skip_info),
                        }
                    }
                },
                else => for (abbrev.attribs) |pair| try skipFORM(pair.form, reader, skip_info),
            }
        }
    }
};

/// gets a stream instead of reader because we need to use the underlying buffer and stream position
pub fn readString(form: u16, stream: anytype, is_64: bool, str_section: []const u8) ![]const u8 {
    _ = is_64;
    var reader = stream.reader();

    switch (form) {
        DW.FORM.string => {
            const start = try stream.getPos();
            const strlen = c.strlen(&stream.buffer[start]);
            try reader.skipBytes(strlen + 1, .{});
            return stream.buffer[start .. start + strlen];
        },
        DW.FORM.strp, DW.FORM.line_strp, DW.FORM.strp_sup => {
            const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
            const strlen = c.strlen(&str_section[strp]);
            return str_section[strp .. strp + strlen];
        },
        else => std.debug.panic("invalid FORM 0x{x} for string\n", .{form}),
    }
}

const SkipInfo = struct {
    address_size: u8,
    is_64: bool,
    version: u16,
};

fn readFormAddress(self: Dwarf, reader: anytype, address_size: u8) !u64 {
    _ = self;
    _ = reader;
    _ = address_size;
}

pub fn skipFORM(form: u16, reader: anytype, skip_info: SkipInfo) !void {
    switch (form) {
        // address
        DW.FORM.addr => try reader.skipBytes(skip_info.address_size, .{}),
        //.addrx
        //.addrx1
        //.addrx2
        //.addrx3
        //.addrx4

        // block
        //.block
        //.block1
        //.block2
        //.block4

        // constant
        DW.FORM.data1 => try reader.skipBytes(1, .{}),
        DW.FORM.data2 => try reader.skipBytes(2, .{}),
        DW.FORM.data4 => try reader.skipBytes(4, .{}),
        DW.FORM.data8 => try reader.skipBytes(8, .{}),
        DW.FORM.data16 => try reader.skipBytes(16, .{}),
        //.sdata
        DW.FORM.udata => _ = try std.leb.readULEB128(u64, reader),
        //.implicit_const

        // exprloc
        //.exprloc

        // flag
        DW.FORM.flag => try reader.skipBytes(1, .{}),
        DW.FORM.flag_present => {},

        // loclist
        //.loclistx

        // rnglist
        //.rnglistx

        // reference
        //.ref_addr
        //.ref1
        //.ref2
        //.ref4
        //.ref8
        //.ref_udata
        //.ref_sup4
        //.ref_sup8
        //.ref_sig8

        // string
        //.string
        DW.FORM.strp => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        //.strx
        //.strp_sup
        DW.FORM.line_strp => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),
        //.strx1
        //.strx2
        //.strx3
        //.strx4

        // addrptr, lineptr, loclistptr, macptr, rnglistptr, stroffsetsptr
        DW.FORM.sec_offset => try reader.skipBytes(if (skip_info.is_64) 8 else 4, .{}),

        // other
        //.indirect

        else => std.debug.panic("unknown FORM=0x{x}. can't skip.\n", .{form}),
    }
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
    files: []FileInfo,
    ops: []const u8,

    // some things filled during init to help with searching without having to actually run
    // every single line program in the debug information
    address_range: [2]u64,

    pub const FileInfo = struct {
        name: []const u8,
        dir: usize,
        mod_time: u64,
        len: usize,
    };

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

    /// this returns the amount of bytes we need to skip (starting from `offset`) to reach the
    /// end of the .debug_line "section" that starts at `offset`
    pub fn getSectionSize(debug_line: []const u8, offset: usize) !usize {
        var reader = std.io.fixedBufferStream(debug_line[offset..]).reader();
        const initial_len = try reader.readIntLittle(u32);
        if (initial_len < 0xffff_fff0) return 4 + initial_len else {
            return 4 + 8 + (try reader.readIntLittle(u64));
        }
    }

    /// `debug_line` and `debug_line_str` must remain valid memory while this LineProg is used
    pub fn init(allocator: Allocator, debug_line: []const u8, offset: usize, debug_line_str: []const u8, comp_dir: []const u8) !LineProg {
        _ = debug_line_str;
        var self = @as(LineProg, undefined);
        self.allocator = allocator;

        var stream = std.io.fixedBufferStream(debug_line[offset..]);
        var reader = stream.reader();

        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntLittle(u64);
        };
        const is_64 = (initial_len == 0xffff_ffff);
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
            self.include_dirs = include_dirs.toOwnedSlice();

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
            self.files = files.toOwnedSlice();
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
            for (self.include_dirs) |*dir, i| {
                for (dir_entry_formats) |format| {
                    if (format.content_type == DW.LNCT.path and format.form == DW.FORM.line_strp) {
                        const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
                        const strlen = c.strlen(&debug_line_str[strp]);
                        dir.* = debug_line_str[strp .. strp + strlen];
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
                            DW.FORM.string => blk: {
                                const str_start = try stream.getPos();
                                const strlen = c.strlen(&self.raw_data[str_start]);
                                try reader.skipBytes(strlen + 1, .{});
                                break :blk self.raw_data[str_start .. str_start + strlen];
                            },
                            DW.FORM.line_strp => blk: {
                                const strp = if (is_64) try reader.readIntLittle(u64) else @intCast(u64, try reader.readIntLittle(u32));
                                const strlen = c.strlen(&debug_line_str[strp]);
                                break :blk debug_line_str[strp .. strp + strlen];
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

        // run the whole program once to find the range of addresses
        // this helps later with searching through multiple LineProg's
        self.address_range = .{ std.math.maxInt(u64), 0 };
        var state = State{ .is_stmt = self.default_is_stmt };
        var op_stream = std.io.fixedBufferStream(self.ops);
        var op_reader = op_stream.reader();
        while ((try op_stream.getPos()) < self.ops.len) {
            const new_row = try self.updateState(&state, op_reader);
            if (new_row) |row| {
                self.address_range[0] = std.math.min(self.address_range[0], row.address);
                self.address_range[1] = std.math.max(self.address_range[1], row.address);
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

    pub fn findAddrForSrc(self: LineProg, file: u32, line: u32) !?State {
        var state = State{ .is_stmt = self.default_is_stmt };
        var stream = std.io.fixedBufferStream(self.ops);
        var reader = stream.reader();
        while ((try stream.getPos()) < self.ops.len) {
            const new_row = try self.updateState(&state, reader);
            if (new_row) |row| {
                if (row.file == file and row.line >= line) return row;
                if (row.end_sequence) break;
            }
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
};

pub fn reachedHere(src: std.builtin.SourceLocation) void {
    printWait("reached {s}:{s}:{}:{}\n", src);
}

pub fn printWait(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.time.sleep(100);
}

const DW = struct {
    const dwarf = @import("dwarf_constants.zig");

    pub const TAG = dwarf.TAG;
    pub const AT = dwarf.AT;
    pub const OP = dwarf.OP;
    pub const LANG = dwarf.LANG;
    pub const FORM = dwarf.FORM;
    pub const ATE = dwarf.ATE;
    pub const LLE = dwarf.LLE;
    pub const CFA = dwarf.CFA;
    pub const CHILDREN = dwarf.CHILDREN;
    pub const LNS = dwarf.LNS;
    pub const LNE = dwarf.LNE;
    pub const UT = dwarf.UT;
    pub const LNCT = dwarf.LNCT;
    pub const RLE = dwarf.RLE;
    pub const CC = dwarf.CC;
};
