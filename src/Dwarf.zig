const std = @import("std");
const Allocator = std.mem.Allocator;
pub const forms = @import("dwarf/forms.zig");
pub const LineProg = @import("dwarf/LineProg.zig");
pub const Frame = @import("dwarf/Frame.zig");
pub const DW = @import("dwarf/constants.zig");
const stringFromTable = @import("Elf.zig").stringFromTable;
const Registers = @import("Session.zig").Registers;

const Dwarf = @This();

/// user must init these slices (using this allocator) before using this type
/// (note that calling `deinit` will free these)
allocator: Allocator,
debug_info: []const u8,
debug_abbrev: []const u8,
debug_str: []const u8,
debug_line: []const u8,
debug_line_str: []const u8,
debug_ranges: []const u8,
debug_frame: []const u8,
debug_loc: []const u8,
debug_loclists: []const u8,
eh_frame: []const u8, // TODO: use .eh_frame when .debug_frame is not available

/// these are filled by `initTables`
line_progs: []LineProg,
units: []DebugUnit,
frames: []Frame,

pub fn deinit(self: *Dwarf) void {
    self.allocator.free(self.debug_info);
    self.allocator.free(self.debug_abbrev);
    self.allocator.free(self.debug_str);
    self.allocator.free(self.debug_line);
    self.allocator.free(self.debug_line_str);
    self.allocator.free(self.debug_ranges);
    self.allocator.free(self.debug_frame);
    self.allocator.free(self.debug_loc);
    self.allocator.free(self.debug_loclists);
    self.allocator.free(self.eh_frame);

    for (self.line_progs) |*prog| prog.deinit();
    self.allocator.free(self.line_progs);

    for (self.units) |*unit| unit.deinit();
    self.allocator.free(self.units);

    //for (self.frames) |*frame| frame.deinit();
    self.allocator.free(self.frames);
}

const Sections = struct {
    info: ?[]const u8 = null,
    abbrev: ?[]const u8 = null,
    str: ?[]const u8 = null,
    line: ?[]const u8 = null,
    line_str: ?[]const u8 = null,
    ranges: ?[]const u8 = null,
    frame: ?[]const u8 = null,
    loc: ?[]const u8 = null,
    loclists: ?[]const u8 = null,
    eh_frame: ?[]const u8 = null,
};

pub fn initTables(self: *Dwarf) !void {
    const sections = Sections{
        .info = if (self.debug_info.len > 0) self.debug_info else null,
        .abbrev = if (self.debug_abbrev.len > 0) self.debug_abbrev else null,
        .str = if (self.debug_str.len > 0) self.debug_str else null,
        .line = if (self.debug_line.len > 0) self.debug_line else null,
        .line_str = if (self.debug_line_str.len > 0) self.debug_line_str else null,
        .ranges = if (self.debug_ranges.len > 0) self.debug_ranges else null,
        .frame = if (self.debug_frame.len > 0) self.debug_frame else null,
        .loc = if (self.debug_loc.len > 0) self.debug_loc else null,
        .loclists = if (self.debug_loclists.len > 0) self.debug_loclists else null,
        .eh_frame = if (self.eh_frame.len > 0) self.eh_frame else null,
    };
    _ = sections;

    var debug_units = std.ArrayList(DebugUnit).init(self.allocator);
    var debug_unit_offset: usize = 0;
    while (debug_unit_offset < self.debug_info.len) {
        try debug_units.append(try DebugUnit.init(
            self.allocator,
            self.debug_info,
            debug_unit_offset,
            self.debug_abbrev,
            self.debug_str,
            self.debug_line_str,
            self.debug_loc,
            self.debug_loclists,
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

    self.frames = try loadAllFrames(self.allocator, self.debug_frame, self.eh_frame);
    //std.debug.print("all frames:\n", .{});
    //for (self.frames) |frame| {
    //    std.debug.print("pc_begin=0x{x:0>12}, pc_end=0x{x:0>12}\n", .{ frame.pc_begin, frame.pc_end });
    //}
}

pub const SrcLoc = struct {
    dir: []const u8,
    file: []const u8,
    line: u32,
    column: u32,

    pub fn format(value: SrcLoc, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("{{.dir={s}, .file={s}, .line={}, column={}}}", value);
    }
};

pub const DeclCoords = struct {
    file: u32,
    line: u32,
    column: u32,

    pub fn toSrcLoc(self: DeclCoords, line_prog: LineProg) SrcLoc {
        return .{
            .dir = line_prog.dirs[line_prog.files[self.file].dir],
            .file = line_prog.files[self.file].name,
            .line = self.line,
            .column = self.column,
        };
    }
};

pub const Abbrev = struct {
    tag: u16,
    has_children: bool,
    attribs: []Attrib,

    pub fn deinit(self: *Abbrev, allocator: Allocator) void {
        allocator.free(self.attribs);
    }
};

pub const Attrib = struct { attrib: u16, form: u16, implicit_value: i64 };

pub const Function = struct {
    name: []const u8,
    decl_coords: ?DeclCoords,

    @"extern": ?bool,
    low_pc: ?usize,
    high_pc: ?usize,
    frame_base: ?LocationDesc,

    ret_type: ?*Type,
    comp_unit: *DebugUnit,
    params: []const Variable,
};

pub const Variable = struct {
    name: []const u8,
    decl_coords: ?DeclCoords,

    loc: ?LocationDesc,

    @"type": ?*Type,
    function: ?*Function,
};

pub const Type = struct {
    name: []const u8,
    data: union(enum) {
        Base: struct {},
        Struct: struct {},
        Enum: struct {},
    },
};

pub const LocationDesc = union(enum) {
    expr: []const u8, // DWARF expression
    loclist_offset: usize,
    // TODO: DWARF loclistx
};

pub const DebugUnit = struct {
    allocator: Allocator,
    version: u16,
    unit_type: u8,
    address_size: u8,
    is_64: bool,
    abbrev_offset: usize,
    entries_buf: []const u8, // not owned by this struct

    header_size: usize,

    abbrevs: []Abbrev,

    debug_info: []const u8,

    comp_dir: []const u8,
    functions: []Function,
    variables: []Variable,
    types: []Type,

    /// this returns the amount of bytes we need to skip (starting from `offset`) to reach the
    /// end of the debug unit that starts at `offset`
    pub fn getSectionSize(debug_info: []const u8, offset: usize) !usize {
        var stream = std.io.fixedBufferStream(debug_info[offset..]);
        var reader = stream.reader();
        return (try readLengthField(reader)).fullLength();
    }

    /// this struct holds no references to the `debug_abbrev` data
    /// `debug_info` and `debug_str` however must remain valid while using this DebugUnit
    /// call `deinit` to free memory
    pub fn init(
        allocator: Allocator,
        debug_info: []const u8,
        offset: usize,
        debug_abbrev: []const u8,
        debug_str: []const u8,
        debug_line_str: []const u8,
        debug_loc: []const u8,
        debug_loclists: []const u8,
    ) !DebugUnit {
        var self = @as(DebugUnit, undefined);
        self.debug_info = debug_info;
        self.allocator = allocator;

        var stream = std.io.fixedBufferStream(debug_info[offset..]);
        var reader = stream.reader();

        const length = try readLengthField(reader);
        const unit_len = length.length;
        self.is_64 = length.is_64;
        const section_size = (try stream.getPos()) + unit_len;

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

        self.header_size = try stream.getPos();
        std.debug.print("header_size=0x{x}\n", .{self.header_size});

        const entries_buf_start = offset + self.header_size;
        const entries_buf_size = section_size - self.header_size;
        self.entries_buf = debug_info[entries_buf_start .. entries_buf_start + entries_buf_size];

        // before Dwarf v5 the line program implicitly refers to the compilation directory
        // so we have to parse at least that much of the .debug_info section (just the first
        // entry) to make sure we can use LineProg correctly
        self.comp_dir = try self.getCompilationDir(debug_str, debug_line_str);

        const fixup_info = try self.loadFunctions(debug_str);
        const variable_fixups = fixup_info.variable_fixups;
        defer self.allocator.free(variable_fixups);
        const function_fixups = fixup_info.function_fixups;
        defer self.allocator.free(function_fixups);

        const type_fixup_info = try self.loadTypes(debug_str);
        const type_offsets = type_fixup_info.type_offsets;
        defer self.allocator.free(type_offsets);
        const type_fixups = type_fixup_info.type_fixups;
        defer self.allocator.free(type_fixups);

        for (variable_fixups) |fixup| {
            const var_ptr = &self.variables[fixup.var_idx];
            if (fixup.fn_idx) |idx| {
                const fn_ptr = &self.functions[idx];
                var_ptr.function = fn_ptr;
            }
            if (fixup.type_offset) |type_offset| {
                const type_idx = std.mem.indexOfScalar(usize, type_offsets, type_offset) orelse
                    std.debug.panic("var {s}: no type with offset 0x{x}\n", .{ var_ptr.name, type_offset });
                const type_ptr = &self.types[type_idx];
                var_ptr.@"type" = type_ptr;
            }
        }

        for (function_fixups) |fixup| {
            const fn_ptr = &self.functions[fixup.fn_idx];
            const type_idx = std.mem.indexOfScalar(usize, type_offsets, fixup.type_offset) orelse
                std.debug.panic("fn {s}: no type with offset 0x{x}\n", .{ fn_ptr.name, fixup.type_offset });
            const type_ptr = &self.types[type_idx];
            fn_ptr.ret_type = type_ptr;
        }

        //for (self.functions) |function| {
        //    std.debug.print("function: {s}, decl_coords={}, ret_type.name={s}\n", .{
        //        function.name,
        //        function.decl_coords,
        //        if (function.ret_type) |ty| ty.name else "null",
        //    });
        //}

        _ = debug_loc;
        _ = debug_loclists;
        //for (self.variables) |variable| {
        //    std.debug.print("variable: {s}, decl_coords={}, type={s}, function={s}, loc={}\n", .{
        //        variable.name,
        //        variable.decl_coords,
        //        if (variable.@"type") |ty| ty.name else "null",
        //        if (variable.function) |func| func.name else "null",
        //        variable.loc,
        //    });
        //    if (variable.loc) |loc| {
        //        switch (loc) {
        //            .expr => {},
        //            .loclist_offset => |loc_offset| {
        //                if (debug_loc.len > 0) {
        //                    std.debug.print("  using .debug_loc ... @ offset=0x{x}\n", .{loc_offset});
        //                    var loc_stream = std.io.fixedBufferStream(debug_loc[loc_offset..]);
        //                    var loc_reader = loc_stream.reader();
        //                    while (true) {
        //                        const entry_offset = loc_offset + (try loc_stream.getPos());

        //                        const addr_start = try loc_reader.readIntLittle(usize);
        //                        const addr_end = try loc_reader.readIntLittle(usize);
        //                        if (addr_start == 0 and addr_end == 0) break;

        //                        const block_len = try loc_reader.readIntLittle(u16);
        //                        const block_start = loc_offset + (try loc_stream.getPos());
        //                        const expr_data = debug_loc[block_start .. block_start + block_len];
        //                        try loc_reader.skipBytes(block_len, .{});

        //                        const first_op: ?u8 = if (block_len > 0) expr_data[0] else null;
        //                        std.debug.print("  - [0x{x:0>8}] addr offset range (0x{x} -> 0x{x}): block_len={}, first op: {s}\n", .{
        //                            entry_offset,
        //                            addr_start,
        //                            addr_end,
        //                            block_len,
        //                            if (first_op) |op| DW.OP.asStr(op) else "null",
        //                        });

        //                        //const Expression = @import("dwarf/Expression.zig");
        //                        //const registers = [_]usize{0x8_0000_0000} ** @typeInfo(Register).Enum.fields.len;
        //                        //const expr_result = Expression.result(expr_data, registers, 6);
        //                        //std.debug.print("   \\ expression result: {}\n", .{expr_result});
        //                    }
        //                } else if (debug_loclists.len > 0) {
        //                    std.debug.print("  using .debug_loclist ...\n", .{});
        //                } else @panic("wat");
        //            },
        //        }
        //    }
        //}

        return self;
    }

    pub fn deinit(self: *DebugUnit) void {
        for (self.abbrevs) |*abbrev| abbrev.deinit(self.allocator);
        self.allocator.free(self.abbrevs);
        self.allocator.free(self.functions);
        self.allocator.free(self.variables);
        self.allocator.free(self.types);
    }

    fn readAbbrevTable(self: *DebugUnit, debug_abbrev: []const u8) !void {
        var stream = std.io.fixedBufferStream(debug_abbrev[self.abbrev_offset..]);
        var reader = stream.reader();

        var valid_codes = std.ArrayList(usize).init(self.allocator);
        defer valid_codes.deinit();

        var abbrevs = std.ArrayList(Abbrev).init(self.allocator);
        try abbrevs.append(.{ // entry 0 is reserved
            .tag = undefined,
            .has_children = false,
            .attribs = &[0]Attrib{},
        });
        try valid_codes.append(0);

        var code = try std.leb.readULEB128(usize, reader);
        while (code != 0) : (code = try std.leb.readULEB128(u16, reader)) {
            var tag = try std.leb.readULEB128(u16, reader);
            var has_children = (try reader.readByte()) == DW.CHILDREN.yes;

            var attribs = std.ArrayList(Attrib).init(self.allocator);
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
            abbrevs.items[code] = .{
                .tag = tag,
                .has_children = has_children,
                .attribs = attribs.toOwnedSlice(),
            };
            try valid_codes.append(code);
        }

        self.abbrevs = abbrevs.toOwnedSlice();

        // because we're placing the abbrevs into the array using their code (and because
        // these codes have no obligations as to size or order of declaration) some items
        // in the abbrev list will be garbage that we need to init to null entries
        for (self.abbrevs) |*abbrev, i| {
            var is_undef = std.mem.indexOfScalar(usize, valid_codes.items, i) == null;
            if (is_undef) abbrev.* = .{
                .tag = undefined,
                .has_children = false,
                .attribs = &[0]Attrib{},
            };
        }
    }

    fn getCompilationDir(self: *DebugUnit, debug_str: []const u8, debug_line_str: []const u8) ![]const u8 {
        var stream = std.io.fixedBufferStream(self.entries_buf);
        var reader = stream.reader();

        const skip_info = forms.SkipInfo{
            .address_size = self.address_size,
            .is_64 = self.is_64,
        };
        const read_info = forms.ReadInfo{
            .address_size = self.address_size,
            .is_64 = self.is_64,
            .version = self.version,
            .debug_info_opt = self.debug_info,
            .debug_str_opt = debug_str,
            .debug_line_str_opt = debug_line_str,
        };

        const abbrev_code = try std.leb.readULEB128(usize, reader);
        const abbrev = self.abbrevs[abbrev_code];
        std.debug.assert(abbrev.tag == DW.TAG.compile_unit);

        for (abbrev.attribs) |pair| {
            switch (pair.attrib) {
                DW.AT.comp_dir => return forms.readString(read_info, reader, pair.form),
                else => try forms.skip(skip_info, reader, pair.form),
            }
        }
        @panic("didn't find a comp_dir attribute");
    }

    const VariableFixUp = struct { var_idx: usize, fn_idx: ?usize, type_offset: ?usize };
    const FunctionFixUp = struct { fn_idx: usize, type_offset: usize };
    const FixUpInfo = struct { variable_fixups: []VariableFixUp, function_fixups: []FunctionFixUp };

    fn loadFunctions(self: *DebugUnit, debug_str: []const u8) !FixUpInfo {
        var variables = std.ArrayList(Variable).init(self.allocator);
        var variable_fixups = std.ArrayList(VariableFixUp).init(self.allocator);
        var functions = std.ArrayList(Function).init(self.allocator);
        var function_fixups = std.ArrayList(FunctionFixUp).init(self.allocator);

        var stream = std.io.fixedBufferStream(self.entries_buf);
        var reader = stream.reader();

        const skip_info = forms.SkipInfo{
            .address_size = self.address_size,
            .is_64 = self.is_64,
        };
        const read_info = forms.ReadInfo{
            .address_size = self.address_size,
            .is_64 = self.is_64,
            .version = self.version,
            .debug_info_opt = self.debug_info,
            .debug_str_opt = debug_str,
        };

        var parent_function_level: ?usize = null;
        var parent_function: ?usize = null;
        var child_level: usize = 0;
        while ((try stream.getPos()) < self.entries_buf.len) {
            const abbrev_code = try std.leb.readULEB128(usize, reader);
            const abbrev = self.abbrevs[abbrev_code];

            if (abbrev.has_children) child_level += 1;
            if (abbrev.tag == 0) {
                if (parent_function_level) |level| {
                    if (level == child_level) {
                        parent_function = null;
                        parent_function_level = null;
                    }
                }
                child_level -= 1;
                if (child_level == 0) break;
            }

            switch (abbrev.tag) {
                DW.TAG.subprogram => {
                    var function = Function{
                        .name = &[0]u8{},
                        .decl_coords = null,
                        .@"extern" = null,
                        .low_pc = null,
                        .high_pc = null,
                        .frame_base = null,
                        .ret_type = null,
                        .comp_unit = self,
                        .params = &[0]Variable{},
                    };

                    for (abbrev.attribs) |pair| {
                        switch (pair.attrib) {
                            DW.AT.name => function.name = try forms.readString(read_info, reader, pair.form),
                            //DW.AT.main_subprogram => function.is_entry_point = try forms.readFlag(read_info, reader, pair.form),
                            DW.AT.external => function.@"extern" = try forms.readFlag(read_info, reader, pair.form),
                            DW.AT.low_pc => function.low_pc = try forms.readAddress(read_info, reader, pair.form),
                            DW.AT.high_pc => function.high_pc = switch (DW.Class.fromForm(pair.form)) {
                                .address => try forms.readAddress(read_info, reader, pair.form),
                                .constant => function.low_pc.? + try forms.readConstant(usize, read_info, reader, pair),
                                else => unreachable,
                            },
                            DW.AT.decl_file => {
                                if (function.decl_coords == null) function.decl_coords = .{ .file = 0, .line = 0, .column = 0 };
                                function.decl_coords.?.file = try forms.readConstant(u32, read_info, reader, pair);
                            },
                            DW.AT.decl_line => {
                                if (function.decl_coords == null) function.decl_coords = .{ .file = 0, .line = 0, .column = 0 };
                                function.decl_coords.?.line = try forms.readConstant(u32, read_info, reader, pair);
                            },
                            DW.AT.decl_column => {
                                if (function.decl_coords == null) function.decl_coords = .{ .file = 0, .line = 0, .column = 0 };
                                function.decl_coords.?.column = try forms.readConstant(u32, read_info, reader, pair);
                            },
                            DW.AT.@"type" => {
                                const type_offset = try forms.readReference(read_info, reader, pair.form);
                                try function_fixups.append(.{ .fn_idx = functions.items.len, .type_offset = type_offset });
                            },
                            else => try forms.skip(skip_info, reader, pair.form),
                        }
                    }

                    //std.debug.print("function: {s}, extern={}, low_pc=0x{x}, high_pc=0x{x}, decl_coords={}\n", .{
                    //    function.name,
                    //    function.@"extern",
                    //    function.low_pc,
                    //    function.high_pc,
                    //    function.decl_coords,
                    //});

                    parent_function = functions.items.len;
                    parent_function_level = child_level;
                    try functions.append(function);
                },
                DW.TAG.formal_parameter => {
                    if (parent_function != null and parent_function_level.? == child_level) {
                        var name_opt: ?[]const u8 = null;

                        for (abbrev.attribs) |pair| {
                            switch (pair.attrib) {
                                DW.AT.name => name_opt = try forms.readString(read_info, reader, pair.form),
                                else => try forms.skip(skip_info, reader, pair.form),
                            }
                        }

                        //std.debug.print("  param: {s}\n", .{
                        //    name_opt,
                        //});
                    } else {
                        for (abbrev.attribs) |pair| try forms.skip(skip_info, reader, pair.form);
                    }
                },
                DW.TAG.variable => {
                    var variable = Variable{
                        .name = &[0]u8{},
                        .decl_coords = null,
                        .loc = null,
                        .@"type" = null,
                        .function = null,
                    };
                    var type_offset = @as(?usize, null);
                    for (abbrev.attribs) |pair| {
                        switch (pair.attrib) {
                            DW.AT.name => variable.name = try forms.readString(read_info, reader, pair.form),
                            DW.AT.@"type" => type_offset = try forms.readReference(read_info, reader, pair.form),
                            DW.AT.decl_file => {
                                if (variable.decl_coords == null) variable.decl_coords = .{ .file = 0, .line = 0, .column = 0 };
                                variable.decl_coords.?.file = try forms.readConstant(u32, read_info, reader, pair);
                            },
                            DW.AT.decl_line => {
                                if (variable.decl_coords == null) variable.decl_coords = .{ .file = 0, .line = 0, .column = 0 };
                                variable.decl_coords.?.line = try forms.readConstant(u32, read_info, reader, pair);
                            },
                            DW.AT.decl_column => {
                                if (variable.decl_coords == null) variable.decl_coords = .{ .file = 0, .line = 0, .column = 0 };
                                variable.decl_coords.?.column = try forms.readConstant(u32, read_info, reader, pair);
                            },
                            DW.AT.location => {
                                variable.loc = switch (pair.form) {
                                    DW.FORM.exprloc => .{ .expr = try forms.readExprLoc(reader, pair.form) },
                                    DW.FORM.sec_offset => .{ .loclist_offset = try readIs64(reader, self.is_64) },
                                    else => std.debug.panic("TODO: {s} for {s}\n", .{ DW.FORM.asStr(pair.form), DW.AT.asStr(pair.attrib) }),
                                };
                            },
                            else => try forms.skip(skip_info, reader, pair.form),
                        }
                    }
                    try variable_fixups.append(.{
                        .var_idx = variables.items.len,
                        .fn_idx = if (functions.items.len > 0) functions.items.len - 1 else null,
                        .type_offset = type_offset,
                    });
                    try variables.append(variable);
                },
                else => {
                    for (abbrev.attribs) |pair| try forms.skip(skip_info, reader, pair.form);
                },
            }
        }

        self.variables = variables.toOwnedSlice();
        self.functions = functions.toOwnedSlice();
        return FixUpInfo{
            .variable_fixups = variable_fixups.toOwnedSlice(),
            .function_fixups = function_fixups.toOwnedSlice(),
        };
    }

    const TypeFixUp = struct { type_idx: usize, base_type_offset: usize };
    const TypeFixUpInfos = struct { type_offsets: []usize, type_fixups: []TypeFixUp };

    fn loadTypes(self: *DebugUnit, debug_str: []const u8) !TypeFixUpInfos {
        var types = std.ArrayList(Type).init(self.allocator);
        var type_offsets = std.ArrayList(usize).init(self.allocator);

        var stream = std.io.fixedBufferStream(self.entries_buf);
        var reader = stream.reader();

        const skip_info = forms.SkipInfo{
            .address_size = self.address_size,
            .is_64 = self.is_64,
        };
        const read_info = forms.ReadInfo{
            .address_size = self.address_size,
            .is_64 = self.is_64,
            .version = self.version,
            .debug_info_opt = self.debug_info,
            .debug_str_opt = debug_str,
        };

        var parent_function_level: ?usize = null;
        var parent_function: ?usize = null;
        var child_level: usize = 0;
        while ((try stream.getPos()) < self.entries_buf.len) {
            const tag_offset = (try stream.getPos()) + self.header_size;
            const abbrev_code = try std.leb.readULEB128(usize, reader);
            const abbrev = self.abbrevs[abbrev_code];

            if (abbrev.has_children) child_level += 1;
            if (abbrev.tag == 0) {
                if (parent_function_level) |level| {
                    if (level == child_level) {
                        parent_function = null;
                        parent_function_level = null;
                    }
                }
                child_level -= 1;
                if (child_level == 0) break;
            }

            switch (abbrev.tag) {
                DW.TAG.base_type,
                DW.TAG.const_type,
                DW.TAG.pointer_type,
                DW.TAG.reference_type,
                DW.TAG.typedef,
                DW.TAG.array_type,
                DW.TAG.structure_type,
                DW.TAG.enumeration_type,
                => {
                    var name_opt: ?[]const u8 = null;
                    for (abbrev.attribs) |pair| {
                        switch (pair.attrib) {
                            DW.AT.name => name_opt = try forms.readString(read_info, reader, pair.form),
                            else => try forms.skip(skip_info, reader, pair.form),
                        }
                    }

                    try type_offsets.append(tag_offset);
                    try types.append(.{
                        .name = name_opt orelse "",
                        .data = undefined,
                    });
                },
                else => {
                    for (abbrev.attribs) |pair| try forms.skip(skip_info, reader, pair.form);
                },
            }
        }

        self.types = types.toOwnedSlice();
        return TypeFixUpInfos{ .type_offsets = type_offsets.toOwnedSlice(), .type_fixups = &[0]TypeFixUp{} };
    }
};

pub fn pathIntoSrc(self: Dwarf, path: []const u8) !?SrcLoc {
    var tmpbuf: [0x4000]u8 = undefined;
    for (self.line_progs) |prog| {
        for (prog.files) |file| {
            const dir = prog.include_dirs[file.dir];
            const test_path = try std.fmt.bufPrint(&tmpbuf, "{s}/{s}", .{ dir, file.name });
            if (std.mem.eql(u8, path, test_path)) {
                return SrcLoc{ .dir = dir, .file = file.name, .line = 0, .column = 0 };
            }
        }
    }
    return null;
}

// note: this mapping between DWARF register and machine registers is described here:
// https://refspecs.linuxbase.org/elf/x86_64-SysV-psABI.pdf, section 3.6.2, table 3.18
// zig fmt: off
pub const Register = enum {
    rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,               // 0-7
    r8, r9, r10, r11, r12, r13, r14, r15,                 // 8-15
    ret,                                                  // 16
    xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,       // 17-24
    xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15, // 25-32
    st0, st1, st2, st3, st4, st5, st6, st7,               // 33-40
    mm0, mm1, mm2, mm3, mm4, mm5, mm6, mm7,               // 41-48
};
// zig fmt: on

// caller owns returned memory
pub fn callStackAddrs(
    self: Dwarf,
    allocator: Allocator,
    starting_addr: usize,
    starting_regs: Registers,
    proc_mem: std.fs.File,
) ![]usize {
    var call_stack = std.ArrayList(usize).init(allocator);

    var addr = starting_addr;
    var regs = starting_regs;
    frame_loop: while (true) {
        try call_stack.append(addr);

        const frame = self.findFrameForAddr(addr) orelse break;
        const rules = try frame.rulesForAddr(addr);

        const cfa = switch (rules.cfa) {
            .@"undefined" => unreachable,
            .register => |rule| blk: {
                const reg_value = regs.getPtr(@intToEnum(Register, rule.reg)).*;
                break :blk @intCast(usize, @intCast(isize, reg_value) + rule.offset);
            },
            .expression => @panic("TODO: DWARF expression for CFA rule"),
        };

        const ret_addr = switch (rules.regs[frame.return_address_register]) {
            .@"undefined" => break :frame_loop,
            .offset => |offset| blk: {
                const reg_saved_addr = @intCast(usize, @intCast(isize, cfa) + offset);
                try proc_mem.seekTo(reg_saved_addr);
                break :blk try proc_mem.reader().readIntLittle(usize);
            },
            else => std.debug.panic("TODO: {s}\n", .{std.meta.activeTag(rules.regs[frame.return_address_register])}),
        };

        addr = ret_addr;
        for (rules.regs) |reg, i| {
            if (i == frame.return_address_register) continue;
            switch (reg) {
                .@"undefined" => {},
                .offset => |offset| {
                    const reg_saved_addr = @intCast(usize, @intCast(isize, cfa) + offset);
                    try proc_mem.seekTo(reg_saved_addr);
                    const reg_value = try proc_mem.reader().readIntLittle(usize);
                    regs.getPtr(@intToEnum(Register, i)).* = reg_value;
                },
                else => std.debug.panic("TODO: {s}\n", .{std.meta.activeTag(reg)}),
            }
        }
    }

    return call_stack.toOwnedSlice();
}

pub fn findFrameForAddr(self: Dwarf, addr: usize) ?Frame {
    if (self.frames.len == 0) return null;

    // because there are a *lot* of frames, even for small programs, we do a binary search
    // (we assume `frames` is sorted for this to work)
    if (addr >= self.frames[self.frames.len - 1].pc_end) return null;
    var search_size = self.frames.len / 2;
    var search_idx = search_size;
    while (search_size > 0) {
        const frame = self.frames[search_idx];
        if (frame.pc_begin <= addr and addr < frame.pc_end) return frame;

        search_size /= 2;
        if (addr < frame.pc_begin) {
            search_idx -= search_size;
        } else {
            search_idx += search_size;
        }
    }
    return null;
}

/// the returned data holds references into `debug_frame` and `eh_frame`
fn loadAllFrames(allocator: Allocator, debug_frame: []const u8, eh_frame: []const u8) ![]Frame {
    // https://refspecs.linuxfoundation.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/ehframechpt.html
    _ = eh_frame;
    if (debug_frame.len == 0) @panic("TODO: use `.eh_frame` when `.debug_frame` is not available\n");

    var frames = std.ArrayList(Frame).init(allocator);

    var stream = std.io.fixedBufferStream(debug_frame);
    var reader = stream.reader();

    while ((try stream.getPos()) < debug_frame.len) {
        const cie_offset = try stream.getPos();

        // CIE header
        const cie_length_field = try readLengthField(reader);
        const cie_id = try readIs64(reader, cie_length_field.is_64);
        const version = try reader.readByte();
        std.debug.assert(version == 4); // note: .debug_frame is the same in DWARFv4 and DWARFv5
        const augmentation = stringFromTable(debug_frame, try stream.getPos());
        try reader.skipBytes(augmentation.len + 1, .{});
        const address_size = try reader.readByte();
        const segment_selector_size = try reader.readByte();
        const code_alignment_factor = try std.leb.readULEB128(usize, reader);
        const data_alignment_factor = try std.leb.readILEB128(isize, reader);
        const return_address_register = try std.leb.readULEB128(usize, reader);

        const end_of_cie = cie_offset + cie_length_field.fullLength();
        const initial_instructions = debug_frame[try stream.getPos()..end_of_cie];
        try reader.skipBytes(initial_instructions.len, .{});

        fde_loop: while ((try stream.getPos()) < debug_frame.len) {
            const fde_offset = try stream.getPos();

            const address_is_64 = switch (address_size) {
                4 => false,
                8 => true,
                else => |size| std.debug.panic("address_size={}\n", .{size}),
            };

            // FDE header
            const length_field = try readLengthField(reader);
            const cie_pointer = try readIs64(reader, length_field.is_64);
            if (cie_pointer == cie_id) {
                try stream.seekTo(fde_offset);
                break :fde_loop;
            }
            std.debug.assert(cie_pointer == cie_offset);
            if (segment_selector_size > 0) try reader.skipBytes(segment_selector_size, .{});
            const initial_location = try readIs64(reader, address_is_64);
            const address_range = try readIs64(reader, address_is_64);

            const end_of_fde = fde_offset + length_field.fullLength();
            const instructions = debug_frame[try stream.getPos()..end_of_fde];
            try reader.skipBytes(instructions.len, .{});

            try frames.append(.{
                .is_64 = length_field.is_64,
                .address_size = address_size,
                .segment_selector_size = segment_selector_size,
                .code_alignment_factor = code_alignment_factor,
                .data_alignment_factor = data_alignment_factor,
                .return_address_register = return_address_register,
                .pc_begin = initial_location,
                .pc_end = initial_location + address_range,
                .initial_ops = initial_instructions,
                .ops = instructions,
            });
        }
    }

    // make sure the frames are sorted by address
    for (frames.items[1..]) |frame, i| {
        const last_frame = frames.items[i];
        std.debug.assert(frame.pc_begin >= last_frame.pc_end);
    }

    return frames.toOwnedSlice();
}

const LengthField = struct {
    length: u64,
    is_64: bool,
    pub fn fullLength(self: LengthField) usize {
        return if (self.is_64) 4 + 8 + self.length else 4 + self.length;
    }
};

pub fn readLengthField(reader: anytype) !LengthField {
    const initial_len = try reader.readIntLittle(u32);
    if (initial_len < 0xffff_fff0) {
        return LengthField{ .length = initial_len, .is_64 = false };
    } else {
        std.debug.assert(initial_len == 0xffff_ffff);
        return LengthField{ .length = try reader.readIntLittle(u64), .is_64 = true };
    }
}

/// some fields are 4 or 8 bytes depending on which DWARF format is used (32-bit/64-bit respectively)
pub fn readIs64(reader: anytype, is_64: bool) !usize {
    return if (is_64)
        @intCast(usize, try reader.readIntLittle(u64))
    else
        @intCast(usize, try reader.readIntLittle(u32));
}
