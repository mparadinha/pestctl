const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");
const forms = @import("dwarf/forms.zig");
pub const LineProg = @import("dwarf/LineProg.zig");
pub const DW = @import("dwarf/constants.zig");

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

pub const SrcLoc = struct { dir: []const u8, file: []const u8, line: u32, column: u32 };

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

    debug_info: []const u8,

    /// this returns the amount of bytes we need to skip (starting from `offset`) to reach the
    /// end of the debug unit that starts at `offset`
    pub fn getSectionSize(debug_info: []const u8, offset: usize) !usize {
        var stream = std.io.fixedBufferStream(debug_info[offset..]);
        var reader = stream.reader();
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
        self.debug_info = debug_info;
        self.allocator = allocator;

        var stream = std.io.fixedBufferStream(debug_info[offset..]);
        var reader = stream.reader();

        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntLittle(u64);
        };
        self.is_64 = (initial_len == 0xffff_ffff);
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

        const entries_buf_start = offset + (try stream.getPos());
        const entries_buf_size = section_size - (try stream.getPos());
        self.entries_buf = debug_info[entries_buf_start .. entries_buf_start + entries_buf_size];

        // before Dwarf v5 the line program implicitly refers to the compilation directory
        // so we have to parse at least that much of the .debug_info section (just the first
        // entry) to make sure we can use LineProg correctly
        self.comp_dir = try self.getCompilationDir(debug_str);

        return self;
    }

    pub fn deinit(self: *DebugUnit) void {
        for (self.abbrevs) |*abbrev| abbrev.deinit(self.allocator);
        self.allocator.free(self.abbrevs);
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

    fn getCompilationDir(self: *DebugUnit, debug_str: []const u8) ![]const u8 {
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

        const abbrev_code = try std.leb.readULEB128(usize, reader);
        const abbrev = self.abbrevs[abbrev_code];
        std.debug.assert(abbrev.tag == DW.TAG.compile_unit);

        for (abbrev.attribs) |pair| {
            switch (pair.attrib) {
                DW.AT.comp_dir => return forms.readString(read_info, reader, pair.form),
                else => try forms.skip(pair.form, reader, skip_info),
            }
        }
        @panic("didn't find a comp_dir attribute");
    }

    //fn parseAllDebugInfo(self: *DebugUnit, debug_str: []const u8) !void {
    //    var stream = std.io.fixedBufferStream(self.entries_buf);
    //    var reader = stream.reader();

    //    const skip_info = forms.SkipInfo{ .address_size = self.address_size, .is_64 = self.is_64 };
    //    const read_info = forms.ReadInfo{
    //        .address_size = self.address_size,
    //        .is_64 = self.is_64,
    //        .version = self.version,
    //        .debug_info_opt = self.debug_info,
    //        .debug_str_opt = debug_str,
    //    };

    //    var child_level: usize = 0;
    //    while ((try stream.getPos()) < self.entries_buf.len) {
    //        const abbrev_code = try std.leb.readULEB128(usize, reader);
    //        const abbrev = self.abbrevs[abbrev_code];

    //        if (abbrev.has_children) child_level += 1;
    //        if (abbrev.tag == 0) {
    //            child_level -= 1;
    //            if (child_level == 0) break;
    //        }

    //        switch (abbrev.tag) {
    //            DW.TAG.compile_unit => try self.parseCompUnit(read_info, skip_info, reader, abbrev),

    //            //DW.TAG.base_type => {},
    //            //DW.TAG.unspecified_type => {},
    //            // type modifiers: these only really have a AT_name and AT_type
    //            //DW.TAG.atomic_type => {},
    //            //DW.TAG.const_type => {},
    //            //DW.TAG.immutable_type => {},
    //            //DW.TAG.packet_type => {},
    //            //DW.TAG.pointer_type => {},
    //            //DW.TAG.reference_type => {},
    //            //DW.TAG.restrict_type => {},
    //            //DW.TAG.rvalue_reference_type => {},
    //            //DW.TAG.shared_type => {},
    //            //DW.TAG.volatile_type => {},
    //            // typedefs always have a AT_name but might not have a AT_type
    //            //DW.TAG.typedef => {}
    //            // arrays can have a AT_name and might have a AT_ordering
    //            // might also have AT_byte/bit_stride
    //            // might also have AT_byte/bit_size
    //            // (they always have a AT_type, the element type)
    //            // if it is a multi-dim array it will have children with tags
    //            // TAG_subrange_type or TAG_enumeration_type
    //            // other things that array_type can have are AT_allocated, AT_associated
    //            // and AT_data_location
    //            //DW.TAG.array_type => {}
    //            //DW.TAG.coarray_type => {} // this is a fortran thing, ignore for now
    //            // struct, union, class
    //            // * might not have a AT_name (members are represented by children,
    //            // and come in the same order as in declared in the source file)
    //            // * not sure what AT_export_symbols does?
    //            // * might have AT_byte/bit_size (in which case its the size of the whole struct,
    //            // including padding)
    //            // * incomplete struct does not have byte size but has AT_declaration
    //            // * if complete struct decl is in a dif. unit theres going to be a an incomplete
    //            // one with a AT_signature
    //            // * if they have AT_specification they don't need to have AT_name
    //            // * these can have a AT_calling_convention for how they are passed in to functions
    //            //DW.TAG.structure_type => {},
    //            //DW.TAG.union_type => {},
    //            //DW.TAG.class_type => {},
    //            // interface types
    //            // * interfaces are basically classes with only abstract methods and const members
    //            // * they have a AT_name
    //            // * members are children
    //            //DW.TAG.interface_type => {},
    //            // extended types / inheritance
    //            //DW.TAG.inheritance => {},

    //            // theres some other types but I'm don't care right now
    //            //DW.TAG.access_declaration, DW.TAG.friend,

    //            // data members
    //            // * can have AT_mutable which is a flag
    //            // * can have AT_accessibility (if it doesn't C++ rules apply, private for classes
    //            //      public for structs, union, interface)
    //            // * can have AT_data_member_location or AT_data_bit_offset (if the beginning of the
    //            //      data member is the same as beginning of containing entity neither is required)
    //            //DW.TAG.data_member

    //            // function members use the TAG_subprogram tag

    //            // variables
    //            //DW.TAG.variable => try parse_ctx.parseVariable(reader, abbrev),

    //            //DW.TAG.formal_parameter => {
    //            //    for (abbrev.attribs) |pair| {
    //            //        if (pair.attrib == DW.AT.name) {
    //            //            std.debug.print("formal param with name: {s}\n", .{readString(read_info, reader, pair.form)});
    //            //        } else {
    //            //            try skipFORM(pair.form, reader, skip_info);
    //            //        }
    //            //    }
    //            //},
    //            //DW.TAG.constant => {
    //            //    for (abbrev.attribs) |pair| {
    //            //        if (pair.attrib == DW.AT.name) {
    //            //            std.debug.print("constant with name: {s}\n", .{readString(read_info, reader, pair.form)});
    //            //        } else {
    //            //            try skipFORM(pair.form, reader, skip_info);
    //            //        }
    //            //    }
    //            //},

    //            else => {
    //                std.debug.assert(DW.TAG.isValid(abbrev.tag));
    //                for (abbrev.attribs) |pair| try forms.skip(pair.form, reader, skip_info);
    //            },
    //        }
    //    }
    //}
};
