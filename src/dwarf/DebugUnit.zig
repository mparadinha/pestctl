const std = @import("std");
const Allocator = std.mem.Allocator;

const Dwarf = @import("../Dwarf.zig");
const forms = Dwarf.forms;
const LineProg = Dwarf.LineProg;
const Frame = Dwarf.Frame;
const Expression = Dwarf.Expression;
const DW = Dwarf.DW;
const SrcLoc = Dwarf.SrcLoc;
const readLengthField = Dwarf.readLengthField;
const readIs64 = Dwarf.readIs64;

const DebugUnit = @This();

allocator: Allocator,
version: u16,
unit_type: u8,
address_size: u8,
is_64: bool,
abbrev_offset: usize,

entries_buf: []const u8,
header_size: usize,

abbrevs: []Abbrev,

debug_info: []const u8,

arena: std.heap.ArenaAllocator,

comp_dir: []const u8,
functions: []Function,
variables: []Variable,
types: []Type,

pub const Abbrev = struct {
    tag: u16,
    has_children: bool,
    attribs: []Attrib,

    pub fn deinit(self: *Abbrev, allocator: Allocator) void {
        allocator.free(self.attribs);
    }
};

pub const Attrib = struct { attrib: u16, form: u16, implicit_value: i64 };

pub const DeclCoords = struct {
    file: u32,
    line: u32,
    column: u32,

    pub fn toSrcLoc(self: DeclCoords, line_prog: LineProg) SrcLoc {
        return .{
            .dir = line_prog.include_dirs[line_prog.files[self.file].dir],
            .file = line_prog.files[self.file].name,
            .line = self.line,
            .column = self.column,
        };
    }
};

pub const Function = struct {
    name: ?[]const u8,
    decl_coords: ?DeclCoords,
    @"extern": ?bool,
    low_pc: ?usize,
    high_pc: ?usize,
    ret_type: ?*Type,
    params: []*Variable,
};

pub const Variable = struct {
    name: ?[]const u8,
    decl_coords: ?DeclCoords,
    loc: ?LocationDesc,
    @"type": ?*Type,
    function: ?*Function,
};

pub const LocationDesc = union(enum) {
    expr: []const u8, // DWARF expression
    loclist_offset: usize,
    // TODO: DWARF loclistx
};

pub const Type = union(enum) {
    Base: struct {
        name: ?[]const u8,
        encoding: u16, // one of the values from DW_ATE
        endianess: Endianess,
        byte_size: usize,
        data_bit_offset: u16,
    },
    Unspecified: struct {
        name: ?[]const u8,
    },
    Atomic: TypeModifier,
    Const: TypeModifier,
    Immutable: TypeModifier,
    Packed: TypeModifier,
    Pointer: TypeModifier,
    Reference: TypeModifier,
    Restrict: TypeModifier,
    RValueRef: TypeModifier,
    Shared: TypeModifier,
    Volatile: TypeModifier,
    Typedef: NameAndType,
    Struct: Container,
    Class: Container,
    Union: Container,
    Enum: NameAndType, // TODO: better than this lol
    Array: NameAndType,
    PtrToMember: struct {
        name: ?[]const u8,
        @"type": ?*Type,
        parent_type: ?*Type,
    },

    pub const NameAndType = struct {
        name: ?[]const u8,
        @"type": ?*Type,
    };

    pub const Container = struct {
        name: ?[]const u8,
        // when a struct/class is only declared but never fully specified this is null
        size: ?usize,
        members: []ContainerMember,
    };

    pub const ContainerMember = struct {
        name: ?[]const u8,
        @"type": *Type,
        offset: usize,
    };

    pub const Endianess = enum { little, big };

    pub const TypeModifier = struct {
        name: ?[]const u8,
        // sometimes if the child_type is supposed to be `void` compilers might
        // emit an entry with no child type
        child_type: ?*Type,
    };
};

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
    _ = debug_loc;
    _ = debug_loclists;

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

    const entries_buf_start = offset + self.header_size;
    const entries_buf_size = section_size - self.header_size;
    self.entries_buf = debug_info[entries_buf_start .. entries_buf_start + entries_buf_size];

    // before Dwarf v5 the line program implicitly refers to the compilation directory
    // so we have to parse at least that much of the .debug_info section (just the first
    // entry) to make sure we can use LineProg correctly
    self.comp_dir = try self.getCompilationDir(debug_str, debug_line_str);

    self.arena = std.heap.ArenaAllocator.init(self.allocator);

    const type_offsets = try self.loadAllTypes(debug_str);
    defer self.allocator.free(type_offsets);
    const fn_type_fixups = try self.loadAllFunctions(debug_str);
    defer self.allocator.free(fn_type_fixups);
    const var_fixups = try self.loadAllVariables(debug_str);
    const var_type_fixups = var_fixups.type_fixups;
    defer self.allocator.free(var_type_fixups);
    const var_fn_fixups = var_fixups.var_fn_fixups;
    defer self.allocator.free(var_fn_fixups);
    const param_fixups = var_fixups.param_fixups;
    defer self.allocator.free(param_fixups);

    for (fn_type_fixups) |fixup| {
        const func = &self.functions[fixup.idx];
        const type_idx = searchForOffset(fixup.offset, type_offsets) orelse
            std.debug.panic("type (@ 0x{x}) for func #{} ({s}) not parsed\n", .{ fixup.offset + offset, fixup.idx, func.name });
        func.ret_type = &self.types[type_idx];
    }
    for (var_type_fixups) |fixup| {
        const v = &self.variables[fixup.idx];
        const type_idx = searchForOffset(fixup.offset, type_offsets) orelse
            std.debug.panic("type (@ 0x{x}) for var #{} ({s}) not parsed\n", .{ fixup.offset + offset, fixup.idx, v.name });
        v.@"type" = &self.types[type_idx];
    }
    for (var_fn_fixups) |fixup| {
        const func_ptr = &self.functions[fixup.func_idx];
        self.variables[fixup.var_idx].function = func_ptr;
    }
    for (param_fixups) |fixup| {
        const var_ptr = &self.variables[fixup.var_idx];
        const func_ptr = &self.functions[fixup.func_idx];
        var_ptr.function = func_ptr;
        func_ptr.params = try reallocAndAppend(*Variable, self.allocator, func_ptr.params, var_ptr);
    }
    for (self.functions) |*func| {
        func.params = try transferToNewAllocator(
            *Variable,
            func.params,
            self.allocator,
            self.arena.allocator(),
        );
    }

    return self;
}

pub fn deinit(self: *DebugUnit) void {
    for (self.abbrevs) |*abbrev| abbrev.deinit(self.allocator);
    self.allocator.free(self.abbrevs);
    self.allocator.free(self.functions);
    self.allocator.free(self.variables);
    self.allocator.free(self.types);
    self.arena.deinit();
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

fn loadAllTypes(self: *DebugUnit, debug_str: []const u8) ![]usize {
    var types = std.ArrayList(Type).init(self.allocator);
    var type_offset_list = std.ArrayList(usize).init(self.allocator);

    var offset_fixups = std.ArrayList(struct {
        idx: usize,
        member_idx: ?usize = null,
        offset: usize,
        is_parent_type: bool = false, // used for PtrToMember fixups
    }).init(self.allocator);
    defer offset_fixups.deinit();

    // index into `types`
    var container_stack = std.ArrayList(usize).init(self.allocator);
    defer container_stack.deinit();

    var read_ctx = EntryReadCtx.init(
        self.allocator,
        self.entries_buf,
        self.abbrevs,
        self.address_size,
        self.is_64,
        self.version,
        self.debug_info,
        debug_str,
    );
    defer read_ctx.deinit();
    while (try read_ctx.nextEntry()) |entry| {
        if (entry == .close_tag) {
            switch (entry.close_tag) {
                DW.TAG.structure_type,
                DW.TAG.class_type,
                DW.TAG.union_type,
                => {
                    const ty_idx = container_stack.pop();
                    const member_list_ptr = switch (types.items[ty_idx]) {
                        .Struct => |*info| &info.members,
                        .Class => |*info| &info.members,
                        .Union => |*info| &info.members,
                        else => unreachable,
                    };
                    member_list_ptr.* = try transferToNewAllocator(
                        Type.ContainerMember,
                        member_list_ptr.*,
                        self.allocator,
                        self.arena.allocator(),
                    );
                },
                else => {},
            }
            continue;
        }

        const abbrev = entry.abbrev;
        const tag_offset = read_ctx.last_tag_offset + self.header_size;
        var added_type = true;
        switch (abbrev.tag) {
            DW.TAG.base_type => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.encoding, .@"type" = u16 },
                    .{ .attrib = DW.AT.endianity, .@"type" = u16 },
                    .{ .attrib = DW.AT.byte_size, .@"type" = usize },
                    .{ .attrib = DW.AT.bit_size, .@"type" = usize },
                    .{ .attrib = DW.AT.data_bit_offset, .@"type" = u16 },
                });
                try types.append(.{ .Base = .{
                    .name = attribs.name,
                    .encoding = attribs.encoding orelse
                        std.debug.panic("base_type (0x{x}) missing encoding\n", .{tag_offset}),
                    .endianess = if (attribs.endianity) |value| switch (value) {
                        DW.END.default => Type.Endianess.little,
                        DW.END.big => Type.Endianess.big,
                        DW.END.little => Type.Endianess.little,
                        else => std.debug.panic("invalid DW_END 0x{x}\n", .{value}),
                    } else .little,
                    .byte_size = if (attribs.byte_size) |size|
                        size
                    else if (attribs.bit_size) |size|
                        @divExact(size, 8)
                    else
                        std.debug.panic("base_type (0x{x}) missing byte/bit size\n", .{tag_offset}),
                    .data_bit_offset = attribs.data_bit_offset orelse 0,
                } });
            },
            DW.TAG.unspecified_type => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                });
                try types.append(.{ .Unspecified = .{
                    .name = attribs.name,
                } });
            },
            DW.TAG.atomic_type,
            DW.TAG.const_type,
            DW.TAG.immutable_type,
            DW.TAG.packed_type,
            DW.TAG.pointer_type,
            DW.TAG.reference_type,
            DW.TAG.restrict_type,
            DW.TAG.rvalue_reference_type,
            DW.TAG.shared_type,
            DW.TAG.volatile_type,
            => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                });
                if (attribs.@"type") |offset| {
                    try offset_fixups.append(.{
                        .idx = type_offset_list.items.len,
                        .offset = offset,
                    });
                }
                const type_mod = Type.TypeModifier{ .name = attribs.name, .child_type = null };
                try types.append(switch (abbrev.tag) {
                    DW.TAG.atomic_type => .{ .Atomic = type_mod },
                    DW.TAG.const_type => .{ .Const = type_mod },
                    DW.TAG.immutable_type => .{ .Immutable = type_mod },
                    DW.TAG.packed_type => .{ .Packed = type_mod },
                    DW.TAG.pointer_type => .{ .Pointer = type_mod },
                    DW.TAG.reference_type => .{ .Reference = type_mod },
                    DW.TAG.restrict_type => .{ .Restrict = type_mod },
                    DW.TAG.rvalue_reference_type => .{ .RValueRef = type_mod },
                    DW.TAG.shared_type => .{ .Shared = type_mod },
                    DW.TAG.volatile_type => .{ .Volatile = type_mod },
                    else => unreachable,
                });
            },
            DW.TAG.typedef => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                });
                if (attribs.@"type") |offset| {
                    try offset_fixups.append(.{
                        .idx = type_offset_list.items.len,
                        .offset = offset,
                    });
                }
                try types.append(.{ .Typedef = .{
                    .name = attribs.name,
                    .@"type" = null,
                } });
            },
            DW.TAG.structure_type,
            DW.TAG.class_type,
            DW.TAG.union_type,
            => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.byte_size, .@"type" = usize },
                    .{ .attrib = DW.AT.specification, .@"type" = usize },
                });
                if (attribs.specification) |_| std.debug.panic("TODO: DW_AT_specification\n", .{});
                if (abbrev.has_children) try container_stack.append(types.items.len);
                const container = Type.Container{
                    .name = attribs.name,
                    .size = attribs.byte_size,
                    .members = &[0]Type.ContainerMember{},
                };
                switch (abbrev.tag) {
                    DW.TAG.structure_type => try types.append(.{ .Struct = container }),
                    DW.TAG.class_type => try types.append(.{ .Class = container }),
                    DW.TAG.union_type => try types.append(.{ .Union = container }),
                    else => unreachable,
                }
            },
            DW.TAG.member => {
                added_type = false;

                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                    .{ .attrib = DW.AT.data_member_location, .@"type" = usize },
                    // TODO: DW_AT_byte_size for struct members (for packed structs maybe?)
                });
                const member = Type.ContainerMember{
                    .name = attribs.name,
                    .@"type" = undefined,
                    .offset = attribs.data_member_location orelse 0,
                };

                const ty_idx = if (container_stack.items.len > 0) blk: {
                    const idx = container_stack.items[container_stack.items.len - 1];
                    if (types.items[idx] != .Struct and types.items[idx] != .Class and types.items[idx] != .Union)
                        std.debug.panic("member @ 0x{x} with invalid parent container\n", .{tag_offset});
                    break :blk idx;
                } else std.debug.panic("member @ 0x{x} with no parent container\n", .{tag_offset});

                const member_list_ptr = switch (types.items[ty_idx]) {
                    .Struct => |*info| &info.members,
                    .Class => |*info| &info.members,
                    .Union => |*info| &info.members,
                    else => unreachable,
                };

                try offset_fixups.append(.{
                    .idx = ty_idx,
                    .member_idx = member_list_ptr.len,
                    .offset = attribs.@"type" orelse
                        std.debug.panic("struct_member @ 0x{x} doesn't have a type", .{tag_offset}),
                });
                member_list_ptr.* = try reallocAndAppend(Type.ContainerMember, self.allocator, member_list_ptr.*, member);
            },
            DW.TAG.ptr_to_member_type => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                    .{ .attrib = DW.AT.containing_type, .@"type" = usize },
                    .{ .attrib = DW.AT.use_location, .@"type" = LocationDesc },
                });
                if (attribs.@"type") |offset| {
                    try offset_fixups.append(.{
                        .idx = type_offset_list.items.len,
                        .offset = offset,
                    });
                }
                if (attribs.containing_type) |offset| {
                    try offset_fixups.append(.{
                        .idx = type_offset_list.items.len,
                        .offset = offset,
                        .is_parent_type = true,
                    });
                }
                try types.append(.{ .PtrToMember = .{
                    .name = attribs.name,
                    .@"type" = null,
                    .parent_type = null,
                } });
            },
            DW.TAG.enumeration_type => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                });
                if (attribs.@"type") |offset| {
                    try offset_fixups.append(.{
                        .idx = type_offset_list.items.len,
                        .offset = offset,
                    });
                }
                try types.append(.{ .Enum = .{
                    .name = attribs.name,
                    .@"type" = null,
                } });
            },
            DW.TAG.array_type => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                });
                if (attribs.@"type") |offset| {
                    try offset_fixups.append(.{
                        .idx = type_offset_list.items.len,
                        .offset = offset,
                    });
                }
                try types.append(.{ .Array = .{
                    .name = attribs.name,
                    .@"type" = null,
                } });
            },
            else => {
                added_type = false;
                try read_ctx.skipTag(abbrev);
            },
        }
        if (added_type) try type_offset_list.append(tag_offset);
    }

    // fixup all the inter-type references before returning
    const type_offsets = type_offset_list.toOwnedSlice();
    self.types = types.toOwnedSlice();
    for (offset_fixups.items) |fixup| {
        const type_idx = searchForOffset(fixup.offset, type_offsets) orelse {
            //std.debug.panic("fixup={{.idx={}, .offset=0x{x}}}\n", .{fixup.idx, fixup.offset});
            //@debug
            std.debug.print("ignoring fixup={{.idx={}, .offset=0x{x}}}\n", .{ fixup.idx, fixup.offset });
            continue;
        };
        const referenced_type = &self.types[type_idx];

        const type_info = &self.types[fixup.idx];
        if (fixup.member_idx) |_|
            std.debug.assert(type_info.* == .Struct or type_info.* == .Class or type_info.* == .Union);

        switch (type_info.*) {
            .Base, .Unspecified => {
                std.debug.panic("idx={}, offset=0x{x}\n", .{ fixup.idx, fixup.offset });
            },
            .Atomic,
            .Const,
            .Immutable,
            .Packed,
            .Pointer,
            .Reference,
            .Restrict,
            .RValueRef,
            .Shared,
            .Volatile,
            => |*info| {
                info.child_type = referenced_type;
            },
            .Typedef, .Enum, .Array => |*info| {
                info.@"type" = referenced_type;
            },
            .Struct, .Class, .Union => |*info| {
                const member_idx = fixup.member_idx orelse unreachable;
                info.members[member_idx].@"type" = referenced_type;
            },
            .PtrToMember => |*info| {
                const type_ptr = if (fixup.is_parent_type) &info.parent_type else &info.@"type";
                type_ptr.* = referenced_type;
            },
        }
    }

    return type_offsets;
}

const TypePtrFixup = struct { idx: usize, offset: usize };

fn loadAllFunctions(self: *DebugUnit, debug_str: []const u8) ![]TypePtrFixup {
    var funcs = std.ArrayList(Function).init(self.allocator);
    var fixups = std.ArrayList(TypePtrFixup).init(self.allocator);

    var read_ctx = EntryReadCtx.init(
        self.allocator,
        self.entries_buf,
        self.abbrevs,
        self.address_size,
        self.is_64,
        self.version,
        self.debug_info,
        debug_str,
    );
    defer read_ctx.deinit();
    while (try read_ctx.nextAbbrev()) |abbrev| {
        switch (abbrev.tag) {
            DW.TAG.subprogram => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.decl_file, .@"type" = u32 },
                    .{ .attrib = DW.AT.decl_line, .@"type" = u32 },
                    .{ .attrib = DW.AT.decl_column, .@"type" = u32 },
                    .{ .attrib = DW.AT.external, .@"type" = bool },
                    .{ .attrib = DW.AT.low_pc, .@"type" = usize },
                    .{ .attrib = DW.AT.high_pc, .@"type" = usize },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                });
                if (attribs.@"type") |offset| {
                    try fixups.append(.{ .idx = funcs.items.len, .offset = offset });
                }
                try funcs.append(.{
                    .name = attribs.name,
                    .decl_coords = maybeDeclCoords(attribs.decl_file, attribs.decl_line, attribs.decl_column),
                    .@"extern" = attribs.external,
                    .low_pc = attribs.low_pc,
                    .high_pc = if (attribs.high_pc) |high_pc| switch (DW.Class.fromForm(attribs.forms_used.high_pc.?)) {
                        .address => high_pc,
                        .constant => attribs.low_pc.? + high_pc,
                        else => unreachable,
                    } else null,
                    .ret_type = if (attribs.@"type") |_| undefined else null,
                    .params = &[0]*Variable{},
                });
            },
            else => try read_ctx.skipTag(abbrev),
        }
    }

    self.functions = funcs.toOwnedSlice();
    return fixups.toOwnedSlice();
}

const VarPtrFixup = struct { func_idx: usize, var_idx: usize };
const VarFixups = struct {
    type_fixups: []TypePtrFixup,
    var_fn_fixups: []VarPtrFixup,
    param_fixups: []VarPtrFixup,
};

// note: this assumes all the function info has already been loaded
fn loadAllVariables(self: *DebugUnit, debug_str: []const u8) !VarFixups {
    var vars = std.ArrayList(Variable).init(self.allocator);
    var type_fixups = std.ArrayList(TypePtrFixup).init(self.allocator);
    var var_fn_fixups = std.ArrayList(VarPtrFixup).init(self.allocator);
    var param_fixups = std.ArrayList(VarPtrFixup).init(self.allocator);

    var read_ctx = EntryReadCtx.init(
        self.allocator,
        self.entries_buf,
        self.abbrevs,
        self.address_size,
        self.is_64,
        self.version,
        self.debug_info,
        debug_str,
    );
    defer read_ctx.deinit();
    var func_stack = std.ArrayList(u16).init(self.allocator);
    defer func_stack.deinit();
    var subprogram_count: usize = 0;
    while (try read_ctx.nextEntry()) |entry| {
        if (std.meta.activeTag(entry) == .close_tag) {
            switch (entry.close_tag) {
                DW.TAG.subprogram, DW.TAG.inlined_subroutine, DW.TAG.subroutine_type => {
                    std.debug.assert(func_stack.pop() == entry.close_tag);
                },
                else => {},
            }
            continue;
        }

        const abbrev = entry.abbrev;
        switch (abbrev.tag) {
            DW.TAG.subprogram, DW.TAG.inlined_subroutine, DW.TAG.subroutine_type => {
                if (abbrev.tag == DW.TAG.subprogram) subprogram_count += 1;
                if (abbrev.has_children) try func_stack.append(abbrev.tag);
                try read_ctx.skipTag(abbrev);
            },
            DW.TAG.variable => {
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.decl_file, .@"type" = u32 },
                    .{ .attrib = DW.AT.decl_line, .@"type" = u32 },
                    .{ .attrib = DW.AT.decl_column, .@"type" = u32 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                    .{ .attrib = DW.AT.location, .@"type" = LocationDesc },
                });
                if (attribs.@"type") |offset| {
                    try type_fixups.append(.{ .idx = vars.items.len, .offset = offset });
                }
                if (std.mem.lastIndexOfScalar(u16, func_stack.items, DW.TAG.subprogram)) |_| {
                    try var_fn_fixups.append(.{
                        .func_idx = if (subprogram_count > 0)
                            subprogram_count - 1
                        else
                            std.debug.panic("0x{x}\n", .{read_ctx.last_tag_offset}),
                        .var_idx = vars.items.len,
                    });
                }
                try vars.append(.{
                    .name = attribs.name,
                    .decl_coords = maybeDeclCoords(attribs.decl_file, attribs.decl_line, attribs.decl_column),
                    .loc = attribs.location,
                    .@"type" = null,
                    .function = null,
                });
            },
            DW.TAG.formal_parameter => {
                if (func_stack.items.len == 0) {
                    try read_ctx.skipTag(abbrev);
                    continue;
                }
                // TODO: formal_parameters for inlined_subroutine and subroutine_type
                if (func_stack.items[func_stack.items.len - 1] != DW.TAG.subprogram) {
                    try read_ctx.skipTag(abbrev);
                    continue;
                }
                const attribs = try genericReadAttribs(&read_ctx, abbrev, &[_]FieldInfo{
                    .{ .attrib = DW.AT.name, .@"type" = []const u8 },
                    .{ .attrib = DW.AT.decl_file, .@"type" = u32 },
                    .{ .attrib = DW.AT.decl_line, .@"type" = u32 },
                    .{ .attrib = DW.AT.decl_column, .@"type" = u32 },
                    .{ .attrib = DW.AT.@"type", .@"type" = usize },
                    .{ .attrib = DW.AT.location, .@"type" = LocationDesc },
                });
                if (attribs.@"type") |offset| {
                    try type_fixups.append(.{ .idx = vars.items.len, .offset = offset });
                }
                try param_fixups.append(.{
                    .func_idx = if (subprogram_count > 0)
                        subprogram_count - 1
                    else
                        std.debug.panic("0x{x}\n", .{read_ctx.last_tag_offset}),
                    .var_idx = vars.items.len,
                });
                try vars.append(.{
                    .name = attribs.name,
                    .decl_coords = maybeDeclCoords(attribs.decl_file, attribs.decl_line, attribs.decl_column),
                    .loc = attribs.location,
                    .@"type" = null,
                    .function = null,
                });
            },
            DW.TAG.constant => {
                // TODO
                try read_ctx.skipTag(abbrev);
            },
            else => try read_ctx.skipTag(abbrev),
        }
    }

    self.variables = vars.toOwnedSlice();
    return VarFixups{
        .type_fixups = type_fixups.toOwnedSlice(),
        .var_fn_fixups = var_fn_fixups.toOwnedSlice(),
        .param_fixups = param_fixups.toOwnedSlice(),
    };
}

fn maybeDeclCoords(file: ?u32, line: ?u32, col: ?u32) ?DeclCoords {
    if (file == null or line == null) return null;
    return DeclCoords{ .file = file.?, .line = line.?, .column = col orelse 0 };
}

// the `old_buf` passed in is deallocated here
fn reallocAndAppend(comptime T: type, allocator: Allocator, old_buf: []T, item: T) ![]T {
    var new_buf = try allocator.realloc(old_buf, old_buf.len + 1);
    new_buf[old_buf.len] = item;
    return new_buf;
}

// the memory passed in is deallocated here
fn transferToNewAllocator(comptime T: type, mem: []T, old_alloc: Allocator, new_alloc: Allocator) ![]T {
    const new_mem = new_alloc.dupe(T, mem);
    old_alloc.free(mem);
    return new_mem;
}

// note: assumes `offsets` is sorted from lowest to highest
fn searchForOffset(target: usize, offsets: []const usize) ?usize {
    const Ctx = struct {
        pub fn cmp(ctx: @This(), lhs: usize, rhs: usize) std.math.Order {
            _ = ctx;
            if (lhs < rhs) return .lt;
            if (lhs > rhs) return .gt;
            return .eq;
        }
    };
    return std.sort.binarySearch(usize, target, offsets, Ctx{}, Ctx.cmp);
}

const VariableFixUp = struct { var_idx: usize, fn_idx: ?usize, type_offset: ?usize };
const FunctionFixUp = struct { fn_idx: usize, type_offset: usize };
const FixUpInfo = struct { variable_fixups: []VariableFixUp, function_fixups: []FunctionFixUp };

fn loadFunctions(self: *DebugUnit, debug_str: []const u8) !FixUpInfo {
    var variables = std.ArrayList(Variable).init(self.allocator);
    var variable_fixups = std.ArrayList(VariableFixUp).init(self.allocator);
    var functions = std.ArrayList(Function).init(self.allocator);
    var function_fixups = std.ArrayList(FunctionFixUp).init(self.allocator);

    var read_ctx = EntryReadCtx.init(
        self.allocator,
        self.entries_buf,
        self.abbrevs,
        self.address_size,
        self.is_64,
        self.version,
        self.debug_info,
        debug_str,
    );
    defer read_ctx.deinit();
    while (try read_ctx.nextAbbrev()) |abbrev| {
        const skip_info = read_ctx.skip_info;
        const read_info = read_ctx.read_info;
        const reader = read_ctx.stream.reader();

        switch (abbrev.tag) {
            DW.TAG.subprogram => {
                var function = Function{
                    .name = null,
                    .decl_coords = null,
                    .@"extern" = null,
                    .low_pc = null,
                    .high_pc = null,
                    .ret_type = null,
                    .params = &[0]Variable{},
                };

                for (abbrev.attribs) |pair| {
                    switch (pair.attrib) {
                        DW.AT.name => function.name = try read_ctx.readAttrib(.string, pair),
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

                try functions.append(function);
            },
            // TODO
            //DW.TAG.formal_parameter => {
            //    if (parent_function != null and parent_function_level.? == child_level) {
            //        var name_opt: ?[]const u8 = null;

            //        for (abbrev.attribs) |pair| {
            //            switch (pair.attrib) {
            //                DW.AT.name => name_opt = try forms.readString(read_info, reader, pair.form),
            //                else => try forms.skip(skip_info, reader, pair.form),
            //            }
            //        }

            //        //std.debug.print("  param: {s}\n", .{
            //        //    name_opt,
            //        //});
            //    } else {
            //        for (abbrev.attribs) |pair| try forms.skip(skip_info, reader, pair.form);
            //    }
            //},
            DW.TAG.variable => {
                var variable = Variable{
                    .name = &[0]u8{},
                    .decl_coords = null,
                    .loc = null,
                    .@"type" = null,
                    .scope = null,
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

const EntryReadCtx = struct {
    allocator: Allocator,
    skip_info: forms.SkipInfo,
    read_info: forms.ReadInfo,
    abbrevs: []const Abbrev,

    stream: std.io.FixedBufferStream([]const u8),
    last_tag_offset: usize,
    tag_hierarchy: std.ArrayList(u16),

    pub fn init(
        allocator: Allocator,
        buf: []const u8,
        abbrevs: []const Abbrev,
        address_size: u8,
        is_64: bool,
        version: u16,
        debug_info_opt: ?[]const u8,
        debug_str_opt: ?[]const u8,
    ) EntryReadCtx {
        return .{
            .allocator = allocator,
            .skip_info = .{
                .address_size = address_size,
                .is_64 = is_64,
            },
            .read_info = .{
                .address_size = address_size,
                .is_64 = is_64,
                .version = version,
                .debug_info_opt = debug_info_opt,
                .debug_str_opt = debug_str_opt,
            },
            .abbrevs = abbrevs,

            .stream = std.io.fixedBufferStream(buf),
            .last_tag_offset = 0,
            .tag_hierarchy = std.ArrayList(u16).init(allocator),
        };
    }

    pub fn deinit(self: EntryReadCtx) void {
        self.tag_hierarchy.deinit();
    }

    pub const Entry = union(enum) {
        abbrev: Abbrev,
        close_tag: u16, // tag which is getting closed
    };
    pub fn nextEntry(self: *EntryReadCtx) anyerror!?Entry {
        if (self.stream.pos >= self.stream.buffer.len) return null;
        const reader = self.stream.reader();

        self.last_tag_offset = self.stream.pos;
        const abbrev_code = try std.leb.readULEB128(usize, reader);
        const abbrev = self.abbrevs[abbrev_code];

        if (abbrev.tag == 0) {
            const close_tag = self.tag_hierarchy.pop();
            return Entry{ .close_tag = close_tag };
        }
        if (abbrev.has_children) {
            try self.tag_hierarchy.append(abbrev.tag);
        }

        return Entry{ .abbrev = abbrev };
    }

    pub fn nextAbbrev(self: *EntryReadCtx) anyerror!?Abbrev {
        if (self.stream.pos >= self.stream.buffer.len) return null;
        const reader = self.stream.reader();

        self.last_tag_offset = self.stream.pos;
        const abbrev_code = try std.leb.readULEB128(usize, reader);
        const abbrev = self.abbrevs[abbrev_code];

        if (abbrev.tag == 0) {
            _ = self.tag_hierarchy.pop();
            if (self.tag_hierarchy.items.len == 0) return null;
            return self.nextAbbrev();
        }
        if (abbrev.has_children) {
            try self.tag_hierarchy.append(abbrev.tag);
        }

        return @as(?Abbrev, abbrev);
    }

    pub fn topTag(self: EntryReadCtx) u16 {
        const tag_stack = self.tag_hierarchy.items;
        std.debug.assert(tag_stack.len > 0);
        return tag_stack[tag_stack.len - 1];
    }

    pub fn skipTag(self: *EntryReadCtx, tag_abbrev: Abbrev) !void {
        for (tag_abbrev.attribs) |attrib| try self.skipAttrib(attrib);
    }

    pub fn skipAttrib(self: *EntryReadCtx, attrib: Attrib) !void {
        try forms.skip(self.skip_info, self.stream.reader(), attrib.form);
    }

    pub fn readAttrib(
        self: *EntryReadCtx,
        comptime class: DW.Class,
        attrib: Attrib,
    ) !DW.Class.Type(class) {
        const reader = self.stream.reader();
        const form = attrib.form;
        switch (class) {
            .address => return forms.readAddress(self.read_info, reader, form),
            .addrptr => @panic("TODO"),
            .block => return forms.readBlock(self.read_info, form),
            .constant => @compileError("use `readConstant` instead"),
            .exprloc => return forms.readExprLoc(reader, form),
            .flag => return forms.readFlag(self.read_info, reader, form),
            .lineptr => return forms.readLinePtr(self.read_info, reader, form),
            .loclist => @panic("TODO"),
            .loclistsptr => @panic("TODO"),
            .macptr => @panic("TODO"),
            .rnglist => @panic("TODO"),
            .rnglistsptr => @panic("TODO"),
            .reference => return forms.readReference(self.read_info, reader, form),
            .string => return forms.readString(self.read_info, reader, form),
            .stroffsetsptr => @panic("TODO"),
        }
    }

    pub fn readConstant(
        self: *EntryReadCtx,
        comptime ConstType: type,
        attrib: Attrib,
    ) !ConstType {
        const reader = self.stream.reader();
        return forms.readConstant(ConstType, self.read_info, reader, attrib);
    }

    pub fn readSecOffset(self: *EntryReadCtx) !usize {
        const reader = self.stream.reader();
        return readIs64(reader, self.read_info.is_64);
    }
};

const FieldInfo = struct { attrib: u16, @"type": type };

fn genericReadAttribs(
    read_ctx: *EntryReadCtx,
    abbrev: Abbrev,
    comptime field_infos: []const FieldInfo,
) !StructFromFieldInfos(field_infos) {
    var read = std.mem.zeroes(StructFromFieldInfos(field_infos));

    for (abbrev.attribs) |attrib| {
        inline for (field_infos) |info| {
            const field_name = comptime DW.dwarfShortString(DW.AT, info.attrib);
            const possible_classes = comptime DW.Class.fromAttribute(info.attrib);
            if (attrib.attrib == info.attrib) {
                inline for (possible_classes) |class| {
                    if (attrib.form == DW.FORM.exprloc or attrib.form == DW.FORM.sec_offset) {
                        if (class == .exprloc and info.@"type" == LocationDesc) {
                            @field(read, field_name) = switch (attrib.form) {
                                DW.FORM.exprloc => .{ .expr = try read_ctx.readAttrib(.exprloc, attrib) },
                                DW.FORM.sec_offset => .{ .loclist_offset = try read_ctx.readSecOffset() },
                                else => unreachable,
                            };
                            @field(read.forms_used, field_name) = attrib.form;
                            break;
                        }
                    }

                    const form_class = DW.Class.fromForm(attrib.form);
                    if (class == form_class) {
                        if (class == .constant) {
                            @field(read, field_name) = try read_ctx.readConstant(info.@"type", attrib);
                            @field(read.forms_used, field_name) = attrib.form;
                            break;
                        } else if (DW.Class.Type(class) == info.@"type") {
                            @field(read, field_name) = try read_ctx.readAttrib(class, attrib);
                            @field(read.forms_used, field_name) = attrib.form;
                            break;
                        }
                    }
                } else std.debug.panic("stream.pos=0x{x}: {s} doesn't support {} (from {s})\n", .{
                    read_ctx.stream.pos, DW.AT.asStr(info.attrib), DW.Class.fromForm(attrib.form), DW.FORM.asStr(attrib.form),
                });
                break;
            }
        } else try read_ctx.skipAttrib(attrib);
    }

    return read;
}

/// construct a new Struct type with one member for each of the attribs in `field_infos`
/// the type of each member is an optional (i.e. u32 -> ?u32)
/// also includes a `forms_used` fields which is another, inner, struct that contains a ?u16
/// (a DW_FORM_xxx) for each field in `field_infos`
/// so `FieldInfo{.attrib = DW.AT.name, .@"type" = []const u8 }` becomes:
/// struct {
///     name: ?[]const u8,
///     forms_used: struct {
///         name: ?u16,
///     },
/// };
fn StructFromFieldInfos(comptime field_infos: []const FieldInfo) type {
    var inner_fields: [field_infos.len]std.builtin.TypeInfo.StructField = undefined;
    inline for (field_infos) |info, idx| {
        const field_name = DW.dwarfShortString(DW.AT, info.attrib);
        inner_fields[idx] = .{
            .name = field_name,
            .field_type = ?u16,
            .default_value = @as(?u16, null),
            .is_comptime = false,
            .alignment = 0,
        };
    }
    const inner_constructed_type = @Type(std.builtin.TypeInfo{ .Struct = .{
        .layout = .Auto,
        .fields = &inner_fields,
        .decls = &[0]std.builtin.TypeInfo.Declaration{},
        .is_tuple = false,
    } });

    var fields: [field_infos.len + 1]std.builtin.TypeInfo.StructField = undefined;
    inline for (field_infos) |info, idx| {
        const field_name = DW.dwarfShortString(DW.AT, info.attrib);
        fields[idx] = .{
            .name = field_name,
            .field_type = ?info.@"type",
            .default_value = @as(?info.@"type", null),
            .is_comptime = false,
            .alignment = 0,
        };
    }
    fields[fields.len - 1] = .{
        .name = "forms_used",
        .field_type = inner_constructed_type,
        .default_value = @as(??inner_constructed_type, null),
        .is_comptime = false,
        .alignment = 0,
    };

    const constructed = std.builtin.TypeInfo{ .Struct = .{
        .layout = .Auto,
        .fields = &fields,
        .decls = &[0]std.builtin.TypeInfo.Declaration{},
        .is_tuple = false,
    } };
    return @Type(constructed);
}
