const std = @import("std");
const Allocator = std.mem.Allocator;
const stringFromTable = @import("Elf.zig").stringFromTable;

pub const forms = @import("dwarf/forms.zig");
pub const LineProg = @import("dwarf/LineProg.zig");
pub const Frame = @import("dwarf/Frame.zig");
pub const Expression = @import("dwarf/Expression.zig");
pub const DebugUnit = @import("dwarf/DebugUnit.zig");
pub const Function = DebugUnit.Function;
pub const Variable = DebugUnit.Variable;
pub const Attrib = DebugUnit.Attrib;
pub const DW = @import("dwarf/constants.zig");

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
    const timer_start = std.time.nanoTimestamp();

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

    //_ = timer_start;
    const timer_elapsed = std.time.nanoTimestamp() - timer_start;
    std.debug.print("Dwarf.initTables took {d:.2}ms\n", .{@intToFloat(f32, timer_elapsed) / std.time.ns_per_ms});
}

pub const SrcLoc = struct {
    dir: []const u8,
    file: []const u8,
    line: u32,
    column: u32,

    pub fn cmp(src_a: SrcLoc, src_b: SrcLoc) bool {
        if (src_a.column != src_b.column) return false;
        if (src_a.line != src_b.line) return false;
        if (!std.mem.eql(u8, src_a.file, src_b.file)) return false;
        if (!std.mem.eql(u8, src_a.dir, src_b.dir)) return false;
        return true;
    }

    pub fn format(value: SrcLoc, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("{{.dir={s}, .file={s}, .line={}, column={}}}", value);
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
pub const Register = enum(u8) {
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
    starting_regs: @import("Session.zig").Registers,
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
    if (debug_frame.len == 0) {
        //@panic("TODO: use `.eh_frame` when `.debug_frame` is not available\n");
        std.debug.print("TODO: use `.eh_frame` when `.debug_frame` is not available\n", .{});
        return &[0]Frame{};
    }

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
