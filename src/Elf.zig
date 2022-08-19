const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");
const elf = std.elf;
const Dwarf = @import("Dwarf.zig");

allocator: Allocator,
sections: []Section,
string_section: []const u8,
dwarf: Dwarf,

const Elf = @This();

pub fn init(allocator: Allocator, exec_path: []const u8) !Elf {
    var self = @as(Elf, undefined);
    self.allocator = allocator;
    self.dwarf = Dwarf{
        .allocator = self.allocator,
        .debug_info = "",
        .debug_abbrev = "",
        .debug_str = "",
        .debug_line = "",
        .debug_line_str = "",
        .debug_ranges = "",
    };

    var file = try std.fs.cwd().openFile(exec_path, .{});
    defer file.close();

    const header = try elf.Header.read(file);
    std.debug.assert(header.is_64);

    const strtab_shdr = try readSectionHeader(file, header, header.shstrndx);
    self.string_section = try (Section{
        .name = "strtab",
        .size = strtab_shdr.sh_size,
        .offset = strtab_shdr.sh_offset,
        .no_bits = strtab_shdr.sh_type == elf.SHT_NOBITS,
    }).read(self.allocator, file);

    self.sections = try self.allocator.alloc(Section, header.shnum);
    var sh_iter = header.section_header_iterator(file);
    for (self.sections) |*section| {
        var shdr = (try sh_iter.next()) orelse break;
        const name_len = c.strlen(self.string_section.ptr + shdr.sh_name);
        section.name = self.string_section[shdr.sh_name .. shdr.sh_name + name_len];
        section.size = shdr.sh_size;
        section.offset = shdr.sh_offset;
        section.no_bits = shdr.sh_type == elf.SHT_NOBITS;

        if (std.mem.eql(u8, section.name, ".debug_info")) {
            self.dwarf.debug_info = try section.read(self.allocator, file);
        } else if (std.mem.eql(u8, section.name, ".debug_abbrev")) {
            self.dwarf.debug_abbrev = try section.read(self.allocator, file);
        } else if (std.mem.eql(u8, section.name, ".debug_str")) {
            self.dwarf.debug_str = try section.read(self.allocator, file);
        } else if (std.mem.eql(u8, section.name, ".debug_line")) {
            self.dwarf.debug_line = try section.read(self.allocator, file);
        } else if (std.mem.eql(u8, section.name, ".debug_line_str")) {
            self.dwarf.debug_line_str = try section.read(self.allocator, file);
        } else if (std.mem.eql(u8, section.name, ".debug_ranges")) {
            self.dwarf.debug_ranges = try section.read(self.allocator, file);
        }
    }

    try self.dwarf.initTables();

    return self;
}

pub fn deinit(self: *Elf) void {
    self.dwarf.deinit();
    self.allocator.free(self.sections);
    self.allocator.free(self.string_section);
}

pub const SrcLoc = struct { line: u64, column: u64, file: []const u8 };

pub fn translateAddrToSrc(self: Elf, addr: usize) !?SrcLoc {
    // i think the .debug_info has a thing with the offset into .debug_line we need
    // but I haven't parsed that yet
    var stream = std.io.fixedBufferStream(self.dwarf.debug_line);
    var reader = stream.reader();
    while ((try stream.getPos()) < self.dwarf.debug_line.len) {
        const offset = try stream.getPos();
        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntBig(u64);
        };
        const section_end = (try stream.getPos()) + unit_len;

        const line_prog = try Dwarf.LineProg.init(self.allocator, self.dwarf.debug_line, self.dwarf.debug_line_str, offset);
        defer line_prog.deinit();

        if (try line_prog.findAddr(addr)) |state| {
            return SrcLoc{
                .line = state.line,
                .column = state.column,
                .file = line_prog.file_names[state.file],
            };
        }

        try stream.seekTo(section_end);
        reader = stream.reader();
    }

    return null;
}

pub fn translateSrcToAddr(self: Elf, src: SrcLoc) ?usize {
    // i think the .debug_info has a thing with the offset into .debug_line we need
    // but I haven't parsed that yet
    var stream = std.io.fixedBufferStream(self.dwarf.debug_line);
    var reader = stream.reader();
    while ((try stream.getPos()) < self.dwarf.debug_line.len) {
        const offset = try stream.getPos();
        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntBig(u64);
        };
        const section_end = (try stream.getPos()) + unit_len;

        const line_prog = try Dwarf.LineProg.init(self.allocator, self.dwarf.debug_line, self.dwarf.debug_line_str, offset);
        defer line_prog.deinit();

        for (line_prog.file_names) |file_name, i| {
            if (std.mem.eql(u8, file_name, src.file)) {
                std.debug.print("line_prog state for main.zig:80 -> {any}\n", .{try line_prog.findAddrForSrc(80, @intCast(u32, i + 1))});
                const file_idx = @intCast(u32, i);
                const state = try line_prog.findAddrForSrc(src.line, file_idx) orelse continue;
                return state.addr;
            }
        }

        try stream.seekTo(section_end);
        reader = stream.reader();
    }

    return null;
}

fn readSectionHeader(file: anytype, header: elf.Header, sh_idx: usize) !elf.Elf64_Shdr {
    const saved_filepos = try file.getPos();

    std.debug.assert(sh_idx < header.shnum);
    try file.seekTo(header.shoff + header.shentsize * sh_idx);
    var shdr: elf.Elf64_Shdr = undefined;
    const bytes_read = try file.read(std.mem.asBytes(&shdr));
    std.debug.assert(bytes_read == @sizeOf(@TypeOf(shdr)));

    try file.seekTo(saved_filepos);

    return shdr;
}

pub fn hasDebugInfo(self: Elf) bool {
    _ = self;
    return true; // lol. lmao
}

pub const Section = struct {
    name: []const u8,
    size: usize,
    offset: usize,
    no_bits: bool,

    /// pass in a std.fs.File. caller owns returned memory
    pub fn read(self: Section, allocator: Allocator, file: anytype) ![]const u8 {
        std.debug.assert(!self.no_bits);

        const saved_filepos = try file.getPos();

        var buf = try allocator.alloc(u8, self.size);
        try file.seekTo(self.offset);
        const bytes_read = try file.read(buf);
        std.debug.assert(bytes_read == buf.len);

        try file.seekTo(saved_filepos);
        return buf;
    }
};
