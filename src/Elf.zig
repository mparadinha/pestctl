const std = @import("std");
const Allocator = std.mem.Allocator;
const elf = std.elf;
const Dwarf = @import("Dwarf.zig");

allocator: Allocator,
sections: []Section,
string_section: []const u8,
dwarf: Dwarf,

const Elf = @This();

pub const SrcLoc = Dwarf.SrcLoc;

pub fn init(allocator: Allocator, exec_path: []const u8) !Elf {
    var self = @as(Elf, undefined);
    self.allocator = allocator;
    self.dwarf = Dwarf{
        .allocator = self.allocator,
        .debug_info = &[0]u8{},
        .debug_abbrev = &[0]u8{},
        .debug_str = &[0]u8{},
        .debug_line = &[0]u8{},
        .debug_line_str = &[0]u8{},
        .debug_ranges = &[0]u8{},
        .debug_frame = &[0]u8{},
        .eh_frame = &[0]u8{},
        .line_progs = undefined,
        .units = undefined,
        .frames = undefined,
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
        section.name = stringFromTable(self.string_section, shdr.sh_name);
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
        } else if (std.mem.eql(u8, section.name, ".debug_frame")) {
            self.dwarf.debug_frame = try section.read(self.allocator, file);
        } else if (std.mem.eql(u8, section.name, ".eh_frame")) {
            self.dwarf.eh_frame = try section.read(self.allocator, file);
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

pub fn translateAddrToSrc(self: Elf, addr: usize) !?SrcLoc {
    for (self.dwarf.line_progs) |prog| {
        if (prog.address_range[0] <= addr and addr <= prog.address_range[1]) {
            if (try prog.findAddr(addr)) |state| {
                return SrcLoc{
                    .dir = prog.include_dirs[prog.files[state.file].dir],
                    .file = prog.files[state.file].name,
                    .line = state.line,
                    .column = state.column,
                };
            }
        }
    }
    return null;
}

pub fn translateSrcToAddr(self: Elf, src: SrcLoc) !?usize {
    for (self.dwarf.line_progs) |prog| {
        for (prog.files) |file_info, i| {
            if (std.mem.eql(u8, file_info.name, src.file)) {
                const file_idx = @intCast(u32, i);
                const state = (try prog.findAddrForSrc(file_idx, src.line)) orelse continue;
                return state.address;
            }
        }
    }
    return null;
}

pub fn getLineProgForSrc(self: Elf, src: SrcLoc) !?Dwarf.LineProg {
    for (self.dwarf.line_progs) |prog| {
        for (prog.files) |file_info| {
            if (std.mem.eql(u8, file_info.name, src.file)) return prog;
        }
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

pub fn stringFromTable(table: []const u8, offset: usize) []const u8 {
    const str_ptr = @ptrCast([*c]const u8, table.ptr + offset);
    return std.mem.sliceTo(str_ptr, 0);
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
