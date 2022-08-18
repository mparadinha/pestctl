const std = @import("std");
const Allocator = std.mem.Allocator;

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
    standard_opcode_lens: []const u8,
    include_dirs: [][]const u8,
    file_names: [][]const u8,

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
    //
    // TODO: v5 dir_entry_formats, file_entry_formats

    const State = struct {
        address: usize = 0,
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

        var reader = std.io.fixedBufferStream(debug_line[offset..]).reader();

        const initial_len = try reader.readIntLittle(u32);
        const unit_len = if (initial_len < 0xffff_fff0) initial_len else blk: {
            std.debug.assert(initial_len == 0xffff_ffff);
            break :blk try reader.readIntBig(u64);
        };
        const is_64 = (initial_len == 0xffff_ffff);
        self.raw_data = debug_line[offset .. offset + unit_len];

        self.version = try reader.readIntLittle(u16);
        if (is_64) {
            _ = try reader.readIntLittle(u64);
        } else _ = try reader.readIntLittle(u32);
        self.min_instr_len = try reader.readByte();
        self.max_ops_per_instr = try reader.readByte();
        self.default_is_stmt = (try reader.readByte()) != 0;
        self.line_base = try reader.readByteSigned();
        self.line_range = try reader.readByte();
        self.opcode_base = try reader.readByte();

        // TODO: add implicit index 0 to the include_dirs and file_names lists

        // from Dwarf v4 spec, chapter 6.2.4, pg.115:
        // A compiler may generate a single null byte for the file names field and define file names
        // using the extended opcode DW_LNE_define_file

        return self;
    }

    pub fn deinit(self: LineProg) void {
        self.deinit(self.include_dirs);
        self.deinit(self.file_names);
    }
};
