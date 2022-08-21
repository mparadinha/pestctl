const std = @import("std");
const Allocator = std.mem.Allocator;
const gl = @import("gl_4v3.zig");
const gfx = @import("graphics.zig");
const math = @import("math.zig");
const vec2 = math.vec2;
const c = @cImport({
    @cInclude("stb_rect_pack.h");
    @cInclude("stb_truetype.h");
});

const Font = @This();

allocator: Allocator,
file_data: [:0]u8,

pixel_size: f32,

texture: gfx.Texture,
texture_data: []u8,

font_info: c.stbtt_fontinfo,
char_data: CharMap,
kerning_data: KerningMap,
packing_ctx: c.stbtt_pack_context,

/// call 'deinit' when you're done with the Font
pub fn from_ttf(allocator: Allocator, filepath: []const u8, size: f32) !Font {
    var self: Font = undefined;
    self.allocator = allocator;
    self.pixel_size = size;
    self.char_data = CharMap.init(allocator);
    self.kerning_data = KerningMap.init(allocator);

    self.file_data = try std.fs.cwd().readFileAllocOptions(
        allocator,
        filepath,
        std.math.maxInt(usize),
        null,
        @alignOf(u8),
        0,
    );

    if (c.stbtt_InitFont(&self.font_info, &self.file_data[0], 0) == 0) @panic("");

    try self.setupPacking(64);

    return self;
}

pub fn deinit(self: *Font) void {
    self.allocator.free(self.texture_data);
    self.texture.deinit();
    self.char_data.deinit();
    self.kerning_data.deinit();
    self.allocator.free(self.file_data);
}

pub fn getScaledMetrics(self: Font) struct {
    ascent: f32, // how much above baseline the font reaches
    descent: f32, // how much below baseline the font reaches
    line_gap: f32, // between two lines
    line_advance: f32, // vertical space taken by one line
} {
    var ascent: i32 = undefined;
    var descent: i32 = undefined;
    var line_gap: i32 = undefined;
    c.stbtt_GetFontVMetrics(&self.font_info, &ascent, &descent, &line_gap);
    const scale = c.stbtt_ScaleForPixelHeight(&self.font_info, self.pixel_size);
    return .{
        .ascent = @intToFloat(f32, ascent) * scale,
        .descent = @intToFloat(f32, descent) * scale,
        .line_gap = @intToFloat(f32, line_gap) * scale,
        .line_advance = @intToFloat(f32, ascent - descent + line_gap) * scale,
    };
}

pub const Quad = packed struct {
    /// points are given in counter clockwise order starting from bottom left
    points: [4]Vertex,
    const Vertex = packed struct { pos: vec2, uv: vec2 };
};

/// caller owns returned memory
pub fn buildQuads(self: *Font, allocator: Allocator, str: []const u8) ![]Quad {
    return self.buildQuadsAt(allocator, str, vec2{ 0, 0 });
}

/// caller owns returned memory
pub fn buildQuadsAt(self: *Font, allocator: Allocator, str: []const u8, start_pos: vec2) ![]Quad {
    var quads = try std.ArrayList(Quad).initCapacity(allocator, try std.unicode.utf8CountCodepoints(str));
    const metrics = self.getScaledMetrics();

    var cursor = @as([2]f32, start_pos);
    var utf8_iter = std.unicode.Utf8View.initUnchecked(str).iterator();
    while (utf8_iter.nextCodepoint()) |codepoint| {
        if (codepoint == '\n') {
            if (utf8_iter.peek(1).len > 0) {
                cursor[0] = start_pos[0];
                cursor[1] += metrics.line_advance; // stb uses +y up
            }
            continue;
        }

        const quad = try self.buildQuad(codepoint, &cursor);
        quads.append(quad) catch unreachable;
    }

    return quads.toOwnedSlice();
}

pub fn buildQuad(self: *Font, codepoint: u21, cursor: *[2]f32) !Quad {
    const packed_char_data = (try self.getCharData(codepoint)).packing_data;

    var stb_quad: c.stbtt_aligned_quad = undefined;
    c.stbtt_GetPackedQuad(
        &packed_char_data,
        @intCast(i32, self.texture.width),
        @intCast(i32, self.texture.height),
        0,
        &cursor[0],
        &cursor[1],
        &stb_quad,
        1,
    );

    const q = stb_quad;
    const quad = Quad{ .points = [4]Quad.Vertex{
        .{ .pos = vec2{ q.x0, -q.y1 }, .uv = vec2{ q.s0, q.t1 } },
        .{ .pos = vec2{ q.x1, -q.y1 }, .uv = vec2{ q.s1, q.t1 } },
        .{ .pos = vec2{ q.x1, -q.y0 }, .uv = vec2{ q.s1, q.t0 } },
        .{ .pos = vec2{ q.x0, -q.y0 }, .uv = vec2{ q.s0, q.t0 } },
    } };

    return quad;
}

pub const Rect = struct { min: vec2, max: vec2 };

pub fn textRect(self: *Font, str: []const u8) !Rect {
    var x_advance: f32 = 0;
    var max_x: f32 = 0;

    var newlines: u32 = 0;

    var utf8_iter = (try std.unicode.Utf8View.init(str)).iterator();
    while (utf8_iter.nextCodepoint()) |codepoint| {
        const next_codepoint: ?u21 = if (utf8_iter.peek(1).len > 0)
            std.unicode.utf8Decode(utf8_iter.peek(1)) catch unreachable // Utf8View already validated the input
        else
            null;

        if (codepoint == '\n') {
            if (next_codepoint != null) {
                max_x = std.math.max(max_x, x_advance);
                x_advance = 0;
                newlines += 1;
            }
            continue;
        }

        const advance_width = (try self.getCharData(codepoint)).advance_width;
        x_advance += @intToFloat(f32, advance_width);

        //if (next_codepoint) |n_codepoint| {
        //    const kern = try self.getKerningAdvance([2]u21{ codepoint, n_codepoint });
        //    x_advance += @intToFloat(f32, kern);
        //}
    }

    max_x = std.math.max(max_x, x_advance);

    const metrics = self.getScaledMetrics();
    const scale = c.stbtt_ScaleForPixelHeight(&self.font_info, self.pixel_size);

    return Rect{
        .min = vec2{ 0, @intToFloat(f32, newlines) * -metrics.line_advance + metrics.descent },
        .max = vec2{ max_x * scale, metrics.ascent },
    };
}

const CharData = struct { packing_data: c.stbtt_packedchar, advance_width: i32 };
const CharMap = std.HashMap(u21, CharData, struct {
    pub fn hash(self: @This(), key: u21) u64 {
        _ = self;
        return @intCast(u64, key);
    }
    pub fn eql(self: @This(), key_a: u21, key_b: u21) bool {
        _ = self;
        return key_a == key_b;
    }
}, std.hash_map.default_max_load_percentage);

const CharPair = [2]u21;
const KerningMap = std.HashMap(CharPair, i32, struct {
    pub fn hash(self: @This(), key: CharPair) u64 {
        _ = self;
        return @intCast(u64, key[0]) * 7 + @intCast(u64, key[1]) * 11;
    }
    pub fn eql(self: @This(), key_a: CharPair, key_b: CharPair) bool {
        _ = self;
        return key_a[0] == key_b[0] and key_a[1] == key_b[1];
    }
}, std.hash_map.default_max_load_percentage);

fn setupPacking(self: *Font, texture_size: u32) !void {
    self.texture_data = try self.allocator.alloc(u8, texture_size * texture_size);
    self.texture = gfx.Texture.init(texture_size, texture_size, gl.RED, null, gl.TEXTURE_2D, &.{
        .{ .name = gl.TEXTURE_MIN_FILTER, .value = gl.LINEAR },
        .{ .name = gl.TEXTURE_MAG_FILTER, .value = gl.LINEAR },
        .{ .name = gl.TEXTURE_WRAP_S, .value = gl.CLAMP_TO_EDGE },
        .{ .name = gl.TEXTURE_WRAP_T, .value = gl.CLAMP_TO_EDGE },
    });

    if (c.stbtt_PackBegin(
        &self.packing_ctx,
        self.texture_data.ptr,
        @intCast(i32, self.texture.width),
        @intCast(i32, self.texture.height),
        0,
        1, // padding between characters
        null,
    ) == 0) @panic("");
    c.stbtt_PackSetOversampling(&self.packing_ctx, 2, 2);
}

fn resetPacking(self: *Font) void {
    self.allocator.free(self.texture_data);
    self.texture.deinit();
    c.stbtt_PackEnd(&self.packing_ctx);
}

// call this if we run out of space in texture. creates a bigger texture and repacks all
// the glyphs we already had in there.
fn increaseTextureAndRepack(self: *Font) !void {
    const old_tex_size = self.texture.width;
    const new_tex_size = old_tex_size * 2;

    self.resetPacking();
    try self.setupPacking(new_tex_size);

    var char_iterator = self.char_data.iterator();
    while (char_iterator.next()) |entry| {
        const pack_result = c.stbtt_PackFontRange(
            &self.packing_ctx,
            self.file_data,
            0,
            self.pixel_size,
            @intCast(i32, entry.key_ptr.*),
            1,
            &entry.value_ptr.packing_data,
        );
        std.debug.assert(pack_result == 1);
    }

    self.texture.updateData(self.texture_data);
}

fn getCharData(self: *Font, codepoint: u21) !CharData {
    if (self.char_data.get(codepoint)) |value| return value;

    var char_data = @as(CharData, undefined);
    const pack_result = c.stbtt_PackFontRange(
        &self.packing_ctx,
        self.file_data,
        0,
        self.pixel_size,
        @intCast(i32, codepoint),
        1,
        &char_data.packing_data,
    );
    // we ran out of space in the texture
    if (pack_result == 0) {
        try self.increaseTextureAndRepack();
        return self.getCharData(codepoint);
    }

    self.texture.updateData(self.texture_data);

    c.stbtt_GetCodepointHMetrics(&self.font_info, codepoint, &char_data.advance_width, null);
    try self.char_data.put(codepoint, char_data);

    return char_data;
}

fn getKerningAdvance(self: *Font, char_pair: [2]u21) !i32 {
    const entry = try self.kerning_data.getOrPut(char_pair);
    if (!entry.found_existing) {
        entry.value_ptr.* = c.stbtt_GetCodepointKernAdvance(&self.font_info, char_pair[0], char_pair[1]);
    }
    return entry.value_ptr.*;
}
