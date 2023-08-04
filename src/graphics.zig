const std = @import("std");
const Allocator = std.mem.Allocator;

const gl = @import("gl_4v3.zig");
const math = @import("math.zig");

pub const Mesh = struct {
    vao: u32,
    vbo: u32,
    ebo: u32,

    n_indices: u16,

    pub const Attrib = struct { n_elems: u32 };

    /// 'deinit' cleans up used resources
    pub fn init(vert_data: []const f32, indices: []const u32, attribs: []const Attrib) Mesh {
        var mesh = Mesh{ .vao = 0, .vbo = 0, .ebo = 0, .n_indices = @as(u16, @intCast(indices.len)) };

        gl.genVertexArrays(1, &mesh.vao);
        gl.bindVertexArray(mesh.vao);

        gl.genBuffers(1, &mesh.vbo);
        gl.bindBuffer(gl.ARRAY_BUFFER, mesh.vbo);
        gl.bufferData(gl.ARRAY_BUFFER, @as(isize, @intCast(vert_data.len * @sizeOf(f32))), vert_data.ptr, gl.STATIC_DRAW);

        var stride: u32 = 0;
        for (attribs) |attrib| stride += attrib.n_elems;
        var offset: u32 = 0;
        for (attribs, 0..) |attrib, i| {
            gl.vertexAttribPointer(
                @as(u32, @intCast(i)),
                @as(i32, @intCast(attrib.n_elems)),
                gl.FLOAT,
                gl.FALSE,
                @as(i32, @intCast(stride)) * @sizeOf(f32),
                if (offset == 0) null else @as(*const anyopaque, @ptrFromInt(offset)),
            );
            gl.enableVertexAttribArray(@as(u32, @intCast(i)));
            offset += attrib.n_elems * @sizeOf(f32);
        }

        gl.genBuffers(1, &mesh.ebo);
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.ebo);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, @as(isize, @intCast(indices.len * @sizeOf(u32))), indices.ptr, gl.STATIC_DRAW);

        return mesh;
    }

    pub fn deinit(self: Mesh) void {
        gl.deleteVertexArrays(1, &self.vao);
        gl.deleteBuffers(1, &self.vbo);
        gl.deleteBuffers(1, &self.ebo);
    }

    pub fn draw(self: Mesh) void {
        gl.bindVertexArray(self.vao);
        gl.drawElements(gl.TRIANGLES, self.n_indices, gl.UNSIGNED_INT, null);
    }
};

pub const Texture = struct {
    id: u32, // sometimes also called the 'name' of the texture
    width: u32,
    height: u32,
    tex_type: u32, // TEXTURE_2D, etc.
    format: u32, // RED, RGB, etc.

    // notes about textures in OpenGL:
    // - 'texture' objects just hold state about the texture (size, sampler type, format, etc.)
    // - 'image' functions change the underlying image data, not the state
    // https://stackoverflow.com/questions/8866904/differences-and-relationship-between-glactivetexture-and-glbindtexture

    pub const Param = struct { name: u32, value: u32 };

    pub fn init(width: u32, height: u32, format: u32, data: ?[]const u8, tex_type: u32, params: []const Param) Texture {
        var self = Texture{
            .id = undefined,
            .width = width,
            .height = height,
            .tex_type = tex_type,
            .format = format,
        };

        gl.genTextures(1, &self.id);
        gl.bindTexture(tex_type, self.id);
        for (params) |param| gl.texParameteri(tex_type, param.name, @as(i32, @intCast(param.value)));
        self.updateData(data);
        gl.generateMipmap(tex_type);

        return self;
    }

    pub fn updateData(self: Texture, data: ?[]const u8) void {
        gl.bindTexture(self.tex_type, self.id);
        // zig fmt: off
        gl.texImage2D(
            self.tex_type, 0, @intCast(self.format),
            @intCast(self.width), @intCast(self.height), 0,
            self.format, gl.UNSIGNED_BYTE,
            if (data) |ptr| @ptrCast(&ptr[0]) else null,
        );
        // zig fmt: on
    }

    pub fn deinit(self: Texture) void {
        gl.deleteTextures(1, &self.id);
    }

    pub fn bind(self: Texture, texture_unit: u32) void {
        gl.activeTexture(gl.TEXTURE0 + texture_unit);
        gl.bindTexture(self.tex_type, self.id);
    }
};

pub const Shader = struct {
    vert_id: u32,
    geom_id: ?u32 = null,
    frag_id: u32,
    prog_id: u32,
    name: []u8,
    allocator: Allocator,

    const Self = @This();

    const src_dir = "shaders";

    const ShaderType = enum { vertex, geometry, fragment };

    /// call `deinit` to cleanup
    pub fn from_srcs(
        allocator: Allocator,
        name: []const u8,
        srcs: struct {
            vertex: []const u8,
            geometry: ?[]const u8 = null,
            fragment: []const u8,
        },
    ) Shader {
        var shader = Shader{
            .vert_id = 0,
            .frag_id = 0,
            .prog_id = 0,
            .name = allocator.dupe(u8, name) catch unreachable,
            .allocator = allocator,
        };

        shader.vert_id = shader.compile_src(srcs.vertex, .vertex);
        if (srcs.geometry) |src| shader.geom_id = shader.compile_src(src, .geometry);
        shader.frag_id = shader.compile_src(srcs.fragment, .fragment);

        if (shader.geom_id) |geom_id| {
            shader.link(&[_]u32{ shader.vert_id, geom_id, shader.frag_id });
        } else {
            shader.link(&[_]u32{ shader.vert_id, shader.frag_id });
        }

        return shader;
    }

    /// call `deinit` to cleanup
    pub fn from_files(
        allocator: Allocator,
        name: []const u8,
        src_paths: struct {
            vertex: []const u8,
            geometry: ?[]const u8 = null,
            fragment: []const u8,
        },
    ) !Shader {
        const max_bytes = std.math.maxInt(usize);
        const dir = std.fs.cwd();
        const vert_src = try dir.readFileAlloc(allocator, src_paths.vertex, max_bytes);
        defer allocator.free(vert_src);
        if (vert_src.len == 0) return error.EmptyFile;
        const geom_src = if (src_paths.geometry) |path| try dir.readFileAlloc(allocator, path, max_bytes) else null;
        defer if (geom_src) |src| allocator.free(src);
        if (geom_src) |src| if (src.len == 0) return error.EmptyFile;
        const frag_src = try dir.readFileAlloc(allocator, src_paths.fragment, max_bytes);
        defer allocator.free(frag_src);
        if (frag_src.len == 0) return error.EmptyFile;

        return Shader.from_srcs(allocator, name, .{
            .vertex = vert_src,
            .geometry = geom_src,
            .fragment = frag_src,
        });
    }

    pub fn deinit(self: Shader) void {
        self.allocator.free(self.name);

        gl.detachShader(self.prog_id, self.vert_id);
        if (self.geom_id) |id| gl.detachShader(self.prog_id, id);
        gl.detachShader(self.prog_id, self.frag_id);

        gl.deleteShader(self.vert_id);
        if (self.geom_id) |id| gl.deleteShader(id);
        gl.deleteShader(self.frag_id);
        gl.deleteProgram(self.prog_id);
    }

    fn compile_src(self: *Shader, src: []const u8, shader_type: ShaderType) u32 {
        const gl_shader_type = switch (shader_type) {
            .vertex => @as(u32, gl.VERTEX_SHADER),
            .geometry => @as(u32, gl.GEOMETRY_SHADER),
            .fragment => @as(u32, gl.FRAGMENT_SHADER),
        };
        var id: u32 = gl.createShader(gl_shader_type);
        gl.shaderSource(id, 1, &(&src[0]), &(@as(c_int, @intCast(src.len))));
        gl.compileShader(id);

        // check compilation errors
        var success: i32 = 0;
        gl.getShaderiv(id, gl.COMPILE_STATUS, &success);
        if (success == gl.FALSE) {
            var msg_buf: [0x1000]u8 = undefined;
            gl.getShaderInfoLog(id, 0x1000, null, &msg_buf[0]);
            std.log.info("{s} (type={s}) compile error:\n{s}", .{
                self.name,
                @tagName(shader_type),
                @as([*c]u8, &msg_buf[0]),
            });
            unreachable;
        }

        return id;
    }

    fn compile_file(self: *Shader, shader_type: ShaderType) u32 {
        const ext = switch (shader_type) {
            .vertex => "vert",
            .geometry => @panic("geometry shaders only supported when using `from_srcs`"),
            .fragment => "frag",
        };
        const filename = std.mem.join(self.allocator, ".", &.{ self.name, ext }) catch unreachable;
        defer self.allocator.free(filename);

        const filepath = std.fs.path.join(self.allocator, &.{ src_dir, filename }) catch unreachable;
        defer self.allocator.free(filepath);

        const src = std.fs.cwd().readFileAlloc(self.allocator, filepath, 0x1_0000) catch |err| {
            std.log.info("error reading '{s}': {}", .{ filepath, err });
            return 0;
        };
        defer self.allocator.free(src);

        return self.compile_src(src, shader_type);
    }

    fn link(self: *Shader, ids: []const u32) void {
        self.prog_id = gl.createProgram();
        for (ids) |shader_id| gl.attachShader(self.prog_id, shader_id);

        gl.linkProgram(self.prog_id);

        // check for linking errors
        var success: i32 = 0;
        gl.getProgramiv(self.prog_id, gl.LINK_STATUS, &success);
        if (success == gl.FALSE) {
            var msg_buf: [0x1000]u8 = undefined;
            gl.getProgramInfoLog(self.prog_id, 0x1000, null, &msg_buf[0]);
            std.log.info("{s} link error: {s}", .{ self.name, @as([*c]u8, &msg_buf[0]) });
            unreachable;
        }
    }

    fn compile_from_files(self: *Shader) void {
        self.vert_id = self.compile_file(.vertex);
        self.frag_id = self.compile_file(.fragment);

        self.link(&.{ self.vert_id, self.frag_id });
    }

    pub fn bind(self: Shader) void {
        gl.useProgram(self.prog_id);
    }

    pub fn uniform(self: Shader, name: []const u8) i32 {
        const loc = gl.getUniformLocation(self.prog_id, &name[0]);
        if (loc == -1) std.log.err("error getting uniform '{s}' from shader '{s}'", .{ name, self.name });
        //if (loc == -1) std.debug.panic("error getting uniform '{s}' from shader '{s}'", .{ name, self.name });
        return loc;
    }

    pub fn set(self: Shader, name: []const u8, obj: anytype) void {
        const obj_type = @TypeOf(obj);
        const loc = self.uniform(name);
        switch (obj_type) {
            i32 => gl.uniform1i(loc, obj),
            u32 => gl.uniform1ui(loc, obj),
            f32 => gl.uniform1f(loc, obj),
            bool => gl.uniform1ui(loc, @intFromBool(obj)),
            math.vec2 => gl.uniform2fv(loc, 1, &obj[0]),
            math.vec3 => gl.uniform3fv(loc, 1, &obj[0]),
            math.vec4 => gl.uniform4fv(loc, 1, &obj[0]),
            math.mat4 => gl.uniformMatrix4fv(loc, 1, gl.FALSE, &obj[0]),

            else => @compileError("need to implement Shader.set for type " ++ @typeName(obj_type)),
        }
    }
};

pub const Framebuffer = struct {
    fbo: u32,
    texture: Texture,

    pub fn init(width: u32, height: u32) Framebuffer {
        // TODO: other types of framebuffer that aren't for shadowmaps

        var self: Framebuffer = undefined;

        var current_fbo: u32 = 0;
        gl.getIntegerv(gl.DRAW_FRAMEBUFFER_BINDING, @as(*i32, @ptrCast(&current_fbo)));
        defer gl.bindFramebuffer(gl.FRAMEBUFFER, current_fbo);

        gl.genFramebuffers(1, &self.fbo);
        gl.bindFramebuffer(gl.FRAMEBUFFER, self.fbo);
        gl.activeTexture(gl.TEXTURE0);
        self.texture = Texture.init(width, height, gl.DEPTH_COMPONENT, null, gl.TEXTURE_2D, &.{
            .{ .name = gl.TEXTURE_MAG_FILTER, .value = gl.NEAREST },
            .{ .name = gl.TEXTURE_MIN_FILTER, .value = gl.NEAREST },
            .{ .name = gl.TEXTURE_WRAP_S, .value = gl.CLAMP_TO_EDGE },
            .{ .name = gl.TEXTURE_WRAP_T, .value = gl.CLAMP_TO_EDGE },
        });

        gl.framebufferTexture(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, self.texture.id, 0);
        gl.drawBuffer(gl.NONE);
        gl.readBuffer(gl.NONE);
        std.debug.assert(gl.checkFramebufferStatus(gl.FRAMEBUFFER) == gl.FRAMEBUFFER_COMPLETE);

        return self;
    }

    pub fn deinit(self: Framebuffer) void {
        gl.deleteFramebuffers(1, &self.fbo);
    }
};

pub fn make_cube_mesh(size: f32) Mesh {
    const vert_data = [_]f32{
        size,  size,  -size, 1,  0,  0,  1, 1,
        size,  size,  size,  1,  0,  0,  0, 1,
        size,  -size, size,  1,  0,  0,  0, 0,
        size,  -size, -size, 1,  0,  0,  1, 0,
        -size, size,  size,  -1, 0,  0,  1, 1,
        -size, size,  -size, -1, 0,  0,  0, 1,
        -size, -size, -size, -1, 0,  0,  0, 0,
        -size, -size, size,  -1, 0,  0,  1, 0,

        size,  size,  -size, 0,  1,  0,  1, 1,
        -size, size,  -size, 0,  1,  0,  0, 1,
        -size, size,  size,  0,  1,  0,  0, 0,
        size,  size,  size,  0,  1,  0,  1, 0,
        size,  -size, size,  0,  -1, 0,  1, 1,
        -size, -size, size,  0,  -1, 0,  0, 1,
        -size, -size, -size, 0,  -1, 0,  0, 0,
        size,  -size, -size, 0,  -1, 0,  1, 0,

        size,  size,  size,  0,  0,  1,  1, 1,
        -size, size,  size,  0,  0,  1,  0, 1,
        -size, -size, size,  0,  0,  1,  0, 0,
        size,  -size, size,  0,  0,  1,  1, 0,
        -size, size,  -size, 0,  0,  -1, 1, 1,
        size,  size,  -size, 0,  0,  -1, 0, 1,
        size,  -size, -size, 0,  0,  -1, 0, 0,
        -size, -size, -size, 0,  0,  -1, 1, 0,
    };
    var indices: [6 * 6]u32 = undefined;
    for (indices, 0..) |*idx, i| {
        const face_idxs = [6]u32{ 0, 1, 2, 0, 2, 3 };
        idx.* = face_idxs[i % 6] + @as(u32, @intCast(4 * (i / 6)));
    }

    return Mesh.init(&vert_data, &indices, &.{
        .{ .n_elems = 3 },
        .{ .n_elems = 3 },
        .{ .n_elems = 2 },
    });
}

/// quad goes from (0, 0) to (1, 1) so we can massage it into having UV's in the vertex
/// shader, if we need to later on
pub fn make_screen_quad_mesh() Mesh {
    const vert_data = [_]f32{ 0, 0, 1, 0, 1, 1, 0, 1 };
    const indices = [_]u32{ 0, 1, 3, 1, 2, 3 };

    return Mesh.init(&vert_data, &indices, &.{.{ .n_elems = 2 }});
}

pub fn make_quad_mesh(size: f32) Mesh {
    const vert_data = [_]f32{
        // pos and tex coords data
        -size, -size, 0, 0, 0,
        size,  -size, 0, 1, 0,
        size,  size,  0, 1, 1,
        -size, size,  0, 0, 1,
    };
    const indices = [_]u32{
        0, 1, 3, 1, 2, 3,
    };

    return Mesh.init(&vert_data, &indices, &.{
        .{ .n_elems = 3 },
        .{ .n_elems = 2 },
    });
}
