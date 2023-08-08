const std = @import("std");

/// convert degrees into radians
pub fn to_radians(x: anytype) @TypeOf(x) {
    return (x * std.math.pi) / 180;
}
/// convert radians into degrees
pub fn to_degrees(x: anytype) @TypeOf(x) {
    return (x * 180) / std.math.pi;
}

pub const vec2 = @Vector(2, f32);
pub const vec3 = @Vector(3, f32);
pub const vec4 = @Vector(4, f32);
pub const uvec2 = @Vector(2, u32);

pub fn splat(comptime VecType: type, scalar: std.meta.Child(VecType)) VecType {
    return @splat(scalar);
}

pub fn zeroes(comptime VecType: type) VecType {
    return splat(VecType, 0);
}

pub fn axis_vec(axis: enum { x, y, z }) vec3 {
    return switch (axis) {
        .x => [3]f32{ 1, 0, 0 },
        .y => [3]f32{ 0, 1, 0 },
        .z => [3]f32{ 0, 0, 1 },
    };
}

pub fn times(vec: anytype, scalar: std.meta.Child(@TypeOf(vec))) @TypeOf(vec) {
    return vec * splat(@TypeOf(vec), scalar);
}

pub fn div(vec: anytype, scalar: std.meta.Child(@TypeOf(vec))) @TypeOf(vec) {
    return vec / splat(@TypeOf(vec), scalar);
}

pub fn dot(vec_a: anytype, vec_b: @TypeOf(vec_a)) std.meta.Child(@TypeOf(vec_a)) {
    const mult = vec_a * vec_b;
    return @reduce(.Add, mult);
}

pub fn size(vec: anytype) f32 {
    const info = @typeInfo(@TypeOf(vec));
    std.debug.assert(info == .Vector);
    std.debug.assert(info.Vector.child != f64);
    return std.math.sqrt(dot(vec, vec));
}

pub fn normalize(vec: anytype) @TypeOf(vec) {
    const info = @typeInfo(@TypeOf(vec));
    std.debug.assert(info == .Vector);
    const vec_size = size(vec);
    return if (vec_size == 0) vec else vec / splat(@TypeOf(vec), vec_size);
}

pub fn cross(vec_a: anytype, vec_b: @TypeOf(vec_a)) @TypeOf(vec_a) {
    const info = @typeInfo(@TypeOf(vec_a)).Vector;
    std.debug.assert(info.len == 3);
    return [3]info.child{
        (vec_a[1] * vec_b[2]) - (vec_a[2] * vec_b[1]),
        (vec_a[2] * vec_b[0]) - (vec_a[0] * vec_b[2]),
        (vec_a[0] * vec_b[1]) - (vec_a[1] * vec_b[0]),
    };
}

/// Matrices are stored in column-major form
pub const mat2 = @Vector(2 * 2, f32);
pub const mat3 = @Vector(3 * 3, f32);
pub const mat4 = @Vector(4 * 4, f32);

/// only for square matrices
pub fn mat_mult(mat_a: anytype, mat_b: @TypeOf(mat_a)) @TypeOf(mat_a) {
    const info = @typeInfo(@TypeOf(mat_a)).Vector;
    const dim = perfect_sqrt(info.len) orelse unreachable;

    var res: mat4 = undefined;
    var i: usize = 0;
    while (i < dim) : (i += 1) {
        var j: usize = 0;
        while (j < dim) : (j += 1) {
            var dot_sum: info.child = 0;
            var k: usize = 0;
            while (k < dim) : (k += 1) dot_sum += mat_a[k * dim + j] * mat_b[i * dim + k];
            res[i * dim + j] = dot_sum;
        }
    }
    return res;
}

// only for square matrices
// multiples the matrices in `mat_list` in reverse order
pub fn mat_mults(comptime MatType: type, mat_list: []MatType) MatType {
    var accum = identity(MatType);
    var i: usize = mat_list.len;
    while (i > 0) : (i -= 1) accum = mat_mult(mat_list[i - 1], accum);
    return accum;
}

// only for square matrices
pub fn mat_vec_mult(mat: anytype, vec: anytype) @TypeOf(vec) {
    const MatType = @TypeOf(mat);
    const VecType = @TypeOf(vec);

    const mat_info = @typeInfo(MatType).Vector;
    const dim = perfect_sqrt(mat_info.len) orelse unreachable;

    var res = zeroes(VecType);
    var i: usize = 0;
    while (i < dim) : (i += 1) {
        var j: usize = 0;
        while (j < dim) : (j += 1) {
            res[i] += vec[j] * mat[i + dim * j];
        }
    }
    return res;
}

pub fn identity(comptime MatType: type) MatType {
    const info = @typeInfo(MatType).Vector;
    const dim = perfect_sqrt(info.len) orelse unreachable;
    var mat = splat(MatType, 0);
    var i: usize = 0;
    while (i < dim) : (i += 1) mat[i * dim + i] = 1;
    return mat;
}

// zig fmt: off
pub fn translation(delta: vec3) mat4 {
    return [4 * 4]f32 {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        delta[0], delta[1], delta[2], 1,
    };
}

pub fn scale(scale_vec: vec3) mat4 {
    return [4 * 4]f32 {
        scale_vec[0], 0, 0, 0,
        0, scale_vec[1], 0, 0,
        0, 0, scale_vec[2], 0,
        0, 0, 0, 1,
    };
}

/// `user_angle` is in degrees
pub fn rotation(user_axis: vec3, user_angle: f32) mat4 {
    const axis = normalize(user_axis);
    const angle = to_radians(user_angle);

    // https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle
    const c: f32 = 1 - @cos(angle);
    return [4 * 4]f32{
        @cos(angle) + axis[0] * axis[0] * c,
        axis[0] * axis[1] * c + axis[2] * @sin(angle),
        axis[0] * axis[1] * c - axis[1] * @sin(angle),
        0,

        axis[0] * axis[1] * c - axis[2] * @sin(angle),
        @cos(angle) + axis[1] * axis[1] * c,
        axis[2] * axis[1] * c + axis[0] * @sin(angle),
        0,

        axis[0] * axis[2] * c + axis[1] * @sin(angle),
        axis[1] * axis[2] * c - axis[0] * @sin(angle),
        @cos(angle) + axis[2] * axis[2] * c,
        0,

        0, 0, 0, 1,
    };
}

/// `fov` is the horizontal FOV angle in degrees
pub fn projection(fov: f32, ratio: f32, near: f32, far: f32) mat4 {
    // https://www.songho.ca/opengl/gl_projectionmatrix.html#perspective
    const half_tan_fov = std.math.tan(fov * std.math.pi / 360.0);
    var mat = splat(mat4, 0);
    mat[0 * 4 + 0] = 1 / (ratio * half_tan_fov);
    mat[1 * 4 + 1] = 1 / half_tan_fov;
    mat[2 * 4 + 2] = (near + far) / (near - far);
    mat[2 * 4 + 3] = -1;
    mat[3 * 4 + 2] = (2 * near * far) / (near - far);
    return mat;
}

pub fn ortho_proj(width: f32, height: f32, near: f32, far: f32) mat4 {
    // https://www.songho.ca/opengl/gl_projectionmatrix.html#ortho
    const r = width / 2;
    const t = height / 2;
    var mat = splat(mat4, 0);
    mat[0 * 4 + 0] = 1 / r;
    mat[1 * 4 + 1] = 1 / t;
    mat[2 * 4 + 2] = 2 / (near - far);
    mat[3 * 4 + 2] = (near + far) / (near - far);
    mat[3 * 4 + 3] = 1;
    return mat;
}

/// `user_yaw` and `user_pitch` are in degress
/// yaw starts at -z axis and rotates around +y according to right hand rule
/// pitch is the angle with the xz plane
pub fn view(pos: vec3, user_yaw: f32, user_pitch: f32) mat4 {
    const yaw = to_radians(user_yaw);
    const pitch = to_radians(user_pitch);

    const x: vec3 = [3]f32{ @cos(yaw), 0, -@sin(yaw) };
    const y: vec3 = [3]f32{ @sin(yaw) * @sin(pitch), @cos(pitch), @cos(yaw) * @sin(pitch) };
    const z: vec3 = [3]f32{ @sin(yaw) * @cos(pitch), -@sin(pitch), @cos(pitch) * @cos(yaw) };
    const dots = [3]f32{ dot(x, pos), dot(y, pos), dot(z, pos) };

    return [4 * 4]f32{
        x[0], y[0], z[0], 0,
        x[1], y[1], z[1], 0,
        x[2], y[2], z[2], 0,
        -dots[0], -dots[1], -dots[2], 1
    };
}

pub fn look_at(eye: vec3, target: vec3) mat4 {
    // https://github.com/HandmadeMath/Handmade-Math/blob/master/HandmadeMath.h#L1613
    const f = normalize(target - eye);
    const s = normalize(cross(f, vec3{0, 1, 0}));
    const u = cross(s, f);
    return mat4{
        s[0], u[0], -f[0], 0,
        s[1], u[1], -f[1], 0,
        s[2], u[2], -f[2], 0,
        -dot(s, eye), -dot(u, eye), dot(f, eye), 1,
    };
}

pub fn look_at_roll(eye: vec3, target: vec3, roll: f32) mat4 {
    const look_mat = look_at(eye, target);
    // rotate the whole scene around +z in opposite direction of roll
    const r = rotation(vec3{0, 0, 1}, roll);
    return mat_mult(r, look_mat);
}
// zig fmt: on

/// Return the sqrt of `x` or `null` if `x` is not a perfect square.
fn perfect_sqrt(x: anytype) ?@TypeOf(x) {
    const info = @typeInfo(@TypeOf(x));
    const sqrt: f32 = switch (info) {
        .Int, .ComptimeInt => @sqrt(@as(f32, @floatFromInt(x))),
        .Float, .ComptimeFloat => @sqrt(@as(f32, @floatCast(x))),
        else => |tag| @compileError("can't calculate perfect for " ++ @tagName(tag)),
    };
    if (@floor(sqrt) != @ceil(sqrt)) return null;
    return switch (info) {
        .Int, .ComptimeInt => @as(@TypeOf(x), @intFromFloat(sqrt)),
        .Float, .ComptimeFloat => @as(@TypeOf(x), @floatCast(sqrt)),
        else => |tag| @compileError("can't calculate perfect for " ++ @tagName(tag)),
    };
}
