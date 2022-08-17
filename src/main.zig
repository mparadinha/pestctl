const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");
const Window = @import("window.zig").Window;
const gl = @import("gl_4v3.zig");
const gfx = @import("graphics.zig");
const math = @import("math.zig");
const vec2 = math.vec2;
const vec4 = math.vec4;
const Font = @import("Font.zig");
const UiContext = @import("UiContext.zig");
const Size = UiContext.Size;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    const allocator = general_purpose_allocator.allocator();

    var width: u32 = 1600;
    var height: u32 = 900;
    // setup GLFW
    var window = Window.init(allocator, width, height, "pestctl");
    window.setup_callbacks();
    defer window.deinit();
    // setup OpenGL
    try gl.load(window.handle, get_proc_address_fn);
    gl.enable(gl.DEBUG_OUTPUT);
    gl.debugMessageCallback(gl_error_callback, null);
    //std.log.info("{s}", .{gl.getString(gl.VERSION).?});

    // GL state that we never change
    gl.clearColor(0.75, 0.36, 0.38, 1);
    gl.enable(gl.CULL_FACE);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
    gl.enable(gl.LINE_SMOOTH);

    var ui = try UiContext.init(allocator, "VictorMono-Regular.ttf", &window);
    defer ui.deinit();

    var last_mouse_pos = vec2{ 0, 0 };
    var last_time = @floatCast(f32, c.glfwGetTime());

    var backing_buf: [0x1000]u8 = undefined;
    var text_buf: []u8 = &backing_buf;
    text_buf.len = 0;
    const large_text_test = @embedFile("UiContext.zig");

    const FileInfo = struct { name: []const u8, content: []const u8 };
    var files = std.ArrayList(FileInfo).init(allocator);
    defer files.deinit();

    while (!window.should_close()) {
        window.framebuffer_size(&width, &height);
        gl.viewport(0, 0, @intCast(i32, width), @intCast(i32, height));
        //const ratio = @intToFloat(f32, width) / @intToFloat(f32, height);

        const cur_time = @floatCast(f32, c.glfwGetTime());
        const dt = cur_time - last_time;
        last_time = cur_time;

        const mouse_pos = window.mouse_pos();
        //var mouse_diff = mouse_pos - last_mouse_pos;
        last_mouse_pos = mouse_pos;

        const mouse_pos_ui = vec2{ mouse_pos[0], @intToFloat(f32, height) - mouse_pos[1] };
        try ui.startFrame(width, height, mouse_pos_ui, &window.event_queue);

        while (window.event_queue.next()) |event| {
            var remove_ev = true;
            switch (event) {
                .KeyUp => |key_ev| switch (key_ev.key) {
                    else => remove_ev = false,
                },
                else => remove_ev = false,
            }
            if (remove_ev) window.event_queue.removeCurrent();
        }

        ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
        _ = ui.textInput("textinput", &backing_buf, &text_buf.len);
        _ = ui.popStyle();

        const parent_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushStyle(.{ .pref_size = parent_size });
        try textDisplay(&ui, "large text", large_text_test, null);
        _ = ui.popStyle();

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        ui.endFrame(dt);
        window.update();
    }

    std.debug.print("final buf:\n{}\n", .{std.fmt.fmtSliceEscapeLower(text_buf)});
}

/// `line` is 0th indexed
fn textDisplay(ui: *UiContext, label: []const u8, text: []const u8, lock_line: ?usize) !void {
    const parent = ui.addNodeF(.{
        .clip_children = true,
        .draw_border = true,
        .draw_background = true,
    }, "###{s}::parent", .{label}, .{ .child_layout_axis = .y });
    ui.pushParent(parent);

    _ = ui.scrollableRegionF("###{s}::text_scroll_region_x", .{label}, .x);
    const x_scroll = ui.topParent();
    _ = ui.scrollableRegionF("###{s}::text_scroll_region_y", .{label}, .y);
    const y_scroll = ui.topParent();

    _ = ui.label(text);

    const x_off = &x_scroll.scroll_offset[0];
    const y_off = &y_scroll.scroll_offset[1];

    if (lock_line) |line| {
        y_off.* = -ui.font.getScaledMetrics().line_advance * @intToFloat(f32, line);
    }

    // hack to cut off scrolling at the ends of text
    const text_rect = try ui.font.textRect(text);
    const text_size = text_rect.max - text_rect.min;
    const text_padd = vec2{ UiContext.text_hpadding, UiContext.text_vpadding };
    var max_offset = text_size - parent.rect.size() + vec2{ 2, 2 } * text_padd;
    max_offset = vec2{ std.math.max(max_offset[0], 0), std.math.max(max_offset[1], 0) };
    x_off.* = std.math.min(x_off.*, 0);
    x_off.* = std.math.max(x_off.*, -max_offset[0]);
    y_off.* = std.math.min(y_off.*, 0);
    y_off.* = std.math.max(y_off.*, -max_offset[1]);

    _ = ui.popParent();
    _ = ui.popParent();

    _ = ui.popParent();
}

fn get_proc_address_fn(window: ?*c.GLFWwindow, proc_name: [:0]const u8) ?*const anyopaque {
    _ = window;
    const fn_ptr = c.glfwGetProcAddress(proc_name);
    // without this I got a "cast discards const qualifier" error
    return @intToPtr(?*opaque {}, @ptrToInt(fn_ptr));
}

fn gl_error_callback(source: u32, error_type: u32, id: u32, severity: u32, len: i32, msg: [*:0]const u8, user_param: ?*const anyopaque) callconv(.C) void {
    _ = len;
    _ = user_param;

    if (severity == gl.DEBUG_SEVERITY_NOTIFICATION) return;

    const source_str = switch (source) {
        0x824B => "SOURCE_OTHER",
        0x824A => "SOURCE_APPLICATION",
        0x8249 => "SOURCE_THIRD_PARTY",
        0x8248 => "SOURCE_SHADER_COMPILER",
        0x8247 => "SOURCE_WINDOW_SYSTEM",
        0x8246 => "SOURCE_API",
        else => unreachable,
    };
    const error_type_str = switch (error_type) {
        0x826A => "TYPE_POP_GROUP",
        0x8269 => "TYPE_PUSH_GROUP",
        0x8268 => "TYPE_MARKER",
        0x8251 => "TYPE_OTHER",
        0x8250 => "TYPE_PERFORMANCE",
        0x824F => "TYPE_PORTABILITY",
        0x824E => "TYPE_UNDEFINED_BEHAVIOR",
        0x824D => "TYPE_DEPRECATED_BEHAVIOR",
        0x824C => "TYPE_ERROR",
        else => unreachable,
    };
    const severity_str = switch (severity) {
        0x826B => "SEVERITY_NOTIFICATION",
        0x9148 => "SEVERITY_LOW",
        0x9147 => "SEVERITY_MEDIUM",
        0x9146 => "SEVERITY_HIGH",
        else => unreachable,
    };
    std.log.info("OpenGL: ({s}, {s}, {s}, id={}) {s}", .{ source_str, severity_str, error_type_str, id, msg });
}
