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

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    const allocator = general_purpose_allocator.allocator();

    var width: u32 = 1000;
    var height: u32 = 600;
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
            _ = event;
        }

        ui.root_node.child_layout_axis = .y;
        //const size1 = [2]UiContext.Size{ UiContext.Size.pixels(100, 1), UiContext.Size.text_dim(1) };
        const size075 = [2]UiContext.Size{ UiContext.Size.pixels(100, 0.75), UiContext.Size.text_dim(1) };
        const size05 = [2]UiContext.Size{ UiContext.Size.pixels(100, 0.5), UiContext.Size.text_dim(1) };
        const size025 = [2]UiContext.Size{ UiContext.Size.pixels(100, 0.25), UiContext.Size.text_dim(1) };
        const size_space = [2]UiContext.Size{ UiContext.Size.percent(1, 0), UiContext.Size.text_dim(0) };
        {
            //const sizes = [2]UiContext.Size{ UiContext.Size.pixels(500 * (@sin(cur_time) + 1), 1), UiContext.Size.pixels(100, 1) };
            const sizes = [2]UiContext.Size{ UiContext.Size.pixels(mouse_pos[0], 1), UiContext.Size.pixels(50, 1) };
            const parent_node = try ui.addNode(.{ .draw_background = true, .draw_border = true }, "parent", .{
                .child_layout_axis = .x,
                .pref_size = sizes,
            });
            try ui.pushParent(parent_node);
            defer _ = ui.popParent();

            //_ = ui.spacer(.x, 1);
            ui.pushStyle(.{ .pref_size = size_space });
            _ = ui.button("S");
            ui.pushStyle(.{ .pref_size = size05 });
            _ = ui.button("A");
            ui.pushStyle(.{ .pref_size = size075 });
            _ = ui.button("B");
            ui.pushStyle(.{ .pref_size = size025 });
            _ = ui.button("C");
            ui.pushStyle(.{ .pref_size = size025 });
            _ = ui.button("D");
        }
        {
            const sizes = [2]UiContext.Size{ UiContext.Size.pixels(mouse_pos[0], 1), UiContext.Size.pixels(50, 1) };
            const parent_node = try ui.addNode(.{ .draw_background = true, .draw_border = true }, "parent1", .{
                .child_layout_axis = .x,
                .pref_size = sizes,
            });
            try ui.pushParent(parent_node);
            defer _ = ui.popParent();

            //_ = ui.spacer(.x, 1);
            ui.pushStyle(.{ .pref_size = size_space });
            _ = ui.button("S1");
            ui.pushStyle(.{ .pref_size = size05 });
            _ = ui.button("A1");
            ui.pushStyle(.{ .pref_size = size025 });
            _ = ui.button("C1");
            ui.pushStyle(.{ .pref_size = size025 });
            _ = ui.button("D1");
        }

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        ui.endFrame(dt);
        window.update();
    }
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
