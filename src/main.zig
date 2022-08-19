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
const Session = @import("Session.zig");

const app_style = .{
    .highlight_color = vec4{ 0, 0, 0.5, 1 },
};

pub const CmdlineArgs = struct {
    exec_path: ?[]const u8 = null,
};

fn parseCmdlineArgs(arg_slices: [][:0]const u8) CmdlineArgs {
    var args = CmdlineArgs{};

    for (arg_slices) |arg, i| {
        if (i == 0) continue;
        args.exec_path = arg;
    }

    return args;
}

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    const allocator = general_purpose_allocator.allocator();

    //var tmpbuf: [0x1000]u8 = undefined;
    //std.debug.print("cwd={s}\n", .{try std.os.getcwd(&tmpbuf)});

    const arg_slices = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, arg_slices);
    const cmdline_args = parseCmdlineArgs(arg_slices);

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

    var session_opt = if (cmdline_args.exec_path) |path| try Session.init(allocator, path) else null;
    defer if (session_opt) |*session| session.deinit();

    // ui variables
    var last_mouse_pos = vec2{ 0, 0 };
    var last_time = @floatCast(f32, c.glfwGetTime());

    var backing_buf: [0x1000]u8 = undefined;
    var text_buf: []u8 = &backing_buf;
    text_buf.len = 0;
    var backing_buf_num: [0x1000]u8 = undefined;
    var text_buf_num: []u8 = &backing_buf_num;
    text_buf_num.len = 0;

    var file_tab = FileTab.init(allocator);
    defer file_tab.deinit();

    while (!window.should_close()) {
        window.framebuffer_size(&width, &height);
        gl.viewport(0, 0, @intCast(i32, width), @intCast(i32, height));
        //const ratio = @intToFloat(f32, width) / @intToFloat(f32, height);

        const cur_time = @floatCast(f32, c.glfwGetTime());
        const dt = cur_time - last_time;
        last_time = cur_time;

        const window_mouse_pos = window.mouse_pos();
        const mouse_pos = vec2{ window_mouse_pos[0], @intToFloat(f32, height) - window_mouse_pos[1] };
        //var mouse_diff = mouse_pos - last_mouse_pos;
        last_mouse_pos = mouse_pos;

        try ui.startFrame(width, height, mouse_pos, &window.event_queue);

        while (window.event_queue.next()) |event| {
            var remove_ev = true;
            switch (event) {
                .KeyUp => |key_ev| switch (key_ev.key) {
                    else => remove_ev = false,
                },
                .MouseUp => |mouse_key| switch (mouse_key) {
                    c.GLFW_MOUSE_BUTTON_RIGHT => {
                        const mods_pressed =
                            window.key_pressed(c.GLFW_KEY_LEFT_SHIFT) and
                            window.key_pressed(c.GLFW_KEY_LEFT_CONTROL) and
                            window.key_pressed(c.GLFW_KEY_LEFT_ALT);
                        if (!mods_pressed) {
                            remove_ev = false;
                            continue;
                        }
                        std.debug.print("all nodes that contain the mouse:\n", .{});
                        for (ui.node_table.key_mappings.items) |keymap| {
                            const node = keymap.value_ptr;
                            if (node.rect.contains(mouse_pos)) {
                                std.debug.print("{*} [{s}###{s}] parent=0x{x}, first=0x{x}, next=0x{x}, rect={d:.2}\n", .{
                                    node,
                                    node.display_string,
                                    node.hash_string,
                                    if (node.parent) |ptr| @ptrToInt(ptr) else 0,
                                    if (node.first) |ptr| @ptrToInt(ptr) else 0,
                                    if (node.next) |ptr| @ptrToInt(ptr) else 0,
                                    node.rect,
                                });
                            }
                        }
                    },
                    else => remove_ev = false,
                },
                else => remove_ev = false,
            }
            if (remove_ev) window.event_queue.removeCurrent();
        }

        ui.topParent().child_layout_axis = .x;

        const left_side_parent = ui.addNode(.{}, "###left_side_parent", .{ .child_layout_axis = .y });
        left_side_parent.pref_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushParent(left_side_parent);
        {
            const open_file_parent = ui.addNode(.{}, "###open_file_parent", .{ .child_layout_axis = .x });
            open_file_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
            ui.pushParent(open_file_parent);
            {
                const open_button_sig = ui.button("Open File");
                _ = open_button_sig;
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                const text_input_sig = ui.textInput("textinput", &backing_buf, &text_buf.len);
                _ = ui.popStyle();
                if (text_buf.len > 0 and (open_button_sig.clicked or text_input_sig.enter_pressed)) {
                    if (file_tab.addFile(text_buf)) {
                        file_tab.active_file = file_tab.files.items.len - 1;
                    } else |err| {
                        ui.openErrorPopUpF("Got an unhandled error: {s}\n{}", .{ @errorName(err), @errorReturnTrace().? });
                    }
                }
            }
            _ = ui.popParent(); // open_file_parent

            try file_tab.display(&ui);
            //_ = ui.labelF("mouse_pos={d:.2}", .{mouse_pos});
        }
        _ = ui.popParent(); // left_side_parent

        const right_side_parent = ui.addNode(.{}, "###right_side_parent", .{ .child_layout_axis = .y });
        right_side_parent.pref_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushParent(right_side_parent);
        {
            if (session_opt) |*session| {
                const rip = session.getRegisters().rip;

                _ = ui.textBoxF("session.exec_path={s}", .{session.exec_path});
                _ = ui.textBoxF("session.pid={}", .{session.pid});
                _ = ui.textBoxF("session.wait_status={}", .{session.wait_status});
                _ = ui.textBoxF("rip=0x{x}", .{rip});

                if (ui.button("Next Instruction").clicked) try session.stepInstructions(1);

                _ = ui.textBoxF("line={}, col={}, file={s}\n", session.currentSrcLoc());

                if (ui.button("do break").clicked) try session.setBreakAtAddr(rip);
                if (ui.button("continue").clicked) session.continueRunning();
                if (ui.button("stop").clicked) session.stopRunning();
                if (ui.button("find main").clicked) try session.elf.findMainFile();

                const bytes_at_rip = c.ptrace(.PEEKTEXT, session.pid, @intToPtr(*anyopaque, rip), null);
                _ = ui.labelF("bytes at rip: 0x{x}\n", .{bytes_at_rip});
            }
        }
        _ = ui.popParent(); // right_side_parent

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        ui.endFrame(dt);
        window.update();
    }
}

const FileTab = struct {
    allocator: Allocator,
    files: std.ArrayList(FileInfo),
    active_file: ?usize,

    const FileInfo = struct { path: []const u8, content: []const u8, lock_line: ?usize };

    pub fn init(allocator: Allocator) FileTab {
        return .{
            .allocator = allocator,
            .files = std.ArrayList(FileInfo).init(allocator),
            .active_file = null,
        };
    }

    pub fn deinit(self: FileTab) void {
        for (self.files.items) |file_info| {
            self.allocator.free(file_info.path);
            self.allocator.free(file_info.content);
        }
        self.files.deinit();
    }

    pub fn addFile(self: *FileTab, path: []const u8) !void {
        for (self.files.items) |file_info, i| {
            if (std.mem.eql(u8, path, file_info.path)) {
                self.allocator.free(file_info.path);
                self.allocator.free(file_info.content);
                _ = self.files.swapRemove(i);
                break;
            }
        }
        const dupe_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(dupe_path);
        const content = try std.fs.cwd().readFileAlloc(self.allocator, path, std.math.maxInt(usize));
        errdefer self.allocator.free(content);
        try self.files.append(.{ .path = dupe_path, .content = content, .lock_line = null });
    }

    pub fn display(self: *FileTab, ui: *UiContext) !void {
        const file_tab_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
        const file_tab_node = ui.addNode(.{}, "FileTab:top_node", .{ .pref_size = file_tab_size });
        ui.pushParent(file_tab_node);

        const buttons_parent_size = [2]Size{ Size.percent(1, 0), Size.by_children(1) };
        const buttons_parent = ui.addNode(.{ .draw_background = true, .draw_border = true }, "FileTab:buttons_parent", .{ .pref_size = buttons_parent_size, .child_layout_axis = .x });
        ui.pushParent(buttons_parent);
        for (self.files.items) |file_info, i| {
            const highlight_color = app_style.highlight_color;
            const is_active = self.active_file != null and self.active_file.? == i;
            if (is_active) ui.pushStyle(.{ .bg_color = highlight_color });
            const sig = ui.button(file_info.path);
            if (sig.clicked) {
                self.active_file = i;
            }
            if (is_active) _ = ui.popStyle();
        }
        _ = ui.popParent();

        const active_name = if (self.active_file) |idx| self.files.items[idx].path else "";
        const active_content = if (self.active_file) |idx| self.files.items[idx].content else "";
        const active_line = if (self.active_file) |idx| self.files.items[idx].lock_line else null;

        // text+line parent
        //const text_box_parent_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
        //const text_box_parent = ui.addNode(.{
        //    .draw_background = true,
        //    .draw_border = true,
        //    .draw_hot_effects = true,
        //}, "FileTab:text_box_parent", .{
        //    .pref_size = text_box_parent_size,
        //    .child_layout_axis = .x,
        //    .border_color = vec4{ 1, 0, 1, 1 },
        //    .border_thickness = 10,
        //});
        //_ = ui.getNodeSignal(text_box_parent);
        //ui.pushParent(text_box_parent);

        //_ = ui.textBox("line");
        //ui.topParent().last.?.flags.draw_border = false; // maybe I should just use addNode 🤔
        //ui.topParent().last.?.pref_size[1] = Size.percent(1, 0); // maybe I should just use addNode 🤔🤔
        //_ = ui.textBox(""); // separator
        //ui.topParent().last.?.flags.draw_text = false; // maybe I should just use addNode 🤔🤔🤔
        //ui.topParent().last.?.pref_size[0] = Size.pixels(1, 1); // maybe I should just use addNode 🤔🤔🤔🤔
        //ui.topParent().last.?.pref_size[1] = Size.percent(1, 0); // maybe I should just use addNode 🤔🤔🤔🤔🤔

        const text_box_size = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };
        ui.pushStyle(.{ .pref_size = text_box_size });
        try textDisplay(ui, active_name, active_content, active_line);
        _ = ui.popStyle();

        //_ = ui.popParent();

        _ = ui.popParent();
    }
};

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

pub fn printStackTrace(ret_addr: usize) void {
    const trace = getStackTrace(ret_addr);
    std.debug.print("{}", .{trace});
}

pub fn getStackTrace(ret_addr: usize) std.builtin.StackTrace {
    var addrs: [10]usize = undefined;
    var trace = std.builtin.StackTrace{ .instruction_addresses = &addrs, .index = 0 };
    std.debug.captureStackTrace(ret_addr, &trace);
    return trace;
}
