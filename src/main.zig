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
const SrcLoc = Session.SrcLoc;

const tracy = @import("tracy.zig");

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

// icon font (and this mapping) was generated using fontello.com
const Icons = struct {
    pub const cancel = "\u{e800}";
    pub const th_list = "\u{e801}";
    pub const search = "\u{e802}";
    pub const plus_circle = "\u{e803}";
    pub const cog = "\u{e804}";
    pub const ok = "\u{e805}";
    pub const circle = "\u{f111}";
    pub const up_open = "\u{e806}";
    pub const right_open = "\u{e807}";
    pub const left_open = "\u{e808}";
    pub const down_opwn = "\u{e809}";
};

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    const allocator = general_purpose_allocator.allocator();
    //const allocator = std.heap.c_allocator;

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

    var ui = try UiContext.init(allocator, "VictorMono-Regular.ttf", "icons.ttf", &window);
    defer ui.deinit();

    var session_opt = if (cmdline_args.exec_path) |path| try Session.init(allocator, path) else null;
    defer if (session_opt) |*session| session.deinit();

    var last_mouse_pos = vec2{ 0, 0 };
    var last_time = @floatCast(f32, c.glfwGetTime());

    var backing_buf: [0x1000]u8 = undefined;
    var text_buf: []u8 = &backing_buf;
    text_buf.len = 0;
    var backing_buf_num: [0x1000]u8 = undefined;
    var text_buf_num = try std.fmt.bufPrint(&backing_buf_num, "105", .{});
    var backing_buf_file: [0x1000]u8 = undefined;
    var text_buf_file = try std.fmt.bufPrint(&backing_buf_file, "main.zig", .{});
    var backing_buf_exec: [0x1000]u8 = undefined;
    var text_buf_exec = try std.fmt.bufPrint(&backing_buf_exec, "", .{});
    var backing_buf_var: [0x1000]u8 = undefined;
    var text_buf_var = try std.fmt.bufPrint(&backing_buf_var, "cur_time", .{});

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

        {
            const ev = window.event_queue.find(.MouseUp, c.GLFW_MOUSE_BUTTON_RIGHT);
            const mods = window.get_modifiers();
            if (mods.shift and mods.control and mods.alt and ev != null) {
                _ = window.event_queue.removeAt(ev.?);
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
            }
        }

        ui.topParent().child_layout_axis = .x;

        const left_side_parent = ui.addNode(.{
            .draw_border = true,
            .draw_background = true,
        }, "###left_side_parent", .{ .child_layout_axis = .y });
        left_side_parent.pref_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushParent(left_side_parent);
        {
            const open_exec_parent = ui.addNode(.{}, "###open_exec_parent", .{ .child_layout_axis = .x });
            open_exec_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
            ui.pushParent(open_exec_parent);
            {
                const open_button_sig = ui.button("Open Executable");
                _ = open_button_sig;
                const text_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = text_input_size });
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                const text_input_sig = ui.textInput("openexecinput", &backing_buf_exec, &text_buf_exec.len);
                _ = ui.popStyle();
                _ = ui.popStyle();
                if (text_buf_exec.len > 0 and (open_button_sig.clicked or text_input_sig.enter_pressed)) {
                    if (session_opt) |*session| session.deinit();
                    session_opt = Session.init(allocator, text_buf_exec) catch |err| blk: {
                        ui.openErrorPopUpF("Got an unhandled error: {s}\n{?}", .{ @errorName(err), @errorReturnTrace() });
                        break :blk null;
                    };
                }
            }
            _ = ui.popParent(); // open_exec_parent

            const open_file_parent = ui.addNode(.{}, "###open_file_parent", .{ .child_layout_axis = .x });
            open_file_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
            ui.pushParent(open_file_parent);
            {
                const open_button_sig = ui.button("Open Source File");
                _ = open_button_sig;
                const text_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = text_input_size });
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                const text_input_sig = ui.textInput("textinput", &backing_buf, &text_buf.len);
                _ = ui.popStyle();
                _ = ui.popStyle();
                if (text_buf.len > 0 and (open_button_sig.clicked or text_input_sig.enter_pressed)) {
                    if (file_tab.addFile(text_buf)) {
                        file_tab.active_file = file_tab.files.items.len - 1;
                    } else |err| {
                        ui.openErrorPopUpF("Got an unhandled error: {s}\n{?}", .{ @errorName(err), @errorReturnTrace() });
                    }
                }
            }
            _ = ui.popParent(); // open_file_parent

            try file_tab.display(&ui);
        }
        _ = ui.popParent(); // left_side_parent

        const right_side_parent = ui.addNode(.{ .draw_background = true }, "###right_side_parent", .{ .child_layout_axis = .y });
        right_side_parent.pref_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushParent(right_side_parent);
        {
            if (session_opt) |*session| {
                try session.update();

                _ = ui.textBoxF("Child pid: {}", .{session.pid});
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
                _ = ui.textBoxF("Child Status: {s}", .{@tagName(session.status)});
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
                _ = ui.textBoxF("wait_status: 0x{x}", .{session.wait_status});
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
                _ = ui.textBoxF("src_loc: {?}", .{session.src_loc});
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);

                const table_regs = .{ "rax", "rcx", "rbx", "rdx", "rip", "rsp" };
                const regs = session.regs;
                inline for (table_regs) |reg_name| {
                    _ = ui.textBoxF(reg_name ++ ": 0x{x:0>16}", .{@field(regs, reg_name)});
                }

                const vars_parent = ui.addNode(.{}, "###vars_parent", .{ .child_layout_axis = .y });
                vars_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                ui.pushParent(vars_parent);
                {
                    const row_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                    const column_box_size = [2]Size{ Size.percent(1.0 / 3.0, 1), Size.text_dim(1) };
                    const table_header_row_parent = ui.addNode(.{}, "###table_header_row_parent", .{ .child_layout_axis = .x });
                    table_header_row_parent.pref_size = row_size;
                    ui.pushParent(table_header_row_parent);
                    {
                        ui.pushStyle(.{ .pref_size = column_box_size });
                        _ = ui.textBox("Variable Name");
                        _ = ui.textBox("Type");
                        _ = ui.textBox("Value");
                        _ = ui.popStyle();
                    }
                    _ = ui.popParent();

                    var var_node = session.watched_vars.first;
                    while (var_node) |node| : (var_node = node.next) {
                        const var_info = node.value;
                        const row_parent = ui.addNodeF(.{}, "###row_parent{s}", .{var_info.name}, .{ .child_layout_axis = .x });
                        row_parent.pref_size = row_size;
                        ui.pushParent(row_parent);
                        {
                            ui.pushStyle(.{ .pref_size = column_box_size });
                            _ = ui.textBoxF("{s}", .{var_info.name});
                            _ = ui.textBoxF("{s}", .{@tagName(std.meta.activeTag(var_info.value))});
                            switch (var_info.value) {
                                .Float => |value| _ = ui.textBoxF("{d}", .{value}),
                            }
                            _ = ui.popStyle();
                        }
                        _ = ui.popParent();
                    }
                }
                _ = ui.popParent();

                const add_var_parent = ui.addNode(.{}, "###add_var_parent", .{ .child_layout_axis = .x });
                add_var_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                ui.pushParent(add_var_parent);
                {
                    const var_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                    const button_sig = ui.button("Add Variable");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = var_input_size });
                    const text_sig = ui.textInput("add_var_textinput", &backing_buf_var, &text_buf_var.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();
                    if (button_sig.clicked or text_sig.enter_pressed) try session.addWatchedVariable(text_buf_var);
                }
                _ = ui.popParent();

                ui.spacer(.y, Size.percent(1, 0));

                const button_size = [2]Size{ Size.percent(0.5, 1), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = button_size });
                if (ui.button("Continue Running").clicked) session.unpause();
                if (ui.button("Pause Child").clicked) session.pause();
                if (ui.button("Next Line").clicked) try session.stepLine();
                if (ui.button("Next Instruction").clicked) try session.stepInstructions(1);
                _ = ui.popStyle();

                const set_break_parent = ui.addNode(.{}, "###set_break_parent", .{ .child_layout_axis = .x });
                set_break_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                ui.pushParent(set_break_parent);
                {
                    if (ui.button("Set Breakpoint").clicked) {
                        const line = try std.fmt.parseUnsigned(u32, text_buf_num, 0);
                        try session.setBreakpointAtSrc(.{ .dir = "src", .file = text_buf_file, .line = line, .column = 0 });
                    }

                    const line_input_size = [2]Size{ Size.percent(0.5, 0.25), Size.text_dim(1) };
                    _ = ui.textBox("Line Number");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = line_input_size });
                    _ = ui.textInput("src_linenum_textinput", &backing_buf_num, &text_buf_num.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();

                    const file_input_size = [2]Size{ Size.percent(0.5, 0.5), Size.text_dim(1) };
                    _ = ui.textBox("File Name");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = file_input_size });
                    _ = ui.textInput("src_filename_textinput", &backing_buf_file, &text_buf_file.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();
                }
                _ = ui.popParent();
            }
        }
        _ = ui.popParent(); // right_side_parent

        // update src viewing window with the next session information
        if (session_opt) |session| {
            if (session.src_loc) |loc| try file_tab.focusOnSrc(loc);
        }

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        file_tab.updateAnimation(dt);

        ui.endFrame(dt);
        window.update();
        tracy.FrameMark();
    }
}

const FileTab = struct {
    allocator: Allocator,
    files: std.ArrayList(FileInfo),
    active_file: ?usize,
    highlight_box: SrcBox,
    highlight_box_target: SrcBox,

    pub const SrcBox = struct {
        file_idx: usize,
        /// these are floats so we can smoothly transition between two boxes
        min: struct { line: f32, column: f32 },
        max: struct { line: f32, column: f32 },
    };

    const FileInfo = struct {
        path: []const u8,
        content: []const u8,
        lock_line: ?usize,
    };

    pub fn init(allocator: Allocator) FileTab {
        return .{
            .allocator = allocator,
            .files = std.ArrayList(FileInfo).init(allocator),
            .active_file = null,
            .highlight_box = .{
                .file_idx = undefined,
                .min = .{ .line = 0, .column = 0 },
                .max = .{ .line = 0, .column = 0 },
            },
            .highlight_box_target = .{
                .file_idx = undefined,
                .min = .{ .line = 0, .column = 0 },
                .max = .{ .line = 0, .column = 0 },
            },
        };
    }

    pub fn deinit(self: FileTab) void {
        for (self.files.items) |file_info| {
            self.allocator.free(file_info.path);
            self.allocator.free(file_info.content);
        }
        self.files.deinit();
    }

    /// if file was already open, this does nothing
    pub fn addFile(self: *FileTab, path: []const u8) !void {
        for (self.files.items) |file_info| {
            if (std.mem.eql(u8, path, file_info.path)) return;
        }
        const content = std.fs.cwd().readFileAlloc(self.allocator, path, std.math.maxInt(usize)) catch |err| {
            if (err == error.FileNotFound) std.debug.print("couldn't open file @ path '{s}'\n", .{path});
            return err;
        };
        errdefer self.allocator.free(content);
        const dupe_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(dupe_path);
        try self.files.append(.{ .path = dupe_path, .content = content, .lock_line = null });
    }

    /// if file was already open, this does nothing
    pub fn addFileFromSrc(self: *FileTab, src: SrcLoc) !void {
        const path = try std.fs.path.join(self.allocator, &.{ src.dir, src.file });
        defer self.allocator.free(path);
        try self.addFile(path);
    }

    /// if the file was not open already we try to open it now
    pub fn focusOnSrc(self: *FileTab, src: SrcLoc) !void {
        const path = try std.fs.path.join(self.allocator, &.{ src.dir, src.file });
        defer self.allocator.free(path);

        try self.addFile(path);
        const file_idx = self.findFile(path) orelse unreachable;

        self.files.items[file_idx].lock_line = src.line;
        //self.active_file = file_idx;

        self.highlight_box_target = .{
            .file_idx = file_idx,
            .min = .{ .line = @intToFloat(f32, src.line), .column = @intToFloat(f32, src.column) },
            .max = .{ .line = @intToFloat(f32, src.line), .column = @intToFloat(f32, src.column) },
        };
    }

    pub fn findFile(self: FileTab, path: []const u8) ?usize {
        for (self.files.items) |file_info, i| {
            if (std.mem.eql(u8, path, file_info.path)) return i;
        }
        return null;
    }

    pub fn findFileFromSrc(self: FileTab, src: SrcLoc) ?usize {
        const path = try std.fs.path.join(self.allocator, &.{ src.dir, src.file });
        defer self.allocator.free(path);
        return self.findFile(path);
    }

    pub fn updateAnimation(self: *FileTab, dt: f32) void {
        const fast_rate = 1 - std.math.pow(f32, 2, -20.0 * dt);
        const target = self.highlight_box_target;
        const cur = &self.highlight_box;
        cur.file_idx = target.file_idx;
        cur.min.line += (target.min.line - cur.min.line) * fast_rate;
        cur.min.column += (target.min.column - cur.min.column) * fast_rate;
        cur.max.line += (target.max.line - cur.max.line) * fast_rate;
        cur.max.column += (target.max.column - cur.max.column) * fast_rate;
    }

    pub fn display(self: *FileTab, ui: *UiContext) !void {
        const file_tab_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
        const file_tab_node = ui.addNode(.{}, "FileTab:top_node", .{ .pref_size = file_tab_size });
        ui.pushParent(file_tab_node);

        const buttons_parent_size = [2]Size{ Size.percent(1, 0), Size.by_children(1) };
        const buttons_parent = ui.addNode(.{
            .draw_background = true,
        }, "FileTab:buttons_parent", .{ .pref_size = buttons_parent_size, .child_layout_axis = .x });
        ui.pushParent(buttons_parent);
        for (self.files.items) |file_info, i| {
            const filename = std.fs.path.basename(file_info.path);
            const highlight_color = app_style.highlight_color;
            const is_active = self.active_file != null and self.active_file.? == i;
            if (is_active) ui.pushStyle(.{ .bg_color = highlight_color });
            const sig = ui.button(filename);
            buttons_parent.last.?.pref_size[0] = Size.text_dim(0);
            if (sig.clicked) self.active_file = i;
            if (is_active) _ = ui.popStyle();
        }
        _ = ui.popParent(); // buttons parent

        const active_name = if (self.active_file) |idx| self.files.items[idx].path else "";
        const active_content = if (self.active_file) |idx| self.files.items[idx].content else "";
        const active_line = if (self.active_file) |idx| self.files.items[idx].lock_line else null;

        const highlight_box = if (self.active_file == self.highlight_box.file_idx)
            self.highlight_box
        else
            null;

        if (self.active_file) |_| {
            // line + text parent
            const file_box_parent_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
            const file_box_parent = ui.addNode(.{
                .clip_children = true,
                .draw_border = true,
            }, "FileTab:text_box_parent", .{});
            file_box_parent.pref_size = file_box_parent_size;
            file_box_parent.child_layout_axis = .x;
            ui.pushParent(file_box_parent);

            _ = ui.scrollableRegion("###file_tab_line_scroll", .y);
            {
                // TODO: what about files with more than 1k lines?
                var tmpbuf: [50_000]u8 = undefined;
                var i: usize = 1;
                var offset: usize = 0;
                while (i < 1000) : (i += 1) {
                    offset += (try std.fmt.bufPrint(tmpbuf[offset..], "{}\n", .{i})).len;
                }
                const lines_text = tmpbuf[0..offset];

                ui.label(lines_text);
            }
            const line_scrollable = ui.popParent();
            line_scrollable.pref_size[0] = Size.by_children(1);

            const text_box_size = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };
            ui.pushStyle(.{ .pref_size = text_box_size });
            try textDisplay(ui, active_name, active_content, active_line, highlight_box);
            _ = ui.popStyle();

            // very hack and fragile
            const scrolling_node =
                file_box_parent.last.? // text parent
                .first.? // scrollable in x
                .first.? // scrollable in x (spacer)
                .next.?; // scrollable in y
            line_scrollable.scroll_offset[1] = scrolling_node.scroll_offset[1];

            _ = ui.popParent(); // file_box_parent (line + text parent)

        }

        _ = ui.popParent(); // file tab parent
    }
};

// line 1 to 1 is the first line
fn textDisplay(ui: *UiContext, label: []const u8, text: []const u8, lock_line: ?usize, highlight_box: ?FileTab.SrcBox) !void {
    const parent = ui.addNodeF(.{
        .clip_children = true,
        //.draw_border = true,
    }, "###{s}::parent", .{label}, .{ .child_layout_axis = .y });
    ui.pushParent(parent);

    _ = ui.scrollableRegionF("###{s}::text_scroll_region_x", .{label}, .x);
    const x_scroll = ui.topParent();
    _ = ui.scrollableRegionF("###{s}::text_scroll_region_y", .{label}, .y);
    const y_scroll = ui.topParent();

    const x_off = &x_scroll.scroll_offset[0];
    const y_off = &y_scroll.scroll_offset[1];
    const line_size = ui.font.getScaledMetrics().line_advance;

    ui.label(text);
    const label_node = y_scroll.last.?;

    if (lock_line) |line| {
        y_off.* = -line_size * @intToFloat(f32, line) + parent.rect.size()[1] / 2;
    }

    // hack to cut off scrolling at the ends of text
    const text_rect = label_node.text_rect;
    const text_size = text_rect.max - text_rect.min;
    const text_padd = vec2{ UiContext.text_hpadding, UiContext.text_vpadding };
    var max_offset = text_size - parent.rect.size() + vec2{ 2, 2 } * text_padd;
    max_offset = vec2{ std.math.max(max_offset[0], 0), std.math.max(max_offset[1], 0) };
    x_off.* = std.math.min(x_off.*, 0);
    x_off.* = std.math.max(x_off.*, -max_offset[0]);
    y_off.* = std.math.min(y_off.*, 0);
    y_off.* = std.math.max(y_off.*, -max_offset[1]);

    if (highlight_box) |box| blk: {
        if (box.min.line == 0 and box.max.line == 0) break :blk;

        const ui_style = ui.topStyle();
        var box_node = @as(UiContext.Node, undefined);
        box_node.flags = UiContext.Flags{ .draw_border = true };
        box_node.border_color = ui_style.border_color;
        box_node.corner_roundness = 0;
        box_node.border_thickness = ui_style.border_thickness;
        box_node.pref_size = undefined; // can't set to zero
        box_node.clip_rect = ui.topParent().clip_rect;
        const text_y_start = parent.rect.max[1] - y_off.*;
        const line_y_start = std.math.max(0, box.min.line - 1) * line_size;
        const box_y_top = text_y_start - (line_y_start + text_padd[1]);
        const line_y_size = std.math.max(1, box.max.line - box.min.line);
        const box_y_size = line_size * line_y_size;
        box_node.rect = UiContext.Rect{
            .min = vec2{ parent.rect.min[0], box_y_top - box_y_size },
            .max = vec2{ parent.rect.max[0], box_y_top },
        };
        try ui.on_top_nodes.append(box_node);
    }

    _ = ui.popParent();
    _ = ui.popParent();

    _ = ui.popParent();
}

fn get_proc_address_fn(window: ?*c.GLFWwindow, proc_name: [:0]const u8) ?*const anyopaque {
    _ = window;
    const fn_ptr = c.glfwGetProcAddress(proc_name.ptr);
    // without this I got a "cast discards const qualifier" error
    return @intToPtr(?*const anyopaque, @ptrToInt(fn_ptr));
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

pub fn reachedHere(src: std.builtin.SourceLocation) void {
    std.debug.print("reached {s}:{s}:{}:{}\n", src);
}
