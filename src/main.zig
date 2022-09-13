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
    // zig fmt: off
    pub const cancel        = utf8LitFromCodepoint(59392);
    pub const th_list       = utf8LitFromCodepoint(59393);
    pub const search        = utf8LitFromCodepoint(59394);
    pub const plus_circled  = utf8LitFromCodepoint(59395);
    pub const cog           = utf8LitFromCodepoint(59396);
    pub const ok            = utf8LitFromCodepoint(59397);
    pub const circle        = utf8LitFromCodepoint(61713);
    pub const up_open       = utf8LitFromCodepoint(59398);
    pub const right_open    = utf8LitFromCodepoint(59399);
    pub const left_open     = utf8LitFromCodepoint(59400);
    pub const down_open     = utf8LitFromCodepoint(59401);
    pub const plus_squared  = utf8LitFromCodepoint(61694);
    pub const minus_squared = utf8LitFromCodepoint(61766);
    pub const plus          = utf8LitFromCodepoint(59402);
    // zig fmt: on

    fn utf8Len(comptime codepoint: u21) u3 {
        return std.unicode.utf8CodepointSequenceLength(codepoint) catch unreachable;
    }
    fn utf8LitFromCodepoint(comptime codepoint: u21) *const [utf8Len(codepoint):0]u8 {
        comptime {
            var buf: [utf8Len(codepoint):0]u8 = undefined;
            _ = std.unicode.utf8Encode(codepoint, &buf) catch unreachable;
            buf[buf.len] = 0;
            return &buf;
        }
    }
};

var show_ctx_menu = false;
var ctx_menu_top_left = @as(vec2, undefined);

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    const allocator = general_purpose_allocator.allocator();
    //const allocator = std.heap.c_allocator;

    const arg_slices = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, arg_slices);
    const cmdline_args = parseCmdlineArgs(arg_slices);

    c.xed_tables_init();

    var width: u32 = 1600;
    var height: u32 = 900;
    // setup GLFW
    var window = Window.init(allocator, width, height, "pestctl");
    window.finish_setup();
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
    var last_session_status: Session.Status = if (session_opt) |s| s.status else .Stopped;

    var last_mouse_pos = vec2{ 0, 0 };
    var last_time = @floatCast(f32, c.glfwGetTime());

    var src_file_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var num_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var file_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var var_buf = try std.BoundedArray(u8, 0x1000).init(0);

    var file_tab = FileTab.init(allocator);
    defer file_tab.deinit();

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
                            node.display_string[0..std.math.min(10, node.display_string.len)],
                            node.hash_string[0..std.math.min(10, node.hash_string.len)],
                            if (node.parent) |ptr| @ptrToInt(ptr) else 0,
                            if (node.first) |ptr| @ptrToInt(ptr) else 0,
                            if (node.next) |ptr| @ptrToInt(ptr) else 0,
                            node.rect,
                        });
                    }
                }
            }
        }

        var update_src_focus = false;

        try ui.startBuild(width, height, mouse_pos, &window.event_queue);

        const whole_x_size = [2]Size{ Size.percent(1, 1), Size.text_dim(1) };
        ui.pushTmpStyle(.{ .pref_size = whole_x_size });
        _ = ui.textBoxF("#nodes={}, frame_time={d:2.4}ms###info_text_box", .{ ui.node_table.key_mappings.items.len, dt * 1000 });

        const tabs_parent = ui.addNode(.{}, "###tabs_parent", .{ .child_layout_axis = .x });
        tabs_parent.pref_size = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };
        ui.pushParent(tabs_parent);

        const left_side_parent = ui.addNode(.{
            .draw_border = true,
            .draw_background = true,
        }, "###left_side_parent", .{ .child_layout_axis = .y });
        left_side_parent.pref_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushParent(left_side_parent);
        {
            const open_file_parent = ui.addNode(.{}, "###open_file_parent", .{ .child_layout_axis = .x });
            open_file_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
            ui.pushParent(open_file_parent);
            {
                const open_button_sig = ui.button("Open Source File");
                _ = open_button_sig;
                const text_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = text_input_size });
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                const text_input_sig = ui.textInput("textinput", &src_file_buf.buffer, &src_file_buf.len);
                _ = ui.popStyle();
                _ = ui.popStyle();
                if (src_file_buf.len > 0 and (open_button_sig.clicked or text_input_sig.enter_pressed)) {
                    if (file_tab.addFile(src_file_buf.slice())) {
                        file_tab.active_file = file_tab.files.items.len - 1;
                    } else |err| return err;
                }
            }
            std.debug.assert(ui.popParent() == open_file_parent);

            try file_tab.display(&ui, if (session_opt) |*s| s else null);

            // testing xed disassembly
            if (session_opt) |session| {
                //const tmp_size = [2]Size{ Size.percent(1, 1), Size.percent(0.35, 1) };
                const tmp_size = [2]Size{ Size.percent(1, 1), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = tmp_size, .text_align = .center });
                if (session.addr_range) |range| {
                    var buf = try std.BoundedArray(u8, 0x1000).init(0);
                    const rip = session.regs.rip;
                    var asm_addr = range[0];
                    while (asm_addr < range[1]) {
                        var inst_bytes: [16]u8 = undefined;
                        for (session.getBytesAtAddr(asm_addr, 8)) |byte, i| inst_bytes[i] = byte;
                        for (session.getBytesAtAddr(asm_addr + 8, 8)) |byte, i| inst_bytes[8 + i] = byte;

                        var decoded_inst: c.xed_decoded_inst_t = undefined;
                        c.xed_decoded_inst_zero(&decoded_inst);
                        c.xed_decoded_inst_set_mode(&decoded_inst, c.XED_MACHINE_MODE_LONG_64, c.XED_ADDRESS_WIDTH_64b);
                        switch (c.xed_decode(&decoded_inst, &inst_bytes[0], inst_bytes.len)) {
                            c.XED_ERROR_NONE => {},
                            else => |err| std.debug.panic("got error code from xed_decode: {s}\n", .{c.xed_error_enum_t2str(err)}),
                        }
                        var write_buf = buf.buffer[buf.len..];
                        const format_res = c.xed_format_context(c.XED_SYNTAX_INTEL, &decoded_inst, write_buf.ptr, @intCast(c_int, write_buf.len), rip, null, null);
                        std.debug.assert(format_res == 1);
                        const strlen = c.strlen(write_buf.ptr);
                        const str = write_buf[0..strlen];

                        _ = ui.textBox(str);

                        asm_addr += c.xed_decoded_inst_get_length(&decoded_inst);
                        buf.len += strlen + 1;
                    }
                } else {
                    _ = ui.textBox("TODO: assembly window");
                }
                _ = ui.popStyle();
            }
        }
        std.debug.assert(ui.popParent() == left_side_parent);

        const right_side_parent = ui.addNode(.{ .draw_background = true }, "###right_side_parent", .{ .child_layout_axis = .y });
        right_side_parent.pref_size = [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) };
        ui.pushParent(right_side_parent);
        {
            if (session_opt) |*session| {
                _ = ui.textBoxF("Child pid: {}", .{session.pid});
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
                switch (session.status) {
                    .Stopped => {
                        _ = ui.textBox("Child Status: Stopped");
                        ui.topParent().last.?.text_color = vec4{ 1, 0.5, 0, 1 };
                    },
                    .Running => _ = ui.textBox("Child Status: Running"),
                }
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
                _ = ui.textBoxF("wait_status: 0x{x}", .{session.wait_status});
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
                if (session.src_loc) |loc| {
                    _ = ui.textBoxF("src_loc: {s}:{}", .{ loc.file, loc.line });
                } else _ = ui.textBox("src_loc: null");
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);

                const table_regs = .{ "rax", "rcx", "rbx", "rdx", "rip", "rsp" };
                const regs = session.regs;
                inline for (table_regs) |reg_name| {
                    _ = ui.textBoxF(reg_name ++ ": 0x{x:0>16}", .{@field(regs, reg_name)});
                }

                if (session.addr_range) |range| {
                    _ = ui.textBoxF("0x{x:0>12} -> 0x{x:0>12}", .{ range[0], range[1] });
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
                    std.debug.assert(ui.popParent() == table_header_row_parent);

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
                        std.debug.assert(ui.popParent() == row_parent);
                    }
                }
                std.debug.assert(ui.popParent() == vars_parent);

                const add_var_parent = ui.addNode(.{}, "###add_var_parent", .{ .child_layout_axis = .x });
                add_var_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                ui.pushParent(add_var_parent);
                {
                    const var_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                    const button_sig = ui.button("Add Variable");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = var_input_size });
                    const text_sig = ui.textInput("add_var_textinput", &var_buf.buffer, &var_buf.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();
                    if (button_sig.clicked or text_sig.enter_pressed) try session.addWatchedVariable(&var_buf.buffer);
                }
                std.debug.assert(ui.popParent() == add_var_parent);

                ui.spacer(.y, Size.percent(1, 0));

                const button_size = [2]Size{ Size.percent(0.5, 1), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = button_size });
                if (ui.button("Continue Running").clicked) {
                    session.unpause();
                    update_src_focus = true;
                }
                if (ui.button("Pause Child").clicked) session.pause();
                if (ui.button("Next Line").clicked) {
                    try session.stepLine();
                    update_src_focus = true;
                }
                if (ui.button("Next Instruction").clicked) {
                    try session.stepInstructions(1);
                    update_src_focus = true;
                }
                _ = ui.popStyle();

                const set_break_parent = ui.addNode(.{}, "###set_break_parent", .{ .child_layout_axis = .x });
                set_break_parent.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                ui.pushParent(set_break_parent);
                {
                    const button_sig = ui.button("Set Breakpoint");

                    const line_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                    _ = ui.textBox("Line Number");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = line_input_size });
                    const line_sig = ui.textInput("src_linenum_textinput", &num_buf.buffer, &num_buf.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();

                    const file_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                    _ = ui.textBox("File Name");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = file_input_size });
                    const file_sig = ui.textInput("src_filename_textinput", &file_buf.buffer, &file_buf.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();

                    if (button_sig.clicked or line_sig.enter_pressed or file_sig.enter_pressed) blk: {
                        const line = std.fmt.parseUnsigned(u32, num_buf.slice(), 0) catch |err| {
                            std.debug.print("{s}: couldn't parse break line number: '{s}'\n", .{ @errorName(err), num_buf.slice() });
                            break :blk;
                        };
                        session.setBreakpointAtSrc(.{ .dir = "src", .file = file_buf.slice(), .line = line, .column = 0 }) catch |err| {
                            std.debug.print("{s}: couldn't set breakpoint at src/{s}:{}\n", .{ @errorName(err), file_buf.slice(), line });
                            break :blk;
                        };
                    }
                }
                std.debug.assert(ui.popParent() == set_break_parent);

                if (last_session_status != session.status) {
                    update_src_focus = true;
                }
                last_session_status = session.status;

                try session.fullUpdate();
            }
        }
        std.debug.assert(ui.popParent() == right_side_parent);

        std.debug.assert(ui.popParent() == tabs_parent);

        ui.endBuild(dt);

        // update src viewing window with the next session information
        if (update_src_focus) {
            if (session_opt) |*session| {
                if (session.src_loc) |loc| try file_tab.focusOnSrc(loc);
            }
        }

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        file_tab.updateAnimations(dt);

        window.update();
        tracy.FrameMark();
    }
}

const FileTab = struct {
    allocator: Allocator,
    files: std.ArrayList(FileInfo),
    active_file: ?usize,

    pub const SrcBox = struct {
        /// these are floats so we can smoothly transition between two boxes
        min: struct { line: f32, column: f32 },
        max: struct { line: f32, column: f32 },
    };

    const FileInfo = struct {
        path: []const u8,
        content: []const u8,
        lock_line: ?f32,
        target_lock_line: ?f32,
        focus_box: ?SrcBox,
        target_focus_box: ?SrcBox,
    };

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
        try self.files.append(.{
            .path = dupe_path,
            .content = content,
            .lock_line = null,
            .target_lock_line = null,
            .focus_box = null,
            .target_focus_box = null,
        });
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

        self.files.items[file_idx].target_lock_line = @intToFloat(f32, src.line);
        self.files.items[file_idx].target_focus_box = .{
            .min = .{ .line = @intToFloat(f32, src.line), .column = @intToFloat(f32, src.column) },
            .max = .{ .line = @intToFloat(f32, src.line), .column = @intToFloat(f32, src.column) },
        };
        self.active_file = file_idx;
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

    pub fn display(self: *FileTab, ui: *UiContext, session_opt: ?*Session) !void {
        _ = session_opt;

        const file_tab_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
        const file_tab_node = ui.addNode(.{
            .draw_border = true,
        }, "FileTab:top_node", .{ .pref_size = file_tab_size });
        ui.pushParent(file_tab_node);

        const buttons_parent_size = [2]Size{ Size.percent(1, 0), Size.by_children(1) };
        const buttons_parent = ui.addNode(.{}, "FileTab:buttons_parent", .{
            .pref_size = buttons_parent_size,
            .child_layout_axis = .x,
        });
        ui.pushParent(buttons_parent);
        for (self.files.items) |file_info, i| {
            const filename = std.fs.path.basename(file_info.path);
            const highlight_color = if (file_info.focus_box) |_| app_style.highlight_color else vec4{ 1, 0, 0, 1 };
            const is_active = self.active_file != null and self.active_file.? == i;
            if (is_active) ui.pushTmpStyle(.{ .bg_color = highlight_color });
            const sig = ui.button(filename);
            buttons_parent.last.?.pref_size[0] = Size.text_dim(0);
            if (sig.clicked) self.active_file = i;
        }
        if (ui.subtleIconButton(Icons.plus).clicked) std.debug.print("TODO: open file menu\n", .{});
        _ = ui.popParent(); // buttons parent

        const active_name = if (self.active_file) |idx| self.files.items[idx].path else "";
        const active_content = if (self.active_file) |idx| self.files.items[idx].content else "";

        if (self.active_file) |file_idx| {
            const file = &self.files.items[file_idx];

            // line + text parent
            const file_box_parent_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
            const file_box_parent = ui.addNode(.{
                .clip_children = true,
            }, "FileTab:text_box_parent", .{});
            file_box_parent.pref_size = file_box_parent_size;
            file_box_parent.child_layout_axis = .x;
            ui.pushParent(file_box_parent);

            const line_scroll_size = [2]Size{ Size.by_children(1), Size.percent(1, 0) };
            const line_scroll_parent = ui.addNodeF(.{
                .scrollable = true,
                .clip_children = true,
            }, "###{s}::line_scroll_parent", .{active_name}, .{ .pref_size = line_scroll_size });
            _ = ui.getNodeSignal(line_scroll_parent);
            ui.pushParent(line_scroll_parent);
            const line_text_node = blk: {
                // TODO: what about files with more than 1k lines?
                var tmpbuf: [50_000]u8 = undefined;
                var i: usize = 1;
                var offset: usize = 0;
                while (i < 1000) : (i += 1) {
                    offset += (try std.fmt.bufPrint(tmpbuf[offset..], "{}\n", .{i})).len;
                }
                const lines_text = tmpbuf[0..offset];

                const line_text_node = ui.addNode(.{
                    .no_id = true,
                    .draw_text = true,
                    .floating_y = true,
                }, lines_text, .{});
                line_text_node.rel_pos_placement = .top_left;
                line_text_node.rel_pos_placement_parent = .top_left;

                break :blk line_text_node;
            };
            _ = ui.popParent(); // line_scroll_parent

            try textDisplay(ui, active_name, active_content, &file.lock_line, &file.target_lock_line, file.focus_box);
            const text_scroll_node = file_box_parent.last.?;

            const text_node_rect = text_scroll_node.first.?.rect;
            const right_click = ui.events.searchAndRemove(.MouseUp, c.GLFW_MOUSE_BUTTON_RIGHT);
            if (right_click and text_node_rect.contains(ui.mouse_pos)) {
                show_ctx_menu = true;
                ctx_menu_top_left = ui.mouse_pos;
            }
            if (show_ctx_menu) {
                ui.startCtxMenu(.{ .top_left = ctx_menu_top_left });
                const ctx_menu_node = ui.topParent();

                if (ui.button("the missile").clicked) std.debug.print("missile\n", .{});

                if (ui.events.match(.MouseDown, {})) |_| {
                    if (!ctx_menu_node.rect.contains(ui.mouse_pos)) show_ctx_menu = false;
                }

                ui.endCtxMenu();
            }

            // scroll the line numbers with the src text
            line_scroll_parent.scroll_offset[1] = text_scroll_node.scroll_offset[1];
            line_text_node.rel_pos[1] = -line_scroll_parent.scroll_offset[1];

            _ = ui.popParent(); // file_box_parent (line + text parent)

        }

        _ = ui.popParent(); // file tab parent
    }

    pub fn updateAnimations(self: *FileTab, dt: f32) void {
        const fast_rate = 1 - std.math.pow(f32, 2, -20.0 * dt);
        for (self.files.items) |*file| {
            if (file.target_lock_line) |target_line| {
                if (file.lock_line) |*cur_line| {
                    cur_line.* += (target_line - cur_line.*) * fast_rate;
                } else file.lock_line = target_line;
            }
            if (file.target_focus_box) |target_box| {
                if (file.focus_box) |*cur_box| {
                    cur_box.min.line += (target_box.min.line - cur_box.min.line) * fast_rate;
                    cur_box.min.column += (target_box.min.column - cur_box.min.column) * fast_rate;
                    cur_box.max.line += (target_box.max.line - cur_box.max.line) * fast_rate;
                    cur_box.max.column += (target_box.max.column - cur_box.max.column) * fast_rate;
                } else file.focus_box = target_box;
            }
        }
    }
};

fn textDisplay(ui: *UiContext, label: []const u8, text: []const u8, lock_line: *?f32, target_lock_line: *?f32, highlight_box: ?FileTab.SrcBox) !void {
    const text_box_size = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };
    const parent = ui.addNodeF(.{
        .scrollable = true,
        .clip_children = true,
    }, "###{s}::parent", .{label}, .{ .child_layout_axis = .y, .pref_size = text_box_size });
    const parent_sig = ui.getNodeSignal(parent);

    ui.pushParent(parent);
    defer std.debug.assert(ui.popParent() == parent);

    const parent_size = parent.rect.size();
    const line_size = ui.font.getScaledMetrics().line_advance;

    // if we manually scroll the text stop the line locking
    if (parent_sig.scroll_offset[0] != 0 or parent_sig.scroll_offset[1] != 0) {
        lock_line.* = null;
        target_lock_line.* = null;
    }

    const x_off = &parent.scroll_offset[0];
    const y_off = &parent.scroll_offset[1];
    if (lock_line.*) |line| {
        y_off.* = -line_size * line + parent_size[1] / 2;
    }

    const lines_that_fit = @trunc(parent.rect.size()[1] / line_size);
    const cur_middle_line = @trunc(std.math.max(0, -y_off.* + parent_size[1] / 2) / line_size);
    const partial_start_line = @floatToInt(usize, @trunc(std.math.max(0, cur_middle_line - lines_that_fit)));
    const partial_end_line = @floatToInt(usize, @trunc(cur_middle_line + lines_that_fit));

    var total_lines: usize = 1;
    var partial_start_idx: usize = 0;
    var partial_end_idx: usize = text.len;
    var last_newline: usize = 0;
    for (text) |char, i| {
        if (char != '\n') continue;
        if (total_lines <= partial_start_line) partial_start_idx = i + 1;
        if (total_lines <= partial_end_line) partial_end_idx = i + 1;
        total_lines += 1;
        last_newline = i;
    }
    if (text.len == 0) total_lines = 0;

    const partial_text = text[partial_start_idx..partial_end_idx];

    const label_node = ui.addNodeStrings(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
        .floating_x = true,
        .floating_y = true,
    }, partial_text, label, .{});

    // hack to cut off scrolling at the ends of text
    const text_size = vec2{ label_node.text_rect.size()[0], line_size * @intToFloat(f32, total_lines) };
    var max_offset = text_size - parent_size + vec2{ 2, 2 } * UiContext.text_padd;
    max_offset = vec2{ std.math.max(max_offset[0], 0), std.math.max(max_offset[1], 0) };
    x_off.* = std.math.min(x_off.*, 0);
    x_off.* = std.math.max(x_off.*, -max_offset[0]);
    y_off.* = std.math.min(y_off.*, 0);
    y_off.* = std.math.max(y_off.*, -max_offset[1]);

    label_node.rel_pos_placement = .top_left;
    label_node.rel_pos_placement_parent = .top_left;
    label_node.rel_pos = vec2{ x_off.*, -y_off.* - @intToFloat(f32, partial_start_line) * line_size };

    if (highlight_box) |box| blk: {
        if (box.min.line == 0 and box.max.line == 0) break :blk;

        const text_y_start = parent.rect.size()[1] - y_off.*;
        const line_y_start = std.math.max(0, box.min.line - 1) * line_size;
        const box_y_top = text_y_start - line_y_start - UiContext.text_padd[1];
        const box_y_size = std.math.max(1, box.max.line - box.min.line) * line_size;

        const box_node = ui.addNode(.{
            .no_id = true,
            .draw_border = true,
            .floating_y = true,
        }, "", .{});
        box_node.pref_size = [2]Size{ Size.percent(1, 1), Size.pixels(box_y_size, 1) };
        box_node.rel_pos[1] = box_y_top - box_y_size;
    }
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
