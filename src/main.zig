const std = @import("std");
const Allocator = std.mem.Allocator;
const clamp = std.math.clamp;
const c = @import("c.zig");
const zig_ui = @import("zig-ui");
const gl = zig_ui.gl;
const vec2 = zig_ui.vec2;
const vec4 = zig_ui.vec4;
const glfw = zig_ui.glfw;
const Window = zig_ui.Window;
const UI = zig_ui.UI;
const Icons = UI.Icons;
const Size = UI.Size;
const Session = @import("Session.zig");
const Elf = @import("Elf.zig");
const Dwarf = @import("Dwarf.zig");
const SrcLoc = Dwarf.SrcLoc;
const widgets = @import("app_widgets.zig");
const FuzzyMatcher = @import("fuzzy_matching.zig").FuzzyMatcher;

const tracy = @import("tracy.zig");

const app_style = .{
    .highlight_color = vec4{ 0, 0, 0.5, 1 },
};
const text_input_style = .{
    .size = [2]Size{ Size.percent(1, 0), Size.text(1) },
    .bg_color = vec4{ 0.75, 0.75, 0.75, 1 },
};
const fill_x_size = Size.fillAxis(.x, Size.children(1));

pub const CmdlineArgs = struct {
    exec_path: ?[]const u8 = null,
    font_scale: ?f32 = null,

    pub fn parse(arg_slices: [][:0]const u8) CmdlineArgs {
        var args = CmdlineArgs{};

        var arg_idx: usize = 1;
        while (arg_idx < arg_slices.len) : (arg_idx += 1) {
            const arg = arg_slices[arg_idx];

            if (std.mem.eql(u8, arg, "--font-scale")) {
                arg_idx += 1;
                const scale_arg = arg_slices[arg_idx];
                args.font_scale = std.fmt.parseFloat(f32, scale_arg) catch
                    std.debug.panic("invalid float arg for `--font-scale`: '{s}'\n", .{scale_arg});
            } else {
                if (args.exec_path) |path| {
                    std.debug.panic("multiple executable passed in arguments: '{s}' and '{s}'\n", .{
                        path, arg,
                    });
                }
                args.exec_path = arg;
            }
        }

        return args;
    }
};

pub const SessionCmd = union(enum) {
    open_src_file: []const u8, // could be relative or absolute path
    set_break_at_src: SrcLoc,
    set_break_at_addr: usize,
    add_watched_variable: Dwarf.Variable,
    continue_execution: void,
    pause_execution: void,
    step_line: void,
    step_instruction: void,
    dump_ui_tree: []const u8, // path of graphviz save file
};

var show_ctx_menu = false;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    const allocator = general_purpose_allocator.allocator();
    //const allocator = std.heap.c_allocator;

    var cwd_buf: [0x4000]u8 = undefined;
    const cwd = try std.posix.getcwd(&cwd_buf);

    const arg_slices = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, arg_slices);
    const cmdline_args = CmdlineArgs.parse(arg_slices);

    var width: u32 = 1600;
    var height: u32 = 900;
    var window = try Window.init(allocator, width, height, "pestctl");
    window.finishSetup();
    defer window.deinit();
    const clear_color = vec4{ 0.75, 0.36, 0.38, 1 };

    var ui = try UI.init(allocator, .{});
    defer ui.deinit();
    // var dbg_ui_view = try UI.DebugView.init(allocator);
    // defer dbg_ui_view.deinit();

    ui.base_style.bg_color = vec4{ 0.24, 0.27, 0.31, 1 };
    ui.base_style.border_color = vec4{ 0.5, 0.5, 0.5, 0.75 };
    ui.base_style.text_color = vec4{ 1, 1, 1, 1 };
    ui.base_style.edge_softness = 1;
    ui.base_style.border_thickness = 2;
    if (cmdline_args.font_scale) |font_scale| {
        ui.base_style.font_size *= font_scale;
    }

    var session_opt = if (cmdline_args.exec_path) |path| try Session.init(allocator, path) else null;
    defer if (session_opt) |*session| session.deinit();

    var last_time: f32 = @floatCast(glfw.getTime());

    var breakpoint_widget = try BreakpointWidget.init(allocator);
    defer breakpoint_widget.deinit();
    var file_picker: ?widgets.FilePicker = null;
    defer if (file_picker) |picker| picker.deinit();

    var src_file_backing_buf: [1000]u8 = undefined;
    var src_file_input = UI.TextInputState.init(&src_file_backing_buf, "");
    var src_file_search = FuzzySearchOptions(SrcFileSearchCtx, 20).init(allocator);
    defer src_file_search.deinit();
    var var_backing_buf: [1000]u8 = undefined;
    var var_input = UI.TextInputState.init(&var_backing_buf, "");
    var var_search = FuzzySearchOptions(VarSearchCtx, 10).init(allocator);
    defer var_search.deinit();
    var mem_view_backing_buf: [1000]u8 = undefined;
    var mem_view_input = UI.TextInputState.init(&mem_view_backing_buf, "");
    var mem_view_addr: ?usize = null;
    var search_backing_buf: [1000]u8 = undefined;
    var search_text_input = UI.TextInputState.init(&search_backing_buf, "");
    var file_search_iter = try FileSearchIterator.init(allocator, cwd);
    defer file_search_iter.deinit();
    var test_search = FuzzyMatcher(FileSearchIterator).init(allocator, &file_search_iter);
    defer test_search.deinit();

    var show_ui_stats = true;
    var lock_framerate = true;

    var disasm_texts = std.ArrayList(AsmTextInfo).init(allocator);
    defer {
        for (disasm_texts.items) |text| text.deinit(allocator);
        disasm_texts.deinit();
    }

    var file_tab = FileTab.init(allocator);
    defer file_tab.deinit();
    var file_viewer = widgets.SourceViewer.init(allocator);
    defer file_viewer.deinit();

    var last_src_loc = @as(?SrcLoc, null);

    const Widgets = enum {
        Process,
        Breakpoints,
        Variables,
        Registers,
        @"Call Stack",
        @"Memory Maps",
        @"Memory View",
        Sources,
        Assembly,
    };
    var active_widget_left: Widgets = .Process;
    var active_widget_right: Widgets = .Registers;
    var call_stack_viewer = CallStackViewer.init(allocator);
    defer call_stack_viewer.deinit();

    var session_cmds = std.ArrayList(SessionCmd).init(allocator);
    defer session_cmds.deinit();

    var frame_idx: u64 = 0;

    while (!window.shouldClose()) {
        // std.debug.print("frame_idx={}\n", .{frame_idx});

        var frame_arena = std.heap.ArenaAllocator.init(allocator);
        defer frame_arena.deinit();

        // grab all window/input information we need for this frame
        const framebuf_size = window.getFramebufferSize();
        width = framebuf_size[0];
        height = framebuf_size[1];
        //const ratio = @intToFloat(f32, width) / @intToFloat(f32, height);
        const cur_time: f32 = @floatCast(glfw.getTime());
        const dt = cur_time - last_time;
        last_time = cur_time;
        const mouse_pos = window.getMousePos();

        try ui.startBuild(width, height, mouse_pos, &window.event_queue, &window);

        {
            const main_bar_parent = ui.pushLayoutParent(.{}, "main_bar_parent", fill_x_size, .x);
            main_bar_parent.flags.draw_background = true;
            defer ui.popParentAssert(main_bar_parent);

            if (ui.button("Open Executable").clicked and file_picker == null)
                file_picker = try widgets.FilePicker.init(allocator, cwd);
            if (file_picker) |*picker| {
                if (session_opt) |_| @panic("TODO: open multiple binaries");
                if (try picker.show(&ui)) |path| {
                    session_opt = try Session.init(allocator, path);
                    picker.deinit();
                    file_picker = null;
                }
            }

            if (ui.checkBox("lock framerate", &lock_framerate).clicked)
                glfw.swapInterval(if (lock_framerate) 1 else 0);

            _ = ui.checkBox("show UI stats", &show_ui_stats);
            if (show_ui_stats) {
                const mem_stats = try getMemoryStats(allocator);
                if (dt >= 0.020) ui.pushTmpStyle(.{ .text_color = vec4{ 1, 0, 0, 1 } });
                ui.labelF("#nodes={}, frame_time={d:2.4}ms, mem(ram/virtual/shared): {d:.2}/{d:.2}/{d:.2}, gpa requested bytes: {d:.2}", .{
                    ui.node_table.count(),
                    dt * 1000,
                    std.fmt.fmtIntSizeBin(mem_stats.in_ram_size),
                    std.fmt.fmtIntSizeBin(mem_stats.virtual_size),
                    std.fmt.fmtIntSizeBin(mem_stats.shared_size),
                    std.fmt.fmtIntSizeBin(general_purpose_allocator.total_requested_bytes),
                });
            }

            ui.spacer(.x, Size.percent(1, 0));
            ui.pushTmpStyle(.{ .bg_color = vec4{ 1, 0, 0.2, 1 } });
            if (ui.iconButton(UI.Icons.cancel).clicked) break;
        }

        const tabs_parent = ui.pushLayoutParent(.{}, "tabs_parent", Size.flexible(.percent, 1, 1), .x);

        blk: {
            const left_side_parent = ui.pushLayoutParent(.{
                .draw_border = true,
                .draw_background = true,
            }, "left_side_parent", Size.exact(.percent, 0.5, 1), .y);
            defer ui.popParentAssert(left_side_parent);

            const session = if (session_opt) |*s| s else break :blk;
            {
                const buttons_parent = ui.addNode(.{ .no_id = true }, "", .{
                    .size = fill_x_size,
                    .layout_axis = .x,
                });
                ui.pushParent(buttons_parent);
                defer ui.popParentAssert(buttons_parent);

                inline for (@typeInfo(Widgets).Enum.fields) |widget_info| {
                    const widget: Widgets = @enumFromInt(widget_info.value);
                    if (active_widget_left == widget)
                        ui.pushTmpStyle(.{ .bg_color = app_style.highlight_color });
                    const btn_sig = ui.button(widget_info.name);
                    if (btn_sig.clicked) active_widget_left = widget;
                }
            }
            switch (active_widget_left) {
                .Process => try showProcessInfo(&ui, &session_cmds, session),
                .Breakpoints => try breakpoint_widget.show(&ui, &frame_arena, &session_cmds, session, cwd),
                .Variables => try doVarTable(&frame_arena, &ui, &session_cmds, session, &var_input, &var_search),
                .Registers => showRegisters(&ui, try session.getRegisters()),
                .@"Call Stack" => try call_stack_viewer.display(&ui, session.call_stack, session.*),
                .@"Memory Maps" => try showMemoryMaps(allocator, &ui, session),
                .@"Memory View" => try doMemoryView(&ui, session, &mem_view_input, &mem_view_addr),
                .Sources => try doSourcesWidget(&frame_arena, &ui, &session_cmds, cwd, &src_file_input, &src_file_search, &file_tab),
                .Assembly => try doDisassemblyWindow(allocator, &ui, session.*, &disasm_texts),
            }
        }

        blk: {
            const right_side_parent = ui.pushLayoutParent(.{
                .draw_background = true,
            }, "right_side_parent", Size.exact(.percent, 0.5, 1), .y);
            defer ui.popParentAssert(right_side_parent);
            {
                // show input box
                _ = ui.lineInput("asdlkfjhasdf", UI.Size.percent(1, 0), &search_text_input);

                try test_search.updateSearch(search_text_input.slice());

                if (pickFuzzyMatches(&ui, &test_search, "test_search", null, null)) |entry|
                    std.debug.print("choose a search entry: {any}\n", .{entry});
            }

            const session = if (session_opt) |*s| s else break :blk;
            {
                const buttons_parent = ui.addNode(.{ .no_id = true }, "", .{
                    .size = fill_x_size,
                    .layout_axis = .x,
                });
                ui.pushParent(buttons_parent);
                defer ui.popParentAssert(buttons_parent);

                inline for (@typeInfo(Widgets).Enum.fields) |widget_info| {
                    const widget: Widgets = @enumFromInt(widget_info.value);
                    if (active_widget_right == widget)
                        ui.pushTmpStyle(.{ .bg_color = app_style.highlight_color });
                    const btn_sig = ui.button(widget_info.name);
                    if (btn_sig.clicked) active_widget_right = widget;
                }
            }
            switch (active_widget_right) {
                .Process => try showProcessInfo(&ui, &session_cmds, session),
                .Breakpoints => try breakpoint_widget.show(&ui, &frame_arena, &session_cmds, session, cwd),
                .Variables => try doVarTable(&frame_arena, &ui, &session_cmds, session, &var_input, &var_search),
                .Registers => showRegisters(&ui, try session.getRegisters()),
                .@"Call Stack" => try call_stack_viewer.display(&ui, session.call_stack, session.*),
                .@"Memory Maps" => try showMemoryMaps(allocator, &ui, session),
                .@"Memory View" => try doMemoryView(&ui, session, &mem_view_input, &mem_view_addr),
                .Sources => try doSourcesWidget(&frame_arena, &ui, &session_cmds, cwd, &src_file_input, &src_file_search, &file_tab),
                .Assembly => try doDisassemblyWindow(allocator, &ui, session.*, &disasm_texts),
            }
        }

        ui.popParentAssert(tabs_parent);

        ui.endBuild(dt);

        // special debug commands
        if (window.event_queue.searchAndRemove(.KeyUp, .{
            .mods = .{ .control = true, .shift = true },
            .key = .t,
        })) {
            try session_cmds.append(.{ .dump_ui_tree = "ui_main_tree.dot" });
        }
        // if (window.event_queue.searchAndRemove(.KeyUp, .{
        //     .mods = .{ .control = true, .shift = true },
        //     .key = .d,
        // })) dbg_ui_view.active = !dbg_ui_view.active;

        const cur_src_loc = if (session_opt) |s| s.src_loc else null;
        var focused_src_loc = cur_src_loc;
        if (last_src_loc != null and cur_src_loc != null) {
            if (SrcLoc.cmp(last_src_loc.?, cur_src_loc.?)) {
                focused_src_loc = null;
            }
        }
        last_src_loc = cur_src_loc;

        if (focused_src_loc) |src| try file_tab.focusOnSrc(src);
        file_tab.updateAnimations(dt);

        // do all the state changes in one place
        if (session_cmds.items.len > 0) std.debug.print("commands in buffer @ frame idx {}\n", .{frame_idx});
        for (session_cmds.items) |cmd| {
            std.debug.print("  - cmd: {s}\n", .{@tagName(std.meta.activeTag(cmd))});
            switch (cmd) {
                .open_src_file => |file| case_blk: {
                    if (file.len == 0) break :case_blk;
                    const path = if (std.fs.path.isAbsolute(file)) file else blk: {
                        break :blk try std.fs.path.join(frame_arena.allocator(), &.{ cwd, file });
                    };
                    if (file_tab.addFile(path)) {
                        file_tab.active_file = file_tab.files.items.len - 1;
                    } else |err| {
                        std.debug.print("{}: couldn't open file @ {s}\n", .{ err, path });
                    }
                },
                .set_break_at_addr => |addr| case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.setBreakpointAtAddr(addr);
                },
                .set_break_at_src => |src| case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.setBreakpointAtSrc(src);
                },
                .add_watched_variable => |variable| case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.watched_vars.append(variable);
                },
                .continue_execution => case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.unpause();
                },
                .pause_execution => case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.pause();
                },
                .step_line => case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.stepLine();
                },
                .step_instruction => case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.stepInstructions(1);
                },
                .dump_ui_tree => |path| {
                    std.debug.print("dumping root tree to {s}\n", .{path});
                    const dump_file = try std.fs.cwd().createFile(path, .{});
                    defer dump_file.close();
                    try ui.dumpNodeTreeGraph(ui.root.?, dump_file);
                },
            }

            // if (session_opt) |*session| try session.fullUpdate();
        }
        session_cmds.clearRetainingCapacity();

        gl.viewport(0, 0, @as(i32, @intCast(width)), @as(i32, @intCast(height)));
        gl.clearColor(clear_color[0], clear_color[1], clear_color[2], clear_color[3]);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();
        // if (dbg_ui_view.active) {
        //     try dbg_ui_view.show(&ui, width, height, mouse_pos, &window.event_queue, &window, dt);
        // }

        window.update();
        frame_idx += 1;
        tracy.FrameMark();
    }
}

fn showProcessInfo(
    ui: *UI,
    session_cmds: *std.ArrayList(SessionCmd),
    session: *Session,
) !void {
    ui.pushStyle(.{ .size = [2]Size{ Size.percent(1, 1), Size.text(1) } });
    ui.labelBoxF("Child pid: {}", .{session.pid});
    ui.labelBoxF("Child Status: {s}", .{@tagName(try session.getState())});
    if (session.src_loc) |loc| {
        ui.labelBoxF("src_loc: {s}:{}", .{ loc.file, loc.line });
    } else ui.labelBox("src_loc: null");
    _ = ui.popStyle();

    if (ui.button("Wait for Signal").clicked) {
        std.debug.print("wait status: {any}\n", .{Session.getWaitStatus(session.pid)});
    }

    if (session.addr_range) |range| {
        ui.labelBoxF("address range: 0x{x:0>12}-0x{x:0>12}", .{ range.start, range.end });
    }

    ui.pushStyle(.{ .size = [2]Size{ Size.percent(0.5, 1), Size.text(1) } });
    if (ui.button("Continue Running").clicked) {
        try session_cmds.append(.{ .continue_execution = {} });
    }
    if (ui.button("Pause Child").clicked) {
        try session_cmds.append(.{ .pause_execution = {} });
    }
    if (ui.button("Next Line").clicked) {
        try session_cmds.append(.{ .step_line = {} });
    }
    if (ui.button("Next Instruction").clicked) {
        try session_cmds.append(.{ .step_instruction = {} });
    }
    _ = ui.popStyle();
}

fn showMemoryMaps(
    allocator: Allocator,
    ui: *UI,
    session: *Session,
) !void {
    const maps = try session.getMemMaps(allocator);
    defer {
        for (maps) |map| map.deinit(allocator);
        allocator.free(maps);
    }
    ui.labelF("memory mappings for pid={}\n", .{session.pid});
    var print_buf: [0x10_000]u8 = undefined;
    var stream = std.io.fixedBufferStream(&print_buf);
    const writer = stream.writer();
    try writer.print("         address range        | perm |   offset   | device|   inode  | path\n", .{});
    try writer.print("------------------------------+------+------------+-------+----------+-----------\n", .{});
    for (maps) |map| {
        try writer.print("0x{x:0>12}-0x{x:0>12} | {c}{c}{c}{c} | 0x{x:0>8} | {x:0>2}:{x:0>2} | {d: >8} | {s}\n", .{
            map.addr_range.start,
            map.addr_range.end,
            ([2]u8{ 'r', '-' })[if (map.perms.read) 0 else 1],
            ([2]u8{ 'w', '-' })[if (map.perms.write) 0 else 1],
            ([2]u8{ 'x', '-' })[if (map.perms.execute) 0 else 1],
            ([2]u8{ 's', 'p' })[if (map.perms.shared) 0 else 1],
            map.offset,
            map.device.major,
            map.device.minor,
            map.inode,
            map.path,
        });
    }
    ui.scrollableLabel("mem_maps_contents", Size.flexible(.percent, 1, 1), stream.getWritten());
}

fn doMemoryView(
    ui: *UI,
    session: *Session,
    mem_view_input: *UI.TextInputState,
    mem_view_addr: *?usize,
) !void {
    const sig = ui.lineInput("mem_view_addr", UI.Size.percent(1, 0), mem_view_input);
    if (sig.enter_pressed) {
        const addr = try std.fmt.parseUnsigned(usize, mem_view_input.slice(), 0);
        mem_view_addr.* = addr;
    }

    if (mem_view_addr.*) |addr| {
        const mem_file = try session.procMemFile();
        defer mem_file.close();

        const page_size = 0x1000;
        const page_addr = addr - (addr % page_size);
        try mem_file.seekTo(page_addr);
        var page_buf: [page_size]u8 = undefined;
        _ = try mem_file.read(&page_buf);

        const lines = page_size / 16;
        const bytes_per_line =
            2 + 12 + 1 // for the hex addr + the ':'
        + (16 * 2) // for the hex bytes
        + 16 / 2 // for the space before each byte pair
        + 1 // for the middle space
        + 1; // for the newline
        var print_buf: [lines * bytes_per_line]u8 = undefined;
        var stream = std.io.fixedBufferStream(&print_buf);
        const writer = stream.writer();
        for (page_buf, 0..) |byte, idx| {
            if (idx % 16 == 0) try writer.print("0x{x:0>12}:", .{page_addr + idx});
            if (idx % 2 == 0) try writer.print(" ", .{});
            if (idx % 16 == 8) try writer.print(" ", .{});
            try writer.print("{x:0>2}", .{byte});
            if (idx % 16 == 15) try writer.print("\n", .{});
        }

        ui.scrollableLabel(
            "mem_view_contents",
            Size.flexible(.percent, 1, 1),
            stream.getWritten(),
        );
    }
}

fn doSourcesWidget(
    frame_arena: *std.heap.ArenaAllocator,
    ui: *UI,
    session_cmds: *std.ArrayList(SessionCmd),
    cwd: []const u8,
    src_file_input: *UI.TextInputState,
    src_file_search: *FuzzySearchOptions(SrcFileSearchCtx, 20),
    file_tab: *FileTab,
) !void {
    try doOpenFileBox(frame_arena, ui, session_cmds, cwd, src_file_input, src_file_search);
    // try file_viewer.show(&ui);
    try file_tab.display(ui, session_cmds);
}

const DwarfSearchIterator = struct {
    session: ?Session,
    unit_idx: usize = 0,
    fn_idx: usize = 0,
    var_idx: usize = 0,

    pub fn init(session: ?Session) DwarfSearchIterator {
        return .{ .session = session };
    }

    pub fn reset(self: *DwarfSearchIterator, query: []const u8) !void {
        _ = query;
        self.unit_idx = 0;
        self.fn_idx = 0;
        self.var_idx = 0;
    }

    pub const Context = struct {
        unit_idx: usize,
        fn_idx: usize,
        var_idx: usize,
    };

    pub fn next(self: *DwarfSearchIterator) !?struct { str: []const u8, ctx: Context } {
        const dwarf = (self.session orelse return null).elf.dwarf;

        if (self.unit_idx == dwarf.units.len) return null;
        const unit = dwarf.units[self.unit_idx];

        var name: ?[]const u8 = null;
        if (self.fn_idx < unit.functions.len) {
            self.fn_idx += 1;
            name = unit.functions[self.fn_idx - 1].name;
        } else if (self.var_idx < unit.variables.len) {
            self.var_idx += 1;
            name = unit.variables[self.var_idx - 1].name;
        } else {
            self.unit_idx += 1;
            self.fn_idx = 0;
            self.var_idx = 0;
        }

        return if (name) |str| .{
            .str = str,
            .ctx = .{
                .unit_idx = self.unit_idx,
                .fn_idx = self.fn_idx,
                .var_idx = self.var_idx,
            },
        } else return self.next();
    }
};

const FileSearchIterator = struct {
    allocator: Allocator,
    dir: std.fs.Dir,
    walker: std.fs.Dir.Walker,

    pub const Context = struct {};

    pub fn init(allocator: Allocator, path: []const u8) !FileSearchIterator {
        const dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
        return .{ .allocator = allocator, .dir = dir, .walker = try dir.walk(allocator) };
    }

    pub fn deinit(self: *FileSearchIterator) void {
        self.walker.deinit();
        self.dir.close();
    }

    pub fn reset(self: *FileSearchIterator, query: []const u8) !void {
        _ = query;
        self.walker.deinit();
        self.walker = try self.dir.walk(self.allocator);
    }

    pub fn next(self: *FileSearchIterator) !?struct { str: []const u8, ctx: Context } {
        const next_entry = try self.walker.next() orelse return null;
        return .{ .str = next_entry.path, .ctx = .{} };
    }
};

fn pickFuzzyMatches(
    ui: *UI,
    matcher: anytype,
    label: []const u8,
    placement: ?UI.RelativePlacement,
    reverse_order: ?bool,
) ?@TypeOf(matcher.*).Context {
    _ = reverse_order;
    _ = placement;
    _ = label;
    // TODO: check that matcher is a pointer type

    matcher.mutex.lock();
    defer matcher.mutex.unlock();

    // TODO: match highlights
    for (matcher.top_matches.slice()) |entry| {
        const btn_sig = ui.buttonF("{s} (score={})", .{ entry.matched_str, entry.score });
        if (btn_sig.clicked) return entry.ctx;
    }

    return null;
}

fn Buffer(comptime capacity: usize) type {
    return struct {
        buffer: [capacity]u8 = [_]u8{0} ** capacity,
        len: usize = 0,

        const Self = @This();

        pub fn slice(self: *const Self) []const u8 {
            return self.buffer[0..self.len];
        }

        pub fn write(self: *Self, buf: []const u8) void {
            std.debug.assert(self.len + buf.len <= self.buffer.len);
            @memcpy(self.buffer[self.len..], buf);
            self.len += buf.len;
        }
    };
}

const InputBuf = Buffer(0x1000);

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
        line_offsets: []const usize,
        lock_line: ?f32,
        target_lock_line: ?f32,
        focus_box: ?SrcBox,
        target_focus_box: ?SrcBox,

        pub fn free(self: FileInfo, allocator: Allocator) void {
            allocator.free(self.path);
            allocator.free(self.content);
            allocator.free(self.line_offsets);
        }
    };

    pub fn init(allocator: Allocator) FileTab {
        return .{
            .allocator = allocator,
            .files = std.ArrayList(FileInfo).init(allocator),
            .active_file = null,
        };
    }

    pub fn deinit(self: FileTab) void {
        for (self.files.items) |file| file.free(self.allocator);
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

        var line_offsets = std.ArrayList(usize).init(self.allocator);
        try line_offsets.append(0);
        for (content, 0..) |char, i| {
            if (char != '\n') continue;
            try line_offsets.append(i);
        }
        errdefer line_offsets.deinit();

        try self.files.append(.{
            .path = dupe_path,
            .content = content,
            .line_offsets = try line_offsets.toOwnedSlice(),
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

        self.files.items[file_idx].target_lock_line = @as(f32, @floatFromInt(src.line));
        self.files.items[file_idx].target_focus_box = .{
            .min = .{ .line = @as(f32, @floatFromInt(src.line)), .column = @as(f32, @floatFromInt(src.column)) },
            .max = .{ .line = @as(f32, @floatFromInt(src.line)), .column = @as(f32, @floatFromInt(src.column)) },
        };
        self.active_file = file_idx;
    }

    pub fn findFile(self: FileTab, path: []const u8) ?usize {
        for (self.files.items, 0..) |file_info, i| {
            if (std.mem.eql(u8, path, file_info.path)) return i;
        }
        return null;
    }

    pub fn findFileFromSrc(self: FileTab, src: SrcLoc) ?usize {
        const path = try std.fs.path.join(self.allocator, &.{ src.dir, src.file });
        defer self.allocator.free(path);
        return self.findFile(path);
    }

    pub fn display(self: *FileTab, ui: *UI, session_cmds: *std.ArrayList(SessionCmd)) !void {
        const trace = tracy.Zone(@src());
        defer trace.End();
        const file_tab_node = ui.pushLayoutParent(.{
            .draw_border = true,
        }, "FileTag:top_node", Size.flexible(.percent, 1, 1), .y);

        const buttons_parent = ui.pushLayoutParent(.{}, "FileTab:buttons_parent", fill_x_size, .x);
        for (self.files.items, 0..) |file_info, i| {
            const filename = std.fs.path.basename(file_info.path);
            const highlight_color = if (file_info.focus_box) |_| app_style.highlight_color else vec4{ 1, 0, 0, 1 };
            const is_active = self.active_file != null and self.active_file.? == i;
            if (is_active) ui.pushTmpStyle(.{ .bg_color = highlight_color });
            if (ui.button(filename).clicked) self.active_file = i;
        }
        if (ui.subtleIconButton(Icons.plus).clicked) std.debug.print("TODO: open file menu\n", .{});
        ui.popParentAssert(buttons_parent);

        if (self.active_file) |file_idx| {
            const file = &self.files.items[file_idx];

            // line + text parent
            const file_box_parent = ui.pushLayoutParent(.{
                .clip_children = true,
                .draw_border = true,
            }, "FileTab:text_box_parent", Size.flexible(.percent, 1, 1), .x);

            const line_scroll_size = [2]Size{ Size.children(1), Size.percent(1, 0) };
            const line_scroll_parent = ui.pushLayoutParentF(.{
                .scroll_children_y = true,
                .clip_children = true,
            }, "{s}::line_scroll_parent", .{file.path}, line_scroll_size, .y);
            const line_sig = line_scroll_parent.signal;
            const line_text_node = blk: {
                const n_lines = file.line_offsets.len;
                const max_line_fmt_size = @as(usize, @intFromFloat(@ceil(@log10(@as(f32, @floatFromInt(n_lines)))))) + 1;
                var tmpbuf = try self.allocator.alloc(u8, max_line_fmt_size * n_lines);
                defer self.allocator.free(tmpbuf);

                var i: usize = 0;
                var offset: usize = 0;
                while (i < n_lines) : (i += 1) {
                    offset += (try std.fmt.bufPrint(tmpbuf[offset..], "{}\n", .{i + 1})).len;
                }

                const lines_text = tmpbuf[0..offset];
                const line_text_node = ui.addNode(.{
                    .no_id = true,
                    .draw_text = true,
                    .floating_y = true,
                }, lines_text, .{});
                line_text_node.rel_pos = UI.RelativePlacement.match(.top_left);

                break :blk line_text_node;
            };
            ui.popParentAssert(line_scroll_parent);

            const text_sig = try textDisplay(
                ui,
                file.path,
                [2]Size{ Size.percent(1, 0), Size.percent(1, 0) },
                .{ .content = file.content, .line_offsets = file.line_offsets },
                file.lock_line,
                if (file.focus_box) |box| &[1]FileTab.SrcBox{box} else &[0]FileTab.SrcBox{},
            );
            const text_scroll_node = file_box_parent.last.?;

            // if we manually scroll the text stop the line locking
            if (text_sig.scroll_amount[0] != 0 or text_sig.scroll_amount[1] != 0) {
                file.lock_line = null;
                file.target_lock_line = null;
            }

            const text_node_rect = text_scroll_node.rect;
            const right_click = ui.events.matchAndRemove(.MouseUp, .{ .button = .right });
            if (right_click != null and (text_sig.hovering or line_sig.hovering)) {
                show_ctx_menu = true;
            }
            if (show_ctx_menu) {
                const ctx_menu_trace = tracy.ZoneN(@src(), "file ctx_menu");
                defer ctx_menu_trace.End();
                ui.startCtxMenu(null);
                defer ui.endCtxMenu();
                const ctx_menu_rect = ui.ctx_menu_root.?.rect;

                const font_pixel_size = ui.topStyle().font_size;
                const line_size = ui.font.getScaledMetrics(font_pixel_size).line_advance;
                const mouse_offset = text_node_rect.max[1] - ctx_menu_rect.get(.top_left)[1] - ui.textPadding(text_scroll_node)[1];
                const line_offset = mouse_offset - text_scroll_node.scroll_offset[1];
                const mouse_line = @floor(line_offset / line_size);
                const src_line = @as(u32, @intFromFloat(mouse_line)) + 1;

                if (ui.buttonF("Set Breakpoint At Line {}\n", .{src_line}).clicked) {
                    try session_cmds.append(.{ .set_break_at_src = .{
                        .dir = std.fs.path.dirname(file.path).?,
                        .file = std.fs.path.basename(file.path),
                        .line = src_line,
                        .column = 0,
                    } });
                    show_ctx_menu = false;
                }

                if (ui.events.match(.MouseDown, .{})) |_| {
                    if (!ctx_menu_rect.contains(ui.mouse_pos)) show_ctx_menu = false;
                }
            }

            // scroll the line numbers with the src text
            line_scroll_parent.scroll_offset[1] = text_scroll_node.scroll_offset[1];
            line_text_node.rel_pos.diff[1] = -line_scroll_parent.scroll_offset[1];

            ui.popParentAssert(file_box_parent);
        }

        ui.popParentAssert(file_tab_node);
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

fn doOpenFileBox(
    frame_arena: *std.heap.ArenaAllocator,
    ui: *UI,
    session_cmds: *std.ArrayList(SessionCmd),
    cwd: []const u8,
    src_file_input: *UI.TextInputState,
    src_file_search: *FuzzySearchOptions(SrcFileSearchCtx, 20),
) !void {
    const open_file_parent = ui.pushLayoutParent(.{}, "open_file_parent", fill_x_size, .x);
    defer ui.popParentAssert(open_file_parent);
    {
        const open_button_sig = ui.button("Open Source File");
        const text_input_sig = ui.lineInput("textinput", UI.Size.percent(1, 0), src_file_input);
        const src_file_slice = src_file_input.slice();
        if (open_button_sig.clicked or text_input_sig.enter_pressed) {
            try session_cmds.append(.{ .open_src_file = src_file_slice });
        }
        if (text_input_sig.focused and src_file_slice.len > 0) {
            const text_input_node = ui.topParent().last.?;
            const input = src_file_slice;
            if (!std.mem.eql(u8, input, src_file_search.target)) {
                try src_file_search.resetSearch(input);

                //const input_path = try std.fs.path.join(frame_arena.allocator(), &.{ cwd, input });
                // TODO: don't crash if user writes "tmp/dir////"
                const inner_dir = if (std.mem.lastIndexOfScalar(u8, input, '/')) |idx| blk: {
                    break :blk input[0 .. idx + 1];
                } else "";
                const full_dir_path = try std.fs.path.join(frame_arena.allocator(), &.{ cwd, inner_dir });

                var dir = try std.fs.openDirAbsolute(full_dir_path, .{ .iterate = true });
                defer dir.close();
                var dir_iter = dir.iterate();
                while (try dir_iter.next()) |entry| {
                    const file_ctx = SrcFileSearchCtx{
                        // these strings get release by the search list
                        .name = try src_file_search.allocator.dupe(u8, entry.name),
                        .inner_dir = inner_dir,
                        .path = try std.fs.path.join(src_file_search.allocator, &.{ cwd, inner_dir, entry.name }),
                        .kind = entry.kind,
                    };
                    src_file_search.addEntry(file_ctx.path, file_ctx);
                }
            }

            const choice = try src_file_search.present(
                frame_arena,
                ui,
                "filepath_chooser",
                UI.RelativePlacement.absolute(.{ .top_left = text_input_node.rect.min }),
                false,
            );
            if (choice) |file_ctx| {
                std.debug.print("TODO: switch input buffer to {s}\n", .{file_ctx.name});
            }
        }
    }
}

fn doDisassemblyWindow(
    allocator: std.mem.Allocator,
    ui: *UI,
    session: Session,
    disasm_texts: *std.ArrayList(AsmTextInfo),
) !void {
    const regs = try session.getRegisters();

    const disasm_text_idx = blk: for (disasm_texts.items, 0..) |text, i| {
        if (text.addr_range.contains(regs.rip)) break :blk i;
    } else null;

    const disasm_text = if (disasm_text_idx) |idx| disasm_texts.items[idx] else text_blk: {
        const function_addr_range = if (session.elf.findFunctionAtAddr(regs.rip)) |func| blk: {
            break :blk Session.AddrRange{
                .start = func.low_pc orelse break :blk null,
                .end = func.high_pc orelse break :blk null,
            };
        } else null;

        const section_addr_range: ?Session.AddrRange = blk: {
            var file = try std.fs.cwd().openFile(session.exec_path, .{});
            defer file.close();
            const header = try std.elf.Header.read(file);
            std.debug.assert(header.is_64);
            var sh_iter = header.section_header_iterator(file);
            while (try sh_iter.next()) |shdr| {
                const addr_range = Session.AddrRange{
                    .start = shdr.sh_addr,
                    .end = shdr.sh_addr + shdr.sh_size,
                };
                if (addr_range.contains(regs.rip)) break :blk addr_range;
            }
            break :blk null;
        };

        const block_addr_range = if (function_addr_range) |func_range|
            func_range
        else if (section_addr_range) |section_range|
            section_range
        else blk: {
            const mem_map = (try session.getMemMapAtAddr(allocator, regs.rip)) orelse return;
            defer mem_map.deinit(allocator);
            break :blk mem_map.addr_range;
        };

        const proc_mem = try session.procMemFile();
        defer proc_mem.close();

        try proc_mem.seekTo(block_addr_range.start);
        const mem_block = try allocator.alloc(u8, block_addr_range.end - block_addr_range.start);
        defer allocator.free(mem_block);
        std.debug.assert((try proc_mem.read(mem_block)) == mem_block.len);

        const text_info = try generateTextInfoForDisassembly(allocator, mem_block, block_addr_range.start);
        try disasm_texts.append(text_info);
        break :text_blk text_info;
    };

    const line_idx = loop: for (disasm_text.line_addrs, 0..) |addr, i| {
        if (addr == regs.rip) break :loop i;
    } else unreachable;

    _ = try showDisassemblyWindow(ui, "main_disasm_window", disasm_text, @as(f32, @floatFromInt(line_idx + 1)));
}

fn showDisassemblyWindow(ui: *UI, label: []const u8, asm_text_info: AsmTextInfo, lock_line: ?f32) !UI.Signal {
    // TODO: hightlight box ranges
    const highlight_box: ?FileTab.SrcBox = if (lock_line) |line| FileTab.SrcBox{
        .min = .{ .line = line, .column = 0 },
        .max = .{ .line = line, .column = 0 },
    } else null;

    const size = [2]Size{ Size.percent(1, 0), Size.percent(0.25, 0) };
    const text_info = TextDisplayInfo{ .content = asm_text_info.data, .line_offsets = asm_text_info.line_offsets };
    const src_boxes = if (highlight_box) |box| &[1]FileTab.SrcBox{box} else &[0]FileTab.SrcBox{};
    return try textDisplay(ui, label, size, text_info, lock_line, src_boxes);
}

const TextDisplayInfo = struct {
    content: []const u8,
    line_offsets: []const usize, // indices into `content`
};

fn textDisplay(
    ui: *UI,
    label: []const u8,
    size: [2]UI.Size,
    text_info: TextDisplayInfo,
    lock_line: ?f32,
    boxes: []const FileTab.SrcBox,
) !UI.Signal {
    // TODO: update this to include scroll-bar when that's implemented in `zig-ui`
    const parent = ui.pushLayoutParent(.{
        .scroll_children_x = true,
        .scroll_children_y = true,
        .clip_children = true,
    }, label, size, .y);
    defer ui.popParentAssert(parent);

    ui.label(text_info.content);

    const font_pixel_size = ui.topStyle().font_size;
    const line_size = ui.font.getScaledMetrics(font_pixel_size).line_advance;

    if (lock_line) |lock| parent.scroll_offset[1] = lock * line_size;

    // TODO: use the functions that alredy exist in UI to calculate text info quickly
    for (boxes) |box| {
        // TODO: use correct box column for rect size/pos
        _ = ui.addNode(.{
            .no_id = true,
            .draw_border = true,
            .floating_y = true,
        }, "", .{
            .size = [2]UI.Size{ UI.Size.percent(1, 0), UI.Size.pixels(line_size, 1) },
            .rel_pos = UI.RelativePlacement{
                .anchor = .top_right,
                .target = .top_right,
                .diff = vec2{ 0, line_size * box.min.line },
            },
        });
    }

    return parent.signal;
}

const AsmTextInfo = struct {
    addr_range: Session.AddrRange,
    data: []const u8,
    line_offsets: []const usize, // indices into `data`
    line_addrs: []const usize,

    pub fn deinit(self: AsmTextInfo, allocator: Allocator) void {
        allocator.free(self.data);
        allocator.free(self.line_offsets);
        allocator.free(self.line_addrs);
    }
};

// HACK: zig for some reason can't translate this macro, which is used internally by some
// error code defines
fn ZYAN_MAKE_STATUS(@"error": u32, module: u32, code: u32) u32 {
    return ((@"error" & 0x01) << 31) |
        ((module & 0x7FF) << 20) |
        (code & 0xFFFFF);
}
// copied from the `cimport.zig` for zydis
pub const ZYAN_MODULE_ZYCORE = 0x001;
pub const ZYAN_MODULE_ZYDIS = 0x002;
pub const ZYAN_MODULE_ARGPARSE = 0x003;
pub const ZYAN_STATUS_SUCCESS = ZYAN_MAKE_STATUS(0, ZYAN_MODULE_ZYCORE, 0x00);
pub const ZYAN_STATUS_FAILED = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x01);
pub const ZYAN_STATUS_TRUE = ZYAN_MAKE_STATUS(0, ZYAN_MODULE_ZYCORE, 0x02);
pub const ZYAN_STATUS_FALSE = ZYAN_MAKE_STATUS(0, ZYAN_MODULE_ZYCORE, 0x03);
pub const ZYAN_STATUS_INVALID_ARGUMENT = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x04);
pub const ZYAN_STATUS_INVALID_OPERATION = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x05);
pub const ZYAN_STATUS_ACCESS_DENIED = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x06);
pub const ZYAN_STATUS_NOT_FOUND = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x07);
pub const ZYAN_STATUS_OUT_OF_RANGE = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x08);
pub const ZYAN_STATUS_INSUFFICIENT_BUFFER_SIZE = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x09);
pub const ZYAN_STATUS_NOT_ENOUGH_MEMORY = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x0A);
pub const ZYAN_STATUS_BAD_SYSTEMCALL = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x0B);
pub const ZYAN_STATUS_OUT_OF_RESOURCES = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x0C);
pub const ZYAN_STATUS_MISSING_DEPENDENCY = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYCORE, 0x0D);
pub const ZYAN_STATUS_ARG_NOT_UNDERSTOOD = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ARGPARSE, 0x00);
pub const ZYAN_STATUS_TOO_FEW_ARGS = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ARGPARSE, 0x01);
pub const ZYAN_STATUS_TOO_MANY_ARGS = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ARGPARSE, 0x02);
pub const ZYAN_STATUS_ARG_MISSES_VALUE = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ARGPARSE, 0x03);
pub const ZYAN_STATUS_REQUIRED_ARG_MISSING = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ARGPARSE, 0x04);
pub const ZYDIS_STATUS_NO_MORE_DATA = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x00);
pub const ZYDIS_STATUS_DECODING_ERROR = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x01);
pub const ZYDIS_STATUS_INSTRUCTION_TOO_LONG = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x02);
pub const ZYDIS_STATUS_BAD_REGISTER = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x03);
pub const ZYDIS_STATUS_ILLEGAL_LOCK = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x04);
pub const ZYDIS_STATUS_ILLEGAL_LEGACY_PFX = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x05);
pub const ZYDIS_STATUS_ILLEGAL_REX = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x06);
pub const ZYDIS_STATUS_INVALID_MAP = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x07);
pub const ZYDIS_STATUS_MALFORMED_EVEX = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x08);
pub const ZYDIS_STATUS_MALFORMED_MVEX = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x09);
pub const ZYDIS_STATUS_INVALID_MASK = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x0A);
pub const ZYDIS_STATUS_SKIP_TOKEN = ZYAN_MAKE_STATUS(0, ZYAN_MODULE_ZYDIS, 0x0B);
pub const ZYDIS_STATUS_IMPOSSIBLE_INSTRUCTION = ZYAN_MAKE_STATUS(1, ZYAN_MODULE_ZYDIS, 0x0C);

/// don't forget to call `AsmTextInfo.deinit` when done with it
fn generateTextInfoForDisassembly(allocator: Allocator, data: []const u8, data_start_addr: usize) !AsmTextInfo {
    const fn_zone = tracy.Zone(@src());
    defer fn_zone.End();

    var text_bytes = std.ArrayList(u8).init(allocator);
    var line_offsets = std.ArrayList(usize).init(allocator);
    var line_addrs = std.ArrayList(usize).init(allocator);

    var instructions_done: usize = 0;
    var data_idx: usize = 0;
    asm_loop: while (data_idx < data.len) {
        const asm_addr = data_idx + data_start_addr;
        const inst_bytes = data[data_idx..@min(data_idx + 15, data.len)];

        var instruction: c.ZydisDisassembledInstruction = undefined;
        const disassemble_result = c.ZydisDisassembleIntel(
            c.ZYDIS_MACHINE_MODE_LONG_64,
            asm_addr,
            inst_bytes.ptr,
            inst_bytes.len,
            &instruction,
        );
        if (!c.ZYAN_SUCCESS(disassemble_result)) {
            const error_str = switch (disassemble_result) {
                ZYDIS_STATUS_NO_MORE_DATA => "ZYDIS_STATUS_NO_MORE_DATA",
                ZYDIS_STATUS_DECODING_ERROR => "ZYDIS_STATUS_DECODING_ERROR",
                ZYDIS_STATUS_INSTRUCTION_TOO_LONG => "ZYDIS_STATUS_INSTRUCTION_TOO_LONG",
                ZYDIS_STATUS_BAD_REGISTER => "ZYDIS_STATUS_BAD_REGISTER",
                ZYDIS_STATUS_ILLEGAL_LOCK => "ZYDIS_STATUS_ILLEGAL_LOCK",
                ZYDIS_STATUS_ILLEGAL_LEGACY_PFX => "ZYDIS_STATUS_ILLEGAL_LEGACY_PFX",
                ZYDIS_STATUS_ILLEGAL_REX => "ZYDIS_STATUS_ILLEGAL_REX",
                ZYDIS_STATUS_INVALID_MAP => "ZYDIS_STATUS_INVALID_MAP",
                ZYDIS_STATUS_MALFORMED_EVEX => "ZYDIS_STATUS_MALFORMED_EVEX",
                ZYDIS_STATUS_MALFORMED_MVEX => "ZYDIS_STATUS_MALFORMED_MVEX",
                ZYDIS_STATUS_INVALID_MASK => "ZYDIS_STATUS_INVALID_MASK",
                ZYDIS_STATUS_SKIP_TOKEN => "ZYDIS_STATUS_SKIP_TOKEN",
                ZYDIS_STATUS_IMPOSSIBLE_INSTRUCTION => "ZYDIS_STATUS_IMPOSSIBLE_INSTRUCTION",
                else => std.debug.panic("unknown error code: 0x{x}\n", .{disassemble_result}),
            };
            if (disassemble_result == ZYDIS_STATUS_NO_MORE_DATA) break :asm_loop;
            std.debug.panic("zyan status code 0x{x}: {s}\n", .{ disassemble_result, error_str });
        }

        const inst_len = instruction.info.length;
        data_idx += inst_len;

        try text_bytes.ensureUnusedCapacity(0x1000);
        const line_offset = text_bytes.items.len;
        var line_buffer = text_bytes.items[line_offset..];
        line_buffer.len = text_bytes.capacity - line_offset;
        var stream = std.io.fixedBufferStream(line_buffer);
        var writer = stream.writer();

        const fmt_zone = tracy.ZoneN(@src(), "fmt_zone");
        try writer.print("0x{x:0>12}: ", .{asm_addr});
        const byte_hex_strs = comptime blk: {
            var str_bufs = @as([256][2]u8, undefined);
            for (&str_bufs, 0..) |*buf, idx| _ = std.fmt.bufPrint(buf, "{x:0>2}", .{idx}) catch unreachable;
            break :blk str_bufs;
        };
        var idx: usize = 0;
        while (idx < @max(10, inst_len)) : (idx += 1) {
            const byte_str = if (idx < inst_len) &byte_hex_strs[inst_bytes[idx]] else "  ";
            try writer.print("{s} ", .{byte_str});
        }
        const instruction_text = std.mem.span(@as([*c]u8, @ptrCast(&instruction.text[0])));
        try writer.print("{s}\n", .{instruction_text});
        fmt_zone.End();

        text_bytes.items.len += stream.pos;
        try line_offsets.append(line_offset);
        try line_addrs.append(asm_addr);

        instructions_done += 1;
    }

    return AsmTextInfo{
        .addr_range = Session.AddrRange{ .start = data_start_addr, .end = data_start_addr + data_idx },
        .data = try text_bytes.toOwnedSlice(),
        .line_offsets = try line_offsets.toOwnedSlice(),
        .line_addrs = try line_addrs.toOwnedSlice(),
    };
}

fn showRegisters(ui: *UI, regs: Session.Registers) void {
    const table_regs = .{ "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "rip" };
    inline for (table_regs) |reg_name| {
        ui.labelF(reg_name ++ ": 0x{x:0>16}", .{@field(regs, reg_name)});
    }
}

const CallStackViewer = struct {
    show_vars: Map,

    const Map = std.HashMap(Session.CallFrame, bool, struct {
        pub fn hash(self: @This(), key: Session.CallFrame) u64 {
            _ = self;
            var buf: [0x4000]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "0x{x}:{d}", .{
                key.addr,
                if (key.src) |src| src.line else 0,
            }) catch unreachable;
            return std.hash_map.hashString(str);
        }
        pub fn eql(self: @This(), key_a: Session.CallFrame, key_b: Session.CallFrame) bool {
            _ = self;
            if (!std.meta.eql(key_a.src, key_b.src)) return false;
            if (key_a.function != null and key_b.function != null) {
                if (key_a.function.?.name != null and key_b.function.?.name != null) {
                    if (!std.mem.eql(u8, key_a.function.?.name.?, key_b.function.?.name.?)) return false;
                }
            }
            return true;
        }
    }, std.hash_map.default_max_load_percentage);

    pub fn init(allocator: Allocator) CallStackViewer {
        return .{ .show_vars = Map.init(allocator) };
    }

    pub fn deinit(self: *CallStackViewer) void {
        self.show_vars.deinit();
    }

    pub fn display(self: *CallStackViewer, ui: *UI, call_stack: []Session.CallFrame, session: Session) !void {
        _ = session;
        for (call_stack, 0..) |frame, idx| {
            const result = try self.show_vars.getOrPut(frame);
            if (!result.found_existing) result.value_ptr.* = false;
            const is_open_ptr = result.value_ptr;

            const fn_parent = ui.pushLayoutParentF(.{}, "call_stack_parent_#{}", .{idx}, [2]Size{ Size.children(1), Size.children(1) }, .x);
            {
                const fn_name = if (frame.function) |func| func.name orelse "???" else "???";
                const icon = if (is_open_ptr.*) Icons.up_open else Icons.down_open;

                ui.labelF("0x{x:0>12}:", .{frame.addr});
                if (ui.subtleIconButtonF("{s}###call_stack_viewer_entry_#{}", .{ icon, idx }).clicked) {
                    is_open_ptr.* = !is_open_ptr.*;
                }
                if (frame.src) |src| {
                    ui.labelF("{s} @ {s}/{s}:{}", .{ fn_name, src.dir, src.file, src.line });
                } else {
                    ui.labelF("{s} @ ???", .{fn_name});
                }
            }
            ui.popParentAssert(fn_parent);

            if (is_open_ptr.*) {
                if (frame.function) |func| {
                    for (func.params) |param| ui.label(param.name orelse "???");
                } else ui.label("<no debug information on this function>");
            }
        }
    }
};

fn doVarTable(
    frame_arena: *std.heap.ArenaAllocator,
    ui: *UI,
    session_cmds: *std.ArrayList(SessionCmd),
    session: *Session,
    var_input: *UI.TextInputState,
    var_search: *FuzzySearchOptions(VarSearchCtx, 10),
) !void {
    const add_var_parent = ui.pushLayoutParent(.{}, "add_var_parent", fill_x_size, .x);
    {
        //const button_sig = ui.button("Add Variable");
        ui.labelBox("Add Variable");
        const text_input_sig = ui.lineInput("add_var_textinput", UI.Size.percent(1, 0), var_input);
        //if (button_sig.clicked or text_input_sig.enter_pressed) {
        //    try session_cmds.append(.{ .add_watched_variable = var_buf.slice() });
        //}
        const var_slice = var_input.slice();
        if (text_input_sig.focused and var_slice.len > 0) {
            const text_input_node = ui.topParent().last.?;

            if (!std.mem.eql(u8, var_slice, var_search.target)) {
                try var_search.resetSearch(var_slice);
                // here we only shows/score variables that would be possible to choose
                // (why? a debug build of the zig compiler has ~700k variables)
                // always add all the globals
                for (session.elf.dwarf.units, 0..) |unit, unit_idx| {
                    for (unit.global_vars) |var_idx| {
                        const variable = unit.variables[var_idx];
                        if (variable.name == null) continue;
                        var_search.addEntry(variable.name.?, .{
                            .variable = variable,
                            .line_progs = session.elf.dwarf.line_progs,
                            .unit_idx = unit_idx,
                        });
                    }
                }
                // and if we're in a function add the locals/parameters too
                for (session.elf.dwarf.units, 0..) |unit, unit_idx| {
                    for (unit.functions, 0..) |func, func_idx| {
                        if (func.low_pc == null or func.high_pc == null) continue;
                        const addr = (try session.getRegisters()).rip;
                        if (!(addr <= func.low_pc.? and addr < func.high_pc.?)) continue;
                        // params
                        for (func.params) |variable| {
                            if (variable.name == null) continue;
                            var_search.addEntry(variable.name.?, .{
                                .variable = variable.*,
                                .line_progs = session.elf.dwarf.line_progs,
                                .unit_idx = unit_idx,
                            });
                        }
                        // locals
                        for (unit.variables) |variable| {
                            if (variable.function == null) continue;
                            if (variable.function.? != &unit.functions[func_idx]) continue;
                            if (variable.name == null) continue;
                            var_search.addEntry(variable.name.?, .{
                                .variable = variable,
                                .line_progs = session.elf.dwarf.line_progs,
                                .unit_idx = unit_idx,
                            });
                        }
                    }
                }
            }

            const choice = try var_search.present(
                frame_arena,
                ui,
                "var_table_chooser",
                UI.RelativePlacement.absolute(.{ .top_left = text_input_node.rect.min }),
                false,
            );
            if (choice) |var_ctx| {
                try session_cmds.append(.{ .add_watched_variable = var_ctx.variable });
            }
        }
    }
    ui.popParentAssert(add_var_parent);

    const vars_parent = ui.pushLayoutParent(.{}, "vars_parent", fill_x_size, .y);
    {
        const row_size = fill_x_size;
        const column_box_size = [2]Size{ Size.percent(1.0 / 3.0, 1), Size.text(1) };
        const table_header_row_parent = ui.pushLayoutParent(.{}, "table_header_row_parent", row_size, .x);
        {
            ui.pushStyle(.{ .size = column_box_size });
            ui.labelBox("Variable Name");
            ui.labelBox("Type");
            ui.labelBox("Value");
            _ = ui.popStyle();
        }
        ui.popParentAssert(table_header_row_parent);

        for (session.watched_vars.items) |var_info| {
            const var_name = var_info.name;
            const row_parent = ui.pushLayoutParentF(.{
                .no_id = true,
            }, "row_parent_{?s}", .{var_name}, row_size, .x);
            {
                ui.pushStyle(.{ .size = column_box_size });
                ui.labelBoxF("{?s}", .{var_name});
                ui.labelBoxF("{s}", .{if (var_info.type) |ty| @tagName(std.meta.activeTag(ty.*)) else "???"});
                if (session.getVariableValue(var_info)) |value| switch (value) {
                    .Float32 => |f| ui.labelBoxF("{d}\n", .{f}),
                    .Uint32 => |uint| ui.labelBoxF("{}\n", .{uint}),
                    .Int32 => |int| ui.labelBoxF("{}\n", .{int}),
                } else |err| switch (err) {
                    Session.Error.VarNotAvailable => ui.labelBox("<not available>"),
                    Session.Error.NoVarLocation => ui.labelBox("<no location>"),
                    Session.Error.NotStopped => ui.labelBox("<not stopped>"),
                    else => return err,
                }
                _ = ui.popStyle();
            }
            ui.popParentAssert(row_parent);
        }
    }
    ui.popParentAssert(vars_parent);
}

const BreakpointWidget = struct {
    allocator: Allocator,
    line_num_input: UI.TextInputState,
    filename_input: UI.TextInputState,
    function_input: UI.TextInputState,
    addr_input: UI.TextInputState,
    func_search: FuzzySearchOptions(FuncSearchCtx, 10),

    pub fn init(allocator: Allocator) !BreakpointWidget {
        return .{
            .allocator = allocator,
            .line_num_input = UI.TextInputState.init(try allocator.alloc(u8, 1000), ""),
            .filename_input = UI.TextInputState.init(try allocator.alloc(u8, 1000), ""),
            .function_input = UI.TextInputState.init(try allocator.alloc(u8, 1000), ""),
            .addr_input = UI.TextInputState.init(try allocator.alloc(u8, 1000), ""),
            .func_search = FuzzySearchOptions(FuncSearchCtx, 10).init(allocator),
        };
    }

    pub fn deinit(self: BreakpointWidget) void {
        self.allocator.free(self.line_num_input.buffer);
        self.allocator.free(self.filename_input.buffer);
        self.allocator.free(self.function_input.buffer);
        self.allocator.free(self.addr_input.buffer);
        self.func_search.deinit();
    }

    pub fn show(
        self: *BreakpointWidget,
        ui: *UI,
        frame_arena: *std.heap.ArenaAllocator,
        session_cmds: *std.ArrayList(SessionCmd),
        session: *Session,
        cwd: []const u8,
    ) !void {
        {
            const set_break_p = ui.pushLayoutParent(.{}, "set_break_parent", fill_x_size, .x);
            defer ui.popParentAssert(set_break_p);

            const button_sig = ui.button("Set Breakpoint");
            ui.labelBox("Line Number");
            const line_sig = ui.lineInput("line_num_textinput", UI.Size.percent(1, 0), &self.line_num_input);
            ui.labelBox("File Name");
            const file_sig = ui.lineInput("filename_textinput", UI.Size.percent(1, 0), &self.filename_input);

            if (button_sig.clicked or line_sig.enter_pressed or file_sig.enter_pressed) blk: {
                const line_num = self.line_num_input.slice();
                const line = std.fmt.parseUnsigned(u32, line_num, 0) catch |err| {
                    std.debug.print("{s}: couldn't parse break line number: '{s}'\n", .{ @errorName(err), line_num });
                    break :blk;
                };
                try session_cmds.append(.{ .set_break_at_src = .{
                    .dir = try std.fs.path.join(frame_arena.allocator(), &.{ cwd, "src" }),
                    .file = self.filename_input.slice(),
                    .line = line,
                    .column = 0,
                } });
            }
        }

        {
            const set_break_func_p = ui.pushLayoutParent(.{}, "set_break_func_parent", fill_x_size, .x);
            defer ui.popParentAssert(set_break_func_p);

            ui.labelBox("Function");
            const text_input_sig = ui.lineInput("src_funcname", UI.Size.percent(1, 0), &self.function_input);
            const function_slice = self.function_input.slice();
            if (text_input_sig.focused and function_slice.len > 0) {
                const text_input_node = ui.topParent().last.?;

                if (!std.mem.eql(u8, function_slice, self.func_search.target)) {
                    try self.func_search.resetSearch(function_slice);
                    for (session.elf.dwarf.units, 0..) |unit, unit_idx| {
                        for (unit.functions, 0..) |func, func_idx| {
                            if (func.name == null) continue;
                            self.func_search.addEntry(func.name.?, .{
                                .name = func.name.?,
                                .loc = if (func.decl_coords) |coords|
                                    coords.toSrcLoc(session.elf.dwarf.line_progs[unit_idx])
                                else
                                    null,
                                .unit_idx = unit_idx,
                                .func_idx = func_idx,
                            });
                        }
                    }
                }
                const choice = try self.func_search.present(
                    frame_arena,
                    ui,
                    "break_func_chooser",
                    UI.RelativePlacement.absolute(.{ .btm_left = text_input_node.rect.get(.top_left) }),
                    true,
                );
                if (choice) |func_ctx| {
                    const func = session.elf.dwarf.units[func_ctx.unit_idx].functions[func_ctx.func_idx];
                    if (func.low_pc) |addr| {
                        try session_cmds.append(.{ .set_break_at_addr = addr });
                    } else if (func.decl_coords) |coords| {
                        const src = coords.toSrcLoc(session.elf.dwarf.line_progs[func_ctx.unit_idx]);
                        try session_cmds.append(.{ .set_break_at_src = src });
                    } else {
                        // TODO: search the ELF symbol table, might have the function addr
                        std.debug.print("couldn't find enough information to set a breakpoint on function '{s}'\n", .{func.name.?});
                    }
                }
            }
        }

        {
            const break_addr_parent = ui.addNode(.{ .no_id = true }, "", .{
                .size = fill_x_size,
                .layout_axis = .x,
            });
            ui.pushParent(break_addr_parent);
            defer ui.popParentAssert(break_addr_parent);

            const button_sig = ui.button("Set breakpoint###break_addr");
            ui.labelBox("Address");
            const text_input_sig = ui.lineInput("break_addr_input", UI.Size.percent(1, 0), &self.addr_input);

            const addr_slice = self.addr_input.slice();
            if (button_sig.clicked or text_input_sig.enter_pressed) blk: {
                const text = addr_slice;
                if (text.len == 0) break :blk;
                const addr = std.fmt.parseUnsigned(usize, text, 0) catch break :blk;
                try session_cmds.append(.{ .set_break_at_addr = addr });
            }
        }

        // show list of current breakpoints and their status
        ui.label("Breakpoints:");
        for (session.breakpoints.items) |breakpoint| {
            if (try session.elf.translateAddrToSrc(breakpoint.addr)) |src| {
                // TODO: show the src path relative to current dir?
                ui.labelF("0x{x:0>8} {s}:{}", .{ breakpoint.addr, src.file, src.line });
            } else {
                ui.labelF("0x{x:0>8}", .{breakpoint.addr});
            }
        }
    }
};

const SrcFileSearchCtx = struct {
    name: []const u8,
    inner_dir: []const u8,
    path: []const u8,
    kind: std.fs.File.Kind,
    pub fn free(self: @This(), allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.path);
    }
    pub fn fmtName(self: @This(), fmt_allocator: Allocator) ![]const u8 {
        return if (self.kind == .directory)
            try std.fmt.allocPrint(fmt_allocator, "{s}{s}/", .{ self.inner_dir, self.name })
        else
            try std.fmt.allocPrint(fmt_allocator, "{s}{s}", .{ self.inner_dir, self.name });
    }
    pub fn fmtExtra(self: @This(), fmt_allocator: Allocator) ![]const u8 {
        _ = self;
        _ = fmt_allocator;
        return "";
    }
};

const VarSearchCtx = struct {
    variable: Dwarf.Variable,
    line_progs: []const Dwarf.LineProg,
    unit_idx: usize,
    pub fn fmtName(self: @This(), fmt_allocator: Allocator) ![]const u8 {
        _ = fmt_allocator;
        return self.variable.name orelse unreachable;
    }
    pub fn fmtExtra(self: @This(), fmt_allocator: Allocator) ![]const u8 {
        if (self.variable.decl_coords) |coords| {
            const src = coords.toSrcLoc(self.line_progs[self.unit_idx]);
            return try std.fmt.allocPrint(fmt_allocator, "({s}:{})", .{
                src.file, src.line,
            });
        } else return "";
    }
};

const FuncSearchCtx = struct {
    name: []const u8,
    loc: ?Dwarf.SrcLoc,
    unit_idx: usize,
    func_idx: usize,
    pub fn fmtName(self: @This(), fmt_allocator: Allocator) ![]const u8 {
        _ = fmt_allocator;
        return self.name;
    }
    pub fn fmtExtra(self: @This(), fmt_allocator: Allocator) ![]const u8 {
        return if (self.loc) |loc| blk: {
            break :blk std.fmt.allocPrint(fmt_allocator, "{s}:{d}", .{ loc.file, loc.line });
        } else "";
    }
};

fn FuzzySearchOptions(comptime Ctx: type, comptime max_slots: usize) type {
    return struct {
        allocator: Allocator,
        slots: [max_slots]Entry,
        slots_filled: usize,
        target: []const u8,

        pub const Entry = struct {
            str: []const u8,
            score: f32,
            ctx: Ctx,
        };

        pub fn init(allocator: Allocator) @This() {
            return .{
                .allocator = allocator,
                .slots = undefined,
                .slots_filled = 0,
                .target = &[0]u8{},
            };
        }

        pub fn deinit(self: @This()) void {
            self.allocator.free(self.target);
            if (@hasDecl(Ctx, "free")) {
                for (self.slots[0..self.slots_filled]) |slot| slot.ctx.free(self.allocator);
            }
        }

        pub fn resetSearch(self: *@This(), new_target: []const u8) !void {
            self.deinit();
            self.target = try self.allocator.dupe(u8, new_target);
            self.slots_filled = 0;
        }

        pub fn addEntry(self: *@This(), test_str: []const u8, ctx: Ctx) void {
            const trace = tracy.Zone(@src());
            defer trace.End();

            std.debug.assert(self.target.len > 0);

            const score = @import("fuzzy_matching.zig").fuzzyScore(self.target, test_str);
            const new_entry = Entry{ .str = test_str, .score = score, .ctx = ctx };
            if (self.slots_filled < self.slots.len) {
                self.slots[self.slots_filled] = new_entry;
                self.slots_filled += 1;
                return;
            } else {
                var worst_idx: usize = 0;
                for (self.slots, 0..) |entry, idx| {
                    if (entry.score < self.slots[worst_idx].score) worst_idx = idx;
                }
                if (@hasDecl(Ctx, "free")) self.slots[worst_idx].ctx.free(self.allocator);
                self.slots[worst_idx] = new_entry;
            }
        }

        pub fn present(
            self: *@This(),
            arena: *std.heap.ArenaAllocator,
            ui: *UI,
            label: []const u8,
            placement: UI.RelativePlacement,
            reverse_order: bool,
        ) !?Ctx {
            const trace = tracy.Zone(@src());
            defer trace.End();

            const allocator = arena.allocator();
            const filled_slots = self.slots[0..self.slots_filled];
            var clicked_option: ?usize = null;

            // sort the entries before displaying (using insertion sort)
            var sort_idx: usize = 1;
            while (sort_idx < filled_slots.len) : (sort_idx += 1) {
                var cmp_idx: usize = sort_idx;
                cmp_loop: while (cmp_idx > 0) : (cmp_idx -= 1) {
                    const left_score = filled_slots[cmp_idx - 1].score;
                    const right_score = filled_slots[cmp_idx].score;
                    const should_swap = if (reverse_order)
                        right_score < left_score
                    else
                        right_score > left_score;
                    if (should_swap) {
                        std.mem.swap(Entry, &filled_slots[cmp_idx], &filled_slots[cmp_idx - 1]);
                    } else break :cmp_loop;
                }
            }

            const bg_node = ui.addNodeAsRoot(.{
                .no_id = true,
                .draw_background = true,
            }, "", .{
                .bg_color = vec4{ 0, 0, 0, 0.85 },
                .size = [2]Size{ Size.children(1), Size.children(1) },
                .rel_pos = placement,
            });
            ui.pushParent(bg_node);
            try ui.window_roots.append(bg_node);
            defer ui.popParentAssert(bg_node);

            for (self.slots[0..self.slots_filled], 0..) |entry, idx| {
                const entry_trace = tracy.Zone(@src());
                defer entry_trace.End();

                const button_node = ui.addNodeStringsF(.{
                    .clickable = true,
                    .draw_text = true,
                    .draw_border = true,
                    .draw_hot_effects = true,
                    .draw_active_effects = true,
                }, "", .{}, "{s}_opt_btn_{d}", .{ label, idx }, .{
                    .border_color = vec4{ 0, 0, 0, 0 },
                    .cursor_type = .pointing_hand,
                    .layout_axis = .x,
                    .size = fill_x_size,
                });
                ui.pushParent(button_node);
                defer ui.popParentAssert(button_node);

                const name = try entry.ctx.fmtName(allocator);
                const extra = try entry.ctx.fmtExtra(allocator);

                const draw_ctx = CustomDrawMatchHighlightCtx{
                    .match_pattern = self.target,
                    .highlight_color = vec4{ 0.99, 0.36, 0.1, 1 }, // orange #fc5b19
                };
                const name_node = ui.addNodeStringsF(.{
                    .draw_text = true,
                    .draw_active_effects = true,
                }, "{s}", .{name}, "{s}_opt_btn_name_{d}", .{ label, idx }, .{
                    .custom_draw_fn = CustomDrawMatchHighlight,
                    .custom_draw_ctx_as_bytes = std.mem.asBytes(&draw_ctx),
                });
                name_node.active_trans = button_node.active_trans;
                const name_size = name_node.rect.size();
                const name_color = name_node.text_color;

                ui.pushTmpStyle(.{ .font_size = name_node.font_size * 0.8 });
                ui.labelF("{s}", .{extra});
                const extra_node = ui.topParent().last.?;
                extra_node.size = [2]Size{ Size.text(1), Size.pixels(name_size[1], 1) };
                extra_node.text_color = vec4{
                    name_color[0] * 0.75,
                    name_color[1] * 0.75,
                    name_color[2] * 0.75,
                    name_color[3],
                };
                extra_node.flags.draw_active_effects = true;
                extra_node.active_trans = button_node.active_trans;

                if (button_node.signal.clicked) clicked_option = idx;
            }

            if (clicked_option) |idx|
                return self.slots[idx].ctx
            else
                return null;
        }

        const CustomDrawMatchHighlightCtx = struct {
            match_pattern: []const u8,
            highlight_color: vec4,
        };
        pub fn CustomDrawMatchHighlight(
            ui: *UI,
            shader_inputs: *std.ArrayList(UI.ShaderInput),
            node: *UI.Node,
        ) error{OutOfMemory}!void {
            const trace = tracy.Zone(@src());
            defer trace.End();

            const draw_ctx = if (node.custom_draw_ctx_as_bytes) |ctx_bytes| ctx: {
                std.debug.assert(ctx_bytes.len == @sizeOf(CustomDrawMatchHighlightCtx));
                // note: workaround for the same bug I found with UI.ShaderInput in
                // conjunction with vec4
                // break :ctx @ptrCast(*align(1) const CustomDrawMatchHighlightCtx, ctx_bytes.ptr).*;
                var rawbuf: [@sizeOf(CustomDrawMatchHighlightCtx) + 32]u8 = undefined;
                const align_amount = 32 - (@intFromPtr(&rawbuf[0]) % 32);
                const buf = rawbuf[align_amount .. align_amount + @sizeOf(CustomDrawMatchHighlightCtx)];
                @memcpy(buf, ctx_bytes);
                break :ctx @as(*align(32) const CustomDrawMatchHighlightCtx, @ptrFromInt(@intFromPtr(buf.ptr))).*;
            } else @panic("forgot to set the draw ctx");

            var text_pos = ui.textPosFromNode(node);
            if (node.flags.draw_active_effects) {
                text_pos[1] -= 0.1 * node.font_size * node.active_trans;
            }

            const display_text = node.display_string;

            // TODO: make this unicode compliant
            var cursor = [2]f32{ 0, 0 };
            for (display_text) |char| {
                const is_highlight = matches: {
                    for (draw_ctx.match_pattern) |match| {
                        if (match == char) break :matches true;
                        if (match >= 'A' and char >= 'A' and (match & 0x1f) == (char & 0x1f)) break :matches true;
                    }
                    break :matches false;
                };
                var font = if (is_highlight) &ui.font_bold else &ui.font;
                const text_color = if (is_highlight) draw_ctx.highlight_color else node.text_color;

                const quads = font.buildQuadsAt(ui.build_arena.allocator(), &.{ char, '_' }, node.font_size, cursor) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                };
                cursor += quads[1].points[0].pos - quads[0].points[0].pos;
                const quad = quads[0];
                var quad_rect = UI.Rect{ .min = quad.points[0].pos, .max = quad.points[2].pos };
                quad_rect.min += text_pos;
                quad_rect.max += text_pos;

                try shader_inputs.append(.{
                    .btm_left_pos = quad_rect.min,
                    .top_right_pos = quad_rect.max,
                    .btm_left_uv = quad.points[0].uv,
                    .top_right_uv = quad.points[2].uv,
                    .top_left_color = text_color,
                    .btm_left_color = text_color,
                    .top_right_color = text_color,
                    .btm_right_color = text_color,
                    .corner_radii = [4]f32{ 0, 0, 0, 0 },
                    .edge_softness = 0,
                    .border_thickness = 0,
                    .clip_rect_min = node.clip_rect.min,
                    .clip_rect_max = node.clip_rect.max,
                    .which_font = if (is_highlight)
                        @intFromEnum(UI.FontType.text_bold)
                    else
                        @intFromEnum(UI.FontType.text),
                });
            }
        }
    };
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

const MemoryStats = struct {
    virtual_size: usize,
    in_ram_size: usize,
    shared_size: usize,
};
pub fn getMemoryStats(allocator: Allocator) !MemoryStats {
    // note: we can use /proc/self/status for a more granular look at memory sizes
    const file = try std.fs.openFileAbsolute("/proc/self/statm", .{});
    defer file.close();
    const data = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(data);

    var tokenizer = std.mem.tokenize(u8, data, " ");
    var values: [3]u64 = undefined;
    for (&values) |*value| {
        value.* = try std.fmt.parseInt(u64, tokenizer.next().?, 0);
    }

    // the values we just parsed are measured in pages, not bytes
    const page_size = @as(u64, @intCast(c.sysconf(c._SC_PAGESIZE)));

    return MemoryStats{
        .virtual_size = values[0] * page_size,
        .in_ram_size = values[1] * page_size,
        .shared_size = values[2] * page_size,
    };
}
