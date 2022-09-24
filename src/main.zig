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
const Elf = @import("Elf.zig");
const Dwarf = @import("Dwarf.zig");
const SrcLoc = Dwarf.SrcLoc;

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

pub const SessionCmd = union(enum) {
    open_src_file: []const u8, // could be relative or absolute path
    set_break_at_src: SrcLoc,
    set_break_at_addr: usize,
    add_watched_variable: []const u8,
    continue_execution: void,
    pause_execution: void,
    step_line: void,
    step_instruction: void,
};

var show_ctx_menu = false;
var ctx_menu_top_left = @as(vec2, undefined);

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 8,
        .enable_memory_limit = true,
    }){};
    defer _ = general_purpose_allocator.detectLeaks();
    //const allocator = general_purpose_allocator.allocator();
    const allocator = std.heap.c_allocator;

    var cwd_buf: [0x4000]u8 = undefined;
    const cwd = try std.os.getcwd(&cwd_buf);

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

    var disasm_texts = std.ArrayList(AsmTextInfo).init(allocator);
    defer {
        for (disasm_texts.items) |text| text.deinit(allocator);
        disasm_texts.deinit();
    }

    // @debug
    try num_buf.appendSlice("85");
    try file_buf.appendSlice("main.zig");

    var file_tab = FileTab.init(allocator);
    defer file_tab.deinit();

    var session_cmds = std.ArrayList(SessionCmd).init(allocator);
    defer session_cmds.deinit();

    var frame_idx: u64 = 0;

    while (!window.should_close()) {
        //std.debug.print("frame_idx={}\n", .{frame_idx});

        var frame_arena = std.heap.ArenaAllocator.init(allocator);
        defer frame_arena.deinit();

        const framebuf_size = window.get_framebuffer_size();
        width = framebuf_size[0];
        height = framebuf_size[1];
        gl.viewport(0, 0, @intCast(i32, width), @intCast(i32, height));
        //const ratio = @intToFloat(f32, width) / @intToFloat(f32, height);

        const cur_time = @floatCast(f32, c.glfwGetTime());
        const dt = cur_time - last_time;
        last_time = cur_time;

        const mouse_pos = window.get_mouse_pos();
        //var mouse_diff = mouse_pos - last_mouse_pos;
        last_mouse_pos = mouse_pos;

        // should I move this to the end of the frame next to the session_cmd execution?
        if (session_opt) |*session| try session.fullUpdate();

        try ui.startBuild(width, height, mouse_pos, &window.event_queue);

        const whole_x_size = [2]Size{ Size.percent(1, 1), Size.text_dim(1) };
        ui.pushTmpStyle(.{ .pref_size = whole_x_size });
        _ = ui.textBoxF("#nodes={}, frame_time={d:2.4}ms ###info_text_box", .{ ui.node_table.key_mappings.items.len, dt * 1000 });

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
            const open_file_parent = ui.pushLayoutParent("open_file_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .x);
            {
                const open_button_sig = ui.button("Open Source File");
                const text_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = text_input_size });
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                const text_input_sig = ui.textInput("textinput", &src_file_buf.buffer, &src_file_buf.len);
                _ = ui.popStyle();
                _ = ui.popStyle();
                if (open_button_sig.clicked or text_input_sig.enter_pressed) {
                    try session_cmds.append(.{ .open_src_file = src_file_buf.slice() });
                }
                if (text_input_sig.focused and src_file_buf.len > 0) {
                    const text_input_node = ui.topParent().last.?;
                    ui.startCtxMenu(.{ .top_left = text_input_node.rect.min });
                    {
                        const children_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
                        const bg_color = vec4{ 0, 0, 0, 0.75 };
                        const tooltip_bg = ui.addNode(.{ .no_id = true, .draw_background = true }, "", .{
                            .bg_color = bg_color,
                            .pref_size = children_size,
                        });
                        ui.pushParent(tooltip_bg);
                        defer std.debug.assert(ui.popParent() == tooltip_bg);

                        const input = src_file_buf.slice();
                        const inner_dir = if (std.mem.lastIndexOfScalar(u8, input, '/')) |idx| blk: {
                            break :blk input[0 .. idx + 1];
                        } else "";

                        var tmpbuf: [0x1000]u8 = undefined;
                        const full_dir_path = try std.fmt.bufPrint(&tmpbuf, "{s}/{s}", .{ cwd, inner_dir });

                        var dir = try std.fs.openDirAbsolute(full_dir_path, .{ .iterate = true });
                        var dir_iter = dir.iterate();
                        const fill_x_size = [2]Size{ Size.percent(1, 1), Size.text_dim(1) };
                        ui.pushStyle(.{ .pref_size = fill_x_size });
                        while (try dir_iter.next()) |entry| {
                            const button_sig = ui.buttonF("{s}{s}", .{ inner_dir, entry.name });
                            ui.topParent().last.?.flags.draw_background = false;
                            ui.topParent().last.?.border_color = vec4{ 0, 0, 0, 0 };
                            if (button_sig.clicked) {
                                std.debug.print("TODO: switch input buffer to {s}\n", .{entry.name});
                            }
                        }
                        _ = ui.popStyle();
                    }
                    ui.endCtxMenu();
                }
            }
            std.debug.assert(ui.popParent() == open_file_parent);

            try file_tab.display(&ui, if (session_opt) |*s| s else null, &session_cmds);

            if (session_opt) |session| disasm_blk: {
                const rip = session.regs.rip;

                const disasm_text_idx = blk: for (disasm_texts.items) |text, i| {
                    if (text.addr_range[0] <= rip and rip < text.addr_range[1])
                        break :blk i;
                } else null;

                const disasm_text = if (disasm_text_idx) |idx| disasm_texts.items[idx] else text_blk: {
                    const function_addr_range = if (session.elf.findFunctionAtAddr(rip)) |func| blk: {
                        if (func.low_pc == null or func.high_pc == null) break :blk null;
                        break :blk [2]usize{ func.low_pc.?, func.high_pc.? };
                    } else null;

                    const section_addr_range: ?[2]usize = blk: {
                        var file = try std.fs.cwd().openFile(cmdline_args.exec_path.?, .{});
                        defer file.close();
                        const header = try std.elf.Header.read(file);
                        std.debug.assert(header.is_64);
                        var sh_iter = header.section_header_iterator(file);
                        while (try sh_iter.next()) |shdr| {
                            const addr_range = [2]usize{ shdr.sh_addr, shdr.sh_addr + shdr.sh_size };
                            if (addr_range[0] <= rip and rip < addr_range[1]) break :blk addr_range;
                        }
                        break :blk null;
                    };

                    const block_addr_range = if (function_addr_range) |func_range|
                        func_range
                    else if (section_addr_range) |section_range|
                        section_range
                    else blk: {
                        const mem_map = (try session.getMemMapAtAddr(allocator, rip)) orelse break :disasm_blk;
                        defer mem_map.deinit(allocator);
                        break :blk mem_map.addr_range;
                    };

                    const proc_mem = try session.procMemFile();
                    defer proc_mem.close();

                    try proc_mem.seekTo(block_addr_range[0]);
                    var mem_block = try allocator.alloc(u8, block_addr_range[1] - block_addr_range[0]);
                    defer allocator.free(mem_block);
                    std.debug.assert((try proc_mem.read(mem_block)) == mem_block.len);

                    std.debug.print("generating diasm (@ rip=0x{x}) for addr_range: 0x{x}-0x{x} (~{} instructions)...", .{
                        rip, block_addr_range[0], block_addr_range[1], @intToFloat(f32, block_addr_range[1] - block_addr_range[0]) / 4.5,
                    });
                    const start_time = c.glfwGetTime();
                    const text_info = try generateTextInfoForDisassembly(allocator, mem_block, block_addr_range[0]);
                    std.debug.print("done. (took {d}s)\n", .{c.glfwGetTime() - start_time});
                    try disasm_texts.append(text_info);
                    break :text_blk text_info;
                };

                const line_idx = loop: for (disasm_text.line_addrs) |addr, i| {
                    if (addr == rip) break :loop i;
                } else unreachable;

                _ = try showDisassemblyWindow(&ui, "main_disasm_window", disasm_text, @intToFloat(f32, line_idx + 1));
            }
        }
        std.debug.assert(ui.popParent() == left_side_parent);

        const right_side_parent = ui.pushLayoutParent("right_side_parent", [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) }, .y);
        right_side_parent.flags.draw_background = true;
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
                if (session.src_loc) |loc| {
                    _ = ui.textBoxF("src_loc: {s}:{}", .{ loc.file, loc.line });
                } else _ = ui.textBox("src_loc: null");
                ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);

                if (ui.button("Wait for Signal").clicked) {
                    std.debug.print("wait status: {}\n", .{Session.getWaitStatus(session.pid)});
                }

                const table_regs = .{ "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "rip" };
                const regs = session.regs;
                inline for (table_regs) |reg_name| {
                    _ = ui.textBoxF(reg_name ++ ": 0x{x:0>16}", .{@field(regs, reg_name)});
                }

                if (session.addr_range) |range| {
                    _ = ui.textBoxF("addr_range[0] = 0x{x:0>12}", .{range[0]});
                    _ = ui.textBoxF("addr_range[1] = 0x{x:0>12}", .{range[1]});
                }

                _ = ui.textBox("call stack:");
                for (session.call_stack) |call_frame| {
                    const function = call_frame.fn_name orelse "???";
                    if (call_frame.src) |src| {
                        _ = ui.textBoxF("0x{x:0>12}: {s} @ {s}/{s}:{}", .{ call_frame.addr, function, src.dir, src.file, src.line });
                    } else {
                        _ = ui.textBoxF("0x{x:0>12}: {s} @ ???", .{ call_frame.addr, function });
                    }
                }

                const vars_parent = ui.pushLayoutParent("vars_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .y);
                {
                    const row_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                    const column_box_size = [2]Size{ Size.percent(1.0 / 3.0, 1), Size.text_dim(1) };
                    const table_header_row_parent = ui.pushLayoutParent("table_header_row_parent", row_size, .x);
                    {
                        ui.pushStyle(.{ .pref_size = column_box_size });
                        _ = ui.textBox("Variable Name");
                        _ = ui.textBox("Type");
                        _ = ui.textBox("Value");
                        _ = ui.popStyle();
                    }
                    std.debug.assert(ui.popParent() == table_header_row_parent);

                    for (session.watched_vars.items) |var_info| {
                        const row_parent = ui.addNodeF(.{ .no_id = true }, "###row_parent_{s}", .{var_info.name}, .{ .child_layout_axis = .x });
                        row_parent.pref_size = row_size;
                        ui.pushParent(row_parent);
                        {
                            ui.pushStyle(.{ .pref_size = column_box_size });
                            _ = ui.textBoxF("{s}###{s}_name", .{ var_info.name, var_info.name });
                            _ = ui.textBoxF("{s}###{s}_type", .{ @tagName(std.meta.activeTag(var_info.value)), var_info.name });
                            switch (var_info.value) {
                                .Float => |value| _ = ui.textBoxF("{d}###{s}_value", .{ value, var_info.name }),
                            }
                            _ = ui.popStyle();
                        }
                        std.debug.assert(ui.popParent() == row_parent);
                    }
                }
                std.debug.assert(ui.popParent() == vars_parent);

                const add_var_parent = ui.pushLayoutParent("add_var_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .x);
                {
                    const var_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                    const button_sig = ui.button("Add Variable");
                    ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                    ui.pushStyle(.{ .pref_size = var_input_size });
                    const text_sig = ui.textInput("add_var_textinput", &var_buf.buffer, &var_buf.len);
                    _ = ui.popStyle();
                    _ = ui.popStyle();
                    if (button_sig.clicked or text_sig.enter_pressed) {
                        try session_cmds.append(.{ .add_watched_variable = var_buf.slice() });
                    }
                }
                std.debug.assert(ui.popParent() == add_var_parent);

                ui.spacer(.y, Size.percent(1, 0));

                const button_size = [2]Size{ Size.percent(0.5, 1), Size.text_dim(1) };
                ui.pushStyle(.{ .pref_size = button_size });
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

                const set_break_parent = ui.pushLayoutParent("set_break_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .x);
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
                        try session_cmds.append(.{ .set_break_at_src = .{
                            .dir = try std.fs.path.join(frame_arena.allocator(), &.{ cwd, "src" }),
                            .file = file_buf.slice(),
                            .line = line,
                            .column = 0,
                        } });
                    }
                }
                std.debug.assert(ui.popParent() == set_break_parent);
            }
        }
        std.debug.assert(ui.popParent() == right_side_parent);

        std.debug.assert(ui.popParent() == tabs_parent);

        ui.endBuild(dt);

        var update_src_focus = false;
        for (session_cmds.items) |cmd| {
            switch (cmd) {
                .continue_execution,
                .pause_execution,
                .step_line,
                .step_instruction,
                => update_src_focus = true,
                else => {},
            }
        }
        if (session_opt) |session| {
            if (last_session_status != session.status) {
                update_src_focus = true;
            }
            last_session_status = session.status;
        }

        // update src viewing window with the next session information
        if (update_src_focus) {
            if (session_opt) |*session| {
                if (session.src_loc) |loc| try file_tab.focusOnSrc(loc);
            }
        }

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        file_tab.updateAnimations(dt);

        // do all the state changes in one place
        if (session_cmds.items.len > 0) std.debug.print("commands in buffer @ frame idx {}\n", .{frame_idx});
        for (session_cmds.items) |cmd| {
            //std.debug.print("  - cmd: {s}\n", .{@tagName(std.meta.activeTag(cmd))});
            std.debug.print("  - {}\n", .{cmd});

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
                    //session.setBreakpointAtAddr(addr) catch |err| {
                    //    std.debug.print("{s}: failed to set breakpoint at addr 0x{x}\n", .{
                    //        @errorName(err), addr,
                    //    });
                    //};
                },
                .set_break_at_src => |src| case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.setBreakpointAtSrc(src);
                    //session.setBreakpointAtSrc(src) catch |err| {
                    //    std.debug.print("{s}: failed to set breakpoint at src={{.dir={s}, .file={s}, .line={}, .column={}}}\n", .{
                    //        @errorName(err), src.dir, src.file, src.line, src.column,
                    //    });
                    //};
                },
                .add_watched_variable => |var_name| case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.addWatchedVariable(var_name);
                    //session.addWatchedVariable(var_buf.slice()) catch |err| {
                    //    std.debug.print("{s}: failed to add watched varible '{s}'\n", .{
                    //        err, var_name,
                    //    });
                    //};
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
            }

            if (session_opt) |*session| try session.fullUpdate();
        }
        session_cmds.clearRetainingCapacity();

        window.update();
        frame_idx += 1;
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

    pub fn display(self: *FileTab, ui: *UiContext, session_opt: ?*Session, session_cmds: *std.ArrayList(SessionCmd)) !void {
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
            const line_sig = ui.getNodeSignal(line_scroll_parent);
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

            const text_sig = try textDisplay(ui, active_name, active_content, file.lock_line, file.focus_box);
            const text_scroll_node = file_box_parent.last.?;

            // if we manually scroll the text stop the line locking
            if (text_sig.scroll_offset[0] != 0 or text_sig.scroll_offset[1] != 0) {
                file.lock_line = null;
                file.target_lock_line = null;
            }

            const text_node_rect = text_scroll_node.rect;
            const right_click = ui.events.searchAndRemove(.MouseUp, c.GLFW_MOUSE_BUTTON_RIGHT);
            if (right_click and (text_sig.hovering or line_sig.hovering)) {
                show_ctx_menu = true;
                ctx_menu_top_left = ui.mouse_pos;
            }
            if (show_ctx_menu) {
                ui.startCtxMenu(.{ .top_left = ctx_menu_top_left });
                const ctx_menu_node = ui.topParent();

                const line_size = ui.font.getScaledMetrics().line_advance;
                const mouse_offset = text_node_rect.max[1] - ctx_menu_top_left[1] - UiContext.text_vpadding;
                const line_offset = mouse_offset - text_scroll_node.scroll_offset[1];
                const mouse_line = @floor(line_offset / line_size);
                const src_line = @floatToInt(u32, mouse_line) + 1;

                try session_cmds.append(.{ .set_break_at_src = .{
                    .dir = std.fs.path.dirname(file.path).?,
                    .file = std.fs.path.basename(file.path),
                    .line = src_line,
                    .column = 0,
                } });
                if (session_opt) |session| {
                    if (ui.buttonF("Set Breakpoint At Line {}\n", .{src_line}).clicked) blk: {
                        var break_loc = (try session.elf.dwarf.pathIntoSrc(file.path)) orelse {
                            std.debug.print("couldn't find {s} in the debug info file list\n", .{file.path});
                            break :blk;
                        };
                        break_loc.line = src_line;
                        session.setBreakpointAtSrc(break_loc) catch |err| {
                            std.debug.print("{s}: couldn't set breakpoint at {s}:{}\n", .{ @errorName(err), file.path, src_line });
                            break :blk;
                        };

                        show_ctx_menu = false;
                    }
                } else {
                    ui.label("No Debug Information Loaded");
                }

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

fn textDisplay(ui: *UiContext, label: []const u8, text: []const u8, lock_line: ?f32, highlight_box: ?FileTab.SrcBox) !UiContext.Signal {
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

    const x_off = &parent.scroll_offset[0];
    const y_off = &parent.scroll_offset[1];
    if (lock_line) |line| {
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

    return parent_sig;
}

const AsmTextInfo = struct {
    addr_range: [2]usize,
    data: []const u8,
    line_offsets: []const usize, // indices into `data`
    line_addrs: []const usize,

    pub fn deinit(self: AsmTextInfo, allocator: Allocator) void {
        allocator.free(self.data);
        allocator.free(self.line_offsets);
        allocator.free(self.line_addrs);
    }
};

/// don't forget to call `TextInfo.deinit` when done with it
fn generateTextInfoForDisassembly(allocator: Allocator, data: []const u8, data_start_addr: usize) !AsmTextInfo {
    var text_bytes = std.ArrayList(u8).init(allocator);
    var line_offsets = std.ArrayList(usize).init(allocator);
    var line_addrs = std.ArrayList(usize).init(allocator);

    var instructions_done: usize = 0;
    var data_idx: usize = 0;
    asm_loop: while (data_idx < data.len) {
        const asm_addr = data_idx + data_start_addr;
        const inst_bytes = data[data_idx..std.math.min(data_idx + 15, data.len)];

        var decoded_inst: c.xed_decoded_inst_t = undefined;
        c.xed_decoded_inst_zero(&decoded_inst);
        c.xed_decoded_inst_set_mode(&decoded_inst, c.XED_MACHINE_MODE_LONG_64, c.XED_ADDRESS_WIDTH_64b);
        switch (c.xed_decode(&decoded_inst, &inst_bytes[0], @intCast(c_uint, inst_bytes.len))) {
            c.XED_ERROR_NONE => {},
            c.XED_ERROR_BUFFER_TOO_SHORT => break :asm_loop,
            else => |err| std.debug.panic("data_idx=0x{x}, xed_error: {s}\n", .{ data_idx, c.xed_error_enum_t2str(err) }),
        }

        var buffer: [0x1000]u8 = undefined;
        const format_res = c.xed_format_context(c.XED_SYNTAX_INTEL, &decoded_inst, &buffer[0], @intCast(c_int, buffer.len), asm_addr, null, null);
        std.debug.assert(format_res == 1);
        const str = std.mem.sliceTo(@ptrCast([*c]const u8, &buffer[0]), 0);

        const inst_len = c.xed_decoded_inst_get_length(&decoded_inst);
        data_idx += inst_len;

        var fmt_buf: [0x1000]u8 = undefined;
        var stream = std.io.fixedBufferStream(&fmt_buf);
        var writer = stream.writer();
        try writer.print("0x{x:0>12}: ", .{asm_addr});
        for (inst_bytes) |byte, idx| {
            if (idx < inst_len) {
                try writer.print("{x:0>2} ", .{byte});
            } else {
                try writer.print("   ", .{});
            }
        }
        try writer.print("{s}\n", .{str});
        const asm_line = stream.getWritten();

        try line_offsets.append(text_bytes.items.len);
        try line_addrs.append(asm_addr);
        try text_bytes.appendSlice(asm_line);

        instructions_done += 1;
    }

    return AsmTextInfo{
        .addr_range = [2]usize{ data_start_addr, data_start_addr + data_idx },
        .data = text_bytes.toOwnedSlice(),
        .line_offsets = line_offsets.toOwnedSlice(),
        .line_addrs = line_addrs.toOwnedSlice(),
    };
}

fn showDisassemblyWindow(ui: *UiContext, label: []const u8, text_info: AsmTextInfo, lock_line: ?f32) !UiContext.Signal {
    // TODO: hightlight box ranges

    const text_box_size = [2]Size{ Size.percent(1, 0), Size.percent(0.3, 0) };
    const parent = ui.addNodeF(.{
        .scrollable = true,
        .clip_children = true,
    }, "###{s}:parent", .{label}, .{ .child_layout_axis = .y, .pref_size = text_box_size });
    const parent_sig = ui.getNodeSignal(parent);
    ui.pushParent(parent);
    defer std.debug.assert(ui.popParent() == parent);

    const parent_size = parent.rect.size();
    const line_size = ui.font.getScaledMetrics().line_advance;

    const x_off = &parent.scroll_offset[0];
    const y_off = &parent.scroll_offset[1];
    if (lock_line) |line| {
        y_off.* = -line_size * line + parent_size[1] / 2;
    }

    const lines_that_fit = @trunc(parent_size[1] / line_size);
    const cur_middle_line = @trunc(std.math.max(0, -y_off.* + parent_size[1] / 2) / line_size);
    const partial_start_line = @floatToInt(usize, @trunc(std.math.max(0, cur_middle_line - lines_that_fit)));
    const partial_end_line = @floatToInt(usize, @trunc(cur_middle_line + lines_that_fit));

    const total_lines = text_info.line_offsets.len;
    const partial_start_idx = text_info.line_offsets[partial_start_line];
    const partial_end_idx = text_info.line_offsets[partial_end_line];
    const partial_text = text_info.data[partial_start_idx..partial_end_idx];

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

    const highlight_box: ?FileTab.SrcBox = if (lock_line) |line| FileTab.SrcBox{
        .min = .{ .line = line, .column = 0 },
        .max = .{ .line = line, .column = 0 },
    } else null;
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

    return parent_sig;
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
