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
pub const Icons = struct {
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
    add_watched_variable: Dwarf.Variable,
    continue_execution: void,
    pause_execution: void,
    step_line: void,
    step_instruction: void,
    dump_ui_tree: []const u8, // path of graphviz save file
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

    var cwd_buf: [0x4000]u8 = undefined;
    const cwd = try std.os.getcwd(&cwd_buf);

    const arg_slices = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, arg_slices);
    const cmdline_args = parseCmdlineArgs(arg_slices);

    c.xed_tables_init();

    var width: u32 = 1600;
    var height: u32 = 900;
    var window = try Window.init(allocator, width, height, "pestctl");
    window.finish_setup();
    defer window.deinit();

    // GL state that we never change
    gl.clearColor(0.75, 0.36, 0.38, 1);
    gl.enable(gl.CULL_FACE);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
    gl.enable(gl.LINE_SMOOTH);

    var ui = try UiContext.init(allocator, "VictorMono-Regular.ttf", "VictorMono-Bold.ttf", "icons.ttf", &window);
    defer ui.deinit();

    // this second UiContext is meant to use for debugging the normal ui (but who debugs this one? 🤔)
    var dbg_ui = try UiContext.init(allocator, "VictorMono-Regular.ttf", "VictorMono-Bold.ttf", "icons.ttf", &window);
    defer dbg_ui.deinit();
    var dbg_ui_active = false;
    var dbg_ui_node_list_idx: usize = 0;
    var dbg_ui_frozen_mouse_pos: ?vec2 = null;
    var dbg_ui_node_info_top_left: ?vec2 = null;

    var session_opt = if (cmdline_args.exec_path) |path| try Session.init(allocator, path) else null;
    defer if (session_opt) |*session| session.deinit();
    var totals = [4]usize{ 0, 0, 0, 0 };
    for (session_opt.?.elf.dwarf.units) |unit, unit_idx| {
        std.debug.print("debug unit #{} has: {} variables ({} globals), {} types, {} functions\n", .{
            unit_idx, unit.variables.len, unit.global_vars.len, unit.types.len, unit.functions.len,
        });
        totals[0] += unit.variables.len;
        totals[1] += unit.global_vars.len;
        totals[2] += unit.types.len;
        totals[3] += unit.functions.len;
    }
    std.debug.print("totals: {} variables ({} globals), {} types, {} functions\n", .{
        totals[0], totals[1], totals[2], totals[3],
    });

    var last_time = @floatCast(f32, c.glfwGetTime());

    var src_file_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var src_file_search = FuzzySearchOptions(SrcFileSearchCtx, 20).init(allocator);
    defer src_file_search.deinit();
    var num_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var file_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var var_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var var_search = FuzzySearchOptions(VarSearchCtx, 10).init(allocator);
    defer var_search.deinit();
    var func_buf = try std.BoundedArray(u8, 0x1000).init(0);
    var func_search = FuzzySearchOptions(FuncSearchCtx, 10).init(allocator);
    defer func_search.deinit();

    var disasm_texts = std.ArrayList(AsmTextInfo).init(allocator);
    defer {
        for (disasm_texts.items) |text| text.deinit(allocator);
        disasm_texts.deinit();
    }

    var file_tab = FileTab.init(allocator);
    defer file_tab.deinit();

    var last_src_loc = @as(?SrcLoc, null);

    const widget_tabs = [_][]const u8{ "Registers", "Call Stack", "Memory Maps" };
    var widget_tab_active_idx: usize = 1;
    var call_stack_viewer = CallStackViewer.init(allocator);
    defer call_stack_viewer.deinit();

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

        try ui.startBuild(width, height, mouse_pos, &window.event_queue);

        const fill_x_size = [2]Size{ Size.percent(1, 1), Size.text_dim(1) };

        const mem_stats = try getMemoryStats(allocator);
        ui.pushTmpStyle(.{ .pref_size = fill_x_size });
        ui.labelBoxF("#nodes={}, frame_time={d:2.4}ms, mem(ram/virtual/shared): {d:.2}/{d:.2}/{d:.2}, gpa requested bytes: {d:.2}", .{
            ui.node_table.key_mappings.items.len,
            dt * 1000,
            std.fmt.fmtIntSizeBin(mem_stats.in_ram_size),
            std.fmt.fmtIntSizeBin(mem_stats.virtual_size),
            std.fmt.fmtIntSizeBin(mem_stats.shared_size),
            std.fmt.fmtIntSizeBin(general_purpose_allocator.total_requested_bytes),
        });

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
                    const input = src_file_buf.slice();

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
                        &frame_arena,
                        &ui,
                        "filepath_chooser",
                        .{ .top_left = text_input_node.rect.min },
                        false,
                    );
                    if (choice) |file_ctx| {
                        std.debug.print("TODO: switch input buffer to {s}\n", .{file_ctx.name});
                    }
                }
            }
            ui.popParentAssert(open_file_parent);

            try file_tab.display(&ui, &session_cmds);

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

                    //std.debug.print("generating disasm (@ rip=0x{x}) for addr_range: 0x{x}-0x{x} (~{} instructions)...", .{
                    //    rip, block_addr_range[0], block_addr_range[1], @intToFloat(f32, block_addr_range[1] - block_addr_range[0]) / 4.5,
                    //});
                    //const start_time = c.glfwGetTime();
                    const text_info = try generateTextInfoForDisassembly(allocator, mem_block, block_addr_range[0]);
                    //std.debug.print("done. (took {d}s)\n", .{c.glfwGetTime() - start_time});
                    try disasm_texts.append(text_info);
                    break :text_blk text_info;
                };

                const line_idx = loop: for (disasm_text.line_addrs) |addr, i| {
                    if (addr == rip) break :loop i;
                } else unreachable;

                _ = try showDisassemblyWindow(&ui, "main_disasm_window", disasm_text, @intToFloat(f32, line_idx + 1));
            }
        }
        ui.popParentAssert(left_side_parent);

        const right_side_parent = ui.pushLayoutParent("right_side_parent", [2]Size{ Size.percent(0.5, 1), Size.percent(1, 0) }, .y);
        right_side_parent.flags.draw_background = true;
        if (session_opt) |*session| {
            ui.labelBoxF("Child pid: {}", .{session.pid});
            ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
            switch (session.status) {
                .Stopped => {
                    ui.labelBox("Child Status: Stopped");
                    ui.topParent().last.?.text_color = vec4{ 1, 0.5, 0, 1 };
                },
                .Running => ui.labelBox("Child Status: Running"),
            }
            ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);
            if (session.src_loc) |loc| {
                ui.labelBoxF("src_loc: {s}:{}", .{ loc.file, loc.line });
            } else ui.labelBox("src_loc: null");
            ui.topParent().last.?.pref_size[0] = Size.percent(1, 1);

            if (ui.button("Wait for Signal").clicked) {
                std.debug.print("wait status: {}\n", .{Session.getWaitStatus(session.pid)});
            }

            if (ui.button("Force Send `ptrace(.CONT)`").clicked) {
                try Session.ptrace(.CONT, session.pid, 0);
            }

            if (session.addr_range) |range| {
                ui.labelBoxF("addr_range[0] = 0x{x:0>12}", .{range[0]});
                ui.labelBoxF("addr_range[1] = 0x{x:0>12}", .{range[1]});
            }

            {
                const right_side_tabs_parent = ui.pushLayoutParent("right_side_tabs_parent", [2]Size{ Size.percent(1, 0), Size.by_children(1) }, .y);
                defer ui.popParentAssert(right_side_tabs_parent);

                const widget_buttons_parent = ui.pushLayoutParent("widget_buttons_parent", [2]Size{ Size.percent(1, 0), Size.by_children(1) }, .x);
                for (widget_tabs) |widget_name, idx| {
                    const is_active = widget_tab_active_idx == idx;
                    if (is_active) ui.pushTmpStyle(.{ .bg_color = app_style.highlight_color });
                    const btn_sig = ui.button(widget_name);
                    //std.debug.print("{s}: .hovering={}, .pressed={}, .clicked={}, .released={}\n", .{
                    //    widget_name, btn_sig.hovering, btn_sig.pressed, btn_sig.clicked, btn_sig.released,
                    //});
                    if (btn_sig.clicked) widget_tab_active_idx = idx;
                }
                ui.popParentAssert(widget_buttons_parent);

                const tab_choice = widget_tabs[widget_tab_active_idx];
                if (std.mem.eql(u8, tab_choice, "Registers")) {
                    showRegisters(&ui, session.regs);
                } else if (std.mem.eql(u8, tab_choice, "Call Stack")) {
                    try call_stack_viewer.display(&ui, session.call_stack, session.*);
                } else {
                    ui.labelF("TODO: <{s}> here", .{tab_choice});
                }
            }

            if (ui.button("print memory mappings").clicked) {
                const maps = try session.getMemMaps(allocator);
                defer {
                    for (maps) |map| map.deinit(allocator);
                    allocator.free(maps);
                }
                std.debug.print("memory mappings for pid={}\n", .{session.pid});
                std.debug.print("         address range        | perm |   offset   | device|   inode  | path\n", .{});
                std.debug.print("------------------------------+------+------------+-------+----------+-----------\n", .{});
                for (maps) |map| {
                    std.debug.print("0x{x:0>12}-0x{x:0>12} | {c}{c}{c}{c} | 0x{x:0>8} | {x:0>2}:{x:0>2} | {d: >8} | {s}\n", .{
                        map.addr_range[0],
                        map.addr_range[1],
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
            }

            const vars_parent = ui.pushLayoutParent("vars_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .y);
            {
                const row_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
                const column_box_size = [2]Size{ Size.percent(1.0 / 3.0, 1), Size.text_dim(1) };
                const table_header_row_parent = ui.pushLayoutParent("table_header_row_parent", row_size, .x);
                {
                    ui.pushStyle(.{ .pref_size = column_box_size });
                    ui.labelBox("Variable Name");
                    ui.labelBox("Type");
                    ui.labelBox("Value");
                    _ = ui.popStyle();
                }
                ui.popParentAssert(table_header_row_parent);

                for (session.watched_vars.items) |var_info| {
                    const var_name = var_info.name;
                    const row_parent = ui.addNodeF(.{ .no_id = true }, "###row_parent_{s}", .{var_name}, .{ .child_layout_axis = .x });
                    row_parent.pref_size = row_size;
                    ui.pushParent(row_parent);
                    {
                        ui.pushStyle(.{ .pref_size = column_box_size });
                        ui.labelBoxF("{s}", .{var_name});
                        ui.labelBoxF("{s}", .{if (var_info.@"type") |ty| @tagName(std.meta.activeTag(ty.*)) else "???"});
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

            const add_var_parent = ui.pushLayoutParent("add_var_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .x);
            {
                const var_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                //const button_sig = ui.button("Add Variable");
                ui.labelBox("Add Variable");
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                ui.pushStyle(.{ .pref_size = var_input_size });
                const text_input_sig = ui.textInput("add_var_textinput", &var_buf.buffer, &var_buf.len);
                _ = ui.popStyle();
                _ = ui.popStyle();
                //if (button_sig.clicked or text_input_sig.enter_pressed) {
                //    try session_cmds.append(.{ .add_watched_variable = var_buf.slice() });
                //}
                if (text_input_sig.focused and var_buf.len > 0) {
                    const text_input_node = ui.topParent().last.?;

                    if (!std.mem.eql(u8, var_buf.slice(), var_search.target)) {
                        try var_search.resetSearch(var_buf.slice());
                        // here we only shows/score variables that would be possible to choose
                        // (why? a debug build of the zig compiler has ~700k variables)
                        // always add all the globals
                        for (session.elf.dwarf.units) |unit, unit_idx| {
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
                        for (session.elf.dwarf.units) |unit, unit_idx| {
                            for (unit.functions) |func, func_idx| {
                                if (func.low_pc == null or func.high_pc == null) continue;
                                const addr = session.regs.rip;
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
                        &frame_arena,
                        &ui,
                        "var_table_chooser",
                        .{ .top_left = text_input_node.rect.min },
                        false,
                    );
                    if (choice) |var_ctx| {
                        try session_cmds.append(.{ .add_watched_variable = var_ctx.variable });
                    }
                }
            }
            ui.popParentAssert(add_var_parent);

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
                ui.labelBox("Line Number");
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                ui.pushStyle(.{ .pref_size = line_input_size });
                const line_sig = ui.textInput("src_linenum_textinput", &num_buf.buffer, &num_buf.len);
                _ = ui.popStyle();
                _ = ui.popStyle();

                const file_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                ui.labelBox("File Name");
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
            ui.popParentAssert(set_break_parent);

            const set_break_func_parent = ui.pushLayoutParent("set_break_func_parent", [2]Size{ Size.percent(1, 1), Size.by_children(1) }, .x);
            {
                const button_sig = ui.button("Set Breakpoint###set_func_breakpoint");
                _ = button_sig;

                const func_input_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
                ui.labelBox("Function");
                ui.pushStyle(.{ .bg_color = vec4{ 0.75, 0.75, 0.75, 1 } });
                ui.pushStyle(.{ .pref_size = func_input_size });
                const text_input_sig = ui.textInput("src_funcname", &func_buf.buffer, &func_buf.len);
                _ = ui.popStyle();
                _ = ui.popStyle();

                //if (button_sig.clicked or func_sig.enter_pressed) {
                if (text_input_sig.focused and func_buf.len > 0) {
                    const text_input_node = ui.topParent().last.?;

                    if (!std.mem.eql(u8, func_buf.slice(), func_search.target)) {
                        try func_search.resetSearch(func_buf.slice());
                        for (session.elf.dwarf.units) |unit, unit_idx| {
                            for (unit.functions) |func, func_idx| {
                                if (func.name == null) continue;
                                func_search.addEntry(func.name.?, .{
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
                    const choice = try func_search.present(
                        &frame_arena,
                        &ui,
                        "break_func_chooser",
                        .{ .btm_left = vec2{ text_input_node.rect.min[0], text_input_node.rect.max[1] } },
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
            ui.popParentAssert(set_break_func_parent);
        }
        ui.popParentAssert(right_side_parent);

        ui.popParentAssert(tabs_parent);

        ui.endBuild(dt);

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        try ui.render();

        // special debug commands
        if (window.event_queue.searchAndRemove(.KeyUp, .{
            .mods = .{ .control = true, .shift = true },
            .key = c.GLFW_KEY_T,
        })) {
            try session_cmds.append(.{ .dump_ui_tree = "ui_main_tree.dot" });
        }
        if (window.event_queue.searchAndRemove(.KeyUp, .{
            .mods = .{ .control = true, .shift = true },
            .key = c.GLFW_KEY_D,
        })) dbg_ui_active = !dbg_ui_active;
        if (dbg_ui_active) dbg_ui_blk: {
            var select_mouse_pos = if (dbg_ui_frozen_mouse_pos) |pos| pos else mouse_pos;
            var selected_nodes = std.ArrayList(*UiContext.Node).init(allocator);
            defer selected_nodes.deinit();
            var node_iter = ui.node_table.valueIterator();
            while (node_iter.next()) |node| {
                if (node.rect.contains(select_mouse_pos)) try selected_nodes.append(node);
            }
            if (selected_nodes.items.len == 0) break :dbg_ui_blk;

            if (window.event_queue.fetchAndRemove(.MouseScroll, null)) |scroll_ev| {
                const scroll = -@floatToInt(isize, scroll_ev.y);
                if (scroll > 0) dbg_ui_node_list_idx += @intCast(usize, scroll);
                if (scroll < 0) {
                    if (-scroll > dbg_ui_node_list_idx) {
                        dbg_ui_node_list_idx = 0;
                    } else {
                        dbg_ui_node_list_idx -= @intCast(usize, -scroll);
                    }
                }
            }
            dbg_ui_node_list_idx = std.math.clamp(dbg_ui_node_list_idx, 0, selected_nodes.items.len - 1);

            if (window.event_queue.find(.MouseUp, c.GLFW_MOUSE_BUTTON_MIDDLE)) |ev_idx| blk: {
                const mods = window.get_modifiers();
                if (!(mods.shift and mods.control)) break :blk;
                if (dbg_ui_node_info_top_left != null) {
                    dbg_ui_node_info_top_left = null;
                } else {
                    dbg_ui_node_info_top_left = mouse_pos;
                }
                _ = window.event_queue.removeAt(ev_idx);
            }

            if (window.event_queue.searchAndRemove(.KeyUp, .{
                .key = c.GLFW_KEY_F,
                .mods = .{ .shift = true, .control = true },
            })) {
                if (dbg_ui_frozen_mouse_pos != null) {
                    std.debug.print("dbg_ui: unfreezing mouse_pos\n", .{});
                    dbg_ui_frozen_mouse_pos = null;
                } else {
                    std.debug.print("dbg_ui: freezing mouse_pos @ {d}\n", .{mouse_pos});
                    dbg_ui_frozen_mouse_pos = mouse_pos;
                }
            }

            try dbg_ui.startBuild(width, height, mouse_pos, &window.event_queue);

            for (selected_nodes.items) |node| {
                const box_node = dbg_ui.addNode(.{
                    .no_id = true,
                    .draw_border = true,
                    .floating_x = true,
                    .floating_y = true,
                }, "", .{});
                box_node.border_color = vec4{ 1, 0, 0, 0.75 };
                const size = node.rect.size();
                box_node.rel_pos = node.rect.min;
                box_node.pref_size = [2]Size{ Size.pixels(size[0], 1), Size.pixels(size[1], 1) };
            }
            const selected_node = selected_nodes.items[dbg_ui_node_list_idx];
            const selected_box_node = dbg_ui.addNode(.{
                .no_id = true,
                .draw_border = true,
                .floating_x = true,
                .floating_y = true,
            }, "", .{});
            selected_box_node.border_color = vec4{ 0, 1, 0, 0.75 };
            const size = selected_node.rect.size();
            selected_box_node.rel_pos = selected_node.rect.min;
            selected_box_node.pref_size = [2]Size{ Size.pixels(size[0], 1), Size.pixels(size[1], 1) };

            if (dbg_ui_node_info_top_left) |top_left| {
                dbg_ui.startCtxMenu(.{ .top_left = top_left });
                defer dbg_ui.endCtxMenu();
                dbg_ui.ctx_menu_root_node.?.child_layout_axis = .x;
                {
                    const left_bg_node = dbg_ui.addNode(.{
                        .no_id = true,
                        .draw_background = true,
                    }, "", .{ .child_layout_axis = .y });
                    left_bg_node.bg_color = vec4{ 0, 0, 0, 1 };
                    left_bg_node.pref_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
                    dbg_ui.pushParent(left_bg_node);
                    defer dbg_ui.popParentAssert(left_bg_node);

                    dbg_ui.pushStyle(.{ .pref_size = fill_x_size });
                    for (selected_nodes.items) |node, idx| {
                        if (idx == dbg_ui_node_list_idx) {
                            _ = dbg_ui.textBoxF("hash=\"{s}\"", .{node.hash_string});
                        } else {
                            dbg_ui.labelF("hash=\"{s}\"", .{node.hash_string});
                        }
                    }
                    _ = dbg_ui.popStyle();
                }
                {
                    const right_bg_node = dbg_ui.addNode(.{
                        .no_id = true,
                        .draw_background = true,
                    }, "", .{ .child_layout_axis = .y });
                    right_bg_node.bg_color = vec4{ 0, 0, 0, 1 };
                    right_bg_node.pref_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
                    dbg_ui.pushParent(right_bg_node);
                    defer dbg_ui.popParentAssert(right_bg_node);

                    const node = selected_nodes.items[dbg_ui_node_list_idx];
                    inline for ([_][]const u8{ "first", "last", "next", "prev", "parent" }) |node_link| {
                        if (@field(node, node_link)) |link| {
                            const hash_str = link.hash_string;
                            if (hash_str.len > 30) {
                                dbg_ui.labelF(node_link ++ ".hash_string=\"{s}...\"", .{hash_str[0..27]});
                            } else {
                                dbg_ui.labelF(node_link ++ ".hash_string=\"{s}\"", .{hash_str});
                            }
                        }
                    }
                    if (node.child_count > 0) dbg_ui.labelF("child_count={d}", .{node.child_count});
                    if (node.flags.draw_text) {
                        if (node.display_string.len > 30) {
                            dbg_ui.labelF("display_string=\"{s}...\"", .{node.display_string[0..27]});
                        } else {
                            dbg_ui.labelF("display_string=\"{s}\"", .{node.display_string});
                        }
                    }
                    inline for (@typeInfo(UiContext.Flags).Struct.fields) |field| {
                        const flag = @field(node.flags, field.name);
                        if (flag) dbg_ui.labelF("flags.{s}={}\n", .{ field.name, flag });
                    }
                    if (node.flags.draw_background) dbg_ui.labelF("bg_color={d}", .{node.bg_color});
                    if (node.flags.draw_border) dbg_ui.labelF("border_color={d}", .{node.border_color});
                    if (node.flags.draw_text) dbg_ui.labelF("text_color={d}", .{node.text_color});
                    dbg_ui.labelF("corner_roundness={d}\n", .{node.corner_roundness});
                    dbg_ui.labelF("border_thickness={d}\n", .{node.border_thickness});
                    dbg_ui.labelF("pref_size={any}\n", .{node.pref_size});
                    dbg_ui.labelF("child_layout_axis={}\n", .{node.child_layout_axis});
                    dbg_ui.labelF("cursor_type={}\n", .{node.cursor_type});
                    dbg_ui.labelF("font_type={}\n", .{node.font_type});
                    dbg_ui.labelF("font_size={d}\n", .{node.font_size});
                    dbg_ui.labelF("text_align={}\n", .{node.text_align});
                    dbg_ui.labelF("text_rect={}\n", .{node.text_rect});
                    dbg_ui.labelF("calc_size={d}\n", .{node.calc_size});
                    dbg_ui.labelF("calc_rel_pos={d}\n", .{node.calc_rel_pos});
                    dbg_ui.labelF("rect={}\n", .{node.rect});
                    dbg_ui.labelF("clip_rect={d}\n", .{node.clip_rect});
                    dbg_ui.labelF("hot_trans={d:.4}\n", .{node.hot_trans});
                    dbg_ui.labelF("active_trans={d:.4}\n", .{node.active_trans});
                    dbg_ui.labelF("rel_pos={d}\n", .{node.rel_pos});
                    dbg_ui.labelF("rel_pos_placement={s}\n", .{@tagName(node.rel_pos_placement)});
                    dbg_ui.labelF("rel_pos_placement_parent={s}\n", .{@tagName(node.rel_pos_placement_parent)});
                    dbg_ui.labelF("cursor={}\n", .{node.cursor});
                    dbg_ui.labelF("mark={}\n", .{node.mark});
                    dbg_ui.labelF("scroll_offset={d}\n", .{node.scroll_offset});
                    dbg_ui.labelF("toggle={}\n", .{node.toggle});
                }
            }

            dbg_ui.endBuild(dt);
            try dbg_ui.render();
        }

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
                .add_watched_variable => |variable| case_blk: {
                    const session = if (session_opt) |*s| s else break :case_blk;
                    try session.watched_vars.append(variable);
                    //session.watched_vars.append(variable) catch |err| {
                    //    std.debug.print("{s}: failed to add watched variable '{s}'\n", .{
                    //        err, variable.name,
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
                .dump_ui_tree => |path| {
                    std.debug.print("dumping root tree to {s}\n", .{path});
                    try ui.dumpNodeTreeGraph(ui.root_node.?, path);
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
        for (content) |char, i| {
            if (char != '\n') continue;
            try line_offsets.append(i);
        }
        errdefer line_offsets.deinit();

        try self.files.append(.{
            .path = dupe_path,
            .content = content,
            .line_offsets = line_offsets.toOwnedSlice(),
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

    pub fn display(self: *FileTab, ui: *UiContext, session_cmds: *std.ArrayList(SessionCmd)) !void {
        const trace = tracy.Zone(@src());
        defer trace.End();
        const file_tab_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
        const file_tab_node = ui.addNode(.{
            .draw_border = true,
        }, "FileTab:top_node", .{ .pref_size = file_tab_size });
        ui.pushParent(file_tab_node);

        const buttons_parent_size = [2]Size{ Size.percent(1, 0), Size.by_children(1) };
        const buttons_parent = ui.pushLayoutParent("FileTab:buttons_parent", buttons_parent_size, .x);
        for (self.files.items) |file_info, i| {
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
            const file_box_parent_size = [2]Size{ Size.percent(1, 1), Size.percent(1, 0) };
            const file_box_parent = ui.addNode(.{
                .clip_children = true,
                .draw_border = true,
            }, "FileTab:text_box_parent", .{});
            file_box_parent.pref_size = file_box_parent_size;
            file_box_parent.child_layout_axis = .x;
            ui.pushParent(file_box_parent);

            const line_scroll_size = [2]Size{ Size.by_children(1), Size.percent(1, 0) };
            const line_scroll_parent = ui.addNodeF(.{
                .scrollable = true,
                .clip_children = true,
            }, "###{s}::line_scroll_parent", .{file.path}, .{ .pref_size = line_scroll_size });
            const line_sig = line_scroll_parent.signal;
            ui.pushParent(line_scroll_parent);
            const line_text_node = blk: {
                const n_lines = file.line_offsets.len;
                const max_line_fmt_size = @floatToInt(usize, @ceil(@log10(@intToFloat(f32, n_lines)))) + 1;
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
                line_text_node.rel_pos_placement = .top_left;
                line_text_node.rel_pos_placement_parent = .top_left;

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
                const ctx_menu_trace = tracy.ZoneN(@src(), "file ctx_menu");
                defer ctx_menu_trace.End();
                ui.startCtxMenu(.{ .top_left = ctx_menu_top_left });
                const ctx_menu_node = ui.topParent();

                const font_pixel_size = ui.topStyle().font_size;
                const line_size = ui.font.getScaledMetrics(font_pixel_size).line_advance;
                const mouse_offset = text_node_rect.max[1] - ctx_menu_top_left[1] - UiContext.text_vpadding;
                const line_offset = mouse_offset - text_scroll_node.scroll_offset[1];
                const mouse_line = @floor(line_offset / line_size);
                const src_line = @floatToInt(u32, mouse_line) + 1;

                if (ui.buttonF("Set Breakpoint At Line {}\n", .{src_line}).clicked) {
                    try session_cmds.append(.{ .set_break_at_src = .{
                        .dir = std.fs.path.dirname(file.path).?,
                        .file = std.fs.path.basename(file.path),
                        .line = src_line,
                        .column = 0,
                    } });
                    show_ctx_menu = false;
                }

                if (ui.events.match(.MouseDown, {})) |_| {
                    if (!ctx_menu_node.rect.contains(ui.mouse_pos)) show_ctx_menu = false;
                }

                ui.endCtxMenu();
            }

            // scroll the line numbers with the src text
            line_scroll_parent.scroll_offset[1] = text_scroll_node.scroll_offset[1];
            line_text_node.rel_pos[1] = -line_scroll_parent.scroll_offset[1];

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

fn showDisassemblyWindow(ui: *UiContext, label: []const u8, asm_text_info: AsmTextInfo, lock_line: ?f32) !UiContext.Signal {
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
    ui: *UiContext,
    label: []const u8,
    size: [2]UiContext.Size,
    text_info: TextDisplayInfo,
    lock_line: ?f32,
    boxes: []const FileTab.SrcBox,
) !UiContext.Signal {
    const parent = ui.startScrollRegion(label);
    parent.pref_size = size;
    const parent_sig = parent.signal;
    const parent_size = parent.rect.size();

    const font_pixel_size = ui.topStyle().font_size;
    const line_size = ui.font.getScaledMetrics(font_pixel_size).line_advance;

    const x_off = &parent.scroll_offset[0];
    const y_off = &parent.scroll_offset[1];
    if (lock_line) |line| y_off.* = -line_size * line + parent_size[1] / 2;

    const total_lines = text_info.line_offsets.len;

    const lines_that_fit = @trunc(parent_size[1] / line_size);
    const cur_middle_line = @trunc(std.math.max(0, -y_off.* + parent_size[1] / 2) / line_size);
    const partial_start_line = @floatToInt(usize, @trunc(std.math.max(0, cur_middle_line - lines_that_fit)));
    const partial_end_line = std.math.min(@floatToInt(usize, @trunc(cur_middle_line + lines_that_fit)), total_lines);

    const partial_start_idx = text_info.line_offsets[partial_start_line];
    const partial_end_idx = if (partial_end_line == total_lines)
        text_info.content.len
    else
        text_info.line_offsets[partial_end_line];
    const partial_text = text_info.content[partial_start_idx..partial_end_idx];

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
    x_off.* = std.math.clamp(x_off.*, -max_offset[0], 0);
    y_off.* = std.math.clamp(y_off.*, -max_offset[1], 0);

    label_node.rel_pos_placement = .top_left;
    label_node.rel_pos_placement_parent = .top_left;
    label_node.rel_pos = vec2{ x_off.*, -y_off.* - @intToFloat(f32, partial_start_line) * line_size };

    for (boxes) |box| {
        if (box.min.line == 0 and box.max.line == 0) break;

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

    ui.endScrollRegion(parent, 0, -max_offset[1]);

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
        const inst_bytes = data[data_idx..std.math.min(data_idx + 15, data.len)];

        var decoded_inst: c.xed_decoded_inst_t = undefined;
        c.xed_decoded_inst_zero(&decoded_inst);
        c.xed_decoded_inst_set_mode(&decoded_inst, c.XED_MACHINE_MODE_LONG_64, c.XED_ADDRESS_WIDTH_64b);
        const decode_zone = tracy.ZoneN(@src(), "xed_decode");
        const decode_result = c.xed_decode(&decoded_inst, &inst_bytes[0], @intCast(c_uint, inst_bytes.len));
        decode_zone.End();
        switch (decode_result) {
            c.XED_ERROR_NONE => {},
            c.XED_ERROR_BUFFER_TOO_SHORT => break :asm_loop,
            else => |err| std.debug.panic("data_idx=0x{x}, xed_error: {s}\n", .{ data_idx, c.xed_error_enum_t2str(err) }),
        }

        const inst_len = c.xed_decoded_inst_get_length(&decoded_inst);
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
            for (str_bufs) |*buf, idx| _ = std.fmt.bufPrint(buf, "{x:0>2}", .{idx}) catch unreachable;
            break :blk str_bufs;
        };
        var idx: usize = 0;
        while (idx < std.math.max(10, inst_len)) : (idx += 1) {
            const byte_str = if (idx < inst_len) &byte_hex_strs[inst_bytes[idx]] else "  ";
            stream.buffer[stream.pos + 0] = byte_str[0];
            stream.buffer[stream.pos + 1] = byte_str[1];
            stream.buffer[stream.pos + 2] = ' ';
            stream.pos += 3;
        }
        fmt_zone.End();

        const buffer = stream.buffer[stream.pos..];
        const format_zone = tracy.ZoneN(@src(), "xed_format_context");
        const format_res = c.xed_format_context(c.XED_SYNTAX_INTEL, &decoded_inst, &buffer[0], @intCast(c_int, buffer.len), asm_addr, null, null);
        format_zone.End();
        std.debug.assert(format_res == 1);
        const format_str = std.mem.sliceTo(@ptrCast([*c]const u8, &buffer[0]), 0);
        stream.pos += format_str.len;
        _ = try writer.write("\n");

        text_bytes.items.len += stream.pos;
        try line_offsets.append(line_offset);
        try line_addrs.append(asm_addr);

        instructions_done += 1;
    }

    return AsmTextInfo{
        .addr_range = [2]usize{ data_start_addr, data_start_addr + data_idx },
        .data = text_bytes.toOwnedSlice(),
        .line_offsets = line_offsets.toOwnedSlice(),
        .line_addrs = line_addrs.toOwnedSlice(),
    };
}

fn showRegisters(ui: *UiContext, regs: Session.Registers) void {
    const table_regs = .{ "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "rip" };
    inline for (table_regs) |reg_name| {
        //ui.labelBoxF(reg_name ++ ": 0x{x:0>16}", .{@field(regs, reg_name)});
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

    pub fn display(self: *CallStackViewer, ui: *UiContext, call_stack: []Session.CallFrame, session: Session) !void {
        _ = session;
        for (call_stack) |frame, idx| {
            const result = try self.show_vars.getOrPut(frame);
            if (!result.found_existing) result.value_ptr.* = false;
            const is_open_ptr = result.value_ptr;

            const fn_parent = ui.pushLayoutParentF("call_stack_parent_#{}", .{idx}, [2]Size{ Size.by_children(1), Size.by_children(1) }, .x);
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
        return if (self.kind == .Directory)
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

/// higher score means better match
fn fuzzyScore(pattern: []const u8, test_str: []const u8) f32 {
    const trace = tracy.Zone(@src());
    defer trace.End();

    var score: f32 = 0;

    const to_lower_lut = comptime lut: {
        var table: [128]u8 = undefined;
        for (table) |*entry, char| {
            entry.* = if ('A' <= char and char <= 'Z') char + 32 else char;
        }
        break :lut table;
    };

    for (pattern) |pat_char, pat_idx| {
        for (test_str) |test_char, test_idx| {
            var char_score: f32 = 0;
            const case_sensitive_match = (pat_char == test_char);
            const case_insensitive_match = to_lower_lut[pat_char] == to_lower_lut[test_char];
            if (!case_insensitive_match) continue;
            char_score += 0.5;
            if (case_sensitive_match) char_score += 1;
            if (test_idx == pat_idx) char_score *= 5;
            score += char_score;
        }
    }

    if (std.mem.indexOf(u8, test_str, pattern)) |idx| score = (score * 5) - @intToFloat(f32, idx);

    return score;
}

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

            const score = fuzzyScore(self.target, test_str);
            const new_entry = Entry{ .str = test_str, .score = score, .ctx = ctx };
            if (self.slots_filled < self.slots.len) {
                self.slots[self.slots_filled] = new_entry;
                self.slots_filled += 1;
                return;
            } else {
                var worst_idx: usize = 0;
                for (self.slots) |entry, idx| {
                    if (entry.score < self.slots[worst_idx].score) worst_idx = idx;
                }
                if (@hasDecl(Ctx, "free")) self.slots[worst_idx].ctx.free(self.allocator);
                self.slots[worst_idx] = new_entry;
            }
        }

        pub fn present(
            self: *@This(),
            arena: *std.heap.ArenaAllocator,
            ui: *UiContext,
            label: []const u8,
            placement: UiContext.Placement,
            reverse_order: bool,
        ) !?Ctx {
            const trace = tracy.Zone(@src());
            defer trace.End();

            var allocator = arena.allocator();
            const filled_slots = self.slots[0..self.slots_filled];
            var clicked_option: ?usize = null;

            // sort the entries before displaying (using insertion sort)
            var sort_idx: usize = 1;
            while (sort_idx < filled_slots.len) : (sort_idx += 1) {
                var cmp_idx: usize = sort_idx;
                cmp_loop: while (cmp_idx > 0) : (cmp_idx -= 1) {
                    if (filled_slots[cmp_idx].score >= filled_slots[cmp_idx - 1].score) {
                        std.mem.swap(Entry, &filled_slots[cmp_idx], &filled_slots[cmp_idx - 1]);
                    } else break :cmp_loop;
                }
            }

            if (reverse_order) std.mem.reverse(Entry, filled_slots);

            ui.startCtxMenu(placement); // TODO: use the new ui "windows"
            defer ui.endCtxMenu();

            const children_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
            const bg_color = vec4{ 0, 0, 0, 0.85 };
            const bg_node = ui.addNode(.{ .no_id = true, .draw_background = true }, "", .{
                .bg_color = bg_color,
                .pref_size = children_size,
            });
            ui.pushParent(bg_node);
            defer ui.popParentAssert(bg_node);

            for (self.slots[0..self.slots_filled]) |entry, idx| {
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
                    .cursor_type = .hand,
                    .child_layout_axis = .x,
                });
                button_node.pref_size = [2]Size{ Size.percent(1, 1), Size.by_children(1) };
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
                extra_node.pref_size = [2]Size{ Size.text_dim(1), Size.pixels(name_size[1], 1) };
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
            ui: *UiContext,
            shader_inputs: *std.ArrayList(UiContext.ShaderInput),
            node: *UiContext.Node,
        ) error{OutOfMemory}!void {
            const trace = tracy.Zone(@src());
            defer trace.End();

            const draw_ctx = if (node.custom_draw_ctx_as_bytes) |ctx_bytes| ctx: {
                std.debug.assert(ctx_bytes.len == @sizeOf(CustomDrawMatchHighlightCtx));
                break :ctx @ptrCast(*align(1) const CustomDrawMatchHighlightCtx, ctx_bytes.ptr).*;
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
                var text_color = if (is_highlight) draw_ctx.highlight_color else node.text_color;

                const quad = font.buildQuad(char, node.font_size, &cursor) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => @panic(@errorName(err)),
                };
                var quad_rect = UiContext.Rect{ .min = quad.points[0].pos, .max = quad.points[2].pos };
                quad_rect.min += text_pos;
                quad_rect.max += text_pos;

                try shader_inputs.append(.{
                    .bottom_left_pos = quad_rect.min,
                    .top_right_pos = quad_rect.max,
                    .bottom_left_uv = quad.points[0].uv,
                    .top_right_uv = quad.points[2].uv,
                    .top_color = text_color,
                    .bottom_color = text_color,
                    .corner_roundness = 0,
                    .border_thickness = 0,
                    .clip_rect_min = node.clip_rect.min,
                    .clip_rect_max = node.clip_rect.max,
                    .which_font = if (is_highlight)
                        @enumToInt(UiContext.FontType.text_bold)
                    else
                        @enumToInt(UiContext.FontType.text),
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
    for (values) |*value| {
        value.* = try std.fmt.parseInt(u64, tokenizer.next().?, 0);
    }

    // the values we just parsed are measured in pages, not bytes
    const page_size = @intCast(u64, c.sysconf(c._SC_PAGESIZE));

    return MemoryStats{
        .virtual_size = values[0] * page_size,
        .in_ram_size = values[1] * page_size,
        .shared_size = values[2] * page_size,
    };
}
