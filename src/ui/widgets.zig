// there's no need to manually include this file, it's already provided by UI.zig

const std = @import("std");
const c = @import("../c.zig");
const math = @import("../math.zig");
const vec2 = math.vec2;
const vec3 = math.vec3;
const vec4 = math.vec4;

const UI = @import("../UI.zig");
const Node = UI.Node;
const Signal = UI.Signal;
const Rect = UI.Rect;
const Size = UI.Size;
const Axis = UI.Axis;
const Placement = UI.Placement;
const RelativePlacement = UI.RelativePlacement;
const Icons = UI.Icons;
const text_ops = @import("text_ops.zig");
const TextAction = text_ops.TextAction;

pub fn spacer(ui: *UI, axis: Axis, size: Size) void {
    const sizes = switch (axis) {
        .x => [2]Size{ size, Size.percent(0, 0) },
        .y => [2]Size{ Size.percent(0, 0), size },
    };
    _ = ui.addNode(.{ .no_id = true }, "", .{ .pref_size = sizes });
}

pub fn label(ui: *UI, string: []const u8) void {
    _ = ui.addNode(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
    }, string, .{});
}

pub fn labelBox(ui: *UI, string: []const u8) void {
    _ = ui.addNode(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
        .draw_background = true,
        .draw_border = true,
    }, string, .{});
}

pub fn text(ui: *UI, string: []const u8) Signal {
    const node = ui.addNode(.{
        .draw_text = true,
    }, string, .{});
    return node.signal;
}

pub fn textBox(ui: *UI, string: []const u8) Signal {
    const node = ui.addNode(.{
        .draw_text = true,
        .draw_border = true,
        .draw_background = true,
    }, string, .{});
    return node.signal;
}

pub fn button(ui: *UI, string: []const u8) Signal {
    const node = ui.addNode(.{
        .clickable = true,
        .draw_text = true,
        .draw_border = true,
        .draw_background = true,
        .draw_hot_effects = true,
        .draw_active_effects = true,
    }, string, .{
        .cursor_type = .hand,
    });
    return node.signal;
}

pub fn iconButton(ui: *UI, string: []const u8) Signal {
    const node = ui.addNode(.{
        .clickable = true,
        .draw_text = true,
        .draw_border = true,
        .draw_background = true,
        .draw_hot_effects = true,
        .draw_active_effects = true,
    }, string, .{
        .cursor_type = .hand,
        .font_type = .icon,
    });
    return node.signal;
}

pub fn subtleIconButton(ui: *UI, string: []const u8) Signal {
    const node = ui.addNode(.{
        .clickable = true,
        .draw_text = true,
        .draw_active_effects = true,
    }, string, .{
        .cursor_type = .hand,
        .font_type = .icon,
    });
    return node.signal;
}

pub fn checkBox(ui: *UI, string: []const u8, value: *bool) Signal {
    const parent_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
    const layout_parent = ui.pushLayoutParentF("{s}_layout_parent", .{string}, parent_size, .x);
    layout_parent.flags.draw_background = true;
    defer ui.popParentAssert(layout_parent);

    const box_icon = if (value.*) Icons.ok else " ";
    const box_signal = ui.iconButtonF("{s}###{s}_button", .{ box_icon, string });
    if (box_signal.clicked) value.* = !value.*;

    ui.label(string);

    return box_signal;
}

/// pushes a new node as parent that is meant only for layout purposes
pub fn pushLayoutParent(
    ui: *UI,
    hash_string: []const u8,
    size: [2]Size,
    layout_axis: Axis,
) *Node {
    return ui.pushLayoutParentFlags(.{}, hash_string, size, layout_axis);
}

pub fn pushLayoutParentFlags(
    ui: *UI,
    flags: UI.Flags,
    hash_string: []const u8,
    size: [2]Size,
    layout_axis: Axis,
) *Node {
    const node = ui.addNodeStrings(flags, "", hash_string, .{
        .pref_size = size,
        .child_layout_axis = layout_axis,
    });
    ui.pushParent(node);
    return node;
}

pub fn startCtxMenu(ui: *UI, pos: ?RelativePlacement) void {
    const root = ui.addNodeAsRoot(.{
        .clip_children = true,
        .draw_border = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "INTERNAL_CTX_MENU_ROOT_NODE", .{
        .pref_size = [2]Size{ Size.by_children(1), Size.by_children(1) },
        .bg_color = vec4{ 0, 0, 0, 0.75 },
        .corner_radii = vec4{ 4, 4, 4, 4 },
    });
    if (pos) |p|
        root.rel_pos = p
    else if (root.first_frame_touched == ui.frame_idx)
        root.rel_pos = RelativePlacement.absolute(.{ .top_left = ui.mouse_pos });

    ui.pushParent(root);
    ui.ctx_menu_root_node = root;
}

pub fn endCtxMenu(ui: *UI) void {
    ui.popParentAssert(ui.ctx_menu_root_node.?);
}

pub fn startTooltip(ui: *UI, pos: ?RelativePlacement) void {
    const root = ui.addNodeAsRoot(.{
        .clip_children = true,
        .draw_border = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "INTERNAL_TOOLTIP_ROOT_NODE", .{
        .pref_size = [2]Size{ Size.by_children(1), Size.by_children(1) },
        .bg_color = vec4{ 0, 0, 0, 0.75 },
        .corner_radii = vec4{ 4, 4, 4, 4 },
        .rel_pos = pos orelse RelativePlacement.absolute(.{ .top_left = ui.mouse_pos }),
    });
    ui.pushParent(root);
    ui.tooltip_root_node = root;
}

pub fn endTooltip(ui: *UI) void {
    ui.popParentAssert(ui.tooltip_root_node.?);
}

pub fn startWindow(
    ui: *UI,
    hash_string: []const u8,
    size: [2]Size,
    pos: RelativePlacement,
) *Node {
    const window_root = ui.addNodeAsRoot(.{
        .clip_children = true,
        .draw_border = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, hash_string, .{
        .pref_size = size,
        .rel_pos = pos,
        .bg_color = vec4{ 0, 0, 0, 0.75 },
    });
    ui.pushParent(window_root);

    ui.window_roots.append(window_root) catch |e|
        ui.setErrorInfo(@errorReturnTrace(), @errorName(e));

    return window_root;
}

pub fn endWindow(ui: *UI, window_root: *Node) void {
    ui.popParentAssert(window_root);
}

/// returns the new parent (which gets pushed on the parent stack) for this region
// TODO: scrolling in x direction as well
pub fn startScrollRegion(ui: *UI, hash_string: []const u8) *Node {
    const parent = ui.addNodeF(.{
        .scroll_children_y = true,
        .clip_children = true,
    }, "###{s}:scroll_region_parent", .{hash_string}, .{
        .child_layout_axis = .y,
    });
    ui.pushParent(parent);
    return parent;
}

pub fn endScrollRegion(ui: *UI, parent: *Node, start_scroll: f32, end_scroll: f32) void {
    const hash_string = parent.hash_string;

    const bar_node = ui.addNode(.{ .draw_background = true, .no_id = true, .floating_x = true }, "", .{});
    bar_node.child_layout_axis = .y;
    bar_node.pref_size = [2]Size{ Size.by_children(1), Size.percent(1, 0) };
    bar_node.bg_color = vec4{ 0, 0, 0, 0.3 };
    bar_node.rel_pos = RelativePlacement.match(.top_right);
    {
        ui.pushParent(bar_node);
        defer std.debug.assert(ui.popParent() == bar_node);

        const up_btn = ui.subtleIconButtonF("{s}###{s}:up_scroll_btn", .{ Icons.up_open, hash_string });
        if (up_btn.held_down) parent.scroll_offset[1] += 50;

        const scroll_bar_region = ui.addNodeF(.{
            .clickable = true,
        }, "###{s}:scroll_bar_region", .{hash_string}, .{});
        scroll_bar_region.pref_size = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };
        {
            ui.pushParent(scroll_bar_region);
            defer std.debug.assert(ui.popParent() == scroll_bar_region);

            const scroll_size = end_scroll - start_scroll;
            const bar_region_size = scroll_bar_region.rect.size()[1];
            const mouse_bar_pct = (scroll_bar_region.rect.max[1] - ui.mouse_pos[1]) / bar_region_size;
            const bar_pct = std.math.clamp(mouse_bar_pct, 0, 1);

            if (scroll_bar_region.signal.held_down) {
                parent.scroll_offset[1] = (scroll_size * bar_pct) + start_scroll;
            }

            const bar_icon_node = ui.addNodeF(.{
                .draw_text = true,
                .floating_y = true,
            }, "{s}###{s}:bar_btn", .{ Icons.circle, hash_string }, .{
                .font_type = .icon,
            });
            const icon_size = bar_icon_node.text_rect.size()[1];
            bar_icon_node.rel_pos = RelativePlacement.match(.top_left);
            bar_icon_node.rel_pos.diff[1] = if (bar_region_size > 0) blk: {
                const scroll_pct = std.math.clamp((parent.scroll_offset[1] - start_scroll) / scroll_size, 0, 1);
                break :blk -std.math.clamp(
                    (bar_region_size * scroll_pct) - (icon_size / 2),
                    0,
                    bar_region_size - icon_size,
                );
            } else 0;
        }

        const down_btn = ui.subtleIconButtonF("{s}###{s}:down_scroll_btn", .{ Icons.down_open, hash_string });
        if (down_btn.held_down) parent.scroll_offset[1] -= 50;
    }

    ui.popParentAssert(parent);
}

// TODO
pub fn dropDownList(ui: *UI, hash_string: []const u8, options: []const []const u8, chosen_idx: *usize, is_open: *bool) void {
    const choice_parent_size = [2]Size{ Size.by_children(1), Size.text_dim(1) };
    const choice_parent = ui.addNodeF(.{}, "###{s}:choice_parent", .{hash_string}, .{ .pref_size = choice_parent_size, .child_layout_axis = .x });
    ui.pushParent(choice_parent);
    {
        ui.label(options[chosen_idx.*]);
        const open_btn_sig = ui.iconButton(if (is_open.*) Icons.up_open else Icons.down_open);
        if (open_btn_sig.clicked) is_open.* = !is_open.*;
    }
    std.debug.assert(ui.popParent() == choice_parent);

    if (is_open.*) {
        const opts_window = ui.startWindow("tmp_opts_window");
        defer ui.endWindow(opts_window);

        const opts_parent_size = [2]Size{ Size.pixels(choice_parent.rect.size()[0], 1), Size.by_children(1) };
        const opts_parent = ui.addNode(.{
            .clip_children = true,
            .draw_background = true,
            .floating_x = true,
            .floating_y = true,
        }, "tmp_opts_window_parent", .{
            .pref_size = opts_parent_size,
            .rel_pos = .{ .target = .top_left, .anchor = .btm_left, .diff = choice_parent.rect.min },
        });
        ui.pushParent(opts_parent);
        defer std.debug.assert(ui.popParent() == opts_parent);

        for (options, 0..) |option, idx| {
            const opt_node = ui.addNodeStringsF(.{
                .clickable = true,
                .draw_border = true,
                .draw_text = true,
                .draw_hot_effects = true,
                .draw_active_effects = true,
            }, "{s}", .{option}, "{s}:opt_node_#{}", .{ hash_string, idx }, .{});
            opt_node.pref_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) };
            if (opt_node.signal.clicked) chosen_idx.* = idx;
        }
    }
}

pub fn textInput(ui: *UI, hash_string: []const u8, buffer: []u8, buf_len: *usize) Signal {
    return textInputRaw(ui, hash_string, buffer, buf_len) catch |e| blk: {
        ui.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk std.mem.zeroes(Signal);
    };
}

pub fn textInputRaw(ui: *UI, hash_string: []const u8, buffer: []u8, buf_len: *usize) !Signal {
    // this is a really hacky way to see if this is the first time we using this node
    // (we need to initialize the cursor on first use)
    const first_time = !ui.node_table.hasKey(hash_string);

    const display_str = buffer[0..buf_len.*];
    // TODO: what is the point of writing this zero byte? the search/match code crashes when I remove it
    buffer[buf_len.*] = 0;

    // note: the node cursor/mark is in bytes into buffer

    const widget_node = ui.addNodeF(.{
        .clickable = true,
        .selectable = true,
        .clip_children = true,
        .draw_background = true,
        .draw_border = true,
    }, "###{s}", .{hash_string}, .{
        .child_layout_axis = .x,
        .cursor_type = .ibeam,
    });
    if (first_time) {
        widget_node.cursor = buf_len.*;
        widget_node.mark = buf_len.*;
    }
    const sig = widget_node.signal;

    ui.pushParent(widget_node);
    defer _ = ui.popParent();

    // make input box darker when not in focus
    if (!sig.focused) widget_node.bg_color = math.times(widget_node.bg_color, 0.85);

    const text_node = ui.addNodeStringsF(.{
        .ignore_hash_sep = true,
        .draw_text = true,
        .floating_x = true,
    }, "{s}", .{display_str}, "{s}_text_node", .{hash_string}, .{});
    text_node.text_color = vec4{ 0, 0, 0, 1 };
    if (first_time) text_node.rel_pos.diff = vec2{ 0, 0 };

    const cursor_node = ui.addNode(.{
        .no_id = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "", .{});
    cursor_node.bg_color = vec4{ 0, 0, 0, 1 };
    const font_pixel_size = ui.topStyle().font_size;
    const text_padd = ui.textPadding(text_node);
    const cursor_height = ui.font.getScaledMetrics(font_pixel_size).line_advance - text_padd[1];
    cursor_node.pref_size = [2]Size{ Size.pixels(1, 1), Size.pixels(cursor_height, 1) };
    const text_before_cursor = buffer[0..widget_node.cursor];
    const partial_text_rect = try ui.font.textRect(text_before_cursor, font_pixel_size);
    cursor_node.rel_pos.diff = text_node.rel_pos.diff + vec2{ partial_text_rect.max[0], 0 } + text_padd;

    // scroll text if it doesn't fit
    if (!first_time) {
        if (cursor_node.rel_pos.diff[0] > widget_node.rect.size()[0] - text_padd[0]) {
            const overflow = cursor_node.rel_pos.diff[0] - (widget_node.rect.size()[0] - text_padd[0]);
            text_node.rel_pos.diff[0] -= overflow;
        }
        if (cursor_node.rel_pos.diff[0] < text_padd[0]) {
            const overflow = text_padd[0] - cursor_node.rel_pos.diff[0];
            text_node.rel_pos.diff[0] += overflow;
        }
    }

    const selection_node = ui.addNode(.{
        .no_id = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "", .{});
    selection_node.bg_color = vec4{ 0, 0, 1, 0.25 };
    const text_before_mark = buffer[0..widget_node.mark];
    const partial_text_rect_mark = try ui.font.textRect(text_before_mark, font_pixel_size);
    const selection_size = @fabs(partial_text_rect_mark.max[0] - partial_text_rect.max[0]);
    selection_node.pref_size = [2]Size{ Size.pixels(selection_size, 1), cursor_node.pref_size[1] };
    selection_node.rel_pos.diff = vec2{
        @min(partial_text_rect_mark.max[0], partial_text_rect.max[0]),
        0,
    } + ui.textPadding(selection_node);

    if (!sig.focused) return sig;

    var text_actions = try std.BoundedArray(TextAction, 100).init(0);

    // triple click is the same as ctrl+a (which is the same as `Home` followed by `shift+End`)
    if (sig.triple_clicked) {
        var action = TextAction{ .flags = .{}, .delta = 0, .codepoint = null };
        action.delta = std.math.minInt(isize);
        try text_actions.append(action);
        action.flags.keep_mark = true;
        action.delta = std.math.maxInt(isize);
        try text_actions.append(action);
    }
    // double click selects the current word
    else if (sig.double_clicked) {
        widget_node.mark = if (text_ops.findFirstDiff(display_str, widget_node.cursor, .left)) |idx| blk: {
            break :blk @min(idx + 1, display_str.len);
        } else 0;
        widget_node.cursor = text_ops.findFirstDiff(display_str, widget_node.cursor, .right) orelse
            display_str.len;
    }
    // use mouse press to select cursor position
    else if (sig.pressed or sig.held_down) {
        // find the index where the mouse is
        var idx: usize = 0;
        while (idx < buf_len.*) {
            const codepoint_len = try std.unicode.utf8ByteSequenceLength(display_str[idx]);
            const partial_text_buf = display_str[0 .. idx + codepoint_len];
            const partial_rect = try ui.font.textRect(partial_text_buf, font_pixel_size);
            if (partial_rect.max[0] + text_padd[0] > sig.mouse_pos[0]) break;
            idx += codepoint_len;
        }

        if (sig.held_down) widget_node.cursor = idx;
        if (sig.pressed) widget_node.mark = idx;
    }
    // TODO: doing a click followed by press and drag in the same timing as a double-click
    // does a selection but using the same "word scan" as the double click code path

    while (ui.window_ptr.event_queue.next()) |event| {
        const has_selection = widget_node.cursor != widget_node.mark;
        var ev_was_used = true;
        switch (event) {
            .KeyDown, .KeyRepeat => |ev| {
                var action = TextAction{ .flags = .{}, .delta = 0, .codepoint = null };
                action.flags.keep_mark = ev.mods.shift;
                action.flags.word_scan = ev.mods.control;
                switch (ev.key) {
                    c.GLFW_KEY_LEFT => action.delta = -1,
                    c.GLFW_KEY_RIGHT => action.delta = 1,
                    c.GLFW_KEY_HOME => action.delta = std.math.minInt(isize),
                    c.GLFW_KEY_END => action.delta = std.math.maxInt(isize),
                    c.GLFW_KEY_DELETE => {
                        if (!has_selection) action.delta = 1;
                        action.flags.delete = true;
                    },
                    c.GLFW_KEY_BACKSPACE => {
                        if (!has_selection) action.delta = -1;
                        action.flags.delete = true;
                    },
                    c.GLFW_KEY_C => {
                        if (ev.mods.control) {
                            action.flags.copy = true;
                            action.flags.keep_mark = true;
                        } else ev_was_used = false;
                    },
                    c.GLFW_KEY_V => {
                        if (ev.mods.control) {
                            action.flags.paste = true;
                        } else ev_was_used = false;
                    },
                    c.GLFW_KEY_X => {
                        if (ev.mods.control) {
                            action.flags.copy = true;
                            action.flags.delete = true;
                        } else ev_was_used = false;
                    },
                    c.GLFW_KEY_A => {
                        if (ev.mods.control) {
                            // `ctrl+a` is the same as doing `Home` followed by `shift+End`
                            const home_action = TextAction{ .flags = .{}, .delta = std.math.minInt(isize), .codepoint = null };
                            try text_actions.append(home_action);
                            action.flags.keep_mark = true;
                            action.delta = std.math.maxInt(isize);
                        } else ev_was_used = false;
                    },
                    else => ev_was_used = false,
                }
                if (ev_was_used) try text_actions.append(action);
            },
            .Char => |codepoint| {
                const add_codepoint_action = TextAction{
                    .flags = .{},
                    .delta = 0,
                    .codepoint = @as(u21, @intCast(codepoint)),
                };
                try text_actions.append(add_codepoint_action);
            },
            else => ev_was_used = false,
        }
        if (ev_was_used) ui.window_ptr.event_queue.removeCurrent();
    }

    for (text_actions.slice()) |action| {
        var unicode_buf: [4]u8 = undefined;
        const cur_buf = buffer[0..buf_len.*];
        const text_op = try text_ops.textOpFromAction(action, widget_node.cursor, widget_node.mark, &unicode_buf, cur_buf);

        text_ops.replaceRange(buffer, buf_len, .{ .start = text_op.range.start, .end = text_op.range.end }, text_op.replace_str);
        if (text_op.copy_str.len > 0) {
            const c_str = try ui.allocator.dupeZ(u8, text_op.copy_str);
            defer ui.allocator.free(c_str);
            c.glfwSetClipboardString(null, c_str);
        }
        widget_node.cursor = text_op.byte_cursor;
        widget_node.mark = text_op.byte_mark;
    }

    return sig;
}

pub fn colorPicker(ui: *UI, color: *vec4) void {
    const square_size = 250;

    var hsv = RGBtoHSV(color.*);

    const background_node = ui.addNode(.{
        .draw_border = true,
        .draw_background = true,
        .no_id = true,
    }, "", .{
        .pref_size = [2]Size{ Size.by_children(1), Size.by_children(1) },
        .child_layout_axis = .x,
    });
    ui.pushParent(background_node);
    defer ui.popParentAssert(background_node);
    // this padding is so scuffed; TODO: maybe introduce some new `Size` for this type of deal
    ui.spacer(.x, Size.pixels(5, 1));
    defer ui.spacer(.x, Size.pixels(5, 1));
    _ = ui.pushLayoutParent(
        "aaaaaaaaaaaaaaaaa",
        [2]Size{ Size.by_children(1), Size.by_children(1) },
        .y,
    );
    defer _ = ui.popParent();
    ui.spacer(.y, Size.pixels(5, 1));
    defer ui.spacer(.y, Size.pixels(4, 1)); // TODO: why won't 5 work here? maybe there's smth wrong w/ spacers?
    {
        const color_part_parent = ui.pushLayoutParent(
            "color_picker_part",
            [2]Size{ Size.by_children(1), Size.by_children(1) },
            .x,
        );
        defer ui.popParentAssert(color_part_parent);

        const color_square = ui.addNode(.{ .clickable = true }, "color square", .{
            .pref_size = Size.pixelsExact(square_size, square_size),
            .custom_draw_fn = (struct {
                pub fn draw(_: *UI, shader_inputs: *std.ArrayList(UI.ShaderInput), node: *UI.Node) error{OutOfMemory}!void {
                    const hue = @as(*align(1) const vec4, @ptrCast(node.custom_draw_ctx_as_bytes.?.ptr)).*;
                    const hue_color = HSVtoRGB(vec4{ hue[0], 1, 1, 1 });
                    var rect = UI.ShaderInput.fromNode(node);
                    rect.edge_softness = 0;
                    rect.border_thickness = 0;
                    rect.top_left_color = vec4{ 1, 1, 1, 1 };
                    rect.btm_left_color = vec4{ 1, 1, 1, 1 };
                    rect.top_right_color = hue_color;
                    rect.btm_right_color = hue_color;
                    try shader_inputs.append(rect);
                    rect.top_left_color = vec4{ 0, 0, 0, 0 };
                    rect.btm_left_color = vec4{ 0, 0, 0, 1 };
                    rect.top_right_color = vec4{ 0, 0, 0, 0 };
                    rect.btm_right_color = vec4{ 0, 0, 0, 1 };
                    try shader_inputs.append(rect);
                    { // circle cursor
                        const center = node.rect.min + vec2{ hue[1], hue[2] } * node.rect.size();
                        const radius: f32 = 10;
                        const radius_vec = math.splat(vec2, radius);
                        rect.top_left_color = math.splat(vec4, 1);
                        rect.btm_left_color = math.splat(vec4, 1);
                        rect.top_right_color = math.splat(vec4, 1);
                        rect.btm_right_color = math.splat(vec4, 1);
                        rect.btm_left_pos = center - radius_vec;
                        rect.top_right_pos = center + radius_vec;
                        rect.corner_radii = [4]f32{ radius, radius, radius, radius };
                        rect.edge_softness = 1;
                        rect.border_thickness = 2;
                        try shader_inputs.append(rect);
                    }
                }
            }).draw,
            .custom_draw_ctx_as_bytes = std.mem.asBytes(&hsv),
        });
        if (color_square.signal.held_down) {
            const norm = color_square.signal.mouse_pos / color_square.rect.size();
            hsv[1] = std.math.clamp(norm[0], 0, 1);
            hsv[2] = std.math.clamp(norm[1], 0, 1);
        }

        ui.spacer(.x, Size.pixels(3, 1));

        const hue_bar = ui.addNode(.{ .clickable = true, .draw_background = true }, "hue_bar", .{
            .pref_size = Size.pixelsExact(square_size / 10, square_size),
            .custom_draw_fn = (struct {
                pub fn draw(_: *UI, shader_inputs: *std.ArrayList(UI.ShaderInput), node: *UI.Node) error{OutOfMemory}!void {
                    var rect = UI.ShaderInput.fromNode(node);
                    rect.edge_softness = 0;
                    rect.border_thickness = 0;
                    const hue_colors = [_]vec4{
                        vec4{ 1, 0, 0, 1 },
                        vec4{ 1, 1, 0, 1 },
                        vec4{ 0, 1, 0, 1 },
                        vec4{ 0, 1, 1, 1 },
                        vec4{ 0, 0, 1, 1 },
                        vec4{ 1, 0, 1, 1 },
                    };
                    const segment_height = node.rect.size()[1] / hue_colors.len;
                    rect.btm_left_pos[1] = rect.top_right_pos[1] - segment_height;
                    for (hue_colors, 0..) |rect_color, idx| {
                        const next_color = hue_colors[(idx + 1) % hue_colors.len];
                        rect.top_left_color = rect_color;
                        rect.btm_left_color = next_color;
                        rect.top_right_color = rect_color;
                        rect.btm_right_color = next_color;
                        try shader_inputs.append(rect);
                        rect.top_right_pos[1] = rect.btm_left_pos[1];
                        rect.btm_left_pos[1] = rect.top_right_pos[1] - segment_height;
                    }

                    inline for (@typeInfo(@TypeOf(rect)).Struct.fields) |field| {
                        if (comptime std.mem.endsWith(u8, field.name, "color"))
                            @field(rect, field.name) = math.splat(vec4, 1);
                    }
                    rect.edge_softness = 1;
                    rect.border_thickness = 2;
                    rect.corner_radii = [4]f32{ 2, 2, 2, 2 };
                    const hsv0 = @as(*align(1) const f32, @ptrCast(node.custom_draw_ctx_as_bytes)).*;
                    const bar_size: f32 = 10;
                    const center_y = blk: {
                        var center = node.rect.max[1] - node.rect.size()[1] * hsv0;
                        break :blk std.math.clamp(
                            center,
                            node.rect.min[1] + bar_size / 2,
                            node.rect.max[1] - bar_size / 2,
                        );
                    };
                    rect.btm_left_pos[1] = center_y - bar_size / 2;
                    rect.top_right_pos[1] = center_y + bar_size / 2;
                    try shader_inputs.append(rect);
                }
            }).draw,
            .custom_draw_ctx_as_bytes = std.mem.asBytes(&hsv[0]),
        });
        if (hue_bar.signal.held_down) {
            const norm = hue_bar.signal.mouse_pos / hue_bar.rect.size();
            hsv[0] = std.math.clamp(1 - norm[1], 0, 1);
        }
    }

    // TODO: add a way to edit the alpha

    color.* = HSVtoRGB(hsv);

    ui.spacer(.y, Size.pixels(3, 1));

    // TODO: the buffers are not working correctly
    // {
    //     const value_part_parent = ui.pushLayoutParent(
    //         "color_picker_values",
    //         [2]Size{ Size.percent(1, 1), Size.by_children(1) },
    //         .x,
    //     );
    //     defer ui.popParentAssert(value_part_parent);

    //     const Buf = Buffer(5);
    //     const color_letters = [_][]const u8{ "R", "G", "B", "A" };
    //     var buffers = [_]Buf{Buf{}} ** color_letters.len;
    //     for (&buffers, color_letters, 0..) |*buf, letter, idx| {
    //         const comp = &color[idx];
    //         ui.label(letter);
    //         buf.len = (std.fmt.bufPrint(&buf.buffer, "{d:1.2}", .{comp.*}) catch unreachable).len;
    //         ui.pushTmpStyle(.{ .pref_size = [2]Size{ Size.percent(1, 0), Size.text_dim(1) } });
    //         _ = ui.textInput(letter, &buf.buffer, &buf.len);
    //     }
    // }
}

fn RGBtoHSV(rgba: vec4) vec4 {
    const r = rgba[0];
    const g = rgba[1];
    const b = rgba[2];
    const x_max = @max(r, g, b);
    const x_min = @min(r, g, b);
    const V = x_max;
    const C = x_max - x_min;
    const H = if (C == 0)
        0
    else if (V == r)
        60 * @mod((g - b) / C, 6)
    else if (V == g)
        60 * (((b - r) / C) + 2)
    else if (V == b)
        60 * (((r - g) / C) + 4)
    else
        unreachable;
    const S_V = if (V == 0) 0 else C / V;

    return vec4{
        H / 360,
        S_V,
        V,
        rgba[3],
    };
}

fn HSVtoRGB(hsva: vec4) vec4 {
    const h = (hsva[0] * 360) / 60;
    const C = hsva[2] * hsva[1];
    const X = C * (1 - @fabs(@mod(h, 2) - 1));
    const rgb_l = switch (@as(u32, @intFromFloat(@floor(h)))) {
        0 => vec3{ C, X, 0 },
        1 => vec3{ X, C, 0 },
        2 => vec3{ 0, C, X },
        3 => vec3{ 0, X, C },
        4 => vec3{ X, 0, C },
        else => vec3{ C, 0, X },
    };
    const m = hsva[2] - C;
    return vec4{
        rgb_l[0] + m,
        rgb_l[1] + m,
        rgb_l[2] + m,
        hsva[3],
    };
}

pub fn labelF(ui: *UI, comptime fmt: []const u8, args: anytype) void {
    const str = ui.fmtTmpString(fmt, args);
    ui.label(str);
}

pub fn labelBoxF(ui: *UI, comptime fmt: []const u8, args: anytype) void {
    const str = ui.fmtTmpString(fmt, args);
    ui.labelBox(str);
}

pub fn textF(ui: *UI, comptime fmt: []const u8, args: anytype) Signal {
    const str = ui.fmtTmpString(fmt, args);
    return ui.text(str);
}

pub fn textBoxF(ui: *UI, comptime fmt: []const u8, args: anytype) Signal {
    const str = ui.fmtTmpString(fmt, args);
    return ui.textBox(str);
}

pub fn buttonF(ui: *UI, comptime fmt: []const u8, args: anytype) Signal {
    const str = ui.fmtTmpString(fmt, args);
    return ui.button(str);
}

pub fn iconButtonF(ui: *UI, comptime fmt: []const u8, args: anytype) Signal {
    const str = ui.fmtTmpString(fmt, args);
    return ui.iconButton(str);
}

pub fn subtleIconButtonF(ui: *UI, comptime fmt: []const u8, args: anytype) Signal {
    const str = ui.fmtTmpString(fmt, args);
    return ui.subtleIconButton(str);
}

pub fn checkBoxF(ui: *UI, comptime fmt: []const u8, args: anytype, value: *bool) Signal {
    const str = ui.fmtTmpString(fmt, args);
    return ui.checkBox(str, value);
}

pub fn pushLayoutParentF(ui: *UI, comptime fmt: []const u8, args: anytype, size: [2]Size, layout_axis: Axis) *Node {
    const str = ui.fmtTmpString(fmt, args);
    return ui.pushLayoutParent(str, size, layout_axis);
}

pub fn pushLayoutParentFlagsF(ui: *UI, flags: UI.Flags, comptime fmt: []const u8, args: anytype, size: [2]Size, layout_axis: Axis) *Node {
    const str = ui.fmtTmpString(fmt, args);
    return ui.pushLayoutParentFlags(flags, str, size, layout_axis);
}

pub fn fmtTmpString(ui: *UI, comptime fmt: []const u8, args: anytype) []const u8 {
    return std.fmt.allocPrint(ui.build_arena.allocator(), fmt, args) catch |e| {
        ui.setErrorInfo(@errorReturnTrace(), @errorName(e));
        return "";
    };
}
