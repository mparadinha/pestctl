// there's no need to manually include this file, it's already provided by UiContext.zig

const std = @import("std");
const c = @import("../c.zig");
const math = @import("../math.zig");
const vec2 = math.vec2;
const vec4 = math.vec4;

const UiContext = @import("../UiContext.zig");
const Node = UiContext.Node;
const Signal = UiContext.Signal;
const Rect = UiContext.Rect;
const Size = UiContext.Size;
const Axis = UiContext.Axis;
const Placement = UiContext.Placement;
const Icons = UiContext.Icons;
const text_hpadding = UiContext.text_hpadding;
const text_vpadding = UiContext.text_vpadding;
const text_ops = @import("text_ops.zig");
const TextAction = text_ops.TextAction;

pub fn spacer(self: *UiContext, axis: Axis, size: Size) void {
    const sizes = switch (axis) {
        .x => [2]Size{ size, Size.percent(0, 0) },
        .y => [2]Size{ Size.percent(0, 0), size },
    };
    _ = self.addNode(.{ .no_id = true }, "", .{ .pref_size = sizes });
}

pub fn label(self: *UiContext, string: []const u8) void {
    _ = self.addNode(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
    }, string, .{});
}

pub fn labelBox(self: *UiContext, string: []const u8) void {
    _ = self.addNode(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
        .draw_background = true,
        .draw_border = true,
    }, string, .{});
}

pub fn text(self: *UiContext, string: []const u8) Signal {
    const node = self.addNode(.{
        .draw_text = true,
    }, string, .{});
    return node.signal;
}

pub fn textBox(self: *UiContext, string: []const u8) Signal {
    const node = self.addNode(.{
        .draw_text = true,
        .draw_border = true,
        .draw_background = true,
    }, string, .{});
    return node.signal;
}

pub fn button(self: *UiContext, string: []const u8) Signal {
    const node = self.addNode(.{
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

pub fn iconButton(self: *UiContext, string: []const u8) Signal {
    self.pushTmpStyle(.{ .font_type = .icon });
    return self.button(string);
}

pub fn subtleIconButton(self: *UiContext, string: []const u8) Signal {
    const node = self.addNode(.{
        .clickable = true,
        .draw_text = true,
        .draw_active_effects = true,
    }, string, .{
        .cursor_type = .hand,
        .font_type = .icon,
    });
    return node.signal;
}

pub fn checkBox(self: *UiContext, string: []const u8, value: *bool) Signal {
    const parent_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
    const layout_parent = self.pushLayoutParentF("{s}_layout_parent", .{string}, parent_size, .x);
    layout_parent.flags.draw_background = true;
    defer self.popParentAssert(layout_parent);

    const box_icon = if (value.*) Icons.ok else " ";
    const box_signal = self.iconButtonF("{s}###{s}_button", .{ box_icon, string });
    if (box_signal.clicked) value.* = !value.*;

    self.label(string);

    return box_signal;
}

/// pushes a new node as parent that is meant only for layout purposes
pub fn pushLayoutParent(
    self: *UiContext,
    hash_string: []const u8,
    size: [2]Size,
    layout_axis: Axis,
) *Node {
    return self.pushLayoutParentFlags(.{}, hash_string, size, layout_axis);
}

pub fn pushLayoutParentFlags(
    self: *UiContext,
    flags: UiContext.Flags,
    hash_string: []const u8,
    size: [2]Size,
    layout_axis: Axis,
) *Node {
    const node = self.addNodeStrings(flags, "", hash_string, .{
        .pref_size = size,
        .child_layout_axis = layout_axis,
    });
    self.pushParent(node);
    return node;
}

// don't forget to call `endCtxMenu`
// TODO: maybe turn this into a generic window function and ctx menu would simply be a special case of a window
pub fn startCtxMenu(self: *UiContext, placement: Placement) void {
    const ctx_menu_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
    const ctx_menu_root = self.addNodeAsRoot(.{
        .clip_children = true,
        .floating_x = true,
        .floating_y = true,
        .selectable = true,
    }, "###INTERNAL_CTX_MENU_ROOT_NODE", .{
        .pref_size = ctx_menu_size,
        .rel_pos = placement.value(),
        .rel_pos_placement = std.meta.activeTag(placement),
    });
    self.ctx_menu_root_node = ctx_menu_root;
}

pub fn endCtxMenu(self: *UiContext) void {
    const parent = self.popParent();
    std.debug.assert(parent == self.ctx_menu_root_node);
}

pub fn startTooltip(self: *UiContext, placement: Placement) void {
    const tooltip_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
    const tooltip_root = self.addNodeAsRoot(.{
        .clip_children = true,
        .floating_x = true,
        .floating_y = true,
        .selectable = true,
    }, "###INTERNAL_TOOLTIP_ROOT_NODE", .{
        .pref_size = tooltip_size,
        .rel_pos = placement.value(),
        .rel_pos_placement = std.meta.activeTag(placement),
    });
    self.tooltip_root_node = tooltip_root;
}

pub fn endTooltip(self: *UiContext) void {
    const parent = self.popParent();
    std.debug.assert(parent == self.tooltip_root_node);
}

pub fn startWindow(self: *UiContext, hash_string: []const u8) *Node {
    const whole_screen_size = [2]Size{ Size.pixels(self.screen_size[0], 1), Size.pixels(self.screen_size[1], 1) };
    const node = self.addNodeAsRoot(.{
        .clip_children = true,
        .no_id = true,
    }, hash_string, .{
        .pref_size = whole_screen_size,
    });

    self.window_roots.append(node) catch |e| {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
    };

    return node;
}

pub fn endWindow(self: *UiContext, window_root: *Node) void {
    std.debug.assert(self.popParent() == window_root);
}

/// returns the new parent (which gets pushed on the parent stack) for this region
pub fn startScrollRegion(self: *UiContext, hash_string: []const u8) *Node {
    const parent = self.addNodeF(.{
        .scrollable = true,
        .clip_children = true,
    }, "###{s}:scroll_region_parent", .{hash_string}, .{ .child_layout_axis = .y });
    self.pushParent(parent);
    return parent;
}

pub fn endScrollRegion(self: *UiContext, parent: *Node, start_scroll: f32, end_scroll: f32) void {
    const hash_string = parent.hash_string;

    const bar_node = self.addNode(.{ .draw_background = true, .no_id = true, .floating_x = true }, "", .{});
    bar_node.child_layout_axis = .y;
    bar_node.pref_size = [2]Size{ Size.by_children(1), Size.percent(1, 0) };
    bar_node.bg_color = vec4{ 0, 0, 0, 0.3 };
    bar_node.rel_pos_placement = .top_right;
    bar_node.rel_pos_placement_parent = .top_right;
    {
        self.pushParent(bar_node);
        defer std.debug.assert(self.popParent() == bar_node);

        const up_btn = self.subtleIconButtonF("{s}###{s}:up_scroll_btn", .{ Icons.up_open, hash_string });
        if (up_btn.held_down) parent.scroll_offset[1] += 50;

        const scroll_bar_region = self.addNodeF(.{
            .clickable = true,
        }, "###{s}:scroll_bar_region", .{hash_string}, .{});
        scroll_bar_region.pref_size = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };
        {
            self.pushParent(scroll_bar_region);
            defer std.debug.assert(self.popParent() == scroll_bar_region);

            const scroll_size = end_scroll - start_scroll;
            const bar_region_size = scroll_bar_region.rect.size()[1];
            const mouse_bar_pct = (scroll_bar_region.rect.max[1] - self.mouse_pos[1]) / bar_region_size;
            const bar_pct = std.math.clamp(mouse_bar_pct, 0, 1);

            if (scroll_bar_region.signal.held_down) {
                parent.scroll_offset[1] = (scroll_size * bar_pct) + start_scroll;
            }

            const bar_icon_node = self.addNodeF(.{
                .draw_text = true,
                .floating_y = true,
            }, "{s}###{s}:bar_btn", .{ Icons.circle, hash_string }, .{
                .font_type = .icon,
            });
            const icon_size = bar_icon_node.text_rect.size()[1];
            bar_icon_node.rel_pos_placement = .top_left;
            bar_icon_node.rel_pos_placement_parent = .top_left;
            bar_icon_node.rel_pos[1] = if (bar_region_size > 0) blk: {
                const scroll_pct = std.math.clamp((parent.scroll_offset[1] - start_scroll) / scroll_size, 0, 1);
                break :blk -std.math.clamp(
                    (bar_region_size * scroll_pct) - (icon_size / 2),
                    0,
                    bar_region_size - icon_size,
                );
            } else 0;
        }

        const down_btn = self.subtleIconButtonF("{s}###{s}:down_scroll_btn", .{ Icons.down_open, hash_string });
        if (down_btn.held_down) parent.scroll_offset[1] -= 50;
    }

    std.debug.assert(self.popParent() == parent);
}

// TODO
pub fn dropDownList(self: *UiContext, hash_string: []const u8, options: []const []const u8, chosen_idx: *usize, is_open: *bool) void {
    const choice_parent_size = [2]Size{ Size.by_children(1), Size.text_dim(1) };
    const choice_parent = self.addNodeF(.{}, "###{s}:choice_parent", .{hash_string}, .{ .pref_size = choice_parent_size, .child_layout_axis = .x });
    self.pushParent(choice_parent);
    {
        self.label(options[chosen_idx.*]);
        const open_btn_sig = self.iconButton(if (is_open.*) Icons.up_open else Icons.down_open);
        if (open_btn_sig.clicked) is_open.* = !is_open.*;
    }
    std.debug.assert(self.popParent() == choice_parent);

    if (is_open.*) {
        const opts_window = self.startWindow("tmp_opts_window");
        defer self.endWindow(opts_window);

        const opts_parent_size = [2]Size{ Size.pixels(choice_parent.rect.size()[0], 1), Size.by_children(1) };
        const opts_parent = self.addNode(.{
            .clip_children = true,
            .draw_background = true,
            .floating_x = true,
            .floating_y = true,
        }, "tmp_opts_window_parent", .{
            .pref_size = opts_parent_size,
            .rel_pos = choice_parent.rect.min,
            .rel_pos_placement = .top_left,
        });
        self.pushParent(opts_parent);
        defer std.debug.assert(self.popParent() == opts_parent);

        for (options, 0..) |option, idx| {
            const opt_node = self.addNodeStringsF(.{
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

pub fn textInput(self: *UiContext, hash_string: []const u8, buffer: []u8, buf_len: *usize) Signal {
    return textInputRaw(self, hash_string, buffer, buf_len) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk std.mem.zeroes(Signal);
    };
}

pub fn textInputRaw(self: *UiContext, hash_string: []const u8, buffer: []u8, buf_len: *usize) !Signal {
    // this is a really hacky way to see if this is the first time we using this node
    // (we need to initialize the cursor on first use)
    const first_time = !self.node_table.hasKey(hash_string);

    const display_str = buffer[0..buf_len.*];
    buffer[buf_len.*] = 0;

    // note: the node cursor/mark is in bytes into buffer

    const widget_node = self.addNodeF(.{
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

    self.pushParent(widget_node);
    defer _ = self.popParent();

    // make input box darker when not in focus
    if (!sig.focused) widget_node.bg_color = math.times(widget_node.bg_color, 0.85);

    const text_node = self.addNodeStringsF(.{
        .ignore_hash_sep = true,
        .draw_text = true,
        .floating_x = true,
    }, "{s}", .{display_str}, "{s}_text_node", .{hash_string}, .{});
    text_node.text_color = vec4{ 0, 0, 0, 1 };
    if (first_time) text_node.rel_pos = vec2{ 0, 0 };

    const cursor_node = self.addNode(.{
        .no_id = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "", .{});
    cursor_node.bg_color = vec4{ 0, 0, 0, 1 };
    const font_pixel_size = self.topStyle().font_size;
    const cursor_height = self.font.getScaledMetrics(font_pixel_size).line_advance - UiContext.text_vpadding;
    cursor_node.pref_size = [2]Size{ Size.pixels(1, 1), Size.pixels(cursor_height, 1) };
    const text_before_cursor = buffer[0..widget_node.cursor];
    const partial_text_rect = try self.font.textRect(text_before_cursor, font_pixel_size);
    cursor_node.rel_pos = text_node.rel_pos + vec2{ partial_text_rect.max[0], 0 } + UiContext.text_padd;

    // scroll text if it doesn't fit
    if (!first_time) {
        if (cursor_node.rel_pos[0] > widget_node.rect.size()[0] - UiContext.text_hpadding) {
            const overflow = cursor_node.rel_pos[0] - (widget_node.rect.size()[0] - UiContext.text_hpadding);
            text_node.rel_pos[0] -= overflow;
        }
        if (cursor_node.rel_pos[0] < UiContext.text_hpadding) {
            const overflow = UiContext.text_hpadding - cursor_node.rel_pos[0];
            text_node.rel_pos[0] += overflow;
        }
    }

    const selection_node = self.addNode(.{
        .no_id = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "", .{});
    selection_node.bg_color = vec4{ 0, 0, 1, 0.25 };
    const text_before_mark = buffer[0..widget_node.mark];
    const partial_text_rect_mark = try self.font.textRect(text_before_mark, font_pixel_size);
    const selection_size = @fabs(partial_text_rect_mark.max[0] - partial_text_rect.max[0]);
    selection_node.pref_size = [2]Size{ Size.pixels(selection_size, 1), cursor_node.pref_size[1] };
    selection_node.rel_pos = vec2{
        @min(partial_text_rect_mark.max[0], partial_text_rect.max[0]),
        0,
    } + UiContext.text_padd;

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
            const partial_rect = try self.font.textRect(partial_text_buf, font_pixel_size);
            if (partial_rect.max[0] + UiContext.text_hpadding > sig.mouse_pos[0]) break;
            idx += codepoint_len;
        }

        if (sig.held_down) widget_node.cursor = idx;
        if (sig.pressed) widget_node.mark = idx;
    }
    // TODO: doing a click followed by press and drag in the same timing as a double-click
    // does a selection but using the same "word scan" as the double click code path

    while (self.window_ptr.event_queue.next()) |event| {
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
        if (ev_was_used) self.window_ptr.event_queue.removeCurrent();
    }

    for (text_actions.slice()) |action| {
        var unicode_buf: [4]u8 = undefined;
        const cur_buf = buffer[0..buf_len.*];
        const text_op = try text_ops.textOpFromAction(action, widget_node.cursor, widget_node.mark, &unicode_buf, cur_buf);

        text_ops.replaceRange(buffer, buf_len, .{ .start = text_op.range.start, .end = text_op.range.end }, text_op.replace_str);
        if (text_op.copy_str.len > 0) {
            const c_str = try self.allocator.dupeZ(u8, text_op.copy_str);
            defer self.allocator.free(c_str);
            c.glfwSetClipboardString(null, c_str);
        }
        widget_node.cursor = text_op.byte_cursor;
        widget_node.mark = text_op.byte_mark;
    }

    return sig;
}

pub fn labelF(self: *UiContext, comptime fmt: []const u8, args: anytype) void {
    const str = self.fmtTmpString(fmt, args);
    self.label(str);
}

pub fn labelBoxF(self: *UiContext, comptime fmt: []const u8, args: anytype) void {
    const str = self.fmtTmpString(fmt, args);
    self.labelBox(str);
}

pub fn textF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = self.fmtTmpString(fmt, args);
    return self.text(str);
}

pub fn textBoxF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = self.fmtTmpString(fmt, args);
    return self.textBox(str);
}

pub fn buttonF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = self.fmtTmpString(fmt, args);
    return self.button(str);
}

pub fn iconButtonF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = self.fmtTmpString(fmt, args);
    return self.iconButton(str);
}

pub fn subtleIconButtonF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = self.fmtTmpString(fmt, args);
    return self.subtleIconButton(str);
}

pub fn checkBoxF(self: *UiContext, comptime fmt: []const u8, args: anytype, value: *bool) Signal {
    const str = self.fmtTmpString(fmt, args);
    return self.checkBox(str, value);
}

pub fn pushLayoutParentF(self: *UiContext, comptime fmt: []const u8, args: anytype, size: [2]Size, layout_axis: Axis) *Node {
    const str = self.fmtTmpString(fmt, args);
    return self.pushLayoutParent(str, size, layout_axis);
}

pub fn pushLayoutParentFlagsF(self: *UiContext, flags: UiContext.Flags, comptime fmt: []const u8, args: anytype, size: [2]Size, layout_axis: Axis) *Node {
    const str = self.fmtTmpString(fmt, args);
    return self.pushLayoutParentFlags(flags, str, size, layout_axis);
}

pub fn fmtTmpString(self: *UiContext, comptime fmt: []const u8, args: anytype) []const u8 {
    return std.fmt.allocPrint(self.build_arena.allocator(), fmt, args) catch |e| {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        return "";
    };
}
