// there's no need to manually include this file, it's already provided by UiContext.zig

const std = @import("std");
const c = @import("c.zig");
const math = @import("math.zig");
const vec2 = math.vec2;
const vec4 = math.vec4;

const UiContext = @import("UiContext.zig");
const Node = UiContext.Node;
const Signal = UiContext.Signal;
const Rect = UiContext.Rect;
const Size = UiContext.Size;
const Axis = UiContext.Axis;
const Placement = UiContext.Placement;
const text_hpadding = UiContext.text_hpadding;
const text_vpadding = UiContext.text_vpadding;

pub fn spacer(self: *UiContext, axis: Axis, size: Size) void {
    const sizes = switch (axis) {
        .x => [2]Size{ size, Size.percent(0, 0) },
        .y => [2]Size{ Size.percent(0, 0), size },
    };
    _ = self.addNode(.{ .no_id = true }, "", .{ .pref_size = sizes });
}

pub fn label(self: *UiContext, string: []const u8) void {
    const label_size = [2]Size{ Size.text_dim(1), Size.text_dim(1) };
    _ = self.addNode(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
    }, string, .{
        .pref_size = label_size,
    });
}

pub fn labelF(self: *UiContext, comptime fmt: []const u8, args: anytype) void {
    const str = std.fmt.allocPrint(self.string_arena.allocator(), fmt, args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    self.label(str);
}

pub fn textBox(self: *UiContext, string: []const u8) Signal {
    const node = self.addNode(.{
        .draw_text = true,
        .draw_border = true,
        .draw_background = true,
    }, string, .{});
    return self.getNodeSignal(node);
}

pub fn textBoxF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = std.fmt.allocPrint(self.string_arena.allocator(), fmt, args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    return self.textBox(str);
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
    return self.getNodeSignal(node);
}

pub fn buttonF(self: *UiContext, comptime fmt: []const u8, args: anytype) Signal {
    const str = std.fmt.allocPrint(self.string_arena.allocator(), fmt, args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    return self.button(str);
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
    return self.getNodeSignal(node);
}

/// pushes a new node as parent that is meant only for layout purposes
pub fn pushLayoutParent(self: *UiContext, hash_string: []const u8, size: [2]Size, layout_axis: Axis) *Node {
    const node = self.addNodeStrings(.{}, "", hash_string, .{
        .pref_size = size,
        .child_layout_axis = layout_axis,
    });
    self.pushParent(node);
    return node;
}

// don't forget to call `endCtxMenu`
// TODO: maybe turn this into a generic window function and ctx menu would simply be a special case of a window
pub fn startCtxMenu(self: *UiContext, placement: Placement) void {
    // the `addNode` function is gonna use whatever parent is at the top of the stack by default
    // so we have to trick it into thinking this is the root node
    const saved_stack_len = self.parent_stack.len();
    self.parent_stack.array_list.items.len = 0;

    const ctx_menu_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
    const ctx_menu_root = self.addNode(.{
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

    self.parent_stack.array_list.items.len = saved_stack_len;
    self.pushParent(ctx_menu_root);
}

pub fn endCtxMenu(self: *UiContext) void {
    const parent = self.popParent();
    std.debug.assert(parent == self.ctx_menu_root_node);
}

pub fn startTooltip(self: *UiContext, placement: Placement) void {
    // the `addNode` function is gonna use whatever parent is at the top of the stack by default
    // so we have to trick it into thinking this is the root node
    const saved_stack_len = self.parent_stack.len();
    self.parent_stack.array_list.items.len = 0;

    const tooltip_size = [2]Size{ Size.by_children(1), Size.by_children(1) };
    const tooltip_root = self.addNode(.{
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

    self.parent_stack.array_list.items.len = saved_stack_len;
    self.pushParent(tooltip_root);
}

pub fn endTooltip(self: *UiContext) void {
    const parent = self.popParent();
    std.debug.assert(parent == self.tooltip_root_node);
}

/// pushes itself as the parent. make sure to use popParent later
pub fn scrollableRegion(self: *UiContext, string: []const u8, axis: Axis) Signal {
    //const percent_sizes = switch (axis) {
    //    .x => [2]Size{ Size.percent(1, 0), Size.percent(1, 0) },
    //    .y => [2]Size{ Size.percent(1, 0), Size.percent(1, 0) },
    //};
    const percent_sizes = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };

    const node = self.addNode(.{
        .scrollable = true,
    }, string, .{ .pref_size = percent_sizes, .child_layout_axis = axis });
    self.pushParent(node);

    const axis_idx: usize = switch (axis) {
        .x => 0,
        .y => 1,
    };
    self.spacer(axis, Size.pixels(node.scroll_offset[axis_idx], 1));

    return self.getNodeSignal(node);
}

pub fn scrollableRegionF(self: *UiContext, comptime fmt: []const u8, args: anytype, axis: Axis) Signal {
    const str = std.fmt.allocPrint(self.string_arena.allocator(), fmt, args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    return self.scrollableRegion(str, axis);
}

pub fn scrollableText(self: *UiContext, hash_string: []const u8, string: []const u8) Signal {
    // TODO: replace this with the new mechanism used for the file tab text
    const top_node = self.addNodeF(.{ .clip_children = true, .draw_border = true, .draw_background = true }, "{s}::top_node", .{hash_string}, .{ .child_layout_axis = .x });
    self.pushParent(top_node);
    defer _ = self.popParent();
    self.label(string);
    return self.getNodeSignal(top_node);
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
    const sig = self.getNodeSignal(widget_node);

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
    const cursor_height = self.font.getScaledMetrics().line_advance - UiContext.text_vpadding;
    cursor_node.pref_size = [2]Size{ Size.pixels(1, 1), Size.pixels(cursor_height, 1) };
    const text_before_cursor = buffer[0..widget_node.cursor];
    const partial_text_rect = try self.font.textRect(text_before_cursor);
    cursor_node.rel_pos = text_node.rel_pos + vec2{ partial_text_rect.max[0], 0 } + UiContext.text_padd;

    // scroll text if it doesn't fit
    if (cursor_node.rel_pos[0] > widget_node.rect.size()[0] - UiContext.text_hpadding) {
        const overflow = cursor_node.rel_pos[0] - (widget_node.rect.size()[0] - UiContext.text_hpadding);
        text_node.rel_pos[0] -= overflow;
    }
    if (cursor_node.rel_pos[0] < UiContext.text_hpadding) {
        const overflow = UiContext.text_hpadding - cursor_node.rel_pos[0];
        text_node.rel_pos[0] += overflow;
    }

    const selection_node = self.addNode(.{
        .no_id = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "", .{});
    selection_node.bg_color = vec4{ 0, 0, 1, 0.25 };
    const text_before_mark = buffer[0..widget_node.mark];
    const partial_text_rect_mark = try self.font.textRect(text_before_mark);
    const selection_size = abs(partial_text_rect_mark.max[0] - partial_text_rect.max[0]);
    selection_node.pref_size = [2]Size{ Size.pixels(selection_size, 1), cursor_node.pref_size[1] };
    selection_node.rel_pos = vec2{
        std.math.min(partial_text_rect_mark.max[0], partial_text_rect.max[0]),
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
        widget_node.mark = if (findFirstDiff(display_str, widget_node.cursor, .left)) |idx| blk: {
            break :blk std.math.min(idx + 1, display_str.len);
        } else 0;
        widget_node.cursor = findFirstDiff(display_str, widget_node.cursor, .right) orelse
            display_str.len;
    }
    // use mouse press to select cursor position
    else if (sig.pressed or sig.held_down) {
        // find the index where the mouse is
        var idx: usize = 0;
        while (idx < buf_len.*) {
            const codepoint_len = try std.unicode.utf8ByteSequenceLength(display_str[idx]);
            const partial_text_buf = display_str[0 .. idx + codepoint_len];
            const partial_rect = try self.font.textRect(partial_text_buf);
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
                    .codepoint = @intCast(u21, codepoint),
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
        const text_op = try textOpFromAction(action, widget_node.cursor, widget_node.mark, &unicode_buf, cur_buf);

        replaceRange(buffer, buf_len, .{ .start = text_op.range.start, .end = text_op.range.end }, text_op.replace_str);
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

const TextAction = struct {
    flags: struct {
        keep_mark: bool = false,
        word_scan: bool = false,
        delete: bool = false,
        copy: bool = false,
        paste: bool = false,
    },
    delta: isize,
    codepoint: ?u21,
};

const TextOp = struct {
    range: struct { start: usize, end: usize },
    replace_str: []const u8, // contents to replace range with
    copy_str: []const u8, // contents to send to clipboard
    byte_cursor: usize, // new cursor position after applying this op
    byte_mark: usize, // new mark position after applying this op
};

// zig fmt: off
const word_seps = [_]u8{
    '`', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '=', '+',
    '[', '{', ']', '}', '\\', '|',
    ';', ':', '\'', '"',
    ' ', ',', '<', '.', '>', '/', '?',
};
// zig fmt: on

// helper function for `textInput`
// note: if we're pasting data from the clipboard (i.e. action.flags.paste == true) the `replace_str` is only valid until
// the next call to this function that uses the clipboard.
fn textOpFromAction(action: TextAction, cursor: usize, mark: usize, unicode_buf: []u8, buf: []const u8) !TextOp {
    var text_op = TextOp{
        .range = .{ .start = 0, .end = 0 },
        .replace_str = &[0]u8{},
        .copy_str = &[0]u8{},
        .byte_cursor = cursor,
        .byte_mark = mark,
    };

    const delta_sign: isize = if (action.delta > 0) 1 else -1;
    var byte_delta: isize = 0;

    // translate high level char/word delta into a buffer specific byte delta
    if (action.delta == 0) {
        _ = byte_delta; // nothing to do;
    } else if (action.delta == std.math.minInt(isize) or action.delta == std.math.maxInt(isize)) {
        byte_delta = delta_sign * @intCast(isize, buf.len);
    } else if (action.flags.word_scan) {
        // when moving the cursor to the next 'word' most programs have slightly different behaviours.
        // for example: firefox jumps over the word separator and always put the cursor on the right
        // of the separator (and jumps over multiple sep. if they're contiguous) while my terminal
        // puts the cursor on the left of the sep. and does not jump over multiple ones.
        // in my OS (using gnome) it does jump over multiple sep. but puts the cursor on the left.
        // Here's the version I'm implementing:
        // when doing a word scan the cursor should jump the next word, i.e. the next character
        // (after the end of the current word) that is a word-valid character (i.e. not a sep.)
        // (so: cursor on left/right side of sep. depending on search direction, jump over contiguous sep.)
        // In short there are two steps:
        // find the end of the current word (i.e. the next char that is a sep.)
        // find the start of the next word after that (i.e. the next char that is *not* a sep.)
        var i = cursor;
        if (buf.len != 0) {
            end_word_loop: while (i >= 0 and i <= buf.len) : (i = castWrappedAdd(i, delta_sign)) {
                if (i == buf.len and delta_sign > 0) break;
                if (i == buf.len and delta_sign < 0) continue;
                for (word_seps) |sep| {
                    if (buf[i] == sep and !(delta_sign < 0 and i == cursor)) break :end_word_loop;
                }
                if (i == 0 and delta_sign < 0) break;
            }
            start_word_loop: while (i >= 0 and i <= buf.len) : (i = castWrappedAdd(i, delta_sign)) {
                if (i == buf.len and delta_sign > 0) break;
                if (i == buf.len and delta_sign < 0) continue;
                for (word_seps) |sep| {
                    if (buf[i] == sep) continue :start_word_loop;
                }
                if (i == 0 and delta_sign < 0) break;
                if (delta_sign < 0 and i < buf.len) i += 1;
                break :start_word_loop;
            }
        }

        byte_delta = castAndSub(isize, i, cursor);
    } else {
        if (action.delta == 1 and cursor < buf.len) {
            byte_delta = try std.unicode.utf8ByteSequenceLength(buf[cursor]);
        } else if (action.delta == -1 and cursor > 0) {
            const utf8_viewer = Utf8Viewer.init(buf[0..cursor]);
            const char_size = utf8_viewer.lastCharByteSize();
            byte_delta = -@intCast(isize, char_size);
        }
    }

    if (action.codepoint) |codepoint| {
        const codepoint_len = std.unicode.utf8Encode(codepoint, unicode_buf) catch
            unreachable; // it was broken when I got here officer. it's GLFW's fault, I swear!
        text_op.replace_str = unicode_buf[0..codepoint_len];
    }

    // clipboard
    if (action.flags.copy) {
        text_op.copy_str = if (cursor >= mark) buf[mark..cursor] else buf[cursor..mark];
    }
    if (action.flags.paste) {
        const clipboard_str = c.glfwGetClipboardString(null);
        text_op.replace_str = clipboard_str[0..c.strlen(clipboard_str)];
    }

    // calculate the range we're gonna operate on
    const new_byte_cursor = @intCast(isize, text_op.byte_cursor) + byte_delta;
    const size_diff = castAndSub(isize, text_op.replace_str.len, text_op.range.end - text_op.range.start);
    const new_buf_len = castWrappedAdd(buf.len, size_diff);
    text_op.byte_cursor = @intCast(usize, std.math.clamp(new_byte_cursor, 0, new_buf_len));
    text_op.range = .{
        .start = std.math.min(text_op.byte_cursor, text_op.byte_mark),
        .end = std.math.max(text_op.byte_cursor, text_op.byte_mark),
    };
    if (text_op.replace_str.len == 0 and !action.flags.delete) text_op.range = .{ .start = 0, .end = 0 };

    // set the new cursor/mark position after the op is done
    const range_len = text_op.range.end - text_op.range.start;
    // adjust cursor if we changed the size of the buffer (deleting/inserting)
    if (text_op.replace_str.len != range_len and text_op.byte_cursor == text_op.range.end) {
        const diff = castAndSub(isize, text_op.replace_str.len, range_len);
        text_op.byte_cursor = castWrappedAdd(text_op.byte_cursor, diff);
    }
    if (!action.flags.keep_mark) text_op.byte_mark = text_op.byte_cursor;

    return text_op;
}

const SearchDir = enum { left, right };

// return the index of the first separator to the left/right of `start_idx` or
// null if we couldn't find any in that direction
fn findFirstSep(buf: []const u8, start_idx: usize, search_dir: SearchDir) ?usize {
    const delta: isize = switch (search_dir) {
        .left => -1,
        .right => 1,
    };

    // note: because all of our words separators are ASCII we can use a non-unicode aware search
    var search_idx: isize = @intCast(isize, start_idx);
    while (search_idx >= 0 and search_idx < buf.len) : (search_idx += delta) {
        const idx = @intCast(usize, search_idx);
        for (word_seps) |sep| {
            if (buf[idx] == sep) return idx;
        }
    }

    return null;
}
fn findFirstNonSep(buf: []const u8, start_idx: usize, search_dir: SearchDir) ?usize {
    const delta: isize = switch (search_dir) {
        .left => -1,
        .right => 1,
    };

    // note: because all of our words separators are ASCII we can use a non-unicode aware search
    var search_idx: isize = @intCast(isize, start_idx);
    outer_loop: while (search_idx >= 0 and search_idx < buf.len) : (search_idx += delta) {
        const idx = @intCast(usize, search_idx);
        for (word_seps) |sep| {
            if (buf[idx] == sep) continue :outer_loop;
        }
        return idx;
    }

    return null;
}

// if `start_idx` is a separator this is the same `findFirstNonSep`
// if `start_idx` is a not separator this is the same `findFirstSep`
fn findFirstDiff(buf: []const u8, start_idx: usize, search_dir: SearchDir) ?usize {
    if (buf.len == 0) return 0;
    if (start_idx == buf.len) return null;

    const start_char = buf[start_idx];
    for (word_seps) |sep| {
        if (start_char == sep) return findFirstNonSep(buf, start_idx, search_dir);
    }
    return findFirstSep(buf, start_idx, search_dir);
}

// helper function for `textInput`. `range.end` is exclusive
fn replaceRange(buffer: []u8, buf_len: *usize, range: struct { start: usize, end: usize }, new: []const u8) void {
    const range_len = range.end - range.start;

    // move contents after the range forward if we need more space
    const space_needed = if (new.len > range_len) new.len - range_len else 0;
    if (space_needed > 0) {
        var i: usize = buf_len.* + space_needed - 1;
        while (i >= range.end + space_needed) : (i -= 1) {
            buffer[i] = buffer[i - space_needed];
        }
        buf_len.* += space_needed;
    }

    // move contents after the range backward to fill empty space
    const space_shrink = if (range_len > new.len) range_len - new.len else 0;
    if (space_shrink > 0) {
        for (buffer[range.start + new.len .. buf_len.* - space_shrink]) |*byte, i| {
            byte.* = buffer[range.end + i];
        }
        buf_len.* -= space_shrink;
    }

    // copy the new contents in
    for (new) |byte, i| buffer[range.start + i] = byte;
}

// I can't believe this isn't in the std lib
fn abs(value: anytype) @TypeOf(value) {
    return if (value < 0) -value else value;
}

fn castWrappedAdd(src: anytype, diff: anytype) @TypeOf(src) {
    const SrcType = @TypeOf(src);
    const DiffType = @TypeOf(diff);
    return @intCast(SrcType, @intCast(DiffType, src) + diff);
}

fn castAndSub(comptime T: type, a: anytype, b: anytype) T {
    return @intCast(T, a) - @intCast(T, b);
}

const Utf8Viewer = struct {
    bytes: []const u8,

    pub fn init(bytes: []const u8) Utf8Viewer {
        std.debug.assert(std.unicode.utf8ValidateSlice(bytes));
        return .{ .bytes = bytes };
    }

    /// number of bytes occupied by the last character
    pub fn lastCharByteSize(self: Utf8Viewer) u3 {
        const len = self.bytes.len;
        var size = std.math.min(4, len);
        while (size > 0) : (size -= 1) {
            const slice = self.bytes[len - size .. len];
            if (std.unicode.utf8ValidateSlice(slice)) {
                const codepoint_bytes = std.unicode.utf8ByteSequenceLength(slice[0]) catch unreachable;
                if (codepoint_bytes == size) return codepoint_bytes;
            }
        }
        unreachable;
    }

    /// number of bytes occupied by character at a character index
    pub fn byteSizeAt(self: Utf8Viewer, pos: usize) usize {
        var idx: usize = 0;
        var utf8_iter = std.unicode.Utf8View.initUnchecked(self.bytes).iterator();
        while (utf8_iter.nextCodepointSlice()) |codepoint_slice| : (idx += 1) {
            if (idx == pos) return codepoint_slice.len;
        }
        unreachable;
    }

    pub fn charPosIntoBytes(self: Utf8Viewer, pos: usize) usize {
        var idx: usize = 0;
        var bytes: usize = 0;
        var utf8_iter = std.unicode.Utf8View.initUnchecked(self.bytes).iterator();
        while (utf8_iter.nextCodepointSlice()) |codepoint_slice| : (idx += 1) {
            if (idx == pos) break;
            bytes += codepoint_slice.len;
        }
        return bytes;
    }

    /// return the index of the match in codepoints, *not* bytes
    pub fn findLast(self: Utf8Viewer, to_match: u21) ?usize {
        var idx: usize = 0;
        var match_idx: ?usize = null;
        var utf8_iter = std.unicode.Utf8View.initUnchecked(self.bytes).iterator();
        while (utf8_iter.nextCodepoint()) |codepoint| : (idx += 1) {
            if (codepoint == to_match) match_idx = idx;
        }
        return match_idx;
    }

    /// convert a character index based range into the bytes range
    pub fn bytesRange(self: Utf8Viewer, start: usize, end: usize) []const u8 {
        var start_byte_idx: usize = 0;

        var idx: usize = 0;
        var bytes: usize = 0;
        var utf8_iter = std.unicode.Utf8View.initUnchecked(self.bytes).iterator();
        while (utf8_iter.nextCodepointSlice()) |codepoint_slice| : (idx += 1) {
            if (idx == start) start_byte_idx = bytes;
            if (idx == end) break;
            bytes += codepoint_slice.len;
        }

        const end_byte_idx = bytes;
        return self.bytes[start_byte_idx..end_byte_idx];
    }
};
