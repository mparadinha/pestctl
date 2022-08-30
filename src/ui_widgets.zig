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
        .hover_cursor = .hand,
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

/// pushes itself as the parent. make sure to use popParent later
pub fn scrollableRegion(self: *UiContext, string: []const u8, axis: Axis) Signal {
    //const percent_sizes = switch (axis) {
    //    .x => [2]Size{ Size.percent(1, 0), Size.percent(1, 0) },
    //    .y => [2]Size{ Size.percent(1, 0), Size.percent(1, 0) },
    //};
    const percent_sizes = [2]Size{ Size.percent(1, 0), Size.percent(1, 0) };

    const node = self.addNode(.{
        .clickable = true,
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
    // * parent widget (layout in x)
    // | * scrollable in y (x size percent(1, 0))
    // | | * scrollable in x (y size percent(1, 0))
    // | | | * text
    // | | | * x scrollbar parent (layout in x) (x size percent(1, 0), y size by_children(1))
    // | | | | * spacer(x, pixels(scroll_value, 1))
    // | | | | * scroolbar grabber thingy
    // | | | | * spacer(x, percent(1, 0))
    // | | * y scrollbar parent (layout in y) (x size bychildren(1), y size percent(1, 0))
    // | | | * spacer(y, pixels(scroll_value, 1))
    // | | | * scroolbar grabber thingy
    // | | | * spacer(y, percent(1, 0))

    const top_node = self.addNodeF(.{ .clip_children = true, .draw_border = true, .draw_background = true }, "{s}::top_node", .{hash_string}, .{ .child_layout_axis = .x });
    self.pushParent(top_node);
    defer _ = self.popParent();

    const scrollable_y_sig = self.scrollableRegionF("{s}::scrollable_y", .{hash_string}, .y);

    const scrollable_x_sig = self.scrollableRegionF("{s}::scrollable_x", .{hash_string}, .x);

    const text_node = self.addNode(.{ .draw_text = true }, string, .{});
    _ = text_node;

    _ = scrollable_x_sig;
    //const x_scrollbar = self.addNode(.{ .no_id = true }, "", .{ .child_layout_axis = .x });
    //x_scrollbar.pref_size = [2]Size{ Size.percent(1, 0), Size.by_children(1) };
    //self.pushParent(x_scrollbar);
    //{
    //     self.spacer(.x, Size.pixels(scrollable_x_sig.scroll_offset[0], 1));
    //    const x_scrollbar_grabber = self.addNodeF(.{ .clickable = true, .draw_background = true }, "{s}::x_grabber", .{hash_string}, .{});
    //    x_scrollbar_grabber.pref_size = [2]Size{ Size.pixels(10, 1), Size.pixels(10, 1) };
    //     self.spacer(.x, Size.percent(1, 0));
    //}
    //_ = self.popParent(); // pop x_scrollbar

    _ = self.popParent(); // pop scrollable region x

    const y_scrollbar = self.addNode(.{ .no_id = true }, "", .{ .child_layout_axis = .y });
    y_scrollbar.pref_size = [2]Size{ Size.by_children(1), Size.percent(1, 0) };
    self.pushParent(y_scrollbar);
    {
        self.spacer(.y, Size.pixels(scrollable_y_sig.scroll_offset[1], 1));
        const y_scrollbar_grabber = self.addNodeF(.{ .clickable = true, .draw_background = true }, "{s}::y_grabber", .{hash_string}, .{});
        y_scrollbar_grabber.pref_size = [2]Size{ Size.pixels(10, 1), Size.pixels(10, 1) };
        self.spacer(.y, Size.percent(1, 0));
    }
    _ = self.popParent(); // pop y_scrollbar

    _ = self.popParent(); // pop scrollable region y

    return self.getNodeSignal(top_node);
}

pub fn textInput(self: *UiContext, hash_string: []const u8, buffer: []u8, buf_len: *usize) Signal {
    // * widget node (layout in x)
    // | * text node (layout in x)
    // | | * cursor node

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
        .hover_cursor = .ibeam,
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

    const text_node = self.addNode(.{
        .no_id = true,
        .ignore_hash_sep = true,
        .draw_text = true,
        .floating_x = true,
    }, display_str, .{});
    text_node.text_color = vec4{ 0, 0, 0, 1 };
    text_node.rel_pos = vec2{ 0, 0 };

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
    const partial_text_rect = self.font.textRect(text_before_cursor) catch unreachable;
    cursor_node.rel_pos = vec2{ partial_text_rect.max[0], 0 } + UiContext.text_padd;

    const selection_node = self.addNode(.{
        .no_id = true,
        .draw_background = true,
        .floating_x = true,
        .floating_y = true,
    }, "", .{});
    selection_node.bg_color = vec4{ 0, 0, 1, 0.25 };
    const text_before_mark = buffer[0..widget_node.mark];
    const partial_text_rect_mark = self.font.textRect(text_before_mark) catch unreachable;
    const selection_size = abs(partial_text_rect_mark.max[0] - partial_text_rect.max[0]);
    selection_node.pref_size = [2]Size{ Size.pixels(selection_size, 1), cursor_node.pref_size[1] };
    selection_node.rel_pos = vec2{
        std.math.min(partial_text_rect_mark.max[0], partial_text_rect.max[0]),
        0,
    } + UiContext.text_padd;

    if (!sig.focused) return sig;

    var text_actions = std.BoundedArray(TextAction, 100).init(0) catch unreachable;

    while (self.window_ptr.event_queue.next()) |event| {
        const has_selection = widget_node.cursor != widget_node.mark;
        var ev_was_used = true;
        switch (event) {
            // TODO: mouse input
            // [ ] click to position cursor
            // [ ] hold+drag to change mark
            // [ ] double click to select work
            // [ ] click while selection if valid to reset selection+cursor
            // [ ] triple click to select all
            // [ ] drag while selection is the double-click work selection does word scan type drag
            .MouseDown => {},
            .MouseDrag => {},
            .MouseUp => {},

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
                            text_actions.append(home_action) catch unreachable;
                            action.flags.keep_mark = true;
                            action.delta = std.math.maxInt(isize);
                        } else ev_was_used = false;
                    },
                    else => ev_was_used = false,
                }
                if (ev_was_used) text_actions.append(action) catch unreachable;
            },
            .Char => |codepoint| {
                const add_codepoint_action = TextAction{
                    .flags = .{},
                    .delta = 0,
                    .codepoint = @intCast(u21, codepoint),
                };
                text_actions.append(add_codepoint_action) catch unreachable;
            },
            else => ev_was_used = false,
        }
        if (ev_was_used) self.window_ptr.event_queue.removeCurrent();
    }

    for (text_actions.slice()) |action| {
        var unicode_buf: [4]u8 = undefined;
        const text_op = textOpFromAction(action, widget_node.cursor, widget_node.mark, &unicode_buf, buffer, buf_len.*);

        replaceRange(buffer, buf_len, .{ .start = text_op.range.start, .end = text_op.range.end }, text_op.replace_str);
        if (text_op.copy_str.len > 0) {
            const c_str = self.allocator.dupeZ(u8, text_op.copy_str) catch unreachable;
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

// helper function for `textInput`
// note: if we're pasting data from the clipboard (i.e. action.flags.paste == true) the `replace_str` is only valid until
// the next call to this function that uses the clipboard.
fn textOpFromAction(action: TextAction, cursor: usize, mark: usize, unicode_buf: []u8, buffer: []const u8, buf_len: usize) TextOp {
    var text_op = TextOp{
        .range = .{ .start = 0, .end = 0 },
        .replace_str = &[0]u8{},
        .copy_str = &[0]u8{},
        .byte_cursor = cursor,
        .byte_mark = mark,
    };

    // translate high level char/word delta into a buffer specific byte delta
    const delta_sign: isize = if (action.delta > 0) 1 else -1;
    var byte_delta: isize = 0;

    if (action.delta == 0) {
        _ = byte_delta; // nothing to do;
    } else if (action.delta == std.math.minInt(isize) or action.delta == std.math.maxInt(isize)) {
        byte_delta = delta_sign * @intCast(isize, buf_len);
    } else if (action.flags.word_scan) {
        // note: because all of our words separators are ASCII we can use a non-unicode aware search
        // zig fmt: off
        const word_seps = [_]u8{
            '`', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '=', '+',
            '[', '{', ']', '}', '\\', '|',
            ';', ':', '\'', '"',
            ' ', ',', '<', '.', '>', '/', '?',
        };
        // zig fmt: on

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
        end_word_loop: while (i >= 0 and i <= buf_len) : (i = castWrappedAdd(i, delta_sign)) {
            for (word_seps) |sep| {
                if (buffer[i] == sep and !(delta_sign < 0 and i == cursor)) break :end_word_loop;
            }
            if (i == 0 and delta_sign < 0) break;
        }
        start_word_loop: while (i >= 0 and i <= buf_len) : (i = castWrappedAdd(i, delta_sign)) {
            for (word_seps) |sep| {
                if (buffer[i] == sep) continue :start_word_loop;
            }
            if (i == 0 and delta_sign < 0) break;
            if (delta_sign < 0 and i < buf_len) i += 1;
            break :start_word_loop;
        }

        byte_delta = castAndSub(isize, i, cursor);
    } else {
        if (action.delta == 1 and cursor < buf_len) {
            byte_delta = std.unicode.utf8ByteSequenceLength(buffer[cursor]) catch unreachable;
        } else if (action.delta == -1 and cursor > 0) {
            const utf8_viewer = Utf8Viewer.init(buffer[0..cursor]);
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
        text_op.copy_str = if (cursor >= mark) buffer[mark..cursor] else buffer[cursor..mark];
    }
    if (action.flags.paste) {
        const clipboard_str = c.glfwGetClipboardString(null);
        text_op.replace_str = clipboard_str[0..c.strlen(clipboard_str)];
    }

    // calculate the range we're gonna operate on
    const new_byte_cursor = @intCast(isize, text_op.byte_cursor) + byte_delta;
    const size_diff = castAndSub(isize, text_op.replace_str.len, text_op.range.end - text_op.range.start);
    const new_buf_len = castWrappedAdd(buf_len, size_diff);
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
        for (buffer[range.start + new.len .. buf_len.* - space_shrink]) |i| {
            buffer[i] = buffer[i + space_shrink];
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
