// there's no need to manually include this file, it's already provided
// by UiContext.zig

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

/// if `buf` runs out of space input is truncated
/// unlike other widgets, the string here is only used for the hash and not display
pub fn textInput(self: *UiContext, hash_string: []const u8, buf: []u8, buf_len: *usize) Signal {
    // * widget/parent node (layout in x)
    // | * text node (layout in x)
    // | | * cursor node y padding node
    // | | | * cursor node

    const display_buf = buf[0..buf_len.*];

    // this is a really hacky way to see if this is the first time we using this node
    // (we need to initialize the cursor on first use)
    const first_time = !self.node_table.hasKey(hash_string);

    // NOTE: the text_cursor is in characters/codepoints *not* bytes into buf
    // (this will still be wrong for glyphs/graphemes that are several codepoints
    // long, like emoji with modifiers, for example)

    const widget_node = self.addNodeF(.{
        .clip_children = true,
        .selectable = true,
        .draw_background = true,
        .draw_border = true,
    }, "###{s}", .{hash_string}, .{
        .child_layout_axis = .x,
        .hover_cursor = .ibeam,
    });
    if (first_time) widget_node.text_cursor = @intToFloat(f32, std.unicode.utf8CountCodepoints(display_buf) catch unreachable);

    const node_key = self.keyFromNode(widget_node);
    var sig = self.getNodeSignal(widget_node);

    // make input box darker when not in focus
    if (self.active_node_key != node_key)
        widget_node.bg_color = math.times(widget_node.bg_color, 0.85);

    self.pushParent(widget_node);
    defer _ = self.popParent();

    // text until the cursor
    const partial_text_buf = Utf8Viewer.init(display_buf).bytesRange(0, @floatToInt(usize, widget_node.text_cursor));
    const partial_text_rect = self.font.textRect(partial_text_buf) catch unreachable;

    const cursor_rel_pos = partial_text_rect.max[0] + text_hpadding;
    const max_cursor_rel_pos = widget_node.rect.size()[0] - text_hpadding;
    const cursor_overflow = std.math.max(0, cursor_rel_pos - max_cursor_rel_pos);
    self.spacer(.x, Size.pixels(-cursor_overflow, 1));

    const text_node = self.addNode(.{ .no_id = true, .draw_text = true }, display_buf, .{
        .text_color = vec4{ 0, 0, 0, 1 },
        .child_layout_axis = .x,
    });
    self.pushParent(text_node);
    defer _ = self.popParent();

    self.spacer(.x, Size.pixels(partial_text_rect.max[0], 1));
    self.spacer(.x, Size.pixels(text_hpadding, 1)); // TODO: implement general padding and the we can remove the ad-hoc text padding
    {
        const v_pad_node = self.addNode(.{ .no_id = true }, "", .{ .child_layout_axis = .y });
        self.pushParent(v_pad_node);
        defer _ = self.popParent();
        self.spacer(.y, Size.pixels(text_vpadding, 1)); // TODO: implement general padding and the we can remove the ad-hoc text padding

        const cursor_node = self.addNode(.{ .no_id = true, .draw_background = true }, "", .{
            .bg_color = vec4{ 0.2, 0.2, 0.2, 1 },
        });
        cursor_node.pref_size[0] = Size.pixels(2, 1);
        cursor_node.pref_size[1] = Size.percent(0.75, 1);
    }

    if (self.active_node_key != node_key) return sig;

    self.events.iter_idx = null;
    var next_event = self.events.next();
    while (next_event) |ev| : (next_event = self.events.next()) {
        var remove_ev = true;
        const cur_buf = buf[0..buf_len.*];
        const view = Utf8Viewer.init(cur_buf);
        var cur_buf_len = buf_len.*;
        switch (ev) {
            // for control type key presses (backspace, ctrl+arrows, etc)
            // actual text is handled with the Char inputs
            .KeyDown, .KeyRepeat => |key_ev| switch (key_ev.key) {
                c.GLFW_KEY_ENTER => {
                    sig.enter_pressed = true;
                    self.active_node_key = null;
                },
                c.GLFW_KEY_BACKSPACE => {
                    const old_cursor = @floatToInt(usize, widget_node.text_cursor);
                    const new_cursor = if (key_ev.mods.control) blk: {
                        if (view.findLast(' ')) |space_pos| {
                            break :blk if (space_pos < old_cursor) space_pos + 1 else 0;
                        } else break :blk 0;
                    } else blk: {
                        break :blk if (old_cursor == 0) 0 else old_cursor - 1;
                    };
                    const old_bytes_cursor = view.charPosIntoBytes(old_cursor);
                    const new_bytes_cursor = view.charPosIntoBytes(new_cursor);

                    widget_node.text_cursor = @intToFloat(f32, new_cursor);

                    std.debug.assert(new_bytes_cursor <= old_bytes_cursor);
                    // erase buf[new_bytes_cursor .. old_bytes_cursor]
                    // move buf[old_bytes_cursor ..] to buf[new_bytes_cursor ..]
                    for (cur_buf[old_bytes_cursor..]) |b, i| {
                        buf[new_bytes_cursor + i] = b;
                    }
                    cur_buf_len -= (old_bytes_cursor - new_bytes_cursor);
                },
                c.GLFW_KEY_DELETE => {
                    const n_chars = std.unicode.utf8CountCodepoints(cur_buf) catch unreachable;
                    const cur_cursor = @floatToInt(usize, widget_node.text_cursor);
                    if (cur_cursor >= n_chars) continue;

                    const old_byte_cursor = view.charPosIntoBytes(cur_cursor);
                    const new_byte_cursor = old_byte_cursor + view.byteSizeAt(cur_cursor);

                    // note: cursor stays in the same place

                    // erase the character at cursor. move everything back one char's worth
                    // (i.e move buf[new_byte_cursor ..] to  buf[old_byte_cursor ..])
                    for (cur_buf[new_byte_cursor..]) |b, i| {
                        buf[old_byte_cursor + i] = b;
                    }
                    cur_buf_len -= (new_byte_cursor - old_byte_cursor);
                },
                c.GLFW_KEY_LEFT => {
                    if (widget_node.text_cursor > 0) widget_node.text_cursor -= 1;
                },
                c.GLFW_KEY_RIGHT => {
                    const max_cursor = std.unicode.utf8CountCodepoints(cur_buf) catch unreachable;
                    if (widget_node.text_cursor != @intToFloat(f32, max_cursor)) widget_node.text_cursor += 1;
                },
                c.GLFW_KEY_HOME => {},
                c.GLFW_KEY_END => {},
                else => remove_ev = false,
            },
            .Char => |codepoint| {
                const unicode_pt = @intCast(u21, codepoint);
                const buf_space_left = buf.len - buf_len.*;
                const codepoint_len = std.unicode.utf8CodepointSequenceLength(unicode_pt) catch
                    unreachable; // it was broken when I got here officer. it's GLFW's fault, I swear!
                if (codepoint_len > buf_space_left) break;
                var write_buf = buf[buf_len.*..];
                cur_buf_len += std.unicode.utf8Encode(unicode_pt, write_buf) catch
                    unreachable; // maybe we're all at fault here??? no, it is GLFW who is wrong.
                widget_node.text_cursor += 1;
            },
            else => remove_ev = false,
        }
        buf_len.* = cur_buf_len;
        if (remove_ev) self.events.removeCurrent();
    }

    return sig;
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
