const std = @import("std");
const c = @import("../c.zig");

pub const TextAction = struct {
    flags: struct {
        keep_mark: bool = false,
        word_scan: bool = false,
        delete: bool = false,
        copy: bool = false,
        paste: bool = false,
    },
    delta: isize,
    codepoint: ?u21 = null,
};

pub const TextOp = struct {
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
pub fn textActionsFromKeyEvent(key: i32, has_selection: bool, shift: bool, control: bool) std.BoundedArray(TextAction, 5) {
    var actions = std.BoundedArray(TextAction, 5){};

    var action = TextAction{
        .flags = .{
            .keep_mark = shift,
            .word_scan = control,
        },
        .delta = 0,
        .codepoint = null,
    };
    var valid_action = true;
    switch (key) {
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
            if (control) {
                action.flags.copy = true;
                action.flags.keep_mark = true;
            } else valid_action = false;
        },
        c.GLFW_KEY_V => {
            if (control) {
                action.flags.paste = true;
            } else valid_action = false;
        },
        c.GLFW_KEY_X => {
            if (control) {
                action.flags.copy = true;
                action.flags.delete = true;
            } else valid_action = false;
        },
        c.GLFW_KEY_A => {
            if (control) {
                // `ctrl+a` is the same as doing `Home` followed by `shift+End`
                const home_action = TextAction{ .flags = .{}, .delta = std.math.minInt(isize) };
                actions.append(home_action) catch unreachable;
                action.flags.keep_mark = true;
                action.delta = std.math.maxInt(isize);
            } else valid_action = false;
        },
        else => valid_action = false,
    }

    if (valid_action) actions.append(action) catch unreachable;

    return actions;
}

// helper function for `textInput`
// note: if we're pasting data from the clipboard (i.e. action.flags.paste == true)
// the `replace_str` is only valid until the next call to this function that uses the clipboard.
pub fn textOpFromAction(action: TextAction, cursor: usize, mark: usize, unicode_buf: []u8, buf: []const u8) !TextOp {
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
        // nothing to do;
    } else if (action.delta == std.math.minInt(isize) or action.delta == std.math.maxInt(isize)) {
        byte_delta = delta_sign * @as(isize, @intCast(buf.len));
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
                if (i == 0 and delta_sign < 0) break;
                for (word_seps) |sep| {
                    if (buf[i] == sep) continue :start_word_loop;
                }
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
            byte_delta = -@as(isize, @intCast(char_size));
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
    const new_byte_cursor = @as(isize, @intCast(text_op.byte_cursor)) + byte_delta;
    const size_diff = castAndSub(isize, text_op.replace_str.len, text_op.range.end - text_op.range.start);
    const new_buf_len = castWrappedAdd(buf.len, size_diff);
    text_op.byte_cursor = @intCast(std.math.clamp(new_byte_cursor, 0, @as(isize, @intCast(new_buf_len))));
    text_op.range = .{
        .start = @min(text_op.byte_cursor, text_op.byte_mark),
        .end = @max(text_op.byte_cursor, text_op.byte_mark),
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
    var search_idx: isize = @intCast(start_idx);
    while (search_idx >= 0 and search_idx < buf.len) : (search_idx += delta) {
        const idx = @as(usize, @intCast(search_idx));
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
    var search_idx: isize = @intCast(start_idx);
    outer_loop: while (search_idx >= 0 and search_idx < buf.len) : (search_idx += delta) {
        const idx: usize = @intCast(search_idx);
        for (word_seps) |sep| {
            if (buf[idx] == sep) continue :outer_loop;
        }
        return idx;
    }

    return null;
}

// if `start_idx` is a separator this is the same `findFirstNonSep`
// if `start_idx` is a not separator this is the same `findFirstSep`
pub fn findFirstDiff(buf: []const u8, start_idx: usize, search_dir: SearchDir) ?usize {
    if (buf.len == 0) return 0;
    if (start_idx == buf.len) return null;

    const start_char = buf[start_idx];
    for (word_seps) |sep| {
        if (start_char == sep) return findFirstNonSep(buf, start_idx, search_dir);
    }
    return findFirstSep(buf, start_idx, search_dir);
}

// helper function for `textInput`. `range.end` is exclusive
pub fn replaceRange(buffer: []u8, buf_len: *usize, range: struct { start: usize, end: usize }, new: []const u8) void {
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
        for (buffer[range.start + new.len .. buf_len.* - space_shrink], 0..) |*byte, i| {
            byte.* = buffer[range.end + i];
        }
        buf_len.* -= space_shrink;
    }

    // copy the new contents in
    for (new, 0..) |byte, i| buffer[range.start + i] = byte;
}

fn castWrappedAdd(src: usize, diff: isize) usize {
    const new_src = @as(isize, @intCast(src)) + diff;
    return @intCast(@max(0, new_src));
}

fn castAndSub(comptime T: type, a: anytype, b: anytype) T {
    return @as(T, @intCast(a)) - @as(T, @intCast(b));
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
        var size = @min(4, len);
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
