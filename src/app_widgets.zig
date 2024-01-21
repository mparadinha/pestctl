//! widgets not in the sense of basic UI widgets (like button,checkbox,etc)
//! but instead higher level widgets, like "File Picker" or "Register View"

const std = @import("std");
const Allocator = std.mem.Allocator;
const zig_ui = @import("zig-ui");
const vec2 = zig_ui.vec2;
const vec4 = zig_ui.vec4;
const UI = zig_ui.UI;
const Size = UI.Size;

// TODO: allow choosing a directory instead of a file?
// TODO: ctrl+l for editing path bar with auto-complete
pub const FilePicker = struct {
    allocator: Allocator,
    path: []const u8,
    active_entry: ?usize,

    pub fn init(allocator: Allocator, path: []const u8) !FilePicker {
        return .{
            .allocator = allocator,
            .path = try allocator.dupe(u8, path),
            .active_entry = null,
        };
    }

    pub fn deinit(self: FilePicker) void {
        self.allocator.free(self.path);
    }

    /// returns non-null if a file was choosen
    /// return path is valid memory until FilePicker is deinit'd
    pub fn show(self: *FilePicker, ui: *UI) !?[]const u8 {
        const window = ui.startWindow(
            "file picker",
            [2]Size{ Size.percent(0.5, 0), Size.percent(0.5, 0) },
            UI.RelativePlacement.match(.center),
        );
        defer ui.endWindow(window);

        ui.label(self.path);

        {
            const parent = ui.pushLayoutParent(
                .{},
                "file picker name/attribs bar",
                [2]Size{ Size.percent(1, 1), Size.children(1) },
                .x,
            );
            defer ui.popParentAssert(parent);
            ui.pushStyle(.{ .border_color = vec4{ 0, 0, 0, 0 } });
            defer _ = ui.popStyle();
            _ = ui.button("Name");
            parent.last.?.size[0] = Size.percent(1, 0);
            _ = ui.button("Size");
            parent.last.?.size[0] = Size.text(1);
            _ = ui.button("Modified");
            parent.last.?.size[0] = Size.text(1);
            var child = parent.first;
            while (child) |child_node| : (child = child_node.next) {
                child_node.flags.draw_background = false;
                if (ui.hot_node_key) |hot_key| {
                    if (hot_key == ui.keyFromNode(child_node)) {
                        parent.flags.draw_border = true;
                        parent.border_color = vec4{ 1, 1, 1, 0.2 };
                    }
                }
            }
        }

        const files_parent = ui.pushLayoutParent(.{
            .scroll_children_x = true,
            .scroll_children_y = true,
            .clip_children = true,
        }, "file picker files", [2]Size{
            Size.percent(1, 0), Size.percent(1, 0),
        }, .y);
        defer ui.popParentAssert(files_parent);

        // TODO: handle root dir for this
        if (ui.button("../").double_clicked) {
            const parent_dir = try std.fs.path.join(self.allocator, &.{
                self.path, "..",
            });
            defer self.allocator.free(parent_dir);
            self.allocator.free(self.path);
            self.path = try std.fs.realpathAlloc(self.allocator, parent_dir);
        }

        var dir = try std.fs.openIterableDirAbsolute(self.path, .{ .access_sub_paths = true });
        defer dir.close();

        // TODO: sort entries based on name and other attributes
        // TODO: show entry attributes like last mod. time and size

        var iter_idx: usize = 0;
        var dir_iter = dir.iterate();
        while (try dir_iter.next()) |entry| {
            const btn_sig = ui.button(entry.name);
            if (btn_sig.clicked) self.active_entry = iter_idx;
            if (btn_sig.double_clicked) {
                const new_path = try std.fs.path.join(self.allocator, &.{
                    self.path, entry.name,
                });
                self.allocator.free(self.path);
                self.path = new_path;
                if (entry.kind == .file) return self.path;
            }
            iter_idx += 1;
        }

        // TODO: arrow keys to select active entry
        // TODO: pressing enter to pick the current active entry

        return null;
    }
};

pub const SourceViewer = struct {
    allocator: Allocator,
    files: std.ArrayList(File),

    // TODO: highlight boxes (with animation when switching to new locations)
    const File = struct {
        path: []const u8,
        content: []const u8,

        pub fn free(self: File, allocator: Allocator) void {
            allocator.free(self.path);
            allocator.free(self.content);
        }
    };

    pub fn init(allocator: Allocator) SourceViewer {
        return .{
            .allocator = allocator,
            .files = std.ArrayList(File).init(allocator),
        };
    }

    pub fn deinit(self: SourceViewer) void {
        for (self.files.items) |file| file.free(self.allocator);
        self.files.deinit();
    }

    pub fn show(self: *SourceViewer, ui: *UI) !void {
        if (self.files.items.len == 0) return;
        const scroll_parent = ui.pushLayoutParent(.{
            .draw_background = true,
            .clip_children = true,
            .scroll_children_x = true,
            .scroll_children_y = true,
        }, "SourceViewer", Size.flexible(.percent, 1, 1), .x);
        defer ui.popParentAssert(scroll_parent);

        const text = self.files.items[0].content;

        const number_of_lines = blk: {
            const vec_size = comptime std.simd.suggestVectorSize(u8).?;
            var total: usize = 0;
            for (0..text.len / vec_size) |chunk_idx| {
                const chunk: @Vector(vec_size, u8) = text[chunk_idx * vec_size ..][0..vec_size].*;
                total += std.simd.countElementsWithValue(chunk, '\n');
            }
            total += std.mem.count(u8, text[text.len - text.len % vec_size ..], "\n");
            break :blk total;
        };
        const digits_per_line = @floor(std.math.log10(@as(f64, @floatFromInt(number_of_lines)))) + 1;
        const buffer_size = number_of_lines * (1 + @as(usize, @intFromFloat(digits_per_line)));
        var line_number_bytes = try self.allocator.alloc(u8, buffer_size);
        defer self.allocator.free(line_number_bytes);
        var stream = std.io.fixedBufferStream(line_number_bytes);
        for (0..number_of_lines) |line_num| try stream.writer().print("{d}\n", .{line_num});
        const line_numbers = stream.getWritten();

        ui.label(line_numbers);
        ui.label(self.files.items[0].content);

        // TODO: add floating child at end for scroll bar
    }

    pub const Focus = struct {};

    pub fn focusOn(self: *SourceViewer, focus: Focus) void {
        _ = focus;
        _ = self;
    }

    pub fn addFile(self: *SourceViewer, absolute_path: []const u8) !void {
        const file = try std.fs.openFileAbsolute(absolute_path, .{});
        defer file.close();
        const content = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        errdefer self.allocator.free(content);
        const path = try self.allocator.dupe(u8, absolute_path);
        errdefer self.allocator.free(path);
        try self.files.append(.{ .path = path, .content = content });
    }
};
