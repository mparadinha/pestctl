//! widgets not in the sense of basic UI widgets (like button,checkbox,etc)
//! but instead higher level widgets, like "File Picker" or "Register View"

const std = @import("std");
const Allocator = std.mem.Allocator;
const math = @import("math.zig");
const vec2 = math.vec2;
const vec4 = math.vec4;
const UI = @import("UI.zig");
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
                "file picker name/attribs bar",
                [2]Size{ Size.percent(1, 1), Size.by_children(1) },
                .x,
            );
            defer ui.popParentAssert(parent);
            ui.pushStyle(.{ .border_color = vec4{ 0, 0, 0, 0 } });
            defer _ = ui.popStyle();
            _ = ui.button("Name");
            parent.last.?.pref_size[0] = Size.percent(1, 0);
            _ = ui.button("Size");
            parent.last.?.pref_size[0] = Size.text_dim(1);
            _ = ui.button("Modified");
            parent.last.?.pref_size[0] = Size.text_dim(1);
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

        const files_parent = ui.pushLayoutParentFlags(.{
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
