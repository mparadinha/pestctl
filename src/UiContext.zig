const std = @import("std");
const Allocator = std.mem.Allocator;
const gl = @import("gl_4v3.zig");
const c = @import("c.zig");
const gfx = @import("graphics.zig");
const math = @import("math.zig");
const vec2 = math.vec2;
const vec3 = math.vec3;
const vec4 = math.vec4;
const mat4 = math.mat4;
const Font = @import("Font.zig");
const window = @import("window.zig");

const UiContext = @This();
pub usingnamespace @import("ui_widgets.zig");

allocator: Allocator,
generic_shader: gfx.Shader,
font: Font,
icon_font: Font,
string_arena: std.heap.ArenaAllocator,
node_table: NodeTable,
prng: PRNG,

window_ptr: *window.Window, // only used for setting the cursor

// to prevent having error return in all the functions, we ignore the errors during the
// ui building phase, and return one only at the end of the building phase.
// so we store the stack trace of the first error that occurred here
first_error_trace: ?*std.builtin.StackTrace,
first_error_name: []const u8,

// per-frame data
parent_stack: Stack(*Node),
style_stack: Stack(Style),
auto_pop_style: bool,
root_node: *Node,
ctx_menu_root_node: ?*Node,
tooltip_root_node: ?*Node,
screen_size: vec2,
mouse_pos: vec2, // in pixels
events: *window.EventQueue,

// cross-frame data
frame_idx: usize,
hot_node_key: ?NodeKey,
active_node_key: ?NodeKey,
focused_node_key: ?NodeKey,

const NodeKey = NodeTable.Hash;

// call `deinit` to cleanup resources
pub fn init(allocator: Allocator, font_path: []const u8, icon_font_path: []const u8, window_ptr: *window.Window) !UiContext {
    var self = UiContext{
        .allocator = allocator,
        .generic_shader = gfx.Shader.from_srcs(allocator, "ui_generic", .{
            .vertex = vertex_shader_src,
            .geometry = geometry_shader_src,
            .fragment = fragment_shader_src,
        }),
        .font = try Font.from_ttf(allocator, font_path, 18),
        .icon_font = try Font.from_ttf(allocator, icon_font_path, 18),
        .string_arena = std.heap.ArenaAllocator.init(allocator),
        .node_table = NodeTable.init(allocator),
        .prng = PRNG.init(0),

        .window_ptr = window_ptr,

        .first_error_trace = null,
        .first_error_name = "",

        .parent_stack = Stack(*Node).init(allocator),
        .style_stack = Stack(Style).init(allocator),
        .auto_pop_style = false,
        .root_node = undefined,
        .tooltip_root_node = null,
        .ctx_menu_root_node = null,
        .screen_size = undefined,
        .mouse_pos = undefined,
        .events = undefined,

        .frame_idx = 0,
        .hot_node_key = null,
        .active_node_key = null,
        .focused_node_key = null,
    };
    return self;
}

pub fn deinit(self: *UiContext) void {
    self.style_stack.deinit();
    self.parent_stack.deinit();
    self.node_table.deinit();
    self.string_arena.deinit();
    self.font.deinit();
    self.icon_font.deinit();
    self.generic_shader.deinit();
}

pub const Flags = packed struct {
    clickable: bool = false,
    selectable: bool = false, // maintains focus when clicked
    scrollable: bool = false, // makes it so scroll wheel updates the Node.scroll_offset

    clip_children: bool = false,
    draw_text: bool = false,
    draw_border: bool = false,
    draw_background: bool = false,
    draw_hot_effects: bool = false,
    draw_active_effects: bool = false,

    floating_x: bool = false,
    floating_y: bool = false,

    no_id: bool = false,
    ignore_hash_sep: bool = false,
};

pub const Node = struct {
    // tree links (updated every frame)
    first: ?*Node,
    last: ?*Node,
    next: ?*Node,
    prev: ?*Node,
    parent: ?*Node,
    child_count: usize,

    // per-frame params
    flags: Flags,
    display_string: []const u8,
    hash_string: []const u8,
    bg_color: vec4,
    border_color: vec4,
    text_color: vec4,
    corner_roundness: f32,
    border_thickness: f32,
    pref_size: [2]Size,
    child_layout_axis: Axis,
    cursor_type: window.CursorType,
    font_type: FontType,
    text_align: TextAlign,

    // per-frame sizing information
    text_rect: Rect,

    // post-size-determination data
    calc_size: vec2,
    calc_rel_pos: vec2, // relative to bottom left (0, 0) corner of the parent

    // post-layout data
    rect: Rect,
    clip_rect: Rect,

    // persistent cross-frame state
    hot_trans: f32,
    active_trans: f32,
    first_frame_touched: usize,
    last_frame_touched: usize,

    // cross-frame state for specific features
    rel_pos: vec2, // for floating nodes
    rel_pos_placement: Placement.Tag, // which coords of this node is `rel_pos` relative to
    rel_pos_placement_parent: Placement.Tag, // which coords of the parent is `rel_pos` relative to
    cursor: usize, // used for text input
    mark: usize, // used for text input
    last_click_time: f32, // used for double click checks
    last_double_click_time: f32, // used for triple click checks
    scroll_offset: vec2,
};

pub const Axis = enum { x, y };
pub const FontType = enum { text, icon };
pub const TextAlign = enum { left, center, right };

pub const Style = struct {
    bg_color: vec4 = vec4{ 0.24, 0.27, 0.31, 1 },
    border_color: vec4 = vec4{ 0.5, 0.5, 0.5, 0.75 },
    text_color: vec4 = vec4{ 1, 1, 1, 1 },
    corner_roundness: f32 = 0,
    border_thickness: f32 = 2,
    pref_size: [2]Size = .{ Size.text_dim(1), Size.text_dim(1) },
    child_layout_axis: Axis = .y,
    cursor_type: window.CursorType = .arrow,
    font_type: FontType = .text,
    text_align: TextAlign = .left,
};

pub const Size = union(enum) {
    pixels: struct { value: f32, strictness: f32 },
    text_dim: struct { strictness: f32 },
    percent: struct { value: f32, strictness: f32 },
    by_children: struct { strictness: f32 },

    pub fn pixels(value: f32, strictness: f32) Size {
        return Size{ .pixels = .{ .value = value, .strictness = strictness } };
    }
    pub fn text_dim(strictness: f32) Size {
        return Size{ .text_dim = .{ .strictness = strictness } };
    }
    pub fn percent(value: f32, strictness: f32) Size {
        return Size{ .percent = .{ .value = value, .strictness = strictness } };
    }
    pub fn by_children(strictness: f32) Size {
        return Size{ .by_children = .{ .strictness = strictness } };
    }

    pub fn getStrictness(self: Size) f32 {
        return switch (self) {
            .pixels => |pixels| pixels.strictness,
            .text_dim => |text_dim| text_dim.strictness,
            .percent => |percent| percent.strictness,
            .by_children => |by_children| by_children.strictness,
        };
    }
};

pub const Rect = struct {
    min: vec2,
    max: vec2,

    pub fn size(self: Rect) vec2 {
        return self.max - self.min;
    }

    pub fn contains(self: Rect, pos: vec2) bool {
        return pos[0] < self.max[0] and pos[0] > self.min[0] and
            pos[1] < self.max[1] and pos[1] > self.min[1];
    }

    pub fn containsRect(self: Rect, other: Rect) bool {
        return self.contains(other.min) and self.contains(other.max);
    }

    pub fn snapped(self: Rect, placement: Placement) Rect {
        const calc_size = self.size();
        const bottom_left = placement.convertTo(.btm_left, calc_size);
        return .{ .min = bottom_left, .max = bottom_left + calc_size };
    }

    pub fn format(value: Rect, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        //try writer.print("{{ .min={" ++ fmt ++ "}, .max={" ++ fmt ++ "}}}", .{ value.min, value.max });
        _ = fmt;
        try writer.print("{{.min={d:.2}, .max={d:.2}}}", .{ value.min, value.max });
    }
};

pub const Placement = union(enum) {
    top_left: vec2,
    btm_left: vec2,
    top_right: vec2,
    btm_right: vec2,
    center: vec2,

    const Tag = std.meta.Tag(Placement);

    // `@unionInit` only works if the tag is comptime known
    pub fn init(tag: Tag, v: vec2) Placement {
        return switch (tag) {
            .top_left => .{ .top_left = v },
            .btm_left => .{ .btm_left = v },
            .top_right => .{ .top_right = v },
            .btm_right => .{ .btm_right = v },
            .center => .{ .center = v },
        };
    }

    pub fn value(self: Placement) vec2 {
        return switch (self) {
            .top_left => |v| v,
            .btm_left => |v| v,
            .top_right => |v| v,
            .btm_right => |v| v,
            .center => |v| v,
        };
    }

    pub fn convertTo(self: Placement, new_tag: Tag, size: vec2) Placement {
        if (std.meta.activeTag(self) == new_tag) return self;
        const center = self.getCenter(size);
        const half_size = size / vec2{ 2, 2 };
        return switch (new_tag) {
            .top_left => Placement{ .top_left = center + vec2{ -half_size[0], half_size[1] } },
            .btm_left => Placement{ .btm_left = center - half_size },
            .top_right => Placement{ .top_right = center + half_size },
            .btm_right => Placement{ .btm_right = center + vec2{ half_size[0], -half_size[1] } },
            .center => Placement{ .center = center },
        };
    }

    pub fn getCenter(self: Placement, size: vec2) vec2 {
        const half_size = size / vec2{ 2, 2 };
        return switch (self) {
            .top_left => |tl| tl + vec2{ half_size[0], -half_size[1] },
            .btm_left => |bl| bl + half_size,
            .top_right => |tr| tr - half_size,
            .btm_right => |br| br + vec2{ -half_size[0], half_size[1] },
            .center => |cntr| cntr,
        };
    }
};

pub const Signal = struct {
    clicked: bool,
    pressed: bool,
    released: bool,
    double_clicked: bool,
    triple_clicked: bool,
    mouse_pos: vec2, // these are relative to bottom-left corner of node
    hovering: bool,
    held_down: bool,
    enter_pressed: bool,
    mouse_drag: ?Rect, // relative coordinates, just like `Signal.mouse_pos`
    scroll_offset: vec2,
    focused: bool,
};

pub fn addNode(self: *UiContext, flags: Flags, string: []const u8, init_args: anytype) *Node {
    if (!std.unicode.utf8ValidateSlice(string)) {
        std.debug.panic("`string` passed in for Node is not valid utf8:\n{}", .{std.fmt.fmtSliceEscapeLower(string)});
    }

    const node = self.addNodeRaw(flags, string, init_args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk self.root_node;
    };
    return node;
}

pub fn addNodeF(self: *UiContext, flags: Flags, comptime fmt: []const u8, args: anytype, init_args: anytype) *Node {
    const str = std.fmt.allocPrint(self.string_arena.allocator(), fmt, args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    return self.addNode(flags, str, init_args);
}

pub fn addNodeStrings(self: *UiContext, flags: Flags, display_string: []const u8, hash_string: []const u8, init_args: anytype) *Node {
    if (!std.unicode.utf8ValidateSlice(display_string)) {
        std.debug.panic("`display_string` passed in for Node is not valid utf8:\n{}", .{std.fmt.fmtSliceEscapeLower(display_string)});
    }
    if (!std.unicode.utf8ValidateSlice(hash_string)) {
        std.debug.panic("`hash_string` passed in for Node is not valid utf8:\n{}", .{std.fmt.fmtSliceEscapeLower(hash_string)});
    }

    const node = self.addNodeRawStrings(flags, display_string, hash_string, init_args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk self.root_node;
    };
    return node;
}

pub fn addNodeStringsF(
    self: *UiContext,
    flags: Flags,
    comptime display_fmt: []const u8,
    display_args: anytype,
    comptime hash_fmt: []const u8,
    hash_args: anytype,
    init_args: anytype,
) *Node {
    const display_str = std.fmt.allocPrint(self.string_arena.allocator(), display_fmt, display_args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    const hash_str = std.fmt.allocPrint(self.string_arena.allocator(), hash_fmt, hash_args) catch |e| blk: {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
        break :blk "";
    };
    return self.addNodeStrings(flags, display_str, hash_str, init_args);
}

pub fn addNodeRaw(self: *UiContext, flags: Flags, string: []const u8, init_args: anytype) !*Node {
    const display_string = if (flags.ignore_hash_sep) string else displayPartOfString(string);
    const hash_string = if (flags.no_id) blk: {
        // for `no_id` nodes we use a random number as the hash string, so they don't clobber each other
        break :blk hashPartOfString(&randomString(&self.prng));
    } else if (flags.ignore_hash_sep) string else hashPartOfString(string);

    return self.addNodeRawStrings(flags, display_string, hash_string, init_args);
}

pub fn addNodeRawStrings(self: *UiContext, flags: Flags, display_string: []const u8, hash_string: []const u8, init_args: anytype) !*Node {
    // if a node already exists that matches this one we just use that one
    // this way the persistant cross-frame data is possible
    const lookup_result = try self.node_table.getOrPut(hash_string);
    var node = lookup_result.value_ptr;

    // link node into the tree
    var parent = self.parent_stack.top();
    node.first = null;
    node.last = null;
    node.next = null;
    node.prev = if (parent) |parent_node| blk: {
        break :blk if (parent_node.last == node) null else parent_node.last;
    } else null;
    node.parent = parent;
    node.child_count = 0;
    if (node.prev) |prev| prev.next = node;
    if (parent) |parent_node| {
        if (parent_node.child_count == 0) parent_node.first = node;
        parent_node.child_count += 1;
        parent_node.last = node;
    }

    // set per-frame data
    node.flags = flags;
    node.display_string = display_string;
    node.hash_string = hash_string;
    const style = self.style_stack.top().?;
    inline for (@typeInfo(Style).Struct.fields) |field_type_info| {
        const field_name = field_type_info.name;
        @field(node, field_name) = @field(style, field_name);
    }

    // calling textRect is too expensive to do multiple times per frame
    const font_rect = try ((switch (node.font_type) {
        .text => &self.font,
        .icon => &self.icon_font,
    }).textRect(display_string));
    node.text_rect = .{ .min = font_rect.min, .max = font_rect.max };

    // reset layout data (but not the final screen rect which we need for signal stuff)
    node.calc_size = vec2{ 0, 0 };
    node.calc_rel_pos = vec2{ 0, 0 };

    // update cross-frame (persistant) data
    node.last_frame_touched = self.frame_idx;
    if (!lookup_result.found_existing) {
        node.first_frame_touched = self.frame_idx;
        node.rel_pos_placement = .btm_left;
        node.rel_pos_placement_parent = .btm_left;
        node.last_click_time = 0;
        node.last_double_click_time = 0;
        node.scroll_offset = vec2{ 0, 0 };
    }

    // user overrides of node data
    inline for (@typeInfo(@TypeOf(init_args)).Struct.fields) |field_type_info| {
        const field_name = field_type_info.name;
        @field(node, field_name) = @field(init_args, field_name);
    }

    if (self.auto_pop_style) {
        _ = self.popStyle();
        self.auto_pop_style = false;
    }

    return node;
}

pub fn pushParent(self: *UiContext, node: *Node) void {
    self.parent_stack.push(node) catch |e|
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
}
pub fn popParent(self: *UiContext) *Node {
    return self.parent_stack.pop().?;
}
pub fn topParent(self: *UiContext) *Node {
    return self.parent_stack.top().?;
}

pub fn pushStyle(self: *UiContext, partial_style: anytype) void {
    var style = self.style_stack.top().?;
    inline for (@typeInfo(@TypeOf(partial_style)).Struct.fields) |field_type_info| {
        const field_name = field_type_info.name;
        if (!@hasField(Node, field_name)) {
            @compileError("Style does not have a field named '" ++ field_name ++ "'");
        }
        @field(style, field_name) = @field(partial_style, field_name);
    }
    self.style_stack.push(style) catch |e| {
        self.setErrorInfo(@errorReturnTrace(), @errorName(e));
    };
}
pub fn popStyle(self: *UiContext) Style {
    return self.style_stack.pop().?;
}
pub fn topStyle(self: *UiContext) Style {
    return self.style_stack.top().?;
}
/// same as `pushStyle` but the it gets auto-pop'd after the next `addNode`
pub fn pushTmpStyle(self: *UiContext, partial_style: anytype) void {
    self.pushStyle(partial_style);
    if (self.auto_pop_style) std.debug.panic("only one auto-pop'd style can be in the stack\n", .{});
    self.auto_pop_style = true;
}

pub fn getNodeSignal(self: *UiContext, node: *Node) Signal {
    var signal = Signal{
        .clicked = false,
        .pressed = false,
        .released = false,
        .double_clicked = false,
        .triple_clicked = false,
        .mouse_pos = self.window_ptr.mouse_pos() - node.rect.min,
        .hovering = false,
        .held_down = false,
        .enter_pressed = false,
        .mouse_drag = null,
        .scroll_offset = vec2{ 0, 0 },
        .focused = false,
    };

    const mouse_is_over = node.rect.contains(self.mouse_pos);
    const node_key = self.keyFromNode(node);

    const hot_key_matches = if (self.hot_node_key) |key| key == node_key else false;
    const active_key_matches = if (self.active_node_key) |key| key == node_key else false;
    const focused_key_matches = if (self.focused_node_key) |key| key == node_key else false;

    var is_hot = mouse_is_over;
    var is_active = active_key_matches;
    var is_focused = focused_key_matches;

    const mouse_down_ev = self.events.find(.MouseDown, c.GLFW_MOUSE_BUTTON_LEFT);
    var used_mouse_down_ev = false;
    const mouse_up_ev = self.events.find(.MouseUp, c.GLFW_MOUSE_BUTTON_LEFT);
    var used_mouse_up_ev = false;
    const enter_down_ev = self.events.match(.KeyDown, .{ .key = c.GLFW_KEY_ENTER });
    var used_enter_down_ev = false;
    const enter_up_ev = self.events.match(.KeyUp, .{ .key = c.GLFW_KEY_ENTER });
    var used_enter_up_ev = false;

    signal.hovering = is_hot;

    if (node.flags.clickable) {
        // begin/end a click if there was a mouse down/up event on this node
        if (is_hot and !active_key_matches and mouse_down_ev != null) {
            signal.pressed = true;
            is_active = true;
            used_mouse_down_ev = true;
        } else if (is_hot and active_key_matches and mouse_up_ev != null) {
            signal.released = true;
            signal.clicked = true;
            is_active = false;
            used_mouse_up_ev = true;
        }

        signal.held_down = is_active;
    }

    if (node.flags.selectable) {
        if (mouse_down_ev != null) is_focused = hot_key_matches;

        // selectables support recieving clicks when the mouse up event happens outside
        if (is_focused and active_key_matches and mouse_up_ev != null) {
            signal.released = true;
            signal.clicked = true;
            is_active = false;
            used_mouse_up_ev = true;
        }

        // TODO: maybe we should remove the whole enter_pressed thing
        // and just have it use the clicked one instead?
        if (is_focused and enter_up_ev != null) {
            signal.enter_pressed = true;
            used_enter_up_ev = true;
        }
    }

    if (node.flags.scrollable) {
        if (is_hot) {
            // HACK for scrollable regions. we should remove the event when consuming it
            // I prob need to implement "floating" (whatever that is)
            if (self.events.fetch(.MouseScroll, null)) |ev| {
                var scroll_off = vec2{ ev.x, ev.y };
                if (ev.shift_held) scroll_off = vec2{ ev.y, ev.x };
                node.scroll_offset += math.times(scroll_off, 50);
                signal.scroll_offset = scroll_off;
            }
        }
    }

    signal.focused = is_focused;

    // set/reset the hot and active keys
    if (is_hot and !hot_key_matches) self.hot_node_key = node_key;
    if (!is_hot and hot_key_matches) self.hot_node_key = null;
    if (is_active and !active_key_matches) self.active_node_key = node_key;
    if (!is_active and active_key_matches) self.active_node_key = null;
    if (is_focused and !focused_key_matches) self.focused_node_key = node_key;
    if (!is_focused and focused_key_matches) self.focused_node_key = null;

    if (used_mouse_down_ev) _ = self.events.removeAt(mouse_down_ev.?);
    if (used_mouse_up_ev) _ = self.events.removeAt(mouse_up_ev.?);
    if (used_enter_down_ev) _ = self.events.removeAt(enter_down_ev.?);
    if (used_enter_up_ev) _ = self.events.removeAt(enter_up_ev.?);

    // double/triple click logic
    const delay_time = 0.4; // 400 milliseconds
    const cur_time = @floatCast(f32, c.glfwGetTime());
    if (signal.clicked and node.last_click_time + delay_time > cur_time)
        signal.double_clicked = true;
    if (signal.double_clicked and node.last_double_click_time + delay_time > cur_time)
        signal.triple_clicked = true;
    if (signal.clicked) node.last_click_time = cur_time;
    if (signal.double_clicked) node.last_double_click_time = cur_time;

    return signal;
}

pub fn startBuild(self: *UiContext, screen_w: u32, screen_h: u32, mouse_pos: vec2, events: *window.EventQueue) !void {
    // remove the `no_id` nodes from the hash table before starting this new frame
    var node_iter = self.node_table.valueIterator();
    while (node_iter.next()) |node| {
        if (node.flags.no_id) try node_iter.removeCurrent();
    }

    const screen_size = vec2{ @intToFloat(f32, screen_w), @intToFloat(f32, screen_h) };
    self.screen_size = screen_size;
    self.mouse_pos = mouse_pos;
    self.events = events;

    std.debug.assert(self.parent_stack.len() == 0);

    self.style_stack.clear();
    const default_style = Style{};
    try self.style_stack.push(default_style);

    const root_pref_sizes = [2]Size{ Size.pixels(screen_size[0], 1), Size.pixels(screen_size[1], 1) };
    const whole_screen_rect = Rect{ .min = vec2{ 0, 0 }, .max = screen_size };
    self.root_node = try self.addNodeRaw(.{ .clip_children = true }, "###INTERNAL_ROOT_NODE", .{
        .pref_size = root_pref_sizes,
        .rect = whole_screen_rect,
        .clip_rect = whole_screen_rect,
    });
    try self.parent_stack.push(self.root_node);

    self.ctx_menu_root_node = null;
    self.tooltip_root_node = null;

    self.first_error_trace = null;

    var mouse_cursor = window.CursorType.arrow;
    if (self.focused_node_key) |key| mouse_cursor = self.node_table.getFromHash(key).?.cursor_type;
    if (self.hot_node_key) |key| mouse_cursor = self.node_table.getFromHash(key).?.cursor_type;
    if (self.active_node_key) |key| mouse_cursor = self.node_table.getFromHash(key).?.cursor_type;
    self.window_ptr.set_cursor(mouse_cursor);
}

pub fn endBuild(self: *UiContext, dt: f32) void {
    if (self.first_error_trace) |error_trace| {
        std.debug.print("{}\n", .{error_trace});
        std.debug.panic("An error occurred during the UI building phase: {s}", .{self.first_error_name});
    }

    _ = self.style_stack.pop().?;
    const parent = self.parent_stack.pop().?;
    std.debug.assert(parent == self.root_node);

    std.debug.assert(self.parent_stack.len() == 0);

    // stale node pruning, or else they just keep taking up memory forever
    var node_iter = self.node_table.valueIterator();
    while (node_iter.next()) |node_ptr| {
        if (node_ptr.last_frame_touched < self.frame_idx) {
            node_iter.removeCurrent() catch unreachable;
        }
    }

    // in case the hot/active/focused node key is pointing to a stale node
    if (self.hot_node_key != null and !self.node_table.hasKeyHash(self.hot_node_key.?)) self.hot_node_key = null;
    if (self.active_node_key != null and !self.node_table.hasKeyHash(self.active_node_key.?)) self.active_node_key = null;
    if (self.focused_node_key != null and !self.node_table.hasKeyHash(self.focused_node_key.?)) self.focused_node_key = null;

    // update the transition/animation values
    const fast_rate = 1 - std.math.pow(f32, 2, -20.0 * dt);
    node_iter = self.node_table.valueIterator();
    while (node_iter.next()) |node_ptr| {
        const node_key = self.keyFromNode(node_ptr);
        const is_hot = (self.hot_node_key == node_key);
        const is_active = (self.active_node_key == node_key);
        const hot_target = if (is_hot) @as(f32, 1) else @as(f32, 0);
        const active_target = if (is_active) @as(f32, 1) else @as(f32, 0);
        node_ptr.hot_trans += (hot_target - node_ptr.hot_trans) * fast_rate;
        node_ptr.active_trans += (active_target - node_ptr.active_trans) * fast_rate;
    }

    self.frame_idx += 1;
}

pub fn render(self: *UiContext) !void {
    var shader_inputs = std.ArrayList(ShaderInput).init(self.allocator);
    defer shader_inputs.deinit();

    try self.setupTreeForRender(&shader_inputs, self.root_node);
    if (self.ctx_menu_root_node) |node| try self.setupTreeForRender(&shader_inputs, node);
    if (self.tooltip_root_node) |node| try self.setupTreeForRender(&shader_inputs, node);

    // create vertex buffer
    var inputs_vao: u32 = 0;
    gl.genVertexArrays(1, &inputs_vao);
    defer gl.deleteVertexArrays(1, &inputs_vao);
    gl.bindVertexArray(inputs_vao);
    var inputs_vbo: u32 = 0;
    gl.genBuffers(1, &inputs_vbo);
    defer gl.deleteBuffers(1, &inputs_vbo);
    gl.bindBuffer(gl.ARRAY_BUFFER, inputs_vbo);
    const stride = @sizeOf(ShaderInput);
    gl.bufferData(gl.ARRAY_BUFFER, @intCast(isize, shader_inputs.items.len * stride), shader_inputs.items.ptr, gl.STATIC_DRAW);
    var field_offset: usize = 0;
    inline for (@typeInfo(ShaderInput).Struct.fields) |field, i| {
        const elems = switch (@typeInfo(field.field_type)) {
            .Float => 1,
            .Array => |array| array.len,
            else => @compileError("new type in ShaderInput struct: " ++ @typeName(field.field_type)),
        };
        const child_type = switch (@typeInfo(field.field_type)) {
            .Array => |array| array.child,
            else => field.field_type,
        };
        const gl_type = switch (@typeInfo(child_type)) {
            .Float => gl.FLOAT,
            else => @compileError("new type in ShaderInput struct: " ++ @typeName(child_type)),
        };
        const offset_ptr = if (field_offset == 0) null else @intToPtr(*const anyopaque, field_offset);
        gl.vertexAttribPointer(i, elems, gl_type, gl.FALSE, stride, offset_ptr);
        gl.enableVertexAttribArray(i);
        field_offset += @sizeOf(field.field_type);
    }

    // always draw on top of whatever was on screen, no matter what
    var saved_depth_func: c_int = undefined;
    gl.getIntegerv(gl.DEPTH_FUNC, &saved_depth_func);
    defer gl.depthFunc(@intCast(c_uint, saved_depth_func));
    gl.depthFunc(gl.ALWAYS);

    self.generic_shader.bind();
    self.generic_shader.set("screen_size", self.screen_size);
    self.generic_shader.set("text_atlas", @as(i32, 0));
    self.font.texture.bind(0);
    self.generic_shader.set("icon_atlas", @as(i32, 1));
    self.icon_font.texture.bind(1);
    gl.bindVertexArray(inputs_vao);
    gl.drawArrays(gl.POINTS, 0, @intCast(i32, shader_inputs.items.len));
}

fn setupTreeForRender(self: *UiContext, shader_inputs: *std.ArrayList(ShaderInput), root: *Node) !void {
    // do the whole layout right before rendering
    self.solveIndependentSizes(root);
    self.solveDownwardDependent(root);
    self.solveUpwardDependent(root);
    self.solveViolations(root);
    self.solveFinalPos(root);

    var node_iterator = DepthFirstNodeIterator{ .cur_node = root };
    while (node_iterator.next()) |node| {
        try self.addShaderInputsForNode(shader_inputs, node);
    }
}

// small helper for `UiContext.render`
fn addShaderInputsForNode(self: *UiContext, shader_inputs: *std.ArrayList(ShaderInput), node: *Node) !void {
    const base_rect = ShaderInput{
        .bottom_left_pos = node.rect.min,
        .top_right_pos = node.rect.max,
        .bottom_left_uv = vec2{ 0, 0 },
        .top_right_uv = vec2{ 0, 0 },
        .top_color = vec4{ 0, 0, 0, 0 },
        .bottom_color = vec4{ 0, 0, 0, 0 },
        .corner_roundness = node.corner_roundness,
        .border_thickness = node.border_thickness,
        .clip_rect_min = node.clip_rect.min,
        .clip_rect_max = node.clip_rect.max,
        .use_icon_font = switch (node.font_type) {
            .text => 0,
            .icon => 1,
        },
    };

    // draw background
    if (node.flags.draw_background) {
        var rect = base_rect;
        rect.top_color = node.bg_color;
        rect.bottom_color = node.bg_color;
        rect.border_thickness = 0;
        try shader_inputs.append(rect);

        const hot_remove_factor = if (node.flags.draw_active_effects) node.active_trans else 0;
        const effective_hot_trans = node.hot_trans * (1 - hot_remove_factor);

        if (node.flags.draw_hot_effects) {
            rect = base_rect;
            rect.border_thickness = 0;
            rect.top_color = vec4{ 1, 1, 1, 0.1 * effective_hot_trans };
            try shader_inputs.append(rect);
        }
        if (node.flags.draw_active_effects) {
            rect = base_rect;
            rect.border_thickness = 0;
            rect.bottom_color = vec4{ 1, 1, 1, 0.1 * node.active_trans };
            try shader_inputs.append(rect);
        }
    }

    // draw border
    if (node.flags.draw_border) {
        var rect = base_rect;
        rect.top_color = node.border_color;
        rect.bottom_color = node.border_color;
        try shader_inputs.append(rect);

        if (node.flags.draw_hot_effects) {
            rect = base_rect;
            rect.top_color = vec4{ 1, 1, 1, 0.2 * node.hot_trans };
            rect.bottom_color = vec4{ 1, 1, 1, 0.2 * node.hot_trans };
            try shader_inputs.append(rect);
        }
    }

    // draw text
    if (node.flags.draw_text) {
        const font = switch (node.font_type) {
            .text => &self.font,
            .icon => &self.icon_font,
        };

        var text_pos = self.textPosFromNode(node);
        if (node.flags.draw_active_effects) {
            text_pos[1] -= 0.1 * font.pixel_size * node.active_trans;
        }

        const display_text = node.display_string;

        const quads = try font.buildQuads(self.allocator, display_text);
        defer self.allocator.free(quads);
        for (quads) |quad| {
            var quad_rect = Rect{ .min = quad.points[0].pos, .max = quad.points[2].pos };
            quad_rect.min += text_pos;
            quad_rect.max += text_pos;

            try shader_inputs.append(.{
                .bottom_left_pos = quad_rect.min,
                .top_right_pos = quad_rect.max,
                .bottom_left_uv = quad.points[0].uv,
                .top_right_uv = quad.points[2].uv,
                .top_color = node.text_color,
                .bottom_color = node.text_color,
                .corner_roundness = 0,
                .border_thickness = 0,
                .clip_rect_min = base_rect.clip_rect_min,
                .clip_rect_max = base_rect.clip_rect_max,
                .use_icon_font = base_rect.use_icon_font,
            });
        }
    }
}

// this struct must have the exact layout expected by the shader
const ShaderInput = extern struct {
    bottom_left_pos: [2]f32,
    top_right_pos: [2]f32,
    bottom_left_uv: [2]f32,
    top_right_uv: [2]f32,
    top_color: [4]f32,
    bottom_color: [4]f32,
    corner_roundness: f32,
    border_thickness: f32,
    clip_rect_min: [2]f32,
    clip_rect_max: [2]f32,
    use_icon_font: f32,
};

const vertex_shader_src =
    \\#version 330 core
    \\
    \\layout (location = 0) in vec2 attrib_bottom_left_pos;
    \\layout (location = 1) in vec2 attrib_top_right_pos;
    \\layout (location = 2) in vec2 attrib_bottom_left_uv;
    \\layout (location = 3) in vec2 attrib_top_right_uv;
    \\layout (location = 4) in vec4 attrib_top_color;
    \\layout (location = 5) in vec4 attrib_bottom_color;
    \\layout (location = 6) in float attrib_corner_roundness;
    \\layout (location = 7) in float attrib_border_thickness;
    \\layout (location = 8) in vec2 attrib_clip_rect_min;
    \\layout (location = 9) in vec2 attrib_clip_rect_max;
    \\layout (location = 10) in float attrib_use_icon_font;
    \\
    \\uniform vec2 screen_size; // in pixels
    \\
    \\out VS_Out {
    \\    vec2 bottom_left_pos;
    \\    vec2 top_right_pos;
    \\    vec2 bottom_left_uv;
    \\    vec2 top_right_uv;
    \\    vec4 top_color;
    \\    vec4 bottom_color;
    \\    float corner_roundness;
    \\    float border_thickness;
    \\    vec2 rect_size;
    \\    vec2 clip_rect_min;
    \\    vec2 clip_rect_max;
    \\    float use_icon_font;
    \\} vs_out;
    \\
    \\void main() {
    \\    // the input position coordinates come in pixel screen space (which goes from
    \\    // (0, 0) at the bottom left of the screen, to (screen_size.x, screen_size.y) at
    \\    // the top right of the screen) so we need to transform them into NDC (which goes
    \\    // from (-1, -1) to (1, 1))
    \\    vs_out.bottom_left_pos = (attrib_bottom_left_pos / screen_size) * 2 - vec2(1);
    \\    vs_out.top_right_pos = (attrib_top_right_pos / screen_size) * 2 - vec2(1);
    \\    vs_out.bottom_left_uv = attrib_bottom_left_uv;
    \\    vs_out.top_right_uv = attrib_top_right_uv;
    \\    vs_out.top_color = attrib_top_color;
    \\    vs_out.bottom_color = attrib_bottom_color;
    \\    vs_out.corner_roundness = attrib_corner_roundness;
    \\    vs_out.border_thickness = attrib_border_thickness;
    \\    vs_out.rect_size = attrib_top_right_pos - attrib_bottom_left_pos;
    \\    vs_out.clip_rect_min = attrib_clip_rect_min;
    \\    vs_out.clip_rect_max = attrib_clip_rect_max;
    \\    vs_out.use_icon_font = attrib_use_icon_font;
    \\}
    \\
;
const geometry_shader_src =
    \\#version 330 core
    \\
    \\layout (points) in;
    \\layout (triangle_strip, max_vertices = 6) out;
    \\
    \\uniform vec2 screen_size; // in pixels
    \\
    \\in VS_Out {
    \\    vec2 bottom_left_pos;
    \\    vec2 top_right_pos;
    \\    vec2 bottom_left_uv;
    \\    vec2 top_right_uv;
    \\    vec4 top_color;
    \\    vec4 bottom_color;
    \\    float corner_roundness;
    \\    float border_thickness;
    \\    vec2 rect_size;
    \\    vec2 clip_rect_min;
    \\    vec2 clip_rect_max;
    \\    float use_icon_font;
    \\} gs_in[];
    \\
    \\out GS_Out {
    \\    vec2 uv;
    \\    vec4 color;
    \\    vec2 quad_coords;
    \\    float quad_size_ratio;
    \\    float corner_roundness;
    \\    float border_thickness;
    \\    vec2 rect_size;
    \\    vec2 clip_rect_min;
    \\    vec2 clip_rect_max;
    \\    float use_icon_font;
    \\} gs_out;
    \\
    \\void main() {
    \\    vec4 bottom_left_pos  = vec4(gs_in[0].bottom_left_pos, 0, 1);
    \\    vec2 bottom_left_uv   = gs_in[0].bottom_left_uv;
    \\    vec4 top_right_pos    = vec4(gs_in[0].top_right_pos, 0, 1);
    \\    vec2 top_right_uv     = gs_in[0].top_right_uv;
    \\    vec4 bottom_right_pos = vec4(top_right_pos.x, bottom_left_pos.y, 0, 1);
    \\    vec2 bottom_right_uv  = vec2(top_right_uv.x,  bottom_left_uv.y);
    \\    vec4 top_left_pos     = vec4(bottom_left_pos.x, top_right_pos.y, 0, 1);
    \\    vec2 top_left_uv      = vec2(bottom_left_uv.x,  top_right_uv.y);
    \\
    \\    vec2 quad_size = gs_in[0].top_right_pos - gs_in[0].bottom_left_pos;
    \\
    \\    // some things are the same for all verts of the quad
    \\    gs_out.corner_roundness = gs_in[0].corner_roundness;
    \\    gs_out.border_thickness = gs_in[0].border_thickness;
    \\    gs_out.rect_size = gs_in[0].rect_size;
    \\    gs_out.quad_size_ratio = (quad_size.x / quad_size.y) * (screen_size.x / screen_size.y);
    \\    gs_out.clip_rect_min = gs_in[0].clip_rect_min;
    \\    gs_out.clip_rect_max = gs_in[0].clip_rect_max;
    \\    gs_out.use_icon_font = gs_in[0].use_icon_font;
    \\
    \\    gl_Position        = bottom_left_pos;
    \\    gs_out.uv          = bottom_left_uv;
    \\    gs_out.color       = gs_in[0].bottom_color;
    \\    gs_out.quad_coords = vec2(0, 0);
    \\    EmitVertex();
    \\    gl_Position        = bottom_right_pos;
    \\    gs_out.uv          = bottom_right_uv;
    \\    gs_out.color       = gs_in[0].bottom_color;
    \\    gs_out.quad_coords = vec2(1, 0);
    \\    EmitVertex();
    \\    gl_Position        = top_right_pos;
    \\    gs_out.uv          = top_right_uv;
    \\    gs_out.color       = gs_in[0].top_color;
    \\    gs_out.quad_coords = vec2(1, 1);
    \\    EmitVertex();
    \\    EndPrimitive();
    \\
    \\    gl_Position        = bottom_left_pos;
    \\    gs_out.uv          = bottom_left_uv;
    \\    gs_out.color       = gs_in[0].bottom_color;
    \\    gs_out.quad_coords = vec2(0, 0);
    \\    EmitVertex();
    \\    gl_Position        = top_right_pos;
    \\    gs_out.uv          = top_right_uv;
    \\    gs_out.color       = gs_in[0].top_color;
    \\    gs_out.quad_coords = vec2(1, 1);
    \\    EmitVertex();
    \\    gl_Position        = top_left_pos;
    \\    gs_out.uv          = top_left_uv;
    \\    gs_out.color       = gs_in[0].top_color;
    \\    gs_out.quad_coords = vec2(0, 1);
    \\    EmitVertex();
    \\    EndPrimitive();
    \\}
    \\
;
const fragment_shader_src =
    \\#version 330 core
    \\
    \\in vec4 gl_FragCoord;
    \\
    \\in GS_Out {
    \\    vec2 uv;
    \\    vec4 color;
    \\    vec2 quad_coords;
    \\
    \\    float quad_size_ratio;
    \\    float corner_roundness; // 0 is square quad, 1 is full circle
    \\
    \\    // these are all in pixels
    \\    float border_thickness;
    \\    vec2 rect_size;
    \\    vec2 clip_rect_min;
    \\    vec2 clip_rect_max;
    \\
    \\    float use_icon_font;
    \\} fs_in;
    \\
    \\uniform vec2 screen_size; // in pixels
    \\uniform sampler2D text_atlas;
    \\uniform sampler2D icon_atlas;
    \\
    \\out vec4 FragColor;
    \\
    \\bool rectContains(vec2 rect_min, vec2 rect_max, vec2 point) {
    \\    return (rect_min.x <= point.x && point.x <= rect_max.x) &&
    \\           (rect_min.y <= point.y && point.y <= rect_max.y);
    \\}
    \\
    \\bool insideBorder(vec2 quad_coords) {
    \\    if (fs_in.border_thickness == 0) return true;
    \\
    \\    vec2 coords = abs((quad_coords * 2) - vec2(1));
    \\    vec2 side_dist = (fs_in.rect_size / 2) * (vec2(1) - coords);
    \\    return side_dist.x <= fs_in.border_thickness || side_dist.y <= fs_in.border_thickness;
    \\}
    \\
    \\void main() {
    \\    vec2 pixel_coord = gl_FragCoord.xy - vec2(0.5);
    \\    if (!rectContains(fs_in.clip_rect_min, fs_in.clip_rect_max, pixel_coord)) {
    \\        FragColor = vec4(0);
    \\        return;
    \\    }
    \\
    \\    vec2 uv = fs_in.uv;
    \\    vec4 color = fs_in.color;
    \\    vec2 quad_coords = fs_in.quad_coords;
    \\
    \\    float text_alpha = texture(text_atlas, uv).r;
    \\    float icon_alpha = texture(icon_atlas, uv).r;
    \\    float alpha = (fs_in.use_icon_font == 0) ? text_alpha : icon_alpha;
    \\    if (uv == vec2(0, 0)) alpha = 1;
    \\
    \\    if (!insideBorder(quad_coords)) alpha = 0;
    \\
    \\    FragColor = color * vec4(1, 1, 1, alpha);
    \\}
    \\
;

fn solveIndependentSizes(self: *UiContext, node: *Node) void {
    const work_fn = solveIndependentSizesWorkFn;
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .x });
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .y });
}

fn solveDownwardDependent(self: *UiContext, node: *Node) void {
    const work_fn = solveDownwardDependentWorkFn;
    layoutRecurseHelperPost(work_fn, .{ .self = self, .node = node, .axis = .x });
    layoutRecurseHelperPost(work_fn, .{ .self = self, .node = node, .axis = .y });
}

fn solveUpwardDependent(self: *UiContext, node: *Node) void {
    const work_fn = solveUpwardDependentWorkFn;
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .x });
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .y });
}

fn solveViolations(self: *UiContext, node: *Node) void {
    const work_fn = solveViolationsWorkFn;
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .x });
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .y });
}

fn solveFinalPos(self: *UiContext, node: *Node) void {
    const work_fn = solveFinalPosWorkFn;
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .x });
    layoutRecurseHelperPre(work_fn, .{ .self = self, .node = node, .axis = .y });
}

fn solveIndependentSizesWorkFn(self: *UiContext, node: *Node, axis: Axis) void {
    _ = self;
    const axis_idx: usize = @enumToInt(axis);
    switch (node.pref_size[axis_idx]) {
        .pixels => |pixels| node.calc_size[axis_idx] = pixels.value,
        // this is wrong for percent (the correct one is calculated later) but this gives
        // and upper bound on the size, which might be needed for "downward dependent" nodes
        // which have children with `Size.percent`
        .percent,
        .text_dim,
        => {
            //const text_rect = self.font.textRect(node.display_string) catch |e| switch (e) {
            //    error.InvalidUtf8 => unreachable, // we already check this on node creation
            //    error.OutOfMemory => std.debug.panic("that's fucking unlucky!\n", .{}),
            //};
            const text_rect = node.text_rect;
            const text_size = text_rect.max - text_rect.min;
            const text_padding = switch (axis) {
                .x => text_hpadding,
                .y => text_vpadding,
            };
            node.calc_size[axis_idx] = text_size[axis_idx] + 2 * text_padding;
        },
        else => {},
    }
}

fn solveDownwardDependentWorkFn(self: *UiContext, node: *Node, axis: Axis) void {
    _ = self;

    const axis_idx: usize = @enumToInt(axis);
    const is_layout_axis = (axis == node.child_layout_axis);

    switch (node.pref_size[axis_idx]) {
        .by_children => {
            var value: f32 = 0;
            var child = node.first;
            while (child) |child_node| : (child = child_node.next) {
                if (is_layout_axis) {
                    value += child_node.calc_size[axis_idx];
                } else {
                    value = std.math.max(value, child_node.calc_size[axis_idx]);
                }
            }
            node.calc_size[axis_idx] = value;
        },
        else => {},
    }
}

fn solveUpwardDependentWorkFn(self: *UiContext, node: *Node, axis: Axis) void {
    _ = self;
    const axis_idx: usize = @enumToInt(axis);
    switch (node.pref_size[axis_idx]) {
        .percent => |percent| {
            if (node.parent) |parent| {
                node.calc_size[axis_idx] = parent.calc_size[axis_idx] * percent.value;
            }
        },
        else => {},
    }
}

fn solveViolationsWorkFn(self: *UiContext, node: *Node, axis: Axis) void {
    _ = self;
    if (node.child_count == 0) return;

    const axis_idx: usize = @enumToInt(axis);
    const is_layout_axis = (axis == node.child_layout_axis);

    // collect sizing information about children
    var total_children_size: f32 = 0;
    var max_child_size: f32 = 0;
    var zero_strict_take_budget: f32 = 0;
    var other_children_leeway: f32 = 0;
    var zero_strict_children = std.BoundedArray(*Node, 1000).init(0) catch unreachable;
    var other_children = std.BoundedArray(*Node, 1000).init(0) catch unreachable;
    var child = node.first;
    while (child) |child_node| : (child = child_node.next) {
        const is_floating = switch (axis) {
            .x => child_node.flags.floating_x,
            .y => child_node.flags.floating_y,
        };
        if (is_floating) continue;

        const strictness = child_node.pref_size[axis_idx].getStrictness();
        const child_size = child_node.calc_size[axis_idx];

        total_children_size += child_size;
        max_child_size = std.math.max(max_child_size, child_size);
        if (strictness == 0) {
            zero_strict_take_budget += child_size;
            zero_strict_children.append(child_node) catch unreachable;
        } else {
            other_children_leeway += (1 - strictness);
            other_children.append(child_node) catch unreachable;
        }
    }

    const total_size = if (is_layout_axis) total_children_size else max_child_size;
    var overflow = std.math.max(0, total_size - node.calc_size[axis_idx]);

    // shrink zero strictness children as much as we can (to 0 size if needed) before
    // trying to shrink other children with strictness > 0
    const zero_strict_remove_amount = std.math.min(overflow, zero_strict_take_budget);
    for (zero_strict_children.slice()) |z_child| {
        if (is_layout_axis) {
            const z_child_percent = z_child.calc_size[axis_idx] / zero_strict_take_budget;
            z_child.calc_size[axis_idx] -= zero_strict_remove_amount * z_child_percent;
        } else {
            const extra_size = z_child.calc_size[axis_idx] - node.calc_size[axis_idx];
            z_child.calc_size[axis_idx] -= std.math.max(0, extra_size);
        }
    }
    overflow -= zero_strict_remove_amount;

    // if there's still overflow, shrink the other children as much as we can
    // (proportionally to their strictness values, i.e least strict shrinks the most)
    if (overflow > 0) {
        for (other_children.slice()) |child_node| {
            const strictness = child_node.pref_size[axis_idx].getStrictness();
            if (strictness == 1) continue;
            const child_size = child_node.calc_size[axis_idx];
            const child_take_budget = child_size * strictness;
            const leeway_percent = (1 - strictness) / other_children_leeway;
            const remove_amount = if (is_layout_axis)
                overflow * leeway_percent
            else
                std.math.max(0, child_size - node.calc_size[axis_idx]);
            child_node.calc_size[axis_idx] -= std.math.min(child_take_budget, remove_amount);
        }
    }
}

fn solveFinalPosWorkFn(self: *UiContext, node: *Node, axis: Axis) void {
    _ = self;

    const axis_idx: usize = @enumToInt(axis);
    const is_layout_axis = (axis == node.child_layout_axis);

    if (node.parent == null) {
        const is_floating = switch (axis) {
            .x => node.flags.floating_x,
            .y => node.flags.floating_y,
        };
        if (is_floating) {
            const placement = Placement.init(node.rel_pos_placement, node.rel_pos);
            const bottom_left = placement.convertTo(.btm_left, node.calc_size).value();
            node.calc_rel_pos[axis_idx] = bottom_left[axis_idx];
        }
        node.rect.min[axis_idx] = node.calc_rel_pos[axis_idx];
        node.rect.max[axis_idx] = node.calc_rel_pos[axis_idx] + node.calc_size[axis_idx];
    }

    if (node.child_count == 0) return;

    // start layout at the top left
    const start_rel_pos: f32 = switch (axis) {
        .x => 0,
        .y => node.calc_size[1],
    };

    // position all the children
    var rel_pos: f32 = start_rel_pos;
    var child = node.first;
    while (child) |child_node| : (child = child_node.next) {
        const is_floating = switch (axis) {
            .x => child_node.flags.floating_x,
            .y => child_node.flags.floating_y,
        };
        if (is_floating) {
            const calc_rel_pos = Placement.init(child_node.rel_pos_placement, child_node.rel_pos)
                .convertTo(.btm_left, child_node.calc_size)
                .convertTo(child_node.rel_pos_placement_parent, node.calc_size)
                .value();
            child_node.calc_rel_pos[axis_idx] = calc_rel_pos[axis_idx];
            continue;
        }

        if (is_layout_axis) {
            const rel_pos_advance = child_node.calc_size[axis_idx];
            switch (axis) {
                .x => {
                    child_node.calc_rel_pos[axis_idx] = rel_pos;
                    rel_pos += rel_pos_advance;
                },
                .y => {
                    rel_pos -= rel_pos_advance;
                    child_node.calc_rel_pos[axis_idx] = rel_pos;
                },
            }
        } else {
            switch (axis) {
                .x => child_node.calc_rel_pos[axis_idx] = start_rel_pos,
                .y => child_node.calc_rel_pos[axis_idx] = start_rel_pos - child_node.calc_size[axis_idx],
            }
        }
    }

    // calculate the final screen pixel rect
    child = node.first;
    while (child) |child_node| : (child = child_node.next) {
        child_node.rect.min[axis_idx] = node.rect.min[axis_idx] + child_node.calc_rel_pos[axis_idx];
        child_node.rect.max[axis_idx] = child_node.rect.min[axis_idx] + child_node.calc_size[axis_idx];
        // propagate the clipping to children
        child_node.clip_rect = if (node.flags.clip_children) node.rect else node.clip_rect;
    }
}

const LayoutWorkFn = fn (*UiContext, *Node, Axis) void;
const LayoutWorkFnArgs = struct { self: *UiContext, node: *Node, axis: Axis };
/// do the work before recursing
fn layoutRecurseHelperPre(work_fn: LayoutWorkFn, args: LayoutWorkFnArgs) void {
    work_fn(args.self, args.node, args.axis);
    var child = args.node.first;
    while (child) |child_node| : (child = child_node.next) {
        layoutRecurseHelperPre(work_fn, .{ .self = args.self, .node = child_node, .axis = args.axis });
    }
}
/// do the work after recursing
fn layoutRecurseHelperPost(work_fn: LayoutWorkFn, args: LayoutWorkFnArgs) void {
    var child = args.node.first;
    while (child) |child_node| : (child = child_node.next) {
        layoutRecurseHelperPost(work_fn, .{ .self = args.self, .node = child_node, .axis = args.axis });
    }
    work_fn(args.self, args.node, args.axis);
}

pub const text_hpadding: f32 = 4;
pub const text_vpadding: f32 = 4;
pub const text_padd = vec2{ text_hpadding, text_vpadding };

pub fn textPosFromNode(self: *UiContext, node: *Node) vec2 {
    _ = self;

    const node_size = node.rect.size();
    const text_rect = node.text_rect;
    const text_size = text_rect.size();

    const rel_text_x = switch (node.text_align) {
        .left => text_hpadding,
        .center => (node_size[0] / 2) - (text_size[0] / 2),
        .right => node_size[0] - text_hpadding - text_size[0],
    };

    const box_middle = node.rect.min + (node_size / vec2{ 2, 2 });

    return vec2{
        node.rect.min[0] + rel_text_x,
        box_middle[1] + (text_size[1] / 2) - text_rect.max[1],
    };
}

pub fn setErrorInfo(self: *UiContext, trace: ?*std.builtin.StackTrace, name: []const u8) void {
    self.first_error_trace = trace;
    self.first_error_name = name;
}

pub fn nodeFromKey(self: UiContext, key: NodeKey) ?*Node {
    return self.node_table.getFromHash(key);
}

pub fn keyFromNode(self: UiContext, node: *Node) NodeKey {
    return self.node_table.ctx.hash(hashPartOfString(node.hash_string));
}

pub fn displayPartOfString(string: []const u8) []const u8 {
    if (std.mem.indexOf(u8, string, "###")) |idx| {
        return string[0..idx];
    } else return string;
}

pub fn hashPartOfString(string: []const u8) []const u8 {
    if (std.mem.indexOf(u8, string, "###")) |idx| {
        return string[idx + 3 ..];
    } else return string;
}

pub const DepthFirstNodeIterator = struct {
    cur_node: *Node,
    parent_level: usize = 0, // how many times have we gone down the hierarchy

    pub fn next(self: *DepthFirstNodeIterator) ?*Node {
        if (self.cur_node.child_count > 0) {
            self.parent_level += 1;
            self.cur_node = self.cur_node.first.?;
        } else if (self.cur_node.next) |next_sibling| {
            self.cur_node = next_sibling;
        } else {
            while (self.cur_node.next == null) {
                self.cur_node = self.cur_node.parent orelse return null;
                self.parent_level -= 1;
                if (self.parent_level == 0) return null;
            }
            self.cur_node = self.cur_node.next.?;
        }
        return self.cur_node;
    }
};

/// very small wrapper around std.ArrayList that provides push, pop, and top functions
pub fn Stack(comptime T: type) type {
    return struct {
        array_list: std.ArrayList(T),

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return Self{ .array_list = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: Self) void {
            self.array_list.deinit();
        }

        pub fn push(self: *Self, item: T) !void {
            try self.array_list.append(item);
        }

        pub fn pop(self: *Self) ?T {
            return self.array_list.popOrNull();
        }

        pub fn top(self: Self) ?T {
            if (self.array_list.items.len == 0) return null;
            return self.array_list.items[self.array_list.items.len - 1];
        }

        pub fn len(self: Self) usize {
            return self.array_list.items.len;
        }

        pub fn clear(self: *Self) void {
            self.array_list.clearRetainingCapacity();
        }
    };
}

/// Hash map where pointers to entries remains stable when adding new ones.
/// Supports removing entries while iterating over them.
pub const NodeTable = struct {
    allocator: Allocator,
    ctx: HashContext,

    key_mappings: KeyMap,

    const K = []const u8;
    const V = Node;
    const Hash = u64;

    // note: this makes lookups O(n). if that starts to become a problem
    // we can just switch this ArrayList to a HashMap and NodeTable becomes
    // essentially a wrapper around std.HashMap
    const KeyMap = std.ArrayList(struct { key_hash: Hash, value_ptr: *V });

    const HashContext = struct {
        pub fn hash(self: @This(), key: K) u64 {
            _ = self;
            return std.hash_map.hashString(key);
        }
        pub fn eql(self: @This(), key_a: K, key_b: K) bool {
            _ = self;
            return std.mem.eql(u8, key_a, key_b);
        }
    };

    pub fn init(allocator: Allocator) NodeTable {
        return .{
            .allocator = allocator,
            .ctx = HashContext{},
            .key_mappings = KeyMap.init(allocator),
        };
    }

    pub fn deinit(self: *NodeTable) void {
        for (self.key_mappings.items) |map| self.allocator.destroy(map.value_ptr);
        self.key_mappings.deinit();
    }

    pub const GetOrPutResult = struct { found_existing: bool, value_ptr: *V };

    pub fn getOrPut(self: *NodeTable, key: K) !GetOrPutResult {
        const key_hash = self.ctx.hash(key);
        for (self.key_mappings.items) |key_map| {
            if (key_map.key_hash == key_hash) {
                return GetOrPutResult{ .found_existing = true, .value_ptr = key_map.value_ptr };
            }
        }

        const value_ptr = try self.allocator.create(V);
        try self.key_mappings.append(.{ .key_hash = key_hash, .value_ptr = value_ptr });

        return GetOrPutResult{ .found_existing = false, .value_ptr = value_ptr };
    }

    pub fn getFromHash(self: *NodeTable, hash: Hash) ?*V {
        for (self.key_mappings.items) |key_map| {
            if (key_map.key_hash == hash) {
                return key_map.value_ptr;
            }
        }
        return null;
    }

    /// does nothing if the key doesn't exist
    pub fn remove(self: *NodeTable, key: K) void {
        const key_hash = self.ctx.hash(key);
        for (self.key_mappings.items) |key_map, i| {
            if (key_map.key_hash == key_hash) {
                self.allocator.destroy(key_map.value_ptr);
                _ = self.key_mappings.swapRemove(i);
                return;
            }
        }
    }

    pub fn hasKey(self: *NodeTable, key: K) bool {
        return self.hasKeyHash(self.ctx.hash(key));
    }

    pub fn hasKeyHash(self: *NodeTable, hash: Hash) bool {
        for (self.key_mappings.items) |key_map| {
            if (key_map.key_hash == hash) return true;
        }
        return false;
    }

    pub fn valueIterator(self: *NodeTable) ValueIterator {
        return ValueIterator{ .table = self, .iter_idx = null };
    }

    pub const ValueIterator = struct {
        table: *NodeTable,
        iter_idx: ?usize, // non-null while iterating

        pub fn next(self: *ValueIterator) ?*V {
            const idx = if (self.iter_idx) |idx| idx else blk: {
                self.iter_idx = 0;
                break :blk 0;
            };
            self.iter_idx.? += 1;

            if (idx >= self.table.key_mappings.items.len) return null;

            return self.table.key_mappings.items[idx].value_ptr;
        }

        pub const RemoveError = error{NotIterating};

        /// remove the thing that was last return by `next` from the table
        /// sets the removed entry to undefined memory
        pub fn removeCurrent(self: *ValueIterator) RemoveError!void {
            const idx = if (self.iter_idx) |idx| idx else return RemoveError.NotIterating;
            if (idx > self.table.key_mappings.items.len) return RemoveError.NotIterating;

            self.table.allocator.destroy(self.table.key_mappings.items[idx - 1].value_ptr);
            _ = self.table.key_mappings.swapRemove(idx - 1);
            self.iter_idx.? -= 1;
        }
    };
};

pub const PRNG = struct {
    state: u64,

    pub fn init(seed: u64) PRNG {
        return .{ .state = seed };
    }

    pub fn next(self: *PRNG) u64 {
        // 3 random primes I generated online
        self.state = ((self.state * 2676693499) + 5223158351) % 4150081079;
        return self.state;
    }
};

pub fn randomString(prng: *PRNG) [32]u8 {
    var buf: [32]u8 = undefined;
    _ = std.fmt.bufPrint(&buf, "{x:0>16}{x:0>16}", .{ prng.next(), prng.next() }) catch unreachable;
    return buf;
}

pub fn dumpNodeTree(self: *UiContext) void {
    var node_iter = self.node_table.valueIterator();
    while (node_iter.next()) |node| {
        std.debug.print("{*} :: first=0x{x:0>15}, last=0x{x:0>15}, next=0x{x:0>15}, prev=0x{x:0>15}, parent=0x{x:0>15}, child_count={}\n", .{
            node,
            if (node.first) |ptr| @ptrToInt(ptr) else 0,
            if (node.last) |ptr| @ptrToInt(ptr) else 0,
            if (node.next) |ptr| @ptrToInt(ptr) else 0,
            if (node.prev) |ptr| @ptrToInt(ptr) else 0,
            if (node.parent) |ptr| @ptrToInt(ptr) else 0,
            node.child_count,
        });
    }
}
