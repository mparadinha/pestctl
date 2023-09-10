const std = @import("std");
const Allocator = std.mem.Allocator;
const gl = @import("gl_4v3.zig");
const c = @import("c.zig");
const math = @import("math.zig");
const vec2 = math.vec2;
const uvec2 = math.uvec2;

const Window = @This();

handle: *c.GLFWwindow,
event_queue: EventQueue,
cursors: struct {
    arrow: *c.GLFWcursor,
    ibeam: *c.GLFWcursor,
    crosshair: *c.GLFWcursor,
    hand: *c.GLFWcursor,
    hresize: *c.GLFWcursor,
    vresize: *c.GLFWcursor,
},
mouse_pos: ?vec2,
framebuffer_size: ?uvec2,

pub const InitError = GlfwError || gl.LoadError;

/// call 'finish_setup' right after 'init'
/// call 'deinit' to clean up resources
pub fn init(allocator: Allocator, width: u32, height: u32, title: []const u8) InitError!Window {
    _ = c.glfwSetErrorCallback(glfw_error_callback); // returns previous callback

    if (c.glfwInit() != c.GLFW_TRUE) {
        if (returnGlfwError()) unreachable else |err| return err;
    }

    c.glfwWindowHint(c.GLFW_CONTEXT_VERSION_MAJOR, 3);
    c.glfwWindowHint(c.GLFW_CONTEXT_VERSION_MINOR, 3);
    c.glfwWindowHint(c.GLFW_OPENGL_PROFILE, c.GLFW_OPENGL_CORE_PROFILE);
    c.glfwWindowHint(c.GLFW_OPENGL_FORWARD_COMPAT, gl.TRUE);
    c.glfwWindowHint(c.GLFW_TRANSPARENT_FRAMEBUFFER, c.GLFW_TRUE);
    // c.glfwWindowHint(c.GLFW_SAMPLES, 4);
    const handle = c.glfwCreateWindow(
        @as(c_int, @intCast(width)),
        @as(c_int, @intCast(height)),
        @as([*c]const u8, @ptrCast(title)),
        null,
        null,
    ) orelse if (returnGlfwError()) unreachable else |err| return err;

    c.glfwMakeContextCurrent(handle);
    c.glfwSwapInterval(1);

    var self = Window{
        .handle = handle,
        .event_queue = EventQueue.init(allocator),
        .cursors = .{
            .arrow = c.glfwCreateStandardCursor(c.GLFW_ARROW_CURSOR).?,
            .ibeam = c.glfwCreateStandardCursor(c.GLFW_IBEAM_CURSOR).?,
            .crosshair = c.glfwCreateStandardCursor(c.GLFW_CROSSHAIR_CURSOR).?,
            .hand = c.glfwCreateStandardCursor(c.GLFW_HAND_CURSOR).?,
            .hresize = c.glfwCreateStandardCursor(c.GLFW_HRESIZE_CURSOR).?,
            .vresize = c.glfwCreateStandardCursor(c.GLFW_VRESIZE_CURSOR).?,
        },
        .mouse_pos = null,
        .framebuffer_size = null,
    };

    // setup callbacks
    // these all return the previous callback, or NULL if there was none
    _ = c.glfwSetKeyCallback(self.handle, glfw_key_callback);
    _ = c.glfwSetMouseButtonCallback(self.handle, glfw_mouse_button_callback);
    _ = c.glfwSetCursorPosCallback(self.handle, glfw_cursor_pos_callback);
    _ = c.glfwSetScrollCallback(self.handle, glfw_scroll_callback);
    _ = c.glfwSetCharCallback(self.handle, glfw_char_callback);

    // load OpenGL now
    try gl.load(handle, get_proc_address_fn);
    gl.enable(gl.DEBUG_OUTPUT);
    gl.debugMessageCallback(gl_error_callback, null);

    return self;
}

pub fn deinit(self: *Window) void {
    c.glfwDestroyCursor(self.cursors.arrow);
    c.glfwDestroyCursor(self.cursors.ibeam);
    c.glfwDestroyCursor(self.cursors.crosshair);
    c.glfwDestroyCursor(self.cursors.hand);
    c.glfwDestroyCursor(self.cursors.hresize);
    c.glfwDestroyCursor(self.cursors.vresize);

    c.glfwDestroyWindow(self.handle);
    c.glfwTerminate();

    self.event_queue.deinit();
}

pub fn finishSetup(self: *Window) void {
    c.glfwSetWindowUserPointer(self.handle, self);
}

pub fn shouldClose(self: *Window) bool {
    return c.glfwWindowShouldClose(self.handle) == c.GLFW_TRUE;
}

pub fn update(self: *Window) void {
    // discard unprocessed events from the last frame because if they were needed they would've been processed
    self.event_queue.clearRetainingCapacity();

    c.glfwSwapBuffers(self.handle);
    c.glfwPollEvents();

    // there might be some bug in glfw that makes `glfwGetCursorPos` take up to 7ms *per call*
    // in frames where we type really fast, so we cache it per frame for now
    self.mouse_pos = null;
    // same for `glfwGetFramebufferSize`
    self.framebuffer_size = null;
}

pub fn getFramebufferSize(self: *Window) GlfwError!uvec2 {
    return if (self.framebuffer_size) |fb_size| fb_size else blk: {
        var width: u32 = undefined;
        var height: u32 = undefined;
        c.glfwGetFramebufferSize(self.handle, @as(*i32, @ptrCast(&width)), @as(*i32, @ptrCast(&height)));
        if (returnGlfwError()) {} else |err| return err;
        self.framebuffer_size = uvec2{ width, height };
        break :blk self.framebuffer_size.?;
    };
}

pub fn toggleCursorMode(self: *Window) void {
    const current_mode = c.glfwGetInputMode(self.handle, c.GLFW_CURSOR);
    const new_mode = switch (current_mode) {
        c.GLFW_CURSOR_NORMAL => c.GLFW_CURSOR_DISABLED,
        c.GLFW_CURSOR_HIDDEN => c.GLFW_CURSOR_NORMAL,
        c.GLFW_CURSOR_DISABLED => c.GLFW_CURSOR_NORMAL,
        else => unreachable,
    };
    c.glfwSetInputMode(self.handle, c.GLFW_CURSOR, new_mode);
}

pub const CursorMode = enum { normal, infinite };
pub fn setCursorMode(self: *Window, mode: CursorMode) void {
    const cursor_mode = switch (mode) {
        .normal => c.GLFW_CURSOR_NORMAL,
        .infinite => c.GLFW_CURSOR_DISABLED,
    };
    c.glfwSetInputMode(self.handle, c.GLFW_CURSOR, cursor_mode);
}

pub fn setCursor(self: *Window, cursor_type: CursorType) void {
    const cursor = switch (cursor_type) {
        .arrow => self.cursors.arrow,
        .ibeam => self.cursors.ibeam,
        .crosshair => self.cursors.crosshair,
        .hand => self.cursors.hand,
        .hresize => self.cursors.hresize,
        .vresize => self.cursors.vresize,
    };
    c.glfwSetCursor(self.handle, cursor);
}

pub fn keyPressed(self: *Window, key: i32) bool {
    return c.glfwGetKey(self.handle, key) == c.GLFW_PRESS;
}

/// +1 if `pos_key_id` is pressed, -1 if `neg_key_id` is pressed or 0 if both are
pub fn keyPair(self: *Window, pos_key_id: i32, neg_key_id: i32) f32 {
    return (if (self.keyPressed(pos_key_id)) @as(f32, 1) else @as(f32, 0)) +
        (if (self.keyPressed(neg_key_id)) @as(f32, -1) else @as(f32, 0));
}

pub fn getModifiers(self: *Window) InputEvent.Modifiers {
    return .{
        .shift = self.keyPressed(c.GLFW_KEY_LEFT_SHIFT) or
            self.keyPressed(c.GLFW_KEY_RIGHT_SHIFT),
        .control = self.keyPressed(c.GLFW_KEY_LEFT_CONTROL) or
            self.keyPressed(c.GLFW_KEY_RIGHT_CONTROL),
        .alt = self.keyPressed(c.GLFW_KEY_LEFT_ALT) or
            self.keyPressed(c.GLFW_KEY_RIGHT_ALT),
        .super = self.keyPressed(c.GLFW_KEY_LEFT_SUPER) or
            self.keyPressed(c.GLFW_KEY_RIGHT_SUPER),
        // TODO: these two need their state to be preserved, this is probably wrong
        // TODO: check these two!
        .caps_lock = self.keyPressed(c.GLFW_KEY_CAPS_LOCK),
        .num_lock = self.keyPressed(c.GLFW_KEY_NUM_LOCK),
    };
}

pub fn getMousePos(self: *Window) GlfwError!vec2 {
    const mouse_pos = if (self.mouse_pos) |pos| pos else blk: {
        var xpos: f64 = undefined;
        var ypos: f64 = undefined;
        c.glfwGetCursorPos(self.handle, &xpos, &ypos);
        if (returnGlfwError()) {} else |err| return err;
        self.mouse_pos = vec2{ @as(f32, @floatCast(xpos)), @as(f32, @floatCast(ypos)) };
        break :blk self.mouse_pos.?;
    };
    const framebuffer_size = try self.getFramebufferSize();

    return vec2{
        mouse_pos[0],
        @as(f32, @floatFromInt(framebuffer_size[1])) - mouse_pos[1],
    };
}

pub fn getMousePosNDC(self: *Window) GlfwError!vec2 {
    const mouse_pos = try self.getMouse_pos();
    const framebuffer_size = try self.getFramebufferSize();
    const relative = mouse_pos / @as(f32, @floatFromInt(framebuffer_size));
    return (vec2{ 2, 2 } * relative) - vec2{ 1, 1 };
}

fn glfw_error_callback(error_code: c_int, error_msg: [*c]const u8) callconv(.C) void {
    std.debug.print("glfw error (code={}): {s}", .{ error_code, error_msg });
}

fn glfw_key_callback(glfw_window: ?*c.GLFWwindow, key: i32, scancode: i32, action: i32, mods: i32) callconv(.C) void {
    const self = @as(*align(8) Window, @ptrCast(@alignCast(c.glfwGetWindowUserPointer(glfw_window).?)));

    if (key == c.GLFW_KEY_UNKNOWN) return;
    const key_ev = InputEvent.KeyEvent{ .key = key, .mods = .{
        .shift = (mods & c.GLFW_MOD_SHIFT) != 0,
        .control = (mods & c.GLFW_MOD_CONTROL) != 0,
        .alt = (mods & c.GLFW_MOD_ALT) != 0,
        .super = (mods & c.GLFW_MOD_SUPER) != 0,
        .caps_lock = (mods & c.GLFW_MOD_CAPS_LOCK) != 0,
        .num_lock = (mods & c.GLFW_MOD_NUM_LOCK) != 0,
    } };
    if (action == c.GLFW_PRESS)
        self.event_queue.append(.{ .KeyDown = key_ev }) catch unreachable;
    if (action == c.GLFW_RELEASE)
        self.event_queue.append(.{ .KeyUp = key_ev }) catch unreachable;
    if (action == c.GLFW_REPEAT)
        self.event_queue.append(.{ .KeyRepeat = key_ev }) catch unreachable;

    _ = scancode;
}

fn glfw_mouse_button_callback(glfw_window: ?*c.GLFWwindow, button: i32, action: i32, mods: i32) callconv(.C) void {
    const self = @as(*align(8) Window, @ptrCast(@alignCast(c.glfwGetWindowUserPointer(glfw_window).?)));
    if (action == c.GLFW_PRESS)
        self.event_queue.append(.{ .MouseDown = button }) catch unreachable;
    if (action == c.GLFW_RELEASE)
        self.event_queue.append(.{ .MouseUp = button }) catch unreachable;
    _ = mods;
}

fn glfw_cursor_pos_callback(glfw_window: ?*c.GLFWwindow, xpos: f64, ypos: f64) callconv(.C) void {
    const self = @as(*align(8) Window, @ptrCast(@alignCast(c.glfwGetWindowUserPointer(glfw_window).?)));

    const held_mouse_button: ?i32 =
        if (c.glfwGetMouseButton(self.handle, c.GLFW_MOUSE_BUTTON_LEFT) == c.GLFW_TRUE)
        c.GLFW_MOUSE_BUTTON_LEFT
    else if (c.glfwGetMouseButton(self.handle, c.GLFW_MOUSE_BUTTON_RIGHT) == c.GLFW_TRUE)
        c.GLFW_MOUSE_BUTTON_RIGHT
    else
        null;

    if (held_mouse_button) |button| {
        self.event_queue.append(.{ .MouseDrag = .{
            .x = @as(f32, @floatCast(xpos)),
            .y = @as(f32, @floatCast(ypos)),
            .held_button = button,
        } }) catch unreachable;
    }
}

fn glfw_scroll_callback(glfw_window: ?*c.GLFWwindow, xoffset: f64, yoffset: f64) callconv(.C) void {
    const self = @as(*align(8) Window, @ptrCast(@alignCast(c.glfwGetWindowUserPointer(glfw_window).?)));
    self.event_queue.append(.{ .MouseScroll = .{
        .x = @as(f32, @floatCast(xoffset)),
        .y = @as(f32, @floatCast(yoffset)),
        .shift_held = self.keyPressed(c.GLFW_KEY_LEFT_SHIFT) or self.keyPressed(c.GLFW_KEY_RIGHT_SHIFT),
    } }) catch unreachable;
}

fn glfw_char_callback(glfw_window: ?*c.GLFWwindow, codepoint: u32) callconv(.C) void {
    const self = @as(*align(8) Window, @ptrCast(@alignCast(c.glfwGetWindowUserPointer(glfw_window).?)));
    self.event_queue.append(.{ .Char = codepoint }) catch unreachable;
}

pub const InputEvent = union(enum) {
    KeyUp: KeyEvent,
    KeyDown: KeyEvent,
    KeyRepeat: KeyEvent,
    MouseUp: i32,
    MouseDown: i32,
    MouseDrag: struct { x: f32, y: f32, held_button: i32 }, // NOTE: this uses GLFW mouse coordinates that are relative to top-left corner
    MouseScroll: struct { x: f32, y: f32, shift_held: bool },
    GamepadUp: usize,
    GamepadDown: usize,
    Char: u32, // unicode codepoint

    pub const KeyEvent = struct {
        key: i32,
        mods: Modifiers,
    };

    // TODO: maybe the mouse should also use this type of event payload, like the key one
    // but this would make querying for simple mouse up/down in the UI worse
    //pub const MouseButtonEvent = struct {
    //    button: i32,
    //    mods: Modifiers,
    //};

    pub const Modifiers = struct {
        shift: bool = false,
        control: bool = false,
        alt: bool = false,
        super: bool = false,
        caps_lock: bool = false,
        num_lock: bool = false,
    };

    pub fn payload(self: InputEvent, comptime T: Tag) Payload(T) {
        return switch (T) {
            .KeyUp => self.KeyUp,
            .KeyDown => self.KeyDown,
            .KeyRepeat => self.KeyRelease,
            .MouseUp => self.MouseUp,
            .MouseDown => self.MouseDown,
            .MouseDrag => self.MouseDrag,
            .MouseScroll => self.MouseScroll,
            .GamepadUp => self.GamepadUp,
            .GamepadDown => self.GamepadDown,
            .Char => self.Char,
        };
    }

    pub const Tag = std.meta.Tag(InputEvent);

    pub fn Payload(comptime ev_type: Tag) type {
        return std.meta.TagPayload(InputEvent, ev_type);
    }
};

pub const EventQueue = struct {
    events: std.ArrayList(InputEvent),
    /// valid while iterating, null otherwise
    iter_idx: ?usize,

    const EventTag = std.meta.Tag(InputEvent);
    pub fn EventPayload(comptime T: EventTag) type {
        return InputEvent.Payload(T);
    }

    /// call `deinit` to clean up
    pub fn init(allocator: Allocator) EventQueue {
        return .{
            .events = std.ArrayList(InputEvent).init(allocator),
            .iter_idx = null,
        };
    }

    pub fn deinit(self: *EventQueue) void {
        self.events.deinit();
    }

    pub fn next(self: *EventQueue) ?InputEvent {
        if (self.iter_idx == null) self.iter_idx = 0;
        if (self.iter_idx.? >= self.events.items.len) return null;
        defer self.iter_idx.? += 1;
        return self.events.items[self.iter_idx.?];
    }

    /// remove the Event that was last return from `next`
    pub fn removeCurrent(self: *EventQueue) void {
        if (self.iter_idx) |iter_idx| {
            _ = self.events.orderedRemove(iter_idx - 1);
            self.iter_idx.? -= 1;
        }
    }

    /// remove event at index `idx` in the queue.
    /// works while iterating
    pub fn removeAt(self: *EventQueue, idx: usize) InputEvent {
        const ev = self.events.orderedRemove(idx);
        if (self.iter_idx) |*iter_idx| {
            if (idx < iter_idx.*) iter_idx.* -= 1;
        }
        return ev;
    }

    /// if an event matches, remove it from the queue and return it,
    /// or return null, if there are no matches
    /// (if multiple events match, only the first one is returned/removed)
    pub fn fetchAndRemove(
        self: *EventQueue,
        comptime ev_type: EventTag,
        match_ev_content: ?EventPayload(ev_type),
    ) ?EventPayload(ev_type) {
        const found_idx = self.find(ev_type, match_ev_content);
        if (found_idx) |idx| {
            return self.removeAt(idx).payload(ev_type);
        } else return null;
    }

    /// like `fetchAndRemove`, but don't remove
    pub fn fetch(self: *EventQueue, comptime ev_type: EventTag, match_ev_content: ?EventPayload(ev_type)) ?EventPayload(ev_type) {
        const found_idx = self.find(ev_type, match_ev_content);
        if (found_idx) |idx| {
            return self.events.items[idx].payload(ev_type);
        } else return null;
    }

    /// like `fetchAndRemove` but don't return the event
    /// returns true if found, false otherwise
    pub fn searchAndRemove(
        self: *EventQueue,
        comptime ev_type: EventTag,
        match_ev_content: ?EventPayload(ev_type),
    ) bool {
        const found_idx = self.find(ev_type, match_ev_content);
        if (found_idx) |idx| _ = self.removeAt(idx);
        return found_idx != null;
    }

    /// find the index in the queue for an event that matches the input
    /// returns `null` if no event matches
    pub fn find(
        self: *EventQueue,
        comptime ev_type: EventTag,
        match_ev_content: ?EventPayload(ev_type),
    ) ?usize {
        for (self.events.items, 0..) |ev, i| {
            if (ev != ev_type) continue;
            if (match_ev_content) |match_ev| {
                if (!std.meta.eql(match_ev, ev.payload(ev_type))) continue;
            }
            return i;
        }
        return null;
    }

    pub fn matchAndRemove(
        self: *EventQueue,
        comptime ev_type: EventTag,
        match_content: anytype,
    ) ?EventPayload(ev_type) {
        const match_idx = self.match(ev_type, match_content);
        if (match_idx) |idx| {
            return self.removeAt(idx).payload(ev_type);
        } else return null;
    }

    pub fn match(self: *EventQueue, comptime ev_type: EventTag, match_content: anytype) ?usize {
        const T = @TypeOf(match_content);
        const Payload = EventPayload(ev_type);

        if (@typeInfo(Payload) == .Struct) {
            if (@typeInfo(T) != .Struct) @compileError("match_content must Struct");
        } else {
            if (T != Payload and T != void)
                @compileError(@tagName(ev_type) ++ " has type " ++ @typeName(Payload) ++ " not " ++ @typeName(T));
        }

        ev_loop: for (self.events.items, 0..) |ev, i| {
            if (ev != ev_type) continue;
            if (T == void) return i;

            const ev_payload: Payload = ev.payload(ev_type);
            if (@typeInfo(Payload) == .Struct) {
                inline for (@typeInfo(T).Struct.fields) |field| {
                    const name = field.name;
                    if (!@hasField(Payload, name))
                        @compileError(@typeName(T) ++ "has no field named " ++ name);
                    if (@field(ev_payload, name) != @field(match_content, name))
                        continue :ev_loop;
                }
            } else {
                if (ev_payload != match_content) continue :ev_loop;
            }
            return i;
        }

        return null;
    }

    pub fn append(self: *EventQueue, new: InputEvent) !void {
        try self.events.append(new);
    }

    pub fn clearRetainingCapacity(self: *EventQueue) void {
        self.events.clearRetainingCapacity();
        self.iter_idx = null;
    }
};

pub const CursorType = enum {
    arrow,
    ibeam,
    crosshair,
    hand,
    hresize,
    vresize,
};

pub const GlfwError = error{
    NotInitialized,
    NoCurrentContext,
    InvalidEnum,
    InvalidValue,
    OutOfMemory,
    ApiUnavailable,
    VersionUnavailable,
    PlatformError,
    FormatUnavailable,
    NoWindowContext,
};

pub fn returnGlfwError() GlfwError!void {
    const error_code = c.glfwGetError(null);
    return switch (error_code) {
        c.GLFW_NO_ERROR => {},
        c.GLFW_NOT_INITIALIZED => GlfwError.NotInitialized,
        c.GLFW_NO_CURRENT_CONTEXT => GlfwError.NoCurrentContext,
        c.GLFW_INVALID_ENUM => GlfwError.InvalidEnum,
        c.GLFW_INVALID_VALUE => GlfwError.InvalidValue,
        c.GLFW_OUT_OF_MEMORY => GlfwError.OutOfMemory,
        c.GLFW_API_UNAVAILABLE => GlfwError.ApiUnavailable,
        c.GLFW_VERSION_UNAVAILABLE => GlfwError.VersionUnavailable,
        c.GLFW_PLATFORM_ERROR => GlfwError.PlatformError,
        c.GLFW_FORMAT_UNAVAILABLE => GlfwError.FormatUnavailable,
        c.GLFW_NO_WINDOW_CONTEXT => GlfwError.NoWindowContext,
        else => std.debug.panic("unknown glfw error code 0x{x}\n", .{error_code}),
    };
}

fn get_proc_address_fn(window: ?*c.GLFWwindow, proc_name: [:0]const u8) ?*const anyopaque {
    _ = window;
    const fn_ptr = c.glfwGetProcAddress(proc_name.ptr);
    // without this I got a "cast discards const qualifier" error
    return @as(?*const anyopaque, @ptrFromInt(@intFromPtr(fn_ptr)));
}

fn gl_error_callback(
    source: u32,
    error_type: u32,
    id: u32,
    severity: u32,
    len: i32,
    msg: [*:0]const u8,
    user_param: ?*const anyopaque,
) callconv(.C) void {
    _ = len;
    _ = user_param;

    if (severity == gl.DEBUG_SEVERITY_NOTIFICATION) return;

    const source_str = switch (source) {
        0x824B => "SOURCE_OTHER",
        0x824A => "SOURCE_APPLICATION",
        0x8249 => "SOURCE_THIRD_PARTY",
        0x8248 => "SOURCE_SHADER_COMPILER",
        0x8247 => "SOURCE_WINDOW_SYSTEM",
        0x8246 => "SOURCE_API",
        else => unreachable,
    };
    const error_type_str = switch (error_type) {
        0x826A => "TYPE_POP_GROUP",
        0x8269 => "TYPE_PUSH_GROUP",
        0x8268 => "TYPE_MARKER",
        0x8251 => "TYPE_OTHER",
        0x8250 => "TYPE_PERFORMANCE",
        0x824F => "TYPE_PORTABILITY",
        0x824E => "TYPE_UNDEFINED_BEHAVIOR",
        0x824D => "TYPE_DEPRECATED_BEHAVIOR",
        0x824C => "TYPE_ERROR",
        else => unreachable,
    };
    const severity_str = switch (severity) {
        0x826B => "SEVERITY_NOTIFICATION",
        0x9148 => "SEVERITY_LOW",
        0x9147 => "SEVERITY_MEDIUM",
        0x9146 => "SEVERITY_HIGH",
        else => unreachable,
    };

    std.debug.print("OpenGL: ({s}, {s}, {s}, id={}) {s}\n", .{ source_str, severity_str, error_type_str, id, msg });
}
