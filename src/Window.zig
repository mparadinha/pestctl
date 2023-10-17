const std = @import("std");
const Allocator = std.mem.Allocator;
const gl = @import("gl_4v3.zig");
const math = @import("math.zig");
const vec2 = math.vec2;
const uvec2 = math.uvec2;
const glfw = @import("mach-glfw");

const Window = @This();

window: glfw.Window,
event_queue: EventQueue,
cursors: struct {
    arrow: glfw.Cursor,
    ibeam: glfw.Cursor,
    crosshair: glfw.Cursor,
    pointing_hand: glfw.Cursor,
    hresize: glfw.Cursor,
    vresize: glfw.Cursor,
},

pub const Cursor = enum {
    arrow,
    ibeam,
    crosshair,
    pointing_hand,
    hresize,
    vresize,
};

pub const InitError = glfw.ErrorCode || gl.LoadError;

/// call 'finish_setup' right after 'init'
/// call 'deinit' to clean up resources
pub fn init(allocator: Allocator, width: u32, height: u32, title: []const u8) InitError!Window {
    _ = glfw.setErrorCallback(glfw_error_callback); // returns previous callback

    if (!glfw.init(.{})) return glfw.mustGetErrorCode();

    const window = glfw.Window.create(
        width,
        height,
        @ptrCast(title),
        null,
        null,
        .{
            .context_version_major = 3,
            .context_version_minor = 3,
            .opengl_profile = .opengl_core_profile,
            .opengl_forward_compat = true,
            .transparent_framebuffer = true,
            // .glfw_samples = 4,
        },
    ) orelse return glfw.mustGetErrorCode();

    glfw.makeContextCurrent(window);
    glfw.swapInterval(1);

    // setup callbacks
    // these all return the previous callback, or NULL if there was none
    _ = window.setKeyCallback(key_callback);
    _ = window.setMouseButtonCallback(mouse_button_callback);
    _ = window.setScrollCallback(scroll_callback);
    _ = window.setCharCallback(char_callback);

    // load OpenGL now
    try gl.load({}, get_proc_address_fn);
    gl.enable(gl.DEBUG_OUTPUT);
    gl.debugMessageCallback(gl_error_callback, null);

    return Window{
        .window = window,
        .event_queue = EventQueue.init(allocator),
        .cursors = .{
            .arrow = glfw.Cursor.createStandard(.arrow).?,
            .ibeam = glfw.Cursor.createStandard(.ibeam).?,
            .crosshair = glfw.Cursor.createStandard(.crosshair).?,
            .pointing_hand = glfw.Cursor.createStandard(.pointing_hand).?,
            .hresize = glfw.Cursor.createStandard(.resize_ew).?,
            .vresize = glfw.Cursor.createStandard(.resize_ns).?,
        },
    };
}

pub fn deinit(self: *Window) void {
    self.cursors.arrow.destroy();
    self.cursors.ibeam.destroy();
    self.cursors.crosshair.destroy();
    self.cursors.pointing_hand.destroy();
    self.cursors.hresize.destroy();
    self.cursors.vresize.destroy();

    self.window.destroy();
    glfw.terminate();

    self.event_queue.deinit();
}

pub fn finishSetup(self: *Window) void {
    self.window.setUserPointer(self);
}

pub fn shouldClose(self: *Window) bool {
    return self.window.shouldClose();
}

pub fn update(self: *Window) void {
    // discard unprocessed events from the last frame because if they were needed they would've been processed
    self.event_queue.clearRetainingCapacity();

    self.window.swapBuffers();
    glfw.pollEvents();
}

pub fn getFramebufferSize(self: *Window) uvec2 {
    const size = self.window.getFramebufferSize();
    return uvec2{ size.width, size.height };
}

pub fn toggleCursorMode(self: *Window) void {
    const current_mode = self.window.getInputModeCursor();
    self.window.setInputMode(switch (current_mode) {
        .normal => glfw.Window.Input.disabled,
        .hidden => glfw.Window.Input.normal,
        .disabled => glfw.Window.Input.normal,
        else => unreachable,
    });
}

pub const CursorMode = enum { normal, infinite };
pub fn setCursorMode(self: *Window, mode: CursorMode) void {
    self.window.setInputMode(switch (mode) {
        .normal => glfw.InputModeCursor.normal,
        .infinite => glfw.InputModeCursor.disabled,
    });
}

pub fn setCursor(self: *Window, cursor_type: Cursor) void {
    self.window.setCursor(switch (cursor_type) {
        .arrow => self.cursors.arrow,
        .ibeam => self.cursors.ibeam,
        .crosshair => self.cursors.crosshair,
        .pointing_hand => self.cursors.pointing_hand,
        .hresize => self.cursors.hresize,
        .vresize => self.cursors.vresize,
    });
}

pub fn keyPressed(self: Window, key: glfw.Key) bool {
    return self.window.getKey(key) == .press;
}

pub fn getModifiers(self: Window) InputEvent.Modifiers {
    return .{
        .shift = self.keyPressed(.left_shift) or self.keyPressed(.right_shift),
        .control = self.keyPressed(.left_control) or self.keyPressed(.right_control),
        .alt = self.keyPressed(.left_alt) or self.keyPressed(.right_alt),
        .super = self.keyPressed(.left_super) or self.keyPressed(.right_super),
        // TODO: these two need their state to be preserved, this is probably wrong
        // or maybe we use `glfwSetInputMode(window, GLFW_LOCK_KEY_MODS, GLFW_TRUE)`
        // TODO: check these two!
        .caps_lock = self.keyPressed(.caps_lock),
        .num_lock = self.keyPressed(.num_lock),
    };
}

pub fn getMousePos(self: *Window) vec2 {
    const cursor = self.window.getCursorPos();
    const mouse_pos = vec2{
        @as(f32, @floatCast(cursor.xpos)),
        @as(f32, @floatCast(cursor.ypos)),
    };
    const framebuffer_size = self.getFramebufferSize();

    return vec2{
        mouse_pos[0],
        @as(f32, @floatFromInt(framebuffer_size[1])) - mouse_pos[1],
    };
}

fn glfw_error_callback(error_code: glfw.ErrorCode, error_msg: [:0]const u8) void {
    std.debug.print("glfw error (code={}): {s}", .{ error_code, error_msg });
}

fn modifiersFromGLFW(mods: glfw.Mods) InputEvent.Modifiers {
    return .{
        .shift = mods.shift,
        .control = mods.control,
        .alt = mods.alt,
        .super = mods.super,
        .caps_lock = mods.caps_lock,
        .num_lock = mods.num_lock,
    };
}

fn key_callback(window: glfw.Window, key: glfw.Key, scancode: i32, action: glfw.Action, mods: glfw.Mods) void {
    const self: *Window = window.getUserPointer(Window).?;

    if (key == .unknown) return;
    const key_ev = InputEvent.KeyEvent{ .key = key, .mods = modifiersFromGLFW(mods) };
    if (action == .press)
        self.event_queue.append(.{ .KeyDown = key_ev }) catch unreachable;
    if (action == .release)
        self.event_queue.append(.{ .KeyUp = key_ev }) catch unreachable;
    if (action == .repeat)
        self.event_queue.append(.{ .KeyRepeat = key_ev }) catch unreachable;

    _ = scancode;
}

fn mouse_button_callback(window: glfw.Window, button: glfw.MouseButton, action: glfw.Action, mods: glfw.Mods) void {
    const self: *Window = window.getUserPointer(Window).?;

    const btn_ev = InputEvent.MouseButtonEvent{ .button = button, .mods = modifiersFromGLFW(mods) };
    if (action == .press)
        self.event_queue.append(.{ .MouseDown = btn_ev }) catch unreachable;
    if (action == .release)
        self.event_queue.append(.{ .MouseUp = btn_ev }) catch unreachable;
}

fn scroll_callback(window: glfw.Window, xoffset: f64, yoffset: f64) void {
    const self: *Window = window.getUserPointer(Window).?;
    self.event_queue.append(.{ .MouseScroll = .{
        .x = @as(f32, @floatCast(xoffset)),
        .y = @as(f32, @floatCast(yoffset)),
        .mods = self.getModifiers(),
    } }) catch unreachable;
}

fn char_callback(window: glfw.Window, codepoint: u21) void {
    const self: *Window = window.getUserPointer(Window).?;
    self.event_queue.append(.{ .Char = codepoint }) catch unreachable;
}

pub const InputEvent = union(enum) {
    KeyUp: KeyEvent,
    KeyDown: KeyEvent,
    KeyRepeat: KeyEvent,
    MouseUp: MouseButtonEvent,
    MouseDown: MouseButtonEvent,
    MouseScroll: struct { x: f32, y: f32, mods: Modifiers },
    GamepadUp: usize,
    GamepadDown: usize,
    Char: u32, // unicode codepoint

    pub const KeyEvent = struct {
        key: glfw.Key,
        mods: Modifiers,
    };

    pub const MouseButtonEvent = struct {
        button: glfw.MouseButton,
        mods: Modifiers,
    };

    // TODO: handle both left & right alt/shift/control
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
            inline else => @field(self, @tagName(T)),
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

fn get_proc_address_fn(_: void, proc_name: [:0]const u8) ?*const anyopaque {
    return glfw.getProcAddress(proc_name.ptr);
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
