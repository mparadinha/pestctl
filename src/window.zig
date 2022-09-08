const std = @import("std");
const Allocator = std.mem.Allocator;
const gl = @import("gl_4v3.zig");
const c = @import("c.zig");
const math = @import("math.zig");
const vec2 = math.vec2;

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
        for (self.events.items) |ev, i| {
            if (std.meta.activeTag(ev) != ev_type) continue;
            if (match_ev_content) |match_ev| {
                if (!std.meta.eql(match_ev, ev.payload(ev_type))) continue;
            }
            return i;
        }
        return null;
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

        ev_loop: for (self.events.items) |ev, i| {
            if (std.meta.activeTag(ev) != ev_type) continue;
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

pub const Window = struct {
    handle: *c.GLFWwindow,

    event_queue: EventQueue,

    gamepads: [c.GLFW_JOYSTICK_LAST + 1]Gamepad,
    active_gamepad_idx: ?usize,
    // we save the button states on the active gamepad to create input events for them
    last_update_gamepad_buttons: [c.GLFW_GAMEPAD_BUTTON_LAST + 1]u8 =
        [_]u8{0} ** (c.GLFW_GAMEPAD_BUTTON_LAST + 1),

    cursors: struct {
        arrow: *c.GLFWcursor,
        ibeam: *c.GLFWcursor,
        crosshair: *c.GLFWcursor,
        hand: *c.GLFWcursor,
        hresize: *c.GLFWcursor,
        vresize: *c.GLFWcursor,
    },

    /// call 'finish_setup' right after 'init'
    /// call 'deinit' to clean up resources
    pub fn init(allocator: Allocator, width: u32, height: u32, title: []const u8) Window {
        _ = c.glfwSetErrorCallback(glfw_error_callback); // returns previous callback

        if (c.glfwInit() != c.GLFW_TRUE) unreachable;

        c.glfwWindowHint(c.GLFW_CONTEXT_VERSION_MAJOR, 3);
        c.glfwWindowHint(c.GLFW_CONTEXT_VERSION_MINOR, 3);
        c.glfwWindowHint(c.GLFW_OPENGL_PROFILE, c.GLFW_OPENGL_CORE_PROFILE);
        c.glfwWindowHint(c.GLFW_OPENGL_FORWARD_COMPAT, gl.TRUE);
        //c.glfwWindowHint(c.GLFW_SAMPLES, 4);
        const handle = c.glfwCreateWindow(
            @intCast(c_int, width),
            @intCast(c_int, height),
            @ptrCast([*c]const u8, title),
            null,
            null,
        ) orelse unreachable;

        c.glfwMakeContextCurrent(handle);
        c.glfwSwapInterval(1);
        //c.glfwSwapInterval(0);

        var self = Window{
            .handle = handle,
            .event_queue = EventQueue.init(allocator),
            .gamepads = undefined,
            .active_gamepad_idx = null,
            .cursors = .{
                .arrow = c.glfwCreateStandardCursor(c.GLFW_ARROW_CURSOR).?,
                .ibeam = c.glfwCreateStandardCursor(c.GLFW_IBEAM_CURSOR).?,
                .crosshair = c.glfwCreateStandardCursor(c.GLFW_CROSSHAIR_CURSOR).?,
                .hand = c.glfwCreateStandardCursor(c.GLFW_HAND_CURSOR).?,
                .hresize = c.glfwCreateStandardCursor(c.GLFW_HRESIZE_CURSOR).?,
                .vresize = c.glfwCreateStandardCursor(c.GLFW_VRESIZE_CURSOR).?,
            },
        };

        // setup callbacks
        // these all return the previous callback, or NULL if there was none
        _ = c.glfwSetKeyCallback(self.handle, glfw_key_callback);
        _ = c.glfwSetMouseButtonCallback(self.handle, glfw_mouse_button_callback);
        _ = c.glfwSetCursorPosCallback(self.handle, glfw_cursor_pos_callback);
        _ = c.glfwSetScrollCallback(self.handle, glfw_scroll_callback);
        _ = c.glfwSetCharCallback(self.handle, glfw_char_callback);

        // setup gamepads
        for (self.gamepads) |*pad, i| {
            const id = @intCast(i32, i);
            pad.id = id;
            pad.connected = c.glfwJoystickIsGamepad(id) == c.GLFW_TRUE;
            if (pad.connected) pad.name = std.mem.span(c.glfwGetGamepadName(id));
        }
        self.update_gamepads();

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

    pub fn finish_setup(self: *Window) void {
        c.glfwSetWindowUserPointer(self.handle, self);
    }

    pub fn should_close(self: *Window) bool {
        return c.glfwWindowShouldClose(self.handle) == c.GLFW_TRUE;
    }

    pub fn update(self: *Window) void {
        // discard unprocessed events from the last frame because if they were needed they would've been processed
        self.event_queue.clearRetainingCapacity();

        c.glfwSwapBuffers(self.handle);
        c.glfwPollEvents();

        self.update_gamepads();

        // create event for gamepad buttons
        if (self.active_gamepad_idx) |pad_idx| {
            const gamepad = self.gamepads[pad_idx];
            const pad_buttons = gamepad.state.buttons;
            for (pad_buttons) |state, button_id| {
                const last_state = self.last_update_gamepad_buttons[button_id];
                if (last_state == c.GLFW_PRESS and state == c.GLFW_RELEASE) {
                    self.event_queue.append(.{ .GamepadUp = button_id }) catch unreachable;
                }
                if (last_state == c.GLFW_RELEASE and state == c.GLFW_PRESS) {
                    self.event_queue.append(.{ .GamepadDown = button_id }) catch unreachable;
                }
            }
            self.last_update_gamepad_buttons = pad_buttons;
        }
    }

    /// updates gamepad axes and buttons and keeps track of which gamepad is the active one
    /// if one is available
    fn update_gamepads(self: *Window) void {
        var first_active: ?usize = null;
        for (self.gamepads) |*pad| {
            if (c.glfwJoystickIsGamepad(pad.id) == c.GLFW_TRUE) {
                _ = c.glfwGetGamepadState(pad.id, &pad.state);
                if (first_active == null) first_active = @intCast(usize, pad.id);
            } else {
                pad.state.buttons = [_]u8{0} ** (c.GLFW_GAMEPAD_BUTTON_LAST + 1);
                pad.state.axes = [_]f32{0} ** (c.GLFW_GAMEPAD_AXIS_LAST + 1);
                if (self.active_gamepad_idx == @intCast(usize, pad.id))
                    self.active_gamepad_idx = null;
            }
        }
        if (self.active_gamepad_idx == null) self.active_gamepad_idx = first_active;
    }

    pub fn framebuffer_size(self: Window, width: *u32, height: *u32) void {
        c.glfwGetFramebufferSize(self.handle, @ptrCast(*i32, width), @ptrCast(*i32, height));
    }

    pub fn toggle_cursor_mode(self: *Window) void {
        const current_mode = c.glfwGetInputMode(self.handle, c.GLFW_CURSOR);
        const new_mode = switch (current_mode) {
            c.GLFW_CURSOR_NORMAL => c.GLFW_CURSOR_DISABLED,
            c.GLFW_CURSOR_HIDDEN => c.GLFW_CURSOR_NORMAL,
            c.GLFW_CURSOR_DISABLED => c.GLFW_CURSOR_NORMAL,
            else => unreachable,
        };
        c.glfwSetInputMode(self.handle, c.GLFW_CURSOR, new_mode);
    }

    pub fn set_cursor(self: *Window, cursor_type: CursorType) void {
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

    /// if there's no active gamepad returns a Gamepad with everything zero'ed out
    // TODO: this should prob return null if theres not active gamepad
    pub fn active_gamepad(self: Window) Gamepad {
        if (self.active_gamepad_idx) |idx| return self.gamepads[idx];

        return Gamepad{
            .id = undefined,
            .name = "",
            .connected = false, // TODO: should this fake gamepad pretend to be connected?
            .state = .{
                .buttons = [_]u8{0} ** (c.GLFW_GAMEPAD_BUTTON_LAST + 1),
                .axes = [_]f32{0} ** (c.GLFW_GAMEPAD_AXIS_LAST + 1),
            },
        };
    }

    pub fn key_pressed(self: *Window, key: i32) bool {
        return c.glfwGetKey(self.handle, key) == c.GLFW_PRESS;
    }

    /// +1 if `pos_key_id` is pressed, -1 if `neg_key_id` is pressed or 0 if both are
    pub fn key_pair(self: *Window, pos_key_id: i32, neg_key_id: i32) f32 {
        return (if (self.key_pressed(pos_key_id)) @as(f32, 1) else @as(f32, 0)) +
            (if (self.key_pressed(neg_key_id)) @as(f32, -1) else @as(f32, 0));
    }

    pub fn get_modifiers(self: *Window) InputEvent.Modifiers {
        return .{
            .shift = self.key_pressed(c.GLFW_KEY_LEFT_SHIFT) or
                self.key_pressed(c.GLFW_KEY_RIGHT_SHIFT),
            .control = self.key_pressed(c.GLFW_KEY_LEFT_CONTROL) or
                self.key_pressed(c.GLFW_KEY_RIGHT_CONTROL),
            .alt = self.key_pressed(c.GLFW_KEY_LEFT_ALT) or
                self.key_pressed(c.GLFW_KEY_RIGHT_ALT),
            .super = self.key_pressed(c.GLFW_KEY_LEFT_SUPER) or
                self.key_pressed(c.GLFW_KEY_RIGHT_SUPER),
            // TODO: these two need their state to be preserved, this is probably wrong
            // TODO: check these two!
            .caps_lock = self.key_pressed(c.GLFW_KEY_CAPS_LOCK),
            .num_lock = self.key_pressed(c.GLFW_KEY_NUM_LOCK),
        };
    }

    pub fn mouse_pos(self: Window) vec2 {
        var xpos: f64 = undefined;
        var ypos: f64 = undefined;
        c.glfwGetCursorPos(self.handle, &xpos, &ypos);
        var width: u32 = undefined;
        var height: u32 = undefined;
        self.framebuffer_size(&width, &height);
        return vec2{ @floatCast(f32, xpos), @intToFloat(f32, height) - @floatCast(f32, ypos) };
    }

    pub fn mouse_pos_ndc(self: Window) vec2 {
        var xpos: f64 = undefined;
        var ypos: f64 = undefined;
        c.glfwGetCursorPos(self.handle, &xpos, &ypos);
        var width: u32 = undefined;
        var height: u32 = undefined;
        self.framebuffer_size(&width, &height);
        return vec2{
            (2 * (xpos / @intToFloat(f32, width)) - 1),
            -(2 * (ypos / @intToFloat(f32, height)) - 1),
        };
    }

    fn glfw_error_callback(error_code: c_int, error_msg: [*c]const u8) callconv(.C) void {
        std.log.info("glfw error (code={}): {s}", .{ error_code, error_msg });
    }

    fn glfw_key_callback(glfw_window: ?*c.GLFWwindow, key: i32, scancode: i32, action: i32, mods: i32) callconv(.C) void {
        const self = @ptrCast(*Window, @alignCast(8, c.glfwGetWindowUserPointer(glfw_window).?));

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
        const self = @ptrCast(*Window, @alignCast(8, c.glfwGetWindowUserPointer(glfw_window).?));
        if (action == c.GLFW_PRESS)
            self.event_queue.append(.{ .MouseDown = button }) catch unreachable;
        if (action == c.GLFW_RELEASE)
            self.event_queue.append(.{ .MouseUp = button }) catch unreachable;
        _ = mods;
    }

    fn glfw_cursor_pos_callback(glfw_window: ?*c.GLFWwindow, xpos: f64, ypos: f64) callconv(.C) void {
        const self = @ptrCast(*Window, @alignCast(8, c.glfwGetWindowUserPointer(glfw_window).?));

        const held_mouse_button: ?i32 =
            if (c.glfwGetMouseButton(self.handle, c.GLFW_MOUSE_BUTTON_LEFT) == c.GLFW_TRUE)
            c.GLFW_MOUSE_BUTTON_LEFT
        else if (c.glfwGetMouseButton(self.handle, c.GLFW_MOUSE_BUTTON_RIGHT) == c.GLFW_TRUE)
            c.GLFW_MOUSE_BUTTON_RIGHT
        else
            null;

        if (held_mouse_button) |button| {
            self.event_queue.append(.{ .MouseDrag = .{
                .x = @floatCast(f32, xpos),
                .y = @floatCast(f32, ypos),
                .held_button = button,
            } }) catch unreachable;
        }
    }

    fn glfw_scroll_callback(glfw_window: ?*c.GLFWwindow, xoffset: f64, yoffset: f64) callconv(.C) void {
        const self = @ptrCast(*Window, @alignCast(8, c.glfwGetWindowUserPointer(glfw_window).?));
        self.event_queue.append(.{ .MouseScroll = .{
            .x = @floatCast(f32, xoffset),
            .y = @floatCast(f32, yoffset),
            .shift_held = self.key_pressed(c.GLFW_KEY_LEFT_SHIFT) or self.key_pressed(c.GLFW_KEY_RIGHT_SHIFT),
        } }) catch unreachable;
    }

    fn glfw_char_callback(glfw_window: ?*c.GLFWwindow, codepoint: u32) callconv(.C) void {
        const self = @ptrCast(*Window, @alignCast(8, c.glfwGetWindowUserPointer(glfw_window).?));
        self.event_queue.append(.{ .Char = codepoint }) catch unreachable;
    }
};

pub const Gamepad = struct {
    id: i32,
    name: []const u8,
    connected: bool,
    state: c.GLFWgamepadstate,

    const axes_dead_zone = 0.1;

    pub fn button_pressed(self: Gamepad, id: usize) bool {
        return self.state.buttons[id] == c.GLFW_PRESS;
    }

    pub fn stick(self: Gamepad, side: enum { left, right }) vec2 {
        var axis = switch (side) {
            .left => vec2{
                self.state.axes[c.GLFW_GAMEPAD_AXIS_LEFT_X],
                self.state.axes[c.GLFW_GAMEPAD_AXIS_LEFT_Y],
            },
            .right => vec2{
                self.state.axes[c.GLFW_GAMEPAD_AXIS_RIGHT_X],
                self.state.axes[c.GLFW_GAMEPAD_AXIS_RIGHT_Y],
            },
        };

        if (std.math.fabs(axis[0]) < Gamepad.axes_dead_zone) axis[0] = 0;
        if (std.math.fabs(axis[1]) < Gamepad.axes_dead_zone) axis[1] = 0;

        return axis;
    }
};
