const std = @import("std");
const Allocator = std.mem.Allocator;

const c = @import("c.zig");

pub const TrackingTimer = struct {
    arena: std.heap.ArenaAllocator,
    buckets: [num_buckets]std.ArrayList(Map),

    pub const Map = struct { key_hash: u64, value_ptr: *Timer };

    pub const num_buckets = 23; // prime number maybe is better?

    const deltas_stored: usize = 100;

    const Timer = struct {
        name: []const u8,
        is_timing: bool,
        start_time: f64,
        total_time: f64,
        times_called: u64,

        deltas: [deltas_stored]f64,
        average_time: f64,
    };

    /// call 'deinit' to clean up resources
    pub fn init(allocator: Allocator) TrackingTimer {
        var self = @as(TrackingTimer, undefined);
        self.arena = std.heap.ArenaAllocator.init(allocator);
        for (self.buckets) |*bucket| {
            bucket.* = std.ArrayList(Map).initCapacity(allocator, 10) catch unreachable;
        }
        return self;
    }

    pub fn deinit(self: *TrackingTimer) void {
        self.arena.deinit();
        for (self.buckets) |*bucket| bucket.deinit();
    }

    pub fn start(self: *TrackingTimer, comptime id: []const u8) void {
        const key_hash = comptime std.hash_map.hashString(id);
        const entry = self.getOrPut(key_hash) catch unreachable;
        // new entry, need to initialize it
        if (!entry.found_existing) {
            entry.value_ptr.* = .{
                .name = id,
                .is_timing = false,
                .start_time = 0,
                .total_time = 0,
                .times_called = 0,
                .deltas = [_]f64{0} ** deltas_stored,
                .average_time = 0,
            };
        }

        entry.value_ptr.start_time = c.glfwGetTime();
        entry.value_ptr.is_timing = true;
    }

    pub fn stop(self: *TrackingTimer, comptime id: []const u8) void {
        const key_hash = comptime std.hash_map.hashString(id);
        const entry = self.getOrPut(key_hash) catch unreachable;
        if (!entry.found_existing) unreachable;

        const timer_ptr = entry.value_ptr;
        if (!timer_ptr.is_timing) unreachable;
        const delta = c.glfwGetTime() - timer_ptr.start_time;
        timer_ptr.total_time += delta;
        timer_ptr.times_called += 1;
        timer_ptr.is_timing = false;

        timer_ptr.deltas[timer_ptr.times_called % deltas_stored] = delta;
        var delta_sum: f64 = 0;
        for (timer_ptr.deltas) |dt| delta_sum += dt;
        timer_ptr.average_time = delta_sum / @intToFloat(f64, deltas_stored);
    }

    pub const Iterator = struct {
        context: TrackingTimer,
        bucket_idx: usize,
        map_idx: usize,

        pub const Entry = struct { key_ptr: *[]const u8, value_ptr: *Timer };

        pub fn next(self: *Iterator) ?Entry {
            var bucket = self.context.buckets[self.bucket_idx];
            while (bucket.items.len == 0) {
                self.bucket_idx += 1;
                self.map_idx = 0;
                if (self.bucket_idx >= num_buckets) return null;
                bucket = self.context.buckets[self.bucket_idx];
            }

            const timer_ptr = bucket.items[self.map_idx].value_ptr;
            const entry = Entry{ .key_ptr = &timer_ptr.name, .value_ptr = timer_ptr };

            self.map_idx += 1;
            if (self.map_idx >= bucket.items.len) {
                self.bucket_idx += 1;
                self.map_idx = 0;
            }
            if (self.bucket_idx >= num_buckets) return null;

            return entry;
        }
    };

    pub fn iterator(self: TrackingTimer) Iterator {
        return .{ .context = self, .bucket_idx = 0, .map_idx = 0 };
    }

    const GetOrPut = struct { found_existing: bool, value_ptr: *Timer };

    fn getOrPut(self: *TrackingTimer, key_hash: u64) !GetOrPut {
        var bucket = &self.buckets[key_hash % num_buckets];
        for (bucket.items) |map| {
            if (map.key_hash == key_hash) {
                return GetOrPut{ .found_existing = true, .value_ptr = map.value_ptr };
            }
        }
        const value_ptr = try self.arena.allocator().create(Timer);
        try bucket.append(.{ .key_hash = key_hash, .value_ptr = value_ptr });
        return GetOrPut{ .found_existing = false, .value_ptr = value_ptr };
    }
};
