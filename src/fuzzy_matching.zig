const std = @import("std");
const Allocator = std.mem.Allocator;

/// Type requirements:
/// * Iterator has `fn reset(*Iterator, query: []const u8) !void`
/// * Iterator has `fn next(*Iterator, Allocator) !?struct { str: []const u8, ctx: Context}`
/// * Iterator has `Context` subtype
pub fn FuzzyMatcher(comptime Iterator: type) type {
    // TODO: assert the type requirements
    //       (for the funcs simply having the ret. type be `void` instead of `!void` should
    //       be allowed)

    return struct {
        allocator: Allocator,
        query: []const u8,
        top_matches: std.BoundedArray(Match, 20),
        // TODO: make top_entries size configurable
        // TODO: add option to keep all scores, so users can scroll through all of the results
        scoring_matrix: []f32,
        search_iterator: *Iterator,

        worker_thread: ?std.Thread,
        mutex: std.Thread.Mutex,
        worker_should_stop: std.atomic.Value(bool),

        const Self = @This();

        pub const Context = Iterator.Context;

        pub const Match = struct {
            score: f32,
            matched_str: []const u8,
            ctx: Context,

            pub fn free(self: Match, allocator: Allocator) void {
                allocator.free(self.matched_str);
            }
        };

        pub fn init(allocator: Allocator, iterator: *Iterator) Self {
            return .{
                .allocator = allocator,
                .query = &[0]u8{},
                .top_matches = .{},
                .scoring_matrix = &[0]f32{},
                .search_iterator = iterator,
                .worker_thread = null,
                .mutex = .{},
                .worker_should_stop = std.atomic.Value(bool).init(false),
            };
        }

        pub fn deinit(self: *Self) void {
            self.stopWorkerThread();
            if (self.query.len != 0) self.allocator.free(self.query);
            if (self.scoring_matrix.len != 0) self.allocator.free(self.scoring_matrix);
            for (self.top_matches.slice()) |entry| entry.free(self.allocator);
        }

        pub fn updateSearch(self: *Self, query_str: []const u8) !void {
            if (query_str.len == 0 and self.query.len == 0) return;
            if (std.mem.eql(u8, query_str, self.query)) return;

            self.stopWorkerThread();
            {
                if (self.query.len != 0) self.allocator.free(self.query);
                self.query = try self.allocator.dupe(u8, query_str);

                for (self.top_matches.slice()) |entry| entry.free(self.allocator);
                try self.top_matches.resize(0);

                try self.search_iterator.reset(self.query);
            }
            self.worker_thread = try std.Thread.spawn(.{}, Self.workFn, .{self});
        }

        fn stopWorkerThread(self: *Self) void {
            if (self.worker_thread) |thread| {
                self.worker_should_stop.store(true, .monotonic);
                thread.join();
            }
            self.worker_should_stop.store(false, .monotonic);
        }

        pub fn workFn(self: *Self) !void {
            const start = std.time.nanoTimestamp();
            if (self.query.len == 0) return;
            search_loop: while (!self.worker_should_stop.load(.monotonic)) {
                const next_target = target: {
                    self.mutex.lock();
                    defer self.mutex.unlock();
                    break :target try self.search_iterator.next() orelse break :search_loop;
                };
                const target = next_target.str;

                const match_score = score: {
                    if (self.query.len == 0 or target.len == 0) break :score 0;
                    const matrix_size = self.query.len * target.len;
                    if (self.scoring_matrix.len < matrix_size) {
                        if (self.scoring_matrix.len != 0) self.allocator.free(self.scoring_matrix);
                        self.scoring_matrix = try self.allocator.alloc(f32, matrix_size);
                    }
                    const scores = self.scoring_matrix[0..matrix_size];
                    break :score calculateScore(scores, self.query, target);
                };

                self.mutex.lock();
                defer self.mutex.unlock();

                var smallest_score: f32 = std.math.floatMax(f32);
                var smallest_idx: usize = 0;
                for (self.top_matches.slice(), 0..) |entry, idx| {
                    if (entry.score < smallest_score) {
                        smallest_score = entry.score;
                        smallest_idx = idx;
                    }
                }
                if (match_score < smallest_score and self.top_matches.len == self.top_matches.capacity())
                    continue;

                const match = Match{
                    .score = match_score,
                    .matched_str = try self.allocator.dupe(u8, target),
                    .ctx = next_target.ctx,
                };
                self.top_matches.append(match) catch {
                    self.top_matches.buffer[smallest_idx].free(self.allocator);
                    self.top_matches.buffer[smallest_idx] = match;
                };
                std.sort.insertion(Match, self.top_matches.slice(), {}, struct {
                    pub fn lessThanFn(_: void, lhs: Match, rhs: Match) bool {
                        return lhs.score > rhs.score; // we want descending order
                    }
                }.lessThanFn);
            }
            std.debug.print("worker thread is done. took {d}ms\n", .{
                @divFloor((std.time.nanoTimestamp() - start), std.time.ns_per_ms),
            });
        }
    };
}

pub fn calculateScoreAlloc(allocator: Allocator, query: []const u8, target: []const u8) !f32 {
    const scores = try allocator.alloc(f32, query.len * target.len);
    defer allocator.free(scores);
    return calculateScore(scores, query, target);
}

/// uses the Smith-Watterman algorithm
pub fn calculateScore(scores: []f32, query: []const u8, target: []const u8) f32 {
    std.debug.assert(scores.len == query.len * target.len);

    // TODO: make this faster, maybe using this implementation:
    // https://sci-hub.hkvisa.net/10.1093/bioinformatics/btl582
    // measure first though! is this even the bottleneck?

    for (scores, 0..) |*value, idx| {
        const up_cell = if (idx < query.len) 0 else scores[idx - query.len];
        const left_cell = if (idx % query.len == 0) 0 else scores[idx - 1];
        const diag_cell = if (idx < query.len or idx % query.len == 0) 0 else scores[idx - query.len - 1];

        const matches = query[idx % query.len] == target[idx / query.len];
        const half_matches = std.ascii.toLower(query[idx % query.len]) == std.ascii.toLower(target[idx / query.len]);
        const match_score: f32 = 1;
        const gap_penalty: f32 = 1;
        const up_score = if (up_cell == 0) 0 else up_cell - gap_penalty;
        const left_score = if (left_cell == 0) 0 else left_cell - gap_penalty;
        const diag_score = if (matches)
            diag_cell + match_score
        else if (half_matches)
            diag_cell + match_score / 2
        else if (diag_cell == 0)
            0
        else
            diag_cell - match_score;
        value.* = @max(diag_score, up_score, left_score);
    }

    return std.mem.max(f32, scores);
}

/// higher score means better match
pub fn fuzzyScore(pattern: []const u8, test_str: []const u8) f32 {
    var score: f32 = 0;

    const to_lower_lut = comptime lut: {
        var table: [256]u8 = undefined;
        for (&table, 0..) |*entry, char| {
            entry.* = if ('A' <= char and char <= 'Z') char + 32 else char;
        }
        break :lut table;
    };

    for (pattern, 0..) |pat_char, pat_idx| {
        for (test_str, 0..) |test_char, test_idx| {
            var char_score: f32 = 0;
            const case_sensitive_match = (pat_char == test_char);
            const case_insensitive_match = to_lower_lut[pat_char] == to_lower_lut[test_char];
            if (!case_insensitive_match) continue;
            char_score += 0.5;
            if (case_sensitive_match) char_score += 1;
            if (test_idx == pat_idx) char_score *= 5;
            score += char_score;
        }
    }

    if (std.mem.indexOf(u8, test_str, pattern)) |idx| score = (score * 5) - @as(f32, @floatFromInt(idx));

    return score;
}
