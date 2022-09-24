const std = @import("std");
const Allocator = std.mem.Allocator;
const pid_t = std.os.pid_t;
const c = @import("c.zig");
const Elf = @import("Elf.zig");
const Dwarf = @import("Dwarf.zig");
const SrcLoc = Dwarf.SrcLoc;

const Session = @This();

allocator: Allocator,
exec_path: [:0]const u8,
pid: pid_t,
status: Status,
elf: Elf,

breakpoints: std.ArrayList(BreakPoint), // TODO: switch to linked list (removing items in the middle is needed, while iterating)
src_loc: ?SrcLoc,
addr_range: ?[2]u64,
call_stack: []CallFrame,
regs: Registers,
watched_vars: std.ArrayList(WatchedVar),

pub const Status = enum {
    Running,
    Stopped,
};

pub const CallFrame = struct {
    addr: usize,
    src: ?SrcLoc,
    fn_name: ?[]const u8,
};

const BreakPoint = struct {
    addr: usize,
    saved_byte: u8,
    only_once: bool = false,
};

pub const WatchedVar = struct {
    name: []const u8,
    rbp_offset: i32, // TODO: all other types of storage lmao
    value: union(enum) {
        Float: f32,
    },
};

pub fn init(allocator: Allocator, exec_path: []const u8) !Session {
    var self = Session{
        .allocator = allocator,
        .exec_path = try allocator.dupeZ(u8, exec_path),
        .pid = undefined,
        .status = .Stopped,
        .elf = try Elf.init(allocator, exec_path),
        .breakpoints = std.ArrayList(BreakPoint).init(allocator),
        .src_loc = null,
        .addr_range = null,
        .call_stack = &[0]CallFrame{},
        .regs = undefined,
        .watched_vars = std.ArrayList(WatchedVar).init(allocator),
    };

    try self.startTracing();

    return self;
}

pub fn deinit(self: *Session) void {
    self.kill();
    self.allocator.free(self.exec_path);
    self.elf.deinit();
    self.breakpoints.deinit();
    self.watched_vars.deinit();
    if (self.call_stack.len > 0) self.allocator.free(self.call_stack);
}

pub fn startTracing(self: *Session) !void {
    self.pid = try std.os.fork();
    if (self.pid == 0) {
        ptrace(.TRACEME, 0, {}) catch unreachable;
        const path_ptr = self.exec_path.ptr;
        const environ_ptr = std.os.environ.ptr;
        _ = std.os.linux.syscall3(.execve, @ptrToInt(path_ptr), 0, @ptrToInt(environ_ptr));
        unreachable;
    }

    // TODO: if the PTRACE_O_TRACEEXEC is not set we might need to manually raise
    // a SIGTRAP right after TRACEME
    // or maybe we can set the O_TRACEEXEC option for ourselves (i.e. the pestctl
    // process) since these options get inherited by children???

    // when using PTRACE_TRACEME the child gets a SIGTRAP before it starts
    // so we get a change to take control (unless PTRACE_O_TRACEEXEC is not
    // in effect, which I think is on by default)
    std.debug.assert((try getWaitStatus(self.pid)).stop_signal.? == .SIGTRAP);

    const opts =
        c.PTRACE.O.EXITKILL |
        //c.PTRACE.O.TRACECLONE | // this was causing some PTRACE_EVENT stuff I'll have to deal with eventually
        c.PTRACE.O.TRACEEXEC |
        c.PTRACE.O.TRACEEXIT;
    try ptrace(.SETOPTIONS, self.pid, opts);
}

pub fn pause(self: *Session) !void {
    if (self.status != .Running) return;
    try sendSignal(self.pid, .SIGSTOP);
    const wait_status = try getWaitStatus(self.pid);
    std.debug.print("[Session.pause] after sending SIGSTOP: {}\n", .{wait_status});
    self.status = .Stopped;
}

pub fn unpause(self: *Session) !void {
    if (self.status != .Stopped) return;

    // invalidate all the data that is only valid while .Stopped
    self.src_loc = null;
    self.addr_range = null;

    // if we're resuming from a breakpoint we have to reset it
    for (self.breakpoints.items) |*breakpt| {
        if (breakpt.addr == self.regs.rip) {
            // first run the instruction we're on, so we can modify it with `int3` for the breakpoint
            try self.stepInstructions(1);

            const saved = try self.insertByteAtAddr(breakpt.addr, 0xcc);
            if (saved != 0xcc) breakpt.saved_byte = saved;
        }
    }

    // TODO: when we send a SIGSTOP to stop the tracee and later do .CONT with
    // the 0 signal argument on it, we must check that we're not erasing any other
    // SIGSTOP that might have been sent to the tracee by some other process

    // note: doing a ptrace(.CONT) does produce a notification from waitpid
    try ptrace(.CONT, self.pid, 0);

    self.status = .Running;
}

pub fn kill(self: *Session) void {
    sendSignal(self.pid, .SIGKILL) catch unreachable;
}

/// checks if the child is stopped at a breakpoint (which we need to restore)
/// some breakpoints need to be re-setup (in case we want to hit them again)
pub fn fullUpdate(self: *Session) !void {
    if (self.status == .Running) return;

    self.regs = try self.getRegisters();
    self.src_loc = try self.elf.translateAddrToSrc(self.regs.rip);

    // calculate address range for the current src location
    if (self.src_loc) |src| {
        for (self.elf.dwarf.line_progs) |prog| {
            for (prog.files) |file, i| {
                if (std.mem.eql(u8, file.name, src.file) and
                    std.mem.eql(u8, prog.include_dirs[file.dir], src.dir))
                {
                    if (try prog.findAddrRangeForSrc(@intCast(u32, i), src.line)) |range| {
                        self.addr_range = range;
                    }
                    break;
                }
            }
        }
    }

    // if we're stopped at breakpoint, restore the clobbered byte and fix rip
    for (self.breakpoints.items) |*breakpt| {
        if (breakpt.addr == self.regs.rip - 1) {
            const cc = try self.insertByteAtAddr(breakpt.addr, breakpt.saved_byte);
            std.debug.assert(cc == 0xcc);
            var new_regs = self.regs;
            new_regs.rip -= 1;
            try self.setRegisters(new_regs);
        }
    }

    self.regs = try self.getRegisters();

    // reset all the breakpoints (except the one we just hit)
    for (self.breakpoints.items) |*breakpt| {
        if (breakpt.addr == self.regs.rip) continue;
        const saved = try self.insertByteAtAddr(breakpt.addr, 0xcc);
        if (saved != 0xcc) breakpt.saved_byte = saved;
    }

    // update watched vars
    for (self.watched_vars.items) |*var_info| {
        switch (var_info.value) {
            .Float => |*float| {
                //std.debug.print("rbp=0x{x:0>16}, offset={}\n", .{ self.regs.rbp, var_info.rbp_offset });
                const tmp = @intCast(isize, self.regs.rbp) + var_info.rbp_offset;
                if (tmp < 0) continue; // TODO
                const addr = @intCast(usize, tmp);
                const bytes = try self.getBytesAtAddr(addr, 4);
                float.* = @bitCast(f32, std.mem.readIntLittle(u32, &bytes));
                //std.debug.print("rbp_offset={}, addr=0x{x:0>16}, bytes={d}, float={d}\n", .{ var_info.rbp_offset, addr, bytes, float });
            },
        }
    }

    const proc_file = try self.procMemFile();
    defer proc_file.close();
    const call_stack = try self.elf.dwarf.callStackAddrs(self.allocator, self.regs.rip, self.regs, proc_file);
    defer self.allocator.free(call_stack);
    if (!(self.call_stack.len == call_stack.len and self.call_stack[0].addr == call_stack[0])) {
        if (self.call_stack.len > 0) self.allocator.free(self.call_stack);
        self.call_stack = try self.allocator.alloc(CallFrame, call_stack.len);
        for (call_stack) |addr, i| {
            var frame = CallFrame{ .addr = addr, .src = null, .fn_name = null };
            frame.src = try self.elf.translateAddrToSrcSpecial(addr);
            frame.fn_name = if (self.elf.findFunctionAtAddr(addr)) |func| func.name else null;
            self.call_stack[i] = frame;
        }
    }
}

pub fn setBreakpointAtAddr(self: *Session, addr: usize) !void {
    const was_running = (self.status == .Running);
    if (was_running) try self.pause();
    //defer if (was_running) self.unpause();

    std.debug.print("status={}\n", .{self.status});
    std.debug.print("rip=0x{x} when setting breakpoint\n", .{(try self.getRegisters()).rip});

    for (self.breakpoints.items) |breakpt| {
        if (breakpt.addr == addr) return;
    }

    const saved_byte = try self.insertByteAtAddr(addr, 0xcc);
    std.debug.print("setting breakpoint @ addr=0x{x}, saved_byte=0x{x}\n", .{ addr, saved_byte });
    try self.breakpoints.append(.{
        .addr = addr,
        .saved_byte = saved_byte,
    });
}

const Error = error{ NoAddrForSrc, VarNotFound };

pub fn setBreakpointAtSrc(self: *Session, src: SrcLoc) !void {
    const addr = (try self.findAddrForThisLineOrNextOne(src)) orelse return Error.NoAddrForSrc;
    try self.setBreakpointAtAddr(addr);
}

pub fn stepInstructions(self: *Session, num_instrs: usize) !void {
    if (self.status != .Stopped) return;

    var instrs_done: usize = 0;
    while (instrs_done < num_instrs) : (instrs_done += 1) {
        try ptrace(.SINGLESTEP, self.pid, {});
        std.debug.assert((try getWaitStatus(self.pid)).stop_signal.? == .SIGTRAP);
    }
}

/// note: this steps into functions
pub fn stepLine(self: *Session) !void {
    if (self.status != .Stopped) return;

    //var src = self.src_loc orelse return;
    var addr_range = self.addr_range orelse return;

    // because of loops and jumps backwards we can't just put a breakpoint
    // on the next line and be done with it. the only solution I can
    // think of right now is this (singlestepping until we hit a different
    // line) but it could, in some cases, freeze the UI for a while, if
    // it takes like 1k+ instructions to reach the next line.

    // TODO: a single src line can have multiple discontiguous address ranges

    var rip = (try self.getRegisters()).rip;
    while (addr_range[0] <= rip and rip < addr_range[1]) {
        try self.stepInstructions(1);
        rip = (try self.getRegisters()).rip;
    }

    while (true) {
        rip = (try self.getRegisters()).rip;
        if (try self.elf.translateAddrToSrc(rip)) |_| return;
        try self.stepInstructions(1);
    }
}

pub const MemMapInfo = struct {
    addr_range: [2]usize,
    perms: struct {
        read: bool,
        write: bool,
        execute: bool,
        shared: bool,
    },
    offset: usize,
    device: struct { major: u32, minor: u32 },
    inode: usize,
    path: []u8,

    pub fn deinit(self: MemMapInfo, allocator: Allocator) void {
        allocator.free(self.path);
    }
};

/// call `MemMapInfo.deinit` to cleanup resources 
pub fn getMemMapAtAddr(self: Session, allocator: Allocator, addr: usize) !?MemMapInfo {
    const maps = try self.getMemMaps(allocator);
    defer {
        for (maps) |map| map.deinit(allocator);
        allocator.free(maps);
    }

    for (maps) |map| {
        if (map.addr_range[0] <= addr and addr < map.addr_range[1]) {
            var copy_map = map;
            copy_map.path = try allocator.dupe(u8, map.path);
            return copy_map;
        }
    }

    return null;
}

/// caller owns returned memory
pub fn getMemMaps(self: Session, allocator: Allocator) ![]MemMapInfo {
    const proc_maps_filepath = try std.fmt.allocPrint(allocator, "/proc/{}/maps", .{self.pid});
    defer allocator.free(proc_maps_filepath);
    const proc_maps = try std.fs.openFileAbsolute(proc_maps_filepath, .{});
    defer proc_maps.close();

    const maps_data = try proc_maps.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(maps_data);

    var maps = std.ArrayList(MemMapInfo).init(allocator);

    var line_iter = std.mem.tokenize(u8, maps_data, "\n");
    while (line_iter.next()) |line| {
        var col_iter = std.mem.tokenize(u8, line, " ");

        const map_info = MemMapInfo{
            .addr_range = if (col_iter.next()) |str| blk: {
                const sep_idx = std.mem.indexOfScalar(u8, str, '-').?;
                const start = try std.fmt.parseUnsigned(usize, str[0..sep_idx], 16);
                const end = try std.fmt.parseUnsigned(usize, str[sep_idx + 1 ..], 16);
                break :blk [2]usize{ start, end };
            } else unreachable,
            .perms = if (col_iter.next()) |str| .{
                .read = str[0] == 'r',
                .write = str[1] == 'w',
                .execute = str[2] == 'x',
                .shared = str[3] == 's',
            } else unreachable,
            .offset = if (col_iter.next()) |str| blk: {
                break :blk try std.fmt.parseUnsigned(usize, str, 16);
            } else unreachable,
            .device = if (col_iter.next()) |str| blk: {
                const sep_idx = std.mem.indexOfScalar(u8, str, ':').?;
                const major = try std.fmt.parseUnsigned(u32, str[0..sep_idx], 16);
                const minor = try std.fmt.parseUnsigned(u32, str[sep_idx + 1 ..], 16);
                break :blk .{ .major = major, .minor = minor };
            } else unreachable,
            .inode = if (col_iter.next()) |str| blk: {
                break :blk try std.fmt.parseUnsigned(usize, str, 10);
            } else unreachable,
            .path = if (col_iter.next()) |str| blk: {
                break :blk try allocator.dupe(u8, str);
            } else "",
        };
        try maps.append(map_info);
    }

    return maps.toOwnedSlice();
}

pub fn addWatchedVariable(self: *Session, name: []const u8) !void {
    const dwarf = self.elf.dwarf;
    const DW = @import("Dwarf.zig").DW;
    const forms = @import("Dwarf.zig").forms;
    for (dwarf.units) |unit| {
        var stream = std.io.fixedBufferStream(unit.entries_buf);
        var reader = stream.reader();
        const skip_info = forms.SkipInfo{ .address_size = unit.address_size, .is_64 = unit.is_64 };
        const read_info = forms.ReadInfo{
            .address_size = unit.address_size,
            .is_64 = unit.is_64,
            .version = unit.version,
            .debug_info_opt = unit.debug_info,
            .debug_str_opt = dwarf.debug_str,
        };

        var child_level: usize = 0;
        while ((try stream.getPos()) < unit.entries_buf.len) {
            const abbrev_code = try std.leb.readULEB128(usize, reader);
            const abbrev = unit.abbrevs[abbrev_code];

            if (abbrev.has_children) child_level += 1;
            if (abbrev.tag == 0) {
                child_level -= 1;
                if (child_level == 0) break;
            }

            switch (abbrev.tag) {
                DW.TAG.variable => {
                    var varloc_opt: ?[]const u8 = null;
                    var name_opt: ?[]const u8 = null;
                    for (abbrev.attribs) |pair| {
                        switch (pair.attrib) {
                            DW.AT.location => {
                                if (pair.form != DW.FORM.exprloc) {
                                    try forms.skip(skip_info, reader, pair.form);
                                    continue;
                                }
                                varloc_opt = try forms.readExprLoc(reader, pair.form);
                            },
                            DW.AT.name => {
                                name_opt = try forms.readString(read_info, reader, pair.form);
                            },
                            else => try forms.skip(skip_info, reader, pair.form),
                        }
                    }
                    if (name_opt == null or varloc_opt == null) continue;

                    if (std.mem.eql(u8, name, name_opt.?)) {
                        const exprloc = varloc_opt.?;
                        std.debug.assert(exprloc[0] == DW.OP.fbreg);
                        var tmp_stream = std.io.fixedBufferStream(exprloc[1..]);
                        var tmp_reader = tmp_stream.reader();
                        const rbp_offset = try std.leb.readILEB128(i32, tmp_reader);
                        try self.watched_vars.append(.{
                            .name = name_opt.?,
                            .rbp_offset = rbp_offset,
                            .value = undefined,
                        });
                        //std.debug.print("found var '{s}' w/ exprloc={} (rbp_offset={})\n", .{
                        //    name, std.fmt.fmtSliceHexLower(varloc_opt.?), rbp_offset,
                        //});
                        return;
                    }
                },
                else => {
                    for (abbrev.attribs) |pair| try forms.skip(skip_info, reader, pair.form);
                },
            }
        }
    }
    std.debug.print("couldn't find debug info for variable named '{s}'\n", .{name});
    return Error.VarNotFound;
}

// translate addr to src location
// if no addr matches this line exactly tries the next line until it matches
fn findAddrForThisLineOrNextOne(self: *Session, src: SrcLoc) !?usize {
    prog_blk: for (self.elf.dwarf.line_progs) |prog| {
        var file_idx: usize = 0;
        for (prog.files) |file, i| {
            file_idx = i;
            if (std.mem.eql(u8, src.file, file.name)) break;
        } else continue :prog_blk;

        // TODO: this speedup check doesn't work because lineprog dirs are full paths
        //for (prog.include_dirs) |dir| {
        //    if (std.mem.eql(u8, src.dir, dir)) break;
        //} else continue :prog_blk;

        var search_loc = src;
        while (search_loc.line <= prog.file_line_range[1]) : (search_loc.line += 1) {
            const state = try prog.findAddrForSrc(@intCast(u32, file_idx), search_loc.line);
            if (state) |s| return s.address;
        }
    }
    return null;
}

fn currentSrcLoc(self: *Session) !?SrcLoc {
    return (try self.elf.translateAddrToSrc(self.getRegisters().rip));
}

/// returns the byte that was there before we inserted the new one
fn insertByteAtAddr(self: *Session, addr: usize, byte: u8) !u8 {
    const data = try ptrace(.PEEKTEXT, self.pid, addr);
    const new_data = (data & 0xffff_ffff_ffff_ff00) | byte;
    try ptrace(.POKETEXT, self.pid, .{ .addr = addr, .data = new_data });
    return @truncate(u8, data);
}

pub fn getBytesAtAddr(self: Session, addr: usize, comptime N: u4) ![N]u8 {
    std.debug.assert(N <= 8);
    const data = try ptrace(.PEEKTEXT, self.pid, addr);
    return switch (N) {
        4 => [4]u8{
            @intCast(u8, (data & 0x0000_00ff)),
            @intCast(u8, (data & 0x0000_ff00) >> 8),
            @intCast(u8, (data & 0x00ff_0000) >> 16),
            @intCast(u8, (data & 0xff00_0000) >> 24),
        },
        8 => [8]u8{
            @intCast(u8, (data & 0x0000_0000_0000_00ff)),
            @intCast(u8, (data & 0x0000_0000_0000_ff00) >> 8),
            @intCast(u8, (data & 0x0000_0000_00ff_0000) >> 16),
            @intCast(u8, (data & 0x0000_0000_ff00_0000) >> 24),
            @intCast(u8, (data & 0x0000_00ff_0000_0000) >> 32),
            @intCast(u8, (data & 0x0000_ff00_0000_0000) >> 40),
            @intCast(u8, (data & 0x00ff_0000_0000_0000) >> 48),
            @intCast(u8, (data & 0xff00_0000_0000_0000) >> 56),
        },
        else => @panic("TODO"),
    };
}

/// call `File.close` when done
pub fn procMemFile(self: Session) !std.fs.File {
    var tmpbuf: [0x1000]u8 = undefined;
    const proc_filename = try std.fmt.bufPrint(&tmpbuf, "/proc/{}/mem", .{self.pid});
    return std.fs.cwd().openFile(proc_filename, .{});
}

pub const Registers = struct {
    rax: usize,
    rcx: usize,
    rdx: usize,
    rbx: usize,

    rsp: usize,
    rbp: usize,
    rsi: usize,
    rdi: usize,

    r8: usize,
    r9: usize,
    r10: usize,
    r11: usize,
    r12: usize,
    r13: usize,
    r14: usize,
    r15: usize,

    rip: usize,
    eflags: usize,

    cs: usize,
    ss: usize,
    fs_base: usize,
    gs_base: usize,
    ds: usize,
    es: usize,
    fs: usize,
    gs: usize,

    orig_rax: usize,

    rdp: usize,

    mxcsr: u32,
    mxcr_mask: u32,
    st: [8]u64,
    xmm: [16]u128,
    // the fp_regs struct in <sys/user.h> also had some other things I don't the meaning of
    // unsigned short int	cwd
    // unsigned short int	swd
    // unsigned short int	ftw
    // unsigned short int	fop

    pub fn getPtr(self: *Registers, reg: @import("Dwarf.zig").Register) *usize {
        return switch (reg) {
            .rax => &self.rax,
            .rbx => &self.rbx,
            .rcx => &self.rcx,
            .rdx => &self.rdx,
            .rsi => &self.rsi,
            .rdi => &self.rdi,
            .rbp => &self.rbp,
            .rsp => &self.rsp,
            .r8 => &self.r8,
            .r9 => &self.r9,
            .r10 => &self.r10,
            .r11 => &self.r11,
            .r12 => &self.r12,
            .r13 => &self.r13,
            .r14 => &self.r14,
            .r15 => &self.r15,
            .ret => unreachable,
            //.xmm0 => &self.xmm[0],
            //.xmm1 => &self.xmm[1],
            //.xmm2 => &self.xmm[2],
            //.xmm3 => &self.xmm[3],
            //.xmm4 => &self.xmm[4],
            //.xmm5 => &self.xmm[5],
            //.xmm6 => &self.xmm[6],
            //.xmm7 => &self.xmm[7],
            //.xmm8 => &self.xmm[8],
            //.xmm9 => &self.xmm[9],
            //.xmm10 => &self.xmm[10],
            //.xmm11 => &self.xmm[11],
            //.xmm12 => &self.xmm[12],
            //.xmm13 => &self.xmm[13],
            //.xmm14 => &self.xmm[14],
            //.xmm15 => &self.xmm[15],
            .xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6, .xmm7, .xmm8, .xmm9, .xmm10, .xmm11, .xmm12, .xmm13, .xmm14, .xmm15 => @panic("TODO: xmm registers"),
            .st0 => &self.st[0],
            .st1 => &self.st[1],
            .st2 => &self.st[2],
            .st3 => &self.st[3],
            .st4 => &self.st[4],
            .st5 => &self.st[5],
            .st6 => &self.st[6],
            .st7 => &self.st[7],
            .mm0, .mm1, .mm2, .mm3, .mm4, .mm5, .mm6, .mm7 => @panic("TODO: mm registers"),
        };
    }
};

pub fn getRegisters(self: Session) !Registers {
    var regs: c.user_regs_struct = undefined;
    try ptrace(.GETREGS, self.pid, &regs);
    var fp_regs: c.user_fpregs_struct = undefined;
    try ptrace(.GETFPREGS, self.pid, &fp_regs);
    var registers = Registers{
        .rax = regs.rax,
        .rcx = regs.rcx,
        .rdx = regs.rdx,
        .rbx = regs.rbx,
        .rsp = regs.rsp,
        .rbp = regs.rbp,
        .rsi = regs.rsi,
        .rdi = regs.rdi,
        .r8 = regs.r8,
        .r9 = regs.r9,
        .r10 = regs.r10,
        .r11 = regs.r11,
        .r12 = regs.r12,
        .r13 = regs.r13,
        .r14 = regs.r14,
        .r15 = regs.r15,
        .rip = regs.rip,
        .eflags = regs.eflags,
        .cs = regs.cs,
        .ss = regs.ss,
        .fs_base = regs.fs_base,
        .gs_base = regs.gs_base,
        .ds = regs.ds,
        .es = regs.es,
        .fs = regs.fs,
        .gs = regs.gs,
        .orig_rax = regs.orig_rax,
        .rdp = fp_regs.rdp,

        .mxcsr = fp_regs.mxcsr,
        .mxcr_mask = fp_regs.mxcr_mask,
        .st = undefined,
        .xmm = undefined,
    };
    @memcpy(
        @ptrCast([*]u8, &registers.st[0]),
        @ptrCast([*]u8, &fp_regs.st_space[0]),
        @sizeOf(@TypeOf(registers.st)),
    );
    @memcpy(
        @ptrCast([*]u8, &registers.xmm[0]),
        @ptrCast([*]u8, &fp_regs.xmm_space[0]),
        @sizeOf(@TypeOf(registers.xmm)),
    );
    return registers;
}

fn setRegisters(self: *Session, registers: Registers) !void {
    var regs = c.user_regs_struct{
        .rax = registers.rax,
        .rcx = registers.rcx,
        .rdx = registers.rdx,
        .rbx = registers.rbx,
        .rsp = registers.rsp,
        .rbp = registers.rbp,
        .rsi = registers.rsi,
        .rdi = registers.rdi,
        .r8 = registers.r8,
        .r9 = registers.r9,
        .r10 = registers.r10,
        .r11 = registers.r11,
        .r12 = registers.r12,
        .r13 = registers.r13,
        .r14 = registers.r14,
        .r15 = registers.r15,
        .rip = registers.rip,
        .eflags = registers.eflags,
        .cs = registers.cs,
        .ss = registers.ss,
        .fs_base = registers.fs_base,
        .gs_base = registers.gs_base,
        .ds = registers.ds,
        .es = registers.es,
        .fs = registers.fs,
        .gs = registers.gs,
        .orig_rax = registers.orig_rax,
    };
    try ptrace(.SETREGS, self.pid, &regs);
    var fp_regs = c.user_fpregs_struct{
        .cwd = undefined,
        .swd = undefined,
        .ftw = undefined,
        .fop = undefined,
        .rip = registers.rip,
        .rdp = registers.rdp,
        .mxcsr = registers.mxcsr,
        .mxcr_mask = registers.mxcr_mask,
        .st_space = undefined,
        .xmm_space = undefined,
        .padding = undefined,
    };
    @memcpy(
        @ptrCast([*]u8, &fp_regs.st_space[0]),
        @ptrCast([*]const u8, &registers.st[0]),
        @sizeOf(@TypeOf(registers.st)),
    );
    @memcpy(
        @ptrCast([*]u8, &fp_regs.xmm_space[0]),
        @ptrCast([*]const u8, &registers.xmm[0]),
        @sizeOf(@TypeOf(registers.xmm)),
    );
    try ptrace(.SETFPREGS, self.pid, &fp_regs);
}

/// A doubly-linked list that manages it's own memory
pub fn FreeList(comptime T: type) type {
    return struct {
        allocator: Allocator,
        first: ?*Node,
        last: ?*Node, // helpfull for adding at the end

        const Self = @This();

        pub const Node = struct {
            prev: ?*Node,
            next: ?*Node,
            list: *Self,
            value: T,

            pub fn removeFromList(self: *Node) void {
                if (self.list.first == self) self.list.first = self.next;
                if (self.list.last == self) self.list.last = self.prev;
                if (self.prev) |p| p.next = self.next;
                if (self.next) |n| n.prev = self.prev;

                self.list.allocator.destroy(self);
                self.prev = null;
                self.next = null;
                self.list = null;
            }
        };

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator, .first = null, .last = null };
        }

        pub fn deinit(self: *Self) void {
            while (self.first) |node| {
                self.first = node.next;
                self.allocator.destroy(node);
            }
        }

        pub fn append(self: *Self, item: T) !void {
            const new_node_ptr = try self.allocator.create(Node);
            new_node_ptr.prev = self.last;
            new_node_ptr.next = null;
            new_node_ptr.list = self;
            new_node_ptr.value = item;

            if (self.first == null) self.first = new_node_ptr;
            self.last = new_node_ptr;
        }
    };
}

fn castIntoPtr(comptime PtrType: type, src: anytype) ?PtrType {
    const SrcType = @TypeOf(src);
    return switch (@typeInfo(SrcType)) {
        .Int => if (src == 0) null else @intToPtr(PtrType, src),
        .ComptimeInt => if (src == 0) null else @intToPtr(PtrType, src),
        .Pointer => @ptrCast(PtrType, src),
        else => {
            @compileError("cannot convert " ++ @typeName(SrcType) ++ " to pointer type " ++ @typeName(PtrType));
        },
    };
}

fn ptrace(comptime req: c.PTRACE.request, pid: pid_t, args: anytype) c.ptrace_error!(switch (req) {
    .PEEKTEXT, .PEEKDATA, .PEEKUSER, .PEEKSIGINFO, .SECCOMP_GET_FILTER => usize,
    else => void,
}) {
    const Args = @TypeOf(args);
    const assert = std.debug.assert;

    switch (req) {
        .TRACEME, .SINGLESTEP, .KILL, .ATTACH, .SYSCALL, .LISTEN, .INTERRUPT => {
            _ = try c.ptrace(req, pid, null, null);
        },
        .PEEKTEXT, .PEEKDATA, .PEEKUSER => {
            const addr = castIntoPtr(*anyopaque, args);
            return try c.ptrace(req, pid, addr, null);
        },
        .CONT, .DETACH, .SETOPTIONS, .SYSEMU, .SYSEMU_SINGLESTEP, .SEIZE => {
            comptime assert(@typeInfo(Args) == .Int or @typeInfo(Args) == .ComptimeInt);
            const data = castIntoPtr(*anyopaque, args);
            _ = try c.ptrace(req, pid, null, data);
        },
        .GETEVENTMSG => {
            comptime assert(@typeInfo(Args) == .Pointer);
            comptime assert(@typeInfo(@typeInfo(Args).Pointer.child) == .Int);
            const data = castIntoPtr(*anyopaque, args);
            _ = try c.ptrace(req, pid, null, data);
        },
        .GETSIGINFO, .SETSIGINFO => {
            comptime std.debug.assert(Args == *c.siginfo_t);
            const data = castIntoPtr(*anyopaque, args);
            _ = try c.ptrace(req, pid, null, data);
        },
        .GETREGS, .SETREGS => {
            comptime assert(Args == *c.user_regs_struct);
            const data = castIntoPtr(*anyopaque, args);
            _ = try c.ptrace(req, pid, null, data);
        },
        .GETFPREGS, .SETFPREGS => {
            comptime assert(Args == *c.user_fpregs_struct);
            const data = castIntoPtr(*anyopaque, args);
            _ = try c.ptrace(req, pid, null, data);
        },
        .POKETEXT, .POKEDATA, .POKEUSER, .PEEKSIGINFO => {
            comptime assert(@typeInfo(Args).Struct.fields.len == 2);
            const addr = castIntoPtr(*anyopaque, @field(args, "addr"));
            const data = castIntoPtr(*anyopaque, @field(args, "data"));
            _ = try c.ptrace(req, pid, addr, data);
        },
        // TODO: assert the correct types for these
        .SECCOMP_GET_FILTER, .GET_SYSCALL_INFO => {
            comptime assert(@typeInfo(Args).Struct.fields.len == 2);
            //const c_ptrace = @cInclude("sys/ptrace.h");
            const addr = castIntoPtr(*anyopaque, @field(args, "addr"));
            const data = castIntoPtr(*anyopaque, @field(args, "data"));
            _ = try c.ptrace(req, pid, addr, data);
        },
        .GETREGSET, .SETREGSET => {
            comptime assert(@typeInfo(Args).Struct.fields.len == 2);
            const addr = castIntoPtr(*anyopaque, @field(args, "addr"));
            const Addr = @TypeOf(addr);
            const data = castIntoPtr(*anyopaque, @field(args, "data"));
            const Data = @TypeOf(data);
            comptime assert(@typeInfo(Addr) == .Int or @typeInfo(Addr) == .ComptimeInt);
            comptime assert(Data == *c.iovec);
            _ = try c.ptrace(req, pid, addr, data);
        },
        .GETSIGMASK, .SETSIGMASK => {
            comptime assert(Args == *c.sigset_t);
            //const addr = castIntoPtr(*anyopaque, @sizeOf(c.sigset_t));
            // https://stackoverflow.com/a/59765080
            const addr = castIntoPtr(*anyopaque, 8);
            const data = castIntoPtr(*anyopaque, args);
            _ = try c.ptrace(req, pid, addr, data);
        },
        else => @compileError("TODO: " ++ @tagName(req)),
    }
}

pub const WaitStatus = struct {
    exit_status: ?u8, // valid if exited normally
    term_signal: ?Signal, // valid if terminated by signal
    stop_signal: ?Signal, // valid if stopped by signal
    resumed: bool, // resumed via SIGCONT
    ptrace_event: ?c.PTRACE.EVENT,

    pub fn parse(status: u32) WaitStatus {
        const low_bits = @intCast(u8, status & 0x7f);
        const second_byte = @intCast(u8, (status & 0xff00) >> 8);
        const third_byte = @intCast(u8, (status & 0xff_0000) >> 16);
        const exited_normally = (low_bits == 0);
        const terminated_by_signal = (@bitCast(i8, low_bits + 1) >> 1) > 0;
        const stopped_by_signal = (status & 0xff) == 0x7f;
        const had_event = second_byte == @enumToInt(Signal.SIGTRAP) and third_byte != 0;
        return .{
            .exit_status = if (exited_normally) second_byte else null,
            .term_signal = if (terminated_by_signal) @intToEnum(Signal, low_bits) else null,
            .stop_signal = if (stopped_by_signal) @intToEnum(Signal, second_byte) else null,
            .resumed = status == 0xffff,
            .ptrace_event = if (had_event) @intToEnum(c.PTRACE.EVENT, third_byte) else null,
        };
    }
};

pub fn getWaitStatus(pid: pid_t) !WaitStatus {
    var wait_status: u32 = 0;

    // from `man ptrace`: "Setting the WCONTINUED flag when calling waitpid(2) is
    // not recommended: the "continued" state is per-process and consuming it can
    // confuse the real parent of the tracee.
    //_ = try waitpid(pid, &wait_status, c.__WALL | c.WCONTINUED);
    _ = try waitpid(pid, &wait_status, c.__WALL);

    return WaitStatus.parse(wait_status);
}

pub fn getWaitStatusNoHang(pid: pid_t) !?WaitStatus {
    var wait_status: u32 = 0;
    const status_pid = try waitpid(pid, &wait_status, c.__WALL | c.WNOHANG);
    return if (status_pid == 0) null else WaitStatus.parse(wait_status);
}

pub const waitpid_error = error{
    ECHILD,
    EINTR,
    EINVAL,
};

pub fn waitpid(pid: pid_t, wstatus: *u32, options: u32) waitpid_error!pid_t {
    const wait_ret = std.os.linux.waitpid(pid, wstatus, options);
    const errno = std.os.linux.getErrno(wait_ret);
    switch (errno) {
        .SUCCESS => {},
        .CHILD => return waitpid_error.ECHILD,
        .INTR => return waitpid_error.EINTR,
        .INVAL => return waitpid_error.EINVAL,
        else => std.debug.panic("invalid errno={} for waitpid\n", .{errno}),
    }
    return @intCast(i32, wait_ret);
}

// from the table in `man 7 signal`
pub const Signal = enum(u8) {
    SIGHUP = 1,
    SIGINT = 2,
    SIGQUIT = 3,
    SIGILL = 4,
    SIGTRAP = 5,
    SIGABRT = 6,
    SIGBUS = 7,
    SIGFPE = 8,
    SIGKILL = 9,
    SIGUSR1 = 10,
    SIGSEGV = 11,
    SIGUSR2 = 12,
    SIGPIPE = 13,
    SIGALRM = 14,
    SIGTERM = 15,
    SIGSTKFLT = 16,
    SIGCHLD = 17,
    SIGCONT = 18,
    SIGSTOP = 19,
    SIGTSTP = 20,
    SIGTTIN = 21,
    SIGTTOU = 22,
    SIGURG = 23,
    SIGXCPU = 24,
    SIGXFSZ = 25,
    SIGVTALRM = 26,
    SIGPROF = 27,
    SIGWINCH = 28,
    SIGIO = 29,
    SIGPWR = 30,
    SIGSYS = 31,
};

pub fn sendSignal(pid: pid_t, sig: Signal) !void {
    try std.os.kill(pid, @enumToInt(sig));
}
