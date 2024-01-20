const std = @import("std");
const Allocator = std.mem.Allocator;
const pid_t = std.os.pid_t;
const stringToEnum = std.meta.stringToEnum;
const c = @import("src/c.zig");
const Elf = @import("src/Elf.zig");
const Dwarf = @import("src/Dwarf.zig");
const Session = @import("src/Session.zig");
const WaitStatus = Session.WaitStatus;
const Signal = Session.Signal;
const sendSignal = Session.sendSignal;

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const filepath = args[1];

    var elf = try Elf.init(allocator, filepath);
    defer elf.deinit();

    const pid = try std.os.fork();
    if (pid == 0) {
        ptrace(.TRACEME, 0, {}) catch unreachable;
        const path_ptr = filepath.ptr;
        const environ_ptr = std.os.environ.ptr;
        _ = std.os.linux.syscall3(.execve, @intFromPtr(path_ptr), 0, @intFromPtr(environ_ptr));
        unreachable;
    }
    // wait for the tracee process to start and get it's SIGTRAP
    std.debug.assert(WaitStatus.wait(pid).stop_signal.? == .SIGTRAP);

    try ptrace(.SETOPTIONS, pid, c.PTRACE.O.EXITKILL); // kill the tracee if we exit

    const stdout = std.io.getStdOut();
    const out = stdout.writer();
    var done = false;
    while (!done) {
        const status = WaitStatus.get(pid);
        _ = status;

        try out.print("> ", .{});

        switch (try readCmd() orelse continue) {
            .quit => done = true,
            .pause => {
                // TODO: if already running: do nothing (or else calling waitpid will hang)
                try sendSignal(pid, .SIGSTOP);
                std.debug.assert(WaitStatus.wait(pid).stop_signal.? == .SIGSTOP);
            },
            .@"continue" => {
                // A child resuming by *not a signal* is not picked up by `waitpid`
                // so we don't need to wait here
                try ptrace(.CONT, pid, 0);
            },
            .cont_sig => |arg| {
                if (stringToEnum(Signal, arg)) |sig| {
                    try ptrace(.CONT, pid, @intFromEnum(sig));
                } else std.debug.print("unknown signal '{s}'\n", .{arg});
            },
            .send => |arg| {
                if (stringToEnum(Signal, arg)) |sig| {
                    try sendSignal(pid, sig);
                } else std.debug.print("unknown signal '{s}'\n", .{arg});
            },
            .sig_info => {
                var siginfo: c.siginfo_t = undefined;
                try ptrace(.GETSIGINFO, pid, &siginfo);
                const sig: Signal = @enumFromInt(siginfo.signo);
                try out.print("signal number: {} ({s})\n", .{ siginfo.signo, @tagName(sig) });
                try out.print("errno: {}\n", .{siginfo.errno});
                try out.print("signal code: {}\n", .{siginfo.code});
            },
            .sig_queue => {
                var signals_buf: [50]c.siginfo_t = undefined;
                const ptrace_args = c.ptrace_peeksiginfo_args{
                    .off = 0,
                    .flags = c.PTRACE.PEEKSIGINFO_SHARED,
                    .nr = signals_buf.len,
                };
                // TODO: can't seem to get any info out of this ptrace call, not sure why
                const read = try ptrace(.PEEKSIGINFO, pid, .{
                    .data = &signals_buf,
                    .addr = &ptrace_args,
                });
                try out.print("signals in queue:\n", .{});
                for (signals_buf[0..read], 0..) |sig, idx| {
                    try out.print("[{}] {s}\n", .{ idx, @tagName(@as(Signal, @enumFromInt(sig.signo))) });
                }
            },
            .set_break_at => |arg| {
                std.debug.print("TODO: set_break_at '{s}'\n", .{arg});

                var matches = std.ArrayList(Match).init(allocator);
                defer matches.deinit();
                for (elf.dwarf.units, 0..) |unit, unit_idx| {
                    for (unit.functions) |func| {
                        const name = func.name orelse continue;
                        const line_prog = elf.dwarf.line_progs[unit_idx];
                        if (std.mem.eql(u8, name, arg)) {
                            try matches.append(.{
                                .func = func,
                                .src_loc = if (func.decl_coords) |coords|
                                    coords.toSrcLoc(line_prog)
                                else
                                    null,
                            });
                        }
                    }
                }

                const choice = try chooseElement(Match, matches.items, out);

                const func = matches.items[choice].func;
                std.debug.print("{}\n", .{func});

                const data = try ptrace(.PEEKTEXT, pid, func.low_pc.?);
                std.debug.print("data=0x{x}\n", .{data});
            },
        }
    }
}

const Breakpoint = struct {};

const Match = struct {
    func: Dwarf.Function,
    src_loc: ?Dwarf.SrcLoc,

    pub fn format(value: Match, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        if (value.src_loc) |loc| {
            try writer.print("{s}/{s}:{}:{}", .{ loc.dir, loc.file, loc.line, loc.column });
        } else try writer.print("??", .{});
    }
};

fn chooseElement(comptime T: type, list: []const T, out: anytype) !usize {
    std.debug.assert(@hasDecl(T, "format"));

    const stdin = std.io.getStdIn();

    for (list, 0..) |elem, idx| {
        try out.print("[{}] {}\n", .{ idx, elem });
    }

    while (true) {
        try out.print("Choose element by index [0-{}]: ", .{list.len - 1});
        var readbuf: [0x1000]u8 = undefined;
        const read_size = try stdin.read(&readbuf);
        const line = readbuf[0 .. @max(read_size, 1) - 1];
        const choice = std.fmt.parseUnsigned(usize, line, 0) catch {
            try out.print("'{s}' not valid index\n", .{line});
            continue;
        };
        if (choice < list.len) return choice;
    }

    unreachable;
}

fn readCmd() !?Command {
    const stdin = std.io.getStdIn();

    var readbuf: [0x1000]u8 = undefined;
    const line = readbuf[0..try stdin.read(&readbuf)];

    const space_idx = std.mem.indexOfScalar(u8, line, ' ');
    const cmd = line[0 .. space_idx orelse line.len - 1];
    const opt_arg: ?[]const u8 = if (space_idx) |idx| line[idx + 1 .. line.len - 1] else null;

    const cmd_tag = stringToEnum(std.meta.Tag(Command), cmd) orelse {
        std.debug.print("unknown command '{s}'\n", .{cmd});
        return null;
    };
    return switch (cmd_tag) {
        .cont_sig => .{ .cont_sig = opt_arg orelse return null },
        .send => .{ .send = opt_arg orelse return null },
        .set_break_at => .{ .set_break_at = opt_arg orelse return null },
        inline else => |tag| @unionInit(Command, @tagName(tag), {}),
    };
}

const Command = union(enum) {
    quit: void,
    pause: void,
    @"continue": void,
    cont_sig: []const u8,
    send: []const u8,
    sig_info: void,
    sig_queue: void,
    set_break_at: []const u8,
};

pub const WaitStatus = struct {
    exit_status: ?u8, // valid if exited normally
    term_signal: ?Signal, // valid if terminated by signal
    stop_signal: ?Signal, // valid if stopped by signal
    resumed: bool, // resumed via SIGCONT
    ptrace_event: ?c.PTRACE.EVENT,

    pub fn parse(status: u32) WaitStatus {
        const low_bits: u8 = @intCast(status & 0x7f);
        const second_byte: u8 = @intCast((status & 0xff00) >> 8);
        const third_byte: u8 = @intCast((status & 0xff_0000) >> 16);
        const exited_normally = (low_bits == 0);
        const terminated_by_signal = (@as(i8, @bitCast(low_bits + 1)) >> 1) > 0;
        const stopped_by_signal = (status & 0xff) == 0x7f;
        const had_event = second_byte == @intFromEnum(Signal.SIGTRAP) and third_byte != 0;
        return .{
            .exit_status = if (exited_normally) second_byte else null,
            .term_signal = if (terminated_by_signal) @enumFromInt(low_bits) else null,
            .stop_signal = if (stopped_by_signal) @enumFromInt(second_byte) else null,
            .resumed = status == 0xffff,
            .ptrace_event = if (had_event) @enumFromInt(third_byte) else null,
        };
    }

    pub fn wait(pid: pid_t) WaitStatus {
        var status: u32 = 0;
        const wait_ret = std.os.linux.waitpid(pid, &status, 0);
        _ = wait_ret;
        return WaitStatus.parse(status);
    }

    pub fn get(pid: pid_t) WaitStatus {
        var status: u32 = 0;
        const wait_ret = std.os.linux.waitpid(pid, &status, c.WNOHANG);
        _ = wait_ret;
        return WaitStatus.parse(status);
    }
};

pub fn ptrace(comptime req: c.PTRACE.request, pid: pid_t, args: anytype) c.ptrace_error!(switch (req) {
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
        .PEEKSIGINFO => {
            comptime assert(@typeInfo(Args).Struct.fields.len == 2);
            const addr = castIntoPtr(*anyopaque, @field(args, "addr"));
            const data = castIntoPtr(*anyopaque, @field(args, "data"));
            return try c.ptrace(req, pid, addr, data);
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
        .POKETEXT, .POKEDATA, .POKEUSER => {
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

fn castIntoPtr(comptime PtrType: type, src: anytype) ?PtrType {
    const SrcType = @TypeOf(src);
    return switch (@typeInfo(SrcType)) {
        .Int => if (src == 0) null else @ptrFromInt(src),
        .ComptimeInt => if (src == 0) null else @ptrFromInt(src),
        .Pointer => @constCast(@ptrCast(src)),
        else => {
            @compileError("cannot convert " ++ @typeName(SrcType) ++ " to pointer type " ++ @typeName(PtrType));
        },
    };
}

const c = struct {
    pub const siginfo_t = std.os.linux.siginfo_t;
    pub const ptrace_peeksiginfo_args = extern struct {
        off: u64,
        flags: u32,
        nr: i32,
    };
    pub const WNOHANG = 1;

    pub const PTRACE = struct {
        // from /usr/include/sys/ptrace.h
        pub const request = enum(u32) {
            TRACEME = 0,
            PEEKTEXT = 1,
            PEEKDATA = 2,
            PEEKUSER = 3,
            POKETEXT = 4,
            POKEDATA = 5,
            POKEUSER = 6,
            CONT = 7,
            KILL = 8,
            SINGLESTEP = 9,
            GETREGS = 12,
            SETREGS = 13,
            GETFPREGS = 14,
            SETFPREGS = 15,
            ATTACH = 16,
            DETACH = 17,
            GETFPXREGS = 18,
            SETFPXREGS = 19,
            SYSCALL = 24,
            GET_THREAD_AREA = 25,
            SET_THREAD_AREA = 26,
            ARCH_PRCTL = 30,
            SYSEMU = 31,
            SYSEMU_SINGLESTEP = 32,
            SINGLEBLOCK = 33,
            SETOPTIONS = 0x4200,
            GETEVENTMSG = 0x4201,
            GETSIGINFO = 0x4202,
            SETSIGINFO = 0x4203,
            GETREGSET = 0x4204,
            SETREGSET = 0x4205,
            SEIZE = 0x4206,
            INTERRUPT = 0x4207,
            LISTEN = 0x4208,
            PEEKSIGINFO = 0x4209,
            GETSIGMASK = 0x420a,
            SETSIGMASK = 0x420b,
            SECCOMP_GET_FILTER = 0x420c,
            SECCOMP_GET_METADATA = 0x420d,
            GET_SYSCALL_INFO = 0x420e,
            GET_RSEQ_CONFIGURATION = 0x420f,
        };

        // flag for PTRACE_PEEKSIGINFO
        pub const PEEKSIGINFO_SHARED = 1 << 0;

        // from /usr/include/bits/ptrace-shared.h
        pub const O = struct {
            // zig fmt: off
            pub const TRACESYSGOOD    = 0x00000001;
            pub const TRACEFORK       = 0x00000002;
            pub const TRACEVFORK      = 0x00000004;
            pub const TRACECLONE      = 0x00000008;
            pub const TRACEEXEC       = 0x00000010;
            pub const TRACEVFORKDONE  = 0x00000020;
            pub const TRACEEXIT       = 0x00000040;
            pub const TRACESECCOMP    = 0x00000080;
            pub const EXITKILL        = 0x00100000;
            pub const SUSPEND_SECCOMP = 0x00200000;
            pub const MASK            = 0x003000ff;
            // zig fmt: on
        };

        pub const EVENT = enum(u8) {
            FORK = 1,
            VFORK = 2,
            CLONE = 3,
            EXEC = 4,
            VFORK_DONE = 5,
            EXIT = 6,
            SECCOMP = 7,
            STOP = 128,
        };
    };

    pub const ptrace_error = error{
        BUSY,
        FAULT,
        INVAL,
        IO,
        PERM,
        SRCH,
    };

    pub fn ptrace(req: PTRACE.request, pid: std.os.pid_t, _addr: ?*anyopaque, _data: ?*anyopaque) ptrace_error!usize {
        var addr = if (_addr) |a| @intFromPtr(a) else 0;
        var data = if (_data) |d| @intFromPtr(d) else 0;

        // both musl and glibc do this, so I'm doing it too
        // this is because the glibc API is different from the kernel API
        // see `C library/kernel differences` section of ptrace man page
        var ret: usize = 0;
        if (@intFromEnum(req) < 4) data = @intFromPtr(&ret);

        const ptrace_ret = std.os.linux.syscall4(
            .ptrace,
            @intFromEnum(req),
            @as(usize, @intCast(pid)),
            addr,
            data,
        );

        const errno = std.os.linux.getErrno(ptrace_ret);
        switch (errno) {
            .SUCCESS => {},
            .BUSY => return ptrace_error.BUSY,
            .FAULT => return ptrace_error.FAULT,
            .INVAL => return ptrace_error.INVAL,
            .IO => return ptrace_error.IO,
            .PERM => return ptrace_error.PERM,
            .SRCH => return ptrace_error.SRCH,
            else => std.debug.panic("invalid error from ptrace(.{s}, pid={}, addr={?}, data={?}) {}\n", .{
                @tagName(req), pid, _addr, _data, errno,
            }),
        }

        if (@intFromEnum(req) < 4) return ret;
        return ptrace_ret;
    }
};
