const std = @import("std");

pub usingnamespace @cImport({
    @cInclude("Zydis/Zydis.h");

    @cInclude("unistd.h");
    @cInclude("sys/user.h");
    @cInclude("sys/wait.h");
    @cInclude("sys/types.h");
    @cInclude("sys/uio.h");
    @cInclude("signal.h");
    @cInclude("string.h");
});

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
