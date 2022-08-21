const std = @import("std");

pub usingnamespace @cImport({
    @cDefine("GLFW_INCLUDE_NONE", "1");
    @cInclude("GLFW/glfw3.h");

    @cInclude("sys/user.h");
    @cInclude("sys/wait.h");
    @cInclude("sys/types.h");
    @cInclude("signal.h");
    @cInclude("string.h");
});

pub const ptrace_request = enum(u32) {
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

pub fn ptrace(req: ptrace_request, pid: std.os.pid_t, _addr: ?*anyopaque, _data: ?*anyopaque) usize {
    var addr = if (_addr) |a| @ptrToInt(a) else 0;
    var data = if (_data) |d| @ptrToInt(d) else 0;

    // both musl and glibc do this, so I'm doing it too
    var ret: usize = 0;
    if (@enumToInt(req) < 4) data = @ptrToInt(&ret);

    const ptrace_ret = std.os.linux.syscall4(
        .ptrace,
        @enumToInt(req),
        @intCast(usize, pid),
        addr,
        data,
    );

    const errno = std.os.errno(ptrace_ret);
    if (errno != .SUCCESS) {
        std.debug.panic("errno={}, {s}\n", .{ errno, @tagName(errno) });
    }

    if (@enumToInt(req) < 4) return ret;
    return ptrace_ret;
}

//pub const WNOHANG = 1;
//pub const WUNTRACED = 2;
//pub const WSTOPPED = 2;
//pub const WEXITED = 4;
//pub const WCONTINUED = 8;
//pub const WNOWAIT = 0x01000000;
//pub const __WNOTHREAD = 0x20000000;
//pub const __WALL = 0x40000000;
//pub const __WCLONE = 0x80000000;
