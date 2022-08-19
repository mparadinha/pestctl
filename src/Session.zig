const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");
const Elf = @import("Elf.zig");

const Session = @This();

allocator: Allocator,
exec_path: [:0]const u8,
pid: std.os.pid_t,
wait_status: u32,
elf: Elf,
breakpts: std.ArrayList(BreakPoint),

const BreakPoint = struct {
    addr: usize,
    saved_byte: u8,
};

pub fn init(allocator: Allocator, exec_path: []const u8) !Session {
    var self = Session{
        .allocator = allocator,
        .exec_path = try allocator.dupeZ(u8, exec_path),
        .pid = undefined,
        .wait_status = undefined,
        .elf = try Elf.init(allocator, exec_path),
        .breakpts = std.ArrayList(BreakPoint).init(allocator),
    };
    try self.startTracing();
    return self;
}

pub fn deinit(self: *Session) void {
    self.killChild();
    self.allocator.free(self.exec_path);
    self.elf.deinit();
    self.breakpts.deinit();
}

pub fn hasDebugInfo(self: Session) bool {
    return self.elf.hasDebugInfo();
}

pub fn startTracing(self: *Session) !void {
    self.pid = try std.os.fork();
    if (self.pid == 0) {
        _ = c.ptrace(.TRACEME, 0, null, null);
        _ = std.os.linux.syscall3(
            .execve,
            @ptrToInt(self.exec_path.ptr),
            0,
            @ptrToInt(std.os.environ.ptr),
        );
        unreachable;
    }
    self.wait();
}

pub fn setBreakAtAddr(self: *Session, addr: usize) !void {
    try self.breakpts.append(.{
        .addr = addr,
        .saved_byte = try self.insertByteAtAddr(addr, 0xcc),
    });
}

/// returns the byte that was there before we inserted the new one
fn insertByteAtAddr(self: *Session, addr: usize, byte: u8) !u8 {
    const data = c.ptrace(.PEEKTEXT, self.pid, @intToPtr(*anyopaque, addr), null);
    const new_data = (data & 0xffff_ffff_ffff_ff00) | byte;
    _ = c.ptrace(.POKETEXT, self.pid, @intToPtr(*anyopaque, addr), @intToPtr(*anyopaque, new_data));
    return @truncate(u8, data);
}

pub fn killChild(self: *Session) void {
    _ = c.kill(self.pid, c.SIGKILL);
}

pub fn stepInstructions(self: *Session, num_instrs: usize) !void {
    var instrs_done: usize = 0;
    while (instrs_done < num_instrs) : (instrs_done += 1) {
        _ = c.ptrace(.SINGLESTEP, self.pid, null, null);
        self.wait();
    }
}

pub const SrcLoc = Elf.SrcLoc;

pub fn stepLine(self: *Session) !void {
    // TODO
    _ = self;
}

pub fn continueRunning(self: *Session) void {
    _ = c.ptrace(.CONT, self.pid, null, null);
}

pub fn stopRunning(self: *Session) void {
    _ = c.kill(self.pid, c.SIGSTOP);
}

pub fn currentSrcLoc(self: *Session) SrcLoc {
    std.debug.assert(self.hasDebugInfo());
    return self.elf.translateAddrToSrc(self.getRegisters().rip) catch
        SrcLoc{ .line = 0, .column = 0, .file = "" };
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

};

pub fn getRegisters(self: Session) Registers {
    var regs: c.user_regs_struct = undefined;
    _ = c.ptrace(.GETREGS, self.pid, null, &regs);
    var fp_regs: c.user_fpregs_struct = undefined;
    _ = c.ptrace(.GETFPREGS, self.pid, null, &fp_regs);
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

pub fn wait(self: *Session) void {
    _ = std.os.linux.waitpid(self.pid, &self.wait_status, 0);
}
