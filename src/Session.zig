const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("c.zig");
const Elf = @import("Elf.zig");

const Session = @This();

allocator: Allocator,
exec_path: [:0]const u8,
pid: std.os.pid_t,
status: enum { Running, Stopped },
wait_status: u32,
elf: Elf,
breakpoints: std.ArrayList(BreakPoint),
src_loc: ?SrcLoc,

const BreakPoint = struct {
    addr: usize,
    saved_byte: u8,
    only_once: bool = false,
};

pub const SrcLoc = Elf.SrcLoc;

pub fn init(allocator: Allocator, exec_path: []const u8) !Session {
    var self = Session{
        .allocator = allocator,
        .exec_path = try allocator.dupeZ(u8, exec_path),
        .pid = undefined,
        .status = undefined,
        .wait_status = undefined,
        .elf = try Elf.init(allocator, exec_path),
        .breakpoints = std.ArrayList(BreakPoint).init(allocator),
        .src_loc = null,
    };
    try self.startTracing();
    return self;
}

pub fn deinit(self: *Session) void {
    self.killChild();
    self.allocator.free(self.exec_path);
    self.elf.deinit();
    self.breakpoints.deinit();
}

/// checks if the child is stopped at a breakpoint (which we need to restore)
/// some breakpoints need to be re-setup (in case we want to hit them again)
pub fn update(self: *Session) !void {
    self.updateStatus();
    if (self.status != .Stopped) return;

    var regs = self.getRegisters();
    self.src_loc = try self.elf.translateAddrToSrc(regs.rip);

    // if we're stopped at breakpoint, restore the clobbered byte and fix rip
    for (self.breakpoints.items) |*breakpt| {
        if (breakpt.addr == regs.rip - 1) {
            const cc = self.insertByteAtAddr(breakpt.addr, breakpt.saved_byte);
            std.debug.print("fixing breakpoint at 0x{x} (rip=0x{x}) with saved_byte=0x{x}, got 0x{x} back\n", .{ breakpt.addr, regs.rip, breakpt.saved_byte, cc });
            std.debug.assert(cc == 0xcc);
            var new_regs = regs;
            new_regs.rip -= 1;
            self.setRegisters(new_regs);
        }
    }

    regs = self.getRegisters();

    // TODO this is not working we're getting back 0x00 for the saved byte stored in the breakpoint
    // reset all the breakpoints (except the one we just hit)
    for (self.breakpoints.items) |*breakpt| {
        if (breakpt.addr == regs.rip) continue;
        const saved = self.insertByteAtAddr(breakpt.addr, 0xcc);
        //std.debug.print("resetting breakpoint at @ addr=0x{x}, (had saved=0x{x}), breakpt.saved_byte=0x{x}\n", .{ breakpt.addr, saved, breakpt.saved_byte });
        if (saved != 0xcc) breakpt.saved_byte = saved;
    }
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
    // hack: to get updateStatus to use .Stopped when the program is stopped right after the
    // initial exec call
    //self.pauseRunning();
}

pub fn setBreakpointAtAddr(self: *Session, addr: usize) !void {
    for (self.breakpoints.items) |breakpt| {
        if (breakpt.addr == addr) return;
    }
    try self.breakpoints.append(.{
        .addr = addr,
        .saved_byte = self.insertByteAtAddr(addr, 0xcc),
    });
    std.debug.print("setting breakpoint at 0x{x}, saved_byte=0x{x}\n", .{ addr, self.breakpoints.items[self.breakpoints.items.len - 1].saved_byte });
}

pub fn stepInstructions(self: *Session, num_instrs: usize) !void {
    var instrs_done: usize = 0;
    while (instrs_done < num_instrs) : (instrs_done += 1) {
        _ = c.ptrace(.SINGLESTEP, self.pid, null, null);
        self.waitForStatusChange();
    }
}

/// this steps over functions
pub fn stepLine(self: *Session) !void {
    // because of loops and jumps backwards we can't just put a breakpoint
    // on the next line and be done with it. the only solution I can
    // think of right now is this (singlestepping until we hit a different
    // line) but it could, in some cases, freeze the UI for a while, if
    // it takes like 1k+ instructions to reach the next line.
    var old_src = self.src_loc orelse return;
    while (true) {
        try self.stepInstructions(1);
        //std.debug.print("step one instruction. addr is now 0x{x}\n", .{self.getRegisters().rip});
        var new_src = (try self.currentSrcLoc()) orelse continue;
        if (old_src.line != new_src.line or old_src.column != new_src.column) {
            return;
        }
    }
}

/// return a bool determining success of this operation
pub fn putBreakpointAtNextSrc(self: *Session, src_loc: SrcLoc) !bool {
    const line_prog = (try self.elf.getLineProgForSrc(src_loc)) orelse std.debug.panic("passed in invalid src\n", .{});
    var src = src_loc;
    while (true) {
        src.line += 1;
        if (src.line > line_prog.file_line_range[1]) return false;
        if (try self.elf.translateSrcToAddr(src)) |new_addr| {
            try self.setBreakpointAtAddr(new_addr);
            return true;
        }
    }
    return false;
}

pub fn continueRunning(self: *Session) void {
    _ = c.ptrace(.CONT, self.pid, null, null);
    self.updateStatus();
}

pub fn killChild(self: *Session) void {
    _ = c.kill(self.pid, c.SIGKILL);
}

pub fn pauseRunning(self: *Session) void {
    _ = c.kill(self.pid, c.SIGSTOP);
}

pub fn currentSrcLoc(self: *Session) !?SrcLoc {
    return (try self.elf.translateAddrToSrc(self.getRegisters().rip));
}

pub fn setBreakpointAtSrc(self: *Session, src: SrcLoc) !void {
    const addr = (try self.elf.translateSrcToAddr(src)) orelse std.debug.panic("could not translate src={} to an address\n", .{src});
    try self.setBreakpointAtAddr(addr);
}

pub fn updateStatus(self: *Session) void {
    _ = std.os.linux.waitpid(self.pid, &self.wait_status, c.WNOHANG | c.WUNTRACED | c.WCONTINUED);
    const is_stopped = (self.wait_status & 0xff) == 0x7f;
    //std.debug.print("wait_status = 0x{x:0>8}, is_stopped: {}\n", .{ self.wait_status, is_stopped });
    self.status = if (is_stopped) .Stopped else .Running;
}

pub fn waitForStatusChange(self: *Session) void {
    _ = std.os.linux.waitpid(self.pid, &self.wait_status, 0);
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

pub fn setRegisters(self: *Session, registers: Registers) void {
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
    _ = c.ptrace(.SETREGS, self.pid, null, &regs);
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
    _ = c.ptrace(.SETFPREGS, self.pid, null, &fp_regs);
}

/// returns the byte that was there before we inserted the new one
/// returns the byte that was there before we inserted the new one
fn insertByteAtAddr(self: *Session, addr: usize, byte: u8) u8 {
    const data = c.ptrace(.PEEKTEXT, self.pid, @intToPtr(*anyopaque, addr), null);
    const new_data = (data & 0xffff_ffff_ffff_ff00) | byte;
    _ = c.ptrace(.POKETEXT, self.pid, @intToPtr(*anyopaque, addr), @intToPtr(*anyopaque, new_data));
    return @truncate(u8, data);
}
