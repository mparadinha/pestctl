const std = @import("std");
const DW = @import("constants.zig");
const Dwarf = @import("../Dwarf.zig");
const readIs64 = Dwarf.readIs64;

const Frame = @This();

is_64: bool,
address_size: u8,
segment_selector_size: u8,
code_alignment_factor: usize,
data_alignment_factor: isize,
return_address_register: usize,
pc_begin: usize,
pc_end: usize,
initial_ops: []const u8,
ops: []const u8,

// see Dwarf v5, chapter 6.4.1, pg.173, line 7
pub const RegRule = union(enum) {
    @"undefined": void,
    same_value: void,
    offset: isize,
    val_offset: isize,
    register: usize,
    expression: []const u8,
    val_expression: []const u8,
    //architectural: void,
};

// see Dwarf v5, chapter 6.4.1, pg.172, line 34
pub const CfaRule = union(enum) {
    @"undefined": void,
    register: struct { reg: u8, offset: isize },
    expression: []const u8,
};

const State = struct {
    loc: usize,
    cfa: CfaRule,
    regs: [max_registers]RegRule,

    pub const max_registers = @typeInfo(Dwarf.Register).Enum.fields.len;
};

pub fn rulesForAddr(self: Frame, addr: usize) !State {
    std.debug.assert(self.pc_begin <= addr and addr < self.pc_end);
    var state = State{
        .loc = self.pc_begin,
        .cfa = .{ .@"undefined" = {} },
        // default register rule is 'undefined'. see: Dwarf v5 spec, chapter 6.4.1, pg. 175, line 22
        .regs = [_]RegRule{RegRule{ .@"undefined" = {} }} ** State.max_registers,
    };

    var initial_stream = std.io.fixedBufferStream(self.initial_ops);
    var initial_reader = initial_stream.reader();
    while ((try initial_stream.getPos()) < self.initial_ops.len) {
        const new_row = try self.updateState(&state, initial_reader, null);
        if (new_row) |row| {
            if (row.loc > addr) return state;
        }
    }

    const initial_state = state;

    var stream = std.io.fixedBufferStream(self.ops);
    var reader = stream.reader();
    while ((try stream.getPos()) < self.ops.len) {
        const new_row = try self.updateState(&state, reader, initial_state);
        if (new_row) |row| {
            if (row.loc > addr) return state;
        }
    }

    return state;
}

/// similarly to LineProg, only some instructions actually create new rows
/// so `null` is returned to indicate no new row is to be created
pub fn updateState(self: Frame, state: *State, reader: anytype, initial_state: ?State) !?State {
    var new_state = @as(?State, null);

    const address_is_64 = switch (self.address_size) {
        4 => false,
        8 => true,
        else => |size| std.debug.panic("address_size={}\n", .{size}),
    };

    const instruction = try reader.readByte();
    // row creation instructions
    if (instruction == DW.CFA.set_loc) {
        if (self.segment_selector_size > 0) try reader.skipBytes(self.segment_selector_size, .{});
        state.loc = try readIs64(reader, address_is_64);
        new_state = state.*;
    } else if ((instruction & 0xc0) == DW.CFA.advance_loc) {
        state.loc += (instruction & 0x3f) * self.code_alignment_factor;
        new_state = state.*;
    } else if (instruction == DW.CFA.advance_loc1) {
        state.loc += (try reader.readByte()) * self.code_alignment_factor;
        new_state = state.*;
    } else if (instruction == DW.CFA.advance_loc2) {
        state.loc += (try reader.readIntLittle(u16)) * self.code_alignment_factor;
        new_state = state.*;
    } else if (instruction == DW.CFA.advance_loc4) {
        state.loc += (try reader.readIntLittle(u32)) * self.code_alignment_factor;
        new_state = state.*;
    }
    // CFA definition instructions
    else if (instruction == DW.CFA.def_cfa) {
        state.cfa = .{ .register = .{
            .reg = try std.leb.readULEB128(u8, reader),
            .offset = @intCast(isize, try std.leb.readULEB128(usize, reader)),
        } };
    } else if (instruction == DW.CFA.def_cfa_sf) {
        state.cfa = .{ .register = .{
            .reg = try std.leb.readULEB128(u8, reader),
            .offset = (try std.leb.readILEB128(isize, reader)) * self.data_alignment_factor,
        } };
    } else if (instruction == DW.CFA.def_cfa_register) {
        state.cfa.register.reg = try std.leb.readULEB128(u8, reader);
    } else if (instruction == DW.CFA.def_cfa_offset) {
        state.cfa.register.offset = @intCast(isize, try std.leb.readULEB128(usize, reader));
    } else if (instruction == DW.CFA.def_cfa_offset_sf) {
        const factored_offset = try std.leb.readILEB128(isize, reader);
        state.cfa.register.offset = factored_offset * self.data_alignment_factor;
    } else if (instruction == DW.CFA.def_cfa_expression) {
        state.cfa.expression = try Dwarf.forms.readExprLoc(reader, DW.FORM.exprloc);
    }
    // register rule instructions
    else if (instruction == DW.CFA.@"undefined") {
        const reg = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = .{ .@"undefined" = {} };
    } else if (instruction == DW.CFA.same_value) {
        const reg = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = .{ .same_value = {} };
    } else if ((instruction & 0xc0) == DW.CFA.offset) {
        const reg = instruction & 0x3f;
        const offset = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = .{ .offset = @intCast(isize, offset) * self.data_alignment_factor };
    } else if (instruction == DW.CFA.offset_extended) {
        const reg = try std.leb.readULEB128(usize, reader);
        const offset = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = .{ .offset = @intCast(isize, offset) * self.data_alignment_factor };
    } else if (instruction == DW.CFA.offset_extended_sf) {
        const reg = try std.leb.readULEB128(usize, reader);
        const offset = try std.leb.readILEB128(isize, reader);
        state.regs[reg] = .{ .offset = offset * self.data_alignment_factor };
    } else if (instruction == DW.CFA.val_offset) {
        const reg = try std.leb.readULEB128(usize, reader);
        const offset = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = .{ .val_offset = @intCast(isize, offset) * self.data_alignment_factor };
    } else if (instruction == DW.CFA.val_offset_sf) {
        const reg = try std.leb.readULEB128(usize, reader);
        const offset = try std.leb.readILEB128(isize, reader);
        state.regs[reg] = .{ .val_offset = offset * self.data_alignment_factor };
    } else if (instruction == DW.CFA.register) {
        const reg = try std.leb.readULEB128(usize, reader);
        const target_reg = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = .{ .register = target_reg };
    } else if (instruction == DW.CFA.expression) {
        const reg = try std.leb.readULEB128(usize, reader);
        const expr = try Dwarf.forms.readBlock(reader, DW.FORM.block);
        state.regs[reg] = .{ .expression = expr };
        // TODO: Dwarf v5, chapter 6.4.1, pg. 180, line 17
    } else if (instruction == DW.CFA.val_expression) {
        const reg = try std.leb.readULEB128(usize, reader);
        const expr = try Dwarf.forms.readBlock(reader, DW.FORM.block);
        state.regs[reg] = .{ .val_expression = expr };
        // TODO: Dwarf v5, chapter 6.4.1, pg. 180, line 27
    } else if ((instruction & 0xc0) == DW.CFA.restore) {
        const reg = instruction & 0x3f;
        state.regs[reg] = initial_state.?.regs[reg];
    } else if (instruction == DW.CFA.restore_extended) {
        const reg = try std.leb.readULEB128(usize, reader);
        state.regs[reg] = initial_state.?.regs[reg];
    }
    // row state instructions
    else if (instruction == DW.CFA.remember_state) {
        std.debug.panic("TODO: CFA stack; {s}\n", .{DW.CFA.asStr(instruction)});
    } else if (instruction == DW.CFA.restore_state) {
        std.debug.panic("TODO: CFA stack; {s}\n", .{DW.CFA.asStr(instruction)});
    }
    // padding instruction
    else if (instruction == DW.CFA.nop) {
        //
    } else {
        std.debug.panic("unknown CFA instruction 0x{x}\n", .{instruction});
    }

    return new_state;
}
