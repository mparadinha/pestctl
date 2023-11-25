const std = @import("std");
const Allocator = std.mem.Allocator;
const Dwarf = @import("../Dwarf.zig");
const DW = @import("constants.zig");

const Expression = @This();

data: []const u8,
stack: BoundedStack(512),

const StackElement = union(enum) {
    generic: Generic,
    typed: Typed,

    pub const Generic = usize;
    pub const Typed = struct {
        type_offset: usize, // points to a DW_TAG_base_type
        byte_size: usize,
        bytes: []const u8, // points into `Expression.data`
    };
};

const max_registers = @typeInfo(Dwarf.Register).Enum.fields.len;

pub fn result(data: []const u8, registers: [max_registers]usize, frame_base_reg_idx: usize) !StackElement {
    var expr = Expression.init(data);

    var stream = std.io.fixedBufferStream(expr.data);
    var reader = stream.reader();
    while ((try stream.getPos()) < expr.data.len) {
        const op = try reader.readByte();
        try expr.executeOp(reader, op, registers, frame_base_reg_idx);
    }

    return expr.stack.top();
}

pub fn init(data: []const u8) Expression {
    return .{
        .data = data,
        .stack = BoundedStack(512).init(),
    };
}

const ReaderType = std.io.FixedBufferStream([]const u8).Reader;

// TODO: other things an expression might need to do its job:
// - a functions that reads an arbitrary amount of bytes from the child process at some address
//   (for DW_OP_deref and family)
// - the value of the CFA (for DW_OP_call_frame_cfa)

pub fn executeOp(
    self: *Expression,
    reader: ReaderType,
    op: u8,
    registers: [max_registers]usize,
    frame_base_reg_idx: usize,
) !void {
    switch (op) {
        DW.OP.addr => try self.stack.push(try reader.readIntLittle(usize)),
        DW.OP.deref => {
            std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)});
            const addr = (try self.stack.pop()).generic;
            _ = addr;
            try self.stack.push(@as(usize, undefined));
        },
        DW.OP.const1u => try self.stack.push(try reader.readIntLittle(u8)),
        DW.OP.const1s => try self.stack.push(try reader.readIntLittle(i8)),
        DW.OP.const2u => try self.stack.push(try reader.readIntLittle(u16)),
        DW.OP.const2s => try self.stack.push(try reader.readIntLittle(i16)),
        DW.OP.const4u => try self.stack.push(try reader.readIntLittle(u32)),
        DW.OP.const4s => try self.stack.push(try reader.readIntLittle(i32)),
        DW.OP.const8u => try self.stack.push(try reader.readIntLittle(u64)),
        DW.OP.const8s => try self.stack.push(try reader.readIntLittle(i64)),
        DW.OP.constu => try self.stack.push(try std.leb.readULEB128(usize, reader)),
        DW.OP.consts => try self.stack.push(try std.leb.readILEB128(isize, reader)),
        DW.OP.dup => try self.stack.pushElement(try self.stack.top()),
        DW.OP.drop => _ = try self.stack.pop(),
        DW.OP.over => {
            std.debug.assert(self.stack.used > 1);
            const elem = self.stack.buffer[self.stack.used - 2];
            try self.stack.pushElement(elem);
        },
        DW.OP.pick => {
            const idx = try reader.readByte();
            std.debug.assert(idx < self.stack.used);
            const elem = self.stack.buffer[self.stack.used - 1 - idx];
            try self.stack.pushElement(elem);
        },
        DW.OP.swap => {
            std.debug.assert(self.stack.used >= 2);
            const end = self.stack.used - 1;
            std.mem.swap(StackElement, &self.stack.buffer[end - 1], &self.stack.buffer[end]);
        },
        DW.OP.rot => {
            std.debug.assert(self.stack.used >= 3);
            const end = self.stack.used - 1;
            std.mem.swap(StackElement, &self.stack.buffer[end - 1], &self.stack.buffer[end]);
            std.mem.swap(StackElement, &self.stack.buffer[end - 2], &self.stack.buffer[end - 1]);
        },
        DW.OP.xderef => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.abs => {
            const value = (try self.stack.pop()).generic;
            const signed: isize = @bitCast(value);
            try self.stack.push(try std.math.absInt(signed));
        },
        DW.OP.@"and" => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_one & value_two);
        },
        DW.OP.div => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(@divTrunc(@as(isize, @bitCast(value_two)), @as(isize, @bitCast(value_one))));
        },
        DW.OP.minus => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two -% value_one);
        },
        DW.OP.mod => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two % value_one);
        },
        DW.OP.mul => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_one *% value_two);
        },
        DW.OP.neg => {
            const value = (try self.stack.pop()).generic;
            try self.stack.push(-@as(isize, @bitCast(value)));
        },
        DW.OP.not => {
            const value = (try self.stack.pop()).generic;
            try self.stack.push(~value);
        },
        DW.OP.@"or" => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two | value_one);
        },
        DW.OP.plus => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two +% value_one);
        },
        DW.OP.plus_uconst => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = try std.leb.readULEB128(usize, reader);
            try self.stack.push(value_two +% value_one);
        },
        DW.OP.shl => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two << @as(u6, @intCast(value_one)));
        },
        DW.OP.shr => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two >> @as(u6, @intCast(value_one)));
        },
        DW.OP.shra => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            const top_bit = value_two & 0x8000_0000_0000_0000;
            const shift_res = value_two >> @as(u6, @intCast(value_one));
            const arith_shift_res = top_bit | (shift_res & 0x7fff_ffff_ffff_ffff);
            try self.stack.push(arith_shift_res);
        },
        DW.OP.xor => {
            const value_one = (try self.stack.pop()).generic;
            const value_two = (try self.stack.pop()).generic;
            try self.stack.push(value_two ^ value_one);
        },
        DW.OP.bra => {
            const offset = try reader.readIntLittle(i16);
            const stack_top = (try self.stack.pop()).generic;
            if (stack_top != 0) try reader.context.seekBy(offset);
        },
        DW.OP.eq => {
            const value_one = @as(isize, @bitCast((try self.stack.pop()).generic));
            const value_two = @as(isize, @bitCast((try self.stack.pop()).generic));
            try self.stack.push(@as(usize, if (value_two == value_one) 1 else 0));
        },
        DW.OP.ge => {
            const value_one = @as(isize, @bitCast((try self.stack.pop()).generic));
            const value_two = @as(isize, @bitCast((try self.stack.pop()).generic));
            try self.stack.push(@as(usize, if (value_two >= value_one) 1 else 0));
        },
        DW.OP.gt => {
            const value_one = @as(isize, @bitCast((try self.stack.pop()).generic));
            const value_two = @as(isize, @bitCast((try self.stack.pop()).generic));
            try self.stack.push(@as(usize, if (value_two > value_one) 1 else 0));
        },
        DW.OP.le => {
            const value_one = @as(isize, @bitCast((try self.stack.pop()).generic));
            const value_two = @as(isize, @bitCast((try self.stack.pop()).generic));
            try self.stack.push(@as(usize, if (value_two <= value_one) 1 else 0));
        },
        DW.OP.lt => {
            const value_one = @as(isize, @bitCast((try self.stack.pop()).generic));
            const value_two = @as(isize, @bitCast((try self.stack.pop()).generic));
            try self.stack.push(@as(usize, if (value_two < value_one) 1 else 0));
        },
        DW.OP.ne => {
            const value_one = @as(isize, @bitCast((try self.stack.pop()).generic));
            const value_two = @as(isize, @bitCast((try self.stack.pop()).generic));
            try self.stack.push(@as(usize, if (value_two != value_one) 1 else 0));
        },
        DW.OP.skip => {
            const offset = try reader.readIntLittle(i16);
            try reader.context.seekBy(offset);
        },
        // zig fmt: off
        DW.OP.lit0, DW.OP.lit1, DW.OP.lit2, DW.OP.lit3,
        DW.OP.lit4, DW.OP.lit5, DW.OP.lit6, DW.OP.lit7,
        DW.OP.lit8, DW.OP.lit9, DW.OP.lit10, DW.OP.lit11,
        DW.OP.lit12, DW.OP.lit13, DW.OP.lit14, DW.OP.lit15,
        DW.OP.lit16, DW.OP.lit17, DW.OP.lit18, DW.OP.lit19,
        DW.OP.lit20, DW.OP.lit21, DW.OP.lit22, DW.OP.lit23,
        DW.OP.lit24, DW.OP.lit25, DW.OP.lit26, DW.OP.lit27,
        DW.OP.lit28, DW.OP.lit29, DW.OP.lit30, DW.OP.lit31,
        // zig fmt: on
        => {
            const lit = op - DW.OP.lit0;
            try self.stack.push(lit);
        },
        // zig fmt: off
        DW.OP.reg0, DW.OP.reg1, DW.OP.reg2, DW.OP.reg3,
        DW.OP.reg4, DW.OP.reg5, DW.OP.reg6, DW.OP.reg7,
        DW.OP.reg8, DW.OP.reg9, DW.OP.reg10, DW.OP.reg11,
        DW.OP.reg12, DW.OP.reg13, DW.OP.reg14, DW.OP.reg15,
        DW.OP.reg16, DW.OP.reg17, DW.OP.reg18, DW.OP.reg19,
        DW.OP.reg20, DW.OP.reg21, DW.OP.reg22, DW.OP.reg23,
        DW.OP.reg24, DW.OP.reg25, DW.OP.reg26, DW.OP.reg27,
        DW.OP.reg28, DW.OP.reg29, DW.OP.reg30, DW.OP.reg31,
        // zig fmt: on
        => {
            // these ops imply the final result is in some register. if we assume this is the
            // last op of the expression we just push it into the top of the stack
            const reg_code = op - DW.OP.reg0;
            try self.stack.push(registers[reg_code]);
            // note: there could be a DW_OP_piece after this instead
            std.debug.assert(reader.context.pos == self.data.len or self.data[reader.context.pos] == DW.OP.piece);
        },
        // zig fmt: off
        DW.OP.breg0, DW.OP.breg1, DW.OP.breg2, DW.OP.breg3,
        DW.OP.breg4, DW.OP.breg5, DW.OP.breg6, DW.OP.breg7,
        DW.OP.breg8, DW.OP.breg9, DW.OP.breg10, DW.OP.breg11,
        DW.OP.breg12, DW.OP.breg13, DW.OP.breg14,DW.OP.breg15,
        DW.OP.breg16, DW.OP.breg17, DW.OP.breg18, DW.OP.breg19,
        DW.OP.breg20, DW.OP.breg21, DW.OP.breg22, DW.OP.breg23,
        DW.OP.breg24, DW.OP.breg25, DW.OP.breg26, DW.OP.breg27,
        DW.OP.breg28, DW.OP.breg29, DW.OP.breg30, DW.OP.breg31,
        // zig fmt: on
        => {
            const offset = try std.leb.readILEB128(isize, reader);
            const breg = registers[op - DW.OP.breg0];
            const value = @as(usize, @intCast(@as(isize, @intCast(breg)) + offset));
            try self.stack.push(value);
        },
        DW.OP.regx => {
            // this OP implies the final result is in some register. if we assume this is the
            // last op of the expression we just push it into the top of the stack
            const reg_code = try std.leb.readULEB128(usize, reader);
            try self.stack.push(registers[reg_code]);
            std.debug.assert(reader.context.pos == self.data.len or self.data[reader.context.pos] == DW.OP.piece);
        },
        DW.OP.fbreg => {
            const offset = try std.leb.readILEB128(isize, reader);
            const fb_reg = registers[frame_base_reg_idx];
            const value = @as(usize, @intCast(@as(isize, @intCast(fb_reg)) + offset));
            try self.stack.push(value);
        },
        DW.OP.bregx => {
            const reg_code = try std.leb.readULEB128(usize, reader);
            const breg = registers[reg_code];
            const offset = try std.leb.readILEB128(isize, reader);
            const value = @as(usize, @intCast(@as(isize, @intCast(breg)) + offset));
            try self.stack.push(value);
        },
        DW.OP.piece => {
            std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)});
            // TODO: actually use this
            _ = try std.leb.readULEB128(usize, reader);
        },
        DW.OP.deref_size => {
            std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)});
            const addr = (try self.stack.pop()).generic;
            _ = addr;
            const byte_size = try reader.readByte();
            _ = byte_size;
            try self.stack.push(@as(usize, undefined));
        },
        DW.OP.xderef_size => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.nop => {},
        DW.OP.push_object_address => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.call2 => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.call4 => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.call_ref => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.form_tls_address => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.call_frame_cfa => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.bit_piece => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.implicit_value => {
            std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)});
            // TODO: actually use this
            const len = try std.leb.readULEB128(usize, reader);
            try reader.skipBytes(len, .{});
        },
        DW.OP.stack_value => {
            // this OP means that the object the user was looking for by executing
            // this expression is on top of the stack.
            // assuming there are no more ops after this one (which is the only thing
            // that makes sense to me) then the top of the stack will be returned as
            // the result. so we do nothing
            std.debug.assert(reader.context.pos == self.data.len or self.data[reader.context.pos] == DW.OP.piece);
        },
        DW.OP.implicit_pointer => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.addrx => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.constx => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.entry_value => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.const_type => {
            const type_offset = try std.leb.readULEB128(usize, reader);
            const byte_size = try reader.readByte();
            const cur_pos = reader.context.pos;
            const bytes = reader.context.buffer[cur_pos .. cur_pos + byte_size];
            try reader.skipBytes(byte_size, .{});
            try self.stack.pushTyped(.{
                .type_offset = type_offset,
                .byte_size = byte_size,
                .bytes = bytes,
            });
        },
        DW.OP.regval_type => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.deref_type => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.xderef_type => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.convert => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        DW.OP.reinterpret => std.debug.panic("TODO: {s}\n", .{DW.OP.asStr(op)}),
        else => std.debug.panic("unknown code DW_OP: 0x{x}\n", .{op}),
    }
}

fn BoundedStack(comptime max_size: usize) type {
    return struct {
        buffer: [max_size]StackElement,
        used: usize,

        const Self = @This();

        pub fn init() Self {
            return .{ .buffer = undefined, .used = 0 };
        }

        const Error = error{ BufferFull, BufferEmpty };

        pub fn push(self: *Self, item_generic: anytype) Error!void {
            const item = switch (@TypeOf(item_generic)) {
                u64, i64, usize, isize => @as(usize, @bitCast(item_generic)),
                comptime_int => @as(usize, item_generic),
                u8, u16, u32 => @as(usize, @intCast(item_generic)),
                i8 => @as(usize, @intCast(@as(u8, @bitCast(item_generic)))),
                i16 => @as(usize, @intCast(@as(u16, @bitCast(item_generic)))),
                i32 => @as(usize, @intCast(@as(u32, @bitCast(item_generic)))),
                else => @compileError(@typeName(@TypeOf(item_generic))),
            };
            return self.pushElement(.{ .generic = item });
        }

        pub fn pushTyped(self: *Self, item: StackElement.Typed) Error!void {
            return self.pushElement(.{ .typed = item });
        }

        pub fn pushElement(self: *Self, elem: StackElement) Error!void {
            if (self.used == self.buffer.len) return Error.BufferFull;
            self.buffer[self.used] = elem;
            self.used += 1;
        }

        pub fn pop(self: *Self) Error!StackElement {
            if (self.used == 0) return Error.BufferEmpty;
            const item = self.buffer[self.used - 1];
            self.used -= 1;
            return item;
        }

        pub fn top(self: Self) Error!StackElement {
            if (self.used == 0) return Error.BufferEmpty;
            return self.buffer[self.used - 1];
        }
    };
}
