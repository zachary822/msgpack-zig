const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;
const Writer = Io.Writer;

pub fn serialize(writer: *Writer, msg: anytype) !void {
    const T = @TypeOf(msg);

    switch (@typeInfo(T)) {
        .void, .null => {
            try writer.writeByte(0xc0);
            return;
        },
        .bool => {
            try writer.writeByte(if (msg) 0xc3 else 0xc2);
            return;
        },
        .int => |info| {
            if (info.bits > 64) @compileError("msgpack: integers wider than 64 bits are not supported: " ++ @typeName(T));
            if (msg < 0 and msg >= -32) {
                try writer.writeByte(@bitCast(@as(i8, @intCast(msg))));
                return;
            }
            if (msg >= 0 and msg < 128) {
                try writer.writeByte(@intCast(msg));
                return;
            }
            const storage_bits = comptime std.math.ceilPowerOfTwoAssert(u16, @max(info.bits, 8));
            const storage_bytes = storage_bits / 8;
            const Int = @Int(info.signedness, storage_bits);
            var buf: [storage_bytes]u8 = undefined;
            std.mem.writeInt(Int, &buf, @intCast(msg), .big);
            const tag: u8 = (if (info.signedness == .unsigned) 0xcc else 0xd0) + @as(u8, @intCast(@ctz(storage_bytes)));
            try writer.writeByte(tag);
            try writer.writeAll(&buf);
            return;
        },
        .float => |info| {
            if (info.bits > 64) @compileError("msgpack: floats wider than 64 bits are not supported: " ++ @typeName(T));
            const storage_bits = if (info.bits <= 32) 32 else 64;
            const storage_bytes = storage_bits / 8;
            const Int = if (storage_bits == 32) u32 else u64;
            var buf: [storage_bytes]u8 = undefined;
            std.mem.writeInt(Int, &buf, @bitCast(msg), .big);
            const tag = if (storage_bits == 32) 0xca else 0xcb;
            try writer.writeByte(tag);
            try writer.writeAll(&buf);
            return;
        },
        .comptime_int => {
            if (msg < std.math.minInt(i64) or msg > std.math.maxInt(u64))
                @compileError("msgpack: integers wider than 64 bits are not supported: " ++ @typeName(T));
            if (msg < 0) {
                return serialize(writer, @as(i64, msg));
            }
            return serialize(writer, @as(u64, msg));
        },
        .comptime_float => return serialize(writer, @as(f64, msg)),
        .optional => {
            if (msg) |m| return serialize(writer, m);
            return serialize(writer, null);
        },
        .pointer => |info| {
            if (info.size == .slice) return serializeSequence(writer, msg);
            return serialize(writer, msg.*);
        },
        .array => return serializeSequence(writer, msg),
        .@"enum" => |info| {
            if (info.mode == .exhaustive) return serialize(writer, @tagName(msg));
            return serialize(writer, @backingInt(msg));
        },
        .@"struct" => |info| {
            if (info.is_tuple) {
                try writeArrayHeader(writer, info.field_names.len);
                inline for (info.field_names) |name| {
                    try serialize(writer, @field(msg, name));
                }
                return;
            }

            try writeMapHeader(writer, info.field_names.len);
            inline for (info.field_names) |name| {
                try serialize(writer, name);
                try serialize(writer, @field(msg, name));
            }
            return;
        },
        else => unreachable,
    }
    unreachable;
}

fn serializeSequence(writer: *Writer, msg: anytype) !void {
    const Child = switch (@typeInfo(@TypeOf(msg))) {
        .array => |info| info.child,
        .pointer => |info| info.child,
        else => unreachable,
    };
    const values = msg[0..];

    if (values.len > std.math.maxInt(u32)) return error.TooLong;
    if (Child == u8) {
        const is_unicode = std.unicode.utf8ValidateSlice(values);
        if (values.len < 32 and is_unicode) {
            try writer.writeByte(@as(u8, @intCast(values.len)) | 0b10100000);
        } else if (values.len <= std.math.maxInt(u8)) {
            try writer.writeAll(&.{ if (is_unicode) 0xd9 else 0xc4, @intCast(values.len) });
        } else if (values.len <= std.math.maxInt(u16)) {
            try writer.writeByte(if (is_unicode) 0xda else 0xc5);
            var buf: [2]u8 = undefined;
            std.mem.writeInt(u16, &buf, @intCast(values.len), .big);
            try writer.writeAll(&buf);
        } else if (values.len <= std.math.maxInt(u32)) {
            try writer.writeByte(if (is_unicode) 0xdb else 0xc6);
            var buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &buf, @intCast(values.len), .big);
            try writer.writeAll(&buf);
        }
        try writer.writeAll(values);
        return;
    }
    try writeArrayHeader(writer, values.len);
    for (values) |e| try serialize(writer, e);
}

fn writeArrayHeader(writer: *Writer, len: usize) !void {
    if (len > std.math.maxInt(u32)) return error.TooLong;
    if (len < 16) {
        try writer.writeByte(@as(u8, @intCast(len)) | 0b10010000);
    } else if (len <= std.math.maxInt(u16)) {
        try writer.writeByte(0xdc);
        var buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &buf, @intCast(len), .big);
        try writer.writeAll(&buf);
    } else if (len <= std.math.maxInt(u32)) {
        try writer.writeByte(0xdd);
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @intCast(len), .big);
        try writer.writeAll(&buf);
    }
}

fn writeMapHeader(writer: *Writer, len: usize) !void {
    if (len > std.math.maxInt(u32)) return error.TooLong;
    if (len < 16) {
        try writer.writeByte(@as(u8, @intCast(len)) | 0b10000000);
    } else if (len <= std.math.maxInt(u16)) {
        try writer.writeByte(0xde);
        var buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &buf, @intCast(len), .big);
        try writer.writeAll(&buf);
    } else if (len <= std.math.maxInt(u32)) {
        try writer.writeByte(0xdf);
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @intCast(len), .big);
        try writer.writeAll(&buf);
    }
}

pub fn deserializeAlloc(comptime T: type, gpa: Allocator, reader: *Io.Reader) !T {
    switch (@typeInfo(T)) {
        .bool => return switch (try reader.takeByte()) {
            0xc2 => false,
            0xc3 => true,
            else => error.BadValue,
        },
        .int => {
            const b = try reader.takeByte();
            // uint
            if ((b & 0b10000000) == 0) return @intCast(b);
            // int
            if ((b & 0b11100000) == 0b11100000) return @intCast(@as(i8, @bitCast(b)));
            return switch (b) {
                // uint
                0xcc => @intCast(try reader.takeByte()),
                0xcd => @intCast(try reader.takeInt(u16, .big)),
                0xce => @intCast(try reader.takeInt(u32, .big)),
                0xcf => @intCast(try reader.takeInt(u64, .big)),
                // int
                0xd0 => @intCast(try reader.takeInt(i8, .big)),
                0xd1 => @intCast(try reader.takeInt(i16, .big)),
                0xd2 => @intCast(try reader.takeInt(i32, .big)),
                0xd3 => @intCast(try reader.takeInt(i64, .big)),

                else => error.BadValue,
            };
        },
        .float => return switch (try reader.takeByte()) {
            0xca => @floatCast(@as(f32, @bitCast(try reader.takeInt(u32, .big)))),
            0xcb => @floatCast(@as(f64, @bitCast(try reader.takeInt(u64, .big)))),
            else => error.BadValue,
        },
        .optional => |info| {
            if (try reader.peekByte() == 0xc0) {
                reader.toss(1);
                return null;
            }
            return try deserializeAlloc(info.child, gpa, reader);
        },
        .pointer => |info| {
            if (info.size == .slice) return deserializeSequence(T, gpa, reader);
            return error.BadValue;
        },
        .array => |info| {
            var out: T = undefined;
            const b = try reader.takeByte();
            if (info.child == u8) {
                if (try strLen(b, reader)) |n| {
                    if (n != info.len) return error.BadValue;
                    try reader.readSliceAll(&out);
                    return out;
                }
            }
            const n = try arrLen(b, reader) orelse return error.BadValue;
            if (n != info.len) return error.BadValue;
            for (&out) |*o| {
                o.* = try deserializeAlloc(info.child, gpa, reader);
            }
            return out;
        },
        else => @compileError("msgpack: unsupported type " ++ @typeName(T)),
    }
    unreachable;
}

fn deserializeSequence(comptime T: type, gpa: Allocator, reader: *Io.Reader) !T {
    const Child = std.meta.Child(T);
    const b = try reader.takeByte();
    if (try arrLen(b, reader)) |n| {
        const out = try gpa.alloc(Child, n);
        errdefer gpa.free(out);
        for (out) |*o| {
            o.* = try deserializeAlloc(Child, gpa, reader);
        }
        return out;
    }
    if (Child != u8) return error.BadValue;
    const n = try strLen(b, reader) orelse return error.BadValue;
    return try reader.readAlloc(gpa, n);
}

fn strLen(b: u8, reader: *Io.Reader) !?usize {
    if (b & 0b11100000 == 0b10100000) {
        return b & 0b00011111;
    }
    return switch (b) {
        0xd9, 0xc4 => @intCast(try reader.takeByte()),
        0xda, 0xc5 => @intCast(try reader.takeInt(u16, .big)),
        0xdb, 0xc6 => @intCast(try reader.takeInt(u32, .big)),
        else => null,
    };
}

fn arrLen(b: u8, reader: *Io.Reader) !?usize {
    if (b & 0b11110000 == 0b10010000)
        return b & 0b00001111;
    return switch (b) {
        0xdc => @intCast(try reader.takeInt(u16, .big)),
        0xdd => @intCast(try reader.takeInt(u32, .big)),
        else => null,
    };
}

test serialize {
    const gpa = std.testing.allocator;

    var w: Io.Writer.Allocating = .init(gpa);
    defer w.deinit();

    const writer = &w.writer;

    try serialize(writer, @as(u64, 8589934591));
    try std.testing.expectEqualStrings(&.{ 0xcf, 0x00, 0x00, 0x00, 0x01, 0xff, 0xff, 0xff, 0xff }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, null);
    try std.testing.expectEqualStrings(&.{0xc0}, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, @as(f64, 0.64));
    try std.testing.expectEqualStrings(&.{ 0xcb, 0x3f, 0xe4, 0x7a, 0xe1, 0x47, 0xae, 0x14, 0x7b }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, @as(?u64, null));
    try std.testing.expectEqualStrings(&.{0xc0}, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, @as(?u64, 8589934591));
    try std.testing.expectEqualStrings(&.{ 0xcf, 0x00, 0x00, 0x00, 0x01, 0xff, 0xff, 0xff, 0xff }, w.written());
    w.clearRetainingCapacity();

    const a: ?u64 = 8589934591;

    try serialize(writer, &a);
    try std.testing.expectEqualStrings(&.{ 0xcf, 0x00, 0x00, 0x00, 0x01, 0xff, 0xff, 0xff, 0xff }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, "abc");
    try std.testing.expectEqualStrings(&.{ 0b10100011, 0x61, 0x62, 0x63 }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, "Str format family stores a byte array in 1, 2, 3, or 5 bytes of extra bytes in addition to the size of the byte array.");
    try std.testing.expectEqualStrings(&.{ 0xd9, 0x76, 0x53, 0x74, 0x72, 0x20, 0x66, 0x6f, 0x72, 0x6d, 0x61, 0x74, 0x20, 0x66, 0x61, 0x6d, 0x69, 0x6c, 0x79, 0x20, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x73, 0x20, 0x61, 0x20, 0x62, 0x79, 0x74, 0x65, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x69, 0x6e, 0x20, 0x31, 0x2c, 0x20, 0x32, 0x2c, 0x20, 0x33, 0x2c, 0x20, 0x6f, 0x72, 0x20, 0x35, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x6f, 0x66, 0x20, 0x65, 0x78, 0x74, 0x72, 0x61, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x69, 0x6e, 0x20, 0x61, 0x64, 0x64, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x74, 0x6f, 0x20, 0x74, 0x68, 0x65, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x20, 0x6f, 0x66, 0x20, 0x74, 0x68, 0x65, 0x20, 0x62, 0x79, 0x74, 0x65, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x2e }, w.written());
    w.clearRetainingCapacity();

    const arr = [_]u64{ 1, 2, 3 };
    try serialize(writer, arr);
    try std.testing.expectEqualStrings(&.{ 0x93, 0x01, 0x02, 0x03 }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, &.{ 1, 2, 3 });
    try std.testing.expectEqualStrings(&.{ 0x93, 0x01, 0x02, 0x03 }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, .{ .key = 3 });
    try std.testing.expectEqualStrings(&.{ 0x81, 0xa3, 0x6b, 0x65, 0x79, 0x03 }, w.written());
    w.clearRetainingCapacity();

    const S = struct {
        key: i64,
    };
    try serialize(writer, S{ .key = 3 });
    try std.testing.expectEqualStrings(&.{ 0x81, 0xa3, 0x6b, 0x65, 0x79, 0x03 }, w.written());
    w.clearRetainingCapacity();
}

test deserializeAlloc {
    const gpa = std.testing.allocator;
    {
        var r: Io.Reader = .fixed(&.{0xc0});
        try std.testing.expectEqual(null, try deserializeAlloc(?u8, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{0xc3});
        try std.testing.expectEqual(true, try deserializeAlloc(bool, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{0xfd});
        try std.testing.expectEqual(-3, try deserializeAlloc(i8, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{ 0xcf, 0x00, 0x00, 0x00, 0x01, 0xff, 0xff, 0xff, 0xff });
        try std.testing.expectEqual(8589934591, try deserializeAlloc(u64, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{ 0xcb, 0x3f, 0xe4, 0x7a, 0xe1, 0x47, 0xae, 0x14, 0x7b });
        try std.testing.expectEqual(0.64, try deserializeAlloc(f64, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{ 0b10100011, 0x61, 0x62, 0x63 });
        const out = try deserializeAlloc([]const u8, gpa, &r);
        defer gpa.free(out);
        try std.testing.expectEqualStrings("abc", out);
    }
    {
        var r: Io.Reader = .fixed(&.{ 0b10100011, 0x61, 0x62, 0x63 });
        const out = try deserializeAlloc([3]u8, gpa, &r);
        try std.testing.expectEqualStrings("abc", &out);
    }
    {
        var r: Io.Reader = .fixed(&.{ 0xd9, 0x76, 0x53, 0x74, 0x72, 0x20, 0x66, 0x6f, 0x72, 0x6d, 0x61, 0x74, 0x20, 0x66, 0x61, 0x6d, 0x69, 0x6c, 0x79, 0x20, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x73, 0x20, 0x61, 0x20, 0x62, 0x79, 0x74, 0x65, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x69, 0x6e, 0x20, 0x31, 0x2c, 0x20, 0x32, 0x2c, 0x20, 0x33, 0x2c, 0x20, 0x6f, 0x72, 0x20, 0x35, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x6f, 0x66, 0x20, 0x65, 0x78, 0x74, 0x72, 0x61, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x69, 0x6e, 0x20, 0x61, 0x64, 0x64, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x74, 0x6f, 0x20, 0x74, 0x68, 0x65, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x20, 0x6f, 0x66, 0x20, 0x74, 0x68, 0x65, 0x20, 0x62, 0x79, 0x74, 0x65, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x2e });
        const out = try deserializeAlloc([]const u8, gpa, &r);
        defer gpa.free(out);
        try std.testing.expectEqualStrings(
            "Str format family stores a byte array in 1, 2, 3, or 5 bytes of extra bytes in addition to the size of the byte array.",
            out,
        );
    }
    {
        var r: Io.Reader = .fixed(&.{ 0x93, 0x01, 0x02, 0x03 });
        const out = try deserializeAlloc([]u8, gpa, &r);
        defer gpa.free(out);
        try std.testing.expectEqualStrings(&.{ 1, 2, 3 }, out);
    }
    {
        var r: Io.Reader = .fixed(&.{ 0xdc, 0x00, 0x03, 0x01, 0xcd, 0x01, 0x00, 0x03 });
        try std.testing.expectEqual([3]u64{ 1, 256, 3 }, try deserializeAlloc([3]u64, gpa, &r));
    }
}
