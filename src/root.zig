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
        .pointer => return serialize(writer, msg.*),
        .array => |info| {
            if (msg.len > std.math.maxInt(u32)) return error.TooLong;
            if (info.child == u8) {
                const is_unicode = std.unicode.utf8ValidateSlice(&msg);
                if (msg.len < 32 and is_unicode) {
                    try writer.writeByte(@as(u8, msg.len) | 0b10100000);
                } else if (msg.len <= std.math.maxInt(u8)) {
                    try writer.writeAll(&.{ if (is_unicode) 0xd9 else 0xc4, @intCast(msg.len) });
                } else if (msg.len <= std.math.maxInt(u16)) {
                    try writer.writeByte(if (is_unicode) 0xda else 0xc5);
                    var buf: [2]u8 = undefined;
                    std.mem.writeInt(u16, &buf, @intCast(msg.len), .big);
                    try writer.writeAll(&buf);
                } else if (msg.len <= std.math.maxInt(u32)) {
                    try writer.writeByte(if (is_unicode) 0xdb else 0xc6);
                    var buf: [4]u8 = undefined;
                    std.mem.writeInt(u32, &buf, @intCast(msg.len), .big);
                    try writer.writeAll(&buf);
                }
                try writer.writeAll(&msg);
                return;
            }
            try writeArrayHeader(writer, msg.len);
            for (msg) |e| try serialize(writer, e);
            return;
        },
        .@"struct" => |info| {
            if (info.is_tuple) {
                try writeArrayHeader(writer, info.field_names.len);
                inline for (info.field_names) |name| {
                    try serialize(writer, @field(msg, name));
                }
                return;
            }
            // map is handled here
        },
        else => unreachable,
    }
    unreachable;
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
}
