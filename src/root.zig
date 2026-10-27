const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;
const Writer = Io.Writer;

pub fn serialize(writer: *Writer, msg: anytype) !void {
    const T = @TypeOf(msg);

    switch (@typeInfo(T)) {
        .void, .null => try writer.writeByte(0xc0),
        .bool => try writer.writeByte(if (msg) 0xc3 else 0xc2),
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
            const tag: u8 = (if (info.signedness == .unsigned) 0xcc else 0xd0) + @as(u8, @intCast(@ctz(storage_bytes)));
            try writer.writeByte(tag);
            try writer.writeInt(Int, @intCast(msg), .big);
        },
        .float => |info| {
            if (info.bits > 64) @compileError("msgpack: floats wider than 64 bits are not supported: " ++ @typeName(T));
            const F = if (info.bits <= 32) f32 else f64;
            try writer.writeByte(if (F == f32) 0xca else 0xcb);
            try writer.writeInt(@Int(.unsigned, @bitSizeOf(F)), @bitCast(@as(F, @floatCast(msg))), .big);
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
            if (T == Io.Timestamp) {
                const ns = msg.toNanoseconds();
                const sec: i64 = @intCast(@divFloor(ns, std.time.ns_per_s));
                const nsec: u32 = @intCast(@mod(ns, std.time.ns_per_s));
                if (sec >= 0 and sec >> 34 == 0) {
                    if (nsec == 0 and sec <= std.math.maxInt(u32)) {
                        try writer.writeAll(&.{ 0xd6, 0xff });
                        try writer.writeInt(u32, @intCast(sec), .big);
                    } else {
                        try writer.writeAll(&.{ 0xd7, 0xff });
                        try writer.writeInt(u64, (@as(u64, nsec) << 34) | @as(u64, @intCast(sec)), .big);
                    }
                } else {
                    try writer.writeAll(&.{ 0xc7, 12, 0xff });
                    try writer.writeInt(u32, nsec, .big);
                    try writer.writeInt(i64, sec, .big);
                }
                return;
            }
            if (info.is_tuple) {
                try writeHeader(writer, info.field_names.len, 0x90, 0xdc);
                inline for (info.field_names) |name| {
                    try serialize(writer, @field(msg, name));
                }
                return;
            }

            try writeHeader(writer, info.field_names.len, 0x80, 0xde);
            inline for (info.field_names) |name| {
                try serialize(writer, name);
                try serialize(writer, @field(msg, name));
            }
        },
        else => @compileError("msgpack: type not supported: " ++ @typeName(T)),
    }
}

fn serializeSequence(writer: *Writer, msg: anytype) !void {
    const Child = switch (@typeInfo(@TypeOf(msg))) {
        .array => |info| info.child,
        .pointer => |info| info.child,
        else => unreachable,
    };
    const values = msg[0..];

    if (Child == u8) {
        if (values.len > std.math.maxInt(u32)) return error.TooLong;
        const is_unicode = std.unicode.utf8ValidateSlice(values);
        if (values.len < 32 and is_unicode) {
            try writer.writeByte(@as(u8, @intCast(values.len)) | 0b10100000);
        } else if (values.len <= std.math.maxInt(u8)) {
            try writer.writeAll(&.{ if (is_unicode) 0xd9 else 0xc4, @intCast(values.len) });
        } else if (values.len <= std.math.maxInt(u16)) {
            try writer.writeByte(if (is_unicode) 0xda else 0xc5);
            try writer.writeInt(u16, @intCast(values.len), .big);
        } else {
            try writer.writeByte(if (is_unicode) 0xdb else 0xc6);
            try writer.writeInt(u32, @intCast(values.len), .big);
        }
        try writer.writeAll(values);
        return;
    }
    try writeHeader(writer, values.len, 0x90, 0xdc);
    for (values) |e| try serialize(writer, e);
}

fn writeHeader(writer: *Writer, len: usize, fix: u8, tag16: u8) !void {
    if (len > std.math.maxInt(u32)) return error.TooLong;
    if (len < 16) {
        try writer.writeByte(fix | @as(u8, @intCast(len)));
    } else if (len <= std.math.maxInt(u16)) {
        try writer.writeByte(tag16);
        try writer.writeInt(u16, @intCast(len), .big);
    } else {
        try writer.writeByte(tag16 + 1);
        try writer.writeInt(u32, @intCast(len), .big);
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
            const v: i128 =
                if ((b & 0b10000000) == 0) // uint
                    b
                else if ((b & 0b11100000) == 0b11100000) // int
                    @as(i8, @bitCast(b))
                else switch (b) {
                    // uint
                    0xcc => try reader.takeByte(),
                    0xcd => try reader.takeInt(u16, .big),
                    0xce => try reader.takeInt(u32, .big),
                    0xcf => try reader.takeInt(u64, .big),
                    // int
                    0xd0 => try reader.takeInt(i8, .big),
                    0xd1 => try reader.takeInt(i16, .big),
                    0xd2 => try reader.takeInt(i32, .big),
                    0xd3 => try reader.takeInt(i64, .big),

                    else => return error.BadValue,
                };
            return std.math.cast(T, v) orelse error.BadValue;
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
            var done: usize = 0;
            errdefer for (out[0..done]) |o| free(gpa, o);
            for (&out) |*o| {
                o.* = try deserializeAlloc(info.child, gpa, reader);
                done += 1;
            }
            return out;
        },
        .@"struct" => |info| {
            if (T == Io.Timestamp) {
                const b = try reader.takeByte();
                var sec: i64 = undefined;
                var nsec: u32 = 0;
                switch (b) {
                    0xd6 => {
                        if (try reader.takeByte() != 0xff) return error.BadValue;
                        sec = try reader.takeInt(u32, .big);
                    },
                    0xd7 => {
                        if (try reader.takeByte() != 0xff) return error.BadValue;
                        const data = try reader.takeInt(u64, .big);
                        sec = @intCast(data & ((1 << 34) - 1));
                        nsec = @intCast(data >> 34);
                    },
                    0xc7 => {
                        const hdr = try reader.takeArray(2);
                        if (!std.mem.eql(u8, hdr, &.{ 12, 0xff })) return error.BadValue;
                        nsec = try reader.takeInt(u32, .big);
                        sec = try reader.takeInt(i64, .big);
                    },
                    else => return error.BadValue,
                }
                if (nsec >= std.time.ns_per_s) return error.BadValue;
                return Io.Timestamp.fromNanoseconds(@as(i96, sec) * std.time.ns_per_s + nsec);
            }

            inline for (info.field_attrs) |attrs| {
                if (attrs.@"comptime") @compileError("msgpack: types with comptime fields are not supported: " ++ @typeName(T));
            }
            var out: T = undefined;
            const b = try reader.takeByte();

            if (info.is_tuple) {
                const n = try arrLen(b, reader) orelse return error.BadValue;
                if (n != info.field_names.len) return error.BadValue;

                var done: usize = 0;

                errdefer inline for (info.field_names, 0..) |name, idx| {
                    if (idx < done) free(gpa, @field(out, name));
                };
                inline for (info.field_names, info.field_types) |name, FT| {
                    @field(out, name) = try deserializeAlloc(FT, gpa, reader);
                    done += 1;
                }

                return out;
            }

            const n = try mapLen(b, reader) orelse return error.BadValue;

            if (n > info.field_names.len) return error.BadValue;

            var seen: std.StaticBitSet(info.field_names.len) = .empty;
            errdefer inline for (info.field_names, 0..) |name, idx| {
                if (seen.isSet(idx)) free(gpa, @field(out, name));
            };
            for (0..n) |_| {
                const kb = try reader.takeByte();
                const klen = try strLen(kb, reader) orelse return error.BadValue;
                var buf: [maxNameLen(info.field_names)]u8 = undefined;
                if (klen > buf.len) return error.BadValue;
                const key = buf[0..klen];
                try reader.readSliceAll(key);

                const field = std.meta.stringToEnum(std.meta.FieldEnum(T), key) orelse return error.BadValue;
                switch (field) {
                    inline else => |f| {
                        const idx = @backingInt(f);
                        if (seen.isSet(idx)) return error.BadValue;
                        seen.set(idx);
                        @field(out, @tagName(f)) = try deserializeAlloc(@FieldType(T, @tagName(f)), gpa, reader);
                    },
                }
            }
            inline for (info.field_names, info.field_types, info.field_attrs, 0..) |name, FT, attrs, idx| {
                if (!seen.isSet(idx)) {
                    if (comptime attrs.defaultValue(FT)) |d| {
                        @field(out, name) = d;
                    } else {
                        return error.MissingField;
                    }
                }
            }

            return out;
        },
        .@"enum" => |info| {
            if (info.mode == .nonexhaustive) {
                return @fromBackingInt(try deserializeAlloc(info.tag_type, gpa, reader));
            }
            const b = try reader.takeByte();
            const len = try strLen(b, reader) orelse return error.BadValue;
            var buf: [maxNameLen(info.field_names)]u8 = undefined;
            if (len > buf.len) return error.BadValue;
            const name = buf[0..len];
            try reader.readSliceAll(name);
            return std.meta.stringToEnum(T, name) orelse error.BadValue;
        },
        else => @compileError("msgpack: unsupported type " ++ @typeName(T)),
    }
    unreachable;
}

fn deserializeSequence(comptime T: type, gpa: Allocator, reader: *Io.Reader) !T {
    const Child = std.meta.Child(T);
    const b = try reader.takeByte();
    if (try arrLen(b, reader)) |n| {
        var done: usize = 0;
        const out = try gpa.alloc(Child, n);
        errdefer {
            for (out[0..done]) |o| free(gpa, o);
            gpa.free(out);
        }
        for (out) |*o| {
            o.* = try deserializeAlloc(Child, gpa, reader);
            done += 1;
        }
        return out;
    }
    if (Child != u8) return error.BadValue;
    const n = try strLen(b, reader) orelse return error.BadValue;
    return try reader.readAlloc(gpa, n);
}

fn maxNameLen(comptime names: []const [:0]const u8) usize {
    var max: usize = 0;
    for (names) |name| max = @max(max, name.len);
    return max;
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

fn mapLen(b: u8, reader: *Io.Reader) !?usize {
    if (b & 0b11110000 == 0b10000000)
        return b & 0b00001111;

    return switch (b) {
        0xde => @intCast(try reader.takeInt(u16, .big)),
        0xdf => @intCast(try reader.takeInt(u32, .big)),
        else => null,
    };
}

pub fn free(gpa: Allocator, value: anytype) void {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .optional => if (value) |v| free(gpa, v),
        .pointer => |info| if (info.size == .slice) {
            for (value) |e| free(gpa, e);
            gpa.free(value);
        },
        .array => for (value) |e| free(gpa, e),
        .@"struct" => |info| inline for (info.field_names, info.field_attrs) |name, attrs| {
            if (!attrs.@"comptime") free(gpa, @field(value, name));
        },
        else => {},
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

    try serialize(writer, .{ .key = 3 });
    try std.testing.expectEqualStrings(&.{ 0x81, 0xa3, 0x6b, 0x65, 0x79, 0x03 }, w.written());
    w.clearRetainingCapacity();

    const S = struct {
        key: i64,
    };
    try serialize(writer, S{ .key = 3 });
    try std.testing.expectEqualStrings(&.{ 0x81, 0xa3, 0x6b, 0x65, 0x79, 0x03 }, w.written());
    w.clearRetainingCapacity();

    try serialize(writer, @as(f16, 1.5));
    try std.testing.expectEqualStrings(&.{ 0xca, 0x3f, 0xc0, 0x00, 0x00 }, w.written());
    w.clearRetainingCapacity();
}

test "serialize Io.Timestamp" {
    const cases = .{
        // timestamp 32
        .{ 0, &[_]u8{ 0xd6, 0xff, 0x00, 0x00, 0x00, 0x00 } },
        .{ 1_000_000_000, &[_]u8{ 0xd6, 0xff, 0x00, 0x00, 0x00, 0x01 } },
        // largest seconds value for timestamp 32
        .{ 4_294_967_295_000_000_000, &[_]u8{ 0xd6, 0xff, 0xff, 0xff, 0xff, 0xff } },
        // timestamp 64: nanoseconds part
        .{ 1_500_000_000, &[_]u8{ 0xd7, 0xff, 0x77, 0x35, 0x94, 0x00, 0x00, 0x00, 0x00, 0x01 } },
        // timestamp 64: seconds do not fit in u32
        .{ 4_294_967_296_000_000_000, &[_]u8{ 0xd7, 0xff, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00 } },
        // largest value for timestamp 64
        .{ 17_179_869_183_999_999_999, &[_]u8{ 0xd7, 0xff, 0xee, 0x6b, 0x27, 0xff, 0xff, 0xff, 0xff, 0xff } },
        // timestamp 96: seconds do not fit in 34 bits
        .{ 17_179_869_184_000_000_000, &[_]u8{ 0xc7, 0x0c, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00 } },
        // timestamp 96: negative, sec = -1 and nsec = 999999999
        .{ -1, &[_]u8{ 0xc7, 0x0c, 0xff, 0x3b, 0x9a, 0xc9, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff } },
        // timestamp 96: negative, sec = -2 and nsec = 500000000
        .{ -1_500_000_000, &[_]u8{ 0xc7, 0x0c, 0xff, 0x1d, 0xcd, 0x65, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe } },
    };
    inline for (cases) |c| {
        var buf: [16]u8 = undefined;
        var w: Writer = .fixed(&buf);
        try serialize(&w, Io.Timestamp.fromNanoseconds(c[0]));
        try std.testing.expectEqualStrings(c[1], w.buffered());
    }
}

test "deserializeAlloc Io.Timestamp" {
    const gpa = std.testing.allocator;
    {
        const cases = .{
            // timestamp 32
            .{ 0, &[_]u8{ 0xd6, 0xff, 0x00, 0x00, 0x00, 0x00 } },
            .{ 4_294_967_295_000_000_000, &[_]u8{ 0xd6, 0xff, 0xff, 0xff, 0xff, 0xff } },
            // timestamp 64
            .{ 1_500_000_000, &[_]u8{ 0xd7, 0xff, 0x77, 0x35, 0x94, 0x00, 0x00, 0x00, 0x00, 0x01 } },
            .{ 17_179_869_183_999_999_999, &[_]u8{ 0xd7, 0xff, 0xee, 0x6b, 0x27, 0xff, 0xff, 0xff, 0xff, 0xff } },
            // timestamp 64 for a value that fits in timestamp 32
            .{ 1_000_000_000, &[_]u8{ 0xd7, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 } },
            // timestamp 96
            .{ 17_179_869_184_000_000_000, &[_]u8{ 0xc7, 0x0c, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00 } },
            .{ -1, &[_]u8{ 0xc7, 0x0c, 0xff, 0x3b, 0x9a, 0xc9, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff } },
            .{ -1_500_000_000, &[_]u8{ 0xc7, 0x0c, 0xff, 0x1d, 0xcd, 0x65, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe } },
            // timestamp 96 for a value that fits in timestamp 32
            .{ 1_000_000_000, &[_]u8{ 0xc7, 0x0c, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 } },
        };
        inline for (cases) |c| {
            var r: Io.Reader = .fixed(c[1]);
            const out = try deserializeAlloc(Io.Timestamp, gpa, &r);
            try std.testing.expectEqual(@as(i96, c[0]), out.toNanoseconds());
        }
    }
    {
        const cases = .{
            // not an extension
            &[_]u8{0xc0},
            // extension type is not -1
            &[_]u8{ 0xd6, 0x01, 0x00, 0x00, 0x00, 0x00 },
            &[_]u8{ 0xd7, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
            &[_]u8{ 0xc7, 0x0c, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
            // ext 8 length is not 12
            &[_]u8{ 0xc7, 0x08, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
            // nsec = 1000000000 in timestamp 64
            &[_]u8{ 0xd7, 0xff, 0xee, 0x6b, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00 },
            // nsec = 1000000000 in timestamp 96
            &[_]u8{ 0xc7, 0x0c, 0xff, 0x3b, 0x9a, 0xca, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
        };
        inline for (cases) |c| {
            var r: Io.Reader = .fixed(c);
            try std.testing.expectError(error.BadValue, deserializeAlloc(Io.Timestamp, gpa, &r));
        }
    }
    {
        const cases = .{
            &[_]u8{ 0xd6, 0xff, 0x00, 0x00 },
            &[_]u8{ 0xd7, 0xff, 0x00, 0x00, 0x00, 0x00 },
            &[_]u8{ 0xc7, 0x0c },
            &[_]u8{ 0xc7, 0x0c, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00 },
        };
        inline for (cases) |c| {
            var r: Io.Reader = .fixed(c);
            try std.testing.expectError(error.EndOfStream, deserializeAlloc(Io.Timestamp, gpa, &r));
        }
    }
    {
        const cases = [_]i96{
            0,
            1,
            999_999_999,
            1_000_000_000,
            1_500_000_000,
            4_294_967_295_000_000_000,
            4_294_967_296_000_000_000,
            17_179_869_183_999_999_999,
            17_179_869_184_000_000_000,
            -1,
            -1_000_000_000,
            -1_500_000_000,
            @as(i96, std.math.maxInt(i64)) * std.time.ns_per_s + 999_999_999,
            @as(i96, std.math.minInt(i64)) * std.time.ns_per_s,
        };
        for (cases) |ns| {
            var buf: [16]u8 = undefined;
            var w: Writer = .fixed(&buf);
            try serialize(&w, Io.Timestamp.fromNanoseconds(ns));
            var r: Io.Reader = .fixed(w.buffered());
            const out = try deserializeAlloc(Io.Timestamp, gpa, &r);
            try std.testing.expectEqual(ns, out.toNanoseconds());
            try std.testing.expectEqual(0, r.bufferedLen());
        }
    }
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
    {
        const S = struct { key: u64 };
        var r: Io.Reader = .fixed(&.{ 0x81, 0xa3, 0x6b, 0x65, 0x79, 0x03 });
        const out = try deserializeAlloc(S, gpa, &r);
        try std.testing.expectEqualDeep(S{ .key = 3 }, out);
    }
    {
        const S = struct { a: u64, b: u64 };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa1, 'b', 0x02, 0xa1, 'a', 0x01 });
        try std.testing.expectEqual(S{ .a = 1, .b = 2 }, try deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { a: u64, b: u64 };
        var r: Io.Reader = .fixed(&.{ 0x81, 0xa1, 'a', 0x01 });
        try std.testing.expectError(error.MissingField, deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { a: u64, b: u64 };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa1, 'a', 0x01, 0xa1, 'a', 0x01 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { a: u64 = 5 };
        var r: Io.Reader = .fixed(&.{ 0x81, 0xa1, 'z', 0x01 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { a: u64, b: u64 = 7 };
        var r: Io.Reader = .fixed(&.{ 0x81, 0xa1, 'a', 0x01 });
        try std.testing.expectEqual(S{ .a = 1, .b = 7 }, try deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { a: u64, b: u64 };
        var buf: [64]u8 = undefined;
        var w: Writer = .fixed(&buf);
        try serialize(&w, S{ .a = 300, .b = 4 });
        var r: Io.Reader = .fixed(w.buffered());
        try std.testing.expectEqual(S{ .a = 300, .b = 4 }, try deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { s: []u8, x: u64 };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa1, 's', 0xa2, 'h', 'i', 0xa1, 'x', 0xc3 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(S, gpa, &r));
    }
    {
        const S = struct { x: u64, s: []u8 };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa1, 'x', 0xc3 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(S, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{ 0x92, 0xa2, 'h', 'i', 0xc3 });
        try std.testing.expectError(error.BadValue, deserializeAlloc([][]u8, gpa, &r));
    }
    {
        var r: Io.Reader = .fixed(&.{ 0x92, 0xa2, 'h', 'i', 0xc3 });
        try std.testing.expectError(error.BadValue, deserializeAlloc([2][]u8, gpa, &r));
    }
    {
        const S = struct { name: []u8, tags: [][]u8 };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa4, 'n', 'a', 'm', 'e', 0xa2, 'h', 'i', 0xa4, 't', 'a', 'g', 's', 0x92, 0xa1, 'x', 0xa1, 'y' });
        const out = try deserializeAlloc(S, gpa, &r);
        defer free(gpa, out);
        try std.testing.expectEqualStrings("hi", out.name);
        try std.testing.expectEqual(2, out.tags.len);
        try std.testing.expectEqualStrings("x", out.tags[0]);
        try std.testing.expectEqualStrings("y", out.tags[1]);
    }
    {
        const max_u64 = [_]u8{ 0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
        const min_i64 = [_]u8{ 0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
        const cases = .{
            .{ u8, &[_]u8{ 0xcd, 0x01, 0x00 } },
            .{ u8, &[_]u8{0xff} },
            .{ i8, &[_]u8{ 0xcc, 0x80 } },
            .{ i64, &max_u64 },
            .{ u64, &min_i64 },
            .{ u3, &[_]u8{0x08} },
        };
        inline for (cases) |c| {
            var r: Io.Reader = .fixed(c[1]);
            try std.testing.expectError(error.BadValue, deserializeAlloc(c[0], gpa, &r));
        }
    }
    {
        const cases = .{
            .{ @as(u64, std.math.maxInt(u64)), &[_]u8{ 0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff } },
            .{ @as(i64, std.math.minInt(i64)), &[_]u8{ 0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } },
            .{ @as(i8, -128), &[_]u8{ 0xd0, 0x80 } },
            .{ @as(i8, -1), &[_]u8{0xff} },
            .{ @as(u8, 5), &[_]u8{ 0xd0, 0x05 } },
            .{ @as(u3, 7), &[_]u8{0x07} },
            .{ @as(u0, 0), &[_]u8{0x00} },
        };
        inline for (cases) |c| {
            var r: Io.Reader = .fixed(c[1]);
            try std.testing.expectEqual(c[0], try deserializeAlloc(@TypeOf(c[0]), gpa, &r));
        }
    }
    {
        const cases = .{
            @as(i64, std.math.minInt(i64)),
            @as(i8, -33),
            @as(i8, -32),
            @as(u64, std.math.maxInt(u64)),
            @as(u16, 128),
            @as(i16, 300),
            @as(f16, 1.5),
        };
        inline for (cases) |v| {
            var buf: [16]u8 = undefined;
            var w: Writer = .fixed(&buf);
            try serialize(&w, v);
            var r: Io.Reader = .fixed(w.buffered());
            try std.testing.expectEqual(v, try deserializeAlloc(@TypeOf(v), gpa, &r));
        }
    }
    {
        const P = struct { u64, i8 };
        var buf: [16]u8 = undefined;
        var w: Writer = .fixed(&buf);
        try serialize(&w, P{ 300, -5 });
        var r: Io.Reader = .fixed(w.buffered());
        try std.testing.expectEqual(P{ 300, -5 }, try deserializeAlloc(P, gpa, &r));
    }
    {
        const P = struct { u64, u64 };
        var r: Io.Reader = .fixed(&.{ 0x93, 0x01, 0x02, 0x03 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(P, gpa, &r));
    }
    {
        const P = struct { u64, u64 };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa1, '0', 0x01, 0xa1, '1', 0x02 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(P, gpa, &r));
    }
    {
        const P = struct { []u8, u64 };
        var r: Io.Reader = .fixed(&.{ 0x92, 0xa2, 'h', 'i', 0xc3 });
        try std.testing.expectError(error.BadValue, deserializeAlloc(P, gpa, &r));
    }
    {
        const P = struct { []u8, [][]u8 };
        var r: Io.Reader = .fixed(&.{ 0x92, 0xa2, 'h', 'i', 0x91, 0xa1, 'x' });
        const out = try deserializeAlloc(P, gpa, &r);
        defer free(gpa, out);
        try std.testing.expectEqualStrings("hi", out[0]);
        try std.testing.expectEqual(1, out[1].len);
        try std.testing.expectEqualStrings("x", out[1][0]);
    }
    {
        const E = enum(u8) {
            a = 1,
        };
        var r: Io.Reader = .fixed(&.{ 0xa1, 'a' });
        try std.testing.expectEqual(E.a, try deserializeAlloc(E, gpa, &r));
    }
    {
        const E = enum { red, green };
        const N = enum(i8) { a = -3, _ };
        const cases = .{ E.red, E.green, @as(N, .a), @as(N, @fromBackingInt(100)), @as(N, @fromBackingInt(-128)) };
        inline for (cases) |v| {
            var buf: [16]u8 = undefined;
            var w: Writer = .fixed(&buf);
            try serialize(&w, v);
            var r: Io.Reader = .fixed(w.buffered());
            try std.testing.expectEqual(v, try deserializeAlloc(@TypeOf(v), gpa, &r));
        }
    }
    {
        const E = enum { red, green };
        const N = enum(i8) { a = -3, _ };
        const cases = .{
            // unknown tag name
            .{ E, &[_]u8{ 0xa4, 'b', 'l', 'u', 'e' } },
            // tag name with a different case
            .{ E, &[_]u8{ 0xa3, 'R', 'e', 'd' } },
            // integer for an exhaustive enum
            .{ E, &[_]u8{0x00} },
            // string for a non-exhaustive enum
            .{ N, &[_]u8{ 0xa1, 'a' } },
            // 200 does not fit in i8
            .{ N, &[_]u8{ 0xcc, 0xc8 } },
        };
        inline for (cases) |c| {
            var r: Io.Reader = .fixed(c[1]);
            try std.testing.expectError(error.BadValue, deserializeAlloc(c[0], gpa, &r));
        }
    }
    {
        const E = enum { red, green };
        const S = struct { color: E, tags: []E };
        var r: Io.Reader = .fixed(&.{ 0x82, 0xa5, 'c', 'o', 'l', 'o', 'r', 0xa5, 'g', 'r', 'e', 'e', 'n', 0xa4, 't', 'a', 'g', 's', 0x92, 0xa3, 'r', 'e', 'd', 0xa5, 'g', 'r', 'e', 'e', 'n' });
        const out = try deserializeAlloc(S, gpa, &r);
        defer free(gpa, out);
        try std.testing.expectEqual(E.green, out.color);
        try std.testing.expectEqualSlices(E, &.{ .red, .green }, out.tags);
    }
}
