const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

fn get_blocking_relative(comptime width: u20, buffer: []const u8, blockeds: []const u8, i: usize, k: usize, current: u8) u8 {
    const line_width = comptime width + 1;
    const relative = @max(blockeds[i * width + k], buffer[i * line_width + k]);
    return if (relative > current) relative else 0;
}

fn solution(comptime width: u20, buffer: []const u8) usize {
    var blocked_ups = std.mem.zeroes([width * width]u8);
    var blocked_downs = std.mem.zeroes([width * width]u8);
    var blocked_lefts = std.mem.zeroes([width * width]u8);
    var blocked_rights = std.mem.zeroes([width * width]u8);
    const line_width = comptime width + 1;

    // first, swipe both ways, marking where you found the taller trees
    var i: usize = 1;
    while (i < width - 1) : (i += 1) {
        var k: usize = 1;
        while (k < width - 1) : (k += 1) {
            const current_ch = buffer[i * line_width + k];
            blocked_ups[i * width + k] = get_blocking_relative(width, buffer, &blocked_ups, i - 1, k, current_ch);
            blocked_downs[i * width + k] = get_blocking_relative(width, buffer, &blocked_downs, i + 1, k, current_ch);
            blocked_lefts[i * width + k] = get_blocking_relative(width, buffer, &blocked_lefts, i, k - 1, current_ch);
            blocked_rights[i * width + k] = get_blocking_relative(width, buffer, &blocked_rights, i, k + 1, current_ch);
        }
    }

    // now, count the zeroes
    var zero_count: usize = 0;
    for (blocked_ups) |b| {
        if (b == 0) zero_count += 1;
    }
    for (blocked_downs) |b| {
        if (b == 0) zero_count += 1;
    }
    for (blocked_lefts) |b| {
        if (b == 0) zero_count += 1;
    }
    for (blocked_rights) |b| {
        if (b == 0) zero_count += 1;
    }

    return zero_count;
}

test "it works" {
    const inp = @embedFile("data/day08.txt");
    try std.testing.expectEqual(21, solution(5, inp));
}

pub fn main() !void {
    defer _ = util.gpa_impl.deinit();

    const buffer: []u8 = try util.readInput();

    defer gpa.free(buffer);

    const sol = solution(5, buffer);

    std.log.debug("nonzero counts: {}\n", .{sol});
}

// Useful stdlib functions
const tokenize = std.mem.tokenize;
const split = std.mem.split;
const indexOf = std.mem.indexOfScalar;
const indexOfAny = std.mem.indexOfAny;
const indexOfStr = std.mem.indexOfPosLinear;
const lastIndexOf = std.mem.lastIndexOfScalar;
const lastIndexOfAny = std.mem.lastIndexOfAny;
const lastIndexOfStr = std.mem.lastIndexOfLinear;
const trim = std.mem.trim;
const sliceMin = std.mem.min;
const sliceMax = std.mem.max;

const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const min = std.math.min;
const min3 = std.math.min3;
const max = std.math.max;
const max3 = std.math.max3;

const print = std.debug.print;
const assert = std.debug.assert;

const sort = std.sort.sort;
const asc = std.sort.asc;
const desc = std.sort.desc;

// Generated from template/template.zig.
// Run `zig build generate` to update.
// Only unmodified days will be updated.
