const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

const data = @embedFile("data/day01.txt");

fn parseCalories(it_: std.mem.TokenIterator(u8), alloc: std.mem.Allocator) !std.ArrayList(u32) {
    var list = std.ArrayList(u32).init(alloc);
    var it = it_;
    errdefer list.deinit();

    var current_sum: u32 = 0;

    while (it.next()) |line| {
        current_sum = if (line.len == 0) b: {
            try list.append(current_sum);
            break :b 0;
        } else current_sum + try parseInt(u32, line, 10);
    }
    try list.append(current_sum);
    return list;
}

pub fn main() !void {}

test "parsing calories" {
    const all = "4000\n2000\n\n3000";

    const input = tokenize(u8, all, "\n");
    const calories: std.ArrayList(u32) = try parseCalories(input, std.testing.allocator);

    const items = calories.items;

    try std.testing.expectEqual(items.len, 2);
    try std.testing.expectEqual(items[0], 6000);
    try std.testing.expectEqual(items[1], 3000);
}

test "parsing lines" {
    const cal = "42384";

    const a = parseInt(u32, cal, 10);
    try std.testing.expectEqual(a, 42384);
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
