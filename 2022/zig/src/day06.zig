const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

fn solution(input: []const u8, window_len: usize) usize {
    var i: usize = 0;
    nextwin: while (i < input.len - window_len) : (i += 1) {
        const window = input[i .. i + window_len];
        for (window) |c, k| {
            for (window[0..k]) |p| if (p == c) continue :nextwin;
        } else break;
    }

    return i + window_len;
}

pub fn main() !void {
    defer _ = util.gpa_impl.deinit();

    const buffer = try util.readInput();
    defer gpa.free(buffer);

    const input = buffer[0 .. buffer.len - 1];
    std.debug.print("{s}\n", .{input});
    const part = (if (std.os.argv.len != 2) null else parseInt(u2, util.argToU8Arr(std.os.argv[1]), 10) catch null) orelse {
        std.log.err("Hey, I need to know if part 1 or part 2 (1 or 2). Thanks!\n", .{});
        return;
    };

    const window_len: usize = if (part == 1) 4 else 14;

    const start_index = solution(input, window_len);

    std.debug.print("index: {}\n", .{start_index});
}

test "part 1" {
    try std.testing.expectEqual(solution("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), 7);
    try std.testing.expectEqual(solution("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), 5);
    try std.testing.expectEqual(solution("nppdvjthqldpwncqszvftbrmjlhg", 4), 6);
    try std.testing.expectEqual(solution("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), 10);
    try std.testing.expectEqual(solution("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), 11);
}

test "part 2" {
    try std.testing.expectEqual(solution("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), 19);
    try std.testing.expectEqual(solution("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), 23);
    try std.testing.expectEqual(solution("nppdvjthqldpwncqszvftbrmjlhg", 14), 23);
    try std.testing.expectEqual(solution("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), 29);
    try std.testing.expectEqual(solution("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), 26);
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
