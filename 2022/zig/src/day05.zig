const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

const data = @embedFile("data/day05.txt");

const MoveReq = struct { count: u6, from: u4, to: u4 };

// max number for count is 36, so 6 bits needed.
// the cages are numbered 1-9 so 0-8 -> 4 bits of data needed.
fn parse_line(input: []const u8) !MoveReq {
    var it = tokenize(u8, input, " ");

    _ = it.next().?;
    const count = try parseInt(u6, it.next().?, 10);
    _ = it.next().?;
    const from = try parseInt(u4, it.next().?, 10) - 1;
    _ = it.next().?;
    const to = try parseInt(u4, it.next().?, 10) - 1;

    return MoveReq{ .count = count, .from = from, .to = to };
}

pub fn main() !void {
    defer _ =util.gpa_impl.deinit();

    if (std.os.argv.len != 2) {
        std.log.err("please tell me if it's part 2 or 1 using <1> or <2>\n", .{});
        return;
    }

    const part_len = std.mem.indexOfSentinel(u8, 0, std.os.argv[1]);
    const whichPart = (parseInt(u2, std.os.argv[1][0..part_len], 10) catch
        1);

    var buffer = try util.readInput();
    defer gpa.destroy(buffer);

    var stacks = std.mem.zeroes([9]std.ArrayListUnmanaged(u8));

    defer for (stacks) |*m| m.deinit(gpa);

    var tok = tokenize(u8, buffer, "\n");

    {
        var i: u6 = 0;
        while (i < 8) : (i += 1) {
            const line = tok.next().?;

            for (stacks) |*m, k| {
                const ch = line[k * 4 + 1];
                if (ch != ' ') try m.append(gpa, ch);
            }
        }
    }

    for (stacks) |*m| std.mem.reverse(u8, m.items);

    // ignore the next line since it's the 1..9 line
    _ = tok.next().?;

    if (whichPart == 1) {
        while (tok.next()) |line| {
            const req = try parse_line(line);
            var i: u6 = 0;
            while (i < req.count) : (i += 1) {
                if (stacks[req.from].popOrNull()) |elem| {
                    try stacks[req.to].append(gpa, elem);
                }
            }
        }
    } else {
        while (tok.next()) |line| {
            const req = try parse_line(line);

            const from = stacks[req.from].items;
            const len = @min(@as(usize, req.count), stacks[req.from].items.len);

            try stacks[req.to].appendSlice(gpa, from[from.len - len .. from.len]);
            try stacks[req.from].resize(gpa, from.len - len);
        }
    }

    var message: [9]u8 = undefined;
    for (stacks) |*m, k| {
        const value = m.popOrNull() orelse ' ';
        message[k] = value;
    }

    std.debug.print("{s}\n", .{message});
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
