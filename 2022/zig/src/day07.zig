const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

const data = @embedFile("data/day07.txt");

const DirEntry = struct {
    const Meta = struct { name: []const u8, after: usize };
    meta: Meta,
    file_size: usize,
};

pub fn main() !void {
    defer _ = util.gpa_impl.deinit();

    const buffer = try util.readInput();
    defer gpa.free(buffer);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var ally = arena.allocator();

    var tok = tokenize(u8, buffer, "\n");

    var entries = std.MultiArrayList(DirEntry){};
    defer entries.deinit(ally);

    try entries.append(ally, .{ .meta = DirEntry.Meta{ .name = ":root:", .after = 0 }, .file_size = 0 });

    var dir_stack = try std.ArrayListUnmanaged(usize).initCapacity(ally, 1);
    defer dir_stack.deinit(ally);
    dir_stack.appendAssumeCapacity(0);

    // the first line is the '/' directory
    _ = tok.next().?;
    while (tok.next()) |line| {
        if (std.mem.eql(u8, line, "$ ls")) continue; // we don't care about 'ls' since there's nothing to do here.
        const current_dir = dir_stack.items[dir_stack.items.len - 1];
        if (std.mem.startsWith(u8, line, "$ cd")) {
            const dir = line[5..line.len];

            const index = if (std.mem.eql(u8, dir, "..")) {
                _ = dir_stack.pop();
                continue;
            } else for (entries.items(.meta)) |meta, idx| {
                if (meta.after == current_dir and std.mem.eql(u8, meta.name, dir)) break idx;
            } else {
                std.log.err("should have already seen this directory", .{});
                return;
            };

            try dir_stack.append(ally, index);
        } else {
            if (std.mem.startsWith(u8, line, "dir")) {
                const listed_dir = line[4..line.len];
                // the traversal doesn't ls the same directory twice, even if it has the same name.
                // so this means that every directory entry that we get from an 'ls line' is unique.
                try entries.append(ally, .{ .meta = .{ .name = listed_dir, .after = current_dir }, .file_size = 0 });
            } else {
                const num = try parseInt(usize, line[0..std.mem.indexOfScalar(u8, line, ' ').?], 10);
                const file_sizes = entries.items(.file_size);
                for (dir_stack.items) |idx| file_sizes[idx] += num;
            }
        }
    }

    const slice = entries.slice();
    const sizes = slice.items(.file_size);

    std.debug.print("total size: (/) {}\n", .{sizes[0]});

    var sum: usize = 0;
    for (sizes) |s| {
        if (s <= 100_000) sum += s;
    }

    std.debug.print("sum of sizes < 100k: {}\n", .{sum});

    // part 2: find the the minimum size that can be removed
    const current_free_space = 70_000_000 - sizes[0];

    std.debug.print("current free space: {}\n", .{current_free_space});

    var min_sz = sizes[0];
    for (sizes) |s| {
        if (current_free_space + s >= 30_000_000 and s < min_sz) min_sz = s;
    }

    std.debug.print("minimum size that can be freed: {}\n", .{min_sz});
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
