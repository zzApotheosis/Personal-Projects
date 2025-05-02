const std = @import("std");

pub fn fill(v: *std.ArrayList(u8), l: u32) !void {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();

    var n: u8 = undefined;
    for (0..l) |_| {
        n = rand.int(u8);
        try v.append(n);
    }
}

pub fn dump(v: *const std.ArrayList(u8)) !void {
    const stdout = std.io.getStdOut().writer();
    const dump_width: u32 = 8;
    var counter: u32 = 0;
    for (0..v.items.len) |i| {
        try stdout.print("0x{x:0>2} ", .{v.items[i]});
        counter += 1;
        if (counter >= dump_width) {
            try stdout.print("\n", .{});
            counter = 0;
        }
    }
}
