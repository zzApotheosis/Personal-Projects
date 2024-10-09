const std = @import("std");
const c_foo = @cImport({
    @cInclude("foo.h");
});

pub fn main() !void {
    call_c_foo();
}

fn call_c_foo() void {
    const stdout = std.io.getStdOut();
    stdout.writer().print("This is the main function in Zig!\n", .{}) catch |e| {
        std.log.err("{}", .{e});
        return;
    };
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.posix.getrandom(std.mem.asBytes(&seed)) catch |e| {
            std.log.err("error: {}", .{e});
            return;
        };
        break :blk seed;
    });
    const rand = prng.random();
    const num: u32 = rand.intRangeAtMost(u32, 0, 110);
    c_foo.c_foo(num);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
