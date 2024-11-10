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

test "C struct in Zig" {
    const Point = ?*c_foo.Point;
    const Point_new = c_foo.Point_new;
    const Point_destroy = c_foo.Point_destroy;
    const Point_set_x = c_foo.Point_set_x;
    const Point_set_y = c_foo.Point_set_y;
    const Point_set_z = c_foo.Point_set_z;
    const Point_get_x = c_foo.Point_get_x;
    const Point_get_y = c_foo.Point_get_y;
    const Point_get_z = c_foo.Point_get_z;

    const point: Point = Point_new();
    if (point == null) {
        return;
    }

    const x: c_int = @as(c_int, 1);
    const y: c_int = @as(c_int, 2);
    const z: c_int = @as(c_int, 3);
    _ = Point_set_x(point, x);
    _ = Point_set_y(point, y);
    _ = Point_set_z(point, z);
    std.debug.assert(Point_get_x(point) == x);
    std.debug.assert(Point_get_y(point) == y);
    std.debug.assert(Point_get_z(point) == z);
    std.debug.assert(Point_get_x(point) == 5);
    Point_destroy(point);
}
