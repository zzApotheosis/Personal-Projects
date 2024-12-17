const std = @import("std");
const c_foo = @cImport({
    @cInclude("foo.h");
});

const Point = c_foo.Point;
const Point_new = c_foo.Point_new;
const Point_destroy = c_foo.Point_destroy;
const Point_set_x = c_foo.Point_set_x;
const Point_set_y = c_foo.Point_set_y;
const Point_set_z = c_foo.Point_set_z;
const Point_get_x = c_foo.Point_get_x;
const Point_get_y = c_foo.Point_get_y;
const Point_get_z = c_foo.Point_get_z;

const ZigPoint = struct {
    const Self = @This();

    x: i32 = 0,
    y: i32 = 0,
    z: i32 = 0,
};

pub fn main() void {
    call_c_foo();
}

const POINT_COUNT: usize = 32_768;
const POINT_MAX_RANGE: i32 = 5000;
const POINT_MIN_RANGE: i32 = -POINT_MAX_RANGE;

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

    std.io.getStdErr().writer().print("Generating dataset: {} randomized Points\n", .{POINT_COUNT}) catch {};

    // Let's make some objects with C functions
    var points: [POINT_COUNT]?*Point = undefined;
    for (0..points.len) |i| {
        points[i] = Point_new();
    }
    var origin: ?*Point = Point_new(); // Point_new() should default to (0, 0, 0)
    defer Point_destroy(&origin);

    // Initialize points
    var n: i32 = undefined;
    for (points) |point| {
        n = rand.intRangeAtMost(i32, POINT_MIN_RANGE, POINT_MAX_RANGE);
        Point_set_x(point, n);
        n = rand.intRangeAtMost(i32, POINT_MIN_RANGE, POINT_MAX_RANGE);
        Point_set_y(point, n);
        n = rand.intRangeAtMost(i32, POINT_MIN_RANGE, POINT_MAX_RANGE);
        Point_set_z(point, n);
    }

    // Calculate the point that is closest to the origin
    var closest_point: ?*Point = undefined;
    var closest_distance: f64 = std.math.floatMax(f64);
    for (points) |point| {
        const distance = Point_distance(point, origin);
        if (distance < closest_distance) {
            closest_distance = distance;
            closest_point = point;
        }
    }

    // Calculate the point that is furthest from the origin
    var furthest_point: ?*Point = undefined;
    var furthest_distance: f64 = std.math.floatMin(f64);
    for (points) |point| {
        const distance = Point_distance(point, origin);
        if (distance > furthest_distance) {
            furthest_distance = distance;
            furthest_point = point;
        }
    }

    // TODO: maybe gather statistical data

    // Announce points
    std.io.getStdOut().writer().print("Closest point to the origin ({d:.3}):\n", .{closest_distance}) catch {};
    Point_dump(closest_point);
    std.io.getStdOut().writer().print("Furthest point from the origin ({d:.3}):\n", .{furthest_distance}) catch {};
    Point_dump(furthest_point);

    // Free the memory allocated to the Point objects
    for (0..points.len) |i| {
        Point_destroy(&points[i]);
    }

    const max_distance: f64 = 20.0;
    std.io.getStdOut().writer().print("Searching for a randomized point that is less than {d:.3} distance from the origin...\n", .{max_distance}) catch {};
    l: while (true) {
        var point = ZigPoint{};
        point.x = rand.intRangeAtMost(i32, POINT_MIN_RANGE, POINT_MAX_RANGE);
        point.y = rand.intRangeAtMost(i32, POINT_MIN_RANGE, POINT_MAX_RANGE);
        point.z = rand.intRangeAtMost(i32, POINT_MIN_RANGE, POINT_MAX_RANGE);

        const distance = Point_distance(@as(?*Point, @ptrCast(&point)), origin);
        if (distance <= max_distance) {
            std.debug.print("Found a point that is very close to the origin ({d:.3}):\n", .{distance});
            Point_dump(@as(?*Point, @ptrCast(&point)));
            break :l;
        }
    }
}

fn Point_dump(p: ?*Point) void {
    if (p == null)
        return;
    std.io.getStdErr().writer().print("Point({}, {}, {})\n", .{ Point_get_x(p), Point_get_y(p), Point_get_z(p) }) catch {};
}

fn Point_distance(p1: ?*Point, p2: ?*Point) f64 {
    if (p1 == null or p2 == null)
        return 0.0;

    // Define functions for convenience
    const sqrt = std.math.sqrt;
    const pow = std.math.pow;

    // Get coordinate values from given points
    const x1: i32 = Point_get_x(p1);
    const y1: i32 = Point_get_y(p1);
    const z1: i32 = Point_get_z(p1);
    const x2: i32 = Point_get_x(p2);
    const y2: i32 = Point_get_y(p2);
    const z2: i32 = Point_get_z(p2);

    // Calculate distance with Pythagoras' Theorem
    const distance: f64 = sqrt(pow(f64, @floatFromInt(x2 - x1), 2.0) +
        pow(f64, @floatFromInt(y2 - y1), 2.0) +
        pow(f64, @floatFromInt(z2 - z1), 2.0));

    return distance;
}
