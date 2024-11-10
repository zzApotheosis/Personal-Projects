const std = @import("std");
const c_foo = @cImport({
    @cInclude("foo.h");
});

const ZigPoint = struct {
    x: c_int = 0,
    y: c_int = 0,
    z: c_int = 0,
};

// Assign types and functions to more convenient names
const Point = ?*c_foo.Point;
const Point_new = c_foo.Point_new;
const Point_destroy = c_foo.Point_destroy;
const Point_set_x = c_foo.Point_set_x;
const Point_set_y = c_foo.Point_set_y;
const Point_set_z = c_foo.Point_set_z;
const Point_get_x = c_foo.Point_get_x;
const Point_get_y = c_foo.Point_get_y;
const Point_get_z = c_foo.Point_get_z;

pub fn main() void {
    const num_points: usize = 4;
    var points: [num_points]Point = undefined;
    for (0..num_points) |i| {
        points[i] = Point_new();
        if (points[i] == null) {
            unreachable;
        }
    }

    var zigpoint = ZigPoint{};

    const x: c_int = @as(c_int, 1337);
    const y: c_int = @as(c_int, 420);
    const z: c_int = @as(c_int, 69);

    for (0..num_points) |i| {
        Point_set_x(points[i], x + @as(c_int, @intCast(i)));
        Point_set_y(points[i], y + @as(c_int, @intCast(i)));
        Point_set_z(points[i], z + @as(c_int, @intCast(i)));
    }
    Point_set_x(@ptrCast(&zigpoint), x);
    Point_set_y(@ptrCast(&zigpoint), y);
    Point_set_z(@ptrCast(&zigpoint), z);

    std.debug.print("ZigPoint\n", .{});
    std.debug.print("x = {}\n", .{Point_get_x(@ptrCast(&zigpoint))});
    std.debug.print("y = {}\n", .{Point_get_y(@ptrCast(&zigpoint))});
    std.debug.print("z = {}\n\n", .{Point_get_z(@ptrCast(&zigpoint))});

    for (0..num_points) |i| {
        std.debug.assert(Point_get_x(points[i]) == x + @as(c_int, @intCast(i)));
        std.debug.assert(Point_get_y(points[i]) == y + @as(c_int, @intCast(i)));
        std.debug.assert(Point_get_z(points[i]) == z + @as(c_int, @intCast(i)));

        std.debug.print("Point {}\n", .{i});
        std.debug.print("x = {}\n", .{Point_get_x(points[i])});
        std.debug.print("y = {}\n", .{Point_get_y(points[i])});
        std.debug.print("z = {}\n", .{Point_get_z(points[i])});
        std.debug.print("\n", .{});
    }

    std.debug.print("Before free()\n", .{});
    for (0..num_points) |i| {
        std.debug.print("point {} = {?}\n", .{ i, points[i] });
    }
    std.debug.print("\n", .{});

    for (0..num_points) |i| {
        Point_destroy(&points[i]);
    }

    std.debug.print("After free()\n", .{});
    for (0..num_points) |i| {
        std.debug.print("point {} = {?}\n", .{ i, points[i] });
    }
}
