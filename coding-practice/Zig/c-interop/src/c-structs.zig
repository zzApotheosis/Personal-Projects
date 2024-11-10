const std = @import("std");
const c_foo = @cImport({
    @cInclude("foo.h");
});

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
    var points: [2]Point = undefined;
    for (0..points.len) |i| {
        points[i] = Point_new();
        if (points[i] == null) {
            unreachable;
        }
    }

    const x: c_int = @as(c_int, 1337);
    const y: c_int = @as(c_int, 420);
    const z: c_int = @as(c_int, 69);

    Point_set_x(points[0], x);
    Point_set_y(points[0], y);
    Point_set_z(points[0], z);
    Point_set_x(points[1], x + 1);
    Point_set_y(points[1], y + 1);
    Point_set_z(points[1], z + 1);

    std.debug.assert(Point_get_x(points[0]) == x);
    std.debug.assert(Point_get_y(points[0]) == y);
    std.debug.assert(Point_get_z(points[0]) == z);
    std.debug.assert(Point_get_x(points[1]) == x + 1);
    std.debug.assert(Point_get_y(points[1]) == y + 1);
    std.debug.assert(Point_get_z(points[1]) == z + 1);

    std.debug.print("Point 1\n", .{});
    std.debug.print("x = {}\n", .{Point_get_x(points[0])});
    std.debug.print("y = {}\n", .{Point_get_y(points[0])});
    std.debug.print("z = {}\n", .{Point_get_z(points[0])});

    std.debug.print("\nPoint 2\n", .{});
    std.debug.print("x = {}\n", .{Point_get_x(points[1])});
    std.debug.print("y = {}\n", .{Point_get_y(points[1])});
    std.debug.print("z = {}\n", .{Point_get_z(points[1])});

    for (0..points.len) |i| {
        Point_destroy(points[i]);
    }
}
