const std = @import("std");
const c_foo = @cImport({
    @cInclude("foo.h");
});

pub fn main() void {
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
    defer Point_destroy(point); // This prevents memory leak (classic C mistake)

    const x: c_int = @as(c_int, 1337);
    const y: c_int = @as(c_int, 420);
    const z: c_int = @as(c_int, 69);
    _ = Point_set_x(point, x);
    _ = Point_set_y(point, y);
    _ = Point_set_z(point, z);
    std.debug.assert(Point_get_x(point) == x);
    std.debug.assert(Point_get_y(point) == y);
    std.debug.assert(Point_get_z(point) == z);

    std.debug.print("x = {}\n", .{Point_get_x(point)});
    std.debug.print("y = {}\n", .{Point_get_y(point)});
    std.debug.print("z = {}\n", .{Point_get_z(point)});
}
