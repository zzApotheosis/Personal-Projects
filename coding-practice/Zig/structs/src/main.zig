const std = @import("std");
const lib = @import("root.zig");

pub fn main() !void {
    use_lib() catch |e| {
        std.log.err("{}", .{e});
    };
}

fn use_lib() !void {
    const stdout = std.io.getStdOut();

    var my_struct = lib.MyStruct.new();
    defer my_struct.deinit();
    my_struct.append(69);
    try stdout.writer().print("Data = {?}\n", .{my_struct.pop()});
    try stdout.writer().print("Data = {?}\n", .{my_struct.pop()});
}

test "too many pops" {
    var a = lib.MyStruct.new();
    a.append(69);
    a.append(420);

    for (0..5) |_| {
        _ = a.pop();
    }
}
