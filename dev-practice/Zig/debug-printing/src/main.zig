const std = @import("std");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("{s}:{} {s}\n", .{ @src().file, @src().line, "Print statement :D" });
}
