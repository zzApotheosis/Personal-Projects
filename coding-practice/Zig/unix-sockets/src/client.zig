const std = @import("std");
const socket_path = "socket";
pub fn main() void {
    var connection = std.net.connectUnixSocket(socket_path) catch |e| {
        std.debug.print("error: {}\n", .{e});
        return;
    };
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const msg = connection.reader().readAllAlloc(allocator, 256) catch |e| {
        std.debug.print("error: {}\n", .{e});
        return;
    };

    std.debug.print("Received: {s}\n", .{msg});
    connection.close();
}
