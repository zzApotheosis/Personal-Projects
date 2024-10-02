const std = @import("std");

const socket_path = "socket";

pub fn main() void {
    const address: ?std.net.Address = std.net.Address.initUnix(socket_path) catch |e| a: {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        break :a null;
    };
    if (address == null) {
        return;
    }

    std.fs.cwd().deleteFile(socket_path) catch |e| {
        switch (e) {
            error.FileNotFound => {}, // If there was no file to begin with, it's fine
            else => {
                std.debug.print("error: {}\n", .{e});
            },
        }
    };

    var server: ?std.net.Server = std.net.Address.listen(address.?, .{ .reuse_address = true }) catch |e| a: {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        break :a null;
    };
    if (server == null) {
        return;
    }

    while (true) {
        const client: ?std.net.Server.Connection = server.?.accept() catch |e| a: {
            std.debug.print("error: {}\n", .{e});
            break :a null;
        };
        defer client.?.stream.close();
    }
}
