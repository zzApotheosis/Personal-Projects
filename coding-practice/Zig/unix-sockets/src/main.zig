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
            error.FileNotFound => {},
            else => {
                std.debug.print("error: {}\n", .{e});
            },
        }
    };

    const server: ?std.net.Server = std.net.Address.listen(address.?, .{ .reuse_address = true }) catch |e| a: {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        break :a null;
    };
    if (server == null) {
        return;
    }
}
