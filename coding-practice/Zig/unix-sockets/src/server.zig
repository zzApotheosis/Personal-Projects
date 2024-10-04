const std = @import("std");

const socket_path = "socket";

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

pub fn main() void {
    const address: std.net.Address = std.net.Address.initUnix(socket_path) catch |e| {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        return;
    };

    std.fs.cwd().deleteFile(socket_path) catch |e| {
        switch (e) {
            error.FileNotFound => {}, // If there was no file to begin with, it's fine
            else => {
                std.debug.print("error: {}\n", .{e});
                return;
            },
        }
    };

    var server: std.net.Server = std.net.Address.listen(address, .{}) catch |e| {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        return;
    };
    defer server.deinit();

    while (true) {
        const client = server.accept() catch |e| {
            std.debug.print("error: {}\n", .{e});
            continue;
        };
        const t = std.Thread.spawn(.{}, client_handler, .{client}) catch |e| {
            std.debug.print("error: {}\n", .{e});
            continue;
        };
        t.detach();
    }
}

fn client_handler(client: std.net.Server.Connection) void {
    client.stream.writer().print("ligma balls\n", .{}) catch |e| {
        std.debug.print("error: {}\n", .{e});
    };
    client.stream.close();
}
