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
    defer server.?.deinit();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    while (true) {
        var client = allocator.alloc(std.net.Server.Connection, 1) catch |e| {
            std.debug.print("error: {}\n", .{e});
            return;
        };
        std.debug.print("typeof(client) = {}\n", .{@TypeOf(client)});
        client[0] = server.?.accept() catch |e| {
            std.debug.print("error: {}\n", .{e});
            continue;
        };
        //const connection = server.?.accept() catch undefined;
        //client[0] = connection;
        //const client: ?std.net.Server.Connection = server.?.accept() catch |e| a: {
        //    std.debug.print("error: {}\n", .{e});
        //    break :a null;
        //};
        //defer client.?.stream.close();
        var t = std.Thread.spawn(.{}, client_handler, .{client[0]}) catch |e| {
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
