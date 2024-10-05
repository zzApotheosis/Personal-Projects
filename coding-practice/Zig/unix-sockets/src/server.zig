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

const msg_limit: usize = 512;

fn client_handler(client: std.net.Server.Connection) void {
    var client_reader = client.stream.reader();
    var client_writer = std.io.bufferedWriter(client.stream.writer());
    var buffer: [msg_limit]u8 = undefined;
    while (true) {
        @memset(&buffer, 0);
        // Read the client's message
        _ = client_reader.readUntilDelimiter(&buffer, '\n') catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        std.debug.print("Received message: {s}\n", .{buffer});

        if (std.mem.eql(u8, &buffer, "exit\n")) {
            break;
        }

        // Echo the message
        _ = client_writer.writer().write(&buffer) catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        _ = client_writer.writer().writeByte('\n') catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        client_writer.flush() catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
    }
    client.stream.close();
}
