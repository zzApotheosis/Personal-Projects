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

    var client_counter: u32 = 0;
    while (true) {
        const client = server.accept() catch |e| {
            std.debug.print("error: {}\n", .{e});
            continue;
        };
        const t = std.Thread.spawn(.{}, client_handler, .{
            client,
            client_counter,
        }) catch |e| {
            std.debug.print("error: {}\n", .{e});
            continue;
        };
        client_counter += 1;
        t.detach();
    }
}

const msg_limit: usize = 512;

fn client_handler(client: std.net.Server.Connection, client_id: u32) void {
    var client_reader = client.stream.reader();
    var client_writer = std.io.bufferedWriter(client.stream.writer());
    var buffer: [msg_limit]u8 = undefined;
    std.log.info("New client connection ({})", .{client_id});
    while (true) {
        @memset(&buffer, 0);
        // Read the client's message
        const result = client_reader.readUntilDelimiterOrEof(&buffer, '\n') catch |e| {
            std.log.err("{}: {}", .{ client_id, e });
            break;
        };
        if (result == null) {
            std.log.err("result == null", .{});
            break;
        }
        std.debug.print("{}: Received message: {s}\n", .{ client_id, result.? });

        if (std.mem.eql(u8, result.?, "exit")) {
            break;
        }

        // Echo the message
        _ = client_writer.writer().print("{s}\n", .{result.?}) catch |e| {
            std.log.err("{}: {}", .{ client_id, e });
            continue;
        };
        client_writer.flush() catch |e| {
            std.log.err("{}: {}", .{ client_id, e });
            continue;
        };
    }
    std.log.info("{}: Closing connection", .{client_id});
    client.stream.close();
}
