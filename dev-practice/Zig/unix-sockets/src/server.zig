const std = @import("std");

const socket_path = "socket";

var global_shutdown: bool = false;

pub fn main() void {
    const address: std.net.Address = std.net.Address.initUnix(socket_path) catch |e| {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        return;
    };

    delSocket();

    var server: std.net.Server = std.net.Address.listen(address, .{}) catch |e| {
        std.io.getStdErr().writer().print("error: {}\n", .{e}) catch {};
        return;
    };
    defer server.deinit();
    defer delSocket();

    var client_counter: u32 = 0;
    while (!global_shutdown) {
        const client = server.accept() catch |e| {
            std.debug.print("error: {}\n", .{e});
            continue;
        };
        const t = std.Thread.spawn(.{}, clientHandler, .{
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

fn delSocket() void {
    std.fs.cwd().deleteFile(socket_path) catch |e| {
        switch (e) {
            error.FileNotFound => {}, // If there was no file to begin with, it's fine
            else => {
                std.debug.panic("error: {}\n", .{e});
            },
        }
    };
}

const msg_limit: usize = 512;

fn clientHandler(client: std.net.Server.Connection, client_id: u32) void {
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
        const unwrapped_result = result.?;
        std.debug.print("{}: Received message: {s}\n", .{ client_id, unwrapped_result });

        if (std.mem.eql(u8, unwrapped_result, "shutdown")) {
            global_shutdown = true;

            // Make a dummy client connection to break the server loop
            var lastclient = std.net.connectUnixSocket(socket_path) catch |e| {
                std.debug.panic("{}", .{e});
                //unreachable;
            };
            defer lastclient.close();
            break;
        }

        if (std.mem.eql(u8, unwrapped_result, "exit")) {
            break;
        }

        // Echo the message
        _ = client_writer.writer().print("{s}\n", .{unwrapped_result}) catch |e| {
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
