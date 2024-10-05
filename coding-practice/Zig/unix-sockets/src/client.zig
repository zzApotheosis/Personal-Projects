const std = @import("std");
const socket_path = "socket";
pub fn main() void {
    var connection = std.net.connectUnixSocket(socket_path) catch |e| {
        std.debug.print("error: {}\n", .{e});
        return;
    };
    const connection_reader = connection.reader();
    const connection_writer = connection.writer();
    var buffered_connection_writer = std.io.bufferedWriter(connection_writer);

    std.io.getStdOut().writer().print("Connected to Unix socket! Type some messages to the server.\n", .{}) catch {};
    const stdin_reader = std.io.getStdIn().reader();
    var buffer: [512]u8 = undefined;
    while (true) {
        @memset(&buffer, 0);
        std.io.getStdOut().writer().print("> ", .{}) catch {};
        _ = stdin_reader.readUntilDelimiter(&buffer, '\n') catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        std.debug.print("Sending message: {s}\n", .{buffer});
        buffered_connection_writer.writer().writeAll(&buffer) catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        buffered_connection_writer.flush() catch |e| {
            std.log.err("{}", .{e});
            continue;
        };

        if (std.mem.eql(u8, &buffer, "exit")) {
            break;
        }

        std.debug.print("Receiving message...\n", .{});
        @memset(&buffer, 0);
        _ = connection_reader.readUntilDelimiter(&buffer, '\n') catch |e| {
            std.log.err("{}", .{e});
            continue;
        };

        std.io.getStdOut().writer().print("{s}", .{buffer}) catch {};
    }

    connection.close();
}
