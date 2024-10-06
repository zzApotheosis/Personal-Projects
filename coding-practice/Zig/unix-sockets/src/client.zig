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
        var message = stdin_reader.readUntilDelimiter(&buffer, '\n') catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        buffered_connection_writer.writer().print("{s}\n", .{message}) catch |e| {
            std.log.err("{}", .{e});
            continue;
        };
        buffered_connection_writer.flush() catch |e| {
            std.log.err("{}", .{e});
            continue;
        };

        if (std.mem.eql(u8, message, "exit")) {
            break;
        }

        @memset(&buffer, 0);
        message = connection_reader.readUntilDelimiter(&buffer, '\n') catch |e| {
            std.log.err("{}", .{e});
            continue;
        };

        std.io.getStdOut().writer().print("{s}\n", .{message}) catch {};
    }

    connection.close();
}
