const std = @import("std");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    _ = try std.posix.sigaction(std.posix.SIG.INT, &std.posix.Sigaction{
        .handler = .{
            .handler = signal_handler,
        },
        .mask = undefined,
        .flags = 0,
    }, null);

    try stdout.print("use ctrl+c to exit\n", .{});
    try bw.flush();

    while (true) {
        std.time.sleep(10_000_000_000);
    }
}

fn signal_handler(s: i32) callconv(.C) void {
    std.debug.print("\nCaught signal: {}\n", .{s});
    std.process.exit(0);
}
