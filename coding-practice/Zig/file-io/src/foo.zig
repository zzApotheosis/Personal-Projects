const std = @import("std");

pub const ligma = struct {
    const Self = @This();

    x: i32,

    pub fn bofa(_: *const @This(), msg: []const u8) void {
        std.debug.print("{s}", .{
            msg,
        });
    }
};

pub fn readFiles(argc: u64, argv: []const [:0]const u8) !void {
    if (argc < 2) {
        return error.IllegalArguments;
    }

    for (1..argc) |i| {
        const f: std.fs.File = try std.fs.cwd().openFile(argv[i], .{});
        defer f.close();
        try readFile(f);
    }
}

fn readFile(file: std.fs.File) !void {
    var stack_buffer: [4096]u8 = undefined;
    @memset(&stack_buffer, 0);
    var file_reader = std.io.bufferedReader(file.reader());
    var buffered_stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
    var reader = file_reader.reader();
    while (true) {
        const bytes_read = reader.read(&stack_buffer) catch |e| {
            std.debug.print("{}\n", .{e});
            return e;
        };
        _ = try buffered_stdout.writer().write(stack_buffer[0..bytes_read]);
    }
}
