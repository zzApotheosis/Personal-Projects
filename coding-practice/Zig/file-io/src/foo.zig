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
    var file_reader = file.reader();
    var buffered_stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
    l: while (true) {
        const byte: u8 = file_reader.readByte() catch |e| {
            switch (e) {
                error.EndOfStream => {
                    break :l;
                },
                else => {
                    return e;
                },
            }
        };
        _ = try buffered_stdout.writer().writeByte(byte);
        // const bytes_read = file_reader.read(&stack_buffer) catch |e| {
        //     std.debug.print("{}\n", .{e});
        //     break;
        // };
        // _ = try buffered_stdout.writer().write(stack_buffer[0..bytes_read]);
        // if (try file_reader.context.getPos() == try file_reader.context.getEndPos())
        //     break;
    }
    try buffered_stdout.flush();
}
