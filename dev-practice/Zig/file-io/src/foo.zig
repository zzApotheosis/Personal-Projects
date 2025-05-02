const std = @import("std");

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
        const byte: u8 = file_reader.readByte() catch |e| switch (e) {
            error.EndOfStream, error.BrokenPipe => {
                break :l;
            },
            else => {
                return e;
            },
        };
        _ = buffered_stdout.writer().writeByte(byte) catch |e| switch (e) {
            error.BrokenPipe => {
                // Whatever, bro
                break :l;
            },
            else => return e,
        };
    }
    buffered_stdout.flush() catch |e| switch (e) {
        error.BrokenPipe => {
            // OKAY BRO
        },
        else => return e,
    };
}
