const std = @import("std");
const foo = @import("foo.zig");

var argc: u64 = 0;
var argv: std.ArrayList([:0]const u8) = undefined;

pub fn main() !void {
    try parseArgs();
    defer argv.deinit();

    foo.readFiles(argc, argv.items) catch |err| switch (err) {
        error.IllegalArguments => {
            try usage();
            return err;
        },
        else => return err,
    };

    // const f = try std.fs.cwd().createFile("test.txt", .{});
    // defer f.close();
    // var f_writer = std.io.bufferedWriter(f.writer());
    // try f_writer.writer().print("ligma balls\n", .{});
    // try f_writer.flush();
}

fn parseArgs() !void {
    argc = 0;
    argv = std.ArrayList([:0]const u8).init(std.heap.page_allocator);

    var arg_iter = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer arg_iter.deinit();

    // Save arguments into heap-allocated data structure
    while (arg_iter.next()) |arg| {
        argc += 1;
        try argv.append(arg);
    }
}

fn usage() !void {
    try std.io.getStdErr().writer().print("Usage: {s} <file> <file> ...\n", .{argv.items[0]});
}
