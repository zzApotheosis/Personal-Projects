//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");

//pub fn main() !void {
//    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
//    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
//
//    // stdout is for the actual output of your application, for example if you
//    // are implementing gzip, then only the compressed bytes should be sent to
//    // stdout, not any debugging messages.
//    const stdout_file = std.io.getStdOut().writer();
//    var bw = std.io.bufferedWriter(stdout_file);
//    const stdout = bw.writer();
//
//    try stdout.print("Run `zig build test` to run the tests.\n", .{});
//
//    try bw.flush(); // Don't forget to flush!
//
//    const s = "world";
//    try stdout.print("Hello {s}\n", .{s});
//    try bw.flush();
//
//    foo();
//}

//pub fn main() void {
//    const stdout_writer = std.io.getStdOut().writer();
//    try stdout_writer.write("ligma\n");
//}

//pub fn main() !void {
//    const items = [_]i32{ 4, 5, 3, 4, 0 };
//    var sum: i32 = 0;
//
//    for (items) |item| {
//        if (item == 0) {
//            continue;
//        }
//        sum += item;
//    }
//
//    try std.testing.expect(sum == 16);
//
//    for (items[0..1]) |item| {
//        sum += item;
//    }
//    try std.testing.expect(sum == 20);
//
//    sum = 0;
//    for (items, 0..) |_, i| {
//        try std.testing.expect(@TypeOf(i) == usize);
//        sum += @as(i32, @intCast(i));
//    }
//    try std.testing.expect(sum == 10);
//
//    var sum2: usize = 0;
//    for (0..5) |i| {
//        sum2 += i;
//    }
//    try std.testing.expect(sum2 == 10);
//}

pub fn main() !void {
    //const allocator = std.heap.page_allocator;
    //const allocator = std.heap.c_allocator;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    for (0..100000) |i| {
        const n: u8 = @truncate(i % 256);
        try list.append(n);
    }

    for (0..list.items.len, list.items) |i, item| {
        if (i % 257 == 0) {
            std.debug.print("Cool: item[{}] = {}\n", .{ i, item });
        }
    }
}

fn foo() void {
    std.debug.print("Ligma\n", .{});
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const ligma = struct {
        fn testOne(input: []const u8) anyerror!void {
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(ligma.testOne, .{});
}
