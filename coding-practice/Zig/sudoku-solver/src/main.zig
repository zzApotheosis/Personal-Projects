const std = @import("std");
const Board = @import("sudoku/board.zig").Board;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var board: Board = Board{};
    // try board.init([9][9]u8{
    //     .{ 1, 1, 1, 2, 2, 2, 3, 3, 3 },
    //     .{ 1, 1, 1, 2, 2, 2, 3, 3, 3 },
    //     .{ 1, 1, 1, 2, 2, 2, 3, 3, 3 },
    //     .{ 4, 4, 4, 5, 5, 5, 6, 6, 6 },
    //     .{ 4, 4, 4, 5, 5, 5, 6, 6, 6 },
    //     .{ 4, 4, 4, 5, 5, 5, 6, 6, 6 },
    //     .{ 7, 7, 7, 8, 8, 8, 9, 9, 9 },
    //     .{ 7, 7, 7, 8, 8, 8, 9, 9, 9 },
    //     .{ 7, 7, 7, 8, 8, 8, 9, 9, 9 },
    // });
    try board.init(allocator, [9][9]u8{
        .{ 0, 0, 8, 0, 5, 3, 0, 0, 2 },
        .{ 0, 0, 3, 0, 0, 0, 5, 6, 0 },
        .{ 2, 0, 5, 0, 9, 7, 0, 0, 3 },
        .{ 0, 6, 0, 0, 0, 1, 0, 0, 0 },
        .{ 0, 5, 0, 8, 0, 4, 0, 7, 0 },
        .{ 0, 0, 0, 3, 0, 0, 0, 1, 0 },
        .{ 9, 0, 0, 2, 1, 0, 7, 0, 8 },
        .{ 0, 8, 6, 0, 0, 0, 3, 0, 0 },
        .{ 7, 0, 0, 5, 3, 0, 4, 0, 0 },
    });
    defer board.deinit();

    std.debug.print("Initial board:\n", .{});
    try board.print(std.io.getStdErr());

    // Solve the puzzle
    try board.solve();
    std.debug.print("\nFinal board:\n", .{});
    try board.print(std.io.getStdErr());
}
