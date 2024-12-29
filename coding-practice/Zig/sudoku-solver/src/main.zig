const std = @import("std");
const Board = @import("sudoku/board.zig").Board;

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();

    var board: Board = .{};
    board.init();
    try board.print(std.io.getStdErr());
}
