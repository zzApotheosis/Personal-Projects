const std = @import("std");
const Board = @import("sudoku/board.zig").Board;

const Cell = @import("sudoku/cell.zig").Cell;

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();

    var board: Board = .{};
    board.init();
    try board.setBoard([9][9]u8{
        .{ 1, 1, 1, 2, 2, 2, 3, 3, 3 },
        .{ 1, 1, 1, 2, 2, 2, 3, 3, 3 },
        .{ 1, 1, 1, 2, 2, 2, 3, 3, 3 },
        .{ 4, 4, 4, 5, 5, 5, 6, 6, 6 },
        .{ 4, 4, 4, 5, 5, 5, 6, 6, 6 },
        .{ 4, 4, 4, 5, 5, 5, 6, 6, 6 },
        .{ 7, 7, 7, 8, 8, 8, 9, 9, 9 },
        .{ 7, 7, 7, 8, 8, 8, 9, 9, 9 },
        .{ 7, 7, 7, 8, 8, 8, 9, 9, 9 },
    });

    var cells: [9]*const Cell = undefined;
    var values: [9]u8 = undefined;

    try board.getRow(0, &cells);
    for (0..9) |i| {
        values[i] = cells[i].value;
    }
    std.debug.print("Row 0: {any}\n", .{values});

    try board.getColumn(4, &cells);
    for (0..9) |i| {
        values[i] = cells[i].value;
    }
    std.debug.print("Column 4: {any}\n", .{values});

    try board.getCellGroup(8, &cells);
    for (0..9) |i| {
        values[i] = cells[i].value;
    }
    std.debug.print("CellGroup 8: {any}\n", .{values});

    try board.print(std.io.getStdErr());
}
