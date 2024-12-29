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
    var buffer: [9]u8 = undefined;

    try board.getRow(1, &cells);
    for (0..9) |i| {
        buffer[i] = cells[i].value;
    }
    std.debug.print("Row 1: {any}\n", .{buffer});

    try board.getColumn(4, &cells);
    for (0..9) |i| {
        buffer[i] = cells[i].value;
    }
    std.debug.print("Column 4: {any}\n", .{buffer});

    try board.getCellGroup(4, &cells);
    for (0..9) |i| {
        buffer[i] = cells[i].value;
    }
    std.debug.print("CellGroup 4: {any}\n", .{buffer});

    try board.print(std.io.getStdErr());
}
