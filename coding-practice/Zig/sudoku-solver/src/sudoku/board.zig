const std = @import("std");
const Cell = @import("cell.zig").Cell;

pub const Board = struct {
    const Self = @This();
    const X_SIZE: usize = 9;
    const Y_SIZE: usize = 9;
    const CELLGROUP_DIVIDER: usize = 3;

    cells: [Y_SIZE][X_SIZE]Cell = undefined,

    pub fn init(self: *Self) void {
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                self.cells[y][x] = Cell{};
            }
        }
    }

    pub fn setBoard(self: *Self, data: [Y_SIZE][X_SIZE]u8) !void {
        // Check input data. Each value MUST be within bounds
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                if (data[y][x] < 1 or data[y][x] > 9) return error.IllegalArguments;
            }
        }

        // Set data
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                self.cells[y][x].given = data[y][x];
                self.cells[y][x].value = data[y][x];
            }
        }
    }

    pub fn getRow(self: *const Self, row: usize, vector: *[X_SIZE]u8) !void {
        if (row < 0 or row >= X_SIZE)
            return error.IllegalArguments;
        for (0..X_SIZE) |i|
            vector[i] = self.cells[row][i].value;
    }

    pub fn getColumn(self: *const Self, column: usize, vector: *[Y_SIZE]u8) !void {
        if (column < 0 or column >= Y_SIZE)
            return error.IllegalArguments;
        for (0..Y_SIZE) |i|
            vector[i] = self.cells[i][column].value;
    }

    pub fn getCellGroup(self: *const Self, group: usize, vector: *[X_SIZE]u8) !void {
        if (group < 0 or group >= X_SIZE)
            return error.IllegalArguments;
        var buffer: [X_SIZE]u8 = undefined;
        for (0..3) |i| {
            try self.getRow((group / 3) * 3 + i, &buffer);
            for (0..3) |j| {
                vector[3 * i + j] = buffer[3 * (group % 3) + j];
            }
        }
    }

    pub fn print(self: *const Self, f: std.fs.File) !void {
        const fw = f.writer();
        var cell: *const Cell = undefined;
        const corner_char: u8 = '+';
        const horizontal_edge_char: u8 = '-';
        const vertical_edge_char: u8 = '|';

        try fw.print("{c}", .{corner_char});
        for (0..Y_SIZE) |y| {
            try fw.print("{c}", .{horizontal_edge_char});
            if ((y + 1) % CELLGROUP_DIVIDER == 0 and y != Y_SIZE - 1)
                try fw.print("{c}", .{corner_char});
        }
        try fw.print("{c}\n", .{corner_char});

        for (0..Y_SIZE) |y| {
            try fw.print("{c}", .{vertical_edge_char});
            for (0..X_SIZE) |x| {
                cell = &self.cells[y][x];
                try fw.print("{}", .{cell.value});
                if ((x + 1) % CELLGROUP_DIVIDER == 0 and x != X_SIZE - 1)
                    try fw.print("{c}", .{vertical_edge_char});
            }
            try fw.print("{c}\n", .{vertical_edge_char});

            if ((y + 1) % CELLGROUP_DIVIDER == 0 and y != Y_SIZE - 1) {
                try fw.print("{c}", .{corner_char});
                for (0..X_SIZE) |x| {
                    try fw.print("{c}", .{horizontal_edge_char});
                    if ((x + 1) % CELLGROUP_DIVIDER == 0 and x != X_SIZE - 1)
                        try fw.print("{c}", .{corner_char});
                }
                try fw.print("{c}\n", .{corner_char});
            }
        }

        try fw.print("{c}", .{corner_char});
        for (0..Y_SIZE) |y| {
            try fw.print("{c}", .{horizontal_edge_char});
            if ((y + 1) % CELLGROUP_DIVIDER == 0 and y != Y_SIZE - 1)
                try fw.print("{c}", .{corner_char});
        }
        try fw.print("{c}\n", .{corner_char});
    }
};
