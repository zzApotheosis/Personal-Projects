const std = @import("std");
const Cell = @import("cell.zig").Cell;

pub const Board = struct {
    const Self = @This();
    const X_SIZE: usize = 9;
    const Y_SIZE: usize = 9;

    cells: [X_SIZE][Y_SIZE]Cell = undefined,

    pub fn init(self: *Self) void {
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                self.cells[x][y] = Cell{};
            }
        }
    }

    pub fn print(self: *const Self, f: std.fs.File) !void {
        const fw = f.writer();
        var cell: *const Cell = undefined;

        try fw.print("+", .{});
        for (0..Y_SIZE) |y| {
            try fw.print("-", .{});
            if ((y + 1) % 3 == 0 and y != Y_SIZE - 1)
                try fw.print("+", .{});
        }
        try fw.print("+", .{});
        try fw.print("\n", .{});

        for (0..Y_SIZE) |y| {
            try fw.print("|", .{});
            for (0..X_SIZE) |x| {
                cell = &self.cells[x][y];
                try fw.print("{}", .{cell.value});
                if ((x + 1) % 3 == 0 and x != X_SIZE - 1)
                    try fw.print("|", .{});
            }
            try fw.print("|", .{});
            try fw.print("\n", .{});

            if ((y + 1) % 3 == 0 and y != Y_SIZE - 1) {
                try fw.print("+", .{});
                for (0..X_SIZE) |x| {
                    try fw.print("-", .{});
                    if ((x + 1) % 3 == 0 and x != X_SIZE - 1)
                        try fw.print("+", .{});
                }
                try fw.print("+", .{});
                try fw.print("\n", .{});
            }
        }

        try fw.print("+", .{});
        for (0..Y_SIZE) |y| {
            try fw.print("-", .{});
            if ((y + 1) % 3 == 0 and y != Y_SIZE - 1)
                try fw.print("+", .{});
        }
        try fw.print("+", .{});
        try fw.print("\n", .{});
    }
};
