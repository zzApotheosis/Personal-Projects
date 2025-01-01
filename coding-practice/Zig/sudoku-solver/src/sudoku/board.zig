const std = @import("std");
const Cell = @import("cell.zig").Cell;

const Decision = struct {
    x: usize = undefined,
    y: usize = undefined,
    val: u8 = undefined,
    snapshot: [Board.Y_SIZE][Board.X_SIZE]Cell = undefined,
};

pub const Board = struct {
    const Self = @This();
    const X_SIZE: usize = 9;
    const Y_SIZE: usize = 9;
    const CELLGROUP_DIVIDER: usize = 3;
    const CELLGROUP_COUNT: usize = 9;
    const CELLS_PER_CELLGROUP: usize = 9;
    pub const SolutionStatus = enum {
        normal,
        nonlinear,
        illegal,
        finished,
    };

    allocator: std.mem.Allocator = undefined,
    cells: [Y_SIZE][X_SIZE]Cell = undefined,
    decision_stack: std.ArrayList(Decision) = undefined,

    pub fn init(self: *Self, allocator: std.mem.Allocator, data: [Y_SIZE][X_SIZE]u8) !void {
        self.allocator = allocator;
        self.decision_stack = std.ArrayList(Decision).init(allocator);

        // Check input data. Each value MUST be within bounds
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                if (data[y][x] < 0 or data[y][x] > 9) return error.IllegalArguments;
            }
        }

        // Set data
        self.initPossibilities();
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                self.cells[y][x].given = data[y][x];
                self.cells[y][x].value = data[y][x];
            }
        }

        // Determine possible values
        try self.determinePossibilities();
    }

    pub fn deinit(self: *Self) void {
        self.decision_stack.deinit();
    }

    pub fn determinePossibilities(self: *Self) !void {
        self.initPossibilities();
        try self.determineRowPossibilities();
        try self.determineColumnPossibilities();
        try self.determineCellGroupPossibilities();
    }

    fn initPossibilities(self: *Self) void {
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                @memset(&self.cells[y][x].possible_values, if (self.cells[y][x].value == 0) true else false);
            }
        }
    }

    fn determineRowPossibilities(self: *Self) !void {
        var cells: [X_SIZE]*Cell = undefined;
        var known_values: [Cell.POSSIBLE_VALUES_MAX]bool = undefined;
        for (0..Y_SIZE) |y| {
            @memset(&known_values, false);
            try self.getRow(y, &cells);
            for (0..X_SIZE) |i| {
                if (cells[i].value == 0) continue;
                known_values[cells[i].value - 1] = true;
            }

            for (0..X_SIZE) |i| {
                // If there already is a value in this cell, then zeroize the possible values vector
                if (cells[i].value != 0) {
                    @memset(&cells[i].possible_values, false);
                    continue;
                }

                for (0..Cell.POSSIBLE_VALUES_MAX) |j| {
                    if (known_values[j]) cells[i].possible_values[j] = false;
                }
            }
        }
    }

    fn determineColumnPossibilities(self: *Self) !void {
        var cells: [X_SIZE]*Cell = undefined;
        var known_values: [Cell.POSSIBLE_VALUES_MAX]bool = undefined;
        for (0..X_SIZE) |x| {
            @memset(&known_values, false);
            try self.getColumn(x, &cells);
            for (0..Y_SIZE) |i| {
                if (cells[i].value == 0) continue;
                known_values[cells[i].value - 1] = true;
            }

            for (0..Y_SIZE) |i| {
                // If there already is a value in this cell, then zeroize the possible values vector
                if (cells[i].value != 0) {
                    @memset(&cells[i].possible_values, false);
                    continue;
                }

                for (0..Cell.POSSIBLE_VALUES_MAX) |j| {
                    if (known_values[j]) cells[i].possible_values[j] = false;
                }
            }
        }
    }

    fn determineCellGroupPossibilities(self: *Self) !void {
        var cells: [X_SIZE]*Cell = undefined;
        var known_values: [Cell.POSSIBLE_VALUES_MAX]bool = undefined;
        for (0..CELLGROUP_COUNT) |c| {
            @memset(&known_values, false);
            try self.getCellGroup(c, &cells);
            for (0..CELLS_PER_CELLGROUP) |i| {
                if (cells[i].value == 0) continue;
                known_values[cells[i].value - 1] = true;
            }

            for (0..CELLS_PER_CELLGROUP) |i| {
                // If there already is a value in this cell, then zeroize the possible values vector
                if (cells[i].value != 0) {
                    @memset(&cells[i].possible_values, false);
                    continue;
                }

                for (0..Cell.POSSIBLE_VALUES_MAX) |j| {
                    if (known_values[j]) cells[i].possible_values[j] = false;
                }
            }
        }
    }

    pub fn getRow(self: *Self, row: usize, vector: *[X_SIZE]*Cell) !void {
        if (row < 0 or row >= X_SIZE)
            return error.IllegalArguments;
        for (0..X_SIZE) |i|
            vector[i] = &self.cells[row][i];
    }

    pub fn getColumn(self: *Self, column: usize, vector: *[Y_SIZE]*Cell) !void {
        if (column < 0 or column >= Y_SIZE)
            return error.IllegalArguments;
        for (0..Y_SIZE) |i|
            vector[i] = &self.cells[i][column];
    }

    pub fn getCellGroup(self: *Self, group: usize, vector: *[X_SIZE]*Cell) !void {
        if (group < 0 or group >= X_SIZE)
            return error.IllegalArguments;
        var buffer: [X_SIZE]*Cell = undefined;
        for (0..3) |i| {
            try self.getRow((group / 3) * 3 + i, &buffer);
            for (0..3) |j| {
                vector[3 * i + j] = buffer[3 * (group % 3) + j];
            }
        }
    }

    pub fn solve(self: *Self) !void {
        var solution_status = SolutionStatus.normal;
        while (solution_status != SolutionStatus.finished) {
            solution_status = try self.iterateSolution();

            // Guaranteed solution
            if (solution_status == SolutionStatus.normal) continue;

            // Taking a small risk
            if (solution_status == SolutionStatus.nonlinear) {
                try self.traverseNonLinear();
                continue;
            }

            // Backtracking on a bad decision
            if (solution_status == SolutionStatus.illegal) {
                try self.backtrack();
                continue;
            }
        }
        // while (solution_status == SolutionStatus.nonlinear) {
        //     solution_status = try self.traverseNonLinear();
        // }
    }

    fn iterateSolution(self: *Self) !SolutionStatus {
        var solution_status: SolutionStatus = SolutionStatus.nonlinear;
        _0: for (0..Y_SIZE) |y| {
            _1: for (0..X_SIZE) |x| {
                if (self.cells[y][x].value != 0) continue :_1; // We don't care about cells that are already populated

                // Only one possible value in this cell, so we set it
                if (self.cells[y][x].numPossibleValues() == 1) {
                    self.cells[y][x].value = try self.cells[y][x].nextPossibleValue();
                    solution_status = SolutionStatus.normal;
                    break :_0;
                }

                if (self.cells[y][x].numPossibleValues() > 1) continue :_1;

                if (self.cells[y][x].numPossibleValues() == 0 and self.cells[y][x].value == 0) {
                    return SolutionStatus.illegal;
                }
            }
        }
        try self.determinePossibilities();
        if (self.emptyCellCount() == 0 and solution_status == SolutionStatus.normal)
            solution_status = SolutionStatus.finished;
        return solution_status;
    }

    fn traverseNonLinear(self: *Self) !void {
        // Check the board for possible linear solutions
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                if (self.cells[y][x].value != 0) continue;
                if (self.cells[y][x].numPossibleValues() == 1) return error.LinearSolutionAvailable;
            }
        }

        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                if (self.cells[y][x].value != 0) continue;
                if (self.cells[y][x].numPossibleValues() == 2) {
                    // Try the first 50/50 chance we find
                    const val = try self.cells[y][x].nextPossibleValue();
                    try self.snapshot(.{
                        .x = x,
                        .y = y,
                        .val = val,
                        .snapshot = self.cells,
                    });
                    self.cells[y][x].value = val;
                    try self.determinePossibilities();
                    return;
                }
            }
        }

        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                if (self.cells[y][x].value != 0) continue;

                const val = try self.cells[y][x].nextPossibleValue();
                try self.snapshot(.{
                    .x = x,
                    .y = y,
                    .val = val,
                    .snapshot = self.cells,
                });
                self.cells[y][x].value = val;
                try self.determinePossibilities();
                return;
            }
        }
    }

    fn backtrack(self: *Self) !void {
        std.debug.print("\nBacktracking\n", .{});
        try self.print(std.io.getStdErr());
        if (self.decision_stack.popOrNull()) |decision| {
            // Restore the state of the board before the risky decision
            self.cells = decision.snapshot;

            // Recall the coordinates where the risky decision was made
            // and invalidate the decision
            try self.cells[decision.y][decision.x].setImpossibleValue(decision.val);
            const decisions: []Decision = self.decision_stack.allocatedSlice();
            for (0..self.decision_stack.items.len) |i|
                // All snapshots need to be aware that this value in this cell is impossible
                try decisions[i].snapshot[decision.y][decision.x].setImpossibleValue(decision.val);

            // Try the next possible value after invalidating the previous decision
            self.cells[decision.y][decision.x].value = try self.cells[decision.y][decision.x].nextPossibleValue();
            try self.determinePossibilities();
        } else return error.BacktrackError;
    }

    fn snapshot(self: *Self, d: Decision) !void {
        try self.decision_stack.append(d);
    }

    pub fn emptyCellCount(self: *const Self) usize {
        var count: usize = 0;
        for (0..Y_SIZE) |y| {
            for (0..X_SIZE) |x| {
                if (self.cells[y][x].value == 0) count += 1;
            }
        }
        return count;
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
