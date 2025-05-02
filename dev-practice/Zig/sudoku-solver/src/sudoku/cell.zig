const std = @import("std");

pub const Cell = struct {
    const Self: type = @This();
    pub const POSSIBLE_VALUES_MIN: u8 = 1;
    pub const POSSIBLE_VALUES_MAX: u8 = 9;

    value: u8 = 0,
    given: u8 = 0,
    possible_values: [POSSIBLE_VALUES_MAX]bool = .{false} ** POSSIBLE_VALUES_MAX,

    pub fn numPossibleValues(self: *const Self) usize {
        var n: usize = 0;
        for (0..self.possible_values.len) |i| {
            if (self.possible_values[i]) n += 1;
        }
        return n;
    }

    pub fn nextPossibleValue(self: *const Self) !u8 {
        for (0..self.possible_values.len) |i| {
            if (self.possible_values[i]) return @truncate(i + 1);
        }
        return error.NoValue;
    }

    pub fn setPossibleValue(self: *Self, v: u8) !void {
        if (v < POSSIBLE_VALUES_MIN or v > POSSIBLE_VALUES_MAX)
            return error.IllegalArgument;
        self.possible_values[v - 1] = true;
    }

    pub fn setImpossibleValue(self: *Self, v: u8) !void {
        if (v < POSSIBLE_VALUES_MIN or v > POSSIBLE_VALUES_MAX)
            return error.IllegalArgument;
        self.possible_values[v - 1] = false;
    }

    pub fn print(self: *const Self, f: std.fs.File) !void {
        const fw = f.writer();
        try fw.print("Value: {}\n", .{self.value});
        try fw.print("Given: {}\n", .{self.given});
        try fw.print("Possible values: {any}\n", .{self.possible_values});
    }
};
