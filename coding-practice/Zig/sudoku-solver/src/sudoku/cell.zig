const std = @import("std");

pub const Cell = struct {
    const Self: type = @This();
    const POSSIBLE_VALUES_MIN: u8 = 1;
    const POSSIBLE_VALUES_MAX: u8 = 9;

    value: u8 = 0,
    given: u8 = 0,
    possible_values: [POSSIBLE_VALUES_MAX]bool = .{false} ** POSSIBLE_VALUES_MAX,

    pub fn print(self: *const Self, f: std.fs.File) !void {
        const fw = f.writer();
        try fw.print("Cell\n", .{});
        try fw.print("Value: {}\n", .{self.value});
        try fw.print("Given: {}\n", .{self.given});
        try fw.print("Possible values: {any}\n", .{self.possible_values});
    }
};
