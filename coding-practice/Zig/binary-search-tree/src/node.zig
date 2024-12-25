const std = @import("std");

pub fn Node(comptime T: type) type {
    return struct {
        const Self = @This();
        allocator: std.mem.Allocator = undefined,
        data: T = undefined,
        left: ?[]Node(T) = null,
        right: ?[]Node(T) = null,

        pub fn new(allocator: std.mem.Allocator) Node(T) {
            return Node(T){
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.left) |left| {
                for (0..left.len) |i| left[i].deinit();
                self.allocator.free(left);
            }
            if (self.right) |right| {
                for (0..right.len) |i| right[i].deinit();
                self.allocator.free(right);
            }
        }

        pub fn setLeft(self: *Self, new_left: ?T) !void {
            if (new_left == null) {
                self.delLeft();
                return;
            }

            if (self.left == null) {
                self.left = try self.allocator.alloc(Node(T), 1);
                self.left.?[0] = Node(T).new(self.allocator);
            }

            self.left.?[0].data = new_left.?;
        }

        pub fn getLeft(self: *const Self) ?[]Node(T) {
            return self.left;
        }

        pub fn setRight(self: *Self, new_right: ?T) !void {
            if (new_right == null) {
                self.delRight();
                return;
            }

            if (self.right == null) {
                self.right = try self.allocator.alloc(Node(T), 1);
                self.right.?[0] = Node(T).new(self.allocator);
            }

            self.right.?[0].data = new_right.?;
        }

        pub fn getRight(self: *const Self) ?[]Node(T) {
            return self.right;
        }

        pub fn delLeft(self: *Self) void {
            self.left.?[0].deinit();
            self.left = null;
        }

        pub fn delRight(self: *Self) void {
            self.right.?[0].deinit();
            self.right = null;
        }

        pub fn setData(self: *Self, new_data: T) void {
            self.data = new_data;
        }

        pub fn getData(self: *const Self) T {
            return self.data;
        }
    };
}
