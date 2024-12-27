const std = @import("std");

pub fn Node(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();
        allocator: std.mem.Allocator = undefined,
        key: K = undefined,
        value: V = undefined,
        left: ?[]Node(K, V) = null,
        right: ?[]Node(K, V) = null,

        pub fn init(allocator: std.mem.Allocator) Node(K, V) {
            return Node(K, V){
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

        pub fn setLeft(self: *Self, new_key: ?K, new_value: V) !void {
            if (new_key == null) {
                self.delLeft();
                return;
            }

            if (self.left == null) {
                self.left = try self.allocator.alloc(Node(K, V), 1);
                self.left.?[0] = Node(K, V).init(self.allocator);
            }

            self.left.?[0].key = new_key.?;
            self.left.?[0].value = new_value;
        }

        pub fn getLeft(self: *const Self) ?[]Node(K, V) {
            return self.left;
        }

        pub fn setRight(self: *Self, new_key: ?K, new_value: V) !void {
            if (new_key == null) {
                self.delRight();
                return;
            }

            if (self.right == null) {
                self.right = try self.allocator.alloc(Node(K, V), 1);
                self.right.?[0] = Node(K, V).init(self.allocator);
            }

            self.right.?[0].key = new_key.?;
            self.right.?[0].value = new_value;
        }

        pub fn getRight(self: *const Self) ?[]Node(K, V) {
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

        pub fn setData(self: *Self, new_data: K) void {
            self.key = new_data;
        }

        pub fn getData(self: *const Self) K {
            return self.key;
        }
    };
}
