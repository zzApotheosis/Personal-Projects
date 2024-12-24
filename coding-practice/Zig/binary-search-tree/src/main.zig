const std = @import("std");

pub fn main() !void {
    var node: Node(i32) = Node(i32).new(std.heap.c_allocator);
    defer node.deinit();
    node.setData(5);
    try node.setLeft(7);
    try node.setLeft(8);
    try node.setRight(5);
    std.debug.print("data = {}\n", .{node.getData()});

    // const ligma = struct {};
    // std.debug.print("{}\n", .{@TypeOf(ligma)});

    var bst: BST(i32) = BST(i32).new(std.heap.c_allocator);
    defer bst.deinit();
    try bst.add(50);
    try bst.add(2);
    for (0..100) |i| {
        try bst.add(@as(i32, @intCast(i)));
    }
    const m: []u8 = bst.to_string(std.heap.c_allocator);
    defer std.heap.c_allocator.free(m);
    std.debug.print("{s}\n", .{m});
}

fn BST(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator = undefined,
        head: ?[]Node(T) = null,

        pub fn new(allocator: std.mem.Allocator) BST(T) {
            return .{
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.head) |head| {
                head[0].deinit();
                self.allocator.free(head);
            }
        }

        pub fn add(self: *Self, a: T) !void {
            if (self.head) |head| {
                try push(head, a);
            } else {
                self.head = try self.allocator.alloc(Node(T), 1);
                self.head.?[0] = Node(T).new(self.allocator);
                self.head.?[0].data = a;
            }
        }

        fn push(node: ?[]Node(T), a: T) !void {
            if (a < node.?[0].data) {
                if (node.?[0].left == null) {
                    try node.?[0].setLeft(a);
                } else {
                    try push(node.?[0].left, a);
                }
            } else {
                if (node.?[0].right == null) {
                    try node.?[0].setRight(a);
                } else {
                    try push(node.?[0].right, a);
                }
            }
        }

        pub fn to_string(self: *Self, allocator: std.mem.Allocator) []u8 {
            _ = self;
            var a: []u8 = allocator.alloc(u8, 20) catch {
                unreachable; // This is terrible error handling but I don't care
            };
            for (0..a.len) |i| {
                a[i] = 'i';
            }
            return a;
        }
    };
}

fn Node(comptime T: type) type {
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
