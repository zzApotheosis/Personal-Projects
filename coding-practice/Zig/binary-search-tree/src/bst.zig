const std = @import("std");
const node = @import("node.zig");
const Node = node.Node;

pub fn BST(comptime T: type) type {
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
                try addTraverse(head, a);
            } else {
                self.head = try self.allocator.alloc(Node(T), 1);
                self.head.?[0] = Node(T).new(self.allocator);
                self.head.?[0].data = a;
            }
        }

        fn addTraverse(n: ?[]Node(T), a: T) !void {
            if (a < n.?[0].data) {
                if (n.?[0].left == null) {
                    try n.?[0].setLeft(a);
                } else {
                    try addTraverse(n.?[0].left, a);
                }
            } else {
                if (n.?[0].right == null) {
                    try n.?[0].setRight(a);
                } else {
                    try addTraverse(n.?[0].right, a);
                }
            }
        }

        pub fn toString(self: *Self, allocator: std.mem.Allocator) []u8 {
            _ = self;
            var a: []u8 = allocator.alloc(u8, 20) catch {
                unreachable; // This is terrible error handling but I don't care
            };
            for (0..a.len) |i| {
                a[i] = 'i';
            }
            return a;
        }

        pub fn toArrayList(self: *const Self) !std.ArrayList(T) {
            var array_list: std.ArrayList(T) = std.ArrayList(T).init(self.allocator);
            if (self.head) |head|
                try toArrayListTraverse(head[0], &array_list);
            return array_list;
        }

        fn toArrayListTraverse(n: Node(T), array_list: *std.ArrayList(T)) !void {
            if (n.left) |left|
                try toArrayListTraverse(left[0], array_list);
            try array_list.append(n.data);
            if (n.right) |right|
                try toArrayListTraverse(right[0], array_list);
        }

        pub fn size(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                sizeTraverse(head[0], &n);
            return n;
        }

        fn sizeTraverse(n: Node(T), s: *usize) void {
            s.* += 1;

            if (n.left) |left|
                sizeTraverse(left[0], s);

            if (n.right) |right|
                sizeTraverse(right[0], s);
        }

        pub fn depth(self: *const Self, t: T) usize {
            var n: usize = 0;
            if (self.head) |head|
                depthTraverse(head[0], t, 0, &n);
            return n;
        }

        fn depthTraverse(n: Node(T), t: T, d: usize, s: *usize) void {
            if (n.data == t) {
                s.* = d;
                return;
            }

            if (n.left) |left|
                depthTraverse(left[0], t, d + 1, s);

            if (n.right) |right|
                depthTraverse(right[0], t, d + 1, s);
        }

        pub fn maxDepth(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                maxDepthTraverse(head[0], 0, &n);
            return n;
        }

        fn maxDepthTraverse(n: Node(T), d: usize, s: *usize) void {
            if (d > s.*)
                s.* = d;

            if (n.left) |left|
                maxDepthTraverse(left[0], d + 1, s);

            if (n.right) |right|
                maxDepthTraverse(right[0], d + 1, s);
        }

        pub fn dump(self: *const Self, f: std.fs.File) !void {
            if (self.head) |head|
                try recursiveDump(head, f);
        }

        fn recursiveDump(n: []Node(T), f: std.fs.File) !void {
            const writer = f.writer();
            try writer.print("(Node <{*}>, data <{}>, left <{*}>, right <{*}>)\n", .{
                &n[0],
                n[0].data,
                if (n[0].left) |left| &left[0] else null,
                if (n[0].right) |right| &right[0] else null,
            });
            if (n[0].left) |left|
                try recursiveDump(left, f);
            if (n[0].right) |right|
                try recursiveDump(right, f);
        }
    };
}
