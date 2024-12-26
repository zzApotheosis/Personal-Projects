const std = @import("std");
const Node = @import("node.zig").Node;

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
                self.head = null;
            }
        }

        pub fn add(self: *Self, a: T) !void {
            if (self.head) |head| {
                try traverseAdd(head, a);
            } else {
                self.head = try self.allocator.alloc(Node(T), 1);
                self.head.?[0] = Node(T).new(self.allocator);
                self.head.?[0].data = a;
            }
        }

        fn traverseAdd(n: ?[]Node(T), a: T) !void {
            if (a < n.?[0].data) {
                if (n.?[0].left == null) {
                    try n.?[0].setLeft(a);
                } else {
                    try traverseAdd(n.?[0].left, a);
                }
            } else {
                if (n.?[0].right == null) {
                    try n.?[0].setRight(a);
                } else {
                    try traverseAdd(n.?[0].right, a);
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
                try traverseToArrayList(head[0], &array_list);
            return array_list;
        }

        fn traverseToArrayList(n: Node(T), array_list: *std.ArrayList(T)) !void {
            if (n.left) |left|
                try traverseToArrayList(left[0], array_list);
            try array_list.append(n.data);
            if (n.right) |right|
                try traverseToArrayList(right[0], array_list);
        }

        pub fn size(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                traverseSize(head[0], &n);
            return n;
        }

        fn traverseSize(n: Node(T), s: *usize) void {
            s.* += 1;

            if (n.left) |left|
                traverseSize(left[0], s);

            if (n.right) |right|
                traverseSize(right[0], s);
        }

        pub fn depth(self: *const Self, t: T) usize {
            var n: usize = 0;
            if (self.head) |head|
                traverseDepth(head[0], t, 0, &n);
            return n;
        }

        fn traverseDepth(n: Node(T), t: T, d: usize, s: *usize) void {
            if (n.data == t) {
                s.* = d;
                return;
            }

            if (n.left) |left|
                traverseDepth(left[0], t, d + 1, s);

            if (n.right) |right|
                traverseDepth(right[0], t, d + 1, s);
        }

        pub fn maxDepth(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                traverseMaxDepth(head[0], 0, &n);
            return n;
        }

        fn traverseMaxDepth(n: Node(T), d: usize, s: *usize) void {
            if (d > s.*)
                s.* = d;

            if (n.left) |left|
                traverseMaxDepth(left[0], d + 1, s);

            if (n.right) |right|
                traverseMaxDepth(right[0], d + 1, s);
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

        pub fn balance(self: *Self) !void {
            var array_list: std.ArrayList(T) = try self.toArrayList();
            // This is unneeded because the memory owned by
            // array_list is freed when toOwnedSlice() is called.
            // defer array_list.deinit();
            const a: []T = try array_list.toOwnedSlice();
            defer self.allocator.free(a);
            self.deinit();
            try self.traverseBalance(a);
        }

        fn traverseBalance(self: *Self, a: []T) !void {
            const middle: usize = a.len / 2;
            if (a.len > 0)
                try self.add(a[middle]);
            if (a.len > 1) {
                try self.traverseBalance(a[0..middle]);
                try self.traverseBalance(a[middle + 1 .. a.len]);
            }
        }
    };
}
