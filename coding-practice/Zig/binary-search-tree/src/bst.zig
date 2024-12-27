const std = @import("std");
const Node = @import("node.zig").Node;

pub fn BST(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator = undefined,
        head: ?[]Node(K, V) = null,

        pub fn init(allocator: std.mem.Allocator) BST(K, V) {
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

        pub fn add(self: *Self, k: K, v: V) !void {
            if (self.head) |head| {
                try traverseAdd(head, k, v);
            } else {
                self.head = try self.allocator.alloc(Node(K, V), 1);
                self.head.?[0] = Node(K, V).init(self.allocator);
                self.head.?[0].key = k;
                self.head.?[0].value = v;
            }
        }

        fn traverseAdd(node: ?[]Node(K, V), k: K, v: V) !void {
            if (k < node.?[0].key) {
                if (node.?[0].left == null) {
                    try node.?[0].setLeft(k, v);
                } else {
                    try traverseAdd(node.?[0].left, k, v);
                }
            } else {
                if (node.?[0].right == null) {
                    try node.?[0].setRight(k, v);
                } else {
                    try traverseAdd(node.?[0].right, k, v);
                }
            }
        }

        pub fn get(self: *const Self, k: K) !V {
            return try traverseGet(self.head, k);
        }

        fn traverseGet(node: ?[]Node(K, V), k: K) !V {
            if (node) |n| {
                if (k == n[0].key) {
                    return n[0].value;
                }

                if (k < n[0].key) {
                    return try traverseGet(n[0].left, k);
                } else {
                    return try traverseGet(n[0].right, k);
                }
            } else {
                return error.NoSuchKey;
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

        pub fn toArrayList(self: *const Self) !std.ArrayList(Node(K, V)) {
            var array_list: std.ArrayList(Node(K, V)) = std.ArrayList(Node(K, V)).init(self.allocator);
            if (self.head) |head|
                try traverseToArrayList(head[0], &array_list);
            return array_list;
        }

        fn traverseToArrayList(node: Node(K, V), array_list: *std.ArrayList(Node(K, V))) !void {
            if (node.left) |left|
                try traverseToArrayList(left[0], array_list);
            try array_list.append(node);
            if (node.right) |right|
                try traverseToArrayList(right[0], array_list);
        }

        pub fn size(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                traverseSize(head[0], &n);
            return n;
        }

        fn traverseSize(n: Node(K, V), s: *usize) void {
            s.* += 1;

            if (n.left) |left|
                traverseSize(left[0], s);

            if (n.right) |right|
                traverseSize(right[0], s);
        }

        pub fn depth(self: *const Self, t: K) usize {
            var n: usize = 0;
            if (self.head) |head|
                traverseDepth(head[0], t, 0, &n);
            return n;
        }

        fn traverseDepth(n: Node(K, V), t: K, d: usize, s: *usize) void {
            if (n.key == t) {
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

        fn traverseMaxDepth(n: Node(K, V), d: usize, s: *usize) void {
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

        fn recursiveDump(n: []Node(K, V), f: std.fs.File) !void {
            const writer = f.writer();
            try writer.print("(Node <{*}>, <{}>=<{}>, left <{*}>, right <{*}>)\n", .{
                &n[0],
                n[0].key,
                n[0].value,
                if (n[0].left) |left| &left[0] else null,
                if (n[0].right) |right| &right[0] else null,
            });
            if (n[0].left) |left|
                try recursiveDump(left, f);
            if (n[0].right) |right|
                try recursiveDump(right, f);
        }

        pub fn balance(self: *Self) !void {
            var array_list: std.ArrayList(Node(K, V)) = try self.toArrayList();
            // This is unneeded because the memory owned by
            // array_list is freed when toOwnedSlice() is called.
            // defer array_list.deinit();
            const a: []Node(K, V) = try array_list.toOwnedSlice();
            defer self.allocator.free(a);
            self.deinit();
            try self.traverseBalance(a);
        }

        fn traverseBalance(self: *Self, a: []Node(K, V)) !void {
            const middle: usize = a.len / 2;
            if (a.len > 0)
                try self.add(a[middle].key, a[middle].value);
            if (a.len > 1) {
                try self.traverseBalance(a[0..middle]);
                try self.traverseBalance(a[middle + 1 .. a.len]);
            }
        }
    };
}
