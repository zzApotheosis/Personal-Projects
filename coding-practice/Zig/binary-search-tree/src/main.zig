const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    // var node: Node(i32) = Node(i32).new(allocator);
    // defer node.deinit();
    // node.setData(5);
    // try node.setLeft(7);
    // try node.setLeft(8);
    // try node.setRight(5);
    // std.debug.print("data = {}\n", .{node.getData()});

    // const ligma = struct {};
    // std.debug.print("{}\n", .{@TypeOf(ligma)});

    var bst: BST(i32) = BST(i32).new(allocator);
    defer bst.deinit();
    try bst.add(10);
    try bst.add(5);
    try bst.add(15);
    try bst.add(3);
    try bst.add(7);
    try bst.add(13);
    try bst.add(17);
    try bst.add(20);
    const m: []u8 = bst.to_string(allocator);
    defer allocator.free(m);
    std.debug.print("{s}\n", .{m});

    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst depth: {}\n", .{bst.depth()});
    try bst.dump(std.io.getStdErr());

    var bst2: BST(i32) = BST(i32).new(allocator);
    defer bst2.deinit();
    try bst2.add(0);
    std.debug.print("bst2 size: {}\n", .{bst2.size()});
    std.debug.print("bst2 depth: {}\n", .{bst2.depth()});

    var bst3: BST(i32) = BST(i32).new(allocator);
    defer bst3.deinit();
    std.debug.print("bst3 size: {}\n", .{bst3.size()});
    std.debug.print("bst3 depth: {}\n", .{bst3.depth()});
}

fn BST(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator = undefined,
        head: ?[]Node(T) = null,

        fn new(allocator: std.mem.Allocator) BST(T) {
            return .{
                .allocator = allocator,
            };
        }

        fn deinit(self: *Self) void {
            if (self.head) |head| {
                head[0].deinit();
                self.allocator.free(head);
            }
        }

        fn add(self: *Self, a: T) !void {
            if (self.head) |head| {
                try push(head, a);
            } else {
                self.head = try self.allocator.alloc(Node(T), 1);
                self.head.?[0] = Node(T).new(self.allocator);
                self.head.?[0].data = a;
            }
        }

        fn to_string(self: *Self, allocator: std.mem.Allocator) []u8 {
            _ = self;
            var a: []u8 = allocator.alloc(u8, 20) catch {
                unreachable; // This is terrible error handling but I don't care
            };
            for (0..a.len) |i| {
                a[i] = 'i';
            }
            return a;
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

        fn size(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                _size_traverse(head[0], &n);
            return n;
        }

        fn _size_traverse(node: Node(T), n: *usize) void {
            n.* += 1;

            if (node.left) |left|
                _size_traverse(left[0], n);

            if (node.right) |right|
                _size_traverse(right[0], n);
        }

        fn depth(self: *const Self) usize {
            var n: usize = 0;
            if (self.head) |head|
                _depth_traverse(head[0], 0, &n);
            return n;
        }

        fn _depth_traverse(node: Node(T), d: usize, n: *usize) void {
            if (d > n.*)
                n.* = d;

            if (node.left) |left|
                _depth_traverse(left[0], d + 1, n);

            if (node.right) |right|
                _depth_traverse(right[0], d + 1, n);
        }

        fn dump(self: *const Self, f: std.fs.File) !void {
            if (self.head) |head|
                try _recursive_dump(head, f);
        }

        fn _recursive_dump(node: []Node(T), f: std.fs.File) !void {
            const writer = f.writer();
            try writer.print("(Node <{*}>, data <{}>, left <{*}>, right <{*}>)\n", .{
                &node[0],
                node[0].data,
                if (node[0].left) |left| &left[0] else null,
                if (node[0].right) |right| &right[0] else null,
            });
            if (node[0].left) |left|
                try _recursive_dump(left, f);
            if (node[0].right) |right|
                try _recursive_dump(right, f);
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
