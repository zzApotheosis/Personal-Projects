const std = @import("std");
const BST = @import("bst.zig").BST;

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const K: type = i32;
    const V: type = i32;

    const key_of_interest: K = 495;
    const limit: usize = 512;

    var bst = BST(K, V).init(allocator);
    defer bst.deinit();
    for (0..limit) |i| {
        const n: K = @intCast(i);
        try bst.add(n, @as(V, @intCast(limit)) - n);
    }
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        key_of_interest,
        bst.depth(key_of_interest),
    });
    // try bst.dump(std.io.getStdErr());

    try bst.balance();
    std.debug.print("After binary tree rebalance...\n", .{});
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        key_of_interest,
        bst.depth(key_of_interest),
    });
    // try bst.dump(std.io.getStdErr());

    var name_tree: BST(i32, []u8) = BST(i32, []u8).init(allocator);
    const keyname_of_interest = 206;
    defer name_tree.deinit();
    try addNames(&name_tree);
    std.debug.print("name_tree size: {}\n", .{name_tree.size()});
    std.debug.print("name_tree max depth: {}\n", .{name_tree.maxDepth()});
    std.debug.print("name_tree node <{}> depth: {}\n", .{
        keyname_of_interest,
        name_tree.depth(keyname_of_interest),
    });
    // try name_tree.dump(std.io.getStdErr());
    const m = try name_tree.get(keyname_of_interest);
    std.debug.print("{}: {s}\n", .{
        keyname_of_interest,
        m,
    });

    try name_tree.balance();
    std.debug.print("After binary tree rebalance...\n", .{});
    std.debug.print("name_tree size: {}\n", .{name_tree.size()});
    std.debug.print("name_tree max depth: {}\n", .{name_tree.maxDepth()});
    std.debug.print("name_tree node <{}> depth: {}\n", .{
        keyname_of_interest,
        name_tree.depth(keyname_of_interest),
    });

    const names = try name_tree.toArrayList();
    defer names.deinit();
    for (names.items) |node| {
        allocator.free(node.value);
    }
}

fn addNames(bst: *BST(i32, []u8)) !void {
    var name: []u8 = undefined;

    name = try stringFrom(bst.allocator, "Alice");
    try bst.add(10, name);

    name = try stringFrom(bst.allocator, "Bob");
    try bst.add(20, name);

    name = try stringFrom(bst.allocator, "Charlie");
    try bst.add(30, name);

    name = try stringFrom(bst.allocator, "Dwight");
    try bst.add(200, name);

    name = try stringFrom(bst.allocator, "Earl");
    try bst.add(201, name);

    name = try stringFrom(bst.allocator, "Francis");
    try bst.add(202, name);

    name = try stringFrom(bst.allocator, "Gregory");
    try bst.add(203, name);

    name = try stringFrom(bst.allocator, "Harold");
    try bst.add(204, name);

    name = try stringFrom(bst.allocator, "Isaiah");
    try bst.add(205, name);

    name = try stringFrom(bst.allocator, "Jacob");
    try bst.add(206, name);
}

fn stringFrom(allocator: std.mem.Allocator, s: []const u8) ![]u8 {
    const string: []u8 = try allocator.alloc(u8, s.len);
    @memcpy(string, s);
    return string;
}
