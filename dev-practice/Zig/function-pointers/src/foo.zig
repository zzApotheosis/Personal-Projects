pub fn bar(T: type, f: fn (T: type, a: T, b: T) T, a: T, b: T, r: ?*T) void {
    const c = f(T, a, b);
    r.?.* = c;
}
