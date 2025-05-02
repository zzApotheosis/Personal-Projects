const std = @import("std");

const limit: u32 = 15;

var mutex = std.Thread.Mutex{};
const datatype = u8;
var data: datatype = undefined;
var cond = std.Thread.Condition{};

pub fn main() void {
    const producer_thread = std.Thread.spawn(.{}, producer_main, .{}) catch |e| {
        std.log.err("error: {}", .{e});
        return;
    };
    const consumer_thread = std.Thread.spawn(.{}, consumer_main, .{}) catch |e| {
        std.log.err("error: {}", .{e});
        return;
    };
    std.log.info("Joining producer thread", .{});
    producer_thread.join();
    std.log.info("Joining consumer thread", .{});
    consumer_thread.join();
}

fn producer_main() void {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.posix.getrandom(std.mem.asBytes(&seed)) catch |e| {
            std.log.err("error: {}", .{e});
            return;
        };
        break :blk seed;
    });
    const rand = prng.random();

    var iter: u32 = 0;
    while (true) {
        if (iter >= limit) {
            break;
        }
        const delay = rand.float(f32);
        std.time.sleep(@intFromFloat(delay * 2 * std.time.ns_per_s));
        mutex.lock();
        data = rand.int(datatype);
        mutex.unlock();
        cond.signal();
        iter += 1;
    }
}

fn consumer_main() void {
    var iter: u32 = 0;
    while (true) {
        if (iter >= limit) {
            break;
        }
        mutex.lock();
        cond.wait(&mutex);
        std.log.info("data={}", .{data});
        mutex.unlock();
        iter += 1;
    }
}
