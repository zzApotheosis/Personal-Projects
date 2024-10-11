const std = @import("std");
const testing = std.testing;

pub const MyStruct = struct {
    const Self = @This(); // Not necessary, but very convenient

    _allocator: std.mem.Allocator = undefined,

    mutex: std.Thread.Mutex = undefined,
    condition: std.Thread.Condition = undefined,
    data: std.ArrayList(i32) = undefined,
    stop: bool = false,

    pub fn new() Self {
        var instance: Self = .{
            .mutex = .{},
            .condition = .{},
            ._allocator = std.heap.page_allocator,
        };
        instance.data = std.ArrayList(i32).init(instance._allocator);
        return instance;
    }

    pub fn deinit(self: *Self) void {
        self.data.deinit();
    }

    //pub fn increment(self: *Self) void {
    //    self.data += 1;
    //}

    //pub fn decrement(self: *Self) void {
    //    self.data -= 1;
    //}

    pub fn append(self: *Self, n: i32) void {
        self.data.append(n) catch |e| {
            std.log.err("{}", .{e});
        };
    }

    pub fn pop(self: *Self) i32 {
        return self.data.pop();
    }

    pub fn len(self: *Self) usize {
        return self.data.items.len;
    }

    pub fn lock(self: *Self) void {
        self.mutex.lock();
    }

    pub fn unlock(self: *Self) void {
        self.mutex.unlock();
    }

    pub fn wait(self: *Self) void {
        self.condition.wait(&self.mutex);
    }

    pub fn signal(self: *Self) void {
        self.condition.signal();
    }
};

fn producer_main(my_struct: *MyStruct) void {
    var prng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.posix.getrandom(std.mem.asBytes(&seed)) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
        break :blk seed;
    });
    const rand = prng.random();

    while (true) {
        {
            my_struct.lock();
            defer my_struct.unlock();

            if (my_struct.stop) {
                break;
            }

            for (0..rand.intRangeAtMost(usize, 1, 10)) |_| {
                const n = rand.int(i32);
                std.log.info("Producer thread: Appending {} to the data structure", .{n});
                my_struct.append(n);
            }
        }
        my_struct.signal();
        const delay: u64 = rand.int(u64) % 1_000_000_000;
        std.time.sleep(delay);
    }
}

fn consumer_main(my_struct: *MyStruct) void {
    while (true) {
        {
            my_struct.lock();
            defer my_struct.unlock();

            while (my_struct.len() == 0 and !my_struct.stop) {
                my_struct.wait();
            }

            while (my_struct.len() > 0) {
                std.log.info("Consumer thread: Got data ({})", .{my_struct.pop()});
            }

            if (my_struct.stop) {
                break;
            }
        }
    }
}

test "Dumb Multithreading Test" {
    const duration: u64 = 15_000_000_000;

    var my_struct = MyStruct.new();
    my_struct._allocator = std.testing.allocator;
    const producer = std.Thread.spawn(.{}, producer_main, .{&my_struct}) catch |e| {
        std.log.err("{}", .{e});
        return;
    };
    const consumer = std.Thread.spawn(.{}, consumer_main, .{&my_struct}) catch |e| {
        std.log.err("{}", .{e});
        return;
    };

    std.time.sleep(duration);

    // Get in there and terminate this shit
    {
        my_struct.lock();
        defer my_struct.unlock();

        my_struct.stop = true;
    }
    my_struct.signal();

    producer.join();
    consumer.join();
}
