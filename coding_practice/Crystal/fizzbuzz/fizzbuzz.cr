start = 1
stop = 100
step = 1
i = start
while i != stop
    msg = ""
    if i % 3 == 0
        msg = sprintf "%s%s", msg, "fizz"
    end
    if i % 5 == 0
        msg = sprintf "%s%s", msg, "buzz"
    end
    if msg.size == 0
        msg = sprintf "%d", i
    end
    STDOUT.printf "%s\n", msg
    STDOUT.flush
    i += step
end
