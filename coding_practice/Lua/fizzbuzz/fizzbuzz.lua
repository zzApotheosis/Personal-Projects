#!/usr/bin/lua

for i = 1, 100, 1 do
    msg = ""
    if i % 3 == 0 then
        msg = msg .. "fizz"
    end
    if i % 5 == 0 then
        msg = msg .. "buzz"
    end
    if string.len(msg) == 0 then
        msg = msg .. i
    end
    print(msg)
end
