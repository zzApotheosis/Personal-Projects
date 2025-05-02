mut msg := ""
for i := 1; i <= 100; i++ {
    msg = ""
    if i % 3 == 0 {
        msg += "fizz"
    }
    if i % 5 == 0 {
        msg += "buzz"
    }
    if msg.len == 0 {
        msg += i.str()
    }
    println(msg)
}
