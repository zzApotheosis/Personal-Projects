i = 1
while i <= 100
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
  i += 1
end
