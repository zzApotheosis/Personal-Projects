for i in 0..100:
  var msg = ""
  if i %% 3 == 0:
    msg.add("fizz")
  if i %% 5 == 0:
    msg.add("buzz")
  if msg.len() == 0:
    msg.add($i)
  write(stdout, msg & "\n")
