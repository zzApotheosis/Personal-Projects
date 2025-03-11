proc add(a, b: int): int {.exportc, dynlib.} =
  return a + b

proc greet(name: cstring): cstring {.exportc, dynlib.} =
  result = ("Hello, " & $name).cstring
