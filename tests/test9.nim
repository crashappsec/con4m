import unittest, logging, streams

import con4m


let shouldblowup = """

func fact(f) {
  if (f == 1) {
    return f
  }
  
  return fact(f - 1) * f
}

x := 0

for i from 1 to 5 {
  x := fact(i)
  echo(string(x))
}
"""

## Shouldn't blow up
let s = """

func doFact(f) {
  return fact(f)
}

func fact(f) {
  result := 1
  for i from 1 to f {
    result := result * i
  }
}

# This correctly gets detected as a dupe
#func fact(n) {
#  return 1
#}

x := 0

for i from 1 to 10 {
  x := doFact(i)
  echo(format("Fact({i}) = {x}"))
}
"""

test "manual inspection":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  let
    tree = parse(s.newStringStream())
  let
    ctx = tree.evalTree().getOrElse(nil)
  check ctx != nil
