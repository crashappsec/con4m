import unittest, logging, streams

import con4m, nimutils


let blowupIfDisallowRecursion = """

func fact(f) {
  if (f <= 1) {
    result := f
    return
  } 
  return fact(f - 1) * f
}

x := 0

for i from 1 to 10 {
  x := fact(i)
  echo(format("Fact({i}) = {x}"))
}
"""

## Shouldn't blow up ever
when false:
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

#x := 0

for i from 1 to 10 {
  x := doFact(i)
  echo(format("Fact({i}) = {x}"))
}
"""

test "manual inspection":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  let
    tree = parse(blowupIfDisallowRecursion.newStringStream())
  let
    ctx = tree.evalTree().getOrElse(nil)
  check ctx != nil
