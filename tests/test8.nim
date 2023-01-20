import unittest, logging, streams

import con4m, nimutils


let s = """
x := (1, "a", [1, 2, 3])
a := x[0]
b := x[1]
c := x[2]
echo(string(a))
echo(b)
for i from 0 to 2 {
  echo(string(c[i]))
}

d, e, f := x

echo(string(f[1])) # should be 2

out, status := system("ls")

echo("Output is: ", out)
echo("Status was: ", string(status))
"""

test "manual inspection":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  let
    tree = parse(s.newStringStream())
    ctx = tree.evalTree().getOrElse(nil)
  check ctx != nil
