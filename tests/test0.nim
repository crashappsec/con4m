import unittest, logging, streams

import con4m

let prog = """
s := { "hello" : "hi", "world": "there"}

echo(s["hello"])
echo(s["world"])
"""

test "manual inspection":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  let
    tree = parse(prog.newStringStream())
    ctx = tree.evalTree().getOrElse(nil)
    
  check ctx != nil

