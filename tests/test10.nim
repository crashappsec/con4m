import unittest, logging, streams

import con4m, nimutils


let s = """
enum A, B, C, D, E, F, G

func test() {
  echo(format("The integer value for the enum val F is: {F}"))
}

test()
"""

test "manual inspection":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  let
    tree = parse(s.newStringStream())
  let
    ctx = tree.evalTree().getOrElse(nil)
  check ctx != nil
