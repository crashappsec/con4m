import unittest, logging
import con4m

const conffile = "tests/conf.test"

test "hello, world":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))
  addDefaultBuiltins()

  let cfg = evalConfig(conffile)

  check true
