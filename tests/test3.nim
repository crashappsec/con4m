import unittest, logging
import con4m

const conffile = "tests/conf.test"

test "hello, world":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))
  addDefaultBuiltins()

  discard loadConfig(conffile)
  
  check true
