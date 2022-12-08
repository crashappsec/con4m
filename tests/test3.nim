import unittest, logging
import con4m
import options

const conffile = "tests/conf.test"

test "hello, world":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  var
    state: ConfigState
    cfg:   Option[Con4mScope]
    
  (state, cfg) = evalConfig(conffile)

  check true
