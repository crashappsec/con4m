import unittest, logging
import con4m
import con4m/types
import options

const conffile = "tests/conf.test"

test "hello, world":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  var
    state: ConfigState
    cfg: Option[Con4mScope]

  let opt = evalConfig(conffile)

  check opt.isSome()
