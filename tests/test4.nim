import unittest, logging
import con4m
import options
import streams

const conffile = """

good_ids: [12]


defaults {
  test: 12
}

item "okay" {
  foo: 100
}

item "not okay" bar {
  foo: 100
}

"""

test "hello, world":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))
  addDefaultBuiltins()

  var spec = newConfigSpec()
  discard spec.addGlobalAttr("good_ids", "[int]")
  discard spec.addGlobalAttr("fart", "bool", required = false)
  var defaultSection = spec.addSection("defaults")
  discard defaultSection.addAttr("test", "bool")

  var itemSection = spec.addSection("item", validSubSecs = @["*"])
  discard itemSection.addAttr("foo", "int")

  let tree = parse(newStringStream(conffile))

  check tree != nil

  tree.checkTree()
  tree.evalTree()

  let scopes = tree.scopes.get()
  
  let st = scopes.attrs

  let state = validateConfig(st, spec)

  for item in state.errors:
    echo item

  check len(state.errors) == 1
