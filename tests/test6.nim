import unittest, logging
import con4m
import options
import streams

const conffile = """

good_ids: [12]


defaults {
  test: true
}

item {
  foo: 1
}

item "okay" {
  foo: 2
  bar: 2
}
"""

const nextconffile = """
good_ids: [1,2,3]

defaults {
  test: bar
}

item "okay" {
   foo: 4
   boz: 4
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
  discard itemSection.addAttr("bar", "int", required = false)
  discard itemSection.addAttr("boz", "int", required = false)    

  let tree = parse(newStringStream(conffile))

  check tree != nil

  tree.checkTree()
  tree.evalTree()

  let scopes = tree.scopes.get()
  
  let st = scopes.attrs

  let config = newConfigState(st, spec)
  check config.validateConfig()

  for item in config.errors:
    echo item

  check len(config.errors) == 0
