import unittest, logging
import con4m
import options
import streams

const conffile = """

good_ids: [12]


defaults {
  test: 12
}

item {
  foo: 1
}

item "okay" {
  foo: 2
  bar: 2
}

item "not okay" bar {
  foo: 3
  boz: 3
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

  let ctx = newConfigState(st, spec)

  check not ctx.validateConfig()

  for item in ctx.errors:
    echo item

  check len(ctx.errors) == 2
