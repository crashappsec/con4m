import unittest, logging
import con4m
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

  var spec = newConfigSpec()
  discard spec.addGlobalAttr("good_ids", "[int]")
  discard spec.addGlobalAttr("fart", "bool", required = false)
  var defaultSection = spec.addSection("defaults")
  discard defaultSection.addAttr("test", "bool")

  var itemSection = spec.addSection("item", validSubSecs = @["*"])
  discard itemSection.addAttr("foo", "int")
  discard itemSection.addAttr("bar", "int", required = false)
  discard itemSection.addAttr("boz", "int", required = false)    

  let
    tree = parse(newStringStream(conffile))
    
  check tree != nil

  let ctx = tree.checkTree()
  tree.evalTree(ctx)
  ctx.addSpec(spec)
  check ctx.validateConfig()

  for item in ctx.errors:
    echo item

  check len(ctx.errors) == 0
