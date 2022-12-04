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

  check unbox[int](ctx.getConfigVar("item.okay.foo").get()) == 2

  for item in ctx.getSections():
    echo ctx.getSections(item)
