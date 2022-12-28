import unittest, logging
import con4m
import nimutils
import nimutils/box
import options
import streams

const conffile = """

good_ids: [12]

test1: "test"

defaults {
  test: true
}

item {
  foo: 1
}

item "okay" {
  foo: 2
  bar: "example"
}

s: format("set an {item.okay.bar}!")
echo(s)

test2: format("This is {test1} #{item.okay.foo} of format")
echo(test2)





"""

test "hello, world":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  var spec = newConfigSpec()
  spec.addGlobalAttr("good_ids", "[int]")
  spec.addGlobalAttr("fart", "bool", required = false)
  spec.addGlobalAttr("test1", "string")
  spec.addGlobalAttr("test2", "string")
  spec.addGlobalAttr("s", "string")

  var defaultSection = spec.addSection("defaults")
  defaultSection.addAttr("test", "bool")

  var itemSection = spec.addSection("item", validSubSecs = @["*"])
  itemSection.addAttr("foo", "int")
  itemSection.addAttr("bar", "string", required = false)
  itemSection.addAttr("boz", "int", required = false)

  let
    tree = parse(newStringStream(conffile))

  check tree != nil

  let ctx = tree.evalTree().getOrElse(nil)

  check ctx != nil

  ctx.addSpec(spec)

  check ctx.validateConfig()

  for item in ctx.errors:
    echo item

  check unpack[int](ctx.getConfigVar("item.okay.foo").get()) == 2
  check unpack[string](ctx.getConfigVar("s").get()) == "set an example!"

