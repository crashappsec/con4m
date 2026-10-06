import std/[options, tables, unittest]
import con4m, nimutils

proc runtime(): ConfigState =
  result = ConfigState(attrs: AttrScope(parent: none(AttrScope), name: "root"))
  result.attrs.config = result
  result.addDefaultBuiltins()

proc component(state: ConfigState, source: string): ComponentInfo =
  result = state.getComponentReference("params")
  result.cacheComponent(source)
  discard state.loadComponent(result)

suite "component parameter values":
  test "configured variables override defaults":
    let state = runtime()
    let comp = state.component("parameter var p { default: true }\nanswer = p")
    state.setVariableParamValue(comp, "p", pack(false), boolType)
    state.evalComponent(comp)
    check not unpack[bool](state.attrs.attrLookup("answer").get())

  test "duration attribute survives saved-parameter restoration":
    let state = runtime()
    let comp = state.component("parameter p { default: <<30 min>> }")
    state.setAttributeParamValue(comp, "p", pack(120000000), durationType)
    state.evalComponent(comp)
    check unpack[int](state.attrs.attrLookup("p").get()) == 120000000

  test "configured variables without defaults evaluate":
    let state = runtime()
    let comp = state.component("parameter var p {}\nvar p: int\nanswer = p")
    state.setVariableParamValue(comp, "p", pack(7), intType)
    state.evalComponent(comp)
    check unpack[int](state.attrs.attrLookup("answer").get()) == 7

  test "duration variables use the configured value":
    let state = runtime()
    let comp = state.component("parameter var p { default: <<30 min>> }\nanswer = p")
    state.setVariableParamValue(comp, "p", pack(120000000), durationType)
    state.evalComponent(comp)
    check unpack[int](state.attrs.attrLookup("answer").get()) == 120000000

  test "scalar literals retain their boxed representations":
    for literal in ["<<2 kb>>", "'a'", "<<192.168.0.1>>", "<<192.168.0.0/16>>",
                    "<<2004-01-06>>", "<<12:23:01Z>>", "<<2004-01-06T12:23:01Z>>"]:
      let state = runtime()
      let comp = state.component("parameter p { default: " & literal & " }")
      let param = comp.attrParams["p"]
      state.setAttributeParamValue(comp, "p", param.default.get(), param.defaultType)
      state.evalComponent(comp)
      check state.attrs.attrLookup("p").get() == param.default.get()

  test "homogeneous tuples are validated as tuples":
    let state = runtime()
    let comp = state.component("parameter p { default: (1, 2) }")
    let value = pack(@[pack(3), pack(4)])
    state.setAttributeParamValue(comp, "p", value, "tuple[int, int]".toCon4mType())
    state.evalComponent(comp)
    check state.attrs.attrLookup("p").get() == value

  test "nested floats are coerced according to the declared type":
    let state = runtime()
    let comp = state.component("parameter p { default: ([1.0], (2.0, 3.0)) }")
    let value = pack(@[pack(@[pack(4)]), pack(@[pack(5), pack(6)])])
    state.setAttributeParamValue(comp, "p", value,
                                "tuple[list[float], tuple[float, float]]".toCon4mType())
    state.evalComponent(comp)
    let expected = pack(@[pack(@[pack(4.0)]), pack(@[pack(5.0), pack(6.0)])])
    check state.attrs.attrLookup("p").get() == expected

  test "dictionary floats are coerced and checked":
    let state = runtime()
    let comp = state.component("parameter p { default: {\"x\": 1.0} }")
    let value = newOrderedTable[string, Box]()
    value["x"] = pack(2)
    state.setAttributeParamValue(comp, "p", pack(value), "dict[string, float]".toCon4mType())
    state.evalComponent(comp)
    let actual = unpack[OrderedTableRef[string, Box]](state.attrs.attrLookup("p").get())
    check unpack[float](actual["x"]) == 2.0
    value["x"] = pack("invalid")
    expect ValueError:
      state.setAttributeParamValue(comp, "p", pack(value), "dict[string, float]".toCon4mType())

  test "invalid tuple lengths and elements are rejected":
    let state = runtime()
    let comp = state.component("parameter p { default: (1, 2) }")
    for value in [pack(@[pack(3)]), pack(@[pack(3), pack("invalid")]), pack(3)]:
      expect ValueError:
        state.setAttributeParamValue(comp, "p", value, "tuple[int, int]".toCon4mType())

  test "invalid scalar values and missing boxes are rejected":
    let state = runtime()
    let comp = state.component("parameter var p { default: 30.0 }\nanswer = p")
    for value in [pack("invalid"), pack(false), Box(nil)]:
      expect ValueError:
        state.setVariableParamValue(comp, "p", value, floatType)
    state.setVariableParamValue(comp, "p", pack(2), floatType)
    state.evalComponent(comp)
    check unpack[float](state.attrs.attrLookup("answer").get()) == 2.0
