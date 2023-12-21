import options, ../common, checker, strutils

const tvarnames = "dtvwxyzabc"

proc numToTVarName(num: int): string =
  var num = num

  while true:
    result.add(tvarnames[num mod 10])
    num = num div 10
    if num == 0:
      break

proc toRope*(x: TypeRef): Rope
proc toRope*(x: TypeId): Rope

proc toRope(x: TypeId, tvars: var Dict[TypeId, int], nvars: var int): Rope
proc toRope(x: TypeRef, tvars: var Dict[TypeId, int], nvars: var int): Rope =
  var x = x.followForwards()

  case x.kind
  of C4Primitive:
    result = text(typeNameFromId(x.typeId))
  of C4TVar:
    if x.localName.isSome():
      result = text("`" & x.localName.get())
    else:
      if tvars.lookup(x.typeId).isNone():
        nvars += 1
        tvars[x.typeId] = nvars
      result = text("`" & numToTVarName(tvars[x.typeId]))
  of C4List:
    result = text("list[") + x.items[0].toRope(tvars, nvars) + text("]")
  of C4Ref:
    result = text("ref[") + x.items[0].toRope(tvars, nvars) + text("]")
  of C4Maybe:
    result = text("maybe[") + x.items[0].toRope(tvars, nvars) + text("]")
  of C4Dict:
    result = text("dict[") + x.items[0].toRope(tvars, nvars) + text(", ") +
            x.items[1].toRope(tvars, nvars) + text("]")
  of C4Tuple:
    result = text("tuple[")
    var itemInfo: seq[Rope]
    for item in x.items:
      itemInfo.add(item.toRope(tvars, nvars))
    result += itemInfo.join(text(", ")) + text("]")
  of C4Func:
    if x.items.len() == 0:
      return text("(callback w no arg spec)")
    result = text("(")
    var itemInfo: seq[Rope]
    for item in x.items:
      itemInfo.add(item.toRope(tvars, nvars))
    if x.va:
      itemInfo[^2] = text("*") + itemInfo[^2]
    result += itemInfo[0 ..< ^1].join(text(", ")) + text(") -> ")
    result += itemInfo[^1]
  of C4Struct:
    result = text("struct[")

    if x.name != "":
      result += text(x.name)
    else:
      if tvars.lookup(x.typeId).isNone():
        nvars += 1
        tvars[x.typeId] = nvars
      result += text("`" & numToTVarName(tvars[x.typeId]))
  of C4TypeSpec:
    if x.items.len() == 0:
      result = text("typespec")
    else:
      result = text("typespec[")
      result += x.items[0].toRope(tvars, nvars) + text("]")
  of C4OneOf:
    result = text("oneof[")
    var itemInfo: seq[Rope]
    for item in x.items:
      iteminfo.add(item.toRope(tvars, nvars))
    result += itemInfo.join(text(", ")) + text("]")

proc toRope(x: TypeId, tvars: var Dict[TypeId, int], nvars: var int): Rope =
  return typeStore[x].toRope(tvars, nvars)

proc toRope*(x: TypeRef): Rope =
  var
    tbl: Dict[TypeId, int]
    n:   int

  tbl.initDict()
  return x.toRope(tbl, n)

proc toRope*(x: TypeId): Rope =
  typeStore[x].toRope()

proc toStr(x: TypeId, tvars: var Dict[TypeId, int], nvars: var int): string
proc toStr(x: TypeRef, tvars: var Dict[TypeId, int], nvars: var int): string =
  var x = x.followForwards()

  case x.kind
  of C4Primitive:
    result = typeNameFromId(x.typeId)
  of C4TVar:
    if x.localName.isSome():
      result = "`" & x.localName.get()
    else:
      if tvars.lookup(x.typeId).isNone():
        nvars += 1
        tvars[x.typeId] = nvars
      result = "`" & numToTVarName(tvars[x.typeId])
  of C4List:
    result = "list[" & x.items[0].toStr(tvars, nvars) & "]"
  of C4Ref:
    result = "ref[" & x.items[0].toStr(tvars, nvars) & "]"
  of C4Maybe:
    result = "maybe[" & x.items[0].toStr(tvars, nvars) & "]"
  of C4Dict:
    result = "dict[" & x.items[0].toStr(tvars, nvars) & ", " &
            x.items[1].toStr(tvars, nvars) & "]"
  of C4Tuple:
    result = "tuple["
    var itemInfo: seq[string]
    for item in x.items:
      itemInfo.add(item.toStr(tvars, nvars))
    result &= itemInfo.join(", ") & "]"
  of C4Func:
    if x.items.len() == 0:
      return "(callback w no arg spec)"
    result = "("
    var itemInfo: seq[string]
    for item in x.items:
      itemInfo.add(item.toStr(tvars, nvars))
    if x.va:
      itemInfo[^2] = "*" & itemInfo[^2]
    result &= itemInfo[0 ..< ^1].join(", ") & ") -> "
    result &= itemInfo[^1]
  of C4Struct:
    result = "struct["

    if x.name != "":
      result &= x.name
    else:
      if tvars.lookup(x.typeId).isNone():
        nvars += 1
        tvars[x.typeId] = nvars
      result &= "`" & numToTVarName(tvars[x.typeId])
  of C4TypeSpec:
    if x.items.len() == 0:
      result = "typespec"
    else:
      result = "typespec["
      result &= x.items[0].toStr(tvars, nvars) & "]"
  of C4OneOf:
    result = "oneof["
    var itemInfo: seq[string]
    for item in x.items:
      iteminfo.add(item.toStr(tvars, nvars))
    result &= itemInfo.join(", ") & "]"

proc toStr(x: TypeId, tvars: var Dict[TypeId, int], nvars: var int): string =
  return typeStore[x].toStr(tvars, nvars)

proc toStr*(x: TypeRef): string {.exportc, cdecl.} =
  var
    tbl: Dict[TypeId, int]
    n:   int

  tbl.initDict()
  return x.toStr(tbl, n)

proc toString*(x: TypeId): string {.exportc, cdecl.}=
  typeStore[x].toStr()
