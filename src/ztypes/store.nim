import "."/[base, ordinals, chars, floats, strings, list, dict, tup, typespec,
            ipaddr, duration, size, datetime, url, function]
import ".."/err

export base, ordinals, chars, floats, strings, list, dict, tup, typespec,
       ipaddr, duration, size, datetime, url, err

# We do this last, after all the imports above, to make sure 'string'
# is the last resort for inferring := literals.
#
# We will probably replace this fallback with 'Path' soon.

registerSyntax(TString, STOther, @[])

proc isConcrete*(id: TypeId): bool =
  return id.followForwards() < TypeId(0x8000000000000000'u64)

proc isConcrete*(ids: seq[TypeId]): bool =
  var combined: TypeId
  for item in ids:
    combined = combined or item.followForwards()
  return combined.isConcrete()

proc isTVar*(id: TypeId): bool =
  return typeStore[id.followForwards()].kind == C4TVar

proc isConcrete*(tRef: TypeRef): bool =
  return tRef.typeId.followForwards().isConcrete()

template isGeneric*(x: untyped): bool =
  not x.isConcrete()

proc isC4StrType*(id: TypeId): bool {.exportc, cdecl.} =
  if id.followForwards() in [TString, TUtf32, TBuffer]:
    return true

proc typeHash*(x: TypeRef, ctx: var Sha256Ctx, tvars: var Dict[TypeId, int],
               nvars: var int) =
  var x = x
  if x.isGeneric():
    # Make sure that we follow any forwarding that has happened.
    x = x.followForwards()

  # Since newlines aren't allowed anywhere in a type name, this makes
  # it easy to ensure distinct boundaries on types to avoid accidental
  # collisions.
  ctx.update("\n")
  if x.typeId != 0 and x.typeId < TypeId(numDataTypes()):
    ctx.update(typeNameFromId(x.typeId))
    return

  ctx.update($x.kind)

  case x.kind
  of C4Struct:
    # If the struct is still generic (no name is bound), then we
    # treat it like a type variable in the hash.
    #
    # Otherwise, we hash the properties.
    ctx.update(x.name)
    if x.name == "":
      let numOpt = tvars.lookup(x.typeId)
      if numOpt.isNone():
        nvars += 1
        tvars[x.typeId] = nvars
    else:
      for (k, tid) in x.props.items():
        ctx.update(k)
        typeStore[tid].typeHash(ctx, tvars, nvars)
  of C4Func:
    if x.va:
      ctx.update("v")
    if x.items.len() == 0:
      ctx.update("n")
  of C4TVar:
    var numToHash: int

    let numOpt = tvars.lookup(x.typeId)
    if numOpt.isNone():
      nvars += 1
      tvars[x.typeId] = nvars
      numToHash = nvars
    else:
      numToHash = numOpt.get()

    ctx.update($(numToHash))
  else:
    discard

  for item in x.items:
    typeStore[item].typeHash(ctx, tvars, nvars)

proc typeHash*(x: TypeRef): TypeId =
  ## We use this to generate persistant unique IDs for types.  Type
  ## variables make a type NON-hashable.
  ##
  ## Note that typespec is normally a concrete type, but you can
  ## parameterize it by the types that are acceptable values.  So when
  ## there is no argument, the hash returns a concrete slot, but when
  ## there is an argument, it must have an unknown element; Saying
  ## typespec[int] is effectively the same as saying, "this is a
  ## constant type value that must always be int".
  var
    tvars:       Dict[TypeId, int]
    numTypeVars: int
    ctx:         Sha256Ctx

  tvars.initDict()

  if x.kind == C4TVar:
    return x.typeId

  ctx.initSha256()
  x.typeHash(ctx, tvars, numTypeVars)
  let digest = ctx.final()

  result = (cast[ptr TypeId](addr digest[0]))[]

  if numTypeVars == 0:
    result = result and 0x7fffffffffffffff'u64
  else:
    result = result or  0x8000000000000000'u64

proc initTypeStore*() =
  let names = getAllBuiltinTypeNames()

  typeStore.initDict()
  primitiveTypes.initDict()

  for i, item in names:
    let obj = TypeRef(kind: C4Primitive, typeId: cast[TypeId](i),
                      isLocked: true)
    typeStore[TypeId(i)] = obj
    primitiveTypes[item] = obj

proc copyType*(t: TypeRef): TypeRef
proc copyType*(t: TypeId): TypeRef =
  typeStore[t].copyType()

{.emit: """
#include <stdatomic.h>
#include <stdint.h>

_Atomic(uint64_t) next_vid = ATOMIC_VAR_INIT(0x8000000000000000);
uint64_t
next_var_id() {
  return atomic_fetch_add(&next_vid, 1);
}

""" .}

proc nextVarId(): TypeId {. importc: "next_var_id", cdecl, nodecl .}

proc newTypeVar*(): TypeRef =
  let newId = nextVarId()
  result = TypeRef(kind: C4TVar, typeId: newId, tVarId: newId)
  typeStore[newId] = result

template tVar*(): TypeId =
  newTypeVar().typeId

proc newContainerType*(kind: C4TypeKind, items: seq[TypeId]): TypeRef =
  result = TypeRef(kind: kind, items: items)
  let hash = result.typeHash()
  if (hash and TypeId(0x8000000000000000'u64)) != 0:
    result.typeId = nextVarId()
  else:
    result.typeId = result.typeHash()

  typeStore[result.typeId] = result

template newList*(item: TypeId): TypeRef =
  newContainerType(C4List, @[item])

template newList*(item: TypeRef): TypeRef =
  newContainerType(C4List, @[item.followForwards().typeId])

proc tList*(item: TypeId): TypeId {.exportc, cdecl.} =
  newContainerType(C4List, @[item]).typeId

template newDict*(ktype: TypeId, vtype: TypeId): TypeRef =
  newContainerType(C4Dict, @[ktype, vtype])

template newDict*(ktype: TypeRef, vtype: TypeRef): TypeRef =
  newContainerType(C4Dict, @[ktype.followForwards().typeId,
                             vtype.followForwards().typeId])

template tDict*(ktype: TypeId, vtype: TypeId): TypeId =
  newContainerType(C4Dict, @[ktype, vtype]).typeId

template newTuple*(l: seq[TypeId]) : TypeRef =
  newContainerType(C4Tuple, l)

template newTuple*(l: seq[TypeRef]): TypeRef =
  var idSeq: seq[TypeId]
  for item in l:
    idSeq.add(item.followForwards().typeId)
  newContainerType(C4Tuple, idSeq)

proc tTuple*(l: seq[TypeId]) : TypeId {.exportc, cdecl.} =
  newContainerType(C4Tuple, l).typeId

template newRef*(item: TypeId): TypeRef =
  newContainerType(C4Ref, @[item])

template newRef*(item: TypeRef): TypeRef =
  newContainerType(C4Ref, @[item.followForwards().typeId])

template tRef*(item: TypeId): TypeId =
  newContainerType(C4Ref, @[item]).typeId

template newMaybe*(item: TypeId): TypeRef =
  newContainerType(C4Maybe, @[item])

template newMaybe*(item: TypeRef): TypeRef =
  newContainerType(C4Maybe, @[item.followForwards().typeId])

template tMaybe*(item: TypeId): TypeId =
  newContainerType(C4Maybe, @[item]).typeId

proc newTypeSpec*(constraint = TypeId(TBottom)): TypeRef =
  if constraint != TypeId(TBottom):
    if constraint.isConcrete():
      return typeStore[TypeId(TBottom)]
    result = newContainerType(C4TypeSpec, @[constraint])
  else:
    result = newContainerType(C4TypeSpec, @[])

template tTypeSpec*(constraint = TypeId(TBottom)): TypeId =
  if constraint != TypeId(TBottom):
    newContainerType(C4TypeSpec, @[constraint]).typeId
  else:
    newContainerType(C4TypeSpec, @[]).typeId

proc newStructType*(props: var Dict, name = ""): TypeRef =
  # Type variables are hashed individually, since their only
  # relationships occur when linking, which either merge two equal
  # type variables into one, or bind the variable to some other, more
  # specific type.
  #
  # When we don't know a struct's name, but are infering based on
  # fields we see, info on fields we *have* seen is an equivolence, so
  # we can use the type hash for the ID, instead of a unique ID.
  result = TypeRef(kind: C4Struct, name: name)

  for (k, v) in props.items():
    result.props[k] = v.copyType().typeId

  if name != "":
    result.typeId = result.typeHash()
  else:
    result.typeId = nextVarId()

  typeStore[result.typeId] = result

proc newStructType*(): TypeRef =
  result = TypeRef(kind: C4Struct, typeId: nextVarId())

proc tStruct*(props: var Dict, name = ""): TypeId =
  return newStructType(props, name).typeId

proc tStruct*(): TypeId =
  return newStructType().typeId

proc newFuncType*(items: seq[TypeId], va = false): TypeRef =
  result = TypeRef(kind: C4Func, items: items, va: va)
  if items.isGeneric():
    result.typeId = nextVarId()
  else:
    result.typeId = result.typeHash()
  typeStore[result.typeId] = result

proc tFunc*(items: seq[TypeId], va = false): TypeId =
  return newFuncType(items, va).typeId

proc copyType*(t: TypeRef): TypeRef =
  var id = t.typeid.followForwards()

  if id.isConcrete():
    return t

  case t.kind
  of C4TVar:
    return newTypeVar()
  of C4List, C4Dict, C4Tuple, C4TypeSpec, C4Ref, C4Maybe:
    var items: seq[TypeId]
    for item in t.items:
      items.add(item.copyType().typeId)
    return newContainerType(t.kind, items)
  of C4Func:
    var items: seq[TypeId]
    for item in t.items:
      items.add(item.copyType().typeId)
    return newFuncType(items, t.va)
  of C4Struct:
    result = newStructType()
    for (k, v) in t.props.items():
      result.props[k] = v.copyType().typeId
  of C4OneOf:
    var items: seq[TypeId]
    for item in t.items:
      items.add(item.copyType().typeId)
    result = TypeRef(kind: C4OneOf, items: items)
    result.typeId = nextVarId()
  else:
    discard
  typeStore[result.typeId] = result

template tCopy*(t: TypeId): TypeId =
  idToTypeRef(t).copyType().typeId

proc baseunify(id1, id2: TypeId): TypeId {.importc, cdecl.}
proc baseunify(id1, id2: TypeRef): TypeId {.importc, cdecl.}

const oErr = "'oneof' types must have multiple options."
proc newOneOf*(items: seq[TypeId]): TypeRef =
  result = TypeRef(kind: C4OneOf, items: items)

  if items.len() < 1:
    raise newException(ValueError, oErr)
  for i in 0 ..< items.len() - 1:
    let toCompare = items[i].copyType()
    for item in items[i + 1 .. ^1]:
      if toCompare.baseunify(item.copyType()) != TypeId(TBottom):
        raise newException(ValueError, oErr)

  result.typeId = nextVarId()
  typeStore[result.typeId] = result

proc tOneOf*(items: seq[TypeId]): TypeId =
  return newOneOf(items).typeId

let
  allNumericTypes* = [TInt8, TByte, TInt32, TChar, TUint32, TInt, TUint, TFloat]

template isNumericBuiltin*(t: TypeId): bool =
  t.followForwards() in allNumericTypes

template isMaybeType*(t: TypeId): bool =
  let to = t.idToTypeRef()

  to.kind == C4Maybe

template isTypeSpec*(t: TypeId): bool =
  let to = t.idToTypeRef()

  to.kind == C4TypeSpec

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
  of C4None:
    assert false

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
  of C4None:
    assert false

proc toStr(x: TypeId, tvars: var Dict[TypeId, int], nvars: var int): string =
  return typeStore[x].toStr(tvars, nvars)

proc toStr*(x: TypeRef): string {.exportc, cdecl.} =
  var
    tbl: Dict[TypeId, int]
    n:   int

  tbl.initDict()
  return x.toStr(tbl, n)

proc toString*(x: TypeId): string {.exportc, cdecl.} =
  typeStore[x].toStr()

proc resultingNumType*(ctx: Module, t1, t2: TypeId): TypeId =
  var
    t1  = t1.followForwards()
    t2  = t2.followForwards()
    to1 = t1.getDataType()
    to2 = t2.getDataType()

  if to1.intW != 0 and to2.intW != 0:
    if to1.intW > to2.intW:
      if not to1.signed and to2.signed:
        result = uint64(to1.signedVariant.dtid)
        if ctx != nil:
          ctx.irWarn("SignToUnsign", @[t1.toString()])
      elif to1.signed and not to2.signed:
        result = t1
        if ctx != nil:
          ctx.irInfo("SignChange", @[t1.toString(), t2.toString()])
      else:
        result = t1
    elif to2.intW > to1.intW:
      return ctx.resultingNumType(t2, t1)
    else:
      if to1.signed == to2.signed:
        result = t1
      else:
        if to1.signed:
          if ctx != nil:
            ctx.irWarn("SignToUnsign", @[t2.toString()])
          result = t1
        else:
          if ctx != nil:
            ctx.irWarn("SignToUnsign", @[t1.toString()])
          result = t2
  elif t1 == TFloat or t2 == TFloat:
    result = TFloat
  else:
    result = TBottom
    unreachable # Shouldn't get here


initTypeStore()
