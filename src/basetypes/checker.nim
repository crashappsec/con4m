import options, ../common

var
  typeStore*:      Dict[TypeId, TypeRef]
  primitiveTypes*: Dict[string, TypeRef]

proc newTypeVar*(): TypeRef

proc followForwards*(id: TypeId): TypeId =
  # When we resolve generic types, we change the type ID field to
  # forward it to it's new (refined) type. Therefore, every check
  # involving an ID for a generic type should check to see if the
  # type has been updated.
  #
  # The stack detects recursive types; we probably should disallow
  # those, but right now, just shrugging it off.

  var
    stack = @[id]
    refs: seq[TypeRef]

  while true:
    let trefOpt = typeStore.lookup(stack[^1])
    if trefOpt.isNone():
      return id
    let tref = trefOpt.get()
    if tref.typeId in stack:
      for i, item in refs:
        item.typeId = tref.typeId
        typeStore[stack[i]] = tref

      return tref.typeId

    stack.add(tref.typeId)
    refs.add(tref)

proc followForwards*(x: TypeRef): TypeRef =
  let optObj = typestore.lookup(x.typeId)
  if optObj.isSome():
    return typestore[x.typeId.followForwards()]
  else:
    return x

template getTid*(x: TypeId): TypeId =
  x.followForwards()

proc isConcrete*(id: TypeId): bool =
  return id.followForwards() < TypeId(0x8000000000000000'u64)

proc isConcrete*(ids: seq[TypeId]): bool =
  var combined: TypeId
  for item in ids:
    combined = combined or item.followForwards()
  return combined.isConcrete()

proc isConcrete*(tRef: TypeRef): bool =
  return tRef.typeId.followForwards().isConcrete()

template isGeneric*(x: untyped): bool =
  not x.isConcrete()

proc isBasicType*(id: TypeId): bool =
  let n = cast[uint](id.followForwards())
  let l = uint(basicTypes.len())
  if n >= l:
    return false
  else:
    return true

proc intBits*(id: TypeId): int =
  var id = id.followForwards()
  if not id.isBasicType():
    return 0
  return basicTypes[cast[int](id)].intBits

proc isSigned*(id: TypeId): bool =
  var id = id.followForwards()
  return basicTypes[cast[int](id)].signed

proc castToBool*(v: Mixed, t: TypeId): Option[bool] =
  if not t.isBasicType():
    return none(bool)

  var ti = basicTypes[cast[int](t.followForwards())]

  if ti.castToBool == nil:
    return none(bool)

  return some(ti.castToBool(v))

proc canCastToBool*(t: TypeId): bool =
  if not t.isBasicType():
    return false

  var ti = basicTypes[cast[int](t.followForwards())]

  return not (ti.castToBool == nil)

proc castToU128*(v: Mixed, t: TypeId): Option[uint128] =
  if not t.isBasicType():
    return none(uint128)

  var ti = basicTypes[cast[int](t.followForwards())]

  if ti.castToU128 == nil:
    return none(uint128)
  else:
    return some(ti.castToU128(v))

proc castToI128*(v: Mixed, t: TypeId): Option[int128] =
  if not t.isBasicType():
    return none(int128)

  var ti = basicTypes[cast[int](t.followForwards())]

  if ti.castToU128 == nil:
    return none(int128)
  else:
    return some(ti.castToI128(v))

proc rawBuiltinRepr*(tid: TypeId, m: Mixed): string {.cdecl.} =
  var tid = tid.followForwards()

  result = basicTypes[tid].repr(tid, m)

proc builtinRepr*(v: Cbox): string {.cdecl.} =
  let tid = v.t.followForwards()
  result  = basicTypes[tid].repr(tid, v.v)

proc baseValueEq*(a, b: CBox): bool {.cdecl.} =
  let
    t1 = a.t.followForwards()

  result = basicTypes[t1].eqFn(a, b)

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
  if x.typeId != 0 and x.typeId < TypeId(numBuiltinTypes()):
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
    let obj = TypeRef(kind: C4Primitive, typeId: TypeId(i), isLocked: true)
    typeStore[TypeId(i)]                      = obj
    primitiveTypes[typeNameFromId(TypeId(i))] = obj

template bottom*():    TypeRef = typeStore[TBottom]
template tVoid*():     TypeRef = typeStore[TVoid]
template tBool*():     TypeRef = typeStore[TBool]
template tInt*():      TypeRef = typeStore[TInt]
template tUint*():     TypeRef = typeStore[TUint]
template tInt32*():    TypeRef = typeStore[TInt32]
template tUint32*():   TypeRef = typeStore[TUnt32]
template tChar*():     TypeRef = typeStore[TChar]
template tByte*():     TypeRef = typeStore[TByte]
template tFloat*():    TypeRef = typeStore[TFloat]
template tString*():   TypeRef = typeStore[TString]
template tBuffer*():   TypeRef = typeStore[TBuffer]
template tRich*():     TypeRef = typeStore[TRich]
template tUtf32*():    TypeRef = typeStore[TUtf32]
template tDuration*(): TypeRef = typeStore[TDuration]
template tIpv4*():     TypeRef = typeStore[TIpv4]
template tIpv6*():     TypeRef = typeStore[TIpv6]
template tCidr*():     TypeRef = typeStore[TCidr]
template tSize*():     TypeRef = typeStore[TSize]
template tDate*():     TypeRef = typeStore[TDate]
template tTime*():     TypeRef = typeStore[TTime]
template tDateTime*(): TypeRef = typeStore[TDateTime]
template tPath*():     TypeRef = typeStore[TPath]

proc copyType*(t: TypeRef): TypeRef
proc copyType*(t: TypeId): TypeRef =
  typeStore[t].copyType()

template idToTypeRef*(t: TypeId): TypeRef =
  typeStore[t].followForwards()


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

template tUnknown*(): TypeId =
  newTypeVar().typeId

template newList*(item: TypeId): TypeRef =
  newContainerType(C4List, @[item])

template newList*(item: TypeRef): TypeRef =
  newContainerType(C4List, @[item.followForwards().typeId])

template tList*(item: TypeId): TypeId =
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

template tTuple*(l: seq[TypeId]) : TypeId =
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
      return typeStore[TBottom]
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
  var id = t.typeid

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

proc baseunify(id1, id2: TypeId): TypeId

const oErr = "'oneof' types must have multiple options."
proc newOneOf*(items: seq[TypeId]): TypeRef =
  result = TypeRef(kind: C4OneOf, items: items)

  if items.len() < 1:
    raise newException(ValueError, oErr)
  for i in 0 ..< items.len() - 1:
    let toCompare = items[i].copyType().typeid
    for item in items[i + 1 .. ^1]:
      if toCompare.baseunify(item.copyType().typeid) != TBottom:
        raise newException(ValueError, oErr)

  result.typeId = nextVarId()
  typeStore[result.typeId] = result

proc tOneOf*(items: seq[TypeId]): TypeId =
  return newOneOf(items).typeId

proc baseunify(ref1, ref2: TypeRef): TypeId =
  ## Performs unification on two objects, returning the type
  ## both operands have after they've been unified.
  ##
  ## If the operaands are not marked as 'locked', they will
  ## be updated to the new type.
  ##
  ## This operation can make type links that should NOT be committed
  ## if unification fails somewhere else in the type.
  ## For instance, if you try to unify:
  ##
  ## dict[`x, int] and dict[string, string]
  ##
  ## This will bind `x to string before it fails when comparing
  ## int vs string.
  ##
  ## Therefore, the higher-level unify() function essentially will,
  ## when type variables exist, first call this on copies, and if that
  ## unify succeeds, call it again on the actuals.

  var
    type1 = ref1.followForwards()
    type2 = ref2.followForwards()
    id1   = type1.typeId
    id2   = type2.typeId

  if type1.typeId != ref1.typeId:
    ref1.typeId = type1.typeId

  if type2.typeId != ref2.typeId:
    ref2.typeId = type2.typeId

  if id1 == id2:
    return id1

  if id1 == TBottom or id2 == TBottom:
    return TBottom

  if id1.isConcrete() and id2.isConcrete():
    # Both are concrete types, but they are not the same concrete
    # type.  Doesn't mean there can't be a cast, but unification is
    # about type equivolence, not type coercion :)
    return TBottom

  # Before we do any deeper work, we can check the kind to see if
  # we can quickly rule out compatability.
  if type1.kind notin [type2.kind, C4TVar] and type2.kind != C4TVar:
    if type1.kind != C4OneOf:
      return TBottom

  # Also, if only one of these things is a type variable, it makes
  # our life easier if it's always on the same side.
  if (type2.kind == C4TVar and type1.kind != C4TVar) or
    (type2.kind == C4OneOf and type1.kind != C4OneOf):
    let tmp = type1
    type1   = type2
    type2   = tmp
    let tid = id1
    id1     = id2
    id2     = tid

  case type1.kind
  of C4Primitive:
    # Either we early exited, or the rhs is a primitive, the lhs will
    # be a type variable at this point.
    unreachable
  of C4TVar:
    type1.typeid = id2
    result       = id2
  of C4List, C4Dict, C4Tuple, C4TypeSpec, C4Ref, C4Maybe:
    let l = type1.items.len()
    if l != type2.items.len():
      return TBottom

    var newItems: seq[TypeId]
    for i in 0 ..< l:
      let one = type1.items[i].baseunify(type2.items[i])
      if one == TBottom:
        return TBottom
      newItems.add(one)

    type1.items = newItems
    type2.items = newItems

    type1.typeid = type1.typeHash()
    id1 = type1.typeid

    result = id1
  of C4Func:
    if type2.items.len() == 0:
      result = id1
    elif type1.items.len() == 0:
      result = id2
    # Actuals will never be varargs, so if we have two vararg
    # functions, it's only because we're trying to unify two formals.
    elif (not type1.va and not type2.va) or (type1.va and type2.va):
      let l = type1.items.len()
      if l != type2.items.len():
        return TBottom

      for i in 0 ..< l:
        if type1.items[i].baseunify(type2.items[i]) == TBottom:
          return TBottom

      result = id1
    else:
      if type2.va:
        let tmp = type1
        type1   = type2
        type2   = tmp

      # -1 because varargs params are OPTIONAL.
      if type2.items.len() < type1.items.len() - 1:
        return TBottom

      # The last item is always the return type, so we have
      # to unify the last items, plus any items before varargs.
      # Then, if there are any items in type2, they each need to unify
      # with t1's varargs parameter.

      for i in 0 ..< type1.items.len() - 2:
        if type1.items[i].baseunify(type2.items[i]) == TBottom:
          return TBottom

      if type1.items[^1].baseunify(type2.items[^1]) == TBottom:
        return TBottom

      for i in type1.items.len() - 2 ..< type2.items.len() - 1:
        if i < 0:
          continue
        if type1.items[^2].baseunify(type2.items[i]) == TBottom:
          return TBottom

      result = id1
  of C4Struct:
    # If there's a name attached, then the fields are fully
    # fleshed out.
    #
    # Names have global scope, so if they're the same, they're the
    # same (two different object types of the same name can't be
    # imported at the same time.
    var checkFields = false

    if type1.name == type2.name:
      if type1.name == "":
        checkFields = true
    elif type1.name == "" or type2.name == "":
      checkFields = true
    else:
      return TBottom

    if checkFields:
      for (k, v) in type1.props.items():
        let opt2 = type2.props.lookup(k)
        if opt2.isSome():
          if type1.props[k].baseunify(opt2.get()) == TBottom:
            return TBottom
        elif type2.name != "":
          return TBottom
        else:
          type2.props[k] = v
      # If there are fields in type2 not in type1's props, they'll
      # all unify directly now, UNLESS type1 is named.

      if type1.name != "":
        for (k, _) in type2.props.items():
          if type1.props.lookup(k).isNone():
            return TBottom
      else:
        for (k, v) in type2.props.items():
          if type1.props.lookup(k).isNone():
            type1.props[k] = v

    if type2.name == "":
      result = id1
      type2.typeId = type1.typeId
    elif type1.name == "":
      result = id2
      type1.typeId = type2.typeId
    else:
      result = id1
  of C4OneOf:
    var remainingItems: seq[TypeId]
    for i, item in type1.items:
      let opt = item.baseunify(type2.copyType().typeId)
      if opt != TBottom:
        remainingItems.add(opt)
    case len(remainingItems)
    of 0:
      return TBottom
    of 1:
      result       = remainingItems[0]
      type1.typeId = result
      type2.typeId = result
    else:
      type1.items  = remainingItems
      type2.typeId = type1.typeId

  var resObj: TypeRef
  if result == id1:
    resObj = type1
  else:
    resObj = type2

  typestore[result] = resObj

  if result != type1.typeid or typestore.lookup(type1.typeid).isNone():
    typestore[type1.typeId] = resObj
  if result != type2.typeId or typestore.lookup(type1.typeId).isNone():
    typestore[type2.typeId] = type1

proc baseunify(id1, id2: TypeId): TypeId =
  var
    id1 = typeStore[id1].followForwards()
    id2 = typeStore[id2].followForwards()

  return baseunify(id1, id2)

proc toStr*(x: TypeRef): string {.importc, cdecl.}

proc unify*(origtype1, origtype2: TypeRef): TypeId =
  var
    type1 = origtype1
    type2 = origtype2
    copy1: bool
    copy2: bool
    lock1: bool
    lock2: bool

  if type1.isGeneric():
    if type1.isLocked:
      lock1 = true
    type1 = type1.copyType()
    copy1 = true

  if type2.isGeneric():
    if type2.isLocked:
      lock2 = true
    type2 = type2.copyType()
    copy2 = true

  let id = type1.baseunify(type2)

  if id == TBottom:
    return TBottom

  if not copy1 and not copy2:
    return id

  if (lock1 and copy1) or not copy1:
    if (lock2 and copy2) or not copy2:
      return id

  if copy1 and not lock1:
    type1 = origtype1
  if copy2 and not lock2:
    type2 = origtype2

  result = type1.baseunify(type2)


template unify*(t1, t2: TypeId): TypeId =
  let
    opt1 = typestore.lookup(t1)
    opt2 = typestore.lookup(t2)

  if opt1.isNone() or opt2.isNone():
    TBottom
  else:
    unify(opt1.get(), opt2.get())
