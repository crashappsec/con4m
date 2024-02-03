import options, store
export store

proc baseunify*(id1, id2: TypeId): TypeId

proc baseunify*(ref1, ref2: TypeRef): TypeId {.cdecl, exportc.} =
  ## This generally shouldn't be called directly.
  ##
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

  if type1 == nil or type2 == nil:
    return TBottom

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
    if type1.kind notin [C4OneOf, C4Maybe]:
      return TBottom

  # Also, if only one of these things is a type variable, etc, it
  # makes our life easier if it's always on the same side.
  if (type2.kind == C4TVar and type1.kind != C4TVar) or
    (type2.kind == C4OneOf and type1.kind != C4OneOf) or
    (type2.kind == C4Maybe and type1.kind != C4Maybe):
    let tmp = type1
    type1   = type2
    type2   = tmp
    let tid = id1
    id1     = id2
    id2     = tid

  case type1.kind
  of C4Primitive, C4None:
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
    id1          = type1.typeid

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

proc baseunify*(id1, id2: TypeId): TypeId =
  var
    id1 = typeStore[id1].followForwards()
    id2 = typeStore[id2].followForwards()

  return baseunify(id1, id2)

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

proc typeError*(ctx: Module, t1, t2: TypeId, where: ParseNode = nil,
                err = "TypeMismatch") =
  var where = if where == nil: ctx.pt else: where
  ctx.irError(err, @[t1.toString(), t2.toString()], where)

proc typeCheck*(ctx: Module, t1, t2: TypeId, where: ParseNode = nil,
                err = "TypeMismatch"): TypeId {.discardable.} =

  result = t1.getTid().unify(t2.getTid())

  if result == TBottom and t1.getTid() != TBottom and
     t2.getTid() != TBottom:
    ctx.typeError(t1, t2, where, err)

proc typeCheck*(ctx: Module, sym: SymbolInfo, t: TypeId,
              where: ParseNode = nil, err = "TypeMismatch"):
                TypeId {.discardable.} =
  return ctx.typeCheck(sym.tid, t, where, err)
