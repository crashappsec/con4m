import basetypes/[t_bool, t_ints, t_chars, t_float, t_strs, t_url],
       basetypes/[t_duration, t_ipaddr, t_size, t_datetime, checker, repr]
import common, err, mixed

export t_bool, t_ints, t_chars, t_float, t_strs, t_url, t_duration,
       t_ipaddr, t_size, t_datetime, checker, err, mixed, repr

initTypeStore()

let
  allNumericTypes* = [TInt8, TByte, TInt32, TChar, TUint32, TInt, TUint,
                      TInt128, TUint128, TFloat]

template setIntVariants(t1, t2: TypeId) =
  basicTypes[t1].signVariant = t2
  basicTypes[t2].signVariant = t1

setIntVariants(TInt8, TByte)
setIntVariants(TInt32, TUint32)
setIntVariants(Tint, TUint)
setIntVariants(TInt128, TUint128)

template isNumericBuiltin*(t: TypeId): bool =
  t.followForwards() in allNumericTypes

template isIntType*(t: TypeId): bool =
  let tprime = t.followForwards()
  tprime != TFloat and tprime in allNumericTypes

proc resultingNumType*(ctx: Module, t1, t2: TypeId): TypeId =
  var
    t1  = t1.followForwards()
    t2  = t2.followForwards()
    to1 = t1.getTypeInfoObject()
    to2 = t2.getTypeInfoObject()

  if to1.intBits != 0 and to2.intBits != 0:
    if to1.intBits > to2.intBits:
      if not to1.signed and to2.signed:
        result = to1.signVariant
        ctx.irWarn("SignToUnsign", @[t1.toString()])
      elif to1.signed and not to2.signed:
        result = t1
        ctx.irInfo("SignChange", @[t1.toString(), t2.toString()])
      else:
        result = t1
    elif to2.intBits > to1.intBits:
      return ctx.resultingNumType(t2, t1)
    else:
      if to1.signed == to2.signed:
        result = t1
      else:
        if to1.signed:
          ctx.irWarn("SignToUnsign", @[t2.toString()])
          result = t1
        else:
          ctx.irWarn("SignToUnsign", @[t1.toString()])
          result = t2
  elif t1 == TFloat or t2 == TFloat:
    result = TFloat
  else:
    result = TBottom
    unreachable # Shouldn't get here

proc typeError*(ctx: Module, t1, t2: TypeId, where: Con4mNode = nil,
                err = "TypeMismatch") =
  var where = if where == nil: ctx.pt else: where
  ctx.irError(err, @[t1.toString(), t2.toString()], where)

proc typeCheck*(ctx: Module, t1, t2: TypeId, where: Con4mNode = nil,
                err = "TypeMismatch"): TypeId {.discardable.} =

  result = t1.unify(t2)

  if result == TBottom:
    ctx.typeError(t1, t2, where, err)

proc typeCheck*(ctx: Module, sym: SymbolInfo, t: TypeId,
              where: Con4mNode = nil, err = "TypeMismatch"):
                TypeId {.discardable.} =
  return ctx.typeCheck(sym.tid, t, where, err)

proc isBuiltinType*(tid: TypeId): bool =
  return cast[int](tid.followForwards()) < basicTypes.len()
