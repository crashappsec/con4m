import basetypes/[common, t_bool, t_ints, t_chars, t_float, t_strs, t_url],
       basetypes/[t_duration, t_ipaddr, t_size, t_datetime, checker]
import err, mixed

export common, t_bool, t_ints, t_chars, t_float, t_strs, t_url, t_duration,
       t_ipaddr, t_size, t_datetime, checker, err, mixed

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

template numTypeErr(s: Con4mSeverity, errid: string, xtra: seq[string],
                    p = ErrIrGen) =
  err = Con4mError(phase: p, severity: s, code: errid, extra: xtra)

proc resultingNumType*(t1, t2: TypeId, err: var Con4mError): TypeId =
  ## If this produces an error object, the location information
  ## needs to be filled in by the caller.
  ##
  ## Specifically, 'module', 'line', 'offset' and 'cursor'.

  var
    t1  = t1.followForwards()
    t2  = t2.followForwards()
    to1 = t1.getTypeInfoObject()
    to2 = t2.getTypeInfoObject()


  if to1.intBits != 0 and to2.intBits != 0:
    if to1.intBits > to2.intBits:
      if not to1.signed and to2.signed:
        result = to1.signVariant
        numTypeErr(LlWarn, "SignToUnsign", @[$(t1)])
      elif to1.signed and not to2.signed:
        result = t1
        numTypeErr(LlInfo, "SignChange", @[$(t1), $(t2)])
      else:
        result = t1
    elif to2.intBits > to1.intBits:
      return resultingNumType(t2, t1, err)
    else:
      if to1.signed == to2.signed:
        result = t1
      else:
        if to1.signed:
          numTypeErr(LlWarn,  "SignToUnsign", @[$(t2)])
          result = t1
        else:
          numTypeErr(LlWarn,  "SignToUnsign", @[$(t1)])
          result = t2
  elif t1 == TFloat or t2 == TFloat:
    result = TFloat
  else:
    result = TBottom
    unreachable # Shouldn't get here
