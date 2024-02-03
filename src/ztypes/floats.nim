import std/math
import "."/base

var
  f32Ops = newVTable()
  f64Ops = newVTable()

proc cast_f_to_i(pre: pointer, t1, t2: TypeId, err: var string):
                pointer {.cdecl, exportc.} =
  let
    f = cast[float64](pre)
    i = int64(f)

  result = cast[pointer](i)

proc cast_f_to_u(pre: pointer, t1, t2: TypeId, err: var string):
                pointer {.cdecl, exportc.} =
  let
    f = cast[float64](pre)
    i = uint64(f)

  result = cast[pointer](i)

proc get_cast_func_float(dt, ot: DataType, t1, t2: TypeId,
                         err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_identity)

  if ot.isBool:
    return cast[pointer](cast_to_bool)

  if ot.intW == 0:
    return nil

  if dt.signed:
    return cast[pointer](cast_f_to_i)
  else:
    return cast[pointer](cast_f_to_u)

proc new_float_lit(lit: pointer, st: SyntaxType, lmod: string,
                   l: var int, err: var string): pointer {.cdecl.} =
  l = 8

  var
    s      = cast[string](lit)
    dotLoc = s.find('.')
    eLoc   = s.find('e')
    value:        float64
    intPartS:     string
    intPartI:     uint128
    floatPartS:   string
    floatPartI:   uint128
    expPartS:     string
    expPartI:     uint128
    eSignIsMinus: bool
    sign:         bool
    intSz:        int

  if eLoc == -1:
    eLoc = s.find('E')

  if dotLoc != -1:
    intPartS = s[0 ..< dotLoc]
    if eLoc != -1:
      floatPartS = s[dotLoc + 1 ..< eLoc]
    else:
      floatPartS = s[dotLoc + 1 .. ^1]
  else:
    intPartS = s[0 ..< eLoc]

  if eLoc != -1:
    eLoc = eLoc + 1
    case s[eLoc]
    of '+':
      eLoc = eLoc + 1
    of '-':
      eLoc = eLoc + 1
      eSignIsMinus = true
    else:
      discard
    expPartS = s[eLoc .. ^1]

  intSz = parseInt128(intPartS, intParti, sign)
  if intSz > 8:
    err = "FloatTooLarge"
    return

  intSz = parseInt128(expPartS, expPartI, sign)
  if intSz > 8:
    err = "ExpTooLarge"
    return

  if floatPartS != "":
    const
      maxAsString = $(high(int64))
      maxLen      = maxAsString.len()

    if floatPartS.len() >= maxLen:
      floatPartS = floatPartS[0 ..< maxLen]

    discard parseInt128(floatPartS, floatPartI, sign)

    value = int(u128toU64(floatPartI)) / int(10 ^ floatPartS.len())

  value = value + float64(uint64(intPartI))
  value = value * pow(10.0, float64(u128toU64(expPartI)))

  result = cast[pointer](value)

proc repr_float(p: pointer): string {.cdecl.} =
  let f = cast[float64](p)
  return $(f)

proc lt_float(a, b: pointer): bool {.cdecl.} =
  let
    l = cast[float64](a)
    r = cast[float64](b)

  return l < r

proc gt_float(a, b: pointer): bool {.cdecl.} =
  let
    l = cast[float64](a)
    r = cast[float64](b)

  return l > r

proc add_float_impl(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[float64](a)
    r = cast[float64](b)

  result = cast[pointer](l + r)

proc sub_float(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[float64](a)
    r = cast[float64](b)

  result = cast[pointer](l - r)

proc mul_float(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[float64](a)
    r = cast[float64](b)

  result = cast[pointer](l * r)

proc div_float(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[float64](a)
    r = cast[float64](b)

  result = cast[pointer](l / r)


f32Ops[FRepr]    = cast[pointer](repr_float)
f32Ops[FEq]      = cast[pointer](value_eq)
f32Ops[FGt]      = cast[pointer](gt_float)
f32Ops[FLt]      = cast[pointer](lt_float)
f32Ops[FNewLit]  = cast[pointer](new_float_lit)
f32Ops[FAdd]     = cast[pointer](add_float_impl)
f32Ops[FSub]     = cast[pointer](sub_float)
f32Ops[FMul]     = cast[pointer](mul_float)
f32Ops[FFDiv]    = cast[pointer](div_float)
f32Ops[FCastFn]  = cast[pointer](get_cast_func_float)

f64Ops[FRepr]    = cast[pointer](repr_float)
f64Ops[FEq]      = cast[pointer](value_eq)
f64Ops[FNewLit]  = cast[pointer](new_float_lit)
f64Ops[FAdd]     = cast[pointer](add_float_impl)
f64Ops[FSub]     = cast[pointer](sub_float)
f64Ops[FMul]     = cast[pointer](mul_float)
f64Ops[FFDiv]    = cast[pointer](div_float)
f64Ops[FCastFn]  = cast[pointer](get_cast_func_float)

let
  TF32*   = addDataType(name = "f32", byValue = true, concrete = true,
                        fTy = true, ops = f32Ops)
  TFloat* = addDataType(name = "f64", byValue = true, concrete = true,
                        fTy = true, ops = f64Ops)

registerSyntax(TF32,   STFloat,  @[])
registerSyntax(TF32,   STBase10, @[])
registerSyntax(TFloat, STFloat,  @["f", "f64"], primary = true)
registerSyntax(TFloat, STBase10, @["f", "f64"])
