## Includes our bool and character data types.

import base, unicode

var
  boolOps = newVTable()
  byteOps = newVTable()
  charOps = newVTable()
  i8Ops   = newVTable()
  i32Ops  = newVTable()
  u32Ops  = newVTable()
  i64Ops  = newVTable()
  u64Ops  = newVTable()

proc cast_i_to_f(pre: pointer, tfrom, tto: TypeId): pointer {.cdecl.} =
  let f = float64(cast[int64](pre))

  result = cast[pointer](f)

proc cast_u_to_f(pre: pointer, tfrom, tto: TypeId): pointer {.cdecl.} =
  let f = float64(cast[uint64](pre))

  result = cast[pointer](f)

proc get_cast_func_i8(dt: DataType, t1, t2: TypeId,
                      err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_i_to_f)
  if dt.isBool:
    return cast[pointer](cast_to_bool)
  if dt.intW == 0:
    return nil # Cannot cast
  if not dt.signed:
    err = "IToU"

  result = cast[pointer](cast_identity)

proc get_cast_func_bool(dt: DataType, t1, t2: TypeId,
                        err: var string): pointer {.cdecl.} =
  if dt.isBool:
    return cast[pointer](cast_to_bool)

proc get_cast_func_u8(dt: DataType, t1, t2: TypeId,
                      err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_u_to_f)
  if dt.isBool:
    return cast[pointer](cast_to_bool)
  if dt.intW == 0:
    return nil
  if dt.signed and dt.intW == 1:
    err = "UToSSameSz"

  # 0 for OK
  # -1 for No
  # 1 for Yes, to smaller size
  # 2 for Yes, but sign issue (i to u)
  # 3 for Yes, but smaller size and sign issue
  # 4 for Yes, but can overflow (u to i of same size)

  result = cast[pointer](cast_identity)

proc get_cast_func_i32(dt: DataType, t1, t2: TypeId,
                       err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_i_to_f)
  if dt.isBool:
    return cast[pointer](cast_to_bool)
  case dt.intW
  of 0:
    return nil
  of 1, 2:
    err = "CanTruncate"
  else:
    discard

  if not dt.signed:
      err = "SToU"

  result = cast[pointer](cast_identity)

proc get_cast_func_u32(dt: DataType, t1, t2: TypeId,
                       err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_u_to_f)
  if dt.isBool:
    return cast[pointer](cast_to_bool)
  case dt.intW
  of 0:
    return nil
  of 1, 2:
    if dt.signed:
      err = "UToSSmaller"
    else:
      err = "CanTruncate"
  of 4:
    if dt.signed:
      err = "UToSSameSz"
  else:
    discard

  result = cast[pointer](cast_identity)

proc get_cast_func_i64(dt: DataType, t1, t2: TypeId,
                       err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_i_to_f)
  if dt.isBool:
    return cast[pointer](cast_to_bool)
  case dt.intW
  of 0:
    return nil
  of 1, 2, 4:
    err = "CanTruncate"
  else:
    discard

  if not dt.signed:
    if err != "":
      err = "SToSmallerU"
    else:
      err = "SToU"

  result = cast[pointer](cast_identity)

proc get_cast_func_u64(dt: DataType, t1, t2: TypeId,
                       err: var string): pointer {.cdecl.} =
  if dt.fTy:
    return cast[pointer](cast_u_to_f)
  if dt.isBool:
    return cast[pointer](cast_to_bool)
  case dt.intW
  of 0:
    return nil
  of 1, 2, 4:
    if dt.signed:
      err = "UToSSmaller"
    else:
      err = "CanTruncate"
  of 8:
    if dt.signed:
      err = "UToSSameSz"
  else:
    discard

  result = cast[pointer](cast_identity)

proc add_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l + r))

proc sub_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l - r))

proc mul_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l * r))

proc idiv_allint(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](int64(l div r))

proc fdiv_allint(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](l / r)

proc mod_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l mod r))

proc shl_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l shl r))

proc shr_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l shr r))

proc and_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l and r))

proc or_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l or r))

proc xor_i8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int8](cast[int64](a))
    r = cast[int8](cast[int64](b))

  result = cast[pointer](int64(l xor r))

proc add_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l + r))

proc sub_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l - r))

proc mul_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l * r))

proc mod_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l mod r))

proc shl_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l shl r))

proc shr_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l shr r))

proc and_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l and r))

proc or_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l or r))

proc xor_u8(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint8](cast[uint64](a))
    r = cast[uint8](cast[uint64](b))

  result = cast[pointer](uint64(l xor r))

proc add_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l + r))

proc sub_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l - r))

proc mul_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l * r))

proc mod_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l mod r))

proc shl_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l shl r))

proc shr_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l shr r))

proc and_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l and r))

proc or_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l or r))

proc xor_i32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int32](cast[int64](a))
    r = cast[int32](cast[int64](b))

  result = cast[pointer](int64(l xor r))

proc add_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l + r))

proc sub_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l - r))

proc mul_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l * r))

proc mod_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l mod r))

proc shl_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l shl r))

proc shr_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l shr r))

proc and_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l and r))

proc or_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l or r))

proc xor_u32(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint32](cast[uint64](a))
    r = cast[uint32](cast[uint64](b))

  result = cast[pointer](uint64(l xor r))

proc add_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l + r)

proc sub_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l - r)

proc mul_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l * r)

proc mod_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l mod r)

proc shl_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l shl r)

proc shr_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l shr r)

proc and_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l and r)

proc or_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l or r)

proc xor_i64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[int64](a)
    r = cast[int64](b)

  result = cast[pointer](l xor r)

proc add_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l + r)

proc sub_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l - r)

proc mul_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l * r)

proc mod_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l mod r)

proc shl_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l shl r)

proc shr_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l shr r)

proc and_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l and r)

proc or_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l or r)

proc xor_u64(a: pointer, b: pointer): pointer {.cdecl.} =
  let
    l = cast[uint64](a)
    r = cast[uint64](b)

  result = cast[pointer](l xor r)

proc bool_repr(n: pointer): string {.cdecl.} =
  return $(cast[bool](n))

proc signed_int_repr(n: pointer): string {.cdecl.} =
  return $(cast[int64](n))

proc unsigned_int_repr(n: pointer): string {.cdecl.} =
  return $(cast[uint64](n))

proc char_repr(n: pointer): string {.cdecl.} =
  return $(cast[Rune](n))

proc parseHex128(s: string, res: var uint128): int {.cdecl.} =
  var
    i          = 0
    n: uint128 = 0

  if s.len() >= 2 and s[0] == '0' and s[1] == 'x':
    i = 2
    if i == s.len():
      return -3

  if (i + 32) < s.len():
    return -1 # Always too long.

  while i < s.len():
    n = n shl uint128(4)
    case s[i]
    of '0' .. '9':
      n = n or iToU128(uint16((byte(s[i]) - byte('0'))))
    of 'a' .. 'f':
      n = n or iToU128(uint16((byte(s[i]) - byte('a') + 10)))
    of 'A' .. 'F':
      n = n or iToU128(uint16((byte(s[i]) - byte('A') + 10)))
    else:
      return -2
    i = i + 1

  res = n
  if n > high(uint64):
    return 16
  if res > high(uint32):
    return 8
  if res > high(uint16):
    return 4
  return 1


proc construct_int(s: string, st: SyntaxType, err: var string, width: int,
                    sign: var bool): pointer {.cdecl.} =
  var
    num:          uint128
    parsedWidth:  int

  case st
  of STHex:
    parsedWidth = s.parseHex128(num)
    if parsedwidth == -1 or parsedWidth > width:
      err = "HexTooLarge"
      return
    elif parsedwidth < 0:
      err = "BadHex"
      return
  of STBase10:
    parsedwidth = s.parseInt128(num, sign)
    if parsedWidth == -1 or parsedWidth > width:
      err = "IntTooLarge"
      return
    elif parsedWidth < 0:
      err = "BadInt"
      return
  else:
    unreachable

  var val = cast[int64](num.u128ToU64())

  if sign:
    val = -val

  result = cast[pointer](val)

proc new_bool_lit(s: string, st: SyntaxType, lmod: string,
                  l: var int,  err: var string): pointer {.cdecl.} =
  l = 8

  case s
  of "True", "true":
    return cast[pointer](1)
  of "False", "false":
    return cast[pointer](0)
  else:
    err = "BadBool"

proc new_byte_lit(lit: pointer, st: SyntaxType, lmod: string,
                  l: var int, err: var string): pointer {.cdecl.} =
  l = 8

  if st == STChrQuotes:
    var cp = cast[uint](lit)
    if cp > 0xff:
      err = "BadByte"
    else:
      result = lit
  else:
    var sign: bool

    result = construct_int(cast[string](lit), st, err, 1, sign)
    if sign:
      err = "TypeNotSigned"
      return nil

proc new_char_lit(lit: pointer, st: SyntaxType, lmod: string,
                  l: var int, err: var string): pointer {.cdecl.} =

  l = 8

  if st == STChrQuotes:
    result = lit
  else:
    var sign: bool
    result = construct_int(cast[string](lit), st, err, 4, sign)

  let cp = cast[uint](result)
  if cp > 0x10ffff:
    err = "BadCodepoint"
    return nil
  elif cp >= 0xd800 and cp <= 0xdff:
    err = "BadCP2"
    return nil

proc new_i8_lit(lit: pointer, st: SyntaxType, lmod: string,
                l: var int, err: var string): pointer {.cdecl.} =
  var sign: bool

  l = 8

  result = construct_int(cast[string](lit), st, err, 1, sign)
  if sign:
    result = cast[pointer](-cast[int64](result))

proc new_i32_lit(lit: pointer, st: SyntaxType, lmod: string,
                 l: var int, err: var string): pointer {.cdecl.} =
  var sign: bool

  l = 8

  result = construct_int(cast[string](lit), st, err, 4, sign)
  if sign:
    result = cast[pointer](-cast[int64](result))

proc new_u32_lit(lit: pointer, st: SyntaxType, lmod: string,
                 l: var int, err: var string): pointer {.cdecl.} =
  var sign: bool

  l = 8

  result = construct_int(cast[string](lit), st, err, 4, sign)
  if sign:
    err = "TypeNotSigned"
    return nil

proc new_i64_lit(lit: pointer, st: SyntaxType, lmod: string,
                 l: var int, err: var string): pointer {.cdecl.} =
  var sign: bool

  l = 8

  result = construct_int(cast[string](lit), st, err, 8, sign)
  if sign:
    result = cast[pointer](-cast[int64](result))

proc new_u64_lit(lit: pointer, st: SyntaxType, lmod: string,
                 l: var int, err: var string): pointer {.cdecl.} =
  var sign: bool

  l = 8

  result = construct_int(cast[string](lit), st, err, 8, sign)
  if sign:
    err = "TypeNotSigned"
    return nil

boolOps[FRepr]   = cast[pointer](bool_repr)
boolOps[FEq]     = cast[pointer](value_eq)
boolOps[FNewLit] = cast[pointer](new_bool_lit)
boolOps[FCastFn] = cast[pointer](get_cast_func_bool)

byteOps[FRepr]   = cast[pointer](unsigned_int_repr)
byteOps[Feq]     = cast[pointer](value_eq)
byteOps[FLt]     = cast[pointer](value_lt)
byteOps[FGt]     = cast[pointer](value_gt)
byteOps[FNewLit] = cast[pointer](new_byte_lit)
byteOps[FAdd]    = cast[pointer](add_u8)
byteOps[FSub]    = cast[pointer](sub_u8)
byteOps[FMul]    = cast[pointer](mul_u8)
byteOps[FFDiv]   = cast[pointer](fdiv_allint)
byteOps[FIDiv]   = cast[pointer](idiv_allint)
byteOps[FMod]    = cast[pointer](mod_u8)
byteOps[FShl]    = cast[pointer](shl_u8)
byteOps[FShr]    = cast[pointer](shr_u8)
byteOps[FBand]   = cast[pointer](and_u8)
byteOps[FBor]    = cast[pointer](or_u8)
byteOps[FBxor]   = cast[pointer](xor_u8)
boolOps[FCastFn] = cast[pointer](get_cast_func_u8)

charOps[FRepr]   = cast[pointer](char_repr)
charOps[Feq]     = cast[pointer](value_eq)
charOps[FNewLit] = cast[pointer](new_char_lit)
# For now; this isn't quite ok.
charOps[FCastFn] = cast[pointer](get_cast_func_u32)


i8Ops[FRepr]     = cast[pointer](signed_int_repr)
i8Ops[FEq]       = cast[pointer](value_eq)
i8Ops[FLt]       = cast[pointer](value_lt)
i8Ops[FGt]       = cast[pointer](value_gt)
i8Ops[FNewLit]   = cast[pointer](new_i8_lit)
i8Ops[FAdd]      = cast[pointer](add_i8)
i8Ops[FSub]      = cast[pointer](sub_i8)
i8Ops[FMul]      = cast[pointer](mul_i8)
i8Ops[FFDiv]     = cast[pointer](fdiv_allint)
i8Ops[FIDiv]     = cast[pointer](idiv_allint)
i8Ops[FMod]      = cast[pointer](mod_i8)
i8Ops[FShl]      = cast[pointer](shl_i8)
i8Ops[FShr]      = cast[pointer](shr_i8)
i8Ops[FBand]     = cast[pointer](and_i8)
i8Ops[FBor]      = cast[pointer](or_i8)
i8Ops[FBxor]     = cast[pointer](xor_i8)
i8Ops[FCastFn]   = cast[pointer](get_cast_func_i8)

i32Ops[FRepr]    = cast[pointer](signed_int_repr)
i32Ops[FEq]      = cast[pointer](value_eq)
i32Ops[FLt]      = cast[pointer](value_lt)
i32Ops[FGt]      = cast[pointer](value_gt)
i32Ops[FNewLit]  = cast[pointer](new_i32_lit)
i32Ops[FAdd]     = cast[pointer](add_i32)
i32Ops[FSub]     = cast[pointer](sub_i32)
i32Ops[FMul]     = cast[pointer](mul_i32)
i32Ops[FFDiv]    = cast[pointer](fdiv_allint)
i32Ops[FIDiv]    = cast[pointer](idiv_allint)
i32Ops[FMod]     = cast[pointer](mod_i32)
i32Ops[FShl]     = cast[pointer](shl_i32)
i32Ops[FShr]     = cast[pointer](shr_i32)
i32Ops[FBand]    = cast[pointer](and_i32)
i32Ops[FBor]     = cast[pointer](or_i32)
i32Ops[FBxor]    = cast[pointer](xor_i32)
i32Ops[FCastFn]  = cast[pointer](get_cast_func_i32)

u32Ops[FRepr]    = cast[pointer](unsigned_int_repr)
u32Ops[FEq]      = cast[pointer](value_eq)
u32Ops[FLt]      = cast[pointer](value_lt)
u32Ops[FGt]      = cast[pointer](value_gt)
u32Ops[FNewLit]  = cast[pointer](new_u32_lit)
u32Ops[FAdd]     = cast[pointer](add_u32)
u32Ops[FSub]     = cast[pointer](sub_u32)
u32Ops[FMul]     = cast[pointer](mul_u32)
u32Ops[FFDiv]    = cast[pointer](fdiv_allint)
u32Ops[FIDiv]    = cast[pointer](idiv_allint)
u32Ops[FMod]     = cast[pointer](mod_u32)
u32Ops[FShl]     = cast[pointer](shl_u32)
u32Ops[FShr]     = cast[pointer](shr_u32)
u32Ops[FBand]    = cast[pointer](and_u32)
u32Ops[FBor]     = cast[pointer](or_u32)
u32Ops[FBxor]    = cast[pointer](xor_u32)
u32Ops[FCastFn]  = cast[pointer](get_cast_func_u32)

i64Ops[FRepr]    = cast[pointer](signed_int_repr)
i64Ops[FEq]      = cast[pointer](value_eq)
i64Ops[FLt]      = cast[pointer](value_lt)
i64Ops[FGt]      = cast[pointer](value_gt)
i64Ops[FNewLit]  = cast[pointer](new_i64_lit)
i64Ops[FAdd]     = cast[pointer](add_i64)
i64Ops[FSub]     = cast[pointer](sub_i64)
i64Ops[FMul]     = cast[pointer](mul_i64)
i64Ops[FFDiv]    = cast[pointer](fdiv_allint)
i64Ops[FIDiv]    = cast[pointer](idiv_allint)
i64Ops[FMod]     = cast[pointer](mod_i64)
i64Ops[FShl]     = cast[pointer](shl_i64)
i64Ops[FShr]     = cast[pointer](shr_i64)
i64Ops[FBand]    = cast[pointer](and_i64)
i64Ops[FBor]     = cast[pointer](or_i64)
i64Ops[FBxor]    = cast[pointer](xor_i64)
i64Ops[FCastFn]  = cast[pointer](get_cast_func_i64)

u64Ops[FRepr]    = cast[pointer](unsigned_int_repr)
u64Ops[FEq]      = cast[pointer](value_eq)
u64Ops[FLt]      = cast[pointer](value_lt)
u64Ops[FGt]      = cast[pointer](value_gt)
u64Ops[FNewLit]  = cast[pointer](new_u64_lit)
u64Ops[FAdd]     = cast[pointer](add_u64)
u64Ops[FSub]     = cast[pointer](sub_u64)
u64Ops[FMul]     = cast[pointer](mul_u64)
u64Ops[FFDiv]    = cast[pointer](fdiv_allint)
u64Ops[FIDiv]    = cast[pointer](idiv_allint)
u64Ops[FMod]     = cast[pointer](mod_u64)
u64Ops[FShl]     = cast[pointer](shl_u64)
u64Ops[FShr]     = cast[pointer](shr_u64)
u64Ops[FBand]    = cast[pointer](and_u64)
u64Ops[FBor]     = cast[pointer](or_u64)
u64Ops[FBxor]    = cast[pointer](xor_u64)
u64Ops[FCastFn]  = cast[pointer](get_cast_func_u64)

let
  TBool* = addDataType(name = "bool", concrete = true, isBool = true,
                       byValue = true, ops = boolOps)
  TInt8* = addDataType(name = "i8", byValue = true, concrete = true,
                       intw = 1, signed = true, ops = i8ops)
  TByte* = addDataType(name = "byte", byValue = true, concrete = true,
                       intw = 1, signed = false, ops = byteOps,
                       signedVariant = TInt8)
  TInt32*  = addDataType(name = "i32", byValue = true, concrete = true,
                       intw = 4, signed = true, ops = i32ops)
  TChar*   = addDataType(name = "char", byValue = true, concrete = true,
                         intw = 4, signed = false, ops = charOps,
                         signedVariant = TInt32)
  TUint32* = addDataType(name = "u32", byValue = true, concrete = true,
                         intw = 4, signed = false, ops = u32ops,
                         signedVariant = TInt32)
  TInt*    = addDataType(name = "int", byValue = true, concrete = true,
                         intw = 8, signed = true, ops = i64ops)
  TUInt*   = addDataType(name = "uint", byValue = true, concrete = true,
                         intw = 8, signed = false, ops = u64ops,
                         signedVariant = TInt)

registerSyntax(TBool,   STBoolLit,   @[], primary = true)
registerSyntax(TByte,   STBase10,    @["u8"])
registerSyntax(TByte,   STHex,       @["u8"])
registerSyntax(TByte,   StChrQuotes, @["u8"])
registerSyntax(TChar,   STBase10,    @[])
registerSyntax(TChar,   STHex,       @[])
registerSyntax(TChar,   STChrQuotes, @[], primary = true)
registerSyntax(TInt8,   STBase10,    @[])
registerSyntax(TInt8,   STHex,       @[])
registerSyntax(TInt32,  STBase10,    @[])
registerSyntax(TInt32,  STHex,       @[])
registerSyntax(TUint32, STBase10,    @[])
registerSyntax(TUint32, STHex,       @[])
registerSyntax(TInt,    STBase10,    @["i64", "i"], primary = true)
registerSyntax(TInt,    STHex,       @["i64", "i"], primary = true)
registerSyntax(TUint,   STBase10,    @["u64", "u"])
registerSyntax(TUint,   STHex,       @["u64", "u"])
