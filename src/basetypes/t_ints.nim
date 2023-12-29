import ../common

proc parseHex128*(s: string, res: var uint128): int =
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

proc parseInt128*(s: string, res: var uint128, sign: var bool): int =
  # When we're calling this from the lexer, sign will never appear,
  # because it treats that as a separate token.
  var
    i = 0
    last: uint128

  if s.len() == 0:
    return -1
  if s[0] == '-':
    sign = true
    i += 1
    if i == len(s):
      return -3
  else:
    sign = false

  while i < s.len():
    res *= 10
    let n = byte(s[i]) - byte('0')
    if n < 0 or n > 9:
      return -2
    res += iToU128(uint16(n))
    i += 1
    if res < last:  # overflow.
      return -1
    last = res

  if sign:
    if res > iToU128(high(int)) or (sign and res == iToU128(-low(int))):
      return 16
    elif res > iToU128(high(int32)) or (sign and res == iToU128(-low(int32))):
      return 8
    elif res > iToU128(high(int8)) or (sign and res == iToU128(-low(int8))):
      return 4
    else:
      return 1
  else:
    if res > high(uint):
      return 16
    elif res > high(uint32):
      return 8
    elif res > high(uint8):
      return 4
    else:
      return 1

proc constructInteger*[T](s: string, outObj: var Mixed, st: SyntaxType,
                          name: string): string =
  var
    num:   uint128
    val:   T
    sign:  bool
    width: int

  case st
  of STHex:
    width = s.parseHex128(num)
    if width == -1 or width > sizeof(T):
      return "Hex number too large for " & name & " type"
    elif width < 0:
      return "Invalid hex literal"
  of STBase10:
    width = s.parseInt128(num, sign)
    if width == -1 or width > sizeof(T):
      return "Integer literal too large for " & name & " type"
    elif width < 0:
      return "Invalid integer literal"
  else:
    unreachable

  when T is uint128:
    val = num
  when T is int128:
    val = T(num)
  else:
    val = cast[T](num.u128ToU64())

  outObj = val.toMixed()

proc constructUint128(s: string, outObj: var Mixed, st: SyntaxType):
                      string {.cdecl.} =
  return constructInteger[uint128](s, outObj, st, "u128")
proc constructInt128(s: string, outObj: var Mixed, st: SyntaxType):
                     string {.cdecl.} =
  return constructInteger[int128](s, outObj, st, "i128")
proc constructUint64(s: string, outObj: var Mixed, st: SyntaxType):
                     string {.cdecl.} =
  return constructInteger[uint64](s, outObj, st, "uint")
proc constructInt64(s: string, outObj: var Mixed, st: SyntaxType):
                    string {.cdecl.} =
  return constructInteger[int64](s, outObj, st, "int")
proc constructUint32(s: string, outObj: var Mixed, st: SyntaxType):
                     string {.cdecl.} =
  return constructInteger[uint32](s, outObj, st, "u32")
proc constructInt32(s: string, outObj: var Mixed, st: SyntaxType):
                    string {.cdecl.} =
  return constructInteger[int32](s, outObj, st, "i32")
proc constructInt8(s: string, outObj: var Mixed, st: SyntaxType):
                    string {.cdecl.} =
  return constructInteger[int8](s, outObj, st, "i8")


proc repr8(tid: TypeId, m: Mixed): string {.cdecl.} =
    return $(toVal[int8](m))
proc repr32(tid: TypeId, m: Mixed): string {.cdecl.} =
    return $(toVal[int32](m))
proc repr64(tid: TypeId, m: Mixed): string {.cdecl.} =
    return $(toVal[int](m))
proc repr128(tid: TypeId, m: Mixed): string {.cdecl.} =
    return toVal[int128](m).toStr()
proc repru32(tid: TypeId, m: Mixed): string {.cdecl.} =
    return $(toVal[uint32](m))
proc repru64(tid: TypeId, m: Mixed): string {.cdecl.} =
    return $(toVal[uint](m))
proc repru128(tid: TypeId, m: Mixed): string {.cdecl.} =
    return toVal[uint128](m).toStr()
proc largeToBool(m: Mixed): bool {.cdecl.} =
  return toVal[int128](m) != iToI128(0)
proc largeToU128(m: Mixed): uint128 {.cdecl.} =
  return toVal[uint128](m)
proc largeToI128(m: Mixed): int128 {.cdecl.} =
  return toVal[int128](m)

proc eq128(a, b: CBox): bool {.cdecl.} =
  ## I think I forgot to add in a built-in equals, so this
  ## is a tmp hack (TODO)
  toVal[int128](a.v).toStr() == toVal[int128](b.v).toStr()

let
  TInt8*     = addBasicType(name        = "i8",
                            repr        = repr8,
                            kind        = stdIntKind,
                            intBits     = 8,
                            signed      = true,
                            castToBool  = normalSizeIntToBool,
                            castToU128  = normalSizeIntToU128,
                            castToI128  = normalSizeIntToI128,
                            fromRawLit  = constructInt8,
                            eqFn        = basicEq)
  TInt32*    = addBasicType(name        = "i32",
                            repr        = repr32,
                            kind        = stdIntKind,
                            intBits     = 32,
                            signed      = true,
                            castToBool  = normalSizeIntToBool,
                            castToU128  = normalSizeIntToU128,
                            castToI128  = normalSizeIntToI128,
                            fromRawLit  = constructInt32,
                            eqFn        = basicEq)
  TUint32*   = addBasicType(name        = "u32",
                            repr        = repru32,
                            kind        = stdIntKind,
                            intBits     = 32,
                            signed      = false,
                            castToBool  = normalSizeIntToBool,
                            castToU128  = normalSizeIntToU128,
                            castToI128  = normalSizeIntToI128,
                            fromRawLit  = constructUint32,
                            eqFn        = basicEq)
  TInt*      = addBasicType(name        = "int",
                            repr        = repr64,
                            kind        = stdIntKind,
                            litMods     = @["i", "i64"],
                            intBits     = 64,
                            signed      = true,
                            castToBool  = normalSizeIntToBool,
                            castToU128  = normalSizeIntToU128,
                            castToI128  = normalSizeIntToI128,
                            fromRawLit  = constructInt64,
                            eqFn        = basicEq)
  TUint*     = addBasicType(name        = "uint",
                            repr        = repru64,
                            kind        = stdIntKind,
                            litMods     = @["u", "u64"],
                            intBits     = 64,
                            signed      = false,
                            castToBool  = normalSizeIntToBool,
                            castToU128  = normalSizeIntToU128,
                            castToI128  = normalSizeIntToI128,
                            fromRawLit  = constructUint64,
                            eqFn        = basicEq)
  TInt128*   = addBasicType(name        = "i128",
                            repr        = repr128,
                            kind        = stdIntKind,
                            intBits     = 128,
                            signed      = true,
                            castToBool  = largeToBool,
                            castToU128  = largeToU128,
                            castToI128  = largeToI128,
                            fromRawLit  = constructInt128,
                            eqFn        = eq128)
  TUint128*  = addBasicType(name        = "u128",
                            repr        = repru128,
                            kind        = stdIntKind,
                            intBits     = 128,
                            signed      = false,
                            castToBool  = largeToBool,
                            castToU128  = largeToU128,
                            castToI128  = largeToI128,
                            fromRawLit  = constructUint128,
                            eqFn        = eq128)
