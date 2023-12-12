import common

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


let
  TInt8*     = addBasicType(name        = "i8",
                            kind        = stdIntKind,
                            litMods     = @["i8"],
                            intBits     = 8,
                            signed      = true,
                            fromRawLit  = constructInt8)
  TInt32*    = addBasicType(name        = "i32",
                            kind        = stdIntKind,
                            litMods     = @["i32"],
                            intBits     = 32,
                            signed      = true,
                            fromRawLit  = constructInt32)
  TUint32*   = addBasicType(name        = "u32",
                            kind        = stdIntKind,
                            litMods     = @["u32"],
                            intBits     = 32,
                            signed      = false,
                            fromRawLit  = constructUint32)
  TInt*      = addBasicType(name        = "int",
                            kind        = stdIntKind,
                            litMods     = @["i", "i64", "int"],
                            intBits     = 64,
                            signed      = true,
                            fromRawLit  = constructInt64)
  TUint*     = addBasicType(name        = "uint",
                            kind        = stdIntKind,
                            litMods     = @["u", "u64", "uint"],
                            intBits     = 64,
                            signed      = false,
                            fromRawLit  = constructUint64)
  TInt128*   = addBasicType(name        = "i128",
                            kind        = stdIntKind,
                            litMods     = @["i128"],
                            intBits     = 128,
                            signed      = true,
                            fromRawLit  = constructInt128)
  TUint128*  = addBasicType(name        = "u128",
                            kind        = stdIntKind,
                            litMods     = @["u128"],
                            intBits     = 128,
                            signed      = false,
                            fromRawLit  = constructUint128)
