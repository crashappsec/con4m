import ../common, t_ints, nimutils

proc initCodepointLit(cp: uint, outObj: var Mixed): string {.cdecl.} =
  if cp > 0x10ffff:
    return "Invalid codepoint; value beyond U+10FFFF"
  elif cp >= 0xd800 and cp <= 0xdfff:
    return "Codepoints from U+D800 to U+DFFF are invalid."
  else:
    var codepoint = uint32(cp)
    outObj = codepoint.toMixed()

proc initByteLit(cp: uint, outObj: var Mixed): string {.cdecl.} =
  if cp > 0xff:
    return "Invalid value for a byte (cannot be above 0xff)"
  else:
    var b = uint8(cp)
    outObj = b.toMixed()

proc constructUint8(s: string, outObj: var Mixed, st: SyntaxType):
                    string {.cdecl.} =
  return constructInteger[uint8](s, outObj, st, "byte")


proc constructChar(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.} =
  case st
  of STHex, STBase10:
    result = constructInteger[uint32](s, outObj, st, "char")
    if result == "":
      var cp = toVal[uint32](outObj)
      if cp > 0x10ffff:
        return "Invalid codepoint; value beyond U+10FFFF"
      if cp >= 0xd800 and cp <= 0xdfff:
        return "Codepoints from U+D800 to U+DFFF are invalid."
  else:
    unreachable

proc charRepr(tid: TypeId, m: Mixed): string {.cdecl.} =
  return $(toVal[Rune](m))

proc byteRepr(tid: TypeId, m: Mixed): string {.cdecl.} =
  return $(toVal[uint8](m))

let
  TByte*     = addBasicType(name        = "byte",
                            repr        = byteRepr,
                            kind        = stdChrKind,
                            litMods     = @["b", "byte"],
                            fromRawLit  = constructUint8,
                            intBits     = 8,
                            signed      = false,
                            fromCharLit = initByteLit)
  TChar*     = addBasicType(name        = "char",
                            repr        = charRepr,
                            kind        = stdChrKind,
                            litMods     = @["c", "char"],
                            fromRawLit  = constructChar,
                            intBits     = 21,
                            signed      = false,
                            fromCharLit = initCodepointLit)
