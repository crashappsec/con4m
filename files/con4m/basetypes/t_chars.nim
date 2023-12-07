import common, t_ints

proc initCodepointLit(cp: uint, outObj: var Any): string =
  if cp > 0x10ffff:
    return "Invalid codepoint; value beyond U+10FFFF"
  elif cp >= 0xd800 and cp <= 0xdfff:
    return "Codepoints from U+D800 to U+DFFF are invalid."
  else:
    var codepoint = uint32(cp)
    outObj = codepoint.toAny()

proc initByteLit(cp: uint, outObj: var Any): string =
  if cp > 0xff:
    return "Invalid value for a byte (cannot be above 0xff)"
  else:
    var b = uint8(cp)
    outObj = b.toAny()

proc constructUint8(s: string, outObj: var Any, st: SyntaxType):
                    string {.cdecl.} =
  return constructInteger[uint8](s, outObj, st, "byte")


proc constructChar(s: string, outObj: var Any, st: SyntaxType):
                  string {.cdecl.} =
  case st
  of STHex, STBase10:
    result = constructInteger[uint32](s, outObj, st, "char")
    if result == "":
      var cp = outObj.getUint32()
      if cp > 0x10ffff:
        return "Invalid codepoint; value beyond U+10FFFF"
      if cp >= 0xd800 and cp <= 0xdfff:
        return "Codepoints from U+D800 to U+DFFF are invalid."
  else:
    unreachable


let
  TChar*     = addBasicType(name        = "char",
                            kind        = stdChrKind,
                            litMods     = @["c", "char"],
                            fromRawLit  = constructChar,
                            fromCharLit = initCodepointLit)
  TByte*     = addBasicType(name        = "byte",
                            kind        = stdChrKind,
                            litMods     = @["b", "byte"],
                            fromRawLit  = constructUint8,
                            fromCharLit = initByteLit)
