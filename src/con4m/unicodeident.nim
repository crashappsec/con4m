import streams
import unicode
import unicodedb/properties
# I'm surprised unicodedb doesn't provide this info, since it's in the
# Unicode spec.

proc isPatternSyntax*(r: Rune): bool =
  case r.ord()
  of 0x0021 .. 0x002f, 0x003a .. 0x0040, 0x005b .. 0x005e, 0x0060,
     0x007b .. 0x007e, 0x00a1 .. 0x00a7, 0x00a9, 0x00ab .. 0x00ac, 0x00ae,
     0x00b0 .. 0x00b1, 0x00b6, 0x00bb, 0x00bf, 0x00d7, 0x00f7,
     0x2010 .. 0x2027, 0x2030 .. 0x203e, 0x2041 .. 0x2053, 0x2055 .. 0x205e,
     0x2190 .. 0x245f, 0x2500 .. 0x2775, 0x2794 .. 0x2bff, 0x2e00 .. 0x2e7f,
     0x3001 .. 0x3003, 0x3008 .. 0x3020, 0x3030, 0xfd3e .. 0xfd3f,
     0xfe45 .. 0xfe46:
    return true
  else:
    return false

proc isPatternWhiteSpace*(r: Rune): bool =
  case r.ord()
  of 0x0009 .. 0x000d, 0x0020, 0x0085, 0x200e, 0x200f, 0x2028, 0x2029:
    return true
  else:
    return false

proc isOtherIdStart*(r: Rune): bool =
  case r.ord()
  of 0x1885 .. 0x1886, 0x2118, 0x212e, 0x309b .. 0x309c:
    return true
  else:
    return false

proc isOtherIdContinue*(r: Rune): bool =
  case r.ord()
  of 0x00b7, 0x0387, 0x1369 .. 0x1371, 0x19DA:
    return true
  else:
    return false

# \p{L}\p{Nl}\p{Other_ID_Start}-\p{Pattern_Syntax}-\p{Pattern_White_Space}
proc isIdStart*(r: Rune): bool =
  if (r.unicodeCategory() in ctgL+ctgNl) or r.isOtherIdStart():
    if not (r.isPatternSyntax() or r.isPatternWhiteSpace()):
      return true

  return false


# [\p{ID_Start}\p{Mn}\p{Mc}\p{Nd}\p{Pc}\p{Other_ID_Continue}-\p{Pattern_Syntax}
#  -\p{Pattern_White_Space}]
proc isIdContinue*(r: Rune): bool =
  if (r.unicodeCategory() in ctgL+ctgNl+ctgMn+ctgMc+ctgNd+ctgPc) or
     r.isOtherIdStart() or r.isOtherIdContinue():
    if not (r.isPatternSyntax() or r.isPatternWhiteSpace()):
      return true

  return false

proc isValidId*(s: string): bool =
  if s.len() == 0:
    return false

  let l = s.runeLenAt(0)
  
  if not s.runeAt(0).isIdStart():
    return false

  for rune in s[l .. ^1].runes():
    if not rune.isIdContinue():
      return false

  return true

proc readRune*(s: Stream): Rune =
  var str = newString(4)
  let c = s.readChar()
  str[0] = c
  if (uint(c) and 0x80) != 0:
    let c = s.readChar()
    str[1] = c
    if (uint(c) and 0x80) != 0:
      let c = s.readChar()
      str[2] = c
      if (uint(c) and 0x80) != 0:
        str[3] = s.readChar()
  str.fastRuneAt(0, result, false)

proc peekRune*(s: Stream): Rune =
  let n = s.getPosition()
  result = s.readRune()
  s.setPosition(n)
