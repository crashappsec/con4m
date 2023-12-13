import math, ../common, t_ints

proc constructFloat(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.} =
  var
    dotLoc = s.find('.')
    eLoc   = s.find('e')
    value:        float
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

  intSz = parseInt128(intPartS, intPartI, sign)

  if intSz > 8:
    return "Integer portion of float is too large; use 'e' notation."

  intSz = parseInt128(expPartS, expPartI, sign)

  if intSz > 8:
    return "Exponent portion of float is too large."

  # Fow now, we just truncate floating point digits if there are
  # $(high(int)).len() digits or more.

  if floatPartS != "":
    const
      maxAsString = $(high(int64))
      maxLen      = maxAsString.len()

    if floatPartS.len() >= maxLen:
      floatPartS = floatPartS[0 ..< maxLen]

    discard parseInt128(floatPartS, floatPartI, sign)

    value = int(u128toU64(floatPartI)) / int(10 ^ floatPartS.len())

  value = value + float(uint64(intPartI))
  value = value * pow(10.0, float(u128toU64(expPartI)))

  outObj = value.toMixed()

proc repr(t: TypeId, m: Mixed): string {.cdecl.} =
  return $(toVal[float](m))

let
  TFloat* = addBasicType(name        = "float",
                         repr        = repr,
                         kind        = stdFloatKind,
                         litMods     = @["f", "float"],
                         fromRawLit  = constructFloat)
