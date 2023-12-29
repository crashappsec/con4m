import unicode, ../common

proc constructRope(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.}  =
  # The value getting shoved in an Mixed doesn't get incref'd;
  # that means we need to be careful not to leak ropes; they must
  # be decref'd when we're done w/ the Mixed object.
  var r: Rope = text(s)
  GC_ref(r)
  outObj = r.toMixed()

proc constructUtf32(s: string, outObj: var Mixed, st: SyntaxType):
                   string {.cdecl.} =
  var runes: seq[Rune]
  try:
    runes  = s.toRunes()
    outObj = runes.toMixed()
  except:
    return "Invalid Unicode codepoint in string."

proc constructUtf8(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.} =
  var s = s

  if s.validateUtf8() == -1:
    outObj = s.toMixed()
  else:
    return "Invalid UTF-8 character in string."

proc constructBuf(s: string, outObj: var Mixed, st: SyntaxType):
                 string {.cdecl.}  =
  var s = s

  outObj = s.toMixed()

proc reprStr(id: TypeId, m: Mixed): string {.cdecl.} =
  var r = toVal[Rope](m)
  return r.toUtf8()

proc reprBuf(id: TypeId, m: Mixed): string {.cdecl.} =
  return hex(toVal[string](m))

proc reprUtf8(id: TypeId, m: Mixed): string {.cdecl.} =
  return toVal[string](m)

proc reprUtf32(id: TypeId, m: Mixed): string {.cdecl.} =
  var r = toVal[seq[Rune]](m)

  return $r

proc basicStrToBool*(m: Mixed): bool {.cdecl.} =
  let s = toVal[string](m)
  return s.len() != 0

proc ropeToBool*(m: Mixed): bool {.cdecl.} =
  return toVal[Rope](m) != nil

proc u32ToBool*(m: Mixed): bool {.cdecl.} =
  let r = toVal[seq[Rune]](m)
  return r.len() != 0

proc strEq(a, b: CBox): bool {.cdecl.} =
  var
    s1 = toVal[string](a.v)
    s2 = toVal[string](b.v)

  return s1 == s2

proc runeEq(a, b: CBox): bool {.cdecl.} =
  var
    s1 = toVal[Rune](a.v)
    s2 = toVal[Rune](b.v)

  return s1 == s2

let
  TRich*     = addBasicType(name        = "rich",
                            repr        = reprStr,
                            kind        = stdStrKind,
                            litMods     = @["r"],
                            castToBool  = ropeToBool,
                            fromRawLit  = constructRope,
                            eqFn        = pointerEq)
  TBuffer*   = addBasicType(name        = "buffer",
                            repr        = reprBuf,
                            kind        = stdStrKind,
                            castToBool  = basicStrToBool,
                            litMods     = @["bin"],
                            fromRawLit  = constructBuf,
                            eqFn        = strEq)
  TString*   = addBasicType(name        = "string",
                            repr        = reprUtf8,
                            kind        = stdStrKind,
                            castToBool  = basicStrToBool,
                            litMods     = @["u", "utf8"],
                            fromRawLit  = constructUtf8,
                            eqFn        = strEq)
  TUtf32*    = addBasicType(name        = "utf32",
                            repr        = reprutf32,
                            kind        = stdStrKind,
                            litMods     = @["u32"],
                            castToBool  = u32ToBool,
                            fromRawLit  = constructUtf32,
                            eqFn        = runeEq)
  TPath*     = addBasicType(name        = "path",
                            repr        = reprutf8,
                            kind        = stdStrKind,
                            litMods     = @["p"],
                            castToBool  = basicStrToBool,
                            fromRawLit  = constructUtf8,
                            eqFn        = strEq)
