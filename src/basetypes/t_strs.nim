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
  return $r

proc reprBuf(id: TypeId, m: Mixed): string {.cdecl.} =
  return hex(toVal[string](m))

proc reprUtf8(id: TypeId, m: Mixed): string {.cdecl.} =
  return toVal[string](m)

proc reprUtf32(id: TypeId, m: Mixed): string {.cdecl.} =
  var r = toVal[seq[Rune]](m)

  return $r

let
  TString*   = addBasicType(name        = "string",
                            repr        = reprStr,
                            kind        = stdStrKind,
                            litMods     = @["r"],
                            fromRawLit  = constructRope)
  TBuffer*   = addBasicType(name        = "buffer",
                            repr        = reprBuf,
                            kind        = stdStrKind,
                            litMods     = @["b"],
                            fromRawLit  = constructBuf)
  TUtf8*     = addBasicType(name        = "utf8",
                            repr        = reprUtf8,
                            kind        = stdStrKind,
                            litMods     = @["u"],
                            fromRawLit  = constructUtf8)
  TUtf32*    = addBasicType(name        = "utf32",
                            repr        = reprutf32,
                            kind        = stdStrKind,
                            litMods     = @["u32"],
                            fromRawLit  = constructUtf32)
  TPath*     = addBasicType(name        = "path",
                            repr        = reprutf8,
                            kind        = stdStrKind,
                            litMods     = @["p", "path"],
                            fromRawLit  = constructUtf8)
