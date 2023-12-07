import unicode, common

proc constructRope(s: string, outObj: var Any, st: SyntaxType):
                  string {.cdecl.}  =
  # The value getting shoved in an Any doesn't get incref'd;
  # that means we need to be careful not to leak ropes; they must
  # be decref'd when we're done w/ the Any object.
  var r: Rope = text(s)
  GC_ref(r)
  outObj = r.toAny()

proc constructUtf32(s: string, outObj: var Any, st: SyntaxType):
                   string {.cdecl.} =
  var runes: seq[Rune]
  try:
    runes  = s.toRunes()
    outObj = runes.toAny()
  except:
    return "Invalid Unicode codepoint in string."

proc constructUtf8(s: string, outObj: var Any, st: SyntaxType):
                  string {.cdecl.} =
  var s = s

  if s.validateUtf8() == -1:
    outObj = s.toAny()
  else:
    return "Invalid UTF-8 character in string."

proc constructBuf(s: string, outObj: var Any, st: SyntaxType):
                 string {.cdecl.}  =
  var s = s

  outObj = s.toAny()

let
  TString*   = addBasicType(name        = "string",
                            kind        = stdStrKind,
                            litMods     = @["r"],
                            fromRawLit  = constructRope)
  TBuffer*   = addBasicType(name        = "buffer",
                            kind        = stdStrKind,
                            litMods     = @["b"],
                            fromRawLit  = constructBuf)
  TUtf8*     = addBasicType(name        = "utf8",
                            kind        = stdStrKind,
                            litMods     = @["u"],
                            fromRawLit  = constructUtf8)
  TUtf32*    = addBasicType(name        = "utf32",
                            kind        = stdStrKind,
                            litMods     = @["u32"],
                            fromRawLit  = constructUtf32)
  TPath*     = addBasicType(name        = "path",
                            kind        = stdStrKind,
                            litMods     = @["p", "path"],
                            fromRawLit  = constructUtf8)
