import std/uri, common

proc constructUrl(s: string, outObj: var Mixed, st: SyntaxType):
                 string {.cdecl.} =
  var uri: Uri

  try:
    uri    = parseUri(s)
    outObj = uri.toMixed()
  except:
    return "Invalid url syntax."

let
  TUrl* = addBasicType(name        = "url",
                       kind        = stdStrKind,
                       litMods     = @["url"],
                       fromRawLit  = constructUrl)
