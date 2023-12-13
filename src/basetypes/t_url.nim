import std/uri, ../common

proc constructUrl(s: string, outObj: var Mixed, st: SyntaxType):
                 string {.cdecl.} =
  var uri: Uri

  try:
    uri    = parseUri(s)
    outObj = uri.toMixed()
  except:
    return "Invalid url syntax."

proc repr(x: TypeId, m: Mixed): string {.cdecl.} =
  return $(toVal[Uri](m))

let
  TUrl* = addBasicType(name        = "url",
                       repr        = repr,
                       kind        = stdStrKind,
                       litMods     = @["url"],
                       fromRawLit  = constructUrl)
