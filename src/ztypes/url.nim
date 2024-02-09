import std/uri, base

proc repr_url(m: pointer): string {.cdecl.} =
  return $(extractRef[Uri](m))

proc eq_url(a, b: pointer): bool {.cdecl.} =
  return `$`(extractRef[Uri](a)) == `$`(extractRef[Uri](b))

proc new_url(s: string, st: SyntaxType, lmod: string,
             err: var string): pointer {.cdecl.} =
  var uri: Uri

  if s.len() < 4 or s[0 .. 3] != "http":
    err = "BadUrl"
    return
  try:
    uri    = parseUri(s)
    result = alloc0(s.len() + 1)
    copyMem(result, addr s[0], s.len())

  except:
    err = "BadUrl"

var urlOps = newVTable()

urlOps[FRepr]   = cast[pointer](repr_url)
urlOps[FEq]     = cast[pointer](eq_url)
urlOps[FNewLit] = cast[pointer](new_url)

let TUrl* = addDataType(name = "url", concrete = true, ops = urlOps)
registerSyntax(TUrl, STOther, @[])
registerSyntax(TUrl, STStrQuotes, @[])
