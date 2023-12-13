import ../common

proc constructBool*(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.} =
  var
    s = s
    b: bool

  case s
  of "True", "true":
    b = true
  of "False", "false":
    b = false
  else:
    return "Invalid boolean value."

  outObj = toMixed(b)

proc repr(tid: TypeId, m: Mixed): string {.cdecl.} =
  return $(toVal[bool](m))

let
  TBool* = addBasicType(name        = "bool",
                        repr        = repr,
                        kind        = stdBoolKind,
                        fromRawLit  = constructBool)
