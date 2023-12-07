import common

proc constructBool*(s: string, outObj: var Any, st: SyntaxType):
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

  outObj = toAny(b)


let
  TBool* = addBasicType(name        = "bool",
                        kind        = stdBoolKind,
                        fromRawLit  = constructBool)
