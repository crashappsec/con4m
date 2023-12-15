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

proc castToSelf(n: Mixed): bool {.cdecl.} =
  result = toVal[bool](n)

let
  TBool* = addBasicType(name        = "bool",
                        repr        = repr,
                        castToBool  = castToSelf,
                        kind        = stdBoolKind,
                        fromRawLit  = constructBool,
                        eqFn        = basicEq)
