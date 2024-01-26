import base

var tsOps = newVTable()

proc toString(x: TypeId): string {.importc, cdecl.}
proc parse_con4m_type(s: string): TypeId {.importc, cdecl.}

proc tspec_repr(t: TypeId): string {.cdecl.} =
  return t.toString()

proc tspec_load_lit(cstr: cstring, l: cint): TypeId {.cdecl, exportc.} =
  return parse_con4m_type($cstr)

proc new_tspec_lit(t: TypeId): TypeId {.cdecl.} = t

tsOps[FRepr]    = cast[pointer](tspec_repr)
tsOps[FEq]      = cast[pointer](value_eq)
tsOps[FNewLit]  = cast[pointer](new_tspec_lit)
tsOps[FLoadLit] = cast[pointer](tspec_load_lit)

TTSpec = addDataType(name = "typespec", concrete = true, ops = tsOps,
                     byValue = true, ckind = C4TypeSpec)
