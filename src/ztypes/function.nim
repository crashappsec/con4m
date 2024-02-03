import base, ../common, strutils

proc toString(x: TypeId): string {.importc, cdecl.}

proc fn_repr(c: ptr ZCallback): cstring {.exportc, cdecl.} =
  let
    rt  = get_con4m_runtime()
    eix = rt.obj.staticdata.find('\0', c.nameoffset)
    n   = rt.obj.staticdata[c.nameOffset ..< eix]
    s   = newC4Str("func " & n & c.tid.toString())

  return cast[cstring](s)

proc fn_eq(c1: ptr ZCallback, c2: ptr ZCallback): bool {.exportc, cdecl.} =
  return c1.impl == c2.impl

proc fn_copy(c1: ptr ZCallback): ptr ZCallback {.exportc, cdecl.} =
  result = ZCallback.create()
  result.impl       = c1.impl
  result.nameoffset = c1.nameoffset
  result.tid        = c1.tid
  result.ffi        = c1.ffi

var fnOps = newVtable()

fnOps[FRepr] = cast[pointer](fn_repr)
fnOps[FEq]   = cast[pointer](fn_eq)
fnOps[FCopy] = cast[pointer](fn_copy)

TFunc = addDataType(name = "func", concrete = true, ops = fnOps, ckind = C4Func)
