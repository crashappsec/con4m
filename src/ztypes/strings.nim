import base, unicode, ordinals

let richLitMods = @["r", "md", "html", "h1", "h2", "h3",
                    "h4", "h5", "h6", "p", "em", "i", "b",
                    "strong", "underline", "pre", "code",
                    "inv"]
var
  strOps   = newVTable()
  bufOps   = newVTable()
  utf32Ops = newVTable()
  richOps  = newVTable()

proc str_new_lit(s: string, st: SyntaxType, lmod: string, l: var int,
                 err: var string): pointer {.cdecl.} =
  l = s.len() + 1
  result = cast[pointer](newC4Str(l))
  copyMem(result, addr s[0], l)

proc rich_new_lit(s: string, st: SyntaxType, lmod: string,
                  l: var int, err: var string): pointer {.cdecl.} =
  var toStore: string

  toStore = lmod & ":" & s
  l      = toStore.len() + 1

  result = cast[pointer](newC4Str(l))
  copyMem(result, addr toStore[0], l)

proc str_repr(pre: pointer): string {.cdecl.} =
  let s = cast[cstring](pre)
  return $(s)

proc u32_repr(pre: C4Str): string {.cdecl.} =
  var
    l = (pre.len() div 4) * 4  # If there's a byte for a trailing null, nuke it
    s = newSeq[Rune](int(l))

  copyMem(addr s[0], cast[pointer](pre), l)
  return $(s)

proc rich_repr(s: Rope): string {.cdecl.} =
  return s.toUtf8()

proc rich_pluseq(a: pointer, b: Rope): void {.cdecl.} =
  # I *think* I can declare `a` a `var Rope` here, but just in case.
  var a = cast[Rope](a)

  a += b

proc rich_add(a, b: Rope): Rope {.cdecl.} =
  return a + b

proc str_index(a: C4Str, b: int, err: var bool): int64 {.cdecl.} =
  var x = cast[cstring](a)

  if b < 0 or b >= x.len():
    err = true
    return 0

  return int64(x[b])

proc u32_index(a: pointer, b: int, err: var bool): int64 {.cdecl.} =
  # Pointer is to the char* part of a C4String.
  let bytelen = b * 4

  if bytelen < 0 or bytelen >= c4str_len(cast[C4Str](a)):
    err = true
    return 0

  let p = cast[ptr Rune](cast[uint64](a) + uint64(bytelen))

  return int64(p[])

proc rich_len(p: pointer): int {.cdecl.} =
  let r = cast[string](p)
  return r.runeLength()

proc u32_len(s: C4Str): int {.cdecl.} =
  return s.len() div 4

proc rich_copy(r: Rope): Rope {.cdecl, exportc.} =
  result = r.copy()
  GC_ref(result)

proc cast_str_to_u32(pre: C4Str, tfrom, tto: TypeId, err: var string):
                    C4Str {.cdecl, exportc.} =
  let
    l = pre.len()
    s = `$`(cast[cstring](pre)).toRunes()

  result = newC4Str(l * 4)

  if l != 0:
    copyMem(cast[pointer](result), addr s[0], l)

proc cast_str_to_rich(pre: pointer, tfrom, tto: TypeId,
                      err: var string): pointer {.cdecl, exportc.} =

  let s = $(cast[cstring](pre))
  var rope = text(s)

  GC_ref(rope)

  return cast[pointer](rope)

proc cast_u32_to_rich(pre: C4Str, tfrom, tto: TypeId, err: var string):
                     Rope {.cdecl, exportc.} =
  var
    l = (pre.len() div 4) * 4
    s = newSeq[Rune](int(l))

  if l != 0:
    copyMem(addr s[0], cast[pointer](pre), l)

  result = text($(s))
  GC_ref(result)

proc cast_u32_to_str(pre: C4Str, tfrom, tto: TypeId, err: var string):
                    C4Str {.cdecl, exportc.} =
  var
    l = (pre.len() div 4) * 4
    s = newSeq[Rune](int(l))

  if l != 0:
    copyMem(addr s[0], cast[pointer](pre), l)

  result = newC4Str($s)

proc cast_rich_to_u32(r: Rope, tfrom, to: TypeId, err: var string):
                     C4Str {.cdecl, exportc.} =
  let
    s = r.toUtf8(r.runeLength())
    u = s.toRunes()
    l = u.len() * 4

  result = newC4Str(l)
  if l != 0:
    copyMem(cast[pointer](result), addr u[0], l)

proc cast_rich_to_str(r: Rope, tfrom, tto: TypeId, err: var string):
                     C4Str {.cdecl, exportc.} =
  return newC4Str(r.toUtf8(r.runeLength()))

proc str_slice(a: pointer, b, c: int, err: bool): pointer =
  let
    x = cast[cstring](a)
    l = x.len()

  var
    b = b
    c = c

  if b < 0:
    b += l
  if c < 0:
    b += l

  if b >= c or b < 0 or c >= l:
    return nil

  return newC4Str(`$`(x)[b .. c])

proc u32_slice(a: pointer, b, c: int, err: var bool): pointer =
  return str_slice(a, b * 4, c * 4, err)

proc str_load_lit(cstr: cstring, l: cint): pointer =
  var s = newC4Str(int64(l))

  if l != 0:
    copyMem(cast[pointer](s), cstr, l)

  result = cast[pointer](s)

proc rich_load_lit(cstr: cstring, l: cint): pointer =
  let
    full = $cstr
    f    = full.find(':')
    lmod = full[0 ..< f]
    s    = full[f + 1 .. ^1]

  var r: Rope

  case lmod
  of "md":
    r = markdown(s)
  of "html":
    r = html(s)
  of "h1":
    r = h1(s)
  of "h2":
    r = h2(s)
  of "h3":
    r = h3(s)
  of "h4":
    r = h4(s)
  of "h5":
    r = h5(s)
  of "h6":
    r = h6(s)
  of "p":
    r = paragraph(s)
  of "em":
    r = em(s)
  of "i":
    r = italic(s)
  of "b":
    r = bold(s)
  of "strong":
    r = strong(s)
  of "underline":
    r = underline(s)
  of "pre":
    r = pre(s)
  of "code":
    r = code(s)
  of "inv":
    r = inverse(s)
  else:
    r = atom(s)

  GC_ref(r)

  result = cast[pointer](r)

# These need to be fw referenced as we need TString, etc. defined first.
proc get_cast_from_string(dt: DataType, t1, t2: TypeId,
                          err: var string): pointer
proc get_cast_from_u32(dt: Datatype, t1, t2: TypeId,
                       err: var string): pointer
proc get_cast_from_rich(dt: Datatype, t1, t2: TypeId,
                        err: var string): pointer

strOps[FRepr]         = cast[pointer](str_repr)
strOps[FCastFn]       = cast[pointer](get_cast_from_string)
strOps[FEq]           = cast[pointer](c4str_eq)
strOps[FLt]           = cast[pointer](c4str_lt)
strOps[FGt]           = cast[pointer](c4str_gt)
strOps[FAdd]          = cast[pointer](c4str_add)
strOps[FIndex]        = cast[pointer](str_index)
strOps[FSlice]        = cast[pointer](str_slice)
strOps[FNewLit]       = cast[pointer](str_new_lit)
strOps[FLoadLit]      = cast[pointer](str_load_lit)
strOps[FCopy]         = cast[pointer](c4str_copy)
strOps[FLen]          = cast[pointer](c4str_len)
bufOps[FRepr]         = cast[pointer](str_repr)
bufOps[FCastFn]       = cast[pointer](get_cast_from_string)
bufOps[FEq]           = cast[pointer](c4str_eq)
bufOps[FLt]           = cast[pointer](c4str_lt)
bufOps[FGt]           = cast[pointer](c4str_gt)
bufOps[FAdd]          = cast[pointer](c4str_add)
bufOps[FIndex]        = cast[pointer](str_index)
bufOps[FSlice]        = cast[pointer](str_slice)
bufOps[FNewLit]       = cast[pointer](str_new_lit)
bufOps[FCopy]         = cast[pointer](c4str_copy)
bufOps[FLen]          = cast[pointer](c4str_len)
utf32Ops[FRepr]       = cast[pointer](u32_repr)
utf32Ops[FStaticRepr] = cast[pointer](u32_repr)
utf32Ops[FCastFn]     = cast[pointer](get_cast_from_u32)
utf32Ops[FEq]         = cast[pointer](c4str_eq)
utf32Ops[FLt]         = cast[pointer](c4str_lt)
utf32Ops[FGt]         = cast[pointer](c4str_gt)
utf32Ops[FAdd]        = cast[pointer](c4str_add)
utf32Ops[FIndex]      = cast[pointer](u32_index)
utf32Ops[FSlice]      = cast[pointer](u32_slice)
utf32Ops[FNewLit]     = cast[pointer](str_new_lit)
utf32Ops[FCopy]       = cast[pointer](c4str_copy)
utf32Ops[FLen]        = cast[pointer](u32_len)
richOps[FRepr]        = cast[pointer](rich_repr)
richOps[FStaticRepr]  = cast[pointer](str_repr)
richOps[FCastFn]      = cast[pointer](get_cast_from_rich)
richOps[FEq]          = cast[pointer](value_eq)
richOps[FAdd]         = cast[pointer](rich_add)
richOps[FNewLit]      = cast[pointer](rich_new_lit)
richOps[FLoadLit]     = cast[pointer](rich_load_lit)
richOps[FCopy]        = cast[pointer](rich_copy)
richOps[FLen]         = cast[pointer](rich_len)
richOps[FPlusEqRef]   = cast[pointer](rich_pluseq)

let
  TString* = addDataType(name = "string", concrete = true,
                                strTy = true, ops = strOps)
  TBuffer* = addDataType(name = "buffer", concrete = true,
                                strTy = true, ops = bufOps)
  TUtf32*  = addDataType(name = "utf32",  concrete = true,
                                strTy = true, ops = utf32Ops)
  TRich*   = addDataType(name = "rich",   concrete = true, ops = richOps)

registerSyntax(TString, STStrQuotes, @["u", "u8"], primary = true)
registerSyntax(TBuffer, STStrQuotes, @["bin"])
registerSyntax(TUtf32,  STStrQuotes, @["u32"])
registerSyntax(TRich,   STStrQuotes, richLitMods)

proc get_cast_from_string(dt: DataType, t1, t2: TypeId,
                          err: var string): pointer =
  if dt.dtid == TUtf32:
    return cast[pointer](cast_str_to_u32)
  elif dt.dtid == TRich:
    return cast[pointer](cast_str_to_rich)
  elif dt.dtid in [TString, TBuffer]:
    return cast[pointer](cast_identity)
  elif dt.dtid == TBool:
    return cast[pointer](cast_to_bool)

proc get_cast_from_u32(dt: Datatype, t1, t2: TypeId,
                       err: var string): pointer =
  if dt.dtid == TUtf32:
    return cast[pointer](cast_identity)
  elif dt.dtid == TRich:
    return cast[pointer](cast_u32_to_rich)
  elif dt.dtid in [TBuffer, TString]:
    return cast[pointer](cast_u32_to_str)
  elif dt.dtid == TBool:
    return cast[pointer](cast_to_bool)

proc get_cast_from_rich(dt: Datatype, t1, t2: TypeId,
                        err: var string): pointer =
  if dt.dtid == TUtf32:
    result = cast[pointer](cast_rich_to_u32)
    err    = "LoseFormat"
  elif dt.dtid == TRich:
    result = cast[pointer](cast_identity)
  elif dt.dtid == TBuffer:
    result = cast[pointer](cast_rich_to_str)
    err    = "LoseFormat"
  elif dt.dtid == TString:
    result = cast[pointer](cast_rich_to_str)
    err    = "LoseFormat"
  elif dt.dtid == TBool:
    result = cast[pointer](cast_to_bool)
    err    = "LoseFormat"
