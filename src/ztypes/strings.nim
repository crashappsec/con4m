import base, unicode, ordinals

# These all need forward declarations because they refer to types that
# haven't been declared yet.
proc cast_str_to_u32(pre: pointer): pointer {.cdecl.}
proc cast_str_to_rich(pre: pointer): pointer {.cdecl.}
proc cast_u32_to_rich(pre: pointer): pointer {.cdecl.}
proc cast_u32_to_u8(pre: pointer): pointer {.cdecl.}
proc cast_u32_to_str(pre: pointer): pointer {.cdecl.}
proc cast_rich_to_u32(pre: pointer): pointer {.cdecl.}
proc cast_rich_to_u8(pre: pointer): pointer {.cdecl.}
proc cast_rich_to_str(pre: pointer): pointer {.cdecl.}
proc get_cast_from_string(dt: DataType, err: var string): pointer {.cdecl.}
proc get_cast_from_u32(dt: Datatype, err: var string): pointer {.cdecl.}
proc get_cast_from_rich(dt: Datatype, err: var string): pointer {.cdecl.}
proc str_add(a, b: pointer): pointer {.cdecl.}
proc buf_add(a, b: pointer): pointer {.cdecl.}
proc u32_add(a, b: pointer): pointer {.cdecl.}
proc rich_add(a, b: pointer): pointer {.cdecl.}
proc str_slice(a: pointer, b, c: int): pointer {.cdecl.}
proc buf_slice(a: pointer, b, c: int, err: var bool): pointer {.cdecl.}
proc u32_slice(a: pointer, b, c: int, err: var bool): pointer {.cdecl.}
proc new_str_lit(s: string, st: SyntaxType, lmod: string,
                 err: var string): pointer {.cdecl.}
proc new_buf_lit(s: string, st: SyntaxType, lmod: string,
                 err: var string): pointer {.cdecl.}
proc new_u32_lit(s: string, st: SyntaxType, lmod: string,
                 err: var string): pointer {.cdecl.}
proc new_rich_lit(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer {.cdecl.}
proc str_copy(p: pointer): pointer {.cdecl.}
proc buf_copy(p: pointer): pointer {.cdecl.}
proc u32_copy(p: pointer): pointer {.cdecl.}
proc rich_copy(p: pointer): pointer {.cdecl.}



var
  strOps   = newVTable()
  bufOps   = newVTable()
  utf32Ops = newVTable()
  richOps  = newVTable()

proc str_repr(pre: pointer): string {.cdecl.} =
  let s = extractRef[string](pre)
  return s

proc u32_repr(pre: pointer): string {.cdecl.} =
  let s = extractRef[seq[Rune]](pre)
  return $(s)

proc rich_repr(pre: pointer): string {.cdecl.} =
  let s = extractRef[Rope](pre)
  return s.toUtf8()


proc str_eq(a, b: pointer): bool {.cdecl.} =
  let
    x = extractRef[string](a)
    y = extractRef[string](b)

  return x == y

proc u32_eq(a, b: pointer): bool {.cdecl.} =
  let
    x = extractRef[seq[Rune]](a)
    y = extractRef[seq[Rune]](b)

  return x == y

# For now, we just use the 'by value' cmp, which is really by ref for
# Rich.
# proc rich_eq(a, b: pointer): bool {.cdecl.} =

proc str_lt(a, b: pointer): bool {.cdecl.} =
  let
    x = extractRef[string](a)
    y = extractRef[string](b)

  return x < y

proc str_gt(a, b: pointer): bool {.cdecl.} =
  let
    x = extractRef[string](a)
    y = extractRef[string](b)

  return x > y

proc u32_lt(a, b: pointer): bool {.cdecl.} =
  ## Todo... faster comparisons here.
  let
    x = extractRef[seq[Rune]](a)
    y = extractRef[seq[Rune]](b)

  return $(x) < $(y)

proc u32_gt(a, b: pointer): bool {.cdecl.} =
  let
    x = extractRef[seq[Rune]](a)
    y = extractRef[seq[Rune]](b)

  return $(x) > $(y)

proc rich_pluseq(a, b: pointer): void {.cdecl.} =
  var
    x = extractRef[Rope](a)
    y = extractRef[Rope](b)

  x += y

proc str_index(a: pointer, b: int, err: var bool): pointer {.cdecl.} =
  var x = extractRef[string](a)

  if b < 0 or b >= x.len():
    err = true
    return nil

  return cast[pointer](int64(x[b]))

proc u32_index(a: pointer, b: int, err: var bool): pointer {.cdecl.} =
  var x = extractRef[seq[Rune]](a)

  if b < 0 or b >= x.len():
    err = true
    return nil

  return cast[pointer](int64(x[b]))


proc str_len(p: pointer): int {.cdecl.} =
  let s = extractRef[string](p)
  return s.len()

proc u32_len(p: pointer): int {.cdecl.} =
  let s = extractRef[seq[Rune]](p)
  return s.len()

proc rich_len(p: pointer): int {.cdecl.} =
  let r = extractRef[string](p)
  return r.runeLength()

strOps[FRepr]       = cast[pointer](str_repr)
strOps[FCastFn]     = cast[pointer](get_cast_from_string)
strOps[FEq]         = cast[pointer](str_eq)
strOps[FLt]         = cast[pointer](str_lt)
strOps[FGt]         = cast[pointer](str_gt)
strOps[FAdd]        = cast[pointer](str_add)
strOps[FIndex]      = cast[pointer](str_index)
strOps[FSlice]      = cast[pointer](str_slice)
strOps[FNewLit]     = cast[pointer](new_str_lit)
strOps[FCopy]       = cast[pointer](str_copy)
strOps[FLen]        = cast[pointer](str_len)
bufOps[FRepr]       = cast[pointer](str_repr)
bufOps[FCastFn]     = cast[pointer](get_cast_from_string)
bufOps[FEq]         = cast[pointer](str_eq)
bufOps[FLt]         = cast[pointer](str_lt)
bufOps[FGt]         = cast[pointer](str_gt)
bufOps[FAdd]        = cast[pointer](buf_add)
bufOps[FIndex]      = cast[pointer](str_index)
bufOps[FSlice]      = cast[pointer](buf_slice)
bufOps[FNewLit]     = cast[pointer](new_buf_lit)
bufOps[FCopy]       = cast[pointer](buf_copy)
bufOps[FLen]        = cast[pointer](str_len)
utf32Ops[FRepr]     = cast[pointer](u32_repr)
utf32Ops[FCastFn]   = cast[pointer](get_cast_from_u32)
utf32Ops[FEq]       = cast[pointer](u32_eq)
utf32Ops[FLt]       = cast[pointer](u32_lt)
utf32Ops[FGt]       = cast[pointer](u32_gt)
utf32Ops[FAdd]      = cast[pointer](u32_add)
utf32Ops[FIndex]    = cast[pointer](u32_index)
utf32Ops[FSlice]    = cast[pointer](u32_slice)
utf32Ops[FNewLit]   = cast[pointer](new_u32_lit)
utf32Ops[FCopy]     = cast[pointer](u32_copy)
utf32Ops[FLen]      = cast[pointer](u32_len)
richOps[FRepr]      = cast[pointer](rich_repr)
richOps[FCastFn]    = cast[pointer](get_cast_from_rich)
richOps[FEq]        = cast[pointer](value_eq)
richOps[FAdd]       = cast[pointer](rich_add)
richOps[FNewLit]    = cast[pointer](new_rich_lit)
richOps[FCopy]      = cast[pointer](rich_copy)
richOps[FLen]       = cast[pointer](rich_len)
richOps[FPlusEqRef] = cast[pointer](rich_pluseq)

let
  TString* = addDataType(name = "string", concrete = true, ops = strOps)
  TBuffer* = addDataType(name = "buffer", concrete = true, ops = bufOps)
  TUtf32*  = addDataType(name = "utf32",  concrete = true, ops = utf32Ops)
  TRich*   = addDataType(name = "rich",   concrete = true, ops = richOps)

registerSyntax(TString, STStrQuotes, @["u", "u8"], primary = true)
registerSyntax(TBuffer, STStrQuotes, @["bin"])
registerSyntax(TUtf32,  STStrQuotes, @["u32"])
registerSyntax(TRich,   STStrQuotes, @["r", "md", "html", "h1", "h2", "h3",
                                       "h4", "h5", "h6", "p", "em", "i", "b",
                                       "strong", "underline", "pre", "code",
                                       "inv"])

proc str_add(a, b: pointer): pointer =
  let
    x = extractRef[string](a)
    y = extractRef[string](b)

  return newRefValue[string](x & y, TString)

proc buf_add(a, b: pointer): pointer =
  let
    x = extractRef[string](a)
    y = extractRef[string](b)

  return newRefValue[string](x & y, TBuffer)

proc cast_str_to_u32(pre: pointer): pointer =
  let s = extractRef[string](pre)

  return newRefValue[seq[Rune]](s.toRunes(), TUtf32)

proc cast_str_to_rich(pre: pointer): pointer =
  let s = extractRef[string](pre)

  return newRefValue[Rope](text(s), TRich)

proc cast_u32_to_rich(pre: pointer): pointer =
  let runes = extractRef[seq[Rune]](pre)

  return newRefValue[Rope](text(`$`(runes)), TRich)

proc cast_u32_to_u8(pre: pointer): pointer =
  let s = extractRef[seq[Rune]](pre)

  return newRefValue[string](`$`(s), TBuffer)

proc cast_u32_to_str(pre: pointer): pointer =
  let s = extractRef[seq[Rune]](pre)

  return newRefValue[string](`$`(s), TString)

proc cast_rich_to_u32(pre: pointer): pointer =
  let
    r = extractRef[Rope](pre)
    s = r.toUtf8(r.runeLength())
    u = s.toRunes()

  return newRefValue[seq[Rune]](u, TUtf32)

proc cast_rich_to_u8(pre: pointer): pointer =
  let
    r = extractRef[Rope](pre)
    s = r.toUtf8(r.runeLength())

  return newRefValue[string](s, TBuffer)

proc cast_rich_to_str(pre: pointer): pointer =
  let
    r = extractRef[Rope](pre)
    s = r.toUtf8(r.runeLength())

  return newRefValue[string](s, TString)

proc get_cast_from_string(dt: DataType, err: var string): pointer =
  if dt.dtid == TUtf32:
    return cast[pointer](cast_str_to_u32)
  elif dt.dtid == TRich:
    return cast[pointer](cast_str_to_rich)
  elif dt.dtid in [TString, TBuffer]:
    return cast[pointer](cast_identity)
  elif dt.dtid == TBool:
    return cast[pointer](cast_to_bool)


proc get_cast_from_u32(dt: Datatype, err: var string): pointer =
  if dt.dtid == TUtf32:
    return cast[pointer](cast_identity)
  elif dt.dtid == TRich:
    return cast[pointer](cast_u32_to_rich)
  elif dt.dtid == TBuffer:
    return cast[pointer](cast_u32_to_u8)
  elif dt.dtid == TString:
    return cast[pointer](cast_u32_to_str)
  elif dt.dtid == TBool:
    return cast[pointer](cast_to_bool)

proc get_cast_from_rich(dt: Datatype, err: var string): pointer =
  if dt.dtid == TUtf32:
    result = cast[pointer](cast_rich_to_u32)
    err    = "LoseFormat"
  elif dt.dtid == TRich:
    result = cast[pointer](cast_identity)
  elif dt.dtid == TBuffer:
    result = cast[pointer](cast_rich_to_u8)
    err    = "LoseFormat"
  elif dt.dtid == TString:
    result = cast[pointer](cast_rich_to_str)
    err    = "LoseFormat"
  elif dt.dtid == TBool:
    result = cast[pointer](cast_to_bool)
    err    = "LoseFormat"

proc u32_add(a, b: pointer): pointer =
  let
    x = extractRef[seq[Rune]](a)
    y = extractRef[seq[Rune]](b)

  return newRefValue[seq[Rune]](x & y, TUtf32)

proc rich_add(a, b: pointer): pointer =
  let
    x = extractRef[Rope](a)
    y = extractRef[Rope](b)

  return newRefValue[Rope](x + y, TRich)


proc str_slice(a: pointer, b, c: int): pointer =
  let
    x = extractRef[string](a)
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

  return newRefValue[string](x[b .. c], TString)

proc buf_slice(a: pointer, b, c: int, err: var bool): pointer =
  let
    x = extractRef[string](a)
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

  return newRefValue[string](x[b .. c], TBuffer)

proc u32_slice(a: pointer, b, c: int, err: var bool): pointer =
  let
    x = extractRef[seq[Rune]](a)
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

  return newRefValue[seq[Rune]](x[b .. c], TUtf32)

proc new_str_lit(s: string, st: SyntaxType, lmod: string,
                 err: var string): pointer =
  return newRefValue[string](s, TString)

proc new_buf_lit(s: string, st: SyntaxType, lmod: string,
                 err: var string): pointer =
  return newRefValue[string](s, TBuffer)

proc new_u32_lit(s: string, st: SyntaxType, lmod: string,
                 err: var string): pointer =
  return newRefValue[seq[Rune]](s.toRunes(), TUtf32)

proc new_rich_lit(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer =
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

  return newRefValue[Rope](r, TRich)

proc str_copy(p: pointer): pointer =
  let s = extractRef[string](p)
  return newRefValue[string](s, TString)

proc buf_copy(p: pointer): pointer =
  let s = extractRef[string](p)
  return newRefValue[string](s, TBuffer)

proc u32_copy(p: pointer): pointer =
  let s = extractRef[seq[Rune]](p)
  return newRefValue[seq[Rune]](s, TUtf32)

proc rich_copy(p: pointer): pointer =
  let r = extractRef[Rope](p)
  return newRefValue[Rope](r.copy(), TRich)
