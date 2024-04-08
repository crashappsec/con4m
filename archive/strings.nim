import "."/[base, ordinals, marshal]

proc tList(item: TypeId): TypeId {.importc, cdecl.}

let richLitMods = @["r", "md", "html", "h1", "h2", "h3",
                    "h4", "h5", "h6", "p", "em", "i", "b",
                    "strong", "underline", "pre", "code",
                    "inv"]
var
  strOps   = newVTable()
  bufOps   = newVTable()
  utf32Ops = newVTable()
  richOps  = newVTable()

proc str_new_lit(s: string, st: SyntaxType, lmod: string, err: var string):
                Rich {.cdecl.} =
  return newRich(s)

proc u32_new_lit(s: string, st: SyntaxType, lmod: string, err: var string):
                Rich {.cdecl.} =
  # Going to get rid of this as a separate data type.
  return newRich(s)

proc rich_new_lit(s: string, st: SyntaxType, lmod: string, err: var string):
                 Rope {.cdecl.} =
  case lmod
  of "md":
    result = markdown(s)
  of "html":
    result = html(s)
  of "h1":
    result = h1(s)
  of "h2":
    result = h2(s)
  of "h3":
    result = h3(s)
  of "h4":
    result = h4(s)
  of "h5":
    result = h5(s)
  of "h6":
    result = h6(s)
  of "p":
    result = paragraph(s)
  of "em":
    result = em(s)
  of "i":
    result = italic(s)
  of "b":
    result = bold(s)
  of "strong":
    result = strong(s)
  of "underline":
    result = underline(s)
  of "pre":
    result = pre(s)
  of "code":
    result = code(s)
  of "inv":
    result = inverse(s)
  else:
    result = atom(s)

  GC_ref(result)

proc u32_repr(pre: pointer): string {.cdecl.} =
  let s = cast[Rich](pre)

  if s == nil:
    return ""
  else:
    return s.toNimStr()

proc str_repr(n: Rich): Rich {.cdecl.} =
  return n

proc rich_repr(s: Rope): Rich {.cdecl.} =
  return newRich(s.toUtf8())

proc rich_pluseq(a: pointer, b: Rope): void {.cdecl.} =
  # I *think* I can declare `a` a `var Rope` here, but just in case.
  var a = cast[Rope](a)

  a += b

proc rich_add(a, b: Rope): Rope {.cdecl.} =
  return a + b

proc str_index(a: Rich, b: int, err: var bool): int64 {.cdecl.} =
  var x = cast[cstring](a)

  if b < 0 or b >= x.len():
    err = true
    return 0

  return int64(x[b])

proc u32_index(a: pointer, b: int, err: var bool): int64 {.cdecl.} =
  # Pointer is to the char* part of a Riching.
  let bytelen = b * 4

  if bytelen < 0 or bytelen >= string_codepoints(cast[Rich](a)):
    err = true
    return 0

  let p = cast[ptr Rune](cast[uint64](a) + uint64(bytelen))

  return int64(p[])

proc rich_len(p: pointer): int {.cdecl.} =
  let r = cast[string](p)
  return r.runeLength()

proc u32_len(s: Rich): int {.cdecl.} =
  return s.len() div 4

proc rich_copy(r: Rope): Rope {.cdecl, exportc.} =
  result = r.copy()
  GC_ref(result)

proc cast_str_to_u32(pre: Rich, tfrom, tto: TypeId, err: var string):
                    Rich {.cdecl, exportc.} =
  let
    l = pre.len()
    s = `$`(cast[cstring](pre)).toRunes()

  result = newRich(l * 4)

  if l != 0:
    copyMem(cast[pointer](result), addr s[0], l)

proc cast_str_to_rich(pre: pointer, tfrom, tto: TypeId,
                      err: var string): pointer {.cdecl, exportc.} =

  let s = $(cast[cstring](pre))
  var rope = text(s)

  GC_ref(rope)

  return cast[pointer](rope)

proc nim_str_to_con4m(s: string, tid: TypeId): Rich =
  result = newRich(s)

proc nim_str_to_con4m_u32(s: string, tid: TypeId): Rich =
  if s.len() == 0:
    return newRich("")
  result = newRich(s.len() * 4)
  let u32 = s.toRunes()
  copyMem(result, addr u32[0], s.len() * 4)

proc cast_u32_to_rich(pre: Rich, tfrom, tto: TypeId, err: var string):
                     Rope {.cdecl, exportc.} =
  var
    l = (pre.len() div 4) * 4
    s = newSeq[Rune](int(l))

  if l != 0:
    copyMem(addr s[0], cast[pointer](pre), l)

  result = text($(s))
  GC_ref(result)

proc cast_u32_to_str(pre: Rich, tfrom, tto: TypeId, err: var string):
                    Rich {.cdecl, exportc.} =
  var
    l = (pre.len() div 4) * 4
    s = newSeq[Rune](int(l))

  if l != 0:
    copyMem(addr s[0], cast[pointer](pre), l)

  result = newRich($s)

proc cast_rich_to_u32(r: Rope, tfrom, to: TypeId, err: var string):
                     Rich {.cdecl, exportc.} =
  let
    s = r.toUtf8(r.runeLength())
    u = s.toRunes()
    l = u.len() * 4

  result = newRich(l)
  if l != 0:
    copyMem(cast[pointer](result), addr u[0], l)

proc cast_rich_to_str(r: Rope, tfrom, tto: TypeId, err: var string):
                     Rich {.cdecl, exportc.} =
  return newRich(r.toUtf8(r.runeLength()))

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

  return newRich(`$`(x)[b .. c])

proc u32_slice(a: pointer, b, c: int, err: var bool): pointer =
  return str_slice(a, b * 4, c * 4, err)

# These need to be fw referenced as we need TString, etc. defined first.
proc get_cast_from_string(dt: DataType, t1, t2: TypeId,
                          err: var string): pointer
proc get_cast_from_u32(dt: Datatype, t1, t2: TypeId,
                       err: var string): pointer
proc get_cast_from_rich(dt: Datatype, t1, t2: TypeId,
                        err: var string): pointer

proc str_marshal(s: Rich, t: TypeId, memos: Memos): Rich {.exportc, cdecl.} =
  if s == nil:
    return marshal_32_bit_value(0)

  let
    str_len   = int32(s.len())
    total_len = str_len + sizeof(int32)

  result = newRich(total_len)
  c4str_write_offset(result, marshal_32_bit_value(str_len), 0)
  c4str_write_offset(result, s, sizeof(int32))

proc str_unmarshal(s: var cstring, t: TypeId, memos: Memos):
                  Rich {.exportc, cdecl.} =

  let
    l = s.unmarshal_32_bit_value()

  if l == 0:
    return nil

  result = newRich(l)
  copyMem(cast[pointer](result), addr s[0], l)
  pointer_add(s, l)

proc marshal_style(s: FmtStyle): Rich =
  var
    flags:    int64
    toConcat: seq[Rich]
    num:      int32

  if s.isNil:
    return marshal_64_bit_value(cast[pointer](flags))

  if s.textColor.isSome():
    flags = flags or 0x00000001
    toConcat.add(marshal_nim_string(s.textColor.get()))
  if s.bgColor.isSome():
    flags = flags or 0x00000002
    toConcat.add(marshal_nim_string(s.bgColor.get()))
  if s.overflow.isSome():
    flags = flags or 0x00000004
    num   = cast[int32](s.overflow.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.hang.isSome():
    flags = flags or 0x00000008
    num   = cast[int32](s.hang.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.lpad.isSome():
    flags = flags or 0x00000010
    num   = cast[int32](s.lpad.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.rpad.isSome():
    flags = flags or 0x00000020
    num   = cast[int32](s.rpad.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.tpad.isSome():
    flags = flags or 0x00000040
    num   = cast[int32](s.tpad.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.bpad.isSome():
    flags = flags or 0x00000080
    num   = cast[int32](s.lpad.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.casing.isSome():
    flags = flags or 0x00000100
    num   = cast[int32](s.casing.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.bold.isSome():
    flags = flags or 0x00000200
    if s.bold.get():
      flags = flags or 0x00000400
  if s.inverse.isSome():
    flags = flags or 0x00000800
    if s.inverse.get():
      flags = flags or 0x00001000
  if s.strikethrough.isSome():
    flags = flags or 0x00002000
    if s.strikethrough.get():
      flags = flags or 0x00004000
  if s.italic.isSome():
    flags = flags or 0x00008000
    if s.italic.get():
      flags = flags or 0x00010000
  if s.useTopBorder.isSome():
    flags = flags or 0x00020000
    if s.useTopBorder.get():
      flags = flags or 0x00040000
  if s.useBottomBorder.isSome():
    flags = flags or 0x00080000
    if s.useBottomBorder.get():
      flags = flags or 0x00100000
  if s.useLeftBorder.isSome():
    flags = flags or 0x00200000
    if s.useLeftBorder.get():
      flags = flags or 0x00400000
  if s.useRightBorder.isSome():
    flags = flags or 0x00800000
    if s.useRightBorder.get():
      flags = flags or 0x01000000
  num = int32(s.bulletChar.get(Rune(0)))
  toConcat.add(marshal_32_bit_value(num))
  if s.underlineStyle.isSome():
    flags = flags or 0x02000000
    num   = cast[int32](s.underlineStyle.get())
    toConcat.add(marshal_32_bit_value(num))
  if s.boxStyle.isSome():
    flags = flags or 0x04000000
    let bs  = s.boxStyle.get()
    let str = $(bs.horizontal) & $(bs.vertical)  & $(bs.upperLeft)  &
              $(bs.upperRight) & $(bs.lowerLeft) & $(bs.lowerRight) &
              $(bs.cross)      & $(bs.topT)      & $(bs.bottomT)    &
              $(bs.leftT)      & $(bs.rightT)
    toConcat.add(marshal_nim_string(str))

  if s.alignStyle.isSome():
    flags = flags or 0x08000000
    num   = cast[int32](s.alignStyle.get())
    toConcat.add(marshal_32_bit_value(num))

  let flagfield = marshal_64_bit_value(cast[pointer](flags))

  var
    total_len = flagfield.len()
    offset    = total_len

  for item in toConcat:
    total_len += item.len()

  result = newRich(total_len)

  copyMem(cast[pointer](result), cast[pointer](flagfield), sizeof(int64))

  for item in to_concat:
    c4str_write_offset(result, item, offset)
    offset += item.len()

proc rope_marshal*(r: Rope, t: TypeId, memos: Memos): Rich {.exportc, cdecl.} =
  if r == nil:
    return marshal_64_bit_value(nil)

  basic_marshal_helper:
    let
      as_ptr  = cast[pointer](r)
      memo_opt = memos.map.lookup(as_ptr)

    if memo_opt.isSome():
      return marshal_64_bit_value(memo_opt.get())

    let memo = memos.next_id
    memos.map[as_ptr] = cast[pointer](memo)
    memos.next_id    += 1

    toAdd.add(marshal_64_bit_value(cast[pointer](memo)))
    toAdd.add(marshal_32_bit_value(cast[int32](r.kind)))
    toAdd.add(marshal_32_bit_value(cast[int32](r.siblings.len())))

    for item in r.siblings:
      toAdd.add(item.rope_marshal(t, memos))

    toAdd.add(marshal_nim_string(r.id))
    toAdd.add(marshal_nim_string(r.tag))
    toAdd.add(marshal_nim_string(r.class))
    toAdd.add(marshal_style(r.style))
    toAdd.add(marshal_style(r.tweak))

    case r.kind
    of RopeAtom:
      toAdd.add(marshal_32_bit_value(int32(r.text.len())))
      for i in 0 ..< r.text.len():
        toAdd.add(marshal_32_bit_value(int32(r.text[i])))

    of RopeBreak:
      toAdd.add(marshal_32_bit_value(int32(r.breakType)))
      toAdd.add(r.guts.rope_marshal(t, memos))

    of RopeLink:
      toAdd.add(marshal_nim_string(r.url))
      toAdd.add(r.toHighlight.rope_marshal(t, memos))

    of RopeList:
      toAdd.add(marshal_32_bit_value(int32(r.items.len())))
      for item in r.items:
        toAdd.add(item.rope_marshal(t, memos))

    of RopeTaggedContainer:
      toAdd.add(r.contained.rope_marshal(t, memos))
      toAdd.add(marshal_32_bit_value(int32(r.width)))

    of RopeTable:
      toAdd.add(marshal_32_bit_value(int32(r.colInfo.len())))
      for item in r.colInfo:
        toAdd.add(marshal_32_bit_value(int32(item.wValue)))
        toAdd.add(marshal_bool(item.absVal))

      toAdd.add(r.thead.rope_marshal(t, memos))
      toAdd.add(r.tbody.rope_marshal(t, memos))
      toAdd.add(r.tfoot.rope_marshal(t, memos))
      toAdd.add(r.title.rope_marshal(t, memos))
      toAdd.add(r.caption.rope_marshal(t, memos))

    of RopeTableRow, RopeTableRows:
      toAdd.add(marshal_32_bit_value(int32(r.cells.len())))
      for item in r.cells:
        toAdd.add(item.rope_marshal(t, memos))

    of RopeFgColor, RopeBgColor:
      toAdd.add(marshal_nim_string(r.color))
      toAdd.add(r.toColor.rope_marshal(t, memos))

proc unmarshal_style(s: var cstring): FmtStyle {.exportc, cdecl.} =
  let flags = s.unmarshal_64_bit_value()

  if flags == 0:
    return

  result = FmtStyle()

  if (flags and 0x00000001) != 0:
    result.textColor = some(s.unmarshal_nim_string())
  if (flags and 0x00000002) != 0:
    result.bgColor   = some(s.unmarshal_nim_string())
  if (flags and 0x00000004) != 0:
    result.overflow  = some(cast[OverflowPreference](s.unmarshal_32_bit_value))
  if (flags and 0x00000008) != 0:
    result.hang = some(cast[int](s.unmarshal_32_bit_value()))
  if (flags and 0x00000010) != 0:
    result.lpad = some(cast[int](s.unmarshal_32_bit_value()))
  if (flags and 0x00000020) != 0:
    result.rpad = some(cast[int](s.unmarshal_32_bit_value()))
  if (flags and 0x00000040) != 0:
    result.tpad = some(cast[int](s.unmarshal_32_bit_value()))
  if (flags and 0x00000080) != 0:
    result.bpad = some(cast[int](s.unmarshal_32_bit_value()))
  if (flags and 0x00000100) != 0:
    result.casing = some(cast[TextCasing](s.unmarshal_32_bit_value()))
  if (flags and 0x00000200) != 0:
    result.bold = some(if (flags and 0x00000400) != 0: true else: false)
  if (flags and 0x00000800) != 0:
    result.inverse = some(if (flags and 0x00001000) != 0: true else: false)
  if (flags and 0x00002000) != 0:
    result.strikethrough = some(if (flags and 0x00004000) != 0: true
                                else: false)
  if (flags and 0x00008000) != 0:
    result.italic = some(if (flags and 0x00010000) != 0: true else: false)
  if (flags and 0x00020000) != 0:
    result.useTopBorder = some(if (flags and 0x00040000) != 0: true else: false)
  if (flags and 0x00080000) != 0:
    result.useBottomBorder = some(if (flags and 0x00100000) != 0: true
                                  else: false)
  if (flags and 0x00200000) != 0:
    result.useLeftBorder = some(if (flags and 0x00400000) != 0: true
                                else: false)
  if (flags and 0x00800000) != 0:
    result.useRightBorder = some(if (flags and 0x01000000) != 0: true
                                 else: false)
  let r = s.unmarshal_32_bit_value()
  if r != 0:
    result.bulletChar = some(Rune(r))
  if (flags and 0x02000000) != 0:
    result.underlineStyle = some(cast[UnderlineStyle](s.unmarshal_32_bit_value))
  if (flags and 0x04000000) != 0:
    let
      bstr = s.unmarshal_nim_string().toRunes()
      box  = BoxStyle()

    box.horizontal = bstr[0]
    box.vertical   = bstr[1]
    box.upperLeft  = bstr[2]
    box.upperRight = bstr[3]
    box.lowerLeft  = bstr[4]
    box.lowerRight = bstr[5]
    box.cross      = bstr[6]
    box.topT       = bstr[7]
    box.bottomT    = bstr[8]
    box.leftT      = bstr[9]
    box.rightT     = bstr[10]

    result.boxStyle = some(box)
  if (flags and 0x08000000) != 0:
    result.alignStyle = some(cast[AlignStyle](s.unmarshal_32_bit_value()))

  GC_ref(result)

proc rope_unmarshal*(p: var cstring, t: TypeId, memos: Memos): Rope
    {.exportc, cdecl.} =

  var
    memo = p.unmarshal_64_bit_value()

  if memo == 0:
    return nil

  let ropeOpt = memos.map.lookup(cast[pointer](memo))
  if ropeOpt.isSome():
     result = cast[Rope](ropeOpt.get())

  result = Rope(kind: cast[RopeKind](p.unmarshal_32_bit_value()))
  GC_ref(result)

  memos.map[cast[pointer](memo)] = cast[pointer](result)

  for i in 0 ..< p.unmarshal_32_bit_value():
    result.siblings.add(p.rope_unmarshal(t, memos))

  result.id    = p.unmarshal_nim_string()
  result.tag   = p.unmarshal_nim_string()
  result.class = p.unmarshal_nim_string()
  result.style = p.unmarshal_style()
  result.tweak = p.unmarshal_style()

  case result.kind
  of RopeAtom:
    result.length = p.unmarshal_32_bit_value()
    for i in 0 ..< result.length:
      result.text.add(Rune(p.unmarshal_32_bit_value()))

  of RopeBreak:
    result.breakType = cast[BreakKind](p.unmarshal_32_bit_value())
    result.guts      = p.rope_unmarshal(t, memos)

  of RopeLink:
    result.url         = p.unmarshal_nim_string()
    result.toHighlight = p.rope_unmarshal(t, memos)

  of RopeList:
    for i in 0 ..< p.unmarshal_32_bit_value():
      result.items.add(p.rope_unmarshal(t, memos))

  of RopeTaggedContainer:
    result.contained = p.rope_unmarshal(t, memos)
    result.width     = p.unmarshal_32_bit_value()

  of RopeTable:
    for i in 0 ..< p.unmarshal_32_bit_value():
      result.colInfo.add(ColInfo(wValue: p.unmarshal_32_bit_value(),
                            absVal: p.unmarshal_bool()))

    result.thead   = p.rope_unmarshal(t, memos)
    result.tbody   = p.rope_unmarshal(t, memos)
    result.tfoot   = p.rope_unmarshal(t, memos)
    result.title   = p.rope_unmarshal(t, memos)
    result.caption = p.rope_unmarshal(t, memos)

  of RopeTableRow, RopeTableRows:
    for i in 0 ..< p.unmarshal_32_bit_value():
      result.cells.add(p.rope_unmarshal(t, memos))

  of RopeFgColor, RopeBgColor:
    result.color   = p.unmarshal_nim_string()
    result.toColor = p.rope_unmarshal(t, memos)



strOps[FRepr]         = cast[pointer](str_repr)
strOps[FCastFn]       = cast[pointer](get_cast_from_string)
strOps[FEq]           = cast[pointer](c4str_eq)
strOps[FLt]           = cast[pointer](c4str_lt)
strOps[FGt]           = cast[pointer](c4str_gt)
strOps[FAdd]          = cast[pointer](c4str_add)
strOps[FIndex]        = cast[pointer](str_index)
strOps[FSlice]        = cast[pointer](str_slice)
strOps[FNewLit]       = cast[pointer](str_new_lit)
strOps[FCopy]         = cast[pointer](c4str_copy)
strOps[FLen]          = cast[pointer](c4str_len)
strOps[FMarshal]      = cast[pointer](str_marshal)
strOps[FUnmarshal]    = cast[pointer](str_unmarshal)
strOps[FFromNim]      = cast[pointer](nim_str_to_con4m)
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
bufOps[FMarshal]      = cast[pointer](str_marshal)
bufOps[FUnmarshal]    = cast[pointer](str_unmarshal)
bufOps[FFromNim]      = cast[pointer](nim_str_to_con4m)
utf32Ops[FRepr]       = cast[pointer](u32_repr)
utf32Ops[FCastFn]     = cast[pointer](get_cast_from_u32)
utf32Ops[FEq]         = cast[pointer](c4str_eq)
utf32Ops[FLt]         = cast[pointer](c4str_lt)
utf32Ops[FGt]         = cast[pointer](c4str_gt)
utf32Ops[FAdd]        = cast[pointer](c4str_add)
utf32Ops[FIndex]      = cast[pointer](u32_index)
utf32Ops[FSlice]      = cast[pointer](u32_slice)
utf32Ops[FNewLit]     = cast[pointer](u32_new_lit)
utf32Ops[FCopy]       = cast[pointer](c4str_copy)
utf32Ops[FLen]        = cast[pointer](u32_len)
utf32Ops[FMarshal]    = cast[pointer](str_marshal)
utf32Ops[FUnmarshal]  = cast[pointer](str_unmarshal)
utf32Ops[FFromNim]    = cast[pointer](nim_str_to_con4m_u32)
richOps[FRepr]        = cast[pointer](rich_repr)
richOps[FCastFn]      = cast[pointer](get_cast_from_rich)
richOps[FEq]          = cast[pointer](value_eq)
richOps[FAdd]         = cast[pointer](rich_add)
richOps[FNewLit]      = cast[pointer](rich_new_lit)
richOps[FCopy]        = cast[pointer](rich_copy)
richOps[FLen]         = cast[pointer](rich_len)
richOps[FPlusEqRef]   = cast[pointer](rich_pluseq)
richOps[FMarshal]     = cast[pointer](rope_marshal)
richOps[FUnmarshal]   = cast[pointer](rope_unmarshal)

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

proc str_strip*(s1: Rich): Rich {.exportc, cdecl.} =
  newRich(unicode.strip(s1.toNimStr()))

addStaticFunction("str_strip", str_strip)

proc str_contains*(s1: Rich, s2: Rich): bool {.exportc, cdecl.} =
  s1.toNimStr().contains(s2.toNimStr())

addStaticFunction("str_contains", str_contains)

proc str_starts_with(s1: Rich, s2: Rich): bool {.exportc, cdecl.} =
  s1.toNimStr().startswith(s2.toNimStr())

addStaticFunction("str_starts_with", str_starts_with)

proc str_ends_with(s1: Rich, s2: Rich): bool {.exportc, cdecl.} =
  s1.toNimStr().endswith(s2.toNimStr())

addStaticFunction("str_ends_with", str_ends_with)

proc str_find(s1: Rich, s2: Rich): int {.exportc, cdecl.} =
  s1.toNimStr().find(s2.toNimStr())

addStaticFunction("str_find", str_find)
addStaticFunction("str_len", c4str_len)

import std/base64
proc str_base64(s: Rich): Rich {.exportc, cdecl.} =
  newRich(base64.encode(s.toNimStr()))

addStaticFunction("str_base64", str_base64)

proc str_base64_web(s: Rich): Rich {.exportc, cdecl.} =
  newRich(base64.encode(s.toNimStr(), safe = true))

addStaticFunction("str_base64_web", str_base64_web)

proc str_decode(s: Rich): Rich {.exportc, cdecl.} =
  newRich(base64.decode(s.toNimStr()))

addStaticFunction("str_decode", str_decode)

proc str_to_hex(s: Rich): Rich {.exportc, cdecl.} =
  newRich(s.toNimStr().toHex().toLowerAscii())

addStaticFunction("str_to_hex", str_to_hex)

proc str_to_hex_int(s: int): Rich {.exportc, cdecl.} =
  newRich(s.toHex().toLowerAscii())

addStaticFunction("str_to_hex_int", str_to_hex_int)

proc str_from_hex(s: Rich): Rich {.exportc, cdecl.} =
  try:
    return newRich(s.toNimStr().parseHexStr())
  except:
    return newRich("")

addStaticFunction("str_from_hex", str_from_hex)

proc str_sha256(s: Rich): Rich {.exportc, cdecl.} =
  newRich(s.toNimStr().sha256Hex())

addStaticFunction("str_sha256", str_sha256)

proc str_sha512(s: Rich): Rich {.exportc, cdecl.} =
  newRich(s.toNimStr().sha512Hex())

addStaticFunction("str_sha512", str_sha512)

proc str_upper(s: Rich): Rich {.exportc, cdecl.} =
  newRich(unicode.toUpper(s.toNimStr()))

addStaticFunction("str_upper", str_upper)

proc str_lower(s: Rich): Rich {.exportc, cdecl.} =
  newRich(unicode.toLower(s.toNimStr()))

addStaticFunction("str_lower", str_lower)

proc str_split(s1: Rich, s2: Rich): FlexArray[pointer] {.exportc, cdecl.} =
  let
    pieces = s1.toNimStr().split(s2.toNimStr())

  result = newArray[pointer](pieces.len())

  for i, item in pieces:
    result[i] = newRich(item)

  result.metadata = cast[pointer](tList(TString))

addStaticFunction("str_split", str_split)

proc str_join(s: FlexArray[pointer], joiner: Rich): Rich {.exportc, cdecl.} =
  var
    l: seq[string]

  for item in s.items():
    l.add(cast[Rich](item).toNimStr())

  return newRich(l.join(joiner.toNimStr()))

addStaticFunction("str_join", str_join)

proc str_replace(base, match, replacement: Rich): Rich {.exportc, cdecl.} =
  newRich(replace(base.toNimStr(), match.toNimStr(), replacement.toNimStr()))


addStaticFunction("str_replace", str_replace)

proc str_pad(s: Rich, width: int64): Rich {.exportc, cdecl.} =
  let l = s.len()

  if l >= width:
    return s

  result = newRich(width)
  if l != 0:
    copyMem(cast[pointer](result), cast[pointer](s), l)

  var asC = cast[ptr char](cast[int](result) + l)
  for i in l ..< width:
    asC[] = ' '
    asC   = cast[ptr char](cast[int](asC) + 1)

addStaticFunction("str_pad", str_pad)
