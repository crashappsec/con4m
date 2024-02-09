import "."/base

proc toString(t: TypeId): string {.importc, cdecl.}

proc marshal_64_bit_value*(v: pointer): C4Str =
  result = newC4Str(sizeof(uint64))
  var p = cast[ptr uint64](result)

  p[] = cast[uint64](v)

proc marshal_32_bit_value*(v: int32): C4Str =
  result = newC4Str(sizeof(int32))
  var p = cast[ptr int32](result)

  p[] = cast[int32](v)

proc marshal*(v: pointer, t: TypeId, m: Memos): C4Str {.exportc, cdecl.} =
   let dt = t.get_data_type()

   if dt.ops[FMarshal] == nil:
     return marshal_64_bit_value(v)
   else:
     let fn = cast[MarshalFn](dt.ops[FMarshal])
     return fn(v, t, m)

proc c4m_marshal*(v: pointer, t: TypeId): C4Str {.exportc, cdecl.} =
  var memos = Memos()
  memos.map.initDict()

  result = v.marshal(t, memos)

proc unmarshal*(s: var cstring, t: TypeId, m: Memos): pointer
    {.exportc, cdecl.} =
  let dt = t.get_data_type()

  assert dt.ops[FUnmarshal] != nil

  let fn = cast[UnmarshalFn](dt.ops[FUnmarshal])
  return fn(s, t, m)

proc c4m_unmarshal*(s: var cstring, t: TypeId): pointer =
  var memos = Memos()
  memos.map.initDict()

  return s.unmarshal(t, memos)

proc marshal_nim_string*(s: string): C4Str {.exportc, cdecl.} =
  let
    str_len   = s.len()
    total_len = str_len + 4

  result = newC4Str(total_len)
  var p = cast[ptr int32](result)

  p[] = int32(str_len)

  if str_len != 0:
    let p = cast[pointer](cast[int](cast[pointer](result)) + sizeof(int32))
    copyMem(p, addr s[0], str_len)

proc marshal_64_bit_value_main*(n: uint64, t: TypeId, m: Memos):
                              C4Str {.exportc, cdecl.} =
  result = newC4Str(sizeof(uint64))
  var p = cast[ptr uint64](result)

  p[] = n

proc pointer_add*(v: var cstring, n: int) =
  var asint = cast[int64](cast[pointer](v))
  asint = asint + n
  v = cast[cstring](asint)

proc unmarshal_64_bit_value*(s: var cstring): uint64 {.exportc, cdecl.} =
  var p = cast[ptr uint64](addr s[0])
  result = p[]
  s.pointer_add(8)

proc unmarshal_32_bit_value*(s: var cstring): int32 {.exportc, cdecl.} =
  var p = cast[ptr int32](addr s[0])
  result = p[]
  s.pointer_add(4)

proc unmarshal_nim_string*(s: var cstring): string {.exportc, cdecl.} =
  let
    l = s.unmarshal_32_bit_value()

  if l == 0:
    result = ""
  else:
    for i in 0 ..< l:
      result.add(s[i])

    s.pointer_add(l)
