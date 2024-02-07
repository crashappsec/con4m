import "."/base

proc marshal*(v: pointer, t: TypeId, m: Memos): C4Str {.exportc, cdecl.} =
   let dt = t.get_data_type()

   if dt.ops[FMarshal] == nil:
     return nil # Cannot marshal; will generally get skipped.
   else:
     let fn = cast[MarshalFn](dt.ops[FMarshal])
     return fn(v, t, m)

proc c4m_marshal*(v: pointer, t: TypeId): C4Str {.exportc, cdecl.} =
  var memos = Memos()
  memos.map.initDict()

  return v.marshal(t, memos)

proc unmarshal*(s: cstring, t: TypeId, m: Memos): pointer {.exportc, cdecl.} =
  let dt = t.get_data_type()

  assert dt.ops[FUnmarshal] != nil

  let fn = cast[UnmarshalFn](dt.ops[FUnmarshal])
  return fn(s, t, m)

proc c4m_unmarshal*(s: C4Str, t: TypeId): pointer {.exportc, cdecl.} =
  var memos = Memos()
  memos.map.initDict()

  return cast[cstring](s).unmarshal(t, memos)

proc marshal_nim_string*(s: string): C4Str {.exportc, cdecl.} =
  let
    str_len   = s.len()
    total_len = str_len + sizeof(int64)

  result = newC4Str(total_len)
  copyMem(cast[pointer](result), cast[pointer](addr str_len), sizeof(int64))
  if str_len != 0:
    let p = cast[pointer](cast[int](cast[pointer](result)) + sizeof(int64))
    copyMem(p, addr s[0], str_len)

proc marshal_64_bit_value_main*(v: pointer, t: TypeId, m: Memos):
                              C4Str {.exportc, cdecl.} =
  result = newC4Str(sizeof(uint64))
  copyMem(cast[pointer](addr v), cast[pointer](result), sizeof(uint64))

proc marshal_64_bit_value*(v: pointer): C4Str =
  result = newC4Str(sizeof(uint64))
  copyMem(cast[pointer](addr v), cast[pointer](result), sizeof(uint64))

proc pointer_add*(v: var cstring, n: int) =
  v = cast[cstring](cast[int64](addr v[0]) + int64(n))

proc unmarshal_64_bit_value*(s: var cstring): uint64 {.exportc, cdecl.} =
  copyMem(cast[pointer](addr result), cast[pointer](addr s[0]), sizeof(uint64))
  s.pointer_add(sizeof(uint64))

proc unmarshal_nim_string*(s: var cstring): string =
  let
    p = cast[ptr int64](addr s[0])
    l = p[]

  s.pointer_add(sizeof(uint64))
  result = newString(l)
  copyMem(addr result[0], addr s[0], l)
  s.pointer_add(l)
