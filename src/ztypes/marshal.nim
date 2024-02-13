import "."/base

proc marshal_64_bit_value*(v: pointer): C4Str =
  result = newC4Str(sizeof(uint64))
  var p = cast[ptr uint64](result)

  p[] = cast[uint64](v)

proc marshal_32_bit_value*(v: int32): C4Str =
  result = newC4Str(sizeof(int32))
  var p = cast[ptr int32](result)

  p[] = cast[int32](v)

proc marshal_8_bit_value*(v: uint8 | int8): C4Str =
  result = newC4Str(1)
  var p = cast[ptr uint8](result)

  p[] = uint8(v)

proc marshal_16_bit_value*(v: uint16 | int16): C4Str =
  result = newC4Str(sizeof(uint16))
  var p = cast[ptr UncheckedArray[uint8]](result)

  p[0] = uint8(v shr 8)
  p[1] = uint8(v and 0xff)

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

proc unmarshal_8_bit_value*(s: var cstring): int8 {.exportc, cdecl.} =
  var p = cast[ptr int8](addr s[0])
  result = p[]
  s.pointer_add(1)

proc unmarshal_16_bit_value*(s: var cstring): int16 {.exportc, cdecl.} =
  result = int16(s.unmarshal_8_bit_value()) shl 8
  result = result or s.unmarshal_8_bit_value()


proc unmarshal_bool*(s: var cstring): bool {.exportc, cdecl.} =
  return s.unmarshal_8_bit_value() == ord('y')

proc unmarshal_nim_string*(s: var cstring): string {.exportc, cdecl.} =
  let
    l = s.unmarshal_32_bit_value()

  if l == 0:
    result = ""
  else:
    for i in 0 ..< l:
      result.add(s[i])

    s.pointer_add(l)

proc marshal_bool*(v: bool): C4Str {.exportc, cdecl.} =
  # Ascii printables for easier debugging
  if v:
    return newC4Str("y")
  else:
    return newC4Str("n")

template basic_marshal_helper*(base_code: untyped) {.dirty.} =
  var
    toAdd: seq[C4Str]
    sum    = 0
    offset = 0

  base_code

  for item in toAdd:
    sum += item.len()

  result = newC4Str(sum)

  for item in toAdd:
    c4str_write_offset(result, item, offset)
    offset += item.len()

template list_marshal_helper*(view, code: untyped) {.dirty.} =

  basic_marshal_helper:
    toAdd.add(view.len().int32().marshal_32_bit_value())
    for item in view:
      code

template list_unmarshal_helper*(s: var cstring, code: untyped) {.dirty.} =
  let count = s.unmarshal_32_bit_value()

  for ix in 0 ..< count:
    code

template view_marshal_helper*(view, code: untyped) {.dirty.} =
  basic_marshal_helper:
    toAdd.add(view.len().int32().marshal_32_bit_value())
    for (k, v) in view:
      code

template dictionary_marshal_helper*(dictparam, code: untyped) {.dirty.} =
  let param_item_list = dictparam.items(sort=true)
  view_marshal_helper(param_item_list, code)


proc marshal_nimstr_list*(l: seq[string]): C4Str =
  list_marshal_helper(l):
    toAdd.add(item.marshal_nim_string())

proc unmarshal_nimstr_list*(p: var cstring): seq[string] =
  list_unmarshal_helper(p):
    result.add(p.unmarshal_nim_string())
