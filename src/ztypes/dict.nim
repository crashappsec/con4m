import "."/[base, tup, marshal]

proc tTuple(l: seq[TypeId]): TypeId {.importc, cdecl.}
proc tList(item: TypeId): TypeId {.importc, cdecl.}

var dictOps = newVTable()

type Con4mDict* = ref object
  obj*: Dict[pointer, pointer]
  t*: TypeId

proc get_cast_fn(tcur, tdst: DataType, tfrom, tto: TypeId, err: var string):
     pointer {.importc, cdecl.}
proc call_cast(v: pointer, tcur, tdst: TypeId, err: var string): pointer {.
                importc, cdecl.}
proc call_copy(p: pointer, t: TypeId): pointer {.importc, cdecl.}
proc isC4StrType(id: TypeId): bool {.importc, cdecl.}

proc newDict*(t: TypeId): Con4mDict =
  var hashApproach: DictKeyType

  let
    to      = t.idToTypeRef()
    keyType = to.items[0].followForwards()

  if keyType.isIntType():
    hashApproach = KtInt
  elif keyType.isFloatType():
    hashApproach = KtFloat
  elif keyType.isC4StrType():
    hashApproach = KtCStr
  else:
    hashApproach = KtPtr

  result     = Con4mDict(t: t)
  result.obj = Dict[pointer, pointer]()

  result.obj.raw = hatrack_dict_new(hashApproach)
  result.obj.raw.hatrack_dict_set_consistent_views(true)

  GC_ref(result)

proc dict_repr(c: pointer): cstring {.exportc, cdecl.} =
  var
    parts: seq[string] = @[]
    dict               = cast[Con4mDict](c)
    tobj               = dict.t.idToTypeRef()
    kType              = tobj.items[0]
    vType              = tobj.items[1]

  for (k, v) in dict.obj.items():
    let
      ks = $(k.call_repr(kType))
      vs = $(v.call_repr(vType))

    parts.add(ks & " : " & vs)

  return cstring("{" & parts.join(", ") & "}")

proc call_eq(v1, v2: pointer, t: TypeId): bool {.importc, cdecl.}

proc dict_eq(v1, v2: Con4mDict, t: TypeId): bool {.exportc, cdecl.} =
  # This is a moment-in-time thing. Generally try not to use it if not
  # single threaded.

  if v1 == v2:
    return true

  let
    tobj  = v1.t.idToTypeRef()
    kType = tobj.items[0]
    vType = tobj.items[1]
    view1 = v1.obj.items()
    view2 = v2.obj.items()

  if view1.len() != view2.len():
    return false

  # While we can ask for views sorted, they're sorted by insertion time,
  # so we'd have to re-sort everything by value. So for now, just
  # do the brute n^2 approach not the n log n...
  for i, (k1, v1) in view1:
    var found = false
    for (k2, v2) in view2:
      if call_eq(k1, k2, kType):
        if call_eq(v1, v2, vType):
          found = true
          break
        else:
          # Both had the same key, but not the same value, so fail!
          return false
    if not found:
      # Current key in k1 was not in k2
      return false

  # Since # of keys are the same and dupe keys aren't allowed, if we
  # get here, they pass the exam.

  return true

proc dict_index(p: pointer, k: pointer, err: var bool): pointer
               {.exportc, cdecl.} =
  var d = cast[Con4mDict](p)

  let opt = lookup(d.obj, k)
  if opt.isNone():
    err = true
  else:
    return opt.get()

proc dict_assign_index(p: pointer, k, v: pointer, err: var bool)
    {.exportc, cdecl.} =
  #`[]=`(d.obj, k, v)
  var d = cast[Con4mDict](p)

  d.obj[k] = v

proc dict_copy(d: Con4mDict, t: TypeId): Con4mDict {.exportc, cdecl.} =
  result = newDict(t)

  let
    tobj  = t.idToTypeRef()
    kType = tobj.items[0]
    vType = tobj.items[1]


  for (k1, v1) in d.obj.items():
    var
      k2 = k1.call_copy(kType)
      v2 = v1.call_copy(vType)

    `[]=`(result.obj, k2, v2)

proc cast_from_dict_t(d: var Con4mDict, tfrom, tto: TypeId, err: var string):
                     Con4mDict {.cdecl, exportc.} =
  var
    to1 = d.t.idToTypeRef()
    to2 = tto.idToTypeRef()
    kt1 = to1.items[0].followForwards()
    vt1 = to1.items[1].followForwards()
    kt2 = to2.items[0].followForwards()
    vt2 = to2.items[1].followForwards()

  if to2.kind != C4Dict:
    err = "CannotCast"
    return nil

  result = newDict(d.t)

  for (k, v) in d.obj.items():
    var k2, v2: pointer

    if kt1 != kt2:
      k2 = call_cast(k, kt1, kt2, err)
      if err != "":
        # TODO: free the underlying dict.
        return nil
    else:
      k2 = call_copy(k, kt1)

    if vt1 != vt2:
      v2 = call_cast(v, kt1, vt2, err)
      if err != "":
        return nil
    else:
      v2 = call_copy(v, vt1)

    `[]=`(result.obj, k2, v2)

proc get_cast_func_dict(dt, ot: DataType, tfrom, tto: TypeId, err: var string):
    pointer {.cdecl, exportc.} =
  return cast[pointer](cast_from_dict_t)

proc dict_len(d: var Con4mDict): int {.exportc, cdecl.} =
  let n = d.obj.items()

  return n.len()

proc dict_marshal(d: Con4mDict, t: TypeId, memos: Memos):
                 C4Str {.exportc, cdecl.} =

  let
    view      = d.obj.items(sort = true)
    to        = t.idToTypeRef()
    kt        = to.items[0]
    vt        = to.items[1]
    paircount = view.len()

  var
    toAdd: seq[(pointer, pointer)]
    l:     int64

  l = sizeof(int64)

  for (k, v) in view.items():
    let
      mkey = k.marshal(kt, memos)
      mval = v.marshal(vt, memos)

    # The parent tracks the length of objects in containers.
    l += mkey.len() + mval.len() + 2 * sizeof(int64)

    toAdd.add((mkey, mval))

  result = newC4Str(l)
  copyMem(cast[pointer](result), cast[pointer](addr paircount), sizeof(int64))

  var
    offset = sizeof(int64)
    lenStr = newC4Str(sizeof(int64))
    objLen = cast[ptr int64](lenstr)

  for (k, v) in toAdd:
    objLen[] = k.len()
    c4str_write_offset(result, lenStr, offset)
    offset += sizeof(int64)
    c4str_write_offset(result, k, offset)
    offset += k.len()

    objLen[] = v.len()
    c4str_write_offset(result, lenStr, offset)
    offset += sizeof(int64)
    c4str_write_offset(result, v, offset)
    offset += v.len()

proc dict_unmarshal(s: var cstring, t: TypeId, memos: Memos):
                   Con4mDict {.exportc, cdecl.} =
  var
    objstart: cstring
    numpairs: int64
    offset:   int64
    objlen:   int64
    lenptr:   ptr int64
    startaddr = cast[int64](cast[pointer](s))
    to        = t.idToTypeRef()
    kt        = to.items[0]
    vt        = to.items[1]

  result   = newDict(t)
  objstart = cast[cstring](s)
  lenptr   = cast[ptr int64](s)
  numpairs = lenptr[]
  offset   = sizeof(int64)

  for i in 0 ..< numpairs:
    lenptr   = cast[ptr int64](startaddr + offset)
    objlen   = lenptr[]
    offset  += sizeof(int64)
    objstart = cast[cstring](startaddr + offset)
    let key  = unmarshal(objstart, kt, memos)
    offset  += objlen

    lenptr   = cast[ptr int64](startaddr + offset)
    objlen   = lenptr[]
    offset  += sizeof(int64)
    objstart = cast[cstring](startaddr + offset)
    let val  = unmarshal(objstart, kt, memos)
    offset  += objlen

    result.obj[key] = val

proc dict_lit(st: SyntaxType, litmod: string, t: TypeId, items: seq[pointer],
              err: var string): Con4mDict {.
                exportc, cdecl.}

dictOps[FRepr]         = cast[pointer](dict_repr)
dictOps[FCastFn]       = cast[pointer](get_cast_func_dict)
dictOps[FContainerLit] = cast[pointer](dict_lit)
dictOps[Feq]           = cast[pointer](dict_eq)
dictOps[FLen]          = cast[pointer](dict_len)
dictOps[FDictIndex]    = cast[pointer](dict_index)
dictOps[FAssignDIx]    = cast[pointer](dict_assign_index)
dictOps[FCopy]         = cast[pointer](dict_copy)
dictOps[FMarshal]      = cast[pointer](dict_marshal)
dictOps[FUnmarshal]    = cast[pointer](dict_unmarshal)

TDict = addDataType(name = "dict", concrete = false, ops = dictOps,
                                              ckind = C4Dict)

registerSyntax(TDict, STDict, @["d"], primary = true)

proc dict_lit(st: SyntaxType, litmod: string, t: TypeId, items: seq[pointer],
              err: var string): Con4mDict =
  result = t.newDict()

  var i = 0

  while i < items.len():
    `[]=`(result.obj, items[i], items[i + 1])
    i += 2

proc dict_contains(d: Con4mDict, item: pointer): bool {.exportc, cdecl.} =
  result = false

  let
    to   = d.t.idToTypeRef()
    kt   = to.items[0]
    view = d.obj.keys()

  for k in view:
    if call_eq(item, k, kt):
      result = true

addStaticFunction("dict_contains", dict_contains)

proc dict_items(d: Con4mDict): FlexArray[pointer] {.exportc, cdecl.} =
  let
    to   = d.t.idToTypeRef()
    kt   = to.items[0]
    vt   = to.items[1]
    tupT = tTuple(@[kt, vt])
    resT = tList(tupT)
    view = d.obj.items()

  result          = newArray[pointer](view.len())
  result.metadata = cast[pointer](resT)

  for i, (k, v) in view:
    let tup = newTuple(tupT)
    discard flexarray_set(tup.obj, 0, k)
    discard flexarray_set(tup.obj, 1, v)
    discard flexarray_set(result.arr, uint64(i), cast[pointer](tup))

addStaticFunction("dict_items", dict_items)

proc dict_keys(d: Con4mDict): FlexArray[pointer] {.exportc, cdecl.} =
  let
    to   = d.t.idToTypeRef()
    kt   = to.items[0]
    view = d.obj.keys()

  result          = newArray[pointer](view.len())
  result.metadata = cast[pointer](tList(kt))

  for i, k in view:
    discard flexarray_set(result.arr, uint64(i), k)

addStaticFunction("dict_keys", dict_keys)


proc dict_values(d: Con4mDict): FlexArray[pointer] {.exportc, cdecl.} =
  let
    to   = d.t.idToTypeRef()
    kt   = to.items[0]
    view = d.obj.values()

  result          = newArray[pointer](view.len())
  result.metadata = cast[pointer](tList(kt))

  for i, k in view:
    discard flexarray_set(result.arr, uint64(i), k)

addStaticFunction("dict_values", dict_values)
