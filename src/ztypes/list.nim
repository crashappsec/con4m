import "."/[base, marshal]

# TODO: this is currently using Nim seq's until we fully wrap hatrack.

var listOps = newVtable()

proc get_cast_fn(tcur, tdst: DataType, tfrom, tto: TypeId, err: var string):
     pointer {.importc, cdecl.}
proc call_cast(v: pointer, tcur, tdst: TypeId, err: var string): pointer {.
                importc, cdecl.}

proc list_repr(c: FlexArray[pointer]): cstring {.exportc, cdecl.} =
  var parts: seq[string]

  let
    list_type = cast[TypeId](c.metadata)
    to        = list_type.idToTypeRef()

  for item in c.items():
    parts.add($(item.call_repr(to.items[0])))

  return cstring("[" & parts.join(", ") & "]")

proc call_eq(v1, v2: pointer, t: TypeId): bool {.importc, cdecl.}

proc list_eq(l1, l2: FlexArray[pointer]): bool {.exportc, cdecl.} =
  if l1 == l2:
    return true

  let
    v1 = l1.items()
    v2 = l2.items()

  if v1.len() != v2.len():
    return false

  if v1.len() == 0:
    return true

  let
    tid1    = cast[TypeId](l1.metadata)
    tid2    = cast[TypeId](l2.metadata)
    subtype = tid1.idToTypeRef().items[0]

  if tid1 != tid2:
    return false

  for i, item in v1:
    if not call_eq(item, v2[i], subtype):
      return false

  return true

proc list_len(l1: FlexArray[pointer]): int {.exportc, cdecl.} =
  return l1.len()

proc toString(x: TypeId): string {.importc, cdecl.}

proc list_index(arr: FlexArray[pointer], i: int, err: var bool): pointer
    {.exportc, cdecl.} =

  let opt = arr.get(i)

  if opt.isNone():
    err = true
  else:
    result = opt.get()

  # let to = cast[TypeId](arr.metadata.idToTypeRef())

proc list_assign_ix(p: FlexArray[pointer], o: pointer, i: int, err: var bool)
    {.exportc, cdecl.} =
  err = not p.put(i, o)

proc list_slice(p: FlexArray[pointer], i, j: int, err: var bool):
               FlexArray[pointer] {.exportc, cdecl.} =
  let view = p.items()
  var
    i = i
    j = j
    l = view.len()

  if i < 0:
    i += l
  if j < 0:
    j += l

  if i >= j or i >= l:
    result = newArrayFromSeq[pointer](@[])
  else:
    result = newArrayFromSeq[pointer](view[i ..< j])

  GC_ref(result)
  result.metadata = p.metadata

proc list_assign_slice(c, v: FlexArray[pointer], i, j: int, err: var bool)
    {.exportc, cdecl.} =
  var
    left, right: seq[pointer]
    orig  = c.items()
    slice = v.items()
    l     = orig.len()
    i     = i
    j     = j

  if i < 0:
    i += l
  if j < 0:
    j += l
  if i > 0:
    left = orig[0 ..< i]
  if j < l:
    right = orig[j .. ^1]

  let
    oldptr  = c.arr
    newlist = newArrayFromSeq[pointer](left & slice & right)

  newlist.metadata = c.metadata

  c.arr = newlist.arr
  flexarray_delete(oldptr)
  GC_ref(newlist)

proc call_copy(p: pointer, t: TypeId): pointer {.importc, cdecl.}

proc list_copy(p: FlexArray[pointer], t: TypeId): FlexArray[pointer]
               {.exportc, cdecl.} =
  var
    dup: seq[pointer]
    view    = p.items()
    tid     = cast[TypeId](p.metadata)
    tobj    = tid.idToTypeRef()
    subtype = tobj.items[0]

  for i, item in view:
    let item = call_copy(cast[pointer](item), subtype)
    dup.add(item)

  result          = newArrayFromSeq[pointer](dup)
  result.metadata = cast[pointer](tid)

  GC_ref(result)

proc cast_from_list_t(pre: FlexArray[pointer], tfrom, tto: TypeId,
                      err: var string): FlexArray[pointer] {.cdecl, exportc.} =

  var
    s: seq[pointer] = @[]
    t1  = cast[TypeId](pre.metadata)
    to1 = t1.idToTypeRef()
    to2 = tto.idToTypeRef()

  if to2.kind != C4List:
    err = "CannotCast"
    return nil

  for item in pre.items():
    let r = call_cast(item,
                    to1.items[0].followForwards(),
                    to2.items[0].followForwards(), err)
    s.add(r)

    if err != "":
      return nil

  result = newArrayFromSeq[pointer](s)
  result.metadata = cast[pointer](tto)
  GC_ref(result)

proc get_cast_func_list(dt, ot: DataType, tfrom, tto: TypeId,
                        err: var string): pointer {.cdecl, exportc.} =
  return cast[pointer](cast_from_list_t)


proc list_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string):
                FlexArray[pointer] {.exportc, cdecl.} =

  result          = newArrayFromSeq[pointer](contents)
  result.metadata = cast[pointer](t)

  GC_ref(result)

proc list_marshal(list: FlexArray[pointer], t: TypeId, memos: Memos):
                 C4Str {.exportc, cdecl.} =
  let
    view      = list.items()
    num_items = int64(view.len())
    len_str    = newC4Str(sizeof(int64))
    obj_len    = cast[ptr int64](lenstr)
    to        = t.idToTypeRef()
    item_type = to.items[0]

  var
    l      = sizeof(int64)
    offset = sizeof(int64)
    to_add: seq[pointer]

  for item in view:
    let marshaled = item.marshal(item_type, memos)

    l += marshaled.len() + sizeof(int64)
    to_add.add(marshaled)

  result = newC4Str(l)
  copyMem(cast[pointer](result), cast[pointer](addr num_items), sizeof(int64))

  for item in to_add:
    obj_len[] = item.len()
    c4str_write_offset(result, len_str, offset)
    offset += sizeof(int64)
    c4str_write_offset(result, item, offset)
    offset += item.len()

proc list_unmarshal(s: cstring, t: TypeId, memos: Memos):
                   FlexArray[pointer] {.exportc, cdecl.} =
  var
    objstart: cstring
    numitems: int64
    offset:   int64
    objlen:   int64
    myitems:  seq[pointer]
    lenptr:   ptr int64
    startaddr = cast[int64](cast[pointer](s))
    to        = t.idToTypeRef()
    itemtype  = to.items[0]

  objstart = cast[cstring](s)
  lenptr   = cast[ptr int64](s)
  numitems = lenptr[]
  offset   = sizeof(int64)

  for i in 0 ..< numitems:
    lenptr   = cast[ptr int64](startaddr + offset)
    objlen   = lenptr[]
    offset  += sizeof(int64)
    objstart = cast[cstring](startaddr + offset)
    myitems.add(unmarshal(objstart, itemtype, memos))
    offset += objlen

  result          = newArrayFromSeq[pointer](myitems)
  result.metadata = cast[pointer](t)
  GC_ref(result)

listOps[FRepr]         = cast[pointer](list_repr)
listOps[FCastFn]       = cast[pointer](get_cast_func_list)
listOps[FContainerLit] = cast[pointer](list_lit)
listOps[Feq]           = cast[pointer](list_eq)
listOps[FLen]          = cast[pointer](list_len)
listOps[FIndex]        = cast[pointer](list_index)
listOps[FAssignIx]     = cast[pointer](list_assign_ix)
listOps[FSlice]        = cast[pointer](list_slice)
listOps[FAssignSlice]  = cast[pointer](list_assign_slice)
listOps[FCopy]         = cast[pointer](list_copy)
listOps[FMarshal]      = cast[pointer](list_marshal)
listOps[FUnmarshal]    = cast[pointer](list_unmarshal)

TList  = addDataType(name = "list", concrete = false, ops = listOps,
                                               ckind = C4List)

registerSyntax(TList, STList, @["l"], primary = true)


proc list_contains(s: FlexArray[pointer], m: pointer): bool {.exportc,
                                                              cdecl.} =
  result = false

  let
    t    = cast[TypeId](s.metadata)
    to   = t.idToTypeRef()
    view = s.items()

  for item in view:
    if call_eq(m, item, to.items[0]):
      result = true
      # Keep looping, try to avoid timing attacks.

addStaticFunction("list_contains", list_contains)
