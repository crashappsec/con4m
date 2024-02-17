import "."/[base, marshal]

# TODO: this is currently using Nim seq's until we fully wrap hatrack.

var listOps = newVtable()

proc call_cast(v: pointer, tcur, tdst: TypeId, err: var string): pointer {.
                importc, cdecl.}

proc list_repr(c: FlexArray[pointer]): cstring {.exportc, cdecl.} =
  if c == nil:
    return cstring("(null)")

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

proc list_index(arr: FlexArray[pointer], i: int, t: TypeId,
                err: var bool): pointer
    {.exportc, cdecl.} =

  let opt = arr.get(i)

  if opt.isNone():
    err = true
  else:
    result = opt.get()

  # let to = cast[TypeId](arr.metadata.idToTypeRef())

proc list_assign_ix(p: FlexArray[pointer], o: pointer, i: int,
                    t: TypeId, err: var bool)
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

  var l: seq[string]
  for item in contents:
    l.add($(cast[int](item)))

  result          = newArrayFromSeq[pointer](contents)
  result.metadata = cast[pointer](t)

  GC_ref(result)

proc list_marshal(fa: FlexArray[pointer], t: TypeId, memos: Memos):
                 C4Str {.exportc, cdecl.} =

  if fa == nil:
    return marshal_32_bit_value(-1)

  let
    to        = cast[TypeId](t).idToTypeRef()
    item_type = to.items[0]

  list_marshal_helper(fa.items()):
      toAdd.add(item.marshal(item_type, memos))

proc list_unmarshal(s: var cstring, t: TypeId, memos: Memos):
                   FlexArray[pointer] {.exportc, cdecl.} =
  var
    numitems = s.unmarshal_32_bit_value()
    myitems: seq[pointer]

  if numitems == -1:
    return nil

  let
    to   = t.idToTypeRef()
    valt = to.items[0]

  result          = newArray[pointer](numitems)
  result.metadata = cast[pointer](t)
  GC_ref(result)

  for i in 0 ..< numitems:
    result[i] = s.unmarshal(valt, memos)

proc nim_to_con4m(x: pointer, tid: TypeId): pointer {.importc, cdecl.}

proc nim_list_to_con4m(x: seq[pointer], t: TypeId): FlexArray[pointer] =
  var processed: seq[pointer]

  let item_type = t.idToTypeRef().items[0]

  for item in x:
    processed.add(item.nim_to_con4m(item_type))

  result = newArrayFromSeq(processed)

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
listOps[FFromNim]      = cast[pointer](nim_list_to_con4m)

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
