import base, ../common, strutils

# TODO: this is currently using Nim seq's until we fully wrap hatrack.

var listOps = newVtable()

proc list_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string): FlexArray[pointer] {.
                exportc, cdecl.}

proc list_repr(pre: pointer): cstring {.exportc, cdecl.} =
  let c = extractRef[Zlist](pre)
  var parts: seq[string]

  for item in c.l:
    parts.add($(item.call_repr(c.tid)))

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

  c.arr = newlist.arr
  flexarray_delete(oldptr)

proc call_copy(p: pointer, t: TypeId): pointer {.importc, cdecl.}

proc list_copy(p: FlexArray[pointer], t: TypeId): FlexArray[pointer]
               {.exportc, cdecl.} =
  var
    dup: seq[pointer]
    view = p.items()
    tid  = cast[TypeId](p.metadata)
    tobj = tid.idToTypeRef()

  for item in view:
    dup.add(call_copy(cast[pointer](item), tobj.items[0]))

  result          = newArrayFromSeq[pointer](dup)
  result.metadata = p.metadata

listOps[FRepr]         = cast[pointer](list_repr)
listOps[FContainerLit] = cast[pointer](list_lit)
listOps[Feq]           = cast[pointer](list_eq)
listOps[FLen]          = cast[pointer](list_len)
listOps[FIndex]        = cast[pointer](list_index)
listOps[FAssignIx]     = cast[pointer](list_assign_ix)
listOps[FSlice]        = cast[pointer](list_slice)
listOps[FAssignSlice]  = cast[pointer](list_assign_slice)
listOps[FCopy]         = cast[pointer](list_copy)


TList  = addDataType(name = "list", concrete = false, ops = listOps,
                                               ckind = C4List)

registerSyntax(TList, STList, @["l"], primary = true)

proc list_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string):
                FlexArray[pointer] {.importc, cdecl.} =

  result          = newArrayFromSeq[pointer](contents)
  result.metadata = cast[pointer](t)

  GC_ref(result)
