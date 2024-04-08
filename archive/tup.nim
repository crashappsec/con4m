import "."/[base, marshal]

var tupOps = newVTable()

proc call_cast(v: pointer, tcur, tdst: TypeId, err: var string): pointer {.
                importc, cdecl.}
proc call_copy(p: pointer, t: TypeId): pointer {.importc, cdecl.}
proc call_eq(v1, v2: pointer, t: TypeId): bool {.importc, cdecl.}

proc newTuple*(t: TypeId): Con4mTuple =
  let n = t.idToTypeRef()

  result     = Con4mTuple()
  result.t   = t
  result.obj = flexarray_new(uint64(n.items.len()))

  GC_ref(result)

proc items*(c: Con4mTuple): seq[pointer] =
  var
    found: bool
    view = c.obj.flexarray_view()

  result = @[]

  while true:
    let item = view.flexarray_view_next(addr found)
    if not found:
      break
    result.add(item)

  view.flexarray_view_delete()

proc newTupleFromSeq(t: TypeId, s: openarray[pointer]): Con4mTuple =
  result = t.newTuple()
  for i, item in s:
    discard flexarray_set(result.obj, uint64(i), item)

proc tup_repr(c: Con4mTuple): cstring {.exportc, cdecl.} =

  if c == nil:
    return cstring("(null)")
  var
    parts: seq[string] = @[]
    tobj = c.t.idToTypeRef()
    view = c.obj.flexarray_view()
    found: bool
    i = 0

  while true:
    let p = view.flexarray_view_next(addr found)
    if not found:
      break
    parts.add($(call_repr(p, tObj.items[i])))
    i = i + 1

  return cstring("(" & parts.join(", ") & ")")

proc tup_eq(v1, v2: Con4mTuple, t: TypeId): bool {.exportc, cdecl.} =
  if v1 == v2:
    return true
  # With tuples, we would never change the length.
  if v1.obj.flexarray_len() != v2.obj.flexarray_len():
    return false
  var
    found: bool
    i     = 0
    to1   = v1.t.idToTypeRef()
    to2   = v2.t.idToTypeRef()
    view1 = v1.obj.flexarray_view()
    view2 = v2.obj.flexarray_view()

  while true:
    let
      i1Type = to1.items[i]
      i2Type = to2.items[i]
      p1     = view1.flexarray_view_next(addr found)
      p2     = view2.flexarray_view_next(addr found)

    if not found or i1Type != i2Type:
      return false
    if not call_eq(p1, p2, i1Type):
      return false
    i += 1

  return true

proc tup_index*(tup: Con4mTuple, i: int, t: TypeId, err: var bool): pointer
    {.exportc, cdecl.} =
  var code: cint

  result = flexarray_get(tup.obj, uint64(i), addr code)

proc tup_assign_ix(tup: Con4mTuple, o: pointer, i: int,
                   t: TypeId, err: var bool) {.exportc, cdecl.} =

  err = not flexarray_set(tup.obj, uint64(i),  o)

proc `[]=`*(tup: Con4mTuple, i: int, v: pointer) =
  var err: bool

  tup_assign_ix(tup, v, i, TVoid, err)

  if err:
    raise newException(ValueError, "Array index out of bounds")

proc `[]`*(tup: Con4mTuple, i: int): pointer =
  var err: bool

  result = tup_index(tup, i, TVoid, err)

  if err:
    raise newException(ValueError, "Array index out of bounds")

proc tup_copy(tup: Con4mTuple, t: TypeId): Con4mTuple {.exportc, cdecl.} =
  let
    to   = tup.t.idToTypeRef()
    l    = to.items.len()
    view = tup.items()

  var dups = newSeq[pointer](l)

  for i, item in view:
    dups[i] = call_copy(item, to.items[i])

  result = newTupleFromSeq(tup.t, dups)

proc tup_len(t: Con4mTuple): int {.cdecl, exportc.} =
  return int(t.obj.flexarray_len())

proc cast_from_tup(tup: Con4mTuple, tfrom, tto: TypeId, err: var string):
            Con4mTuple {.cdecl, exportc.} =
  let
    to1 = tup.t.idToTypeRef()
    to2 = tto.idToTypeRef()

  if to2.kind != C4Tuple:
    err = "CannotCast"
    return nil

  var castItems = newSeq[pointer](to1.items.len())

  for i, item in tup.items():
    if to1.items[i].followForwards() == to2.items[i].followForwards():
      castItems[i] = call_copy(item, to1.items[i])
    else:
      castItems[i] = call_cast(item,
                               to1.items[i].followForwards(),
                               to2.items[i].followForwards(), err)

  if err != "":
    return nil

  result = newTupleFromSeq(tup.t, castItems)

proc get_cast_func_tup(dt, ot: DataType, tfrom, tto: TypeId,
                        err: var string): pointer {.cdecl, exportc.} =
  return cast[pointer](cast_from_tup)

proc tup_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string):
                Con4mTuple {.cdecl, exportc.} =
  result = newTupleFromSeq(t, contents)

proc tup_marshal(tup: Con4mTuple, t: TypeId, memos: Memos):
                C4Str {.exportc, cdecl.} =
  if tup == nil:
    return marshal_32_bit_value(-1)

  let to = t.idToTypeRef()

  list_marshal_helper(tup.items()):
      toAdd.add(item.marshal(to.items[loop_index], memos))

proc tup_unmarshal(s: var cstring, t: TypeId, memos: Memos):
                  Con4mTuple {.cdecl, exportc.} =
  var
    numitems: int
    myitems:  seq[pointer]
    totallen: int
    to        = t.idToTypeRef()

  totallen = s.unmarshal_32_bit_value()

  if totallen == -1:
    return nil

  for i in 0 ..< totallen:
    myitems.add(unmarshal(s, to.items[i], memos))

  result = newTupleFromSeq(t, myitems)

tupOps[FRepr]         = cast[pointer](tup_repr)
tupOps[FCastFn]       = cast[pointer](get_cast_func_tup)
tupOps[FContainerLit] = cast[pointer](tup_lit)
tupOps[Feq]           = cast[pointer](tup_eq)
tupOps[FLen]          = cast[pointer](tup_len)
tupOps[FIndex]        = cast[pointer](tup_index)
tupOps[FAssignIx]     = cast[pointer](tup_assign_ix)
tupOps[FCopy]         = cast[pointer](tup_copy)
tupOps[FMarshal]      = cast[pointer](tup_marshal)
tupOps[FUnmarshal]    = cast[pointer](tup_unmarshal)

TTuple = addDataType(name = "tuple", concrete = false, ops = tupOps,
                                                ckind = C4Tuple)

registerSyntax(TTuple, StTuple, @["t"], primary = true)
