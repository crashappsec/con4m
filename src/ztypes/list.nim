import base, ../common, strutils

# TODO: this is currently using Nim seq's until we fully wrap hatrack.

var listOps = newVtable()

proc list_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string): pointer {.
                exportc, cdecl.}

proc list_repr(pre: pointer): cstring {.exportc, cdecl.} =
  let c = extractRef[Zlist](pre)
  var parts: seq[string]

  for item in c.l:
    parts.add($(item.call_repr(c.tid)))

  return cstring("[" & parts.join(", ") & "]")

proc call_eq(v1, v2: pointer, t: TypeId): bool {.importc, cdecl.}

proc list_eq(l1, l2: pointer): bool {.exportc, cdecl.} =
  let
    ro1 = extractRef[ZList](l1)
    ro2 = extractRef[Zlist](l2)

  if ro1.l.len() != ro2.l.len():
    return false

  if ro1.l.len() == 0:
    return true

  if ro1.tid.followForwards() != ro2.tid.followForwards():
    return false

  let
    typeObj = ro1.tid.idToTypeRef()
    subtype = typeObj.items[0]

  for i in 0 ..< ro1.l.len():
    if not call_eq(ro1.l[i], ro2.l[i], subtype):
      return false

  return true

proc list_len(p: pointer): int {.exportc, cdecl.} =
  let zlist = extractRef[ZList](p)

  return zlist.l.len()

proc z_maybe_incref(p: pointer, t: TypeId) {.importc, cdecl.}
proc toString(x: TypeId): string {.importc, cdecl.}

proc list_index(p: pointer, i: int, err: var bool): pointer
    {.exportc, cdecl.} =
  let zlist = extractRef[ZList](p)

  if i < 0 or i >= zlist.l.len():
    err = true
    return

  result = zlist.l[i]

  let to = zlist.tid.idToTypeRef()
  z_maybe_incref(result, to.items[0])

proc list_assign_ix(p: pointer, o: pointer, i: int, err: var bool)
    {.exportc, cdecl.} =
  let zlist = extractRef[Zlist](p)

  if i < 0 or i >= zlist.l.len():
    err = true
    return

  zlist.l[i] = o

proc list_slice(p: pointer, i, j: int, err: var bool): pointer
    {.exportc, cdecl.} =
  let zlist = extractRef[ZList](p)
  var
    newlist: ZList
    i = i
    j = j

  if i < 0:
    i += zlist.l.len()
  if j < 0:
    j += zlist.l.len()

  if i >= j or i >= zlist.l.len():
    newList = ZList(l: @[], tid: zlist.tid)
  else:
    newList = ZList(l: zlist.l[i ..< j], tid: zlist.tid)
  result = newRefValue(newlist, zlist.tid)

proc list_assign_slice(c, v: pointer, i, j: int, err: var bool)
    {.exportc, cdecl.} =
  var
    l: seq[pointer]
    r: seq[pointer]
    clist = extractRef[Zlist](c)
    vlist = extractRef[Zlist](v)
    i     = i
    j     = j

  if i < 0:
    i += clist.l.len()
  if j < 0:
    j += clist.l.len()
  if i > 0:
    l = clist.l[0 ..< i]
  if j < clist.l.len():
    r = clist.l[j .. ^1]

  clist.l = l & vlist.l & r

proc call_copy(p: pointer, t: TypeId): pointer {.importc, cdecl.}
proc list_copy(p: pointer, t: TypeId): pointer {.exportc, cdecl.} =
  var
    list = extractRef[Zlist](p)

  var
    tobj = list.tid.idToTypeRef()
    s2: seq[pointer]

  for item in list.l:
    s2.add(call_copy(item, tobj.items[0]))

  let zl = ZList(l: s2, tid: t)

  result = newRefValue(zl, t)

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
              contents: seq[pointer], err: var string): pointer =

  let zl = ZList(l: contents, tid: t)

  return newRefValue(zl, t)
