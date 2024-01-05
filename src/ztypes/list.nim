import base, ../common

# TODO: this is currently using Nim seq's until we fully wrap hatrack.

var listOps = newVtable()

type ZList* = ref object
  l:   seq[pointer]
  tid: TypeId

proc list_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string): pointer {.
                exportc, cdecl.}

proc list_repr(pre: pointer): string {.exportc, cdecl.} =
  let c = extractRef[Zlist](pre)
  var parts: seq[string]

  for item in c.l:
    parts.add(item.call_repr(c.tid))


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

proc list_index(p: pointer, i: int, err: var bool): pointer =
  let zlist = extractRef[ZList](p)

  if i < 0 or i >= zlist.l.len():
    err = true
    return

  result = zlist.l[i]

  let to = zlist.tid.idToTypeRef()
  z_maybe_incref(result, to.items[0])

# proc list_slice()
# proc list_assign_ix()
# proc list_slice()
# proc list_assign_slice()
# proc list_copy()

listOps[FRepr]         = cast[pointer](list_repr)
listOps[FContainerLit] = cast[pointer](list_lit)
listOps[Feq]           = cast[pointer](list_eq)
listOps[FLen]          = cast[pointer](list_len)
listOps[FIndex]        = cast[pointer](list_index)

TList  = addDataType(name = "list", concrete = false, ops = listOps,
                                               ckind = C4List)

registerSyntax(TList, STList, @["l"], primary = true)

proc list_lit(st: SyntaxType, litmod: string, t: TypeId,
              contents: seq[pointer], err: var string): pointer =

  let zl = ZList(l: contents, tid: t)

  return newRefValue(zl, t)
