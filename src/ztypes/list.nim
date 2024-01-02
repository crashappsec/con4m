import base, ../common

# TODO: this is currently using Nim seq's until we fully wrap hatrack.

var listOps = newVtable()
let TList*  = addDataType(name = "list", concrete = false, ops = listOps,
                          ckind = C4List)

registerSyntax(TList, STList, @["l"], primary = true)

type ZList* = object
  l:   seq[pointer]
  tid: TypeId

proc list_repr(pre: pointer): string {.cdecl.} =
  let c = extractRef[Zlist](pre)
  var parts: seq[string]

  for item in c.l:
    parts.add(item.call_repr(c.tid))
