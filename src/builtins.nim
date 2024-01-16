import nimutils, strutils, ffi, ztypes/base

proc splitwrap*(s1: cstring, s2: cstring): seq[cstring] {.exportc, cdecl.} =
  let preResult = `$`(s1).split($(s2))

  for item in preResult:
    result.add(cstring(item))

proc callecho*(s1: cstring) {.exportc, cdecl.} =
  echo($(s1))

proc echoanint*(s1: int64) {.exportc, cdecl.} =
  echo($(s1))

proc callprint*(s1: Rope) {.exportc, cdecl.} =
  print(s1)

proc calltable*(s1: pointer): Rope {.exportc, cdecl.} =
  # For now because of the way we're packing we have to do a little dance.
  var
    actual: seq[seq[Rope]]
    ropeZl = extractRef[Zlist](s1)

  for item in ropeZl.l:
    let one = extractRef[Zlist](item)

    var row: seq[Rope]
    for cell in one.l:
      row.add(cast[Rope](cell))
    actual.add(row)

  result = actual.quickTable()
  GC_ref(result)

proc callflattable*(s1: pointer): Rope {.exportc, cdecl.} =
  let
    ropeZl = extractRef[Zlist](s1)
    l      = cast[seq[Rope]](ropeZl.l)

  var
    table: seq[Rope]

  result = l.instantTable()
  GC_ref(result)

proc listlen*(p: pointer): int {.exportc, cdecl.} =
  let l = extractRef[Zlist](p)
  return l.l.len()

proc ropeplus*(a, b: Rope): Rope {.exportc, cdecl.} =
  result = a + b
  GC_ref(result)

addStaticFunction("splitwrap", splitwrap)
addStaticFunction("echoanint",  echoanint)
addStaticFunction("callecho",  callecho)
addStaticFunction("callprint", callprint)
addStaticFunction("calltable", calltable)
addStaticFunction("callflattable", callflattable)
addStaticFunction("listlen", listlen)
addStaticFunction("ropeplus", ropeplus)
