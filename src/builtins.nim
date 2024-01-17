import nimutils, strutils, ffi, ztypes/base

proc splitwrap*(s1: cstring, s2: cstring):
              FlexArray[cstring] {.exportc, cdecl.} =
  let preResult = `$`(s1).split($(s2))
  var arr: seq[cstring]

  for item in preResult:
    arr.add(cstring(item))

  return newArrayFromSeq[cstring](arr)

proc callecho*(s1: cstring) {.exportc, cdecl.} =
  echo($(s1))

proc echoanint*(s1: int64) {.exportc, cdecl.} =
  echo($(s1))

proc callprint*(s1: Rope) {.exportc, cdecl.} =
  print(s1)

proc calltable*(s1: FlexArray[FlexArray[Rope]]): Rope {.exportc, cdecl.} =
  # For now we have to do a little dance.
  var
    actual: seq[seq[Rope]]

  for item in s1.items():
    actual.add(item.items())

  result = actual.quickTable()
  GC_ref(result)

proc callflattable*(s1: FlexArray[Rope]): Rope {.exportc, cdecl.} =

  result = s1.items().instantTable()
  GC_ref(result)

proc listlen*(arr: FlexArray[pointer]): int {.exportc, cdecl.} =
  return int(arr.arr.flexarray_len())

proc ropeplus*(a, b: Rope): Rope {.exportc, cdecl.} =
  result = a + b
  GC_ref(result)

proc listadd(l1, l2: FlexArray[pointer]): FlexArray[pointer]
    {.exportc, cdecl.}=
  return l1 + l2

addStaticFunction("splitwrap", splitwrap)
addStaticFunction("echoanint",  echoanint)
addStaticFunction("callecho",  callecho)
addStaticFunction("callprint", callprint)
addStaticFunction("calltable", calltable)
addStaticFunction("callflattable", callflattable)
addStaticFunction("listlen", listlen)
addStaticFunction("ropeplus", ropeplus)
addStaticFunction("listadd", listadd)
