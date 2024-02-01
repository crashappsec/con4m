import nimutils, strutils, ffi, ztypes/api

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

  for l in s1.items():
    var row: seq[Rope] = @[]

    for item in l.items():
      row.add(item.copy())
    actual.add(row)

  result = actual.quickTable()
  GC_ref(result)

proc con4m_print(p: pointer) {.exportc, cdecl.} =
  # Once we properly integrate the 'Mixed' type we can make this
  # take a variable number of arguments.

  var n: Mixed = cast[Mixed](p)

  if unify(TRich, n.t) != TBottom:
    print(cast[Rope](n.value))
    return

  var err = ""

  let asRope = cast[Rope](call_cast(n.value, n.t, TRich, err))

  if err == "":
    print(asRope)
    return
  else:
    echo call_repr(n.value, n.t)

proc callflattable*(s1: FlexArray[Rope]): Rope {.exportc, cdecl.} =

  var l: seq[Rope]
  for item in s1.items():
    l.add(item.copy())

  result = l.instantTable()

  GC_ref(result)

proc listlen*(arr: FlexArray[pointer]): int {.exportc, cdecl.} =
  return int(arr.arr.flexarray_len())

proc ropeplus*(a, b: Rope): Rope {.exportc, cdecl.} =
  result = a + b
  GC_ref(result)

proc listadd(l1, l2: FlexArray[pointer]): FlexArray[pointer]
    {.exportc, cdecl.}=
  return l1 + l2

proc con4m_repr(p: pointer): cstring {.exportc, cdecl.} =
  var n: Mixed = cast[Mixed](p)

  return cstring(call_repr(n.value, n.t))

addStaticFunction("splitwrap", splitwrap)
addStaticFunction("echoanint",  echoanint)
addStaticFunction("callecho",  callecho)
addStaticFunction("callprint", callprint)
addStaticFunction("con4m_print", con4mprint)
addStaticFunction("con4m_repr", con4m_repr)
addStaticFunction("calltable", calltable)
addStaticFunction("callflattable", callflattable)
addStaticFunction("listlen", listlen)
addStaticFunction("ropeplus", ropeplus)
addStaticFunction("listadd", listadd)
