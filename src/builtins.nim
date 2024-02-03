# Wrappers for con4m builtins that are as light as possible; even if
# we can directly translate to the C API, for the sake of the
# interpreter being able to statically use libffi to call these items,
# we explicitly pull them all in.
import ztypes/api

proc splitwrap*(s1: cstring, s2: cstring):
              FlexArray[cstring] {.exportc, cdecl.} =
  let preResult = `$`(s1).split($(s2))
  var arr: seq[cstring]

  for item in preResult:
    arr.add(cstring(item))

  return newArrayFromSeq[cstring](arr)

addStaticFunction("splitwrap", splitwrap)

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

addStaticFunction("calltable", calltable)


proc callflattable*(s1: FlexArray[Rope]): Rope {.exportc, cdecl.} =

  var l: seq[Rope]
  for item in s1.items():
    l.add(item.copy())

  result = l.instantTable()

  GC_ref(result)

addStaticFunction("callflattable", callflattable)

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

addStaticFunction("con4m_print", con4m_print)


proc listlen*(arr: FlexArray[pointer]): int {.exportc, cdecl.} =
  return int(arr.arr.flexarray_len())

addStaticFunction("listlen", listlen)


proc ropeplus*(a, b: Rope): Rope {.exportc, cdecl.} =
  result = a + b
  GC_ref(result)

addStaticFunction("ropeplus", ropeplus)


proc listadd(l1, l2: FlexArray[pointer]): FlexArray[pointer]
    {.exportc, cdecl.}=
  return l1 + l2

addStaticFunction("listadd", listadd)


proc con4m_repr(p: pointer): cstring {.exportc, cdecl.} =
  var n: Mixed = cast[Mixed](p)

  return cstring(call_repr(n.value, n.t))

addStaticFunction("con4m_repr", con4m_repr)

{.pragma: stdlib, cdecl, importc, header: "<stdlib.h>".}

proc unsetenv(name: cstring): cint {.stdlib.}
proc setenv(name: cstring, val: cstring, overwrite: cint): cint {.stdlib.}
addStaticFunction("getenv", cast[pointer](getenv))
addStaticFunction("unsetenv", unsetenv)
addStaticFunction("setenv", setenv)

proc env_exists(s: cstring): bool {.exportc, cdecl.} =
  return existsEnv($(s))

addStaticFunction("env_exists", env_exists)

proc env_all(): Con4mDict {.exportc, cdecl.} =
  result = newDict(tDict(TString, TString))

  for (k, v) in envPairs():
    result.obj[newC4Str(k)] = newC4Str(v)

addStaticFunction("env_all", env_all)

# These 3 things are just used for tests. con4m_print() above is the
# 'real' call. Prob should remove these and migrate tests to stuff we
# actually use.
proc callecho*(s1: cstring) {.exportc, cdecl.} =
  echo($(s1))

addStaticFunction("callecho",  callecho)

proc echoanint*(s1: int64) {.exportc, cdecl.} =
  echo($(s1))

addStaticFunction("echoanint", echoanint)

proc callprint*(s1: Rope) {.exportc, cdecl.} =
  print(s1)

addStaticFunction("callprint", callprint)
