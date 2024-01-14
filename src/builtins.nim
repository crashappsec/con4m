import nimutils, strutils, ffi

proc splitwrap*(s1: cstring, s2: cstring): seq[cstring] {.exportc, cdecl.} =
  let preResult = `$`(s1).split($(s2))

  for item in preResult:
    result.add(cstring(item))

proc callecho*(s1: cstring) {.exportc, cdecl.} =
  echo($(s1))

proc echoanint*(s1: int64) {.exportc, cdecl.} =
  echo($(s1))

proc callprint*(s1: Rope) {.exportc, cdecl.} =
  let p = cast[pointer](s1)
  print(s1)

proc calltable*(s1: seq[seq[Rope]]): Rope {.exportc, cdecl.} =
  return s1.quickTable()

proc callflattable*(s1: seq[Rope]): Rope {.exportc, cdecl.} =
  return s1.instantTable()

addStaticFunction("splitwrap", splitwrap)
addStaticFunction("echoanint",  echoanint)
addStaticFunction("callecho",  callecho)
addStaticFunction("callprint", callprint)
addStaticFunction("calltable", calltable)
addStaticFunction("callflattable", callflattable)
