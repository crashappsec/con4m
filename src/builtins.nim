import nimutils, strutils, ffi

proc splitwrap*(s1: cstring, s2: cstring): seq[cstring] {.exportc, cdecl.} =
  let preResult = `$`(s1).split($(s2))

  for item in preResult:
    result.add(cstring(item))

proc callecho*(s1: cstring) {.exportc, cdecl.} =
  echo($(s1))

proc echoanint*(s1: uint64) {.exportc, cdecl.} =
  echo($(s1))

addStaticFunction("splitwrap", splitwrap)
addStaticFunction("echoanint",  echoanint)
addStaticFunction("callecho",  callecho)
