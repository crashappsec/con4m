import nimutils, strutils, ffi

proc splitwrap*(s1: string, s2: string): seq[string] {.exportc, cdecl.} =
  return s1.split(s2)

proc callecho*(s1: string) {.exportc, cdecl.} =
  echo(s1)

addStaticFunction("splitwrap", splitwrap)
addStaticFunction("callecho", callecho)
