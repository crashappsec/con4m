# Wrappers for con4m builtins that are as light as possible; even if
# we can directly translate to the C API, for the sake of the
# interpreter being able to statically use libffi to call these items,
# we explicitly pull them all in.
import std/[osproc, streams]
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
  # If you're crashing here, you need to implement a cast to TRich.

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

proc con4m_osname(): C4Str {.exportc, cdecl.} =
  return newC4Str(hostOs)

addStaticFunction("con4m_osname", con4m_osname)

proc con4m_arch(): C4Str {.exportc, cdecl.} =
  return newC4Str(hostCPU)

addStaticFunction("con4m_arch", con4m_arch)

proc con4m_get_argv(): FlexArray[pointer] {.exportc, cdecl.} =
  var params = commandLineParams()

  return cast[FlexArray[pointer]](toCon4m[seq[string]](params, tList(TString)))

addStaticFunction("con4m_get_argv", con4m_get_argv)

proc con4m_get_exe_path(): C4Str {.exportc, cdecl.} =
  return newC4Str(resolvePath(getAppFileName()))

addStaticFunction("con4m_get_exe_path", con4m_get_exe_path)

proc con4m_get_exe_name(): C4Str {.exportc, cdecl.} =
  return newC4Str(resolvePath(getAppFileName().splitPath().tail))

addStaticFunction("con4m_get_exe_name", con4m_get_exe_name)

proc con4m_highest_int(): int64 {.exportc, cdecl.} =
  return high(int64)

addStaticFunction("con4m_highest_int", con4m_highest_int)

proc con4m_lowest_int(): int64 {.exportc, cdecl.} =
  return low(int64)

addStaticFunction("con4m_lowest_int", con4m_lowest_int)

proc con4m_rand(): int64 {.exportc, cdecl.} =
  return secureRand[int64]()

addStaticFunction("con4m_rand", con4m_rand)

proc con4m_now(): uint64 {.exportc, cdecl.} =
  return unixTimeInMs()

addStaticFunction("con4m_now", con4m_now)

var   containerName: Option[string]
const
  mountInfoFile    = "/proc/self/mountinfo"
  mountInfoPreface = "/docker/containers/"

proc getContainerName*(): Option[string] {.inline.} =
  once:
    var f = newFileStream(mountInfoFile)

    if f == nil: return none(string)

    let lines = f.readAll().split("\n")

    for line in lines:
      let prefixIx = line.find(mountInfoPreface)
      if prefixIx == -1: continue

      let
        startIx = prefixIx + mountInfoPreface.len()
        endIx   = line.find("/", startIx)

      containerName = some(line[startIx ..< endIx])

  return containerName

proc con4m_container_name(): C4Str {.exportc, cdecl.} =
  return newC4Str(getContainerName().getOrElse(""))

addStaticFunction("con4m_container_name", con4m_container_name)

proc con4m_in_container(): bool {.exportc, cdecl.} =
  return con4m_container_name().len() != 0

addStaticFunction("con4m_in_container", con4m_in_container)

proc con4m_run(cmd: cstring): C4Str {.exportc, cdecl.} =
  let (output, _) = execCmdEx($(cmd))

  return newC4Str(output)

addStaticFunction("con4m_run", con4m_run)

proc con4m_system(cmd: cstring): Con4mTuple {.exportc, cdecl.} =
  var res = execCmdEx($(cmd))

  return cast[Con4mTuple](toCon4m(res, tTuple(@[TString, Tint])))

addStaticFunction("con4m_system", con4m_system)
