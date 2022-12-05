# Built in functions
import con4m_types
import typecheck
import st
import box

import os
import tables
import osproc
import strformat
import strutils
import options

when defined(posix):
  import posix

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

proc builtinIToS*(args: seq[Box]): Option[Box] =
  let i = unbox[int](args[0])
  var s = $(i)
  var b = box(s)

  return some(b)

proc builtinBToS*(args: seq[Box]): Option[Box] =
  let b = unbox[bool](args[0])
  if b:
    return some(box("true"))
  else:
    return some(box("false"))

proc builtinFToS*(args: seq[Box]): Option[Box] =
  let f = unbox[float](args[0])
  var s = $(f)

  return some(box(s))

proc builtinItoB*(args: seq[Box]): Option[Box] =
  let i = unbox[int](args[0])
  if i != 0:
    return some(box(true))
  else:
    return some(box(false))

proc builtinFtoB*(args: seq[Box]): Option[Box] =
  let f = unbox[float](args[0])
  if f != 0:
    return some(box(true))
  else:
    return some(box(false))

proc builtinStoB*(args: seq[Box]): Option[Box] =
  let s = unbox[string](args[0])
  if s != "":
    return some(box(true))
  else:
    return some(box(false))

proc builtinLToB*(args: seq[Box]): Option[Box] =
  let l = unbox[seq[Box]](args[0])

  if len(l) == 0:
    return some(box(false))
  else:
    return some(box(true))

proc builtinDToB*(args: seq[Box]): Option[Box] =
  let d = unbox[seq[(Box, Box)]](args[0])

  if len(d) == 0:
    return some(box(false))
  else:
    return some(box(true))

proc builtinIToF*(args: seq[Box]): Option[Box] =
  let
    i = unbox[int](args[0])
    f = float(i)

  return some(box(f))

proc builtinFToI*(args: seq[Box]): Option[Box] =
  let
    f = unbox[float](args[0])
    i = int(f)

  return some(box(i))

proc builtinSplit*(args: seq[Box]): Option[Box] =
  let
    big = unbox[string](args[0])
    small = unbox[string](args[1])
    l = big.split(small)

  var res: seq[Box]

  for item in l:
    res.add(box(item))

  return some(box(res))

proc builtinEcho*(args: seq[Box]): Option[Box] =
  var outStr: string

  for item in args:
    outStr = outStr & unbox[string](item)

  echo outStr

proc builtinEnv*(args: seq[Box]): Option[Box] =
  let arg = unbox[string](args[0])

  return some(box(getEnv(arg)))

proc builtinEnvExists*(args: seq[Box]): Option[Box] =
  let arg = unbox[string](args[0])

  return some(box(existsEnv(arg)))

proc builtinEnvAll*(args: seq[Box]): Option[Box] =
  var s: seq[(Box, Box)]

  for (k, v) in envPairs():
    s.add((box(k), box(v)))

  return some(box(s))

proc builtinStrip*(args: seq[Box]): Option[Box] =
  let
    arg = unbox[string](args[0])
    stripped = arg.strip()

  return some(box(stripped))

proc builtinContainsStrStr*(args: seq[Box]): Option[Box] =
  let
    arg1 = unbox[string](args[0])
    arg2 = unbox[string](args[1])
    res = arg1.contains(arg2)

  return some(box(res))

proc builtinFindFromStart*(args: seq[Box]): Option[Box] =
  let
    s = unbox[string](args[0])
    sub = unbox[string](args[1])
    res = s.find(sub)

  return some(box(res))

proc builtinSlice*(args: seq[Box]): Option[Box] =
  let
    s = unbox[string](args[0])
  var
    startix = unbox[int](args[1])
    endix = unbox[int](args[2])

  if startix < 0:
    startix += s.len()
  if endix < 0:
    endix += s.len()

  try:
    return some(box(s[startix .. endix]))
  except:
    return some(box(""))

proc builtinSliceToEnd*(args: seq[Box]): Option[Box] =
  let
    s = unbox[string](args[0])
    endix = s.len() - 1
  var
    startix = unbox[int](args[1])


  if startix < 0:
    startix += s.len()

  try:
    return some(box(s[startix .. endix]))
  except:
    return some(box(""))

proc builtInAbort*(args: seq[Box]): Option[Box] =
  quit()

when defined(posix):
  proc builtinCmd*(args: seq[Box]): Option[Box] =
    ## Essentially calls the posix system() call, except that, a)
    ## stderr gets merged into stdout, b) the exit code is put at the
    ## start of the output, separated from the rest of the output with
    ## a colon, and c) this ALWAYS drops euid and egid in a
    ## setuid/setgid program, to avoid an attacker using a configuration
    ## file to run arbitrary commands as root.
    ##
    ## Note that this *does* restore privileges in the parent process
    ## once the command returns, but in a multi-threaded program,
    ## this might be worth noting, since threads at startup time
    ## are more likely to need permissions.

    var
      cmd = unbox[string](args[0])
    let
      uid = getuid()
      euid = geteuid()
      gid = getgid()
      egid = getegid()

    if (uid != euid) or (gid != egid):
      discard seteuid(uid)
      discard setegid(gid)

    let
      (output, exitCode) = execCmdEx(cmd)
      exitAsStr = $(exitCode)

    result = some(box("{exitAsStr}:{output}".fmt()))

    if (uid != euid): discard seteuid(euid)
    if (gid != egid): discard setegid(egid)
else:
  ## I don't know the permissions models on any non-posix OS, so
  ## this might be wildly insecure on such systems, as far as I know.
  ## to that end, when posix is not defined, this command is removed
  ## from the defaults.
  proc builtinCmd*(args: seq[Box]): Box =
    var cmd: string = unbox(args[0])

    let
      (output, exitCode) = execCmdEx(cmd)
      exitAsStr = $(exitCode)

    return some(box("{exitAsStr}:{output}".fmt()))

proc newBuiltIn*(s: ConfigState, name: string, fn: BuiltInFn, tinfo: string) =
  let b = BuiltInInfo(fn: fn, tinfo: tinfo.toCon4mType())
  if not s.builtins.contains(name):
    s.builtins[name] = @[b]
  else:
    s.builtins[name].add(b)

proc addDefaultBuiltins*(s: ConfigState) =
  s.newBuiltIn("string", builtinBToS, "(bool) -> string")
  s.newBuiltIn("string", builtinIToS, "(int) -> string")
  s.newBuiltIn("string", builtinFToS, "(float) -> string")
  s.newBuiltIn("bool", builtinIToB, "(int) -> bool")
  s.newBuiltIn("bool", builtinFToB, "(float) -> bool")
  s.newBuiltIn("bool", builtinSToB, "(string) -> bool")
  s.newBuiltIn("bool", builtinLToB, "([@x]) -> bool")
  s.newBuiltIn("bool", builtinDToB, "({@x : @y}) -> bool")
  s.newBuiltIn("float", builtinItoF, "(int) -> float")
  s.newBuiltIn("int", builtinFtoI, "(float) -> int")
  s.newBuiltIn("split", builtinSplit, "(string, string) -> [string]")
  s.newBuiltIn("echo", builtinEcho, "(*string)")
  s.newBuiltIn("env", builtinEnv, "(string) -> string")
  s.newBuiltIn("envExists", builtinEnvExists, "(string) -> bool")
  s.newBuiltIn("env", builtinEnvAll, "() -> {string : string}")
  s.newBuiltIn("strip", builtinStrip, "(string) -> string")
  s.newBuiltIn("contains", builtinContainsStrStr, "(string, string) -> bool")
  s.newBuiltIn("find", builtinFindFromStart, "(string, string) -> int")
  s.newBuiltIn("slice", builtinSlice, "(string, int, int) -> string")
  s.newBuiltIn("slice", builtinSliceToEnd, "(string, int) -> string")
  s.newBuiltIn("abort", builtInAbort, "(string)")
  when defined(posix):
    s.newBuiltIn("run", builtinCmd, "(string) -> string")

proc getBuiltinBySig*(s: ConfigState,
                      name: string,
                      t: Con4mType): Option[BuiltInInfo] =
  if not s.builtins.contains(name):
    return

  let bis = s.builtins[name]

  for item in bis:
    if not isBottom(t, item.tinfo):
      return some(item)

proc isBuiltin*(s: ConfigState, name: string): bool =
  return s.builtins.contains(name)

proc sCall*(s: ConfigState,
            name: string,
            a1: seq[Box],
            tinfo: Con4mType): Option[Box] =
  let optBi = s.getBuiltinBySig(name, tinfo)

  if optBi.isSome():
    let bi = optBi.get()
    return bi.fn(a1)

  raise newException(ValueError, "Signature not found")

proc sCall*(s: ConfigState,
            name: string,
            a1: seq[Box],
            tinfo: string = ""): Option[Box] =
  if tinfo == "":
    assert s.builtins[name].len() == 1
    return s.builtins[name][0].fn(a1)
  else:
    let optBi = s.getBuiltinBySig(name, toCon4mType(tinfo))

    if optBi.isSome():
      let bi = optBi.get()
      return bi.fn(a1)

    raise newException(ValueError, "Signature not found")
