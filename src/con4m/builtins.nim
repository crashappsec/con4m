# Built in functions
import con4m_types
import typecheck
import st
import box

import tables
import osproc
import strformat
import options

when defined(posix):
  import posix

var builtins = initTable[string, seq[BuiltInInfo]]()

proc builtinIToS*(args: seq[Box]): Option[Box] =
  let i = unbox[int](args[0])
  var s = $(i)

  return some(box(s))

# This was necessary to get it to work before boxing; check it before deleting.
# proc builtinIToS*(args: seq[Any]): Any =
#   let f = unbox(args[0])
#   var s = new(string)

#   s[] = $(f)

#   return s.toAny()

proc builtinIToF*(args: seq[Box]): Option[Box] =
  let
    i = unbox[int](args[0])
    f = float(i)

  return some(box(f))

proc builtinFToS*(args: seq[Box]): Option[Box] =
  let
    f = unbox[float](args[0])
    s = $(f)

  return some(box(s))

proc builtinEcho*(args: seq[Box]): Option[Box] =
  var outStr: string

  for item in args:
    outStr = outStr & unbox[string](item)

  echo outStr

proc builtinEmpty*(args: seq[Box]): Option[Box] =
  discard

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

proc newBuiltIn*(name: string, fn: BuiltInFn, tinfo: string) =
  let b = BuiltInInfo(fn: fn, tinfo: tinfo.toCon4mType())
  if not builtins.contains(name):
    builtins[name] = @[b]
  else:
    builtins[name].add(b)

proc addDefaultBuiltins*() =
  newBuiltIn("string", builtinIToS, "(int) -> string")
  newBuiltIn("string", builtinFToS, "(float) -> string")
  newBuiltIn("float", builtinItoF, "(int) -> float")
  newBuiltIn("echo", builtinEcho, "(*string)")
  # Not implemented yet.
  newBuiltIn("env", builtinEmpty, "() -> {string : string}")
  newBuiltIn("strip", builtinEmpty, "(string) -> string")
  newBuiltIn("contains", builtinEmpty, "(string, string) -> bool")
  newBuiltIn("dict", builtInEmpty, "(string) -> {string: string}")
  newBuiltIn("abort", builtInEmpty, "(string)")
  when defined(posix):
    newBuiltIn("run", builtinCmd, "(string) -> string")

proc getBuiltinBySig*(name: string, t: Con4mType): Option[BuiltInInfo] =
  if not builtins.contains(name):
    return

  let bis = builtins[name]

  for item in bis:
    if not isBottom(t, item.tinfo):
      return some(item)

proc isBuiltin*(name: string): bool =
  return builtins.contains(name)

proc sCall*(name: string, a1: seq[Box], tinfo: Con4mType): Option[Box] =
  let optBi = getBuiltinBySig(name, tinfo)

  if optBi.isSome():
    let bi = optBi.get()
    return bi.fn(a1)

  raise newException(ValueError, "Signature not found")

proc sCall*(name: string, a1: seq[Box], tinfo: string = ""): Option[Box] =
  if tinfo == "":
    assert builtins[name].len() == 1
    return builtins[name][0].fn(a1)
  else:
    let optBi = getBuiltinBySig(name, toCon4mType(tinfo))

    if optBi.isSome():
      let bi = optBi.get()
      return bi.fn(a1)

    raise newException(ValueError, "Signature not found")


when isMainModule:
  addDefaultBuiltins()
  var
    fBox = box(8675309.123)
    sBoxOpt = sCall("string", @[fBox], "(float) -> string")
    sBox = sBoxOpt.get()
    s2Box = box("echo hello, world #")

  discard sCall("echo", @[s2Box, sBox])
  let b = sCall("run", @[s2Box]).get()

  echo unbox[string](b)
