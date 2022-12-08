## Built in functions.  I will add more of these as I find them
## useful.  They're all exposed so that you can selectively re-use
## them.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

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
import parse # just for fatal()

when defined(posix):
  import posix

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

proc builtinIToS*(args: seq[Box]): Option[Box] =
  ## Cast integers to strings.  Exposed as `string(i)` by default.
  let i = unbox[int](args[0])
  var s = $(i)
  var b = box(s)

  return some(b)

proc builtinBToS*(args: seq[Box]): Option[Box] =
  ## Cast bools to strings.  Exposed as `string(b)` by default.  
  let b = unbox[bool](args[0])
  if b:
    return some(box("true"))
  else:
    return some(box("false"))

proc builtinFToS*(args: seq[Box]): Option[Box] =
  ## Cast floats to strings.  Exposed as `string(f)` by default.    
  let f = unbox[float](args[0])
  var s = $(f)

  return some(box(s))

proc builtinItoB*(args: seq[Box]): Option[Box] =
  ## Cast integers to booleans (testing for non-zero).  Exposed as
  ## `bool(i)` by default.
  let i = unbox[int](args[0])
  if i != 0:
    return some(box(true))
  else:
    return some(box(false))

proc builtinFtoB*(args: seq[Box]): Option[Box] =
  ## Cast floats to booleans (testing for non-zero).  Exposed as
  ## `bool(f)` by default.
  let f = unbox[float](args[0])
  if f != 0:
    return some(box(true))
  else:
    return some(box(false))

proc builtinStoB*(args: seq[Box]): Option[Box] =
  ## Cast strings to booleans (testing for empty strings).  Exposed as
  ## `bool(s)` by default.
  let s = unbox[string](args[0])
  if s != "":
    return some(box(true))
  else:
    return some(box(false))

proc builtinLToB*(args: seq[Box]): Option[Box] =
  ## Cast lists of any type to booleans (testing for empty lists).
  ## Exposed as `bool(s)` by default.
  let l = unbox[seq[Box]](args[0])

  if len(l) == 0:
    return some(box(false))
  else:
    return some(box(true))

proc builtinDToB*(args: seq[Box]): Option[Box] =
  ## Cast dicitonaries of any type to booleans (testing for empty
  ## lists).  Exposed as `bool(s)` by default.

  # Note that the key type should NOT be boxed when we unpack, but we
  # use Box to denote that we don't care about the parameter type.
  let d = unbox[seq[(Box, Box)]](args[0])

  if len(d) == 0:
    return some(box(false))
  else:
    return some(box(true))

proc builtinIToF*(args: seq[Box]): Option[Box] =
  ## Cast an integer to a float.  Exposed as `float(i)` by default.
  let
    i = unbox[int](args[0])
    f = float(i)

  return some(box(f))

proc builtinFToI*(args: seq[Box]): Option[Box] =
  ## Cast an float to an int (truncating).  Exposed as `int(f)` by
  ## default.
  let
    f = unbox[float](args[0])
    i = int(f)

  return some(box(i))

proc builtinSplit*(args: seq[Box]): Option[Box] =
  ## Takes the first argument, and converts it into a list,
  ## spliting it out based on the pattern in the second string.
  ## This should work as expected from other languages.
  ## Exposed as `split(s1, s2)` by default.
  # Note that, since the item type is known, we only box the
  # top-level, not the items.
  
  var
    big = unbox[string](args[0])
    small = unbox[string](args[1])
    l = big.split(small)

  return some(box(l))

proc builtinEcho*(args: seq[Box]): Option[Box] =
  ## Exposed as `echo(*s)` by default.  Prints the parameters to
  ## stdout, followed by a newline at the end.  Note that this does
  ## NOT add spaces between arguments for you.
  
  var outStr: string

  for item in args:
    outStr = outStr & unbox[string](item)

  echo outStr

proc builtinEnv*(args: seq[Box]): Option[Box] =
  ## Exposed as `env(s)` by default.  Returns the value of the
  ## requested environment variable.  If the environment variable is
  ## NOT set, it will return the empty string.  To distingush between
  ## the environment variable not being set, or the variable being set
  ## to the empty string, use `builtinEnvExists`.
  
  let arg = unbox[string](args[0])

  return some(box(getEnv(arg)))

proc builtinEnvExists*(args: seq[Box]): Option[Box] =
  ## Returns true if the requested variable name is set in the
  ## environment, false if it's not.  Exposed as `envExists(s)` by
  ## default.
  ##
  ## Note that this can be used to distinguish between the variable
  ## not existing, and the variable being set explicitly to the empty
  ## string.
  let arg = unbox[string](args[0])

  return some(box(existsEnv(arg)))

proc builtinEnvAll*(args: seq[Box]): Option[Box] =
  ## Return a dictionary with all envvars and their values.
  ## Exposed by default as `env()`
  var s: seq[(string, string)]

  for (k, v) in envPairs():
    s.add((k, v))

  return some(box(s))

proc builtinStrip*(args: seq[Box]): Option[Box] =
  ## Remove leading and trailing white space from a string.
  ## Exposed by default as `strip(s)`
  let
    arg = unbox[string](args[0])
    stripped = arg.strip()

  return some(box(stripped))

proc builtinContainsStrStr*(args: seq[Box]): Option[Box] =
  ## Returns true if `s1` contains the substring `s2`.
  ## Exposed by default as `contains(s1, s2)`
  let
    arg1 = unbox[string](args[0])
    arg2 = unbox[string](args[1])
    res = arg1.contains(arg2)

  return some(box(res))

proc builtinFindFromStart*(args: seq[Box]): Option[Box] =
  ## Returns the index of the substring `s2`'s first appearence in the
  ## string `s1`, or -1 if it does not appear.  Exposed by default as
  ## `find(s1, s2)`
  
  let
    s = unbox[string](args[0])
    sub = unbox[string](args[1])
    res = s.find(sub)

  return some(box(res))

proc builtinSlice*(args: seq[Box]): Option[Box] =
  ## Returns the substring of `s` starting at index `start`, not
  ## including index `end`.  The semantics of this are Pythonic, where
  ## -1 works as expected.
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty string.
  ## usually exposed as `slice(s, start, end)`
  
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
  ## Returns the substring of `s` starting at index `start`, until the
  ## end of the string. The semantics of this are Pythonic, where -1
  ## works as expected (to index from the back).
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty string.
  ## usually exposed as `slice(s, start)`
  
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
  ## Stops the entire program (not just the configuration file).
  ## Generally exposed as `abort()`
  quit()

when defined(posix):
  proc builtinCmd*(args: seq[Box]): Option[Box] =
    ## Generally exposed as `run(s)`
    ##
    ## Essentially calls the posix `system()` call, except that, a)
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
    ##
    ## Currently this is not dropping other Linux capabilities; I've
    ## been developing on my Mac so haven't gotten around to it yet.
    ## Since they can never be returned to a process once dropped,
    ## that might require a fork and a pipe?
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
    ## An unsafe version of this for non-posix OSes.  On such machines,
    ## it is NOT a default builtin.
    var cmd: string = unbox(args[0])

    let
      (output, exitCode) = execCmdEx(cmd)
      exitAsStr = $(exitCode)

    return some(box("{exitAsStr}:{output}".fmt()))

proc newBuiltIn*(s: ConfigState, name: string, fn: BuiltInFn, tinfo: string) =
  ## Allows you to associate a NIM function with the correct signature
  ## to a configuration for use as a builtin con4m function. `name` is
  ## the parameter used to specify the name exposed to con4m.  `tinfo`
  ## is the Conform type signature.
  
  let b = BuiltInInfo(fn: fn, tinfo: tinfo.toCon4mType())
  if not s.builtins.contains(name):
    s.builtins[name] = @[b]
  else:
    s.builtins[name].add(b)

proc addDefaultBuiltins*(s: ConfigState) =
  ## This function loads existing default builtins. It gets called
  ## automatically if you ever call `newConfigState()`, `checkTree(node)`,
  ## `evalTree()`, `evalConfig()`, or `con4m()`.
  ##
  ## That is, you probably don't have to call this, though it will
  ## silently do nothing if you double-add.
  ##
  ## Instead, you probably should just use `newBuiltIn()` to add your
  ## own calls, unless you want to remove or rename things.
  
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
  ## This is not meant to be exposed outside this module, except to
  ## the type checker.
  if not s.builtins.contains(name):
    return

  let bis = s.builtins[name]

  for item in bis:
    if not isBottom(t, item.tinfo):
      return some(item)

proc isBuiltin*(s: ConfigState, name: string): bool =
  ## This is not meant to be exposed outside this module, except to
  ## the type checker.
  return s.builtins.contains(name)

proc sCall*(s: ConfigState,
            name: string,
            a1: seq[Box],
            tinfo: Con4mType,
            node: Con4mNode
           ): Option[Box] =
  ## This is not meant to be exposed outside this module, except to
  ## the evaluator.  This runs a builtin call.

  let optBi = s.getBuiltinBySig(name, tinfo)

  if optBi.isSome():
    let bi = optBi.get()
    return bi.fn(a1)

  fatal("Function {name} not found", node)
