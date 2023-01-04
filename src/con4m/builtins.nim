## Built in functions.  I will add more of these as I find them
## useful.  They're all exposed so that you can selectively re-use
## them.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import types
import typecheck
import st
import parse # just for fatal()
import eval
import nimutils
import nimutils/box

import os
import tables
import osproc
import strformat
import strutils
import options
import streams

when defined(posix):
  import posix

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

let
  trueRet = some(pack(true))
  falseRet = some(pack(false))

proc builtinIToS*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast integers to strings.  Exposed as `string(i)` by default.
  let i = unpack[int](args[0])
  var s = $(i)
  var b = pack(s)

  return some(b)

proc builtinBToS*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast bools to strings.  Exposed as `string(b)` by default.
  let b = unpack[bool](args[0])
  if b:
    return some(pack("true"))
  else:
    return some(pack("false"))

proc builtinFToS*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast floats to strings.  Exposed as `string(f)` by default.
  let f = unpack[float](args[0])
  var s = $(f)

  return some(pack(s))

proc builtinItoB*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast integers to booleans (testing for non-zero).  Exposed as
  ## `bool(i)` by default.
  let i = unpack[int](args[0])
  if i != 0:
    return trueRet
  else:
    return falseRet

proc builtinFtoB*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast floats to booleans (testing for non-zero).  Exposed as
  ## `bool(f)` by default.
  let f = unpack[float](args[0])
  if f != 0:
    return trueRet
  else:
    return falseRet

proc builtinStoB*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast strings to booleans (testing for empty strings).  Exposed as
  ## `bool(s)` by default.
  let s = unpack[string](args[0])
  if s != "":
    return trueRet
  else:
    return falseRet

proc builtinLToB*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast lists of any type to booleans (testing for empty lists).
  ## Exposed as `bool(s)` by default.

  # We don't care what types are in the list, so don't unbox them.
  let l = unpack[seq[Box]](args[0])

  if len(l) == 0:
    return falseRet
  else:
    return trueRet

proc builtinDToB*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast dicitonaries of any type to booleans (testing for empty
  ## lists).  Exposed as `bool(s)` by default.

  # Note that the key type should NOT be boxed when we unpack, but we
  # use Box to denote that we don't care about the parameter type.
  let d = unpack[Con4mDict[Box, Box]](args[0])

  if len(d) == 0:
    return falseRet
  else:
    return trueRet

proc builtinIToF*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast an integer to a float.  Exposed as `float(i)` by default.
  let
    i = unpack[int](args[0])
    f = float(i)

  return some(pack(f))

proc builtinFToI*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Cast an float to an int (truncating).  Exposed as `int(f)` by
  ## default.
  let
    f = unpack[float](args[0])
    i = int(f)

  return some(pack(i))

proc builtinSplit*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Takes the first argument, and converts it into a list,
  ## spliting it out based on the pattern in the second string.
  ## This should work as expected from other languages.
  ## Exposed as `split(s1, s2)` by default.
  # Note that, since the item type is known, we only box the
  # top-level, not the items.

  var
    big = unpack[string](args[0])
    small = unpack[string](args[1])
    l = big.split(small)

  return some(pack[seq[string]](l))

proc builtinEcho*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Exposed as `echo(*s)` by default.  Prints the parameters to
  ## stdout, followed by a newline at the end.  Note that this does
  ## NOT add spaces between arguments for you.

  var outStr: string

  for item in args:
    outStr = outStr & unpack[string](item)

  stderr.writeLine(outStr)

proc builtinEnv*(args: seq[Box],
                 unused1 = cast[Con4mScope](nil),
                 unused2 = cast[VarStack](nil),
                 unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Exposed as `env(s)` by default.  Returns the value of the
  ## requested environment variable.  If the environment variable is
  ## NOT set, it will return the empty string.  To distingush between
  ## the environment variable not being set, or the variable being set
  ## to the empty string, use `builtinEnvExists`.

  let arg = unpack[string](args[0])

  return some(pack(getEnv(arg)))

proc builtinEnvExists*(args: seq[Box],
                       unused1 = cast[Con4mScope](nil),
                       unused2 = cast[VarStack](nil),
                       unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns true if the requested variable name is set in the
  ## environment, false if it's not.  Exposed as `envExists(s)` by
  ## default.
  ##
  ## Note that this can be used to distinguish between the variable
  ## not existing, and the variable being set explicitly to the empty
  ## string.
  let arg = unpack[string](args[0])

  return some(pack(existsEnv(arg)))

proc builtinEnvAll*(args: seq[Box],
                    unused1 = cast[Con4mScope](nil),
                    unused2 = cast[VarStack](nil),
                    unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Return a dictionary with all envvars and their values.
  ## Exposed by default as `env()`
  var s = newCon4mDict[string, string]()

  for (k, v) in envPairs():
    s[k] = v

  var packed = pack(s)
  return some(packed)

proc builtinStrip*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Remove leading and trailing white space from a string.
  ## Exposed by default as `strip(s)`
  let
    arg = unpack[string](args[0])
    stripped = arg.strip()

  return some(pack(stripped))

proc builtinContainsStrStr*(args: seq[Box],
                            unused1 = cast[Con4mScope](nil),
                            unused2 = cast[VarStack](nil),
                            unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns true if `s1` contains the substring `s2`.
  ## Exposed by default as `contains(s1, s2)`
  let
    arg1 = unpack[string](args[0])
    arg2 = unpack[string](args[1])
    res = arg1.contains(arg2)

  return some(pack(res))

proc builtinFindFromStart*(args: seq[Box],
                           unused1 = cast[Con4mScope](nil),
                           unused2 = cast[VarStack](nil),
                           unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns the index of the substring `s2`'s first appearence in the
  ## string `s1`, or -1 if it does not appear.  Exposed by default as
  ## `find(s1, s2)`

  let
    s = unpack[string](args[0])
    sub = unpack[string](args[1])
    res = s.find(sub)

  return some(pack(res))

proc builtinSlice*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns the substring of `s` starting at index `start`, not
  ## including index `end`.  The semantics of this are Pythonic, where
  ## -1 works as expected.
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty string.
  ## usually exposed as `slice(s, start, end)`

  let
    s = unpack[string](args[0])
  var
    startix = unpack[int](args[1])
    endix = unpack[int](args[2])

  if startix < 0:
    startix += s.len()
  if endix < 0:
    endix += s.len()

  try:
    return some(pack(s[startix .. endix]))
  except:
    return some(pack(""))

proc builtinSliceToEnd*(args: seq[Box],
                        unused1 = cast[Con4mScope](nil),
                        unused2 = cast[VarStack](nil),
                        unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns the substring of `s` starting at index `start`, until the
  ## end of the string. The semantics of this are Pythonic, where -1
  ## works as expected (to index from the back).
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty string.
  ## usually exposed as `slice(s, start)`

  let
    s = unpack[string](args[0])
    endix = s.len() - 1
  var
    startix = unpack[int](args[1])


  if startix < 0:
    startix += s.len()

  try:
    return some(pack(s[startix .. endix]))
  except:
    return some(pack(""))

proc builtInFormat*(args: seq[Box],
                    attrs: Con4mScope,
                    vars: VarStack,
                    localScope: Con4mScope
                   ): Option[Box] =
  ## We don't error check on string bounds; when an exception gets
  ## raised, SCall will call fatal().

  var
    s = unpack[string](args[0])
    res = newStringOfCap(len(s)*2)
    optEntry: Option[StEntry]
    key: string
    i = 0
  while i < s.len():
    case s[i]
    of '}':
      i += 1
      if i == s.len() or s[i] == '}':
        res.add(s[i])
        i += 1
      else:
        raise newException(Con4mError,
                           "Unescaped } w/o a matching { in format string")
    of '{':
      i = i + 1
      if s[i] == '{':
        res.add(s[i])
        i = i + 1
        continue
      key = newStringOfCap(20)
      while s[i] != '}':
        key.add(s[i])
        i = i + 1
      i = i + 1
      if key.contains("."):
        let parts = key.split(".")
        optEntry = attrs.dottedLookup(parts)
      # These two branches both drop down past the else block to deal
      # w/ their optEntry, but the other branch gets a box directly.
      # Guess I should make an easier interface to attr lookup.
      elif key in attrs.entries:
        optEntry = attrs.lookupAttr(key)
      else:
        let boxed = vars.runtimeVarLookup(key, localScope)
        case boxed.kind
        of MkStr:
          res.add(unpack[string](boxed))
        of MkInt:
          res.add($(unpack[int](boxed)))
        of MkFloat:
          res.add($(unpack[float](boxed)))
        of MkBool:
          res.add($(unpack[bool](boxed)))
        else:
          raise newException(Con4mError, "Error: Invalid type for format " &
            "argument, Don't currently do lists, tuples or dicts")
        continue

      if optEntry.isNone():
        raise newException(Con4mError, "Error in format: item not found")

      let entry = optEntry.get()

      case entry.tInfo.kind
      of TypeString:
        res.add(unpack[string](entry.value.get()))
      of TypeInt:
        res.add($(unpack[int](entry.value.get())))
      of TypeFloat:
        res.add($(unpack[float](entry.value.get())))
      of TypeBool:
        res.add($(unpack[bool](entry.value.get())))
      else:
        raise newException(Con4mError, "Error: Invalid type for format " &
          "argument, can't do lists, tuples or dicts")
    else:
      res.add(s[i])
      i = i + 1

  return some(pack(res))


proc builtInAbort*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Stops the entire program (not just the configuration file).
  ## Generally exposed as `abort()`
  quit()

proc builtInListLen*(args: seq[Box],
                     unused1 = cast[Con4mScope](nil),
                     unused2 = cast[VarStack](nil),
                     unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns the number of elements in the list.
  var list = unpack[seq[Box]](args[0])

  return some(pack(len(list)))

proc builtInStrLen*(args: seq[Box],
                    unused1 = cast[Con4mScope](nil),
                    unused2 = cast[VarStack](nil),
                    unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns the number of bytes in a string.
  var s = unpack[string](args[0])

  return some(pack(len(s)))

proc builtInDictLen*(args: seq[Box],
                     unused1 = cast[Con4mScope](nil),
                     unused2 = cast[VarStack](nil),
                     unused3 = cast[Con4mScope](nil)): Option[Box] =
  ## Returns the number of k,v pairs in a dictionary.
  var dict = unpack[Con4mDict[Box, Box]](args[0])

  return some(pack(len(dict)))

proc builtInDictKeys*(args: seq[Box],
                      unused1 = cast[Con4mScope](nil),
                      unused2 = cast[VarStack](nil),
                      unused3 = cast[Con4mScope](nil)): Option[Box] =

  var
    keys: seq[Box] = newSeq[Box]()
    box = args[0]

  var
    d: OrderedTableRef[Box, Box] = unpack[OrderedTableRef[Box, Box]](box)

  for k, _ in d:
    keys.add(k)

  return some(pack[seq[Box]](keys))

proc builtinListDir*(args: seq[Box], 
                     unused1 = cast[Con4mScope](nil),
                     unused2 = cast[VarStack](nil),
                     unused3 = cast[Con4mScope](nil)): Option[Box] =
  var res: seq[string] = @[]

  let dir = if len(args) == 0: "." else: resolvePath(unpack[string](args[0]))

  for item in walkdir(dir):
    res.add(item.path)

  return some(pack[seq[string]](res))

proc builtinReadFile*(args: seq[Box],
                     unused1 = cast[Con4mScope](nil),
                     unused2 = cast[VarStack](nil),
                     unused3 = cast[Con4mScope](nil)): Option[Box] =
  let f = newFileStream(resolvePath(unpack[string](args[0])), fmRead)
  if f == nil: return some(pack(""))

  result = some(pack(f.readAll()))
  f.close()

proc builtinWriteFile*(args: seq[Box],
                      unused1 = cast[Con4mScope](nil),
                      unused2 = cast[VarStack](nil),
                      unused3 = cast[Con4mScope](nil)): Option[Box] =
  try:
    let f = newFileStream(resolvePath(unpack[string](args[0])), fmWrite)
    f.write(unpack[string](args[1]))
    f.close()
    return trueRet
  except:
    return falseRet

proc builtinJoinPath*(args: seq[Box],
                      unused1 = cast[Con4mScope](nil),
                      unused2 = cast[VarStack](nil),
                      unused3 = cast[Con4mScope](nil)): Option[Box] =
  let res = joinPath(unpack[string](args[0]), unpack[string](args[1]))
  return some(pack(res))

proc builtinCopyFile*(args: seq[Box],
                      unused1 = cast[Con4mScope](nil),
                      unused2 = cast[VarStack](nil),
                      unused3 = cast[Con4mScope](nil)): Option[Box] =

    let
      src = unpack[string](args[0])
      dst = unpack[string](args[1])

    try:
      copyFile(src, dst)
    except:
      return falseRet
      
    return trueRet

proc builtinResolvePath*(args: seq[Box],
                         unused1 = cast[Con4mScope](nil),
                         unused2 = cast[VarStack](nil),
                         unused3 = cast[Con4mScope](nil)): Option[Box] =
    return some(pack(resolvePath(unpack[string](args[0]))))

proc builtinCwd*(args: seq[Box],
                 unused1 = cast[Con4mScope](nil),
                 unused2 = cast[VarStack](nil),
                 unused3 = cast[Con4mScope](nil)): Option[Box] =
    return some(pack(getCurrentDir())) 

proc builtinChdir*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
    let path = unpack[string](args[0])
    try:
      setCurrentDir(path)
      return trueRet
    except:
      return falseRet

proc builtinMkdir*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
    let path = unpack[string](args[0])
    try:
      createDir(path)
      return trueRet
    except:
      return falseRet

proc builtinSetEnv*(args: seq[Box],
                    unused1 = cast[Con4mScope](nil),
                    unused2 = cast[VarStack](nil),
                    unused3 = cast[Con4mScope](nil)): Option[Box] =
    let
      key = unpack[string](args[0])
      val = unpack[string](args[1])
    try:
      putEnv(key, val)
      return trueRet
    except:
      return falseRet

proc builtinIsDir*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
    let path = unpack[string](args[0])

    try:
      if getFileInfo(path, false).kind == pcDir:
        return trueRet
      else:
        return falseRet
    except:
        return falseRet

proc builtinIsFile*(args: seq[Box],
                    unused1 = cast[Con4mScope](nil),
                    unused2 = cast[VarStack](nil),
                    unused3 = cast[Con4mScope](nil)): Option[Box] =
    let path = unpack[string](args[0])

    try:
      if getFileInfo(path, false).kind == pcFile:
        return trueRet
      else:
        return falseRet
    except:
      return falseRet
      
proc builtinIsLink*(args: seq[Box],
                    unused1 = cast[Con4mScope](nil),
                    unused2 = cast[VarStack](nil),
                    unused3 = cast[Con4mScope](nil)): Option[Box] =

    try:
      let
        path = unpack[string](args[0])
        kind = getFileInfo(path, false).kind

      if kind == pcLinkToDir or kind == pcLinkToFile:
        return trueRet
      else:
        return falseRet
    except:
      return falseRet

proc builtinChmod*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
    let
      path = unpack[string](args[0])
      mode = cast[FilePermission](unpack[int](args[0]))

    try:
      setFilePermissions(path, cast[set[FilePermission]](mode))
      return trueRet
    except:
      return falseRet

proc builtinGetPid*(args: seq[Box],
                    unused1 = cast[Con4mScope](nil),
                    unused2 = cast[VarStack](nil),
                    unused3 = cast[Con4mScope](nil)): Option[Box] =
    return some(pack(getCurrentProcessId()))

proc builtinFileLen*(args: seq[Box],
                     unused1 = cast[Con4mScope](nil),
                     unused2 = cast[VarStack](nil),
                     unused3 = cast[Con4mScope](nil)): Option[Box] =
    let path = unpack[string](args[0])

    try:
      return some(pack(getFileSize(path)))
    except:
      return some(pack(-1))

proc builtinMove*(args: seq[Box],
                  unused1 = cast[Con4mScope](nil),
                  unused2 = cast[VarStack](nil),
                  unused3 = cast[Con4mScope](nil)): Option[Box] =

    try:
      let
        src = unpack[string](args[0])
        dst = unpack[string](args[1])
        kind = getFileInfo(src, false).kind

      if kind == pcDir or kind == pcLinkToDir:
          moveDir(src, dst)
          return trueRet
      else:
        moveFile(src, dst)
        return trueRet
    except:
      return falseRet

proc builtinQuote*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
    return some(pack(quoteShell(unpack[string](args[0]))))

proc builtinRm*(args: seq[Box],
                unused1 = cast[Con4mScope](nil),
                unused2 = cast[VarStack](nil),
                unused3 = cast[Con4mScope](nil)): Option[Box] =
    try:
      let
        path = unpack[string](args[0])
        kind = getFileInfo(path, false).kind

      if kind == pcDir or kind == pcLinkToDir:
          removeDir(path, true)
          return trueRet
      else:
        removeFile(path)
        return trueRet
    except:
      return falseRet

proc splitPath*(args: seq[Box],
                unused1 = cast[Con4mScope](nil),
                unused2 = cast[VarStack](nil),
                unused3 = cast[Con4mScope](nil)): Option[Box] =
    var s: seq[string]

    let (head, tail) = splitPath(unpack[string](args[0]))
    s.add(head)
    s.add(tail)

    return some(pack(s))    

proc builtInPad*(args: seq[Box],
                 unused1 = cast[Con4mScope](nil),
                 unused2 = cast[VarStack](nil),
                 unused3 = cast[Con4mScope](nil)): Option[Box] =
  let
    topad = unpack[string](args[0])
    width = unpack[int](args[1])

  if len(topad) >= width:
    return some(pack(topad))

  return some(pack(topad & repeat(' ', width - len(topad))))

when defined(posix):
  proc builtinCmd*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
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
      cmd = unpack[string](args[0])
    let
      uid = getuid()
      euid = geteuid()
      gid = getgid()
      egid = getegid()

    if (uid != euid) or (gid != egid):
      discard seteuid(uid)
      discard setegid(gid)

    let
      (output, _) = execCmdEx(cmd)

    result = some(pack(output))

    if (uid != euid): discard seteuid(euid)
    if (gid != egid): discard setegid(egid)

  proc builtinSystem*(args: seq[Box],
                      unused1 = cast[Con4mScope](nil),
                      unused2 = cast[VarStack](nil),
                      unused3 = cast[Con4mScope](nil)): Option[Box] =
    ## Generally exposed as `system(s)`
    ##
    ## like `run` except returns a tuple containing the output and the exit code.
    var
      cmd = unpack[string](args[0])
    let
      uid = getuid()
      euid = geteuid()
      gid = getgid()
      egid = getegid()

    if (uid != euid) or (gid != egid):
      discard seteuid(uid)
      discard setegid(gid)

    let (output, exitCode) = execCmdEx(cmd)

    var outlist: seq[Box] = @[]

    outlist.add(pack(output))
    outlist.add(pack(exitCode))

    result = some(pack(outlist))

    if (uid != euid): discard seteuid(euid)
    if (gid != egid): discard setegid(egid)

  proc builtinGetUid*(args: seq[Box],
                      unused1 = cast[Con4mScope](nil),
                      unused2 = cast[VarStack](nil),
                      unused3 = cast[Con4mScope](nil)): Option[Box] =
    return some(pack(getuid()))

  proc builtinGetEuid*(args: seq[Box],
                       unused1 = cast[Con4mScope](nil),
                       unused2 = cast[VarStack](nil),
                       unused3 = cast[Con4mScope](nil)): Option[Box] =
    return some(pack(geteuid()))
    
else:
  ## I don't know the permissions models on any non-posix OS, so
  ## this might be wildly insecure on such systems, as far as I know.
  ## to that end, when posix is not defined, this command is removed
  ## from the defaults.
  proc builtinCmd*(args: seq[Box],
                   unused1 = cast[Con4mScope](nil),
                   unused2 = cast[VarStack](nil),
                   unused3 = cast[Con4mScope](nil)): Option[Box] =
    ## An unsafe version of this for non-posix OSes.  On such machines,
    ## it is NOT a default builtin.
    var cmd = unpack[string](args[0])

    let (output, _) = execCmdEx(cmd)

    return some(pack(output))

proc newCoreFunc(s: ConfigState, name: string, tStr: string, fn: BuiltInFn) =
  ## Allows you to associate a NIM function with the correct signature
  ## to a configuration for use as a builtin con4m function. `name` is
  ## the parameter used to specify the name exposed to con4m.  `tinfo`
  ## is the Conform type signature.

  let
    coreName = if fn == nil: "callback" else: "builtin"
    tinfo = tStr.toCon4mType()

  if tinfo.kind != TypeProc:
    raise newException(Con4mError, fmt"Signature provided for {coreName} " &
                       "is not a function signature.")

  let b = if fn == nil:
            FuncTableEntry(kind: FnCallback,
                           tinfo: tinfo,
                           impl: none(Con4mNode),
                           name: name,
                           cannotCycle: false,
                           locked: false)
          else:
            # We intentionally don't set cannotCycle, seenThisCheck
            # or locked because they shouldn't be used for builtins.
            FuncTableEntry(kind: FnBuiltIn,
                           tinfo: tinfo,
                           builtin: fn,
                           name: name)

  if fn == nil:
    if tinfo.retType.isBottom():
      raise newException(Con4mError, fmt"{coreName}: user callbacks must " &
        "have a return type")

  if not s.funcTable.contains(name):
    s.funcTable[name] = @[b]
  else:
    for item in s.funcTable[name]:
      if not isBottom(copyType(tinfo), copyType(item.tinfo)):
        raise newException(Con4mError, fmt"Type for {coreName} conflicts " &
          "with existing entry in the function table")
    s.funcTable[name].add(b)

proc newBuiltIn*(s: ConfigState, name: string, fn: BuiltInFn, tinfo: string) =
  newCoreFunc(s, name, tinfo, fn)

proc newCallback*(s: ConfigState, name: string, tinfo: string) =
  newCoreFunc(s, name, tinfo, nil)

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

  s.newBuiltIn("string", builtinBToS, "f(bool) -> string")
  s.newBuiltIn("string", builtinIToS, "f(int) -> string")
  s.newBuiltIn("string", builtinFToS, "f(float) -> string")
  s.newBuiltIn("bool", builtinIToB, "f(int) -> bool")
  s.newBuiltIn("bool", builtinFToB, "f(float) -> bool")
  s.newBuiltIn("bool", builtinSToB, "f(string) -> bool")
  s.newBuiltIn("bool", builtinLToB, "f([@x]) -> bool")
  s.newBuiltIn("bool", builtinDToB, "f({@x : @y}) -> bool")
  s.newBuiltIn("float", builtinItoF, "f(int) -> float")
  s.newBuiltIn("int", builtinFtoI, "f(float) -> int")
  s.newBuiltIn("split", builtinSplit, "f(string, string) -> [string]")
  s.newBuiltIn("echo", builtinEcho, "f(*string)")
  s.newBuiltIn("env", builtinEnv, "f(string) -> string")
  s.newBuiltIn("envExists", builtinEnvExists, "f(string) -> bool")
  s.newBuiltIn("env", builtinEnvAll, "f() -> {string : string}")
  s.newBuiltIn("strip", builtinStrip, "f(string) -> string")
  s.newBuiltIn("contains", builtinContainsStrStr, "f(string, string) -> bool")
  s.newBuiltIn("find", builtinFindFromStart, "f(string, string) -> int")
  s.newBuiltIn("slice", builtinSlice, "f(string, int, int) -> string")
  s.newBuiltIn("slice", builtinSliceToEnd, "f(string, int) -> string")
  s.newBuiltIn("abort", builtInAbort, "f(string)")
  s.newBuiltIn("len", builtInStrLen, "f(string) -> int")
  s.newBuiltIn("len", builtInListLen, "f([@x]) -> int")
  s.newBuiltIn("len", builtInDictLen, "f({@x : @y}) -> int")
  s.newBuiltIn("format", builtInFormat, "f(string) -> string")
  s.newBuiltIn("keys", builtInDictKeys, "f({int : @y}) -> [string]")
  s.newBuiltIn("keys", builtInDictKeys, "f({string : @y}) -> [string]")
  s.newBuiltIn("pad", builtInPad, "f(string, int) -> string")
  s.newBuiltIn("listDir", builtinListDir, "f() -> [string]")
  s.newBuiltIn("listDir", builtinListDir, "f(string) -> [string]")
  s.newBuiltIn("readFile", builtinReadFile, "f(string) -> string")
  s.newBuiltIn("writeFile", builtinWriteFile, "f(string, string) -> bool")
  s.newBuiltIn("copyFile", builtInCopyFile, "f(string)->bool") #
  s.newBuiltIn("joinPath", builtinJoinPath, "f(string, string) -> string")
  s.newBuiltIn("resolvePath", builtinResolvePath, "f(string)->string")
  s.newBuiltin("cwd", builtinCwd, "f()->string")
  s.newBuiltin("chdir", builtinChdir, "f(string) -> bool")
  s.newBuiltIn("mkdir", builtinMkdir, "f(string) -> bool") # on down
  s.newBuiltIn("setEnv", builtinSetEnv, "f(string, string) -> bool")
  s.newBuiltIn("isDir", builtinIsDir, "f(string) -> bool")
  s.newBuiltIn("isFile", builtinIsFile, "f(string) -> bool")
  s.newBuiltIn("isLink", builtinIsFile, "f(string) -> bool")  
  s.newBuiltIn("chmod", builtinChmod, "f(string, int) -> bool")
  s.newBuiltIn("getpid", builtinGetPid, "f() -> int")
  s.newBuiltIn("fileLen", builtinFileLen, "f(string) -> int")
  s.newBuiltIn("move", builtinMove, "f(string) -> bool")
  s.newBuiltIn("quote", builtinQuote, "f(string)->string")
  s.newBuiltIn("rm", builtinRm, "f(string)->bool")
  s.newBuiltIn("splitPath", builtinSplit, "f(string) -> (string, string)")
  
  when defined(posix):
    s.newBuiltIn("run", builtinCmd, "f(string) -> string")
    s.newBuiltIn("system", builtinSystem, "f(string) -> (string, int)")
    s.newBuiltIn("getuid", builtinGetUid, "f() -> int")
    s.newBuiltIn("geteuid", builtinGetEuid, "f() -> int")


