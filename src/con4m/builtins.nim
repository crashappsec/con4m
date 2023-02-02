## Built in functions.  I will add more of these as I find them
## useful.  They're all exposed so that you can selectively re-use
## them.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import os, tables, osproc, strformat, strutils, options, streams
import types, typecheck, st, parse, nimutils, errmsg

when defined(posix):
  import posix

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

let
  trueRet  = some(pack(true))
  falseRet = some(pack(false))

proc c4mIToS*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast integers to strings.  Exposed as `string(i)` by default.
  let i = unpack[int](args[0])
  var s = $(i)
  var b = pack(s)

  return some(b)

proc c4mBToS*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast bools to strings.  Exposed as `string(b)` by default.
  let b = unpack[bool](args[0])
  if b:
    return some(pack("true"))
  else:
    return some(pack("false"))

proc c4mFToS*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast floats to strings.  Exposed as `string(f)` by default.
  let f = unpack[float](args[0])
  var s = $(f)

  return some(pack(s))

proc c4mItoB*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast integers to booleans (testing for non-zero).  Exposed as
  ## `bool(i)` by default.
  let i = unpack[int](args[0])
  if i != 0:
    return trueRet
  else:
    return falseRet

proc c4mFtoB*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast floats to booleans (testing for non-zero).  Exposed as
  ## `bool(f)` by default.
  let f = unpack[float](args[0])
  if f != 0:
    return trueRet
  else:
    return falseRet

proc c4mStoB*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast strings to booleans (testing for empty strings).  Exposed as
  ## `bool(s)` by default.
  let s = unpack[string](args[0])
  if s != "":
    return trueRet
  else:
    return falseRet

proc c4mLToB*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast lists of any type to booleans (testing for empty lists).
  ## Exposed as `bool(s)` by default.

  # We don't care what types are in the list, so don't unbox them.
  let l = unpack[seq[Box]](args[0])

  if len(l) == 0:
    return falseRet
  else:
    return trueRet

proc c4mDToB*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast dicitonaries of any type to booleans (testing for empty
  ## lists).  Exposed as `bool(s)` by default.

  # Note that the key type should NOT be boxed when we unpack, but we
  # use Box to denote that we don't care about the parameter type.
  let d = unpack[Con4mDict[Box, Box]](args[0])

  if len(d) == 0:
    return falseRet
  else:
    return trueRet

proc c4mIToF*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast an integer to a float.  Exposed as `float(i)` by default.
  let
    i = unpack[int](args[0])
    f = float(i)

  return some(pack(f))

proc c4mFToI*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Cast an float to an int (truncating).  Exposed as `int(f)` by
  ## default.
  let
    f = unpack[float](args[0])
    i = int(f)

  return some(pack(i))

proc c4mSplit*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Takes the first argument, and converts it into a list,
  ## spliting it out based on the pattern in the second string.
  ## This should work as expected from other languages.
  ## Exposed as `split(s1, s2)` by default.
  # Note that, since the item type is known, we only box the
  # top-level, not the items.

  var
    big   = unpack[string](args[0])
    small = unpack[string](args[1])
    l     = big.split(small)

  return some(pack[seq[string]](l))

proc c4mEcho*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Exposed as `echo(*s)` by default.  Prints the parameters to
  ## stdout, followed by a newline at the end.  Note that this does
  ## NOT add spaces between arguments for you.

  var outStr: string

  for item in args:
    outStr = outStr & unpack[string](item)

  stderr.writeLine(outStr)

proc c4mEnv*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Exposed as `env(s)` by default.  Returns the value of the
  ## requested environment variable.  If the environment variable is
  ## NOT set, it will return the empty string.  To distingush between
  ## the environment variable not being set, or the variable being set
  ## to the empty string, use `c4mEnvExists`.

  let arg = unpack[string](args[0])

  return some(pack(getEnv(arg)))

proc c4mEnvExists*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns true if the requested variable name is set in the
  ## environment, false if it's not.  Exposed as `envExists(s)` by
  ## default.
  ##
  ## Note that this can be used to distinguish between the variable
  ## not existing, and the variable being set explicitly to the empty
  ## string.
  let arg = unpack[string](args[0])

  return some(pack(existsEnv(arg)))

proc c4mEnvAll*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Return a dictionary with all envvars and their values.
  ## Exposed by default as `env()`
  var s = newCon4mDict[string, string]()

  for (k, v) in envPairs():
    s[k] = v

  var packed = pack(s)
  return some(packed)

proc c4mStrip*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Remove leading and trailing white space from a string.
  ## Exposed by default as `strip(s)`
  let
    arg = unpack[string](args[0])
    stripped = arg.strip()

  return some(pack(stripped))

proc c4mContainsStrStr*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns true if `s1` contains the substring `s2`.
  ## Exposed by default as `contains(s1, s2)`
  let
    arg1 = unpack[string](args[0])
    arg2 = unpack[string](args[1])
    res = arg1.contains(arg2)

  return some(pack(res))

proc c4mFindFromStart*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the index of the substring `s2`'s first appearence in the
  ## string `s1`, or -1 if it does not appear.  Exposed by default as
  ## `find(s1, s2)`

  let
    s = unpack[string](args[0])
    sub = unpack[string](args[1])
    res = s.find(sub)

  return some(pack(res))

proc c4mSlice*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the substring of `s` starting at index `start`, not
  ## including index `end`.  The semantics of this are Pythonic, where
  ## -1 works as expected.
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty string.
  ## usually exposed as `slice(s, start, end)`

  let
    s       = unpack[string](args[0])
  var
    startix = unpack[int](args[1])
    endix   = unpack[int](args[2])

  if startix < 0:
    startix += s.len()
  if endix < 0:
    endix += s.len()

  try:
    return some(pack(s[startix .. endix]))
  except:
    return some(pack(""))

proc c4mSliceToEnd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the substring of `s` starting at index `start`, until the
  ## end of the string. The semantics of this are Pythonic, where -1
  ## works as expected (to index from the back).
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty string.
  ## usually exposed as `slice(s, start)`

  let
    s       = unpack[string](args[0])
    endix   = s.len() - 1
  var
    startix = unpack[int](args[1])


  if startix < 0:
    startix += s.len()

  try:
    return some(pack(s[startix .. endix]))
  except:
    return some(pack(""))

proc c4mFormat*(args: seq[Box], state: ConfigState): Option[Box] =
  ## We don't error check on string bounds; when an exception gets
  ## raised, SCall will call fatal().
  var
    s   = unpack[string](args[0])
    res = newStringOfCap(len(s)*2)
    i   = 0
    key:    string
    `box?`: Option[Box]
    box:    Box = nil

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

      `box?` = state.attrLookup(key)
      if `box?`.isNone() and '.' notin key:
        let aoe = state.nodeStash.attrScope.attrLookup([key], 0, vlAttrUse)
        if aoe.isA(AttrOrSub):
          `box?` = aoe.get(AttrOrSub).get(Attribute).attrToVal()
      if `box?`.isSome():
        box = `box?`.get()
      else:
        try:
          box = runtimeVarLookup(state.frames, key)
        except:
          raise newException(Con4mError, fmt"Error in format: {key} not found")

      case box.kind
        of MkStr:
          res.add(unpack[string](box))
        of MkInt:
          res.add($(unpack[int](box)))
        of MkFloat:
          res.add($(unpack[float](box)))
        of MkBool:
          res.add($(unpack[bool](box)))
        else:
          raise newException(Con4mError, "Error: Invalid type for format " &
            "argument, Don't currently do lists, tuples or dicts")
    else:
      res.add(s[i])
      i = i + 1

  return some(pack(res))

proc c4mAbort*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Stops the entire program (not just the configuration file).
  ## Generally exposed as `abort()`
  quit()

proc c4mListLen*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the number of elements in the list.
  var list = unpack[seq[Box]](args[0])

  return some(pack(len(list)))

proc c4mListContains*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    list = unpack[seq[Box]](args[0])
    val  = unpack[Box](args[1])

  for item in list:
    if item == val:
      return trueRet

  return falseRet

proc c4mDictContains*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    list   = unpack[OrderedTableRef[Box, Box]](args[0])
    target = unpack[Box](args[1])

  for key, val in list:
    if target == key:
      return trueRet

  return falseRet

proc c4mStrLen*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the number of bytes in a string.
  var s = unpack[string](args[0])

  return some(pack(len(s)))

proc c4mDictLen*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the number of k,v pairs in a dictionary.
  var dict = unpack[Con4mDict[Box, Box]](args[0])

  return some(pack(len(dict)))

proc c4mDictKeys*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    keys: seq[Box]               = newSeq[Box]()
    box                          = args[0]
    d: OrderedTableRef[Box, Box] = unpack[OrderedTableRef[Box, Box]](box)

  for k, _ in d:
    keys.add(k)

  return some(pack[seq[Box]](keys))

proc c4mDictValues*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    values: seq[Box]             = newSeq[Box]()
    box                          = args[0]
    d: OrderedTableRef[Box, Box] = unpack[OrderedTableRef[Box, Box]](box)

  for _, v in d:
    values.add(v)

  return some(pack[seq[Box]](values))

proc c4mDictItems*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    tup:   seq[Box] # For the output tuples.
    items: seq[Box]              = newSeq[Box]()
    box                          = args[0]
    d: OrderedTableRef[Box, Box] = unpack[OrderedTableRef[Box, Box]](box)

  for k, v in d:
    tup = newSeq[Box]()
    tup.add(k)
    tup.add(v)
    items.add(pack[seq[Box]](tup))

  return some(pack[seq[Box]](items))

proc c4mListDir*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var res: seq[string] = @[]

  let dir = if len(args) == 0: "."
            else: resolvePath(unpack[string](args[0]))

  unprivileged:
    for item in walkdir(dir):
      res.add(item.path)

  return some(pack[seq[string]](res))

proc c4mReadFile*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  unprivileged:
    let f = newFileStream(resolvePath(unpack[string](args[0])), fmRead)
    if f == nil:
      result = some(pack(""))
    else:
      result = some(pack(f.readAll()))
      f.close()

proc c4mWriteFile*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  try:
    let f = newFileStream(resolvePath(unpack[string](args[0])), fmWrite)
    f.write(unpack[string](args[1]))
    f.close()
    return trueRet
  except:
    return falseRet

proc c4mJoinPath*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let res = joinPath(unpack[string](args[0]), unpack[string](args[1]))
  return some(pack(res))

proc c4mCopyFile*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    src = unpack[string](args[0])
    dst = unpack[string](args[1])

  unprivileged:
    try:
      copyFile(src, dst)
      result = trueRet
    except:
      result = falseRet

proc c4mResolvePath*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(resolvePath(unpack[string](args[0]))))

proc c4mCwd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(getCurrentDir()))

proc c4mChdir*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let path = unpack[string](args[0])
  unprivileged:
    try:
      setCurrentDir(path)
      result = trueRet
    except:
      result = falseRet

proc c4mMkdir*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let path = unpack[string](args[0])
  unprivileged:
    try:
      createDir(path)
      result = trueRet
    except:
      result = falseRet

proc c4mSetEnv*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    key = unpack[string](args[0])
    val = unpack[string](args[1])
  unprivileged:
    try:
      putEnv(key, val)
      result = trueRet
    except:
      result = falseRet

proc c4mIsDir*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let path = unpack[string](args[0])

  unprivileged:
    try:
      if getFileInfo(path, false).kind == pcDir:
        result = trueRet
      else:
        result = falseRet
    except:
        result = falseRet

proc c4mIsFile*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let path = unpack[string](args[0])

  unprivileged:
    try:
      if getFileInfo(path, false).kind == pcFile:
        result = trueRet
      else:
        result = falseRet
    except:
      result = falseRet

proc c4mIsLink*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  unprivileged:
    try:
      let
        path = unpack[string](args[0])
        kind = getFileInfo(path, false).kind

      if kind == pcLinkToDir or kind == pcLinkToFile:
        result = trueRet
      else:
        result = falseRet
    except:
      result = falseRet

proc c4mChmod*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    path = unpack[string](args[0])
    mode = cast[FilePermission](unpack[int](args[0]))

  unprivileged:
    try:
      setFilePermissions(path, cast[set[FilePermission]](mode))
      result = trueRet
    except:
      result = falseRet

proc c4mGetPid*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(getCurrentProcessId()))

proc c4mFileLen*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let path = unpack[string](args[0])

  unprivileged:
    try:
      result = some(pack(getFileSize(path)))
    except:
      result = some(pack(-1))

proc c4mMove*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  unprivileged:
    try:
      let
        src = unpack[string](args[0])
        dst = unpack[string](args[1])
        kind = getFileInfo(src, false).kind

      if kind == pcDir or kind == pcLinkToDir:
        moveDir(src, dst)
        result = trueRet
      else:
        moveFile(src, dst)
        result = trueRet
    except:
      result = falseRet

proc c4mQuote*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(quoteShell(unpack[string](args[0]))))

proc c4mGetOsName*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(hostOS))

proc c4mGetArch*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(hostCPU))

proc c4mGetArgv*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(commandLineParams()))

proc c4mGetExePath*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(resolvePath(getAppFilename())))

proc c4mGetExeName*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(getAppFilename().splitPath().tail))

proc c4mIntHigh*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(high(int64)))

proc c4mIntLow*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(low(int64)))

proc c4mBitOr*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    o1 = unpack[int](args[0])
    o2 = unpack[int](args[1])

  return some(pack(o1 or o2))

proc c4mBitAnd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    o1 = unpack[int](args[0])
    o2 = unpack[int](args[1])

  return some(pack(o1 and o2))

proc c4mBitXor*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    o1 = unpack[int](args[0])
    o2 = unpack[int](args[1])

  return some(pack(o1 xor o2))

proc c4mBitShl*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    o1 = unpack[int](args[0])
    o2 = unpack[int](args[1])

  return some(pack(o1 shl o2))

proc c4mBitShr*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    o1 = unpack[int](args[0])
    o2 = unpack[int](args[1])

  return some(pack(o1 shr o2))

proc c4mBitNot*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let innum = unpack[int](args[0])

  return some(pack(not innum))

var replacementState: Option[ConfigState] = none(ConfigState)

proc setReplacementState*(state: ConfigState) =
  replacementState = some(state)

proc clearReplacementState*() =
  replacementState = none(ConfigState)

proc c4mSections*(args: seq[Box], localState: ConfigState): Option[Box] =
  let
    name  = unpack[string](args[0])
    state = replacementState.getOrElse(localState)
    aOrE  = attrLookup(state.attrs, name.split("."), 0, vlExists)

  if aOrE.isA(AttrErr):
    return some(pack[seq[string]](@[]))

  let
    aOrS             = aOrE.get(AttrOrSub)

  if aOrS.isA(Attribute):
    return some(pack[seq[string]](@[]))
  var
    sec              = aOrS.get(AttrScope)
    res: seq[string] = @[]

  for key, aOrS in sec.contents:
    if aOrS.isA(AttrScope):
      res.add(key)


  return some(pack(res))

proc c4mRm*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
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

proc c4mSplitPath*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var s: seq[string]

  let (head, tail) = splitPath(unpack[string](args[0]))
  s.add(head)
  s.add(tail)

  return some(pack(s))

proc c4mPad*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    topad = unpack[string](args[0])
    width = unpack[int](args[1])

  if len(topad) >= width:
    return some(pack(topad))

  return some(pack(topad & repeat(' ', width - len(topad))))

when defined(posix):
  proc c4mCmd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
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

    unprivileged:
      let (output, _) = execCmdEx(cmd)
      result = some(pack(output))

  proc c4mSystem*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    ## Generally exposed as `system(s)`
    ##
    ## like `run` except returns a tuple containing the output and the
    ## exit code.
    var
      cmd               = unpack[string](args[0])
      outlist: seq[Box] = @[]

    unprivileged:
      let (output, exitCode) = execCmdEx(cmd)
      outlist.add(pack(output))
      outlist.add(pack(exitCode))

    result = some(pack(outlist))

  proc c4mGetUid*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    return some(pack(getuid()))

  proc c4mGetEuid*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    return some(pack(geteuid()))

else:
  ## I don't know the permissions models on any non-posix OS, so
  ## this might be wildly insecure on such systems, as far as I know.
  ## to that end, when posix is not defined, this command is removed
  ## from the defaults.
  proc c4mCmd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    ## An unsafe version of this for non-posix OSes.  On such machines,
    ## it is NOT a default builtin.
    var cmd = unpack[string](args[0])

    let (output, _) = execCmdEx(cmd)

    return some(pack(output))

proc newCoreFunc*(s: ConfigState, name: string, tStr: string, fn: BuiltInFn) =
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
            FuncTableEntry(kind:        FnCallback,
                           tinfo:       tinfo,
                           impl:        none(Con4mNode),
                           name:        name,
                           cannotCycle: false,
                           locked:      false)
          else:
            # We intentionally don't set cannotCycle, seenThisCheck
            # or locked because they shouldn't be used for builtins.
            FuncTableEntry(kind:    FnBuiltIn,
                           tinfo:   tinfo,
                           builtin: fn,
                           name:    name)

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
  try:
    newCoreFunc(s, name, tinfo, fn)
  except:
    let msg = getCurrentExceptionMsg()
    raise newException(ValueError,
                       fmt"When adding builtin '{name}({tinfo})': {msg}")

proc newCallback*(s: ConfigState, name: string, tinfo: string) =
  try:
    newCoreFunc(s, name, tinfo, nil)
  except:
    let msg = getCurrentExceptionMsg()
    raise newException(ValueError,
                       fmt"When adding callback '{name}({tinfo})': {msg}")

type BiFn = BuiltInFn # Alias the type to avoid cursed line wrap.
const defaultBuiltins = [
  # Type conversion operations
  (1,   "bool",     BiFn(c4mIToB),           "f(int) -> bool"),
  (2,   "bool",     BiFn(c4mFToB),           "f(float) -> bool"),
  (3,   "bool",     BiFn(c4mSToB),           "f(string) -> bool"),
  (4,   "bool",     BiFn(c4mLToB),           "f([@x]) -> bool"),
  (5,   "bool",     BiFn(c4mDToB),           "f({@x : @y}) -> bool"),
  (6,   "float",    BiFn(c4mItoF),           "f(int) -> float"),
  (7,   "int",      BiFn(c4mFtoI),           "f(float) -> int"),
  (8,   "string",   BiFn(c4mBToS),           "f(bool) -> string"),
  (9,   "string",   BiFn(c4mIToS),           "f(int) -> string"),
  (10,  "string",   BiFn(c4mFToS),           "f(float) -> string"),

  # String manipulation functions.
  (101, "contains", BiFn(c4mContainsStrStr), "f(string, string) -> bool"),
  (102, "find",     BiFn(c4mFindFromStart),  "f(string, string) -> int"),
  (103, "len",      BiFn(c4mStrLen),         "f(string) -> int"),
  (104, "slice",    BiFn(c4mSliceToEnd),     "f(string, int) -> string"),
  (105, "slice",    BiFn(c4mSlice),          "f(string, int, int) -> string"),
  (106, "split",    BiFn(c4mSplit),          "f(string, string) -> [string]"),
  (107, "strip",    BiFn(c4mStrip),          "f(string) -> string"),
  (108, "pad",      BiFn(c4mPad),            "f(string, int) -> string"),
  (109, "format",   BiFn(c4mFormat),         "f(string) -> string"),

  # Container (list and dict) basics.
  (201, "len",      BiFn(c4mListLen),         "f([@x]) -> int"),
  (202, "len",      BiFn(c4mDictLen),         "f({@x : @y}) -> int"),
  (203, "keys",     BiFn(c4mDictKeys),        "f({@x : @y}) -> [@x]"),
  (204, "values",   BiFn(c4mDictValues),      "f({@x: @y}) -> [@y]"),
  (205, "items",    BiFn(c4mDictItems),       "f({@x: @y}) -> [(@x, @y)]"),
  (206, "contains", BiFn(c4mListContains),    "f([@x], @x) -> bool"),
  (207, "contains", BiFn(c4mDictContains),    "f({@x : @y}, @x) -> bool"),

  # File system routines
  (301, "listDir",     BiFn(c4mListDir),      "f() -> [string]"),
  (302, "listDir",     BiFn(c4mListDir),      "f(string) -> [string]"),
  (303, "readFile",    BiFn(c4mReadFile),     "f(string) -> string"),
  (304, "writeFile",   BiFn(c4mWriteFile),    "f(string, string) -> bool"),
  (305, "copyFile",    BiFn(c4mCopyFile),     "f(string, string) -> bool"),
  (306, "moveFile",    BiFn(c4mMove),         "f(string, string) -> bool"),
  (307, "rmFile",      BiFn(c4mRm),           "f(string)->bool"),
  (308, "joinPath",    BiFn(c4mJoinPath),     "f(string, string) -> string"),
  (309, "resolvePath", BiFn(c4mResolvePath),  "f(string) -> string"),
  (310, "splitPath",   BiFn(c4mSplitPath),    "f(string) -> (string, string)"),
  (311, "cwd",         BiFn(c4mCwd),          "f()->string"),
  (312, "chdir",       BiFn(c4mChdir),        "f(string) -> bool"),
  (313, "mkdir",       BiFn(c4mMkdir),        "f(string) -> bool"),
  (314, "isDir",       BiFn(c4mIsDir),        "f(string) -> bool"),
  (315, "isFile",      BiFn(c4mIsFile),       "f(string) -> bool"),
  (316, "isLink",      BiFn(c4mIsFile),       "f(string) -> bool"),
  (317, "chmod",       BiFn(c4mChmod),        "f(string, int) -> bool"),
  (318, "fileLen",     BiFn(c4mFileLen),      "f(string) -> int"),

  # System routines
  (401, "echo",         BiFn(c4mEcho),         "f(*string)"),
  (402, "abort",        BiFn(c4mAbort),        "f(string)"),
  (403, "env",          BiFn(c4mEnvAll),       "f() -> {string : string}"),
  (404, "env",          BiFn(c4mEnv),          "f(string) -> string"),
  (405, "envExists",    BiFn(c4mEnvExists),    "f(string) -> bool"),
  (406, "setEnv",       BiFn(c4mSetEnv),       "f(string, string) -> bool"),
  (407, "getpid",       BiFn(c4mGetPid),       "f() -> int"),
  (408, "quote",        BiFn(c4mQuote),        "f(string)->string"),
  (409, "osname",       BiFn(c4mGetOsName),    "f() -> string"),
  (410, "arch",         BiFn(c4mGetArch),      "f() -> string"),
  (411, "program_args", BiFn(c4mGetArgv),      "f() -> [string]"),
  (412, "program_path", BiFn(c4mGetExePath),   "f() -> string"),
  (413, "program_name", BiFn(c4mGetExeName),   "f() -> string"),
  (414, "high",         BiFn(c4mIntHigh),      "f() -> int"),
  (415, "low",          BiFn(c4mIntLow),       "f() -> int"),

  # Binary ops
  (501, "bitor",        BiFn(c4mBitOr),        "f(int, int) -> int"),
  (502, "bitand",       BiFn(c4mBitAnd),       "f(int, int) -> int"),
  (503, "xor",          BiFn(c4mBitXor),       "f(int, int) -> int"),
  (504, "shl",          BiFn(c4mBitShl),       "f(int, int) -> int"),
  (505, "shr",          BiFn(c4mBitShr),       "f(int, int) -> int"),
  (506, "bitnot",       BiFn(c4mBitNot),       "f(int) -> int"),

  # Con4m-specific stuff
  (601, "sections",     BiFn(c4mSections),     "f(string) -> [string]"),
]

when defined(posix):
  const posixBuiltins = [
    (901, "run",       BiFn(c4mCmd),          "f(string) -> string"),
    (902, "system",    BiFn(c4mSystem),       "f(string) -> (string, int)"),
    (903, "getuid",    BiFn(c4mGetUid),       "f() -> int"),
    (904, "geteuid",   BiFn(c4mGetEuid),      "f() -> int")
   ]

proc addBuiltinSet(s, bi, exclusions: auto) {.inline.} =
  for item in bi:
    let (id, name, impl, sig) = item

    if id in exclusions:
      continue

    s.newBuiltIn(name, impl, sig)

proc addDefaultBuiltins*(s: ConfigState, exclusions: openarray[int] = []) =
  ## This function loads existing default builtins. It gets called
  ## automatically if you ever call `newConfigState()`, `checkTree(node)`,
  ## `evalTree()`, `evalConfig()`, or `con4m()`.
  ##
  ## That is, you probably don't have to call this, though it will
  ## silently do nothing if you double-add.
  ##
  ## Instead, you probably should just use `newBuiltIn()` to add your
  ## own calls, unless you want to remove or rename things.
  ## These calls are grouped here in categories, matching the documentation.
  ## The ordering in the code above does not currently match; it is closer to
  ## the historical order in which things were added.
  ##
  ## You can pass exclusions into the second parameter, identifying
  ## the unique ID of functions you want to exclude.  If you pass in
  ## invalid values, they're ignored.

  s.addBuiltinSet(defaultBuiltins, exclusions)

  when defined(posix):
    s.addBuiltinSet(posixBuiltins, exclusions)
