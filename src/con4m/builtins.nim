## Built in functions.  I will add more of these as I find them
## useful.  They're all exposed so that you can selectively re-use
## them.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022 - 2023

import os, tables, osproc, strformat, strutils, options, streams, base64,
       macros, nimSHA2, types, typecheck, st, parse, nimutils, errmsg,
       otherlits, treecheck, dollars, unicode

when defined(posix):
  import posix

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

template c4mException*(m: string): untyped =
  newException(Con4mError, m)

let
  trueRet  = some(pack(true))
  falseRet = some(pack(false))

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

proc c4mSelfRet*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## This is used for a cast of arg[0] to a type with the exact same
  ## representation when boxed. Could technically no-op it, but
  ## whatever.
  return some(args[0])

macro toOtherLitDecl(name, call, err: untyped): untyped =
  quote do:
    proc `name`*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
      let
        str = unpack[string](args[0])
        opt = `call`(str)
      if opt.isSome():
        return some(pack(opt.get()))
      raise c4mException(`err`)

toOtherLitDecl(c4mSToDur,      otherLitToNativeDuration, "Invalid duration")
toOtherLitDecl(c4mSToIP,       otherLitToIPAddr,         "Invalid IP address")
toOtherLitDecl(c4mSToCIDR,     otherLitToCIDR,           "Invalid CIDR spec")
toOtherLitDecl(c4mSToSize,     otherLitToNativeSize,     "Invalid size spec")
toOtherLitDecl(c4mSToDate,     otherLitToNativeDate,     "Invalid date syntax")
toOtherLitDecl(c4mSToTime,     otherLitToNativeTime,     "Invalid time syntax")
toOtherLitDecl(c4mSToDateTime, otherLitToNativeDateTime, "Invalid date/time")

proc c4mDurAsMsec*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    duration = unpack[int](args[0])
    msec     = duration div 1000

  result = some(pack(msec))

proc c4mDurAsSec*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    duration = unpack[int](args[0])
    sec      = duration div 1000000

  result = some(pack(sec))

proc c4mStrToType*(args: seq[Box], unused: ConfigState): Option[Box] =
  try:    return some(pack(toCon4mType(unpack[string](args[0]))))
  except: raise c4mException(getCurrentExceptionMsg())

proc c4mStrToChars*(args: seq[Box], unused: ConfigState): Option[Box] =
  var s: seq[int] = @[]
  for rune in toRunes(unpack[string](args[0])):
    s.add(int(rune))
  result = some(pack(s))

proc c4mStrToBytes*(args: seq[Box], unused: ConfigState): Option[Box] =
  var s: seq[int] = @[]
  for ch in unpack[string](args[0]):
    s.add(int(ch))
  result = some(pack(s))

proc c4mCharsToString*(args: seq[Box], unused: ConfigState): Option[Box] =
  var r = ""
  for num in unpack[seq[Box]](args[0]):
    r.add($(Rune(unpack[int](num))))

  result = some(pack(r))

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

proc c4mToString*(args: seq[Box], state: ConfigState): Option[Box] =
  let
    actNode  = state.nodeStash.children[1]
    itemType = actNode.children[0].getType()

  return some(pack(oneArgToString(itemType, args[0])))

proc c4mEcho*(args: seq[Box], state: ConfigState): Option[Box] =
  ## Exposed as `echo(*s)` by default.  Prints the parameters to
  ## stdout, followed by a newline at the end.

  var
    actNode = state.nodeStash.children[1]
    toPrint: seq[string] = @[]

  for i, item in args:
    let typeinfo = actNode.children[i].getType()
    toPrint.add(oneArgToString(typeInfo, item))

  stderr.writeLine(toPrint.join(" "))

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

proc c4mEnvAll*(args: seq[Box] = @[], unused = ConfigState(nil)): Option[Box] =
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
    stripped = unicode.strip(arg)

  return some(pack(stripped))

proc c4mContainsStrStr*(args: seq[Box],
                        unused = ConfigState(nil)): Option[Box] =
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

proc c4mListSlice*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  ## Returns the sub-array of `s` starting at index `start`, not
  ## including index `end`.  The semantics of this are Pythonic, where
  ## -1 works as expected.
  ##
  ## Note that an index out of bounds will not error. If both
  ## indicies are out of bounds, you'll get the empty list.
  ## usually exposed as `slice(s, start, end)`

  let
    s       = unpack[seq[Box]](args[0])
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
    return some(pack[seq[Box]](@[]))

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
        raise c4mException("Unescaped } w/o a matching { in format string")
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
          raise c4mException(fmt"Error in format: {key} not found")

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
          raise c4mException("Error: Invalid type for format argument; " &
                             "container types not supported.")
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
  let f = newFileStream(resolvePath(unpack[string](args[0])), fmWrite)

  if f == nil: return falseRet

  f.write(unpack[string](args[1]))
  f.close()
  return trueRet

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

proc c4mTmpWrite*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  # args[0]: contents to write. args[1]: file extension. ret: full path
  let path = joinPath("/tmp", getUlid(dash=false) & unpack[string](args[1]))
  try:
    let f = newFileStream(path, fmWrite)
    f.write(unpack[string](args[0]))
    f.close()
    return some(pack(path))
  except:
    return some(pack(""))

proc c4mBase64*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(encode(unpack[string](args[0]))))

proc c4mBase64Web*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(encode(unpack[string](args[0]), safe = true )))

proc c4mDecode64*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  try:
    return some(pack(decode(unpack[string](args[0]))))
  except:
    raise c4mException(getCurrentExceptionMsg())

proc c4mToHex*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(toHex(unpack[string](args[0]))))

proc c4mInttoHex*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(toHex(unpack[int](args[0]))))

proc c4mFromHex*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  try:
    return some(pack(parseHexStr(unpack[string](args[0]))))
  except:
    raise c4mException(getCurrentExceptionMsg())

proc c4mSha256*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var shaCtx = initSHA[SHA256]()
  shaCtx.update(unpack[string](args[0]))
  return some(pack(shaCtx.final().toHex().toLowerAscii()))

proc c4mSha512*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var shaCtx = initSHA[SHA512]()
  shaCtx.update(unpack[string](args[0]))
  return some(pack(shaCtx.final().toHex().toLowerAscii()))

proc c4mUpper*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(unicode.toUpper(unpack[string](args[0]))))

proc c4mLower*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(unicode.toLower(unpack[string](args[0]))))

proc c4mJoin*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    arr    = unpack[seq[string]](args[0])
    joiner = unpack[string](args[1])
  return some(pack(arr.join(joiner)))

proc c4mReplace*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    baseString  = unpack[string](args[0])
    toReplace   = unpack[string](args[1])
    replaceWith = unpack[string](args[2])

  return some(pack(baseString.replace(toReplace, replaceWith)))

template simpleRuneFunc(arr: seq[Box], f: untyped): Option[Box] =
  let
    i = unpack[int](args[0])
    r = Rune(i)

  some(pack(f(r)))

proc c4mUTF8Len*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  simpleRuneFunc(args, size)

proc c4mIsCombining*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  simpleRuneFunc(args, isCombining)

proc c4mIsLower*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  simpleRuneFunc(args, isLower)

proc c4mIsUpper*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  simpleRuneFunc(args, isUpper)

proc c4mIsSpace*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  simpleRuneFunc(args, isWhiteSpace)

proc c4mIsAlpha*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  simpleRuneFunc(args, isAlpha)

proc c4mIsNum*(args: seq[Box], unused = ConfigStatE(nil)): Option[Box] =
  let
    i = unpack[int](args[0])
    r = Rune(i)

  return if i >= int('0') and i <= int('9'): trueRet else: falseRet

proc c4mIsAlphaNum*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    i = unpack[int](args[0])
    r = Rune(i)

  if r.isAlpha() or (i >= int('0') and i <= int('9')):
    return trueRet
  else:
    return falseRet

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

proc c4mRandom*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(secureRand[uint64]()))

proc c4mNow*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(unixTimeInMS()))

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

# this is so sections() in a con4m spec validation can get the value of
# the spec we're checking, instead of being introspective.
proc getReplacementState*(): Option[ConfigState] =
  return replacementState

proc setReplacementState*(state: ConfigState) =
  replacementState = some(state)

proc clearReplacementState*() =
  replacementState = none(ConfigState)

template scopeWalk(lookingfor: untyped) {.dirty.} =
  let
    name  = unpack[string](args[0])
    state = replacementState.getOrElse(localState)
    aOrE  = attrLookup(state.attrs, name.split("."), 0, vlExists)

  if aOrE.isA(AttrErr):
    return some(pack[seq[string]](@[]))

  let aOrS = aOrE.get(AttrOrSub)

  if aOrS.isA(Attribute):
    return some(pack[seq[string]](@[]))
  var
    sec              = aOrS.get(AttrScope)
    res: seq[string] = @[]

  for key, aOrS in sec.contents:
    if aOrS.isA(lookingfor):
      res.add(key)

proc c4mSections*(args: seq[Box], localState: ConfigState): Option[Box] =
  scopeWalk(AttrScope)
  return some(pack(res))

proc c4mFields*(args:  seq[Box], localState: ConfigState): Option[Box] =
  scopeWalk(Attribute)
  return some(pack(res))

proc c4mTypeOf*(args: seq[Box], localstate: ConfigState): Option[Box] =
  let
    actNode  = localstate.nodeStash.children[1]
    itemType = actNode.children[0].getType()

  return some(pack(itemType))

proc c4mCmpTypes*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let
    t1 = unpack[Con4mType](args[0])
    t2 = unpack[Con4mType](args[1])

  return some(pack(not t1.copyType().unify(t2.copyType()).isBottom()))

proc c4mAttrGetType*(args: seq[Box], localstate: ConfigState): Option[Box] =
  ## This allows us to, from within a c42 spec, query the type of an
  ## attr in the actual con4m file we're checking.  Otherwise, we
  ## could simply use the previous function to get the type of an attribute.
  let
    varName = unpack[string](args[0])
    state   = replacementState.getOrElse(localState)
    aOrE    = attrLookup(state.attrs, varName.split("."), 0, vlExists)

  if aOrE.isA(AttrErr):       return some(pack(bottomType))
  let aOrS = aOrE.get(AttrOrSub)
  if not aOrS.isA(Attribute): return some(pack(bottomType))
  var sym  = aOrS.get(Attribute)

  return some(pack(sym.tInfo))

proc c4mRefTypeCmp*(args: seq[Box], localstate: ConfigState): Option[Box] =
  ## Arg 0 is the field we're type-checking.
  ## Arg 1 is the field that we expect to contain a typespec,
  ## where that typespec should indicate the type of arg 0.
  let
    varName = unpack[string](args[0])
    tsField = unpack[string](args[1])
    state   = replacementState.getOrElse(localState)
    aOrE1   = attrLookup(state.attrs, varName.split("."), 0, vlExists)
    aOrE2   = attrLookup(state.attrs, tsField.split("."), 0, vlExists)

  if aOrE1.isA(AttrErr):       return falseRet
  if aOrE2.isA(AttrErr):       return falseRet
  let
    aOrS1 = aOrE1.get(AttrOrSub)
    aOrS2 = aOrE2.get(AttrOrSub)

  if not aOrS1.isA(Attribute): return falseRet
  if not aOrS2.isA(Attribute): return falseRet

  var
    symToCheck = aOrS1.get(Attribute)
    tsFieldSym = aOrS2.get(Attribute)
    tsValOpt   = tsFieldSym.attrToVal()

  if tsFieldSym.tInfo.resolveTypeVars().kind != TypeTypeSpec:
    raise c4mException("Field '" & tsField & "' is not a typespec.")

  if tsValOpt.isNone():
    raise c4mException("Field '" & tsField & "' has no value provided.")

  var
    tsValType = unpack[Con4mType](tsValOpt.get())
    res       = not tsValType.unify(symToCheck.tInfo).isBottom()

  if not res and tsValType.resolveTypeVars().kind == symToCheck.getType().kind:
    return some(pack(true))

  return some(pack(res))

proc c4mAttrExists*(args: seq[Box], localstate: ConfigState): Option[Box] =
  let
    attrName     = unpack[string](args[0])
    state        = replacementState.getOrElse(localState)
    aOrE         = attrLookup(state.attrs, attrName.split("."), 0, vlExists)

  if aOrE.isA(AttrErr):
    return some(pack(false))
  return some(pack(true))

proc c4mOverride*(args: seq[Box], localState: ConfigState): Option[Box] =
  let
    attrName     = unpack[string](args[0])
    state        = replacementState.getOrElse(localState)
    actNode      = localstate.nodeStash.children[1]
    itemType     = actNode.children[1].getType()
    aOrE         = attrLookup(state.attrs, attrName.split("."), 0, vlExists)

  if aOrE.isA(AttrErr): return falseRet
  let aOrS = aOrE.get(AttrOrSub)

  if not aOrS.isA(Attribute): return falseRet
  let sym = aOrS.get(Attribute)

  if sym.tInfo.copyType().unify(itemType.copyType()).isBottom(): return falseRet
  if sym.locked or sym.override.isSome(): return falseRet

  sym.override = some(args[1])
  if state.nodeStash == nil:
    sym.lastUse = none(Con4mNode)
  else:
    sym.lastUse = some(state.nodeStash)

  return trueRet

proc c4mGetAttr*(args: seq[Box], localstate: ConfigState): Option[Box] =
  let
    attrName     = unpack[string](args[0])
    expectedType = (unpack[Con4mType](args[1])).copyType()
    state        = replacementState.getOrElse(localState)
    aOrE         = attrLookup(state.attrs, attrName.split("."), 0, vlExists)

  if aOrE.isA(AttrErr):
    raise c4mException("Field not found: " & attrName)
  let aOrS = aorE.get(AttrOrSub)

  if not aOrS.isA(Attribute):
    raise c4mException("Got a section (expected attribute) for: "  & attrName)
  let sym = aOrS.get(Attribute)

  if sym.tInfo.copyType().unify(expectedType).isBottom():
    raise c4mException("Typecheck failed for: " & attrName & " (attr type: " &
        $(sym.tInfo) & "; passed type: " & $(expectedType) & ")")

  return sym.value

proc c4mFnExists*(args: seq[Box], localstate: ConfigState): Option[Box] =
  let
    fn         = unpack[CallbackObj](args[0])
    state      = getReplacementState().getOrElse(localstate)
    candidates = state.findMatchingProcs(fn.name, fn.tInfo)

  return some(pack(candidates.len() > 0))

proc c4mSplitAttr*(args: seq[Box], unused: ConfigState): Option[Box] =
  let
    str = unpack[string](args[0])
    ix  = str.rfind('.')

  if ix == -1: return some(pack(@["", str]))
  return some(pack(@[ str[0 ..< ix], str[ix + 1 .. ^1]]))


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

proc c4mLSetItem*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    l    = unpack[seq[Box]](args[0])
    ix   = unpack[int](args[1])
    item = args[2]

  l[ix] = item

  return some(pack[seq[Box]](l))

proc c4mDSetItem*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    t    = unpack[OrderedTableRef[Box, Box]](args[0])
    ix   = args[1]
    item = args[2]

  t[ix] = item

  return some(pack[OrderedTableRef[Box,Box]](t))

proc c4mLDeleteItem*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    l           = unpack[seq[Box]](args[0])
    toDel       = unpack[Box](args[1])
    n: seq[Box] = @[]

  for item in l:
    if item != toDel:
      n.add(item)

  return some(pack(n))

proc c4mDDeleteItem*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    t     = unpack[OrderedTableRef[Box, Box]](args[0])
    toDel = args[1]
    ret   = OrderedTableRef[Box, Box]()

  for k, v in t:
    if k != toDel:
      ret[k] = v

  return some(pack(ret))

proc c4mLRemoveIx*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    l           = unpack[seq[Box]](args[0])
    ix          = unpack[int](args[1])
    n: seq[Box] = @[]

  for i, item in l:
    if ix != i:
      n.add(item)

  return some(pack(n))

proc c4mArrAdd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  var
    a1 = unpack[seq[Box]](args[0])
    a2 = unpack[seq[Box]](args[1])

  return some(pack(a1 & a2))

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

  proc c4mUname*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    var
      unameInfo: Utsname
      items:     seq[string] = @[]
    discard posix.uname(unameInfo)
    items.add($(cast[cstring](addr unameInfo.sysname[0])))
    items.add($(cast[cstring](addr unameInfo.nodename[0])))
    items.add($(cast[cstring](addr unameInfo.release[0])))
    items.add($(cast[cstring](addr unameInfo.version[0])))
    items.add($(cast[cstring](addr unameInfo.machine[0])))
    result = some(pack(items))

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

# For our purposes, if any of these is attached, then it's a tty.
proc c4mIsTty*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  if (isatty(cint(stdout.getFileHandle())) != 0 or
      isatty(cint(stderr.getFileHandle())) != 0 or
      isatty(cint(stdin.getFileHandle()))  != 0):
    return some(pack(true))
  else:
    return some(pack(false))

proc c4mTtyName*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  let fd0: cint = cint(stdin.getFileHandle())
  if isatty(fd0) != 0:
    return some(pack(ttyname(fd0)))
  let fd1: cint = cint(stdout.getFileHandle())
  if isatty(fd1) != 0:
    return some(pack(ttyname(fd1)))
  let fd2: cint = cint(stderr.getFileHandle())
  if isatty(fd2) != 0:
    return some(pack(ttyname(fd2)))

  return some(pack(""))

proc boolStub*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(false))
proc intStub*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(0))
proc floatStub*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(0.0))
proc stringStub*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack(""))
proc listStub*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
  return some(pack[seq[Box]](@[]))
proc dictStub*(args: seq[Box], unused  = ConfigState(nil)): Option[Box] =
  let r = newCon4mDict[Box, Box]()
  return some(pack(r))
proc callbackStub*(args: seq[Box], unused  = ConfigState(nil)): Option[Box] =
  return some(pack(CallbackObj(tInfo: Con4mType(kind: TypeFunc, noSpec: true))))
proc typespecStub*(args: seq[Box], unused  = ConfigState(nil)): Option[Box] =
  return some(pack(Con4mType(kind: TypeTypeSpec)))

proc newCoreFunc*(s: ConfigState, sig: string, fn: BuiltInFn, stub = false) =
  ## Allows you to associate a NIM function with the correct signature
  ## to a configuration for use as a builtin con4m function. `name` is
  ## the parameter used to specify the name exposed to con4m.  `tinfo`
  ## is the Conform type signature.

  let
    ix       = sig.find('(')
    name     = sig[0 ..< ix]
    coreName = if fn == nil: "callback" else: "builtin"
    tinfo    = sig[ix .. ^1].toCon4mType()
  var
    f        = fn

  if tinfo.kind != TypeFunc:
    raise c4mException(fmt"Signature provided for {coreName} " &
                          "is not a function signature.")

  if stub:
    case tInfo.retType.kind
    of TypeString, TypeIPAddr, TypeCIDR, TypeDate, TypeTime, TypeDateTime:
      f = stringStub
    of TypeBool:
      f = boolStub
    of TypeInt, TypeChar, TypeDuration, TypeSize, TypeTVar, TypeBottom:
      f = intStub
    of TypeFloat:
      f = floatStub
    of TypeTuple, TypeList:
      f = listStub
    of TypeDict:
      f = dictStub
    of TypeTypeSpec:
      f = typespecStub
    of TypeFunc:
      f = callbackStub

  let b = if f == nil:
            FuncTableEntry(kind:        FnUserDefined,
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
                           builtin: f,
                           name:    name)

  if f == nil:
    if tinfo.retType.isBottom():
      raise c4mException(fmt"{coreName}: callbacks must have a return type")

  if not s.funcTable.contains(name):
    s.funcTable[name] = @[b]
  else:
    for item in s.funcTable[name]:
      if not isBottom(copyType(tinfo), copyType(item.tinfo)):
        raise c4mException(fmt"Type for {coreName} conflicts with existing " &
                               "entry in the function table")
    s.funcTable[name].add(b)

proc newBuiltIn*(s: ConfigState, sig: string, fn: BuiltInFn) =
  try:
    newCoreFunc(s, sig, fn)
  except:
    let msg = getCurrentExceptionMsg()
    raise newException(ValueError,
                       fmt"When adding builtin '{sig}': {msg}")

const defaultBuiltins* = [
  # Type conversion operations
  ("bool(int) -> bool",               BuiltInFn(c4mIToB)),
  ("bool(float) -> bool",             BuiltInFn(c4mFToB)),
  ("bool(string) -> bool",            BuiltInFn(c4mSToB)),
  ("bool(list[`x]) -> bool",          BuiltInFn(c4mLToB)),
  ("bool(dict[`x,`y]) -> bool",       BuiltInFn(c4mDToB)),
  ("float(int) -> float",             BuiltInFn(c4mItoF)),
  ("int(float) -> int",               BuiltInFn(c4mFtoI)),
  ("$(`t) -> string",                 BuiltInFn(c4mToString)),
  ("Duration(string) -> Duration",    BuiltInFn(c4mSToDur)),
  ("IPAddr(string) -> IPAddr",        BuiltInFn(c4mStoIP)),
  ("CIDR(string) -> CIDR",            BuiltInFn(c4mSToCIDR)),
  ("Size(string) -> Size",            BuiltInFn(c4mSToSize)),
  ("Date(string) -> Date",            BuiltInFn(c4mSToDate)),
  ("Time(string) -> Time",            BuiltInFn(c4mSToTime)),
  ("DateTime(string) -> DateTime",    BuiltInFn(c4mSToDateTime)),
  ("char(int) -> char",               BuiltInFn(c4mSelfRet)),
  ("int(char) -> int",                BuiltInFn(c4mSelfRet)),
  ("to_usec(Duration) -> int",        BuiltInFn(c4mSelfRet)),
  ("to_msec(Duration) -> int",        BuiltInFn(c4mDurAsMSec)),
  ("to_sec(Duration) -> int",         BuiltInFn(c4mDurAsSec)),
  ("to_type(string) -> typespec",     BuiltInFn(c4mStrToType)),
  ("to_chars(string) -> list[char]",  BuiltInFn(c4mStrToChars)),
  ("to_bytes(string) -> list[char]",  BuiltInFn(c4mStrToBytes)),
  ("to_string(list[char]) -> string", BuiltInFn(c4mCharsToString)),


  #[ Not done yet:
  ("get_day(Date) -> int",          BuiltInFn(c4mGetDayFromDate)),
  ("get_month(Date) -> int",        BuiltInFn(c4mGetMonFromDate)),
  ("get_year(Date) -> int",         BuiltInFn(c4mGetYearFromDate)),
  ("get_day(DateTime) -> int",      BuiltInFn(c4mGetDayFromDate)),
  ("get_month(DateTime) -> int",    BuiltInFn(c4mGetMonFromDate)),
  ("get_year(DateTime) -> int",     BuiltInFn(c4mGetYearFromDate)),
  ("get_hour(Time) -> int",         BuiltInFn(c4mGetHourFromTime)),
  ("get_min(Time) -> int",          BuiltInFn(c4mGetMinFromTime)),
  ("get_sec(Time) -> int",          BuiltInFn(c4mGetSecFromTime)),
  ("fractsec(Time) -> int",         BuiltInFn(c4mGetFracFromTime)),
  ("tz_offset(Time) -> string",     BuiltInFn(c4mGetTZOffset)),
  ("get_hour(DateTime) -> int",     BuiltInFn(c4mGetHourFromTime)),
  ("get_min(DateTime) -> int",      BuiltInFn(c4mGetMinFromTime)),
  ("get_sec(DateTime) -> int",      BuiltInFn(c4mGetSecFromTime)),
  ("fractsec(DateTime) -> int",     BuiltInFn(c4mGetFracFromTime)),
  ("tz_offset(DateTime) -> string", BuiltInFn(c4mGetTZOffset)),
  ("ip_part(CIDR) -> IPAddr",       BuiltInFn(c4mCIDRToIP)),
  ("net_size(CIDR) -> int",         BuiltInFn(c4mCIDRToInt)),
  ("to_CIDR(IPAddr, int) -> CIDR",  BuiltInFn(c4mToCIDR)),
  ]#
  # String manipulation functions.
  ("contains(string, string) -> bool",        BuiltInFn(c4mContainsStrStr)),
  ("find(string, string) -> int",             BuiltInFn(c4mFindFromStart)),
  ("len(string) -> int",                      BuiltInFn(c4mStrLen)),
  ("slice(string, int) -> string",            BuiltInFn(c4mSliceToEnd)),
  ("slice(string, int, int) -> string",       BuiltInFn(c4mSlice)),
  ("slice(list[`x], int, int) -> list[`x]",   BuiltInFn(c4mListSlice)),
  ("split(string,string) -> list[string]",    BuiltInFn(c4mSplit)),
  ("strip(string) -> string",                 BuiltInFn(c4mStrip)),
  ("pad(string, int) -> string",              BuiltInFn(c4mPad)),
  ("format(string) -> string",                BuiltInFn(c4mFormat)),
  ("base64(string) -> string",                BuiltInFn(c4mBase64)),
  ("base64_web(string) -> string",            BuiltInFn(c4mBase64Web)),
  ("debase64(string) -> string",              BuiltInFn(c4mDecode64)),
  ("hex(string) -> string",                   BuiltInFn(c4mToHex)),
  ("hex(int) -> string",                      BuiltInFn(c4mIntToHex)),
  ("dehex(string) -> string",                 BuiltInFn(c4mFromHex)),
  ("sha256(string) -> string",                BuiltInFn(c4mSha256)),
  ("sha512(string) -> string",                BuiltInFn(c4mSha512)),
  ("upper(string) -> string",                 BuiltInFn(c4mUpper)),
  ("lower(string) -> string",                 BuiltInFn(c4mLower)),
  ("join(list[string], string) -> string",    BuiltInFn(c4mJoin)),
  ("replace(string, string, string)->string", BuiltInFn(c4mReplace)),
  ("utf8_len(char) -> int",                   BuiltInFn(c4mUTF8Len)),
  ("is_combining(char) -> bool",              BuiltInFn(c4mIsCombining)),
  ("is_lower(char) -> bool",                  BuiltInFn(c4mIsLower)),
  ("is_upper(char) -> bool",                  BuiltInFn(c4mIsUpper)),
  ("is_space(char) -> bool",                  BuiltInFn(c4mIsSpace)),
  ("is_alpha(char) -> bool",                  BuiltInFn(c4mIsAlpha)),
  ("is_num(char) -> bool",                    BuiltInFn(c4mIsNum)),
  ("is_alphanum(char) -> bool",               BuiltInFn(c4mIsAlphaNum)),

  # Container (list and dict) basics.
  ("len(list[`x]) -> int",                   BuiltInFn(c4mListLen)),
  ("len(dict[`x,`y]) -> int",                BuiltInFn(c4mDictLen)),
  ("keys(dict[`x,`y]) -> list[`x]",          BuiltInFn(c4mDictKeys)),
  ("values(dict[`x,`y]) -> list[`y]",        BuiltInFn(c4mDictValues)),
  ("items(dict[`x,`y]) -> list[(`x,`y)]",    BuiltInFn(c4mDictItems)),
  ("contains(list[`x],`x) -> bool",          BuiltInFn(c4mListContains)),
  ("contains(dict[`x ,`y],`x) -> bool",      BuiltInFn(c4mDictContains)),
  ("set(list[`x], int, `x) -> list[`x]",     BuiltInFn(c4mLSetItem)),
  ("set(dict[`k,`v],`k,`v) -> dict[`k,`v]",  BuiltInFn(c4mDSetItem)),
  ("delete(list[`x], `x) -> list[`x]",       BuiltInFn(c4mLDeleteItem)),
  ("delete(dict[`k,`v], `k) -> dict[`k,`v]", BuiltInFn(c4mDDeleteItem)),
  ("remove(list[`x], int) -> list[`x]",      BuiltInFn(c4mLRemoveIx)),
  ("array_add(list[`x],list[`x])->list[`x]", BuiltInFn(c4mArrAdd)),

  # File system routines
  ("list_dir() -> list[string]",             BuiltInFn(c4mListDir)),
  ("list_dir(string) -> list[string]",       BuiltInFn(c4mListDir)),
  ("read_file(string) -> string",            BuiltInFn(c4mReadFile)),
  ("write_file(string, string) -> bool",     BuiltInFn(c4mWriteFile)),
  ("copy_file(string, string) -> bool",      BuiltInFn(c4mCopyFile)),
  ("move_file(string, string) -> bool",      BuiltInFn(c4mMove)),
  ("rm_file(string) -> bool",                BuiltInFn(c4mRm)),
  ("join_path(string, string) -> string",    BuiltInFn(c4mJoinPath)),
  ("resolve_path(string) -> string",         BuiltInFn(c4mResolvePath)),
  ("path_split(string) -> tuple[string, string]", BuiltInFn(c4mSplitPath)),
  ("cwd()->string",                          BuiltInFn(c4mCwd)),
  ("chdir(string) -> bool",                  BuiltInFn(c4mChdir)),
  ("mkdir(string) -> bool",                  BuiltInFn(c4mMkdir)),
  ("is_dir(string) -> bool",                 BuiltInFn(c4mIsDir)),
  ("is_file(string) -> bool",                BuiltInFn(c4mIsFile)),
  ("is_link(string) -> bool",                BuiltInFn(c4mIsFile)),
  ("chmod(string, int) -> bool",             BuiltInFn(c4mChmod)),
  ("file_len(string) -> int",                BuiltInFn(c4mFileLen)),
  ("to_tmp_file(string, string) -> string",  BuiltInFn(c4mTmpWrite)),

  # System routines
  ("echo(*`a)",                       BuiltInFn(c4mEcho)),
  ("abort(string)",                   BuiltInFn(c4mAbort)),
  ("env() -> dict[string, string]",   BuiltInFn(c4mEnvAll)),
  ("env(string) -> string",           BuiltInFn(c4mEnv)),
  ("env_exists(string) -> bool",      BuiltInFn(c4mEnvExists)),
  ("set_env(string, string) -> bool", BuiltInFn(c4mSetEnv)),
  ("getpid() -> int",                 BuiltInFn(c4mGetPid)),
  ("quote(string)->string",           BuiltInFn(c4mQuote)),
  ("osname() -> string",              BuiltInFn(c4mGetOsName)),
  ("arch() -> string",                BuiltInFn(c4mGetArch)),
  ("program_args() -> list[string]",  BuiltInFn(c4mGetArgv)),
  ("program_path() -> string",        BuiltInFn(c4mGetExePath)),
  ("program_name() -> string",        BuiltInFn(c4mGetExeName)),
  ("high() -> int",                   BuiltInFn(c4mIntHigh)),
  ("low() -> int",                    BuiltInFn(c4mIntLow)),
  ("rand() -> int",                   BuiltInFn(c4mRandom)),
  ("now() -> int",                    BuiltInFn(c4mNow)),

  # Binary ops
  ("bitor(int, int) -> int",          BuiltInFn(c4mBitOr)),
  ("bitand(int, int) -> int",         BuiltInFn(c4mBitAnd)),
  ("xor(int, int) -> int",            BuiltInFn(c4mBitXor)),
  ("shl(int, int) -> int",            BuiltInFn(c4mBitShl)),
  ("shr(int, int) -> int",            BuiltInFn(c4mBitShr)),
  ("bitnot(int) -> int",              BuiltInFn(c4mBitNot)),

  # Con4m-specific stuff
  ("sections(string) -> list[string]",          BuiltInFn(c4mSections)),
  ("fields(string) -> list[string]",            BuiltInFn(c4mFields)),
  ("typeof(`a) -> typespec",                    BuiltInFn(c4mTypeOf)),
  ("typecmp(typespec, typespec) -> bool",       BuiltInFn(c4mCmpTypes)),
  ("attr_type(string) -> typespec",             BuiltInFn(c4mAttrGetType)),
  ("attr_typecmp(string, string) -> bool",      BuiltInFn(c4mRefTypeCmp)),
  ("attr_get(string, typespec[`t]) -> `t",      BuiltInFn(c4mGetAttr)),
  ("function_exists(func) -> bool",             BuiltInFn(c4mFnExists)),
  ("attr_split(string)->tuple[string, string]", BuiltInFn(c4mSplitAttr)),
  ("attr_exists(string) -> bool",               BuiltInFn(c4mAttrExists)),
  ("add_override(string, `t) -> bool",          BuiltInFn(c4mOverride)),
  when defined(posix):
    ("run(string) -> string",                BuiltInFn(c4mCmd)),
    ("system(string) -> tuple[string, int]", BuiltInFn(c4mSystem)),
    ("getuid() -> int",                      BuiltInFn(c4mGetUid)),
    ("geteuid() -> int",                     BuiltInFn(c4mGetEuid)),
    ("uname() -> list[string]",              BuiltInFn(c4mUname)),
    ("using_tty() -> bool",                  BuiltInFn(c4mIsTty)),
    ("tty_name() -> string",                 BuiltInFn(c4mTtyName))

]

proc addBuiltinSet(s, bi, exclusions: auto) {.inline.} =
  for item in bi:
    let (name, impl) = item
    s.newBuiltIn(name, impl)

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
