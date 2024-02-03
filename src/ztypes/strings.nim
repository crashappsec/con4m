import base, unicode, ordinals, strutils

proc tList(item: TypeId): TypeId {.importc, cdecl.}

let richLitMods = @["r", "md", "html", "h1", "h2", "h3",
                    "h4", "h5", "h6", "p", "em", "i", "b",
                    "strong", "underline", "pre", "code",
                    "inv"]
var
  strOps   = newVTable()
  bufOps   = newVTable()
  utf32Ops = newVTable()
  richOps  = newVTable()

proc str_new_lit(s: string, st: SyntaxType, lmod: string, l: var int,
                 err: var string): pointer {.cdecl.} =
  l = s.len()
  result = cast[pointer](newC4Str(l))
  copyMem(result, addr s[0], l)

proc rich_new_lit(s: string, st: SyntaxType, lmod: string,
                  l: var int, err: var string): pointer {.cdecl.} =
  var toStore: string

  toStore = lmod & ":" & s
  l      = toStore.len() + 1

  result = cast[pointer](newC4Str(l))
  copyMem(result, addr toStore[0], l)

proc u32_repr(pre: pointer): string {.cdecl.} =
  let s = cast[C4Str](pre)

  if s == nil:
    return ""
  else:
    return s.toNimStr()

# deleteme.
proc str_repr(n: C4Str): cstring {.cdecl.} =
  return cast[cstring](n)

proc rich_repr(s: Rope): string {.cdecl.} =
  return s.toUtf8()

proc rich_pluseq(a: pointer, b: Rope): void {.cdecl.} =
  # I *think* I can declare `a` a `var Rope` here, but just in case.
  var a = cast[Rope](a)

  a += b

proc rich_add(a, b: Rope): Rope {.cdecl.} =
  return a + b

proc str_index(a: C4Str, b: int, err: var bool): int64 {.cdecl.} =
  var x = cast[cstring](a)

  if b < 0 or b >= x.len():
    err = true
    return 0

  return int64(x[b])

proc u32_index(a: pointer, b: int, err: var bool): int64 {.cdecl.} =
  # Pointer is to the char* part of a C4String.
  let bytelen = b * 4

  if bytelen < 0 or bytelen >= c4str_len(cast[C4Str](a)):
    err = true
    return 0

  let p = cast[ptr Rune](cast[uint64](a) + uint64(bytelen))

  return int64(p[])

proc rich_len(p: pointer): int {.cdecl.} =
  let r = cast[string](p)
  return r.runeLength()

proc u32_len(s: C4Str): int {.cdecl.} =
  return s.len() div 4

proc rich_copy(r: Rope): Rope {.cdecl, exportc.} =
  result = r.copy()
  GC_ref(result)

proc cast_str_to_u32(pre: C4Str, tfrom, tto: TypeId, err: var string):
                    C4Str {.cdecl, exportc.} =
  let
    l = pre.len()
    s = `$`(cast[cstring](pre)).toRunes()

  result = newC4Str(l * 4)

  if l != 0:
    copyMem(cast[pointer](result), addr s[0], l)

proc cast_str_to_rich(pre: pointer, tfrom, tto: TypeId,
                      err: var string): pointer {.cdecl, exportc.} =

  let s = $(cast[cstring](pre))
  var rope = text(s)

  GC_ref(rope)

  return cast[pointer](rope)

proc cast_u32_to_rich(pre: C4Str, tfrom, tto: TypeId, err: var string):
                     Rope {.cdecl, exportc.} =
  var
    l = (pre.len() div 4) * 4
    s = newSeq[Rune](int(l))

  if l != 0:
    copyMem(addr s[0], cast[pointer](pre), l)

  result = text($(s))
  GC_ref(result)

proc cast_u32_to_str(pre: C4Str, tfrom, tto: TypeId, err: var string):
                    C4Str {.cdecl, exportc.} =
  var
    l = (pre.len() div 4) * 4
    s = newSeq[Rune](int(l))

  if l != 0:
    copyMem(addr s[0], cast[pointer](pre), l)

  result = newC4Str($s)

proc cast_rich_to_u32(r: Rope, tfrom, to: TypeId, err: var string):
                     C4Str {.cdecl, exportc.} =
  let
    s = r.toUtf8(r.runeLength())
    u = s.toRunes()
    l = u.len() * 4

  result = newC4Str(l)
  if l != 0:
    copyMem(cast[pointer](result), addr u[0], l)

proc cast_rich_to_str(r: Rope, tfrom, tto: TypeId, err: var string):
                     C4Str {.cdecl, exportc.} =
  return newC4Str(r.toUtf8(r.runeLength()))

proc str_slice(a: pointer, b, c: int, err: bool): pointer =
  let
    x = cast[cstring](a)
    l = x.len()

  var
    b = b
    c = c

  if b < 0:
    b += l
  if c < 0:
    b += l

  if b >= c or b < 0 or c >= l:
    return nil

  return newC4Str(`$`(x)[b .. c])

proc u32_slice(a: pointer, b, c: int, err: var bool): pointer =
  return str_slice(a, b * 4, c * 4, err)

proc str_load_lit(cstr: cstring, l: cint): pointer =
  var s = newC4Str(int64(l))

  if l != 0:
    copyMem(cast[pointer](s), cstr, l)

  result = cast[pointer](s)

proc rich_load_lit(cstr: cstring, l: cint): pointer {.cdecl, exportc.} =
  let
    full = $cstr
    f    = full.find(':')
    lmod = full[0 ..< f]
    s    = full[f + 1 .. ^1]

  var r: Rope

  case lmod
  of "md":
    r = markdown(s)
  of "html":
    r = html(s)
  of "h1":
    r = h1(s)
  of "h2":
    r = h2(s)
  of "h3":
    r = h3(s)
  of "h4":
    r = h4(s)
  of "h5":
    r = h5(s)
  of "h6":
    r = h6(s)
  of "p":
    r = paragraph(s)
  of "em":
    r = em(s)
  of "i":
    r = italic(s)
  of "b":
    r = bold(s)
  of "strong":
    r = strong(s)
  of "underline":
    r = underline(s)
  of "pre":
    r = pre(s)
  of "code":
    r = code(s)
  of "inv":
    r = inverse(s)
  else:
    r = atom(s)

  GC_ref(r)

  result = cast[pointer](r)

# These need to be fw referenced as we need TString, etc. defined first.
proc get_cast_from_string(dt: DataType, t1, t2: TypeId,
                          err: var string): pointer
proc get_cast_from_u32(dt: Datatype, t1, t2: TypeId,
                       err: var string): pointer
proc get_cast_from_rich(dt: Datatype, t1, t2: TypeId,
                        err: var string): pointer

strOps[FRepr]         = cast[pointer](str_repr)
strOps[FCastFn]       = cast[pointer](get_cast_from_string)
strOps[FEq]           = cast[pointer](c4str_eq)
strOps[FLt]           = cast[pointer](c4str_lt)
strOps[FGt]           = cast[pointer](c4str_gt)
strOps[FAdd]          = cast[pointer](c4str_add)
strOps[FIndex]        = cast[pointer](str_index)
strOps[FSlice]        = cast[pointer](str_slice)
strOps[FNewLit]       = cast[pointer](str_new_lit)
strOps[FLoadLit]      = cast[pointer](str_load_lit)
strOps[FCopy]         = cast[pointer](c4str_copy)
strOps[FLen]          = cast[pointer](c4str_len)
bufOps[FRepr]         = cast[pointer](str_repr)
bufOps[FCastFn]       = cast[pointer](get_cast_from_string)
bufOps[FEq]           = cast[pointer](c4str_eq)
bufOps[FLt]           = cast[pointer](c4str_lt)
bufOps[FGt]           = cast[pointer](c4str_gt)
bufOps[FAdd]          = cast[pointer](c4str_add)
bufOps[FIndex]        = cast[pointer](str_index)
bufOps[FSlice]        = cast[pointer](str_slice)
bufOps[FNewLit]       = cast[pointer](str_new_lit)
bufOps[FCopy]         = cast[pointer](c4str_copy)
bufOps[FLen]          = cast[pointer](c4str_len)
utf32Ops[FRepr]       = cast[pointer](u32_repr)
utf32Ops[FStaticRepr] = cast[pointer](u32_repr)
utf32Ops[FCastFn]     = cast[pointer](get_cast_from_u32)
utf32Ops[FEq]         = cast[pointer](c4str_eq)
utf32Ops[FLt]         = cast[pointer](c4str_lt)
utf32Ops[FGt]         = cast[pointer](c4str_gt)
utf32Ops[FAdd]        = cast[pointer](c4str_add)
utf32Ops[FIndex]      = cast[pointer](u32_index)
utf32Ops[FSlice]      = cast[pointer](u32_slice)
utf32Ops[FNewLit]     = cast[pointer](str_new_lit)
utf32Ops[FCopy]       = cast[pointer](c4str_copy)
utf32Ops[FLen]        = cast[pointer](u32_len)
richOps[FRepr]        = cast[pointer](rich_repr)
richOps[FStaticRepr]  = cast[pointer](str_repr)
richOps[FCastFn]      = cast[pointer](get_cast_from_rich)
richOps[FEq]          = cast[pointer](value_eq)
richOps[FAdd]         = cast[pointer](rich_add)
richOps[FNewLit]      = cast[pointer](rich_new_lit)
richOps[FLoadLit]     = cast[pointer](rich_load_lit)
richOps[FCopy]        = cast[pointer](rich_copy)
richOps[FLen]         = cast[pointer](rich_len)
richOps[FPlusEqRef]   = cast[pointer](rich_pluseq)

let
  TString* = addDataType(name = "string", concrete = true,
                                strTy = true, ops = strOps)
  TBuffer* = addDataType(name = "buffer", concrete = true,
                                strTy = true, ops = bufOps)
  TUtf32*  = addDataType(name = "utf32",  concrete = true,
                                strTy = true, ops = utf32Ops)
  TRich*   = addDataType(name = "rich",   concrete = true, ops = richOps)

registerSyntax(TString, STStrQuotes, @["u", "u8"], primary = true)
registerSyntax(TBuffer, STStrQuotes, @["bin"])
registerSyntax(TUtf32,  STStrQuotes, @["u32"])
registerSyntax(TRich,   STStrQuotes, richLitMods)

proc get_cast_from_string(dt: DataType, t1, t2: TypeId,
                          err: var string): pointer =
  if dt.dtid == TUtf32:
    return cast[pointer](cast_str_to_u32)
  elif dt.dtid == TRich:
    return cast[pointer](cast_str_to_rich)
  elif dt.dtid in [TString, TBuffer]:
    return cast[pointer](cast_identity)
  elif dt.dtid == TBool:
    return cast[pointer](cast_to_bool)

proc get_cast_from_u32(dt: Datatype, t1, t2: TypeId,
                       err: var string): pointer =
  if dt.dtid == TUtf32:
    return cast[pointer](cast_identity)
  elif dt.dtid == TRich:
    return cast[pointer](cast_u32_to_rich)
  elif dt.dtid in [TBuffer, TString]:
    return cast[pointer](cast_u32_to_str)
  elif dt.dtid == TBool:
    return cast[pointer](cast_to_bool)

proc get_cast_from_rich(dt: Datatype, t1, t2: TypeId,
                        err: var string): pointer =
  if dt.dtid == TUtf32:
    result = cast[pointer](cast_rich_to_u32)
    err    = "LoseFormat"
  elif dt.dtid == TRich:
    result = cast[pointer](cast_identity)
  elif dt.dtid == TBuffer:
    result = cast[pointer](cast_rich_to_str)
    err    = "LoseFormat"
  elif dt.dtid == TString:
    result = cast[pointer](cast_rich_to_str)
    err    = "LoseFormat"
  elif dt.dtid == TBool:
    result = cast[pointer](cast_to_bool)
    err    = "LoseFormat"

proc str_strip*(s1: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(unicode.strip(s1.toNimStr()))

addStaticFunction("str_strip", str_strip)

proc str_contains*(s1: C4Str, s2: C4Str): bool {.exportc, cdecl.} =
  s1.toNimStr().contains(s2.toNimStr())

addStaticFunction("str_contains", str_contains)

proc str_starts_with(s1: C4Str, s2: C4Str): bool {.exportc, cdecl.} =
  s1.toNimStr().startswith(s2.toNimStr())

addStaticFunction("str_starts_with", str_starts_with)

proc str_ends_with(s1: C4Str, s2: C4Str): bool {.exportc, cdecl.} =
  s1.toNimStr().endswith(s2.toNimStr())

addStaticFunction("str_ends_with", str_ends_with)

proc str_find(s1: C4Str, s2: C4Str): int {.exportc, cdecl.} =
  s1.toNimStr().find(s2.toNimStr())

addStaticFunction("str_find", str_find)
addStaticFunction("str_len", c4str_len)

import std/base64
proc str_base64(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(base64.encode(s.toNimStr()))

addStaticFunction("str_base64", str_base64)

proc str_base64_web(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(base64.encode(s.toNimStr(), safe = true))

addStaticFunction("str_base64_web", str_base64_web)

proc str_decode(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(base64.decode(s.toNimStr()))

addStaticFunction("str_decode", str_decode)

proc str_to_hex(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(s.toNimStr().toHex().toLowerAscii())

addStaticFunction("str_to_hex", str_to_hex)

proc str_to_hex_int(s: int): C4Str {.exportc, cdecl.} =
  newC4Str(s.toHex().toLowerAscii())

addStaticFunction("str_to_hex_int", str_to_hex_int)

proc str_from_hex(s: C4Str): C4Str {.exportc, cdecl.} =
  try:
    return newC4Str(s.toNimStr().parseHexStr())
  except:
    return newC4Str("")

addStaticFunction("str_from_hex", str_from_hex)

proc str_sha256(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(s.toNimStr().sha256Hex())

addStaticFunction("str_sha256", str_sha256)

proc str_sha512(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(s.toNimStr().sha512Hex())

addStaticFunction("str_sha512", str_sha512)

proc str_upper(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(unicode.toUpper(s.toNimStr()))

addStaticFunction("str_upper", str_upper)

proc str_lower(s: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(unicode.toLower(s.toNimStr()))

addStaticFunction("str_lower", str_lower)

proc str_split(s1: C4Str, s2: C4Str): FlexArray[pointer] {.exportc, cdecl.} =
  let
    pieces = s1.toNimStr().split(s2.toNimStr())

  result = newArray[pointer](pieces.len())

  for i, item in pieces:
    result[i] = newC4Str(item)

  result.metadata = cast[pointer](tList(TString))

addStaticFunction("str_split", str_split)

proc str_join(s: FlexArray[pointer], joiner: C4Str): C4Str {.exportc, cdecl.} =
  var
    l: seq[string]

  for item in s.items():
    l.add(cast[C4Str](item).toNimStr())

  return newC4Str(l.join(joiner.toNimStr()))

addStaticFunction("str_join", str_join)

proc str_replace(base, match, replacement: C4Str): C4Str {.exportc, cdecl.} =
  newC4Str(replace(base.toNimStr(), match.toNimStr(), replacement.toNimStr()))


addStaticFunction("str_replace", str_replace)

proc str_pad(s: C4Str, width: int64): C4Str {.exportc, cdecl.} =
  let l = s.len()

  if l >= width:
    return s

  result = newC4Str(width)
  if l != 0:
    copyMem(cast[pointer](result), cast[pointer](s), l)

  var asC = cast[ptr char](cast[int](result) + l)
  for i in l ..< width:
    asC[] = ' '
    asC   = cast[ptr char](cast[int](asC) + 1)

addStaticFunction("str_pad", str_pad)

when false:
  proc c4mMimeToDict*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    var
      outDict     = Con4mDict[string, string]()
      tmp: string
    let mimeLines = unpack[string](args[0]).split("\n")

    for line in mimeLines:
      let ix = line.find(':')
      if ix == -1:
        continue

      if ix + 1 == len(line):
        tmp = ""
      else:
        tmp = unicode.strip(line[ix + 1 .. ^1])

      outDict[unicode.strip(line[0 ..< ix])] = tmp

    return some(pack(outDict))

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

  proc c4mRm*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    try:
      let
        path = resolvePath(unpack[string](args[0]))
        kind = getFileInfo(path, false).kind

      if kind == pcDir or kind == pcLinkToDir:
          removeDir(path, true)
          logExternalAction("rm_dir", path)
          return trueRet
      else:
        removeFile(path)
        logExternalAction("rm_file", path)
        return trueRet
    except:
      return falseRet

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

  proc c4mUrlBase*(url: string, post: bool, body: string,
                   headers: OrderedTableRef[string, string],
                  pinnedCert: string, timeout: int): string =
    ## For now, the funcs that call us provide no interface to the timeout;
    ## they hardcode to 5 seconds.

    var
      uri:      Uri = parseURI(url)
      tups:     seq[(string, string)]
      client:   HttpClient
      hdrObj:   HttpHeaders
      context:  SslContext
      response: Response

    if headers != nil:
      for k, v in headers:
        tups.add((k, v))

    hdrObj = newHttpHeaders(tups)

    if uri.scheme == "https":
      context = newContext(verifyMode = CVerifyPeer)
      if pinnedCert != "":
        discard context.context.SSL_CTX_load_verify_file(pinnedCert)
      client = newHttpClient(sslContext = context, timeout = timeout)
    else:
      client = newHttpClient(timeout = timeout)

    if client == nil:
      return "ERR 000 Invalid HTTP configuration"

    if post:
      response = client.safeRequest(url = uri, httpMethod = HttpPost,
                                  body = body, headers = hdrObj)
    else:
      response = client.safeRequest(url = uri, httpMethod = HttpGet,
                                  headers = hdrObj)
    if response.status[0] != '2':
      result = "ERR " & response.status

    elif response.bodyStream == nil:
      result = "ERR 000 Response body was empty (internal error?)"

    else:
      logExternalAction(if post: "POST" else: "GET", url)
      try:
        result = response.bodyStream.readAll()
      except:
        result = "ERR 000 Read of response output stream failed."

    client.close()

  proc c4mUrlGet*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    let
      url    = unpack[string](args[0])
      res    = url.c4mUrlBase(post = false, body = "", headers = nil,
                              pinnedCert = "", timeout = 5000)

    result = some(pack(res))

  proc c4mUrlGetPinned*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    let
      url    = unpack[string](args[0])
      cert   = unpack[string](args[1])
      res    = url.c4mUrlBase(post = false, body = "", headers = nil,
                                pinnedCert = cert, timeout = 5000)

    result = some(pack(res))

  proc c4mUrlPost*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    let
      url     = unpack[string](args[0])
      body    = unpack[string](args[1])
      headers = unpack[OrderedTableRef[string, string]](args[2])
      res     = url.c4mUrlBase(true, body, headers, pinnedCert = "",
                               timeout = 5000)

    result = some(pack(res))

  proc c4mExternalIp*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    result = some(pack(getMyIpV4Addr()))

  proc c4mUrlPostPinned*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    let
      url     = unpack[string](args[0])
      body    = unpack[string](args[1])
      headers = unpack[OrderedTableRef[string, string]](args[2])
      cert    = unpack[string](args[3])
      res     = url.c4mUrlBase(true, body, headers, cert, timeout = 5)

    result = some(pack(res))

when false: #defined(posix):
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
      logExternalAction("run", cmd)
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
      logExternalAction("run", cmd)
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

when false: #else:
  ## I don't know the permissions models on any non-posix OS, so
  ## this might be wildly insecure on such systems, as far as I know.
  ## to that end, when posix is not defined, this command is removed
  ## from the defaults.
  proc c4mCmd*(args: seq[Box], unused = ConfigState(nil)): Option[Box] =
    ## An unsafe version of this for non-posix OSes. On such machines,
    ## it is NOT a default builtin.
    var cmd = unpack[string](args[0])

    let (output, _) = execCmdEx(cmd)

    return some(pack(output))

# For our purposes, if any of these is attached, then it's a tty.
when false:
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

  proc c4mContainerName(args: seq[Box], s: ConfigState): Option[Box] =
    return some(pack(containerName.getOrElse("")))

  proc c4mInContainer(args: seq[Box], s: ConfigState): Option[Box] =
    result = if containerName.isSome(): trueRet else: falseRet
