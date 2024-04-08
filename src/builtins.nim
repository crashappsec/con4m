# Wrappers for con4m builtins that are as light as possible; even if
# we can directly translate to the C API, for the sake of the
# interpreter being able to statically use libffi to call these items,
# we explicitly pull them all in.
import std/[osproc, streams, posix, net, uri, httpclient, openssl]
import nimutils/managedtmp

#proc SSL_CTX_load_verify_file(ctx: SslCtx, CAfile: cstring):
#                       cint {.cdecl, dynlib: DLLSSLName, importc.}

#addStaticFunction("splitwrap", string_split)

# proc calltable*(s1: FlexArray[FlexArray[Rope]]): Rope {.exportc, cdecl.} =
#   # For now we have to do a little dance.
#   var
#     actual: seq[seq[Rope]]

#   for l in s1.items():
#     var row: seq[Rope] = @[]

#     for item in l.items():
#       row.add(item.copy())
#     actual.add(row)

#   result = actual.quickTable()
#   GC_ref(result)

# addStaticFunction("calltable", calltable)


# proc callflattable*(s1: FlexArray[Rope]): Rope {.exportc, cdecl.} =

#   var l: seq[Rope]
#   for item in s1.items():
#     l.add(item.copy())

#   result = l.instantTable()

#   GC_ref(result)

# addStaticFunction("callflattable", callflattable)

# proc con4m_print(p: pointer) {.exportc, cdecl.} =
#   # Once we properly integrate the 'Mixed' type we can make this
#   # take a variable number of arguments.

#   var n: Mixed = cast[Mixed](p)

#   if unify(TRich, n.t) != TBottom:
#     print(cast[Rope](n.value))
#     return

#   var err = ""
#   let asRope = cast[Rope](call_cast(n.value, n.t, TRich, err))
#   # If you're crashing here, you need to implement a cast to TRich.

#   if err == "":
#     print(asRope)
#     return
#   else:
#     echo call_repr(n.value, n.t)

# addStaticFunction("con4m_print", con4m_print)


# proc listlen*(arr: FlexArray[pointer]): int {.exportc, cdecl.} =
#   return int(arr.arr.flexarray_len())

# addStaticFunction("listlen", listlen)


# proc ropeplus*(a, b: Rope): Rope {.exportc, cdecl.} =
#   result = a + b
#   GC_ref(result)

# addStaticFunction("ropeplus", ropeplus)


# proc listadd(l1, l2: FlexArray[pointer]): FlexArray[pointer]
#     {.exportc, cdecl.}=
#   return l1 + l2

# addStaticFunction("listadd", listadd)


# proc con4m_repr(p: pointer): cstring {.exportc, cdecl.} =
#   var n: Mixed = cast[Mixed](p)

#   return cstring(call_repr(n.value, n.t))

# addStaticFunction("con4m_repr", con4m_repr)

# {.pragma: stdlib, cdecl, importc, header: "<stdlib.h>".}

# proc unsetenv(name: cstring): cint {.stdlib.}
# proc setenv(name: cstring, val: cstring, overwrite: cint): cint {.stdlib.}
# addStaticFunction("getenv", cast[pointer](getenv))
# addStaticFunction("unsetenv", unsetenv)
# addStaticFunction("setenv", setenv)

# proc env_exists(s: cstring): bool {.exportc, cdecl.} =
#   return existsEnv($(s))

# addStaticFunction("env_exists", env_exists)

# proc env_all(): Con4mDict {.exportc, cdecl.} =
#   result = newDict(tDict(TString, TString))

#   for (k, v) in envPairs():
#     result.obj[newC4Str(k)] = newC4Str(v)

# addStaticFunction("env_all", env_all)

# # These 3 things are just used for tests. con4m_print() above is the
# # 'real' call. Prob should remove these and migrate tests to stuff we
# # actually use.
# proc callecho*(s1: cstring) {.exportc, cdecl.} =
#   echo($(s1))

# addStaticFunction("callecho",  callecho)

# proc echoanint*(s1: int64) {.exportc, cdecl.} =
#   echo($(s1))

# addStaticFunction("echoanint", echoanint)

# proc callprint*(s1: Rope) {.exportc, cdecl.} =
#   print(s1)

# addStaticFunction("callprint", callprint)

# proc con4m_osname(): C4Str {.exportc, cdecl.} =
#   return newC4Str(hostOs)

# addStaticFunction("con4m_osname", con4m_osname)

# proc con4m_arch(): C4Str {.exportc, cdecl.} =
#   return newC4Str(hostCPU)

# addStaticFunction("con4m_arch", con4m_arch)

# proc con4m_get_argv(): FlexArray[pointer] {.exportc, cdecl.} =
#   var params = commandLineParams()

#   return cast[FlexArray[pointer]](toCon4m[seq[string]](params, tList(TString)))

# addStaticFunction("con4m_get_argv", con4m_get_argv)

# proc con4m_get_exe_path(): C4Str {.exportc, cdecl.} =
#   return newC4Str(resolvePath(getAppFileName()))

# addStaticFunction("con4m_get_exe_path", con4m_get_exe_path)

# proc con4m_get_exe_name(): C4Str {.exportc, cdecl.} =
#   return newC4Str(resolvePath(getAppFileName().splitPath().tail))

# addStaticFunction("con4m_get_exe_name", con4m_get_exe_name)

# proc con4m_highest_int(): int64 {.exportc, cdecl.} =
#   return high(int64)

# addStaticFunction("con4m_highest_int", con4m_highest_int)

# proc con4m_lowest_int(): int64 {.exportc, cdecl.} =
#   return low(int64)

# addStaticFunction("con4m_lowest_int", con4m_lowest_int)

# proc con4m_rand(): int64 {.exportc, cdecl.} =
#   return secureRand[int64]()

# addStaticFunction("con4m_rand", con4m_rand)

# proc con4m_now(): uint64 {.exportc, cdecl.} =
#   return unixTimeInMs()

# addStaticFunction("con4m_now", con4m_now)

# var   containerName: Option[string]
# const
#   mountInfoFile    = "/proc/self/mountinfo"
#   mountInfoPreface = "/docker/containers/"

# proc getContainerName*(): Option[string] {.inline.} =
#   once:
#     var f = newFileStream(mountInfoFile)

#     if f == nil: return none(string)

#     let lines = f.readAll().split("\n")

#     for line in lines:
#       let prefixIx = line.find(mountInfoPreface)
#       if prefixIx == -1: continue

#       let
#         startIx = prefixIx + mountInfoPreface.len()
#         endIx   = line.find("/", startIx)

#       containerName = some(line[startIx ..< endIx])

#   return containerName

# proc con4m_container_name(): C4Str {.exportc, cdecl.} =
#   return newC4Str(getContainerName().getOrElse(""))

# addStaticFunction("con4m_container_name", con4m_container_name)

# proc con4m_in_container(): bool {.exportc, cdecl.} =
#   return con4m_container_name().len() != 0

# addStaticFunction("con4m_in_container", con4m_in_container)

# proc con4m_run(cmd: cstring): C4Str {.exportc, cdecl.} =
#   let (output, _) = execCmdEx($(cmd))

#   return newC4Str(output)

# addStaticFunction("con4m_run", con4m_run)

# proc con4m_system(cmd: cstring): Con4mTuple {.exportc, cdecl.} =
#   var res = execCmdEx($(cmd))

#   return cast[Con4mTuple](toCon4m(res, tTuple(@[TString, Tint])))

# addStaticFunction("con4m_system", con4m_system)

# proc con4m_using_tty(): bool {.exportc, cdecl.} =
#   return isatty(cint(0)) != 0 or
#          isatty(cint(1)) != 0 or
#          isatty(cint(2)) != 0

# addStaticFunction("con4m_using_tty", con4m_using_tty)

# proc con4m_stdin_tty(): bool {.exportc, cdecl.} =
#   return isatty(cint(0)) != 0

# addStaticFunction("con4m_stdin_tty", con4m_stdin_tty)

# proc con4m_stdout_tty(): bool {.exportc, cdecl.} =
#   return isatty(cint(1)) != 0

# addStaticFunction("con4m_stdout_tty", con4m_stdout_tty)

# proc con4m_stderr_tty(): bool {.exportc, cdecl.} =
#   return isatty(cint(2)) != 0

# addStaticFunction("con4m_stderr_tty", con4m_stderr_tty)

# proc con4m_tty_name(): C4Str {.exportc, cdecl.} =
#   if isatty(0) != 0:
#     return newC4Str(ttyname(0))
#   if isatty(1) != 0:
#     return newC4Str(ttyname(1))
#   if isatty(2) != 0:
#     return newC4Str(ttyname(2))

# addStaticFunction("con4m_tty_name", con4m_tty_name)

# proc con4m_find_exe(s: cstring, l: FlexArray[pointer]):
#                    C4Str {.exportc, cdecl.} =
#   var
#     cmdName    = $(s)
#     extraPaths = l.from_con4m_str_list()
#     nimResult  = cmdName.findAllExePaths(extraPaths, true)

#   if nimResult.len() == 0:
#     return newC4Str("")
#   else:
#     return newC4Str(nimResult[0])

# addStaticFunction("con4m_find_exe", con4m_find_exe)

# proc con4m_list_dir(s: cstring): FlexArray[pointer] {.exportc, cdecl.} =
#   var
#     nimResult: seq[string] = @[]
#     dir = resolvePath($s)

#   for item in walkdir(dir):
#     nimResult.add(item.path)

#   return cast[FlexArray[pointer]](toCon4m(nimResult, tlist(TString)))

# addStaticFunction("con4m_list_dir", con4m_list_dir)

# proc con4m_read_file(s: cstring): C4Str {.exportc, cdecl.} =
#   var
#     fname = resolvePath($(s))
#     f     = newFileStream(fname, fmRead)

#   if f == nil:
#     result = newC4Str("")

#   result = newC4Str(f.readAll())

#   f.close()

# addStaticFunction("con4m_read_file", con4m_read_file)

# proc con4m_write_file(f: cstring, contents: cstring): bool {.exportc, cdecl.} =
#   var
#     fname = resolvePath($(f))
#     f     = newFileStream(fname, fmWrite)

#   if f == nil:
#     return false

#   f.write($contents)
#   f.close()

#   return true

# addStaticFunction("con4m_write_file", con4m_write_file)

# proc con4m_copy_file(src: cstring, dst: cstring): bool {.exportc, cdecl.} =
#   try:
#     copyFile($src, $dst)
#     return true
#   except:
#     return false

# addStaticFunction("con4m_copy_file", con4m_copy_file)

# proc con4m_join_path(p1: cstring, p2: cstring): C4Str {.exportc, cdecl.} =
#   return newC4Str(joinPath($(p1), $(p2)))

# addStaticFunction("con4m_join_path", con4m_join_path)

# proc con4m_split_path*(s: cstring): Con4mTuple {.exportc, cdecl.} =
#   result = newTuple(tTuple(@[TString, TString]))

#   let (a, b) = split_path($s)
#   result[0]  = cast[pointer](newC4Str(a))
#   result[1]  = cast[pointer](newC4Str(b))

# addStaticFunction("con4m_split_path", con4m_split_path)

# proc con4m_resolve_path*(s: cstring): C4Str {.exportc, cdecl.} =
#   return newC4Str(resolvePath($(s)))

# addStaticFunction("con4m_resolve_path", con4m_resolve_path)

# proc con4m_cwd(): C4Str {.exportc, cdecl.} =
#   return newC4Str(getCurrentDir())

# addStaticFunction("con4m_cwd", con4m_cwd)

# proc con4m_chdir(s: cstring): bool {.exportc, cdecl.} =
#   try:
#     setCurrentDir($(s))
#     return true
#   except:
#     return false

# addStaticFunction("con4m_chdir", con4m_chdir)

# proc con4m_mkdir(s: cstring): bool {.exportc, cdecl.} =
#   try:
#     createDir($s)
#     return true
#   except:
#     return false

# addStaticFunction("con4m_mkdir", con4m_mkdir)

# proc con4m_is_dir(s: cstring): bool {.exportc, cdecl.} =
#   try:
#     return getFileInfo(resolvePath($s)).kind == pcDir
#   except:
#     return false

# addStaticFunction("con4m_is_dir", con4m_is_dir)

# proc con4m_is_file(s: cstring): bool {.exportc, cdecl.} =
#   try:
#     return getFileInfo(resolvePath($s)).kind == pcFile
#   except:
#     return false


# addStaticFunction("con4m_is_file", con4m_is_file)

# proc con4m_is_link(s: cstring): bool {.exportc, cdecl.} =
#   try:
#     return getFileInfo(resolvePath($s)).kind in [pcLinkToFile, pcLinkToDir]
#   except:
#     return false


# addStaticFunction("con4m_is_link", con4m_is_link)

# proc con4m_chmod(s:    cstring,
#                  mode: set[FilePermission]): bool {.exportc, cdecl.} =
#   # set[FilePermission] is a uint16
#   try:
#     setFilePermissions(resolvePath($s), mode)
#     return true
#   except:
#     return false

# addStaticFunction("con4m_chmod", con4m_chmod)

# proc con4m_file_len(s: cstring): int {.exportc, cdecl.} =
#   try:
#     return resolvePath($s).getFileSize()
#   except:
#     return -1

# addStaticFunction("con4m_file_len", con4m_file_len)

# proc con4m_tmp_write(contents: cstring, endstr: cstring):
#                     C4Str {.exportc, cdecl.} =
#   let
#     prefix = getUlid(dash=false)
#     suffix = $(endstr)

#   try:
#     let (f, path) = getNewTempFile(prefix, suffix)
#     f.write($contents)
#     f.close()

#     return newC4Str(path)

#   except:
#     return newC4Str("")

# addStaticFunction("con4m_tmp_write", con4m_tmp_write)

# proc con4m_mime_to_dict(s: cstring): Con4mDict {.exportc, cdecl.} =
#   result = newDict(tDict(TString, TString))

#   let mimeLines = split($(s), "\n")

#   for line in mimeLines:
#     var tmp = ""
#     let ix = line.find(':')
#     if ix == -1:
#       continue

#     if ix + 1 != len(line):
#       tmp = unicode.strip(line[ix + 1 .. ^1])

#     result.obj[newC4Str(unicode.strip(line[0 ..< ix]))] = newC4Str(tmp)

# addStaticFunction("con4m_mime_to_dict", con4m_mime_to_dict)

# proc con4m_move(fsrc: cstring, fdst: cstring): bool {.exportc, cdecl.} =
#   try:
#     let
#       src  = $fsrc
#       dst  = $fdst
#       kind = getFileInfo(src, false).kind

#     if kind == pcDir or kind == pcLinkToDir:
#       moveDir(src, dst)
#     else:
#       moveFile(src, dst)
#     return true
#   except:
#     return false

# addStaticFunction("con4m_move", con4m_move)

# proc con4m_rm*(p: cstring): bool =
#   try:
#     let
#       path = resolvePath($p)
#       kind = getFileInfo(path, false).kind

#     if kind == pcDir or kind == pcLinkToDir:
#         removeDir(path, true)
#         return true
#     else:
#       removeFile(path)
#       return true
#   except:
#     return false

# addStaticFunction("con4m_rm", con4m_rm)

# proc con4m_shell_quote(s: cstring): C4Str {.exportc, cdecl.} =
#   return newC4Str(quoteShell($s))

# addStaticFunction("con4m_shell_quote", con4m_shell_quote)


# proc con4m_url_fetch(url: cstring, m: uint8, body: cstring,
#                      headers: Con4mDict, pinned_cert: cstring,
#                      timeout: int64): Con4mTuple {.cdecl, exportc.} =
#   var
#     uri:      Uri = parseURI($url)
#     tups:     seq[(string, string)]
#     client:   HttpClient
#     hdrObj:   HttpHeaders
#     context:  SslContext
#     response: Response
#     meth = HttpGet

#   if m == 1:
#     meth = HttpPost
#   elif m == 2:
#     meth = HttpPut

#   result = newTuple(tTuple(@[TBool, TString]))

#   if headers != nil:
#     for (k, v) in headers.obj.items():
#       tups.add(($(cast[cstring](k)),
#                 $(cast[cstring](v))))

#   hdrObj = newHttpHeaders(tups)

#   if uri.scheme == "https":
#     context = newContext(verifyMode = CVerifyPeer)
#     if pinnedCert.len() != 0:
#       discard context.context.SSL_CTX_load_verify_file(pinned_cert)
#     client = newHttpClient(sslContext = context, timeout = int(timeout))
#   else:
#     client = newHttpClient(timeout = int(timeout))

#   if client == nil:
#     result[0] = nil
#     result[1] = cast[pointer](newC4Str("ERR 000 Invalid HTTP configuration"))


#   if meth == HttpGet:
#     response = client.safeRequest(url = uri, httpMethod = HttpGet,
#                                   headers = hdrObj)
#   else:
#     response = client.safeRequest(url = uri, httpMethod = meth,
#                                   body = $body, headers = hdrObj)
#   if response.status[0] != '2':
#     result[0] = nil
#     result[1] = cast[pointer](newC4Str("ERR " & response.status))

#   elif response.bodyStream == nil:
#     result[0] = nil
#     result[1] = cast[pointer](
#       newC4Str("ERR 000 Response body was empty (internal error?)"))

#   else:
#     try:
#       let s = response.bodyStream.readAll()
#       if s.startswith("ERR"):
#         result[0] = nil
#       else:
#         result[0] = cast[pointer](1)

#       result[1] = cast[pointer](newC4Str(s))
#     except:
#       result[0] = nil
#       result[1] = cast[pointer](
#         newC4Str("ERR 000 Read of response output stream failed."))

#   client.close()

# addStaticFunction("con4m_url_fetch", con4m_url_fetch)

# proc con4m_get_ipv4_addr(): C4Str {.exportc, cdecl.} =
#   return newC4Str(getMyIpV4Addr())

# addStaticFunction("con4m_get_ipv4_addr", con4m_get_ipv4_addr)

# proc con4m_get_uid(): int64 {.exportc, cdecl.} =
#   return int64(getuid())

# addStaticFunction("con4m_get_uid", con4m_get_uid)

# proc con4m_get_euid(): int64 {.exportc, cdecl.} =
#   return int64(geteuid())

# addStaticFunction("con4m_get_euid", con4m_get_euid)

# proc con4m_uname(): Flexarray[pointer] {.exportc, cdecl.} =
#   var
#     unameInfo: Utsname
#     items:     seq[string] = @[]

#   discard posix.uname(unameInfo)
#   items.add($(cast[cstring](addr unameInfo.sysname[0])))
#   items.add($(cast[cstring](addr unameInfo.nodename[0])))
#   items.add($(cast[cstring](addr unameInfo.release[0])))
#   items.add($(cast[cstring](addr unameInfo.version[0])))
#   items.add($(cast[cstring](addr unameInfo.machine[0])))

#   return cast[FlexArray[pointer]](toCon4m(items, tList(TString)))

# addStaticFunction("con4m_uname", con4m_uname)
