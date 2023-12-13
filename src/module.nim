import nimutils, os, strutils, httpclient, net, uri, streams
import irgen

type
  Module = object
    url*:         string
    where*:       string
    name*:        string
    ext*:         string
    tree*:        Con4mNode
    ir*:          IrNode
    errs*:        seq[Con4mError]
    tokens*:      seq[Con4mToken]
    globalScope*: Scope
    moduleScope*: Scope
    funcScope*:   Scope
    usedAttrs*:   Scope

var
  modules:    Dict[string, Module]
  modulePath: seq[string] = @[".", "https://chalkdust.io/"]


proc commonLoad(url: string, contents: string): Module =
  var ctx: CompileCtx

  result = Module(url: url)

  (result.where, result.name, result.ext) = url.splitFile()

  ctx = newCompileCtx(contents, result.name & result.ext)

  if ctx.parseModule():
    ctx.toIr()

  result.ir          = ctx.irRoot
  result.tree        = ctx.root
  result.tokens      = ctx.tokens
  result.errs        = ctx.errors
  result.globalScope = ctx.globalScope
  result.moduleScope = ctx.moduleScope
  result.funcScope   = ctx.funcScope
  result.usedAttrs   = ctx.usedAttrs

proc loadModuleFromFile(url: string): Option[Module] =
  result = modules.lookup(url)

  if result.isSome():
    return

  result = some(url.commonLoad(readFile(url)))

proc loadModuleFromUrl(url: string): Option[Module] =
  result = modules.lookup(url)

  if result.isSome():
    return

  var
    uri      = parseUri(url)
    context  = newContext(verifyMode = CVerifyPeer)
    client   = newHttpClient(sslContext = context, timeout = 1000)
    response = client.safeRequest(url = uri, httpMethod = HttpGet)

  if response.status[0] != '2':
    return none(Module)

  result = some(url.commonLoad(response.bodyStream.readAll()))

proc loadModule*(name: string): Option[Module] =
  ## Load module always

  if name.startswith("/"):
    return name.loadModuleFromFile()
  if name.startswith("https://"):
    return name.loadModuleFromUrl()
  if name.startswith("http://"):
    raise newException(ValueError, "Only https URLs and local files accepted")

  for path in modulePath:
    var fullUrl: string

    if path.startswith("https://"):
      if path.endswith("/"):
        fullUrl = path & name
      else:
        fullUrl = path & "/" & name
    else:
      fullUrl = path.joinPath(name).resolvePath()

    result = fullUrl.loadModule()
    if result.isSome():
      return

proc printTokens*(ctx: Module, start = 0, endix = 0) =
  var
    first = start
    last  = endix

  if ctx.tokens.len() == 0:
    return

  if start < 0:
    first += ctx.tokens.len()

  if endix <= 0:
    last += ctx.tokens.len()

  print ctx.tokens[start ..< last].toRope()

proc printParseTree*(ctx: Module) =
  if ctx.tree != nil:
    print(ctx.tree.toRope())
  else:
    print h4("No parse tree produced.")

proc printIr*(ctx: Module) =
  if ctx.ir != nil:
    print(ctx.ir.toRope())
  else:
    print h4("No IR produced.")

proc printGlobalScope*(ctx: Module) =
  print ctx.globalScope.toRope("Globals used")
proc printModuleScope*(ctx: Module) =
  print ctx.moduleScope.toRope("Module scope")
proc printFuncScope*(fn: FuncInfo) =
  print fn.fnScope.toRope("Scope for function " & fn.name)
proc printAllFuncScopes*(ctx: Module) =
  if ctx.moduleScope == nil:
    print h4("No functions found due to parse failure.")
    return
  for sym in ctx.moduleScope.table.values():
    if sym.isFunc:
      for item in sym.fimpls:
        item.printFuncScope()
proc printAttrsUsed*(ctx: Module) =
  if ctx.usedAttrs != nil:
    print ctx.usedAttrs.toRope("Used attributes")

proc printErrors*(ctx: var Module, verbose = true, ll = LlNone) =
  var errsToPrint: seq[Con4mError]

  for item in ctx.errs:
    if cast[int](item.severity) >= cast[int](ll):
      errsToPrint.add(item)

  print errsToPrint.formatErrors(verbose)

when isMainModule:
  useCrashTheme()
  let m = loadModule("ptest.c4m")

  if m.isSome():
    var module = m.get()
    module.printTokens()
    module.printParseTree()
    module.printIr()
    module.printErrors()
    module.printGlobalScope()
    module.printModuleScope()
    module.printAttrsUsed()
    module.printAllFuncScopes()
  else:
    print em("Could not find module 'ptest.c4m'")
