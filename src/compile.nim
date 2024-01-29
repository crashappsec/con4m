import nimutils, os, strutils, httpclient, net, uri, streams, stchecks, builtins
export nimutils, os, net, uri, streams, stchecks, builtins

proc newSpec(): ValidationSpec {.importc, cdecl.}
proc getRootSection(spec: ValidationSpec): SectionSpec {.importc, cdecl.}
proc findAndLoadFromUrl(ctx: CompileCtx, url: string): Option[Module] {.exportc,
                                                                       cdecl.}
proc loadModule(ctx: CompileCtx, module: Module)


proc loadSourceFromFile(ctx: CompileCtx, url: string): Option[string] =
  try:
    return some(readFile(url))
  except:
    return none(string)

proc loadSourceFromUrl(ctx: CompileCtx, url: string): Option[string] =
  var
    uri      = parseUri(url)
    context  = newContext(verifyMode = CVerifyPeer)
    client   = newHttpClient(sslContext = context, timeout = 1000)
    response = client.safeRequest(url = uri, httpMethod = HttpGet)

  if response.status[0] != '2':
    return none(string)
  else:
    return some(response.bodyStream.readAll())

proc loadSourceFromInsecureUrl(ctx: CompileCtx, url: string):
                              Option[string] =
  var
    uri      = parseUri(url)
    client   = newHttpClient(timeout = 1000)
    response = client.safeRequest(url = uri, httpMethod = HttpGet)

  if response.status[0] != '2':
    return none(string)
  else:
    ctx.loadWarn("InsecureUrl", url)
    return some(response.bodyStream.readAll())

proc loadModuleFromLocation(ctx: CompileCtx, location: string,
                            fname: string, ext = ""): Option[Module] =
  var location = location
  if not (location.startsWith("https://") or location.startsWith("http://")):
    location = location.resolvePath()

  var moduleKey = joinPath(location, fname)

  let moduleOpt = ctx.modules.lookup(moduleKey)
  if moduleOpt.isSome():
    # It might not be loaded at this point (recursive imports), but that's okay.
    return moduleOpt
  var ext = if ext == "": ctx.defaultExt else: ext
  let url = moduleKey & ext

  var source: Option[string]
  if url.startswith("/"):
    source = ctx.loadSourceFromFile(url)
  elif url.startswith("https://"):
    source = ctx.loadSourceFromUrl(url)
  elif url.startswith("http://"):
    source = ctx.loadSourceFromInsecureUrl(url)
  else:
    ctx.loadError("BadUrl", url)
    return none(Module)

  if source.isNone():
    return none(Module)

  var module = ctx.newModuleObj(source.get(), fname, location, ext, url,
                                moduleKey)

  ctx.loadModule(module)

  return some(module)

proc isRelativePath(loc: string): bool =
  if loc.len() == 0:
    return true
  if loc[0] == '/':
    return false
  if loc[0] != 'h':
    return true
  if loc.startswith("http://"):
    return false
  if loc.startswith("https://"):
    return false
  return true

proc findAndLoadModule*(ctx: CompileCtx, location, fname, ext: string):
                      Option[Module] {.exportc, cdecl.} =
  if not location.isRelativePath():
    return ctx.loadModuleFromLocation(location, fname, ext)

  for pathItem in ctx.modulePath:
    let
      possibleLoc = joinPath(pathItem, location)

    let
      possibly    = ctx.loadModuleFromLocation(possibleLoc, fname, ext)

    if possibly.isSome():
      let module = possibly.get()
      return possibly

  return none(Module)

proc findAndLoadFromUrl(ctx: CompileCtx, url: string): Option[Module] =
  let (loc, name, ext) = url.splitFile()
  return ctx.findAndLoadModule(loc, name, ext)

proc loadModule(ctx: CompileCtx, module: Module) =
  # The module's global scope is for its own view on what symbols
  # it will use as a global, whether it imports them or exports them.
  #
  # We will check global variables against each other when we do our
  # internal linking pass.

  var err = not parseTopLevel(module)

  if not err:
    module.findDeclarations()
    for (loc, modname) in module.usedModules:
      let depOpt = ctx.findAndLoadModule(loc, modname, "")
      if depOpt.isNone():
        err = true
      else:
        let dep = depOpt.get()
        if dep notin module.imports:
          module.imports.add(dep)

  if err:
    ctx.fatal = true

  module.fatalErrors = err

proc buildIr(ctx: CompileCtx, module: Module) =
  if module == nil or module.ir != nil:
    # Already been done.
    return

  # Even if one phase fails, go through as much as possible to buffer up as
  # many errors as possible.

  # Currently the compile context is only needed during IR gen / folding
  # to look up other modules that we use.
  if ctx.attrSpec == nil:
    ctx.attrSpec = newSpec()

  if ctx.attrSpec.rootSpec == nil:
    discard ctx.attrSpec.getRootSection()

  module.compileCtx = ctx
  module.attrSpec   = ctx.attrSpec
  module.toIr()
  module.compileCtx = nil
  if module.attrSpec != nil:
    ctx.mergeStaticSpec(module)

proc handleFolding(ctx: CompileCtx, module: Module) =
  if module == nil or module.didFoldingPass:
    return

  for item in module.imports:
    ctx.buildIr(item)

  module.foldingPass()

proc buildFromEntryPoint*(ctx: CompileCtx, entrypointName: string):
                        bool {.discardable.} =
  let modOpt        = ctx.findAndLoadFromUrl(entrypointName)

  ctx.entrypoint = modOpt.getOrElse(nil)
  if modOpt.isNone():
    ctx.loadError("FileNotFound", entryPointName)
    return

  ctx.buildIr(ctx.entrypoint)
  ctx.handleFolding(ctx.entrypoint)
  ctx.buildCfg(ctx.entrypoint)
  ctx.buildAllUnbuiltCfgs(ctx.entrypoint)
  ctx.mergeStaticSpec(ctx.entrypoint)

  for module in ctx.modules.values():
    ctx.buildCfg(module)
    ctx.buildAllUnbuiltCfgs(module)

  for module in ctx.modules.values():
    ctx.resolveDeferredSymbols(module)
    module.foldingPass()

  ctx.wholeProgramChecks()
  ctx.globalScope.calculateOffsets()
  return ctx.errors.canProceed()

proc processSystemDirectory(ctx: CompileCtx) =
  if ctx.sysdir == "":
    return
  var all = getAllFileNames(ctx.sysdir.resolvePath())

  for item in all:
    let (dir, fname, ext) = item.splitFile()
    if ext != ".c4m":
      continue
    let opt = ctx.loadModuleFromLocation(dir, fname, ext)

    if opt.isSome():
      let module = opt.get()
      ctx.buildIr(module)

proc newCompileContext*(spec: ValidationSpec = nil,
                        sysdir               = "~/.local/c0/con4m/",
                        path                 = @[".", "https://chalkdust.io/"],
                        ext                  = ".c4m"): CompileCtx =

  result = CompileCtx(modulePath: path, defaultExt: ext, attrSpec: spec,
                      sysdir: sysdir)

  result.globalScope = initScope()
  result.usedAttrs   = initScope()
  result.modules.initDict()
  result.processSystemDirectory()

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
  if ctx.root != nil:
    print(ctx.root.toRope())
  else:
    print h4("No parse tree produced.")

proc printIr*(ctx: Module) =
  if ctx.ir != nil:
    print ctx.ir.toRope()
  else:
    print h4("No IR produced.")

proc printGlobalScope*(ctx: CompileCtx) =
  print ctx.globalScope.toRope("Globals used")

proc printModuleScope*(ctx: Module) =
  print ctx.moduleScope.toRope("Scope for module '" & ctx.modname & "'")

proc printFuncScope*(fn: FuncInfo) =
  print fn.fnScope.toRope("Scope for function " & fn.name)
  if fn.implementation != nil:
    print fn.implementation.toRope()

proc printAllFuncScopes*(ctx: CompileCtx, m: Module) =
  if m.moduleScope == nil:
    print h4("No functions found due to parse failure.")
    return
  for sym in m.moduleScope.table.values():
    if sym.isFunc:
      for item in sym.fimpls:
        if item.externInfo != nil:
          print(em("External: " & item.name & item.tid.toString()))
          continue
        item.printFuncScope()

proc printAttrsUsed*(ctx: Module) =
  if ctx.usedAttrs != nil:
    print ctx.usedAttrs.toRope("Used attributes")

proc printErrors*(ctx: Module, verbose = true, ll = LlNone) =
  var errsToPrint: seq[Con4mError]

  for item in ctx.errors:
    if cast[int](item.severity) >= cast[int](ll):
      errsToPrint.add(item)

  print errsToPrint.formatErrors(verbose)

proc printProgramCfg*(ctx: CompileCtx) =
  print ctx.entrypoint.cfg.toRope(true)

proc printErrors*(ctx: CompileCtx, verbose = true, ll = LlNone):
  bool =
  var errsToPrint: seq[Con4mError]

  for (_, m) in ctx.modules.items():
    for item in m.errors:
      if cast[int](item.severity) >= cast[int](ll):
        errsToPrint.add(item)

  for item in ctx.errors:
    if cast[int](item.severity) >= cast[int](ll):
      errsToPrint.add(item)

  print(errsToPrint.formatErrors(verbose), file = stderr)

  result = errsToPrint.canProceed()