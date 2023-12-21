# TODO:
# === High priority ===
# - When folding, set items in the symbol table if in an assign node,
#   and the symbol is marked as constant.
# - Fix function declaration return type syntax
# - Defined-but-never-used across whole program
# - Add post-IR check that const things have been assigned.
#   elsewhere.
# - Implement _ as a 'discard' variable.
# - Deal with the rhs ambiguity better.
# - C-level interface to attributes
# - Execution from IR (possibly compressing IR more)
# - Stack traces during execution.
# - Spec checking after execution.
# - Checkpointing runtime state.
# - Module state caching for re-linking when conf files change.
# - FFI
# - Swap in hatrack lists (and add rings?).
# - No-side-effect prop for funcs to allow calling functions at compile time.
# - Allow assignment inside var / global / const statements.
# - Component logic
# - ConvertCallbackLit type secolution.
# - In showCallMistakes(), show which functions have the wrong # of args,
#   and which parameters are right / wrong.
# - Base types should be treated more like proper classes?
# - Case statement / typecase
# - Re-implement standard library / wrappings.

# == Medium ==
# - C api and bindings to other languages.
# - Doc API.
# - Hook getopt back up.
# - Update the pretty printer.
# - Extra lines in error messages shouldn't get the huge table indent.
# - Sort errors by file / line (they come out by phase in IR portion).
# - REPL
# - Default parameters
# - Litmods for common rope types
# - Restrict $i and $i_label symbols
# - Give names to enums / turn them into int subtypes
# - Fold for list indexes when the length is fixed size.
# - Add 'error' to functions.
# - Add global enum
# - :: module scope operator; root::, module::, local:: I think?
# - Unreachable funciton analysis
# - Keyword arguments
# - treat assignment to '_' as 'discard'.
# - Move temporary compile state to a throw-away reference obj that's
#   carried in the module state.
# - Debug mode.
# - Enumerate function pointer literals and assume they're always live
#   and called as part of the entry point.
# - Properly handle

# == Lower priority ==
# - Error msg squelching and colating
# - GUI for repl; show trees, etc.
# - let all the IO stuff be themeable
# - Add objects, with typevars that can bind to all fields...
# - Add maybe
# - Add oneof
# - Add ref
# - for x in <container>: generate a call to items() if the object is
#   not one of the built-in types.
# - Todo-- index for non-base types should generate a rewrite func
# - Validation routines need routines to validate their inputs.
# - For dicts and lists: capture the value when possible and perform folding
# - Support litmods for containers.
# - TODO: should there be an option to leave functions in the module scope?
#   If so, what's the syntax?
# - Macros / aspects
# - Allow arbitrary blocks within statements?

import nimutils, os, strutils, httpclient, net, uri, streams, cfg

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

proc loadModule(ctx: CompileCtx, module: Module)

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

  var module = newModuleObj(source.get(), fname, location, ext, url)
  ctx.modules[moduleKey] = module

  ctx.loadModule(module)
  return some(module)

proc findAndLoadModule*(ctx: CompileCtx, location, fname, ext: string):
                      Option[Module] {.exportc, cdecl.} =
  if location != "":
    return ctx.loadModuleFromLocation(location, fname, ext)

  for possibleLoc in ctx.modulePath:
    let possibly = ctx.loadModuleFromLocation(possibleLoc, fname, ext)
    if possibly.isSome():
      return possibly

  ctx.loadError("NotFound", fname)
  return none(Module)

proc findAndLoadModule(ctx: CompileCtx, url: string): Option[Module] =
  let (loc, name, ext) = url.splitFile()
  return ctx.findAndLoadModule(loc, name, ext)

proc loadModule(ctx: CompileCtx, module: Module) =
  module.globalScope = ctx.globalScope
  module.attrSpec    = ctx.attrSpec

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
  if module.ir != nil:
    # Already been done.
    return

  # Even if one phase fails, go through as much as possible to buffer up as
  # many errors as possible.

  # Currently the compile context is only needed during IR gen / folding
  # to look up other modules that we use.
  module.compileCtx = ctx
  module.toIr()
  module.compileCtx = nil

  module.resolveDeferredSymbols()
  module.foldingPass()

proc handleFolding(ctx: CompileCtx, module: Module) =
  if module.didFoldingPass:
    return

  for item in module.imports:
    ctx.buildIr(item)

  module.foldingPass()

proc buildFromEntryPoint*(entrypointName: string): CompileCtx =
  result = CompileCtx()
  result.globalScope = initScope()
  result.modules.initDict()

  let modOpt        = result.findAndLoadModule(entrypointName)
  result.entrypoint = modOpt.getOrElse(nil)

  result.buildIr(result.entrypoint)
  result.handleFolding(result.entrypoint)

  result.buildCfg(result.entrypoint)

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
  print ctx.moduleScope.toRope("Module scope")
proc printFuncScope*(fn: FuncInfo) =
  print fn.fnScope.toRope("Scope for function " & fn.name)
  if fn.implementation != nil:
    print fn.implementation.toRope()
proc printAllFuncScopes*(ctx: Module) =
  if ctx.moduleScope == nil:
    print h4("No functions found due to parse failure.")
    return
  for sym in ctx.moduleScope.table.values():
    if sym.isFunc:
      for item in sym.fimpls:
        item.printFuncScope()
  for sym in ctx.globalScope.table.values():
    if sym.isFunc:
      for item in sym.fimpls:
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

when isMainModule:
  useCrashTheme()
  var session = buildFromEntryPoint("ptest_tiny.c4m")

  if session.entrypoint != nil:
    var module = session.entrypoint
    module.printTokens()
    module.printParseTree()
    module.printIr()
    module.printErrors()
    module.printAttrsUsed()
    module.printAllFuncScopes()
    module.printModuleScope()
    session.printGlobalScope()
    print module.cfg.toRope(true)
  else:
    print em("Could not find module 'ptest.c4m'")
