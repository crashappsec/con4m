import basetypes, nimutils, os, strutils, parse, lex, options, err,
       httpclient, net, uri, streams

type
  Module = object
    url*:    string
    where*:  string
    name*:   string
    ext*:    string
    tokens*: TokenBox
    tree*:   Con4mNode
    errs*:   seq[Con4mError]

var
  modules:    Dict[string, Module]
  modulePath: seq[string] = @[".", "https://chalkdust.io/"]


proc commonLoad(url: string, contents: string): Module =
  result = Module(url: url)

  (result.where, result.name, result.ext) = url.splitFile()

  result.tokens = contents.lex(result.errs, url)

  if result.errs.canProceed():
    result.tree   = result.tokens.parseModule(result.errs, url)

    # TODO: add more phases here.

proc loadModuleFromFile(url: string): Option[Module] =
  result = modules.lookup(url)

  if result.isSome():
    return

  try:
    result = some(url.commonLoad(readFile(url)))
  except:
    return none(Module)

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

  if ctx.tokens == nil:
    return

  if start < 0:
    first += ctx.tokens.tokens.len()

  if endix <= 0:
    last += ctx.tokens.tokens.len()

  print TokenBox(tokens: ctx.tokens.tokens[start ..< last]).toRope()

proc printTree*(ctx: Module) =
  if ctx.tree != nil:
    print(ctx.tree.toRope())

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
    module.printTree()
    module.printErrors()
