import types, nimutils, os, strcursor, strutils, parse, lex, options,
       httpclient, net, uri, streams, style

type
  Module = ref object
    url*:    string
    where*:  string
    name*:   string
    ext*:    string
    raw*:    StringCursor
    tokens*: seq[Con4mToken]
    tree*:   Con4mNode
    err*:    bool

var 
  modules:    Dict[string, Module]
  modulePath: seq[string] = @[".", "https://chalkdust.io/"]


proc commonLoad(url: string, contents: string): Module =
  result = Module(url: url)

  (result.where, result.name, result.ext) = url.splitFile()
  result.raw   = newStringCursor(contents)

  let (valid, tokens) = result.raw.lex(url)

  if not valid:
    let
      tok = tokens[^1]
      msg = case tok.kind:
        of ErrorTok:         "Invalid character found"
        of ErrorLongComment: "Unterminated comment"
        of ErrorStringLit:   "Unterminated string"
        of ErrorCharLit:     "Invalid char literal"
        of ErrorOtherLit:    "Unterminated literal"
        else:                "Unknown error"

    fatal(msg, tok)

  result.tokens = tokens
  result.tree   = tokens.parse(url)

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
    
  if start < 0:
    first += ctx.tokens.len()

  if endix <= 0:
    last += ctx.tokens.len() 

  print(toRope(ctx.tokens[start ..< last]))

proc printTree*(ctx: Module) =
  print(ctx.tree.toRope())

