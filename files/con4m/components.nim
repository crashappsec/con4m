import os, tables, types, nimutils, uri, lex

# This has some cyclic dependencies, so we make sure C prototypes get
# generated with local scope only; we then do not import those modules.

proc parse(s: Stream, filename: string): Con4mNode {.importc, cdecl.}
proc checkTree(node: Con4mNode, s: ConfigState) {.importc, cdecl.}

var
  defaultUrlStore = ""
  componentInfo   = Table[string, ComponentInfo]
  programRoot: ComponentInfo

proc fullComponentSpec*(name, location: string): string =
  if location == "":
    if defaultUrlStore != "":
      result = defaultUrlStore
    else:
      result = getAppDir().resolvePath()

  elif location.startswith("https://"):
    result = location

  else:
    result = location.resolvePath()

proc setDefaultStoreUrl*(url: string) =
  once:
    defaultUrlStore = url

proc getComponentReference*(name: string, location: string): ComponentInfo =
  let key = fullComponentSpec(name, location)

  if key notin componentInfo:
    componentInfo[key] = ComponentInfo(url: key)

  return componentInfo[key]

proc fetchAttempt(url, extension: string): string =
  var
    finalExt = if extension.startswith("."): extension else "." & extension
    uri      = parseUri(url & finalExt)
    context  = newContext(verifyMode = CVerifyPeer)
    client   = newHttpClient(sslContext = context, timeout = 1000)
    response = client.safeRequest(url = uri, httpMethod = HttpGet)

  if response.status[0] != '2':
    return ""

  return response.bodyStream.readAll()

proc fetchComponent*(name, location: string, extension = ".c4m"):
                   ComponentInfo =
  ## This returns a parsed component, but does NOT go beyond that.  The
  ## parse phase will NOT attempt to semantically validate a component,
  ## will NOT go and fetch dependent comonents, and will NOT do cycle
  ## checking at all. Use loadComponent below for those.
  ##
  ## This will raise an exception if anything goes wrong.

  result = getComponentReference(name, location)

  if result.url.startsWith("https://"):
    if checkForUpdates or result.hash == "":
      let
        source = result.url.fetchAttempt(extension)
        hash   = sha256(source)

      if source == "":
        if result.hash == "":
          raise newException(IOError, "Could not retrieve needed source " &
            "file: " & result.url)

      elif hash == result.hash:
        return

      result.hash   = hash
      result.source = source

  else:
    try:
      let
        source = result.url.readFile()
        hash   = sha256(source)

      if hash == result.hash:
        return

      result.hash   = hash
      result.source = source
    except:
      if result.hash == "":
        raise newException(IOError, "Could not retrieve needed source " &
          "file: " & result.url)

  let
    stream          = newStringStream(result.source)
    (valid, toks)   = stream.lex(result.url)

  if not valid:
    let msg = case tokens[^1].kind
    of ErrorTok:         "Invalid character found"
    of ErrorLongComment: "Unterminated comment"
    of ErrorStringLit:   "Unterminated string"
    of ErrorCharLit:     "Invalid char literal"
    of ErrorOtherLit:    "Unterminated literal"
    else:                "Unknown error" # Not be possible w/o a lex bug

  result.entrypoint = tokens.parse(result.url)

proc loadComponent*(s: ConfigState, component: ComponentInfo) =
  let savedComponent = s.currentComponent

  if not result.typed:
    state.currentComponent = result
    state.currentComponent.entrypoint.checkTree(state)
    state.currentComponent.typed = true
    for component in state.currentComponent.componentsUsed:
      s.loadComponent(component)
    state.currentComponent = savedComponent

  # Now that any sub-components have loaded, we *could* do a check for
  # cycles, but we are currently skipping it. It'll be runtime-only
  # for now.

proc loadComponent*(s: ConfigState, name, location: string, extension = ".c4m"):
                  ComponentInfo =

  if s.currentComponent == "":
    # Main file; register the component.
    registerComponent(name, location)

  result = fetchComponent(name, location, extension)
  s.loadComponent(result)
