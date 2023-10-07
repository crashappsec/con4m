import os, tables, streams, strutils, httpclient, net, uri, options, nimutils,
       types, lex, typecheck

# This has some cyclic dependencies, so we make sure C prototypes get
# generated with local scope only; we then do not import those modules.

proc parse(s: seq[Con4mToken], filename: string): Con4mNode {.importc, cdecl.}
proc checkTree(node: Con4mNode, s: ConfigState) {.importc, cdecl.}

var
  defaultUrlStore = ""
  componentInfo: Table[string, ComponentInfo]
  programRoot:   ComponentInfo

proc fullComponentSpec*(name, location: string): string =
  if location == "":
    if defaultUrlStore != "":
      result = defaultUrlStore
    else:
      result = getAppDir().resolvePath()

  elif location.startsWith("https://"):
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
    finalExt = if extension.startsWith("."): extension else: "." & extension
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
    if result.hash == "":
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

  let (valid, toks)   = result.source.lex(result.url)

  if not valid:
    let msg = case toks[^1].kind
    of ErrorTok:         "Invalid character found"
    of ErrorLongComment: "Unterminated comment"
    of ErrorStringLit:   "Unterminated string"
    of ErrorCharLit:     "Invalid char literal"
    of ErrorOtherLit:    "Unterminated literal"
    else:                "Unknown error" # Not be possible w/o a lex bug

  result.entrypoint = toks.parse(result.url)

proc loadComponent*(s: ConfigState, component: ComponentInfo) =
  let savedComponent = s.currentComponent

  if not component.typed:
    s.currentComponent = component
    s.currentComponent.entrypoint.checkTree(s)
    s.currentComponent.typed = true
    for subcomponent in s.currentComponent.componentsUsed:
      s.loadComponent(subcomponent)
    s.currentComponent = savedComponent

  # Now that any sub-components have loaded, we *could* do a check for
  # cycles, but we are currently skipping it. It'll be runtime-only
  # for now.

proc loadComponent*(s: ConfigState, name, location: string, extension = ".c4m"):
                  ComponentInfo =

  if s.currentComponent == ComponentInfo(nil):
    # Main file; register the component.
    programRoot = getComponentReference(name, location)

  result = fetchComponent(name, location, extension)

  s.loadComponent(result)

template setParamValue*(componentName, location, paramName: string,
                        value: Box, valueType: Con4mType,
                        paramStore: untyped) =

  let component = getComponentReference(componentName, location)

  if paramName notin component.paramStore:
    raise newException(ValueError, "Parameter not found: " & paramName)

  let parameter = component.paramStore[paramName]

  if valueType.unify(parameter.defaultType).isBottom():
    raise newException(ValueError, "Incompatable type for: " & paramName)

  parameter.value = some(value)

proc setVariableParamValue*(componentName, location, paramName: string,
                            value: Box, valueType: Con4mType) =
  setParamValue(componentName, location, paramName, value, valueType,
                varParams)

proc setAttributeParamValue*(componentName, location, paramName: string,
                             value: Box, valueType: Con4mType) =
  setParamValue(componentName, location, paramName, value, valueType,
                attrParams)

proc getAllVariableParamInfo*(name, location: string): seq[ParameterInfo] =
  let component = getComponentReference(name, location)

  for _, v in component.varParams:
    result.add(v)

proc getAllAttrParamInfo*(name, location: string): seq[ParameterInfo] =
  let component = getComponentReference(name, location)

  for _, v in component.attrParams:
    result.add(v)
