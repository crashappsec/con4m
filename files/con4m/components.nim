import os, tables, streams, strutils, httpclient, net, uri, options, nimutils,
       types, lex, typecheck, errmsg

# This has some cyclic dependencies, so we make sure C prototypes get
# generated with local scope only; we then do not import those modules.

proc parse(s: seq[Con4mToken], filename: string): Con4mNode {.importc, cdecl.}
proc checkTree(node: Con4mNode, s: ConfigState) {.importc, cdecl.}

var
  defaultUrlStore = ""
  componentInfo: Table[string, ComponentInfo]
  programRoot:   ComponentInfo

proc getRootComponent*(): ComponentInfo =
  return programRoot

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

proc getComponentReference*(url: string): ComponentInfo =
  if url notin componentInfo:
    componentInfo[url] = ComponentInfo(url: url)

  return componentInfo[url]

proc getComponentReference*(name: string, location: string): ComponentInfo =
  return fullComponentSpec(name, location).getComponentReference()

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

proc cacheComponent*(component: ComponentInfo, str: string) =
  ## Use the passed version, especially if stored in memory, etc.

  component.source = str
  component.hash   = sha256(component.source)

  let (valid, toks) = component.source.lex(component.url)

  if not valid:
    let msg = case toks[^1].kind
    of ErrorTok:         "Invalid character found"
    of ErrorLongComment: "Unterminated comment"
    of ErrorStringLit:   "Unterminated string"
    of ErrorCharLit:     "Invalid char literal"
    of ErrorOtherLit:    "Unterminated literal"
    else:                "Unknown error" # Not be possible w/o a lex bug
    fatal(msg, toks[^1])

  component.entrypoint = toks.parse(component.url)

proc cacheComponent*(component: ComponentInfo, stream: Stream) =
  component.cacheComponent(stream.readAll())

proc fetchComponent*(item: ComponentInfo, extension = ".c4m") =
  var source: string

  if item.hash == "":
    if item.url.startsWith("https://"):
      source = item.url.fetchAttempt(extension)

      if source == "":
        raise newException(IOError, "Could not retrieve needed source " &
          "file: " & item.url)
    else:
      try:
        source = item.url.readFile()
      except:
        raise newException(IOError, "Could not retrieve needed source " &
          "file: " & item.url)

    item.cacheComponent(source)

proc fetchComponent*(name, location: string, extension = ".c4m"):
                   ComponentInfo =
  ## This returns a parsed component, but does NOT go beyond that.  The
  ## parse phase will NOT attempt to semantically validate a component,
  ## will NOT go and fetch dependent comonents, and will NOT do cycle
  ## checking at all. Use loadComponent below for those.
  ##
  ## This will raise an exception if anything goes wrong.

  result = getComponentReference(name & extension, location)

  fetchComponent(result, extension)

proc loadComponent*(s: ConfigState, component: ComponentInfo):
                  seq[ComponentInfo] {.discardable.} =
  ## Recursively fetches any dependent components (if not cached) and
  ## checks them.

  if component.hash == "":
    component.fetchComponent()

  let savedComponent = s.currentComponent

  if not component.typed:
    s.currentComponent                     = component
    s.currentComponent.entrypoint.varScope = VarScope(parent: none(VarScope))

    s.currentComponent.entrypoint.checkTree(s)
    s.currentComponent.typed = true

    for subcomponent in s.currentComponent.componentsUsed:
      s.loadComponent(subcomponent)

  for subcomponent in s.currentComponent.componentsUsed:
    let recursiveUsedComponents = s.loadComponent(subcomponent)
    for item in recursiveUsedComponents:
      if item notin result:
        result.add(item)

  if s.currentComponent in result:
    raise newException(ValueError, "Cyclical components are not allowed-- " &
      "component " & s.currentComponent.url & "can import itself")

  s.currentComponent = savedComponent

proc loadCurrentComponent*(s: ConfigState) =
  s.loadComponent(s.currentComponent)

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
