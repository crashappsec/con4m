import con4m_types
import macros
import tables
import strutils
import st
import options
import state
import box

const
  attrCmd = "attr"
  sectCmd = "section"

type
  AttrContents = ref object
    name: string
    typeAsCon4mString: string
    typeAsCon4mNative: Con4mType
    typeAsNimString: string
    defaultVal: NimNode
    required: Option[bool]
    lockOnWrite: Option[bool]
    doc: Option[string]

  AttrSet = OrderedTableRef[string, AttrContents]

  SectContents = ref object
    fullPath: string
    required: Option[NimNode]
    allowed: Option[NimNode]
    attrs: AttrSet
    subsections: OrderedTableRef[string, SectContents]
    customAttrsOk: bool
    doc: Option[string]
    parent: SectContents

  MacroState = ref object
    spec: ConfigSpec
    name: string
    parentSpecName: NimNode
    contents: SectContents
    currentSection: SectContents
    curSecName: string
    nodeStack: seq[NimNode]
    specIdent: NimNode

proc apply*[T, V](s: openArray[T], f: proc (x: T): V{.closure.}):
    seq[V] {.inline.} =
  result = @[]
  for i in 0 ..< s.len:
    result.add(f(s[i]))

proc newSectContents(path: string, parent: SectContents = nil): SectContents =
  result = SectContents(fullPath: path)
  result.attrs = newOrderedTable[string, AttrContents]()
  result.subSections = newOrderedTable[string, SectContents]()
  result.parent = parent

macro dumpAtRuntime*(rest: untyped): untyped =
  var s = treeRepr(rest)

  result = newNimNode(nnkStmtList, lineInfoFrom = rest)
  result.add(nnkCommand.newTree(newIdentNode("echo"), newLit(s)))

proc getValidID(n: NimNode): string =
  case n.kind
  of nnkIdent:
    return n.strVal
  of nnkStrLit:
    if not validIdentifier(n.strVal):
      error("String literal is an invalid identifier.")
    return n.strVal
  else:
    error("Expected an identifier here.", n)

proc getValidType(s: string, n: NimNode): Con4mType =
  try:
    return toCon4mType(s)
  except:
    error("Argument is not a valid Con4m type specification", n)

const tvarnames = "STUVWXYZABCDEFGHIJKLMNOPQRstuvwxyzabcdefghijklmnopqr"

proc toTVarName(n: int): string =
  var v = n + 1
  while v > 0:
    let r = v mod len(tvarnames)
    v = v div len(tvarnames)
    result.add(tvarnames[r])

# TODO: import sugar, Box, tables

proc toNimTypeString(t: Con4mType, tvars: var seq[int]): string =
  case t.kind
  of TypeTVar:
    var i = tvars.find(t.varNum)
    if i == -1:
      i = tvars.len()
      tvars.add(i)
    return toTVarName(i)
  of TypeBool:
    return "bool"
  of TypeInt:
    return "int"
  of TypeFloat:
    return "float"
  of TypeString:
    return "string"
  of TypeList:
    return "seq[" & toNimTypeString(t.itemType, tvars) & "]"
  of TypeDict:
    return "OrderedTableRef[" & toNimTypeString(t.keyType, tvars) &
      ", " & toNimTypeString(t.valType, tvars) & "]"
  of TypeProc:
    var paramTypes: seq[string] = @[]
    for param in t.params:
      paramTypes.add(toNimTypeString(param, tvars))
    if t.va:
      paramTypes[^1] = "varargs[" & paramTypes[^1] & "]"
    let retType = toNimTypeString(t.retType, tvars)

    return "(" & paramTypes.join(", ") & ") -> " & retType
  of TypeBottom:
    return "void"

proc toNimTypeString(t: Con4mType): string =
  var tvars: seq[int]

  return toNimTypeString(t, tvars)

proc getBoolValue(n: NimNode): bool =
  if n.kind != nnkIdent:
    error("Value must be a bool literal (true or false)", n)
  case n.strVal
  of "true":
    return true
  of "false":
    return false
  else:
    error("Value must be a bool literal (true or false)", n)


proc lookForConfigCmds(stmt: NimNode, state: MacroState)

# Arg 0: "attr"
# Arg 1: Name, Identifier, required.
# Arg 2: con4m type, can already be as a string literal, required.
# Arg 3: defaultVal (optional, named: "default = ")
# Arg 4: required   (optional, named: "required = ")
# Arg 5: lockOnWrite (optional, named: "lockOnWrite = ")
# Arg 6: doc (optional, named: "doc = ")
# Arg 7: TODO: validator (optional, named: "validator = ")

proc foundCmdAttr(stmt: NimNode, state: MacroState) =
  if stmt.len() < 3:
    error("Too few nodes to attr. Requires two positional parameters, one " &
          "for the name, name and the second for the con4m type.")
  let
    varName = getValidID(stmt[1])
    strType = toStrLit(stmt[2]).strVal
    c4Type = getValidType(strType, stmt[2])
    nimType = toNimTypeString(c4Type)

  var newAttr = AttrContents(name: varName,
                             typeAsCon4mString: strType,
                             typeAsNimString: nimType,
                             typeAsCon4mNative: c4Type)
  # Now that we're done w/ the positional parameters, scan for
  # keyword parameters.
  var
    i = 3
    foundKw = false
  if i < stmt.len():
    if stmt[i].kind == nnkExprEqExpr:
      foundKw = true
    else:
      newAttr.defaultVal = stmt[i]
      i += 1
  if i < stmt.len() and not foundKw:
    if stmt[i].kind == nnkExprEqExpr:
      foundKw = true
    else:
      newAttr.required = some(getBoolValue(stmt[i]))
      i += 1
  if i < stmt.len() and not foundKw:
    if stmt[i].kind == nnkExprEqExpr:
      foundKw = true
    else:
      newAttr.lockOnWrite = some(getBoolValue(stmt[i]))
      i += 1
  if i < stmt.len() and not foundKw:
    if stmt[i].kind == nnkExprEqExpr:
      foundKw = true
    else:
      newAttr.doc = some(toStrLit(stmt[i]).strVal)
  while i < stmt.len():
    if stmt[i][0].kind != nnkIdent:
      error("Invalid left hand side for keyword parameter", stmt[i][0])
    case stmt[i][0].strVal
    of "defaultVal":
      if newAttr.defaultVal != nil:
        error("Second default value provided", stmt[i][0])
      newAttr.defaultVal = stmt[i][1]
      i += 1
    of "required":
      if newAttr.required.isSome():
        error("Parameter 'required' provided multiple times", stmt[i][0])
      newAttr.required = some(getBoolValue(stmt[i][1]))
      i += 1
    of "lockOnWrite":
      if newAttr.lockOnWrite.isSome():
        error("Parameter 'lockOnWrite' provided multiple times", stmt[i][0])
      newAttr.lockOnWrite = some(getBoolValue(stmt[i][1]))
      i += 1
    of "doc":
      if newAttr.lockOnWrite.isSome():
        error("Parameter 'doc' provided multiple times", stmt[i][0])
      newAttr.doc = some(toStrLit(stmt[i][1]).strVal)
      i += 1
    else:
      error("Unrecognized keyword parameter: " & stmt[i][0].strVal, stmt[i][0])

  state.currentSection.attrs[varName] = newAttr


# Arg 1: section name (ID)
# Arg 2: allowCustomAttrs: bool (optional, kw)
# Arg 3: doc: string (optional, kw)
# Arg 4: requiredSubSections not processed (kw only)
# Arg 5: allowedSubSections not processed (kw only)
# Arg 6: attr and subsection commands (must not be empty)

proc foundCmdSection(stmt: NimNode, state: MacroState) =
  if stmt.len() < 2:
    error("Too few nodes to section. Requires a name.")
  let
    sectName = getValidID(stmt[1])
    parentSection = state.currentSection
    parentPath = parentSection.fullpath
    mypath = if parentPath == "":
               sectName
             else:
               [parentPath, sectName].join(".")

  if parentSection.subsections.contains(sectName):
    error("Duplicate section: " & parentPath, stmt)

  var newSection = newSectContents(myPath, parentSection)
  state.currentSection = newSection
  parentSection.subsections[sectName] = newSection

  var
    i = 2
    foundKw = false
    foundCustomAttr = false
  if i < (stmt.len() - 1):
    case stmt[i].kind
    of nnkExprEqExpr:
      foundKw = true
    of nnkIdent:
      try:
        newSection.customAttrsOk = getBoolValue(stmt[i])
        i = i + 1
        foundCustomAttr = true
      except:
        error("Second argument must be: 1) allowCustomAttrs (opt, bool), " &
              "2) doc (opt, string) or 3) keyword arguments")
    of nnkStrLit:
      newSection.doc = some(stmt[i].strVal)
      i = i + 1
      foundKw = true # No more positional args
    else:
      discard

  if i < (stmt.len() - 1) and not foundKw and stmt[i].kind == nnkStrLit:
    foundKw = true # Redundant, but in case we add a new positional param
    newSection.doc = some(stmt[i].strVal)
    i += 1

  while i < (stmt.len() - 1):
    if stmt[i][0].kind != nnkIdent:
      error("Invalid left hand side for keyword parameter", stmt[i][0])
    case stmt[i][0].strVal
    of "allowCustomAttrs":
      if foundCustomAttr:
        error("Parameter allowCustomAttrs provided twice", stmt[i][0])
      newSection.customAttrsOk = getBoolValue(stmt[i][1])
      foundCustomAttr = true
      i += 1
    of "doc":
      if newSection.doc.isSome():
        error("Parameter 'doc' provided twice", stmt[i][0])
      newSection.doc = some(stmt[i][1].strVal)
      i += 1
    of "requiredSubSections":
      if newSection.required.isSome():
        error("Parameter 'requiredSubSections' provided twice", stmt[i][0])
      newSection.required = some(stmt[i][1])
      i += 1
    of "allowedSubSections":
      if newSection.allowed.isSome():
        error("Parameter 'allowedSubSections' provided twice", stmt[i][0])
      newSection.allowed = some(stmt[i][1])
      i += 1
    else:
      error("Unrecognized keyword parameter: " & stmt[i][0].strVal, stmt[i][0])

  if i != stmt.len() - 1:
    error("Too many parameters to 'section' command", stmt[i+1])

  if stmt[i].kind != nnkStmtList:
    error("Section must contain one or more attr/section statements")

  for item in stmt[^1].items:
    lookForConfigCmds(item, state)

  state.currentSection = parentSection

proc lookForConfigCmds(stmt: NimNode, state: MacroState) =

  case stmt.kind
  of nnkCall, nnkCommand:
    let n = stmt[0]
    if n.strVal == attrCmd:
      foundCmdAttr(stmt, state)
      return
    elif n.strVal == sectCmd:
      foundCmdSection(stmt, state)
      return
    else:
      error("configDef block can only contain 'attr' and 'section' commands",
            stmt)
  else:
    error("configDef block can only contain 'attr' and 'section' commands",
          stmt)

proc parseConfigDef(nameNode: NimNode, rest: NimNode): MacroState =

  nameNode.expectKind(nnkIdent)
  result = MacroState(spec: ConfigSpec(), name: nameNode.strVal)

  result.contents = newSectContents("")
  result.currentSection = result.contents

  #rest.expectKind(nnkStmtList)
  if rest.kind != nnkStmtList:
    error("Configuration must contain one or more attr/section statements")

  for stmt in rest.items:
    lookForConfigCmds(stmt, result)

proc getSectionVarName(sec: SectContents, ctx: MacroState): string =
  let cfgName = ctx.name

  result = cfgName

  if ctx.contents == sec:
    result = result & "Config"
    return

  let parts = sec.fullPath.split(".")

  for item in parts:
    result = result & item.capitalizeAscii()

  for i in 1 ..< parts.len():
    result = result & "Sub"

  result = result & "Section"

# This is used for declaring the destination types, after
# we unbox as far as we can based on static info.
proc con4mTypeToNimNodes(t: Con4mType): NimNode =
  case t.kind
  of TypeBool:
    return newIdentNode("bool")
  of TypeString:
    return newIdentNode("string")
  of TypeInt:
    return newIdentNode("int")
  of TypeFloat:
    return newIdentNode("float")
  of TypeTVar:
    return newIdentNode("Box")
  of TypeBottom:
    return newIdentNode("void")
  of TypeList:
    return nnkBracketExpr.newTree(
             newIdentNode("seq"),
             con4mTypeToNimNodes(t.itemType)
      )
  of TypeDict:
    return nnkBracketExpr.newTree(
             newIdentNode("OrderedTable"),
             con4mTypeToNimNodes(t.keyType),
             con4mTypeToNimNodes(t.valType))
  of TypeProc:
    # Return value goes first.
    let params = nnkFormalParams.newTree(con4mTypeToNimNodes(t.retType))
    for i, item in t.params[0 ..< ^1]:
      params.add(nnkIdentDefs.newTree(
                    newIdentNode("i" & $(i)),
                    con4mTypeToNimNodes(item),
                    newEmptyNode()
        )
      )
    let lastGuts = if t.va:
                     nnkBracketExpr.newTree(
                       newIdentNode("varargs"),
                       con4mTypeToNimNodes(t.params[^1])
                     )
                   else:
                     con4mTypeToNimNodes(t.params[^1])


    params.add(nnkIdentDefs.newTree(
                  newIdentNode("i" & $(len(t.params) - 1)),
                  lastGuts,
                  newEmptyNode()
      )
    )
    return nnkProcTy.newTree(params, newEmptyNode())


proc buildDeclsOneSection(ctx: MacroState) =
  var myItems = newNimNode(nnkRecList) # TODO: get line # info in there.
  let thisSection = ctx.currentSection
  let secName = getSectionVarName(thisSection, ctx)

  # Section decls go in front of us, so we can reference them.
  for name, subsection in thisSection.subsections:
    ctx.currentSection = subsection
    buildDeclsOneSection(ctx)
    ctx.currentSection = thisSection
    let subSecName = getSectionVarName(subsection, ctx)
    myItems.add(nnkIdentDefs.newTree(
                   newIdentNode(name),
                   nnkBracketExpr.newTree(
                     newIdentNode("OrderedTable"),
                     newIdentNode("string"),
                     newIdentNode(subSecname)
      ),
      newEmptyNode()
      )
      )
  
  # Now we can add in attributes.
  for k, v in thisSection.attrs:
    myItems.add(nnkIdentDefs.newTree(
                   newIdentNode(k),
                   con4mTypeToNimNodes(v.typeAsCon4mNative),
                   newEmptyNode()
      )
    )
  let defnode = nnkTypeDef.newTree(newIdentNode(secName),
                                    newEmptyNode(),
                                    nnkRefTy.newTree(
                                      nnkObjectTy.newTree(
                                        newEmptyNode(),
                                        newEmptyNode(),
                                        myItems
                                      )
                              )
  )

  ctx.nodeStack.add(defnode)

proc buildTypeDecls(ctx: MacroState): NimNode =
  ctx.currentSection = ctx.contents
  buildDeclsOneSection(ctx)
  var defList = newNimNode(nnkTypeSection)

  for item in ctx.nodeStack:
    defList.add(item)

  return defList

proc handleBoxing(t: Con4mType, v: NimNode): NimNode =
  case t.kind
  of TypeBool, TypeInt, TypeFloat, TypeString:
    return nnkCall.newTree(newIdentNode("box"), v)
  of TypeTVar: # Already boxed.
    return v
  of TypeProc:
    error("Con4m does not currently support function pointers.")
  of TypeList:
    return nnkCall.newTree(newIdentNode("box"), v)
  of TypeDict:
    return nnkCall.newTree(newIdentNode("boxDict"), v)
  of TypeBottom:
    error("Cannot box bottom value")
    
proc transformValToOptBox(attr: AttrContents): NimNode =
  if attr.defaultVal == nil:
    return nnkCall.newTree(newIdentNode("none"), newIdentNode("Box"))

  return nnkCall.newTree(newIdentNode("some"),
                         handleBoxing(attr.typeAsCon4mNative, attr.defaultVal)
                         )


proc optBoolToIdent(b: Option[bool]): NimNode =
  if b.isNone() or not b.get():
    return newIdentNode("false")
  else:
    return newIdentNode("true")

proc optStrToLit(s: Option[string]): NimNode =
  if s.isNone():
    return newLit("")
  else:
    return newLit(s.get())

proc buildSectionSpec(ctx: MacroState): NimNode =
  result = newNimNode(nnkStmtList)
  
  let
    curSec         = ctx.currentSection
    secPath        = split(curSec.fullpath, ".").join("Dot")
    secSpecVarName = newIdentNode("confSec" & ctx.name & secPath)
    specName       = ctx.parentSpecName
    secLit         = newLit(ctx.curSecName)
    doc            = optStrToLit(curSec.doc)
                       
    allowed  = if curSec.allowed.isSome():
                 curSec.allowed.get()
               else:
                 nnkPrefix.newTree(newIdentNode("@"), newNimNode(nnkBracket))
    required = if curSec.required.isSome():
                 curSec.required.get()
               else:
                 nnkPrefix.newTree(newIdentNode("@"), newNimNode(nnkBracket))
    customOk = if curSec.customAttrsOk:
                 newIdentNode("true")
               else:
                 newIdentNode("false")
                 
    # "quote do" fails spectactularly in weird ways if substitutions are
    # dotted values at all.
    n = quote do:
      var `secSpecVarName` =
        addSection(`specName`,
                   `secLit`,
                   validSubSecs = `allowed`,
                   requiredSubSecs = `required`,
                   doc = `doc`,
                   allowCustomAttrs = `customOk`)
  result.add(n)
  
  for attrName, attrContents in curSec.attrs:
    let
      attrLit  = newLit(attrName)
      typeLit  = newLit(attrContents.typeAsCon4mString)
      boxVal   = transformValToOptBox(attrContents)
      required = optBoolToIdent(attrContents.required)
      loWrite  = optBoolToIdent(attrContents.lockOnWrite)
      doc      = optStrToLit(attrContents.doc)
      # TODO: add in generating the validator here.             
      x = quote do:
        `secSpecVarName`.addAttr(`attrLit`,
                                 `typeLit`,
                                 default = `boxVal`,
                                 required = `required`,
                                 lockOnWrite = `loWrite`,
                                 doc = `doc`)
                                   
    result.add(x)

  let subsections = curSec.subsections
  
  for name, secinf in subsections:
    ctx.curSecName = name
    ctx.currentSection = secinf
    ctx.parentSpecName = specName
    let more = buildSectionSpec(ctx)

    more.copyChildrenTo(result)

proc buildGlobalSectionSpec(ctx: MacroState): NimNode =

  result = newNimNode(nnkStmtList)
  
  for attrName, attrContents in ctx.contents.attrs:
    let
      attrLit  = newLit(attrName)
      typeLit  = newLit(attrContents.typeAsCon4mString)
      boxVal   = transformValToOptBox(attrContents)
      required = optBoolToIdent(attrContents.required)
      loWrite  = optBoolToIdent(attrContents.lockOnWrite)
      doc      = optStrToLit(attrContents.doc)
      specId   = ctx.specIdent
                   
      # TODO: add in generating the validator here.             
      x = quote do:
                   addGlobalAttr(`specId`,
                                 `attrLit`,
                                 `typeLit`,
                                 default = `boxVal`,
                                required = `required`,
                                lockOnWrite = `loWrite`,
                                doc = `doc`)
    result.add(x)

  for name, secinf in ctx.contents.subsections:
    ctx.curSecName = name
    ctx.currentSection = secinf

    let more = buildSectionSpec(ctx)
    more.copyChildrenTo(result)

      
  
proc buildConfigSpec(ctx: MacroState): NimNode =
  let specVarName = "confSpec" & ctx.name
  
  ctx.specIdent      = newIdentNode(specVarName)
  ctx.nodeStack      = @[]
  ctx.parentSpecName = newIdentNode(specVarName)

  result = nnkStmtList.newTree(
    nnkVarSection.newTree(
                          nnkIdentDefs.newTree(
                            newIdentNode(specVarName),
                            newEmptyNode(),
                            nnkCall.newTree(
                             newIdentNode("newConfigSpec")
                            )
                          )
                        )
                      )

                    
  
  let more = buildGlobalSectionSpec(ctx)

  more.copyChildrenTo(result)

  # buildLoadingProc(ctx)

  #echo treerepr(result)
  #echo toStrLit(result)


# The indirection with this macro and the kludge parameter is the only
# way I've found to detect if we're in a module's scope.
# We do not want to generate procedures that are nested inside a proc,
# thus the kludge.
#
# Didn't find this one via Google; was thanks to elegantbeef on the
# Nim discord server (Jason Beetham, beefers331@gmail.com)
macro cDefActual*(kludge: int, nameNode: untyped, rest: untyped): untyped =
  var
    state = parseConfigDef(nameNode, rest)
    owner = kludge.owner

  if owner.symKind != nskModule:
    error("Only use this macro in a module scope")
  
  result = newNimNode(nnkStmtList, lineInfoFrom = nameNode)
  
  let
    typeDecls = buildTypeDecls(state)
 
  result.add(typeDecls)
 
  let rest = buildConfigSpec(state)
  
  rest.copyChildrenTo(result)

template configDef*(nameNode: untyped, rest: untyped): untyped =
  var kludge = 100

  when false:
    dumpAtRuntime(cDefActual(kludge, nameNode, rest))
  else:
    cDefActual(i, nameNode, rest)
    
configDef(Sami):
  attr(config_path, [string], @[".", "~"])
  attr(config_filename, string, "sami.conf")
  attr(color, bool, false)
  attr(log_level, string, "warn")
  attr(dry_run, string, false)
  attr(artifact_search_path, [string], @["."])
  attr(recursive, bool, true)
  attr(output_dir, string, ".")
  attr(output_file, string, "sami-extractions.json")
  section("key", allowedSubSections = @["*", "*.json", "*.binary"]):
    attr("required", bool, false)
    attr("missing_action", string, "warn")
    attr("system", bool, false)
    attr("squash",
         bool,
         defaultVal = true,
         lockOnWrite = true,
         required = true)
    attr("standard", bool, false)
    attr("must_force", bool, false)
    attr("plugin_ok", bool, true)
    attr("skip", bool, false)
    attr("priority", int, required = false)
    attr("since", string, required = false)
    attr("type", string, required = true)
    attr("value", @x, required = false)
    attr("codec", bool, false)
    attr("docstring", string, required = false)


#[
dumpAstGen:
dumpTree:
      proc loadSamiConfig(ctx: ConfigState): SamiConf =
        result = SamiConf()

        var tmpBox: Box

        tmpBox = ctx.getConfigVar("config_path").get()

        let config_path_box_seq_str = unbox[seq[Box]](tmpBox)
        var config_path_seq_str: seq[string] = @[]

        for item in config_path_box_seq_str:
          config_path_seq_str.add(unbox[string](item))

        result.config_path = config_path_seq_str

        tmpBox = ctx.getConfigVar("config_filename").get()
        result.config_filename = unbox[string](tmpBox)

        tmpBox = ctx.getConfigVar("color").get()
        result.color = unbox[bool](tmpBox)

        tmpBox = ctx.getConfigVar("log_level").get()
        result.log_level = unbox[string](tmpBox)

        tmpBox = ctx.getConfigVar("dry_run").get()
        result.dry_run  = unbox[bool](tmpBox)

        tmpBox = ctx.getConfigVar("artifact_search_path").get()

        let artifact_search_path_box_seq_str = unbox[seq[Box]](tmpBox)
        var artifact_search_path_seq_str: seq[string] = @[]

        for item in artifact_search_path_box_seq_str:
          artifact_search_path_seq_str.add(unbox[string](item))

        result.artifact_search_path = artifact_search_path_seq_str

        tmpBox = ctx.getConfigVar("recursive").get()
        result.recursive = unbox[bool](tmpBox)

        tmpBox = ctx.getConfigVar("output_dir").get()
        result.output_dir = unbox[string](tmpBox)

        tmpBox = ctx.getConfigVar("output_file").get()
        result.output_file = unbox[string](tmpBox)

        let sectionInfo = ctx.getAllSectSTs()

        for (k, v) in sectionInfo["key"]:
          var stEntry: STEntry
          var entryOpt: Option[STEntry]

          let sectionKey = k.split(".")[1 .. ^1].join(".")
          var sectionData = SamiKeySection()
          result.key[sectionKey] = sectionData

          stEntry = v.lookupAttr("required").get()
          tmpBox = stEntry.value.get()
          sectionData.required = unbox[bool](tmpBox)

          stEntry = v.lookupAttr("missing_action").get()
          tmpBox = stEntry.value.get()
          sectionData.missing_action = unbox[string](tmpBox)

          stEntry = v.lookupAttr("system").get()
          tmpBox = stEntry.value.get()
          sectionData.system = unbox[bool](tmpBox)

          stEntry = v.lookupAttr("squash").get()
          tmpBox = stEntry.value.get()
          sectionData.squash = unbox[bool](tmpBox)

          stEntry = v.lookupAttr("standard").get()
          tmpBox = stEntry.value.get()
          sectionData.standard = unbox[bool](tmpBox)

          stEntry = v.lookupAttr("must_force").get()
          tmpBox = stEntry.value.get()
          sectionData.must_force = unbox[bool](tmpBox)

          stEntry = v.lookupAttr("plugin_ok").get()
          tmpBox = stEntry.value.get()
          sectionData.plugin_ok = unbox[bool](tmpBox)

          stEntry = v.lookupAttr("skip").get()
          tmpBox = stEntry.value.get()
          sectionData.skip = unbox[bool](tmpBox)

          entryOpt = v.lookupAttr("priority")
          if entryOpt.isSome():
            stEntry = entryOpt.get()
            tmpBox = stEntry.value.get()
            sectionData.priority = some(unbox[int](tmpBox))

          entryOpt = v.lookupAttr("since")
          if entryOpt.isSome():
            stEntry = entryOpt.get()
            tmpBox = stEntry.value.get()
            sectionData.since = some(unbox[string](tmpBox))

          stEntry = v.lookupAttr("type").get()
          tmpBox = stEntry.value.get()
          sectionData.`type` = unbox[string](tmpBox)

          entryOpt = v.lookupAttr("value")
          if entryOpt.isSome():
            stEntry = entryOpt.get()
            tmpBox = stEntry.value.get()
            sectionData.value = some(unbox[Box](tmpBox))

          stEntry = v.lookupAttr("codec").get()
          tmpBox = stEntry.value.get()
          sectionData.codec = unbox[bool](tmpBox)

          entryOpt = v.lookupAttr("docstring")
          if entryOpt.isSome():
            stEntry = entryOpt.get()
            tmpBox = stEntry.value.get()
            sectionData.docstring = some(unbox[string](tmpBox))
          ]#
            # Note that "user-supplied bits need to be here somewhere too.
            # If there's a 'default value', then it doesn't get an option type,
            # even if it's not required.


          # End of tree dumping
