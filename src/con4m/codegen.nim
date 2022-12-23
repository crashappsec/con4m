## Macros for abstracting away config file specification, parsing,
## validation and loading.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import ./types
import st
import options
import spec
import eval

import macros
import tables
import strutils
import parse
import streams

const
  attrCmd = "attr"
  sectCmd = "section"

# In this code, you give a sepecification to the compiler that tells
# it what a valid config file schema looks like.  Here, we take care
# of generating the code that compares a read-in config file to that
# spec, and then, if valid, loads the configuration into data
# structures that are implied by the schema.
#
# For instance, you might have the following spec:
#
# configDef(Test):
#   attr(max_hosts, int, required = true)
#   section(host, allowedSubSections = @["*"])
#     attr("ip", string, required = true)
#     attr("port", int, required = true)
#
# confTest.loadConfTestFromFile("myconfig")
#
# This will allow users to write the following config file:
#
# max_hosts: 10
# host localhost {
#    ip: "127.0.0.1"
#    port: 5000
#  }
#
# host "john's server" {
#    ip: "10.1.100.3"
#    port: 5000
#
#  }
#
# Once the config file is loaded and validated, the configuration will
# be stored in a variable, confTest, which can be queried easily:
#
# assert confTest.max_hosts == 10 (or confTest.maxHosts)
# for name, contents in confTest.host:
#   echo name
#   echo contents.ip, ":", contents.port
#
# The above code prints:
#
# localhost
# 127.0.0.1:5000
# john's server
# 10.1.100.3:5000
#
#
# Basically, here's how this all works:
#
# 1) The configDef() macro generates a data structure called
#    TestConfig, that will be the ultimate home for the data. It has
#    all the fields needed to access data directly once loaded.  These
#    data structures are taken directly from the spec.
#
# 2) The macro generates the code that, at run-time, will create a
#    specification object of type ConfigSpec, containing the information
#    provided from the macro.
#
# 3) We generate the procedure loadConfTestFromFile, which reads the
#    config file, and sends it to the config file parser.  Then, if
#    the parse is successful, the resulting values are compared
#    against the ConfigSpec. If the config file doesn't validate, we
#    bail out.
#
# 4) Finally, we copy over the parser's data into instances of the
#    generated data structures, accessible through the confTest
#    variable.  This allows you to access data easily and quickly,
#    and, if you want to, discard the extra data loaded in (though
#    there are reasons to consider keeping it around).
#
# Note that, currently, these macros are not generating code that will
# properly present keys that were user-added. That is coming.  You can
# currently access those through the parser's raw output.




# The first two of the below data types are used for keeping track of
# state while we are generating code to load the config file.
#
# The AttrContents type captures info we need about attribute for
# compilation, and the SectContents similarly keeps around info we
# need about sections.  This data is all filled in based on the config
# file schema that is provided as input.
#
# Note that there are also RUNTIME data structures representing
# attrs and sections.  These data structures are just for compile
# time info.
#
# The third data structure represents state we pass around across
# calls at compile time, when generating all this code.
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
    specIdent: NimNode
    currentStIdent: NimNode
    entryOptIdent: NimNode
    stEntryIdent: NimNode
    contents: SectContents
    currentSection: SectContents
    curSecName: string
    nodeStack: seq[NimNode]
    genExportMarkers: bool

proc newSectContents(path: string, parent: SectContents = nil): SectContents =
  result = SectContents(fullPath: path)
  result.attrs = newOrderedTable[string, AttrContents]()
  result.subSections = newOrderedTable[string, SectContents]()
  result.parent = parent

proc getValidID(n: NimNode): string =
  ## In the macro's first parameter, we accept strings or literals to
  ## avoid people having to remember. However, we really want a
  ## literal there, so we call Nim's validIdentifier() function, to
  ## make sure we can turn it into a literal.
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
  ## When the user specifies types, we currently have them specify
  ## Con4m types, not Nim types, because Con4m types are far more
  ## limited.  This turns the string that a user enters into
  ## a tree form for us, so we can more easily work with it.
  try:
    return toCon4mType(s)
  except:
    error("Argument is not a valid Con4m type specification", n)

const tvarnames = "STUVWXYZABCDEFGHIJKLMNOPQRstuvwxyzabcdefghijklmnopqr"

proc toTVarName(n: int): string =
  ## This helper creates a new, unique type variable name.  This is a detail
  ## of the type system, and isn't very important.
  var v = n + 1
  while v > 0:
    let r = v mod len(tvarnames)
    v = v div len(tvarnames)
    result.add(tvarnames[r])

# TODO: import sugar, Box, tables
proc toNimTypeString(t: Con4mType, tvars: var seq[int]): string =
  ## This is the helper version of toNimTypeString, which does the bulk
  ## of the work in converting a Con4m type into a Nim type (as a string).
  ## Again, we don't call this verison.
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
  of TypeTuple:
    var s: seq[string]
    for item in t.itemTypes:
      s.add(toNimTypeString(item.itemType, tvars))
    return "tuple[" & s.join(", ") & "]"
  of TypeList:
    return "seq[" & toNimTypeString(t.itemType, tvars) & "]"
  of TypeDict:
    return "TableRef[" & toNimTypeString(t.keyType, tvars) &
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
  ## Convert a Con4m type into a Nim type (as a string).  Currently,
  ## the implementation isn't using this for anything.  Originally, I
  ## was going to get NIM to generate the parse nodes needed for me,
  ## but it had some issue, so now there's a function below
  ## (con4mTypeToNimNodes) that generates the AST we need.  This will
  ## get excised once I know I'm never going to find a use for it.
  var tvars: seq[int]

  return toNimTypeString(t, tvars)

proc getBoolValue(n: NimNode): bool =
  ## This takes a parse tree node that is expected to be JUST a literal
  ## true or literal false, and turns it into an actual literal (i.e.,
  ## for our use in testing).
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

proc foundCmdAttr(stmt: NimNode, state: MacroState) =
  ## Here begins code for the parsing stage, where we parse the macro code
  ## the user wrote.  The user's code at this point is in tree form, so we're
  ## looking at tree nodes, and squirelling off information into our `state`
  ## variable.  The parse tree itself will eventually get thrown away.
  ##
  ## This proc parses the `attr` command.
  ##
  ## Arg 0: "attr"
  ## Arg 1: Name, Identifier, required.
  ## Arg 2: con4m type, can already be as a string literal, required.
  ## Arg 3: defaultVal (optional, named: "defaultVal = ")
  ## Arg 4: required   (optional, named: "required = ")
  ## Arg 5: lockOnWrite (optional, named: "lockOnWrite = ")
  ## Arg 6: doc (optional, named: "doc = ")
  ## Arg 7: TODO: validator (optional, named: "validator = ")
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

proc foundCmdSection(stmt: NimNode, state: MacroState) =
  ## This is the code for walking the tree to parse out a `section` command.
  ## Arg 0: "section"
  ## Arg 1: section name (ID)
  ## Arg 2: allowCustomAttrs: bool (optional, kw)
  ## Arg 3: doc: string (optional, kw)
  ## Arg 4: requiredSubSections not processed (kw only)
  ## Arg 5: allowedSubSections not processed (kw only)
  ## Arg 6: attr and subsection commands (must not be empty)
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
  ## Here, we're identifying the config commands, dispatching to one of
  ## the functions above us.  If we see anything other than calls to our
  ## commands, then we BAIL, because we don't allow the user to intermix
  ## other code with our macro.
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
  of nnkCommentStmt:
    return
  else:
    error("configDef block can only contain 'attr' and 'section' commands",
          stmt)

proc parseConfigDef(nameNode: NimNode, rest: NimNode,
    markers: bool): MacroState =
  ## This is the entry point for parsing; it sets up the state, and kicks
  ## off lookForConfigCommands on each statement in the macro.
  nameNode.expectKind(nnkIdent)
  result = MacroState(spec: ConfigSpec(),
                      name: nameNode.strVal,
                      genExportMarkers: markers)

  result.contents = newSectContents("")
  result.currentSection = result.contents

  #rest.expectKind(nnkStmtList)
  if rest.kind != nnkStmtList:
    error("Configuration must contain one or more attr/section statements")

  for stmt in rest.items:
    lookForConfigCmds(stmt, result)

proc getSectionTypeName(sec: SectContents, ctx: MacroState): string =
  ## Now we're entering into the code that supports generating the
  ## replacement code.  This is where things can probably start to get
  ## confusing.
  result = ctx.name.capitalizeAscii()

  if ctx.contents == sec:
    result = result & "Config"
    return

  let parts = sec.fullPath.split(".")

  for item in parts:
    result = result & item.capitalizeAscii()

  while false: # Currently, we are only supporting top-level section typing.
    for i in 1 ..< parts.len():
      result = result & "Sub"

  result = result & "Section"

proc con4mTypeToNimNodes(t: Con4mType): NimNode =
  ## Above, we'd gone from a Con4m type to a nim type as a string.  This
  ## proc is similar, but instead creates the parse tree nodes we need
  ## to insert when we want to declare something to be of the
  ## destination type.
  ##
  ## This is the call that is ACTUALLY useful.
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
             con4mTypeToNimNodes(t.itemType))
  of TypeTuple:
    result = nnkTupleConstr.newNimNode()
    for item in t.itemTypes:
      result.add(con4mTypeToNimNodes(item))
    return
  of TypeDict:
    return nnkBracketExpr.newTree(
             newIdentNode("Con4mDict"),
             con4mTypeToNimNodes(t.keyType),
             con4mTypeToNimNodes(t.valType))
  of TypeProc:
    # Return value goes first.
    let params = nnkFormalParams.newTree(con4mTypeToNimNodes(t.retType))
    for i, item in t.params[0 ..< ^1]:
      params.add(nnkIdentDefs.newTree(
                    newIdentNode("i" & $(i)),
                    con4mTypeToNimNodes(item),
                    newEmptyNode()))

    let lastGuts = if t.va:
                     nnkBracketExpr.newTree(
                       newIdentNode("varargs"),
                       con4mTypeToNimNodes(t.params[^1]))
                   else:
                     con4mTypeToNimNodes(t.params[^1])


    params.add(nnkIdentDefs.newTree(
                  newIdentNode("i" & $(len(t.params) - 1)),
                  lastGuts,
                  newEmptyNode()))

    return nnkProcTy.newTree(params, newEmptyNode())


const nimReservedWords = ["addr", "and", "as", "asm", "bind", "block",
                          "break", "case", "cast", "concept", "const",
                          "continue", "converter", "defer", "discard",
                          "distinct", "div", "do", "elif", "else",
                          "end", "enum", "except", "export",
                          "finally", "for", "from", "func", "if",
                          "import", "in", "include", "interface",
                          "is", "isnot", "iterator", "let", "macro",
                          "method", "mixin", "mod", "nil", "not",
                          "notin", "object", "of", "or", "out",
                          "proc", "ptr", "raise", "ref", "return",
                          "shl", "shr", "static", "template", "try",
                          "tuple", "type", "using", "var", "when",
                          "while", "xor", "yield"]

proc safeIdent(name: string): NimNode =
  if name in nimReservedWords:
    return nnkAccQuoted.newTree(newIdentNode(name))
  return newIdentNode(name)


proc buildDeclsOneSection(ctx: MacroState) =
  ## This code generates the data type declaration for a section.
  ## The name for the data type is retrieved from getSectionTypeName.
  ##
  ## The actual node layout for such things I get from writing small
  ## examples of what I WANT to generate, then I get Nim to dump the AST,
  ## making it easy for me to make sure I generate the same AST.
  var myItems = newNimNode(nnkRecList) # TODO: get line # info in there.
  let thisSection = ctx.currentSection
  let secName = getSectionTypeName(thisSection, ctx)

  # Section decls go in front of us, so we can reference them.
  for name, subsection in thisSection.subsections:
    ctx.currentSection = subsection
    buildDeclsOneSection(ctx)
    ctx.currentSection = thisSection
    let subSecName = getSectionTypeName(subsection, ctx)
    myItems.add(nnkIdentDefs.newTree(
                   safeIdent(name),
                   nnkBracketExpr.newTree(
                     newIdentNode("TableRef"),
                     newIdentNode("string"),
                     safeIdent(subSecname)),
      newEmptyNode()))

  # Now we can add in attributes.
  for k, v in thisSection.attrs:

    if (v.defaultVal != nil) or (v.required.isSome() and v.required.get()):
      myItems.add(nnkIdentDefs.newTree(
          safeIdent(k),
          con4mTypeToNimNodes(v.typeAsCon4mNative),
          newEmptyNode()))
    else:
      myItems.add(nnkIdentDefs.newTree(
          safeIdent(k),
          nnkBracketExpr.newTree(
            newIdentNode("Option"),
            con4mTypeToNimNodes(v.typeAsCon4mNative)),
          newEmptyNode()))

  let identnode = if ctx.genExportMarkers:
                    nnkPostfix.newTree(newIdentNode("*"), safeIdent(secName))
                  else:
                    safeIdent(secName)

  let defnode = nnkTypeDef.newTree(identnode,
                                    newEmptyNode(),
                                    nnkRefTy.newTree(
                                      nnkObjectTy.newTree(
                                        newEmptyNode(),
                                        newEmptyNode(),
                                        myItems)))

  ctx.nodeStack.add(defnode)

proc buildTypeDecls(ctx: MacroState): NimNode =
  ## This is a small wrapper around the previous function, which
  ## invokes it on each section that the user defined.  Below, we'll
  ## walk the parsed content to generate this list (which I've wrongly
  ## called a stack, as I use it as a queue.
  ctx.currentSection = ctx.contents
  buildDeclsOneSection(ctx)
  var defList = newNimNode(nnkTypeSection)

  for item in ctx.nodeStack:
    defList.add(item)

  return defList

proc con4mTypeToNodes(t: Con4mType): NimNode =
  case t.kind
  of TypeInt:
    return quote do: intType
  of TypeFloat:
    return quote do: floatType
  of TypeBool:
    return quote do: boolType
  of TypeString:
    return quote do: stringType
  of TypeBottom:
    return quote do: bottomType
  of TypeList:
    let itemTypeNode = con4mTypeToNodes(t.itemType)
    return quote do:
      Con4mType(kind: TypeList, itemType: `itemTypeNode`)
  of TypeDict:
    let
      ktNode = con4mTypeToNodes(t.keyType)
      vtNode = con4mTypeToNodes(t.valType)
    return quote do:
      Con4mType(kind: TypeDict, keyType: `ktNode`, valType: `vtNode`)
  of TypeTuple:
    var
      itemTypesNodes = nnkBracket.newTree()
      seqNodes = nnkPrefix.newTree(newIdentNode("@"), itemTypesNodes)

    for item in t.itemTypes:
      itemTypesNodes.add(con4mTypeToNodes(item))
    return quote do:
      Con4mType(kind: TypeTuple, itemTypes: `seqNodes`)
  of TypeTvar:
    if t.link.isSome():
      return con4mTypeToNodes(t.link.get())
    else:
      let varnum = newLit(t.varNum)
      return quote do:
        Con4mType(kind: TypeTVar,
                  varNum: `varnum`,
                  link: none(Con4mType),
                  linksin: @[],
                  cycle: false,
                  constraints: {})
  else:
    error("Cannot box dictionary values of that type")

proc handleBoxing(t: Con4mType, v: NimNode): NimNode =
  ## When the config file parser loads up fields, it does not have any
  ## notion of what the "right" type is for each attribute. In some
  ## uses of a config file, people might not care.  Therefore, the
  ## parser fills its symbol table with values of varying types, which
  ## need to be wrapped (or boxed).
  ##
  ## This function is used to box the default attribute values the
  ## user provided in the specification, so that the runtime parser
  ## can properly insert them if they're omitted from the config file.
  ##
  ## TODO, we could certainly handle this based on the identified Nim type
  ## instead of making them provide a con4m string
  case t.kind
  of TypeBottom:
    error("Cannot box bottom value")
  of TypeProc:
    error("Con4m does not currently support function pointers.")
  of TypeTVar, TypeTuple: # Already boxed.
    return v
  of TypeBool, TypeInt, TypeFloat, TypeString, TypeList, TypeDict:
    let nodes = con4mTypeToNimNodes(t)
    return quote do:
      pack[`nodes`](`v`)

proc transformValToOptBox(attr: AttrContents): NimNode =
  ## This is a helper that generates the tree nodes needed to wrap a
  ## value in an Option[T].  We use this for default values that
  ## the user may or may not supply; the Option tells us whether it
  ## was supplied.
  if attr.defaultVal == nil:
    return quote do: none(Box)
  else:
    var nodes = handleBoxing(attr.typeAsCon4mNative, attr.defaultVal)
    return quote do: some(`nodes`)

proc optBoolToIdent(b: Option[bool]): NimNode =
  ## In our macro parsing state, we have some fields that can be true,
  ## false or unspecified (assumed false).  These are basically
  ## parameters that were OPTIONAL for the user, when invoking our
  ## macros.  This code turns that value into a true or false node.
  if b.isNone() or not b.get():
    return newIdentNode("false")
  else:
    return newIdentNode("true")

proc optStrToLit(s: Option[string]): NimNode =
  ## Same kind of thing, but with string values (e.g., doc strings)
  if s.isNone():
    return newLit("")
  else:
    return newLit(s.get())

proc buildSectionSpec(ctx: MacroState, slist: var NimNode) =
  ## Here, wer'e generating the code to build the "specification" for
  ## a section.  The generated code creates objects at runtime that
  ## represent the schema, which the parser will compare with what it
  ## read in.  This provides the info the parser needs to:
  ##
  ## 1) Fully check attribute types.
  ## 2) Fill in any defaults for items that the user didn't provide.
  ## 3) Bail when required items are missing from a config file, where
  ##    no defaults were provided.

  let
    curSec = ctx.currentSection
    secPath = split(curSec.fullpath, ".").join("Dot")
    secSpecVarName = newIdentNode("confSec" & ctx.name & secPath)
    specName = ctx.parentSpecName
    secLit = newLit(ctx.curSecName)
    doc = optStrToLit(curSec.doc)

    allowed = if curSec.allowed.isSome():
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
  slist.add(n)

  for attrName, attrContents in curSec.attrs:
    let
      attrLit = newLit(attrName)
      typeLit = newLit(attrContents.typeAsCon4mString)
      required = optBoolToIdent(attrContents.required)
      loWrite = optBoolToIdent(attrContents.lockOnWrite)
      doc = optStrToLit(attrContents.doc)

    # TODO: add in generating the validator here.
    let
      boxVal = transformValToOptBox(attrContents)
      x = quote do:
        `secSpecVarName`.addAttr(`attrLit`,
                                 `typeLit`,
                                 default = `boxVal`,
                                required = `required`,
                                lockOnWrite = `loWrite`,
                                   doc = `doc`)
    slist.add(x)

  let subsections = curSec.subsections

  for name, secinf in subsections:
    ctx.curSecName = name
    ctx.currentSection = secinf
    ctx.parentSpecName = specName
    buildSectionSpec(ctx, slist)

proc buildGlobalSectionSpec(ctx: MacroState, slist: var NimNode) =
  ## This is pretty similar to the function above it, but we currently
  ## have to generate slighly different code for the top-level
  ## section.  I probably should remove the special-casing of the
  ## top-level, but I oddly thought it'd lead to a more unintuitive UI
  ## if I didn't get to all the macro stuff.
  for attrName, attrContents in ctx.contents.attrs:
    let
      attrLit = newLit(attrName)
      typeLit = newLit(attrContents.typeAsCon4mString)
      required = optBoolToIdent(attrContents.required)
      loWrite = optBoolToIdent(attrContents.lockOnWrite)
      doc = optStrToLit(attrContents.doc)
      specId = ctx.specIdent

    # TODO: add in generating the validator here.
    let
      boxVal = transformValToOptBox(attrContents)
      x = quote do:
        `specId`.addGlobalAttr(`attrLit`,
                               `typeLit`,
                               default = `boxVal`,
                               required = `required`,
                               lockOnWrite = `loWrite`,
                               doc = `doc`)
    slist.add(x)

  for name, secinf in ctx.contents.subsections:
    ctx.curSecName = name
    ctx.currentSection = secinf

    buildSectionSpec(ctx, slist)

proc loadOneAttr(ctx: MacroState,
                 slist: var NimNode,
                 attrName: string,
                 attrInfo: AttrContents,
                 stVariable: string,
                 dstVariable: string) =
  ## This function generates statements (adding to slist) necessary to
  ## set a single field in a single data object.  The variable name on
  ## the LHS of the assignment is in dstVariable, and the name of the
  ## variable for the symbol table is stVariable.  Note we assume here
  ## that the schema was checked, and that every variable we look up
  ## will therefore be present in the symbol table (the checker
  ## does make sure of it).
  ##
  ## Note that we have to generate different code depending on the
  ## type of the attribute, since the unboxing code will be
  ## type-dependent.

  var
    node: NimNode
    stVariableNode = ctx.currentStIdent
    attrLitNode = newLit(attrName)
    attrIdNode = safeIdent(attrName)
    dstVarNode = safeIdent(dstVariable)
    typeNodes = con4mTypeToNimNodes(attrInfo.typeAsCon4mNative)
    entryOpt = ctx.entryOptIdent
    stEntry = ctx.stEntryIdent

  if (attrInfo.defaultVal != nil) or (attrInfo.required.isSome() and
      attrinfo.required.get()):

    slist = quote do:
      `slist`
      `entryOpt` = `stVariableNode`.lookupAttr(`attrLitNode`)
      if `entryOpt`.isSome():
        `stEntry` = `entryOpt`.get()
        if `stEntry`.override.isSome():
          `dstVarNode`.`attrIdNode` =
            unpack[`typeNodes`](`stEntry`.override.get())
        else:
          `dstVarNode`.`attrIdNode` =
            unpack[`typeNodes`](`stEntry`.value.get())
      else:
        unreachable
  else:
    slist = quote do:
      `slist`
      `entryOpt` = `stVariableNode`.lookupAttr(`attrLitNode`)
      if `entryOpt`.isSome():
        `stEntry` = `entryOpt`.get()
        if `stEntry`.override.isSome():
          `dstVarNode`.`attrIdNode` =
            some(unpack[`typeNodes`](`stEntry`.override.get()))
        elif `stEntry`.value.isSome():
          `dstVarNode`.`attrIdNode` =
            some(unpack[`typeNodes`](`stEntry`.value.get()))
      else:
        `dstVarNode`.`attrIdNode` = none(`typeNodes`)

proc flattenSections(sec: SectContents,
                     secMap: OrderedTableRef[string, SectContents]) =
  for name, item in sec.subsections:
    if name in secMap:
      error("Recursive section not currently allowed, " &
            "leading to a name conflict")
    secMap[name] = item

proc buildSectionLoader(ctx: MacroState,
                        sectionsId: NimNode,
                        slist: var NimNode) =
  ## This is going to generate the code to set up the variable
  ## navigation needed, and then call loadOneAttr.

  # Note that right now, con4m ONLY supports top-level section types
  # (though they can have sub-instances, which will show up in dot
  # syntax).  Still, the macro records section definitions as if
  # nesting is possible.  here, we need to flatten this out.  We don't
  # capture the top-level "section", just explicitly declared ones.

  var secMap: OrderedTableRef[string, SectContents] = newOrderedTable[string,
      SectContents]()
  flattenSections(ctx.contents, secMap)

  for secname, secval in ctx.contents.subsections:
    var
      sectNameLit = safeIdent(secname)
      sectTypeName = safeIdent(getSectionTypeName(secval, ctx))

    let n = quote do:
      result.`sectNameLit` = newTable[string, `sectTypeName`]()
    slist.add(n)

  # Load root-scope attributes.
  let section = ctx.currentSection
  for attrName, attrInfo in section.attrs:
    loadOneAttr(ctx, slist, attrName, attrInfo, "st", "result")

  for sectName, secInfo in secMap:
    let
      sectTypeName = safeIdent(getSectionTypeName(secInfo, ctx))
      sectNameIdentNode = safeIdent(sectName)
      sectNameLitNode = newLit(sectName)
    var
      attrs = newNimNode(nnkStmtList)

    # Build the code to load this section's attributes into the variable
    # named record.  We do this before the code leading up to it, so that
    # we can use quote do: blocks for everything.
    #
    # The resulting tree lives in `attrs`.
    for attrName, attrInfo in secInfo.attrs:
      ctx.loadOneAttr(attrs, attrName, attrInfo, "sectSt", "record")

    # Here's the code we'll generate to go through each section, looking for
    # ones where we should be creating an object, copying the fields, and
    # storing it.
    #
    # Had issues with how quote: do handles deciding what to symgen so
    # didn't use it.
    # This is what the quote do should have contianed:
    # let nodes = quote do:
    #  for dottedName, sectSt in sections:  #'key' : [(fullname, Section)]
    #    for scope in sectSt:
    #      currentSt = scope
    #      let
    #        parts      = dottedName.split(".")
    #      if parts[0] != `sectNameLitNode`: continue
    #      var record = `sectTypeName`()
    #      result.`sectNameIdentNode`[dottedName] = record
    #
    # slist.add(nodes)

    var forBody = nnkStmtList.newTree(
                      nnkIfStmt.newTree(
                        nnkElifBranch.newTree(
                          nnkInfix.newTree(
                            newIdentNode("!="),
                            newIdentNode("toplevelname"),
                            sectNameLitNode
      ),
                          nnkStmtList.newTree(
                            nnkContinueStmt.newTree(newEmptyNode())))),
                      nnkAsgn.newTree(
                        newIdentNode("currentSt"),
                        newIdentNode("sectSt")),
                    nnkVarSection.newTree(
                      nnkIdentDefs.newTree(
                        newIdentNode("record"),
                        newEmptyNode(),
                        nnkCall.newTree(
                          sectTypeName # substitution point
      ))))


    for kid in attrs:
      forBody.add(kid)

    forBody.add(nnkAsgn.newTree(nnkBracketExpr.newTree(
    nnkDotExpr.newTree(
                 newIdentNode("result"),
                 sectNameIdentNode # substitution point
      ),
      newIdentNode("dottedName")),
      newIdentNode("record")))

    slist.add(nnkForStmt.newTree(
                nnkVarTuple.newTree(
                  newIdentNode("topLevelName"),
                  newIdentNode("dottedName"),
                  newIdentNode("sectSt"),
                  newEmptyNode()),
      newIdentNode("sections"),
      forBody))

## More helper functions for creating symbol names that we'll use.
proc getSpecVarName(ctx: MacroState): string =
  return "confSpec" & ctx.name

proc getConfigTypeName(ctx: MacroState): string =
  return capitalizeAscii(ctx.name) & "Config"

proc getLoadingProcName(ctx: MacroState): string =
  return "load" & capitalizeAscii(ctx.name) & "Config"

proc buildLoadingProc(ctx: MacroState, slist: var NimNode) =
  ## This is the wrapper for the loading.
  ## Note that, still not done here is generating the code to:
  ##
  ## 1. read in the config file.
  ##
  ## 2. Overlay config files, so you can have a system default, and
  ##    user overrides. This is how I'm going to make SAMI's config
  ##    file work.  The first config file read will be stored in
  ##    memory, making it easier for me to handle things like schema
  ##    changes.
  let
    loadFuncName = newIdentNode(getLoadingProcName(ctx))
    confTypeName = newIdentNode(getConfigTypeName(ctx))
    ctxName = newIdentNode("ctx")

  # Most of the work is going to be adding statements into the proc's
  # statement block. So delcare that first, and have everything append
  # to it.  Then, at the end of the proc, we'll shove this block into
  # the right place in a procedure declaration.
  #
  # Note that we'll use some helper variables in the generated code:
  # 1) `currentSt`, which represents the current scope we're loading.
  # 2) `sections`, is a table of all the sections available at runtime.
  # 3) `stEntry`, the current symbol table entry
  # 4) `entryOpt`, An option object holding the current stEntry, if any.
  # I need to convert these to gensyms.

  var
    typename = safeIdent(getConfigTypeName(ctx))
    sectionsId = newIdentNode("sections")
    currentStId = newIdentNode("currentSt")
    entryOptId = newIdentNode("entryOpt")
    stEntryId = newIdentNode("stEntry")
    stmts = quote do:
      result = `typename`()
      var
        `sectionsId` = `ctxName`.getAllSectionSTs()
        `currentStId` = `ctxName`.st
        `entryOptId`: Option[STEntry]
        `stEntryId`: StEntry

  ctx.currentStIdent = currentStId
  ctx.entryOptIdent = entryOptId
  ctx.stEntryIdent = stEntryId

  ctx.currentSection = ctx.contents
  buildSectionLoader(ctx, sectionsId, stmts)
  stmts.add:
    quote do: return result

  slist.add quote do:
    proc `loadFuncName`(`ctxName`: ConfigState): `confTypeName` =
      `stmts`

proc buildConfigSpec(ctx: MacroState, slist: var NimNode) =
  ## This is the function that is responsible for creating the actual
  ## spec.  It basically will call all of the logically different
  ## code generation pieces, and lump it together into one tree.
  let
    specVarName = getSpecVarName(ctx)
    specVarNameNode = safeIdent(specVarName)


  ctx.specIdent = specVarNameNode
  ctx.nodeStack = @[]
  ctx.parentSpecName = specVarNameNode

  slist.add quote do:
    var `specVarNameNode` = newConfigSpec()

  buildGlobalSectionSpec(ctx, slist)
  buildLoadingProc(ctx, slist)

  #echo treerepr(slist)
  #echo toStrLit(slist)

macro cDefActual(kludge: int, nameNode: untyped, rest: untyped): untyped =
  ## While this is technically our top-level macro, it's intended that
  ## you instead call configDef() or con4m(), not this.  That's
  ## because we need a kludge to be able to give a good error message
  ## if the user tries to instantiate our macro from within a proc
  ## (which would be bad, because the procs we generate wouldn't be
  ## visible outside that procedure).
  ##
  ## The indirection with this macro and the kludge parameter is the only
  ## way I've found to detect if we're in a module's scope.
  ## We do not want to generate procedures that are nested inside a proc,
  ## thus the kludge.
  ##
  ## Didn't find this one via Google; was thanks to elegantbeef on the
  ## Nim discord server (Jason Beetham, beefers331@gmail.com)
  var
    owner = kludge.owner
    markers = true

  if owner.symKind != nskModule:
    markers = false
    for i in 1 .. 5:
      echo "**************"
    echo "warning: Using con4m macro inside a function is NOT recommended."
    for i in 1 .. 5:
      echo "**************"

  var state = parseConfigDef(nameNode, rest, markers)

  result = newNimNode(nnkStmtList, lineInfoFrom = nameNode)

  let typeDecls = buildTypeDecls(state)

  result.add(typeDecls)

  buildConfigSpec(state, result)

template configDef*(nameNode: untyped, rest: untyped) =
  ## This template calls our macro does all the definitions for us,
  ## but does not handle the parsing, evaluation, or checking.  It
  ## just generates code for us. The kludge variable is to ensure we
  ## don't run in a function scope, so that we can inject the
  ## loadNNNConfig() procedure.
  ##
  ## This is particularly useful when we want to add our own custom
  ## built-in functions.  We may change those to be part of the macro
  ## system.
  var kludge = 100
  cDefActual(kludge, nameNode, rest)

template con4m*(nameBase: untyped, confstr: string, rest: untyped): untyped =
  ## This is our main wrapper that bundles up parsing, checking, evaling, ...
  ## It both returns your config object, but also injects the context object
  ## so you can load a second file on top of it.
  ##
  ## `nameBase` is the base name of your configuration file. Con4m
  ##  will use that to generate some of the symbols it exports.
  ##
  ## Currently, this macro will export a few different symbols,
  ## specifically:
  ##
  ## 1. If the first parameter is `example` It will inject a symbol
  ##    called `ctxExampleConf`, which will be an object representing
  ##    your configuration, from which you can access the fields
  ##    directly once the configuration file is loaded, using the
  ##    names in the specification you provide in this macro (see
  ##    below).
  ##
  ## 2. A type declaration for the previous symbol.  Again, if the first
  ##    parameter is `example`, then the top level type will be called
  ##    `ExampleConfig`.  So your context will have been declared:
  ##    `ctxExampleConf: ExampleConfig`.
  ##
  ## 3. Type declarations for each unique section you
  ##    defined. Currently, if a section name is, for example `host`,
  ##    it will take the form of: `HostSection`.
  ##
  ## 4. There are some helper variables that need to be hidden,
  ##    because they don't need to be exposed, but I haven't done that
  ##    yet: `tmpBox`, `currentSt`, `sections`, `stEntry`, `entryOpt`,
  ##    `confSpecExample` (where Example is your name).  Again, all of
  ##    these should go away.
  ##
  ## 5. A function called `loadExampleConf(ctx: ExampleConfig)`.
  ##    `con4m()` actually calls this function for you, but you can
  ##    re-call it if you need to.  There is a lower-level version of
  ##    the macro that this builds upon, called `configDef`, that does
  ##    NOT call it for you, if you need to customize more.
  ##
  ## The `confstr` argument is the configuration file you want to
  ## parse.
  ##
  ##  While `con4m()` does NOT read in the config file for you, it
  ## does do everything else... parse it, check it for consistency,
  ## evaluate it, check it against your specification.  The
  ## `configDef()` just generates the supporting code to build the
  ## spec.
  ##
  ## Because we generate a proc for you, `con4m()` cannot be invoked
  ## from within a function, only at the module level (a nested
  ## procedure wouldn't be intuitive).
  ##
  ## The last argument to `con4m()` is a block of commands to define
  ## sections and attributes.  No other code may appear in this block;
  ## it will result in an error.
  ##
  ## Currently, there are only two commands:
  ##
  ## `section`, which creates a section definition; and
  ##
  ## `attr`, which defines an attribute, placing it either in the
  ##         section (When the attribute appears in a section block),
  ##         or as an attribute in the global namespace, when not.
  ##
  ## ## The `section` command
  ## This takes only two required arguments:
  ## * The first argument is always the name of a section, as an
  ##   identifier.  Eg, `host`
  ## * The last argument is a block, which can constitute `attr`
  ##   commands, or other section commands. Eventually, nested
  ##   subsections will define sections that only apply inside the
  ##   specified section.  However, they currently just create a new
  ##   top-level section.
  ##
  ## Optional keyword commands to `section`
  ##
  ## 1. `allowCustomAttrs`, a boolean, indicating whether the user should
  ##    be allowed to make up their own attributes in this section. This
  ##    defaults to `false`.
  ##
  ## 2. `doc`, a string, which is documentation for that part of the
  ##    configuration file. While this is stored if provided, it
  ##    currently is unused.
  ##
  ## 3. `requiredSubSections`, a `seq[string]` of subsection names
  ##    that a user MUST provide. Provide each option in dot notation,
  ##    and a name can be replaced with a `"*"` to match anything for
  ##    that dot.  If not provided, there are no required sections.
  ##
  ## 4. `allowedSubSections`, also a `seq[string]`, which is very
  ##    similar, and again, is not checked if not provided.  Note that
  ##    `requiredSubsections` are implicitly allowed, so you don't
  ##    need to double spec them.  If not provided, this defaults to
  ##    "*", allowing any section name, but no nesting.  Not providing
  ##    either would disallow any subsection, only allowing a single
  ##    instance of the toplevel section.
  ##
  ## Subsections are probably misnomers here, where we will probably
  ## change the terminology at some point.  This are more section
  ## modifiers, the set of which needs to be unique.  Subsections will
  ## become nested sections of different types, once we implement
  ## those.
  ##
  ## Currently, the macros do NOT provide an interface to any
  ## user-defined attributes.  You have to search through the raw data
  ## manually. Making such items available more directly is a TODO
  ## item.
  ##
  ## ## The `attr` command
  ##
  ## This command takes the following arguments:
  ##
  ## 1. The name of the attribute, as an identifier.  Note that it's
  ##    okay to use Nim keywords here, they get automatically quoted
  ##    (this also happens when they are used in sections).  This is
  ##    required.
  ##
  ## 2. The Con4m type to enforce for the attribute (NOT the Nim
  ##    type). This is required.
  ##
  ## 3. `defaultVal`, a keyword parameter specifying what value to set
  ##    if the user does not provide something for this attribute,
  ##    when defining a section.
  ##
  ## 4. `required`, a keyword parameter indicating whether con4m
  ##    should throw an error when this is not present in a section.
  ##    If you set `defaultVal`, this will be ignored.  This defaults
  ##    to `true` if not provided.
  ##
  ## 5. `lockOnWrite`, a keyword parameter indicating that, once a
  ##    configuration has set this value, it may not be written in
  ##    future config file loads that reuse the same underlying
  ##    configuration state.  This allows you to provide internal
  ##    "configuration" parameters within your program (by reading
  ##    your config from a string in memory, for instance), without
  ##    risking them getting clobbered.  This defaults to false.
  ##
  ## 6. `doc`, a keyword parameter, represents a doc string. But, as
  ##    above, this is not yet used.
  ##
  ## 7. `validator`, a custom callback for this field so that you can
  ##    easily do additional validation beyond what con4m already does.
  ##    This is NOT yet implemented.
  ##
  ##
  ## Note that fields that are *not* required and do not have a
  ## default provided will be declared as `Optional[]` types so you
  ## can determine whether the user provided them safely.
  ##
  ## In future releases, we expect to handle nested sections, and also
  ## to make it easy to configure adding custom builtin functions with
  ## a separate command.
  ##
  ## Note that Nim allows you to use `attribute_name` and `attributeName`
  ## interchangably.  However, con4m does NOT, it will only accept the
  ## name you provide.
  ##
  # runnableExamples:
  #   let s = """
  #       use_ipsec: false
  #       host localhost {
  #         ip: "127.0.0.1"
  #         port: 8080
  #       }
  #       host workstation {
  #         ip: "10.12.1.10"
  #         port: 8080
  #       }
  #   """
  #   var config = con4m(test, s):
  #     attr(max_hosts, int, required = false) # required = true is the default.
  #     attr(use_ipsec, bool, true) # defaultVal = works too, but positional params work
  #     # Section should take a single argument, no sub-dots.
  #     section(host, allowedSubSections = @["*"]):
  #       attr(ip, string, required = true)
  #       attr(port, string, required = true)
  #
  #   assert config.max_hosts.isNone()
  #   assert config.use_ipsec
  #   assert config.host[1].ip == "10.12.1.10"
  var
    tree = parse(newStringStream(confstr))
    opt = tree.evalTree()

  if not opt.isSome():
    echo "Error: invalid configuration file."
    quit()

  var `ctx nameBase Conf` {.inject.} = opt.get()

  configDef(nameBase, rest)
  `ctx nameBase Conf`.addSpec(`confSpec nameBase`)
  if not `ctx nameBase Conf`.validateConfig():
    for err in `ctx nameBase Conf`.errors:
      echo "Error: ", err
    quit()
  `ctx nameBase Conf`.loadSamiConfig()
