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
    contents: SectContents
    currentSection: SectContents
    curSecName: string
    nodeStack: seq[NimNode]
    specIdent: NimNode

proc newSectContents(path: string, parent: SectContents = nil): SectContents =
  result = SectContents(fullPath: path)
  result.attrs = newOrderedTable[string, AttrContents]()
  result.subSections = newOrderedTable[string, SectContents]()
  result.parent = parent

macro dumpCodeAtRuntime*(x: untyped) : untyped =
  ## This is a helper function; allows me to make sure the code parses, then
  ## when I run the program, show me the code that I am currently generating,
  ## without running the code. Basically, if my macro would return a node N,
  ## I instead return dumpCodeAtRuntime(N).
    return nnkStmtList.newTree(
      nnkCall.newTree(
        newIdentNode("echo"),
        toStrLit(x)
      )
    )
    
macro dumpAtRuntime*(rest: untyped): untyped =
  ## Same as above, but dumps the parse tree.
  var s = treeRepr(rest)

  result = newNimNode(nnkStmtList, lineInfoFrom = rest)
  result.add(nnkCommand.newTree(newIdentNode("echo"), newLit(s)))

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
## Arg 3: defaultVal (optional, named: "default = ")
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
  else:
    error("configDef block can only contain 'attr' and 'section' commands",
          stmt)

proc parseConfigDef(nameNode: NimNode, rest: NimNode): MacroState =
## This is the entry point for parsing; it sets up the state, and kicks
## off lookForConfigCommands on each statement in the macro.  
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
## Now we're entering into the code that supports generating the
## replacement code.  This is where things can probably start to get
## confusing.
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
  ## This code generates the data type declaration for a section.
  ## The name for the data type is retrieved from getSectionVarName.
  ##
  ## The actual node layout for such things I get from writing small
  ## examples of what I WANT to generate, then I get Nim to dump the AST,
  ## making it easy for me to make sure I generate the same AST.
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
  ## This is a helper that generates the tree nodes needed to wrap a
  ## value in an Option[T].  We use this for default values that
  ## the user may or may not supply; the Option tells us whether it
  ## was supplied.
  if attr.defaultVal == nil:
    return nnkCall.newTree(newIdentNode("none"), newIdentNode("Box"))

  return nnkCall.newTree(newIdentNode("some"),
                         handleBoxing(attr.typeAsCon4mNative, attr.defaultVal)
                         )


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

proc buildSectionSpec(ctx: MacroState, slist: NimNode) =
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
  slist.add(n)
  
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
                                   
    slist.add(x)

  let subsections = curSec.subsections
  
  for name, secinf in subsections:
    ctx.curSecName = name
    ctx.currentSection = secinf
    ctx.parentSpecName = specName
    buildSectionSpec(ctx, slist)

proc buildGlobalSectionSpec(ctx: MacroState, slist: NimNode) =
  ## This is pretty similar to the function above it, but we currently
  ## have to generate slighly different code for the top-level
  ## section.  I probably should remove the special-casing of the
  ## top-level, but I oddly thought it'd lead to a more unintuitive UI
  ## if I didn't get to all the macro stuff.
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
    slist.add(x)

  for name, secinf in ctx.contents.subsections:
    ctx.curSecName = name
    ctx.currentSection = secinf

    buildSectionSpec(ctx, slist)

proc loadOneAttr(ctx: MacroState, slist: NimNode, varName: String, attrInfo: AttrContents) =
  ## TODO-- I am here.  This is going to be code that copies the
  ## individual data fields that the parser is holding, boxed, in
  ## symbol tables, into the data structures we generated.
  discard
  
proc buildSectionLoader(ctx: MacroState, slist: NimNode) =
  ## This is going to generate the code to set up the variable navigation needed,
  ## and then call loadOneAttr.
  let section = ctx.currentSection

  for varName, attrInfo in section.attrs:
    loadOneAttr(ctx, slist, varName, attrInfo)

  for secName, contents in section.subsections:
    # Save off previous st to st_n
    # set the value of st to subsection's symbol table
    # set the value of the destination section to some variable.
    #   Note: we probably want to add the type name of the destination section to
    #   the static section information.
    # call buildSectionLoader for
    

proc buildLoadingProc(ctx: MacroState, slist: NimNode) =
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
    loadFuncName = getLoadingProcName(ctx)
    confTypeName = getConfigTypeName(ctx)
  
  var stmts = nnkStmtList.newTree(
                nnkAsgn.newTree(
                  newIdentNode("result"),
                  nnkCall.newTree(
                    newIdentNode("SamiConf")
                  )
                ),
                nnkVarSection.newTree(
                  nnkIdentDefs.newTree(
                    newIdentNode("tmpBox"),
                    newIdentNode("Box"),
                    newEmptyNode()
                  ),
                 nnkIdentDefs.newTree(
                   newIdentNode("currentSt"),
                   newEmptyNode(),
                   nnkDotExpr.newTree(
                     newIdentNode("ctx"),
                     newIdentNode("st")
                     )
                   )
                )
              )
  ctx.currentSection = ctx.contents
  buildSectionLoader(ctx, stmts)
  slist.add(nnkProcDef.newTree(
              nnkIdentNode(loadFuncName),
              newEmptyNode(),
              newEmptyNode(),
              nnkFormalParams.newTree(
                newIdentNode(confTypeName),  # Return type
                nnkIdentDefs.newTree(
                  newIdentNode("ctx"),
                  newIdentNode("ConfigState"),
                  newEmptyNode()
                  )
              ),
              newEmptyNode(),
              newEmptyNode(),
              stmts
            )
          )
              
## More helper functions for creating symbol names that we'll use.    
proc getSpecVarName(ctx: MacroState): string =
  return "confSpec" & ctx.name

proc getConfigTypeName(ctx: MacroState): string =
  return capitalizeAscii(ctx.name) & "Config"

proc getLoadingProcName(ctx: MacroState): string =
  return "load" & capitalizeAscii(ctx.name) & "Config"
  
proc buildConfigSpec(ctx: MacroState, slist: NimNode) =
  ## This is the function that is responsible for creating the actual
  ## spec.  It basically will call all of the logically different
  ## code generation pieces, and lump it together into one tree.
  let specVarName = getSpecVarName(ctx)
  
  ctx.specIdent      = newIdentNode(specVarName)
  ctx.nodeStack      = @[]
  ctx.parentSpecName = newIdentNode(specVarName)

  slist.add(
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

                    
  
  buildGlobalSectionSpec(ctx, slist)

  buildLoadingProc(ctx, slist)

  #echo treerepr(result)
  #echo toStrLit(result)


macro cDefActual*(kludge: int, nameNode: untyped, rest: untyped): untyped =
## While this is technically our top-level macro, it's intended that
## you instead call configDef(), not this.  That's because we need a
## kludge to be able to give a good error message if the user tries to
## instantiate our macro from within a proc (which would be bad,
## because the procs we generate wouldn't be visible outside that
## procedure).
##  
## The indirection with this macro and the kludge parameter is the only
## way I've found to detect if we're in a module's scope.
## We do not want to generate procedures that are nested inside a proc,
## thus the kludge.
##
## Didn't find this one via Google; was thanks to elegantbeef on the
## Nim discord server (Jason Beetham, beefers331@gmail.com)
  var
    state = parseConfigDef(nameNode, rest)
    owner = kludge.owner

  if owner.symKind != nskModule:
    error("Only use this macro in a module scope")
  
  result = newNimNode(nnkStmtList, lineInfoFrom = nameNode)
  
  let typeDecls = buildTypeDecls(state)
 
  result.add(typeDecls)
 
  buildConfigSpec(state, result)


template configDef*(nameNode: untyped, rest: untyped): untyped =
  var kludge = 100

  when false:
    dumpAtRuntime(cDefActual(kludge, nameNode, rest))
  else:
    cDefActual(kludge, nameNode, rest)


# This is just sample code I'm testing with.
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


#[ Below is the code in my current phase of work... I'm trying to generate the below,
   and get it all working.

dumpAstGen:
dumpTree:
      proc loadSamiConfig(ctx: ConfigState): SamiConf = 
        result =  SamiConf()

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

