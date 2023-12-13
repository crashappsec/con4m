## The IR is essentially the graph representation on which we do all checking.
## Ideally, we will keep refining the nodes until we can use them essentially
## as fat VM instructions that we can directly marshal.
#
# TODO:
# Expressions.
#
# Type propogation.
#
# After pass 1, check to see if declared variables are used, and
# warn if not.


import parse, scope, cbox
export parse, scope, cbox

proc getTypeIdFromSyntax*(ctx: var CompileCtx, st: SyntaxType,
                          litMod = ""): TypeId =

  ## If called w/ an empty litmod, returns the primary type
  ## associated with the syntax.
  ##
  ## Doesn't work with 'other' literals, obviously.
  ##
  ## Assumes the parse node is correct.

  if litmod == "":
    case st
    of STBase10:
      return TInt
    of STHex:
      return TInt
    of STFloat:
      return TFloat
    of STBoolLit:
      return TBool
    of STStrQuotes:
      return TString
    of StChrQuotes:
      return TChar
    else: # Don't do these. Internal errors, not user errors.
      return TBottom

  var errType: string = ""

  for v in basicTypes:
    if litMod in v.litMods:
      if v.kind != 0 and uint(st) != 0:
        return v.typeId
      else:
        case st
        of STBase10:
          errType = "integer"
        of STHex:
          errType = "hex"
        of STFloat:
          errType = "float"
        of STBoolLit:
          errType = "boolean"
        of STStrQuotes:
          errType = "string"
        of STChrQuotes:
          errType = "character"
        of STOther:
          errType = "specialized"
        else:
          unreachable # Not supposed to call this w/ complex types

  if errType == "":
    ctx.irError("BadLitMod", @[litMod])
  else:
    ctx.irError("LitModTypeErr", @[litMod, errType])

  return TBottom
proc fmt(s: string, x = "", y = "", t = TBottom): Rope =
  result = atom(s).fgColor("atomiclime")
  if x != "":
    result = result + atom(" " & x).italic()

  if y != "":
    result = result + atom(" (" & y & ")").fgcolor("fandango")

  if t != TBottom:
    result = result + atom(" ") + strong(t.toString())

template reprBasicLiteral(ctx: IrNode): string =
  if ctx.value.isNone():
    ""
  else:
    rawBuiltinRepr(ctx.tid, ctx.value.get())

proc irWalker(ctx: IrNode): (Rope, seq[IrNode]) =
  var
    descriptor: string
    moreinfo:   string

  if ctx == nil or ctx.contents == nil:
    return (em("empty"), @[])

  case ctx.contents.kind
  of IrBlock:
    return (fmt("Block"), ctx.contents.stmts)
  of IrLoop:
    if ctx.contents.label != nil:
      moreinfo = "label: " & ctx.contents.label.getText() & " "

    moreinfo &= "id: " & $ctx.contents.label.id & " "
    if ctx.contents.keyVar == "":
      descriptor = "while"
    else:
      descriptor = "for / "
      if ctx.contents.startIx != nil:
        descriptor &= " from"
      else:
        descriptor &= " in"
      if ctx.contents.valVar != "":
        moreinfo &= "key: " & ctx.contents.keyVar
        moreinfo &= "val: " & ctx.contents.valVar
      else:
        moreinfo &= "ix: " & ctx.contents.keyVar

    return (fmt("Loop", descriptor, moreinfo),
            @[ctx.contents.condition, ctx.contents.loopBody])
  of IrAttrAssign:
    if ctx.contents.lock:
      moreinfo = "+lock"
    return (fmt("AttrAssign", ctx.contents.attrlhs, moreinfo, ctx.tid),
            @[ctx.contents.attrRhs])
  of IrVarAssign:
    if ctx.contents.varlhs.len() > 1:
      moreinfo = "tuple"
    return (fmt("VarAssign", "", moreinfo, ctx.tid),
            ctx.contents.varlhs & @[ctx.contents.varrhs])
  of IrSectionScope:
    descriptor = ctx.contents.secType
    moreInfo   = ctx.contents.secName
    return (fmt("Section", descriptor, moreInfo), @[ctx.contents.secBody])
  of IrConditional:
    return (fmt("Conditional"), @[ctx.contents.predicate,
                                  ctx.contents.trueBranch,
                                  ctx.contents.falseBranch])
  of IrJump:
    if ctx.contents.exitLoop:
      moreinfo = "break"
    else:
      moreinfo = "continue"
    return (fmt("Jump", "id: " & $(ctx.contents.targetNode.parseNode.id)), @[])
  of IrRet:
    return (fmt("Return", "", "", ctx.tid), @[ctx.contents.retVal])
  of IrLit:
    if ctx.tid.isBasicType():
      moreinfo = ctx.reprBasicLiteral()
    return (fmt("Literal", "", moreinfo, ctx.tid), ctx.contents.items)
  of IrMember:
    return (fmt("Member", ctx.contents.name, "", ctx.tid), @[])
  of IrIndex:
    return (fmt("Index"), @[ctx.contents.indexStart, ctx.contents.indexEnd])
  of IrCall:
    descriptor = ctx.contents.fname
    if ctx.contents.module != "":
      descriptor = ctx.contents.module & "::" & descriptor
    if ctx.contents.binop:
      moreinfo = "converted from op"
    return (fmt("Call", descriptor, moreinfo, ctx.tid), ctx.contents.actuals)
  of IrUse:
    return (fmt("Use", ctx.contents.targetModule, ctx.contents.targetLoc), @[])
  of IrUnary:
    return (fmt("UnaryOp", ctx.contents.uOp, "", ctx.tid), @[ctx.contents.uRhs])
  of IrBinary:
    return (fmt("BinaryOp", ctx.contents.bOp, "", ctx.tid),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrStoreLoc:
    return (fmt("Store", ctx.contents.symbol.name, "", ctx.tid), @[])
  of IrLoad:
    return (fmt("Load", ctx.contents.symbol.name, "", ctx.tid), @[])

proc toRope*(ctx: IrNode): Rope =
  if ctx == nil:
    return em("Null node")
  return ctx.quickTree(irWalker)

proc getLitMod(ctx: var CompileCtx): string =
  return ctx.pt.token.litType

proc irNode(ctx: var CompileCtx, kind: IrNodeType): IrNode =
  let payload = IrContents(kind: kind)
  result = IrNode(parseNode: ctx.pt, contents: payload, parent: ctx.current)
  ctx.current = result

proc parseTreeToIr(ctx: var CompileCtx): IrNode

proc downNode(ctx: var CompileCtx, which: int): IrNode =
  var saved   = ctx.current
  ctx.pt      = ctx.pt.children[which]
  result      = ctx.parseTreeToIr()
  ctx.pt      = ctx.pt.parent
  ctx.current = saved

proc downNode(ctx: var CompileCtx, kid, grandkid: int): IrNode =
  var saved = ctx.current

  ctx.pt      = ctx.pt.children[kid].children[grandkid]
  result      = ctx.parseTreeToIr()
  ctx.pt      = ctx.pt.parent.parent
  ctx.current = saved

template independentSubtree(code: untyped) =
  var saved = ctx.current

  ctx.current = nil
  code
  ctx.current = saved

template getText(ctx: var CompileCtx): string =
  ctx.pt.getText()

template getText(ctx: var CompileCtx, which: int): string =
   ctx.pt.children[which].getText()

template getText(ctx: var CompileCtx, kidIx: int, grandIx: int): string =
  ctx.pt.children[kidIx].children[grandIx].getText()

template numKids(ctx: var CompileCtx): int =
  ctx.pt.children.len()

template kidKind(ctx: var CompileCtx, i: int): Con4mNodeKind =
  ctx.pt.children[i].kind

template kidKind(ctx: var CompileCtx, i, j: int): Con4mNodeKind =
  ctx.pt.children[i].children[j].kind

template numGrandKids(ctx: var CompileCtx, i: int): int =
  ctx.pt.children[i].children.len()

template parseKid(ctx: var CompileCtx, i: int): Con4mNode =
  ctx.pt.children[i]

template parseGrandKid(ctx: var CompileCtx, i, j: int): Con4mNode =
  ctx.pt.children[i].children[j]

proc isConstant*(n: IrNode): bool =
  return n.tid != TBottom and n.tid.isBuiltinType() and n.value.isSome()

const
  ntLoops        = [ NodeForStmt, NodeWhileStmt ]
  ntConditionals = [ NodeIfStmt, NodeElifStmt ]

proc addFalseBranch(conditional: IrNode, falseBranch: IrNode) =
  var n = conditional
  while n.contents.falseBranch != nil:
    n = n.contents.falseBranch

  n.contents.falseBranch = falseBranch

proc checkLabelDupe(ctx: var CompileCtx, label: string) =
  var
    n       = ctx.current.parent

  while n != nil:
    if n.contents.kind == IrLoop and n.contents.label.getText() == label:
      ctx.irWarn("LabelDupe", @[label], n.parseNode)
      return
    n = n.parent

proc convertEnum*(ctx: var CompileCtx) =
  var
    usedVals: seq[int]
    nextVal = 0
    value: int64

  independentSubtree:
    for i in 0 ..< ctx.numKids():
      let itemName = ctx.getText(i, 0)

      if ctx.numGrandKids(i) == 2:
        let v = ctx.downNode(i, 1)
        if not v.isConstant():
          ctx.irError("EnumDeclConst", w = ctx.parseKid(i))
          continue
        if not v.tid.isIntType():
          ctx.irError("EnumInt", w = ctx.parseKid(i))
          continue
        value = toVal[int64](v.value.get())
        if value in usedVals:
          ctx.irWarn("EnumReuse", @[$value], w = ctx.parseKid(i))
        else:
          usedVals.add(value)
          if value > nextVal:
            nextVal = value + 1
      else:
        value = nextVal
        usedVals.add(value)
        nextVal = nextVal + 1

      var
        symOpt = ctx.scopeDeclare(ctx.moduleScope, itemName, false, TInt)

      if symOpt.isSome():
        let sym = symOpt.get()
        sym.constValue = some(newCbox(value, TInt))

proc convertIdentifier(ctx: var CompileCtx): IrNode =
  let name = ctx.pt.getText()
  var stOpt: Option[SymbolInfo]

  if ctx.lhsContext:
    result = ctx.irNode(IrStoreLoc)
    stOpt  = ctx.addDef(name, result, tVar())
  else:
    result = ctx.irNode(IrLoad)
    stOpt = ctx.addUse(name, result, tVar())

  result.contents.symbol = stOpt.getOrElse(nil)
  if result.contents.symbol != nil:
    result.tid = result.contents.symbol.tid

proc convertVarAssignment(ctx: var CompileCtx): IrNode =
  var
    lhs:      seq[IrNode]
    itemTids: seq[TypeId]
    rhs:      IrNode

  result = ctx.irNode(IrVarAssign)
  ctx.lhsContext = true

  for i in 0 ..< ctx.pt.children.len() - 1:
    ctx.curSym = nil
    let item = ctx.downNode(i)

    lhs.add(item)
    itemTids.add(item.tid)

  ctx.lhsContext = false
  ctx.curSym = nil
  rhs = ctx.downNode(ctx.pt.children.len() - 1)

  if lhs.len > 1:
    result.tid = tTuple(itemTids)
  else:
    result.tid = itemTids[0]

  result.tid = ctx.typeCheck(result.tid, rhs.tid)

  result.contents.varLhs = lhs
  result.contents.varRhs = rhs

proc extractSymInfo(ctx: var CompileCtx,
                    scope: var Scope): SymbolInfo {.discardable.} =
  # Returns the last symbol; useful for convertParamBlock where
  # it only accepts one symbol.
  var
    toAdd: seq[(string, TypeId)]

  for varNode in ctx.pt.children:
    var
      varNames:  seq[string]
      foundType: TypeId = TBottom
    for kid in varNode.children:
      if kid.kind == NodeIdentifier:
        varNames.add(kid.getText())
      else:
        foundType = kid.buildType()
    for oneVarName in varNames:
      if foundType == TBottom:
        toAdd.add((oneVarName, tVar()))
      else:
        toAdd.add((oneVarName, foundType.copyType().typeId))

  for i, (name, tid) in toAdd:
    let symOpt = ctx.scopeDeclare(scope, name, false, tid)
    if i == toAdd.len() - 1 and symOpt.isSome():
      result = symOpt.get()

proc convertVarStmt(ctx: var CompileCtx) =
  if ctx.funcScope != nil:
    ctx.extractSymInfo(ctx.funcScope)
  else:
    ctx.extractSymInfo(ctx.moduleScope)

proc convertGlobalStmt(ctx: var CompileCtx) =
  ctx.extractSymInfo(ctx.globalScope)

proc convertParamBody(ctx: var CompileCtx, sym: var SymbolInfo) =
  var
    gotShort, gotLong, gotValid, gotDefault: bool
    paramInfo = ParamInfo()

  independentSubtree:
    for i in 0 ..< ctx.numKids():
      if ctx.kidKind(i) != NodeAttrAssign:
        ctx.irError("No code is allowed inside parameter blocks")
        continue
      let propname = ctx.getText(i, 0)
      case propname
      of "shortdoc":
        if gotShort:
          ctx.irError("DupeParamProp", @["shortdoc"], ctx.pt.children[i])
          continue
        if ctx.kidKind(i, 1) != NodeStringLit:
          ctx.irError("BadParamProp", @["shortdoc", "string literal"],
                      ctx.pt.children[i])
          continue
        paramInfo.shortdoc = some(ctx.getText(i, 1))
        gotShort = true
      of "doc":
        if gotLong:
          ctx.irError("DupeParamProp", @["doc"], ctx.pt.children[i])
          continue
        if ctx.kidKind(i, 1) != NodeStringLit:
          ctx.irError("BadParamProp", @["doc", "string literal"],
                      ctx.pt.children[i])
          continue
        paramInfo.doc = some(ctx.getText(i, 1))
        gotLong = true
      of "validator":
        if gotValid:
          ctx.irError("DupeParamProp", @["validator"], ctx.pt.children[i])
          continue
        if ctx.kidKind(i, 1) != NodeCallbackLit:
          ctx.irError("BadParamProp", @["validator", "callback"],
                      ctx.pt.children[i])
        let
          irNode = ctx.downNode(i, 1)
          mixed  = irNode.value.get()
          to     = irNode.tid.idToTypeRef()

        if to.items.len() != 0:
          ctx.typeCheck(sym, to.items[^1])

        paramInfo.validator = some(toVal[Callback](mixed))
        gotValid = true
      of "default":
        if gotDefault:
          ctx.irError("DupeParamProp", @["default"], ctx.pt.children[i])
          continue
        paramInfo.defaultParse = some(ctx.pt.children[i].children[1])
        gotDefault             = true
      else:
        ctx.irError("BadParamProp", @[propname], ctx.pt.children[i])
        continue

  sym.pinfo = paramInfo

proc convertParamBlock(ctx: var CompileCtx) =
  var sym: SymbolInfo

  if ctx.pt.children[0].kind == NodeMember:

    independentSubtree:
      let memberIr = ctx.downNode(0)
      sym = ctx.scopeDeclare(ctx.usedAttrs, memberIr.contents.name, false,
                             tVar()).getOrElse(nil)
  else: # will be something we can call extractSymInfo on.
    var savedPt = ctx.pt
    ctx.pt = ctx.pt.children[0]
    sym = ctx.extractSymInfo(ctx.moduleScope)
    ctx.pt = savedPt

  var savedPt = ctx.pt
  ctx.pt = ctx.pt.children[1]
  ctx.convertParamBody(sym)
  ctx.pt = savedPt

proc convertFormal(info: FuncInfo, n: Con4mNode) =
  var formalInfo = FormalInfo()

  if n.children[0].kind == NodeIdentifier:
    formalInfo.name = n.children[0].getText()
  else:
    formalInfo.va = true
    formalInfo.name = n.children[0].children[0].getText()

  if n.children.len() == 2:
    formalInfo.tid = n.children[1].buildType()
  else:
    formalInfo.tid = newTypeVar().typeid

  info.params.add(formalInfo)

proc setupTypeSignature(info: FuncInfo, n: Con4mNode) =
  if n != nil:
    info.retVal     = FormalInfo(name: "result")
    info.retval.tid = n.buildType()
  else:
    info.retVal = FormalInfo(name: "result", tid: tVar())

  var
    actTypes: seq[TypeId]
    va = false

  if info.params.len() != 0 and info.params[^1].va:
    va = true

  for actual in info.params:
    actTypes.add(actual.tid)

  actTypes.add(info.retVal.tid)

  info.tid = tFunc(actTypes, va)

  if va == true:
    # From the body of the function, the named parameter collects
    # all arguments of the given type into a list. This sets the
    # ACTUAL type that we'll add to the function's symbol table.
    info.params[^1].tid = tList(info.params[^1].tid)

proc handleFuncdefSymbols(ctx:   var CompileCtx,
                          info:  var FuncInfo) =
  var symOpt: Option[SymbolInfo]

  info.fnScope.initScope()

  for item in info.params & @[info.retVal]:
    discard ctx.scopeDeclare(info.fnScope, item.name, false, item.tid)

  symOpt = ctx.scopeDeclare(ctx.moduleScope, info.name, true, TBottom)

  if symOpt.isSome():
    symOpt.get().fimpls.add(info)

proc findExplicitDeclarations(ctx: var CompileCtx, n: Con4mNode)

proc convertFuncDefinition(ctx: var CompileCtx) =
  ## This converts the declaration portion, NOT the body. For now, the
  ## body just goes into the FuncInfo `rawImpl` parameter. Once we have
  ## found all explicitly declared symbols, we then come back to
  ## convert the tree into IR.
  var
    funcName   = ctx.getText(0)
    info       = FuncInfo()
    returnType = Con4mNode(nil)

  # Params are in the second node, and the last item there might
  # have the varargs marker, which changes what we insert into the symbol
  # table for that thing.
  for item in ctx.pt.children[1].children:
    info.convertFormal(item)

  if ctx.numKids() == 4:
    returnType = ctx.pt.children[2]

  info.setupTypeSignature(returnType)
  info.name = funcName
  ctx.handleFuncdefSymbols(info)

  info.rawImpl = ctx.pt.children[^1]

  ctx.funcScope = info.fnScope
  ctx.findExplicitDeclarations(info.rawImpl)
  ctx.funcScope = nil

proc findExplicitDeclarations(ctx: var CompileCtx, n: Con4mNode) =
  ## To make life easier for us when handling def's and uses, we will
  ## scan through either the module scope or individual function scopes
  ## in their entirety, looking just for 'var' and 'global' statements,
  ## so that when we get to def/use handling, we can know definitively
  ## what to do.
  ##
  ## If we're scanning in the module scope, we do NOT descend into
  ## FuncDef nodes; we'll call this function when creating our entry
  ## for a function.
  ##
  ## All executable bodies get properly processed only when all explicit
  ## declarations are found.
  ## This consists of:
  ##
  ## 1) Body content in the top level.
  ## 2) Body content in functions.
  ## 3) The default value in a parameter block.

  let saved = ctx.pt

  for kid in n.children:
    ctx.pt = kid
    case kid.kind
    of NodeFuncDef:
      continue
    of NodeVarStmt:
      ctx.convertVarStmt()
    of NodeGlobalStmt:
      ctx.convertGlobalStmt()
    of NodeParamBlock:
      ctx.convertParamBlock()
    else:
      ctx.findExplicitDeclarations(kid)

  ctx.pt = saved

proc findExplicitDeclarations(ctx: var CompileCtx) =
  # First, pull declarations from the toplevel scope.
  let root = ctx.pt

  ctx.findExplicitDeclarations(ctx.pt)

  for item in root.children:
    case item.kind
    of NodeEnumStmt:
      ctx.pt = item
      ctx.convertEnum() # Enums are currently 100% processed this pass
      continue
    of NodeFuncDef:
      # Function definitions are processed for declarations.
      ctx.pt = item
      ctx.convertFuncDefinition()
      continue
    else:
      ctx.findExplicitDeclarations(item)

proc statementsToIr(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrBlock)
  var skipI = false

  for i in 0 ..< ctx.numKids():
    let item = ctx.parseKid(i)

    case item.kind
    of NodeBreakStmt, NodeContinueStmt, NodeReturnStmt:
      if i != ctx.numKids() + 1:
        ctx.irWarn("DeadCode", @[$(item)], w = item)
        result.contents.stmts.add(ctx.downNode(i))
        return # Don't process that dead code.
    of NodeLabelStmt:
      if i == ctx.numKids() - 1 and ctx.parseKid(i + 1).kind in ntLoops:
          let one            = ctx.downNode(i + 1)
          one.contents.label = ctx.parseGrandKid(i, 0)
          skipI              = true
          result.contents.stmts.add(one)
          ctx.checkLabelDupe(one.contents.label.getText())
      else:
        ctx.irError("LabelLoc", w = item)
        continue
    of NodeElifStmt:
      if i != 0:
        let prev = ctx.parseKid(i - 1)
        if prev.kind in ntConditionals:
          result.contents.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("ElifLoc", w = item)
      continue
    of NodeElseStmt:
      if i != 0:
        let prev = ctx.parseKid(i = 1)
        if prev.kind in ntConditionals:
          result.contents.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("ElseLoc", w = item)
      continue
    # These items all get handled early so that we know what is
    # explicitly declared. Funcdefs and parameter blocks do need some
    # further processing, but that happens before the main body is
    # processed; this function gets called for those nodes seprately.
    of NodeEnumStmt, NodeFuncDef, NodeParamBlock, NodeVarStmt, NodeGlobalStmt:
      continue
    else:
      discard  # An expression.

    if skipI:
      skipI = false
      continue
    else:
      result.contents.stmts.add(ctx.downNode(i))

proc convertLit(ctx: var CompileCtx, st: SyntaxType): IrNode =
  var
    err: string
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)

  result.contents.syntax = st

  result.tid = ctx.getTypeIdFromSyntax(st, lmod)

  if result.tid != TBottom:
    let val = parseLiteral(cast[int](result.tid), ctx.getText(), err, st)
    if err != "":
      ctx.irError(err)
    else:
      result.value = some(val)

proc convertCharLit(ctx: var CompileCtx): IrNode =
  var
    err: string
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)
  result.tid = ctx.getTypeIdFromSyntax(StChrQuotes, lmod)

  if err != "":
      ctx.irError(err)
  else:
    let
      codepoint = ctx.pt.token.codepoint
      val       = initializeCharLiteral(cast[int](result.tid), codepoint, err)
    if err != "":
      ctx.irError(err)
    else:
      result.value = some(val)

proc parseTreeToIr(ctx: var CompileCtx): IrNode =
  case ctx.pt.kind:
    of NodeModule, NodeBody:
      result = ctx.statementsToIr()
    of NodeUnpack, NodeVarAssign:
      result = ctx.convertVarAssignment()
    of NodeIdentifier:
      result = ctx.convertIdentifier()
    of NodeExpression, NodeLiteral:
      result = ctx.downNode(0)
    of NodeStringLit:
      result = ctx.convertLit(StStrQuotes)
    of NodeIntLit:
      result = ctx.convertLit(STBase10)
    of NodeHexLit:
      result = ctx.convertLit(StHex)
    of NodeFloatLit:
      result = ctx.convertLit(STFloat)
    of NodeBoolLit:
      result = ctx.convertLit(StBoolLit)
    of NodeOtherLit:
      result = ctx.convertLit(StOther)
    of NodeCharLit:
      result = ctx.convertCharLit()
    # of NodeType:
    #   result = ctx.convertTypeLit()
    # of NodeDictLit:
    #   result = ctx.convertDictLit()
    # of NodeListLit:
    #   result = ctx.convertListLit()
    # of NodeTupleLit:
    #   result = ctx.convertTupleLit()
    # of NodeCallbackLit:
    #   result = ctx.convertCallbackLit()
    else:
      return ctx.irNode(IrBlock)


  return


  # case ctx.pt.kind
  # of NodeModule, NodeBody:
  #  result = ctx.statementsToIr()
  # of NodeBody:
  #   result = ctx.statementsToIr()
  # of NodeAttrAssign:
  #   result = ctx.convertAttrAssignment()
  # of NodeAttrSetLock:
  #   result      = ctx.downNode(0)
  #   result.lock = true
  # of NodeUnpack, NodeVarAssign:
  #   result = ctx.convertVarAssignment()
  # of NodeSection:
  #   result = ctx.convertSection()
  # of NodeIfStmt, NodeElifStmt:
  #   result = ctx.convertConditional()
  # of NodeElseStmt:
  #   result = ctx.downNode(0)
  # of NodeForStmt:
  #   result = ctx.convertForStmt()
  # of NodeWhileStmt:
  #   result = ctx.convertWhileStmt()
  # of NodeBreakStmt:
  #   result = ctx.loopExit(true)
  # of NodeContinueStmt:
  #   result = ctx.loopExit(false)
  # of NodeReturnStmt:
  #   result = ctx.convertReturn()
  # of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt,
  #    NodeMod, NodeMul, NodeDiv:
  #   result = ctx.convertBinaryOp()
  # of NodePlus, NodeMinus:
  #   if ctx.numKids == 2:
  #     result = ctx.convertBinaryOp()
  #   else:
  #     result = ctx.convertUnaryOp()
  # of NodeNot:
  #   result = ctx.convertUnaryOp()
  # of NodeMember:
  #   result = ctx.convertMember()
  # of NodeLiteral, NodeParenExpr, NodeExpression:
  #   result = ctx.downNode(0)
  # of NodeIndex:
  #   result = ctx.convertIndex()
  # of NodeCall:
  #   result = ctx.convertCall()
  # of NodeUseStmt:
  #   result = ctx.convertUseStmt()
  # of NodeIdentifier:
  #   result = ctx.downNode(0)
  # else:
  #   unreachable

#[


proc convertAttrAssignment(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrAttrAssign)
  var
    parts: seq[string] = @[]
    kid = ctx.parseKid(0)
  while true:
    parts.add(kid.getText())
    if kid.children.len() == 0:
      break
    kid = kid.children[0]
  ctx.lhsContext  = true
  ctx.attrContext = true
  result.attrlhs  = parts.join(".")
  ctx.attrContext = false
  ctx.lhsContext  = false
  result.attrrhs  = ctx.downNode(1)
  result.tid      = result.attrrhs.tid

proc convertConditional(ctx: var CompileCtx): IrNode =
  result            = ctx.irNode(IrConditional)
  result.predicate  = ctx.downNode(0)
  result.trueBranch = ctx.downNode(1)

proc convertSection(ctx: var CompileCtx): IrNode =
  result         = ctx.irNode(IrSectionScope)
  result.secType = ctx.getText(0)
  if ctx.numKids() == 3:
    result.secName = ctx.getText(1)
    result.body    = ctx.downNode(2)
  else:
    result.body    = ctx.downNode(1)

proc convertForStmt(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrLoop)
  if ctx.numKids() == 3:
    result.contents.keyVar = ctx.getText(0, 0)
    if ctx.numGrandKids(0) == 2:
      result.contents.valVar = ctx.getText(0, 1)

    result.contents.condition = ctx.downNode(1)
    result.contents.loopBody  = ctx.downNode(2)
  else:
    result.contents.keyVar   = ctx.getText(0, 0)
    result.contents.startIx  = ctx.downNode(1)
    result.contents.endIx    = ctx.downNode(2)
    result.contents.loopBody = ctx.downNode(3)

proc convertWhileStmt(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrLoop)
  result.contents.condition = ctx.downNode(0)
  result.contents.loopBody  = ctx.downNode(1)

proc loopExit(ctx: var CompileCtx, loopExit: bool): IrNode =
  result = ctx.irNode(IrJump)
  result.contents.exitLoop = loopExit
  if ctx.numKids() != 0:
    let label = ctx.getText(0)
    var n = ctx.parent
    while n != nil:
      if n.contents.kind == IrLoop:
        if n.contents.label == label:
          result.contents.targetNode = n
          return
      n = n.parent
    ctx.irError("BadLoopExit", @[ctx.getText(), label]]

  var n = ctx.parent
  while n != nil:
    if n.kind == IrLoop:
      result.contents.targetNode = n
      return
    n = n.parent

proc convertReturn(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrRet)
  if ctx.numKids() == 1:
    result.contents.retVal = ctx.downNode(0)

proc convertTypeLit(ctx: var CompileCtx): IrNode =
  var
    tvars: Dict[string, TypeId]
    tinfo: TypeId

  result                 = ctx.irNode(IrLit)
  result.tid             = some(tTypeSpec().typeId)
  tinfo                  = ctx.pt.buildType(tvars)
  result.contents.litVal = tinfo.toMixed()

proc convertListLit(ctx: var CompileCtx): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STList
  result.contents.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids:
    result.items.add(ctx.downNode(i))

proc convertDictLit(ctx: var CompileCtx): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STDict
  result.contents.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids():
    result.items.add(ctx.downNode(i, 0))
    result.items.add(ctx.downNode(i, 1))

proc convertTupleLit(ctx: var CompileCtx): IrNode =
  result          = ctx.irNode(IrLit)
  result.contents = STTuple
  result.contents = ctx.getLitMod()
  for i in 0 ..< ctx.numKids():
    result.items.add(ctx.downNode(i))

proc convertCallbackLit(ctx: var CompileCtx): IrNode =
  ## TODO:
  ##
  ##  1) Makes no sense for callback literals to not have a name,
  ##     unless we're actually in a *type* literal, where we might
  ##     want to accept the name as part of the type?? Nuke.
  ##
  ##  2) The callback needs to be run through a addFuncUse(); we need
  ##     to see if it matches anything in the module, and if it
  ##     doesn't, keep it in a list of things we need to resolve when
  ##     linking.
  var
    cb: Callback
  result = ctx.irNode(IrLit)
  if ctx.pt.children[0].kind != NodeNoCallbackName:
    cb.name = getText(0, 0)
  if ctx.numKids() == 2:
    var
      tvars: Dict[string, TypeId]
    cb.tid = ctx.pt.children[1].buildType(tvars)
    result.tid = tref(cb.tid)
  else:
    result.tid = tref(newFuncType(@[]))
  result.value = some(cb.toMixed())

proc convertMember(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrMember)
  var parts: seq[string]
  for i in 0 ..< ctx.numKids():
    parts.add(ctx.getText(i))
  result.name = parts.join(".")

proc convertIndex(ctx: var CompileCtx): IrNode =
  result = ctx.irNode(IrIndex)
  result.indexStart = ctx.downNode(0)
  if ctx.numKids() == 2:
    result.indexEnd = ctx.downNode(1)

proc convertCall(ctx: var CompileCtx): IrNode =
  result       = ctx.irNode(IrCall)
  result.fname = ctx.getText(0)

  for i in 0 ..< ctx.numKids(1):
    result.actuals.add(ctx.downNode(1, i))

proc convertUseStmt(ctx: var CompileCtx): IrNode =
  result                       = ctx.irNode(IrUse)
  result.contents.targetModule = ctx.getText(0)
  if ctx.numKids() == 2:
    result.contents.targetLoc = ctx.getText(1)

proc convertUnaryOp(ctx: var CompileCtx): IrNode =
  result               = ctx.irNode(IrUnary)
  result.contents.uOp  = ctx.getText()
  result.contents.uRhs = ctx.downNode(0)

const notFloatOps = ["shl", "shr", "and", "or", "xor", "div", "%"]

template binOpReplace(ctx: var CompileCtx, o, f: string) =
  if n.binOp == o:
    cc.fname = f
    cc.actuals = @[n.contents.bLhs, n.contents.bRhs]
    n.contents = cc

template boolOpReplace(ctx: var CompileCtx, o, f: string) =
  if n.binOp == o:
    cc.fname = f
    cc.actuals = @[n.contents.bLhs, n.contents.bRhs]
    n.contents = cc
    n.tid = TBool

proc replaceBinOpWithCall(ctx: var CompileCtx, n: IrNode) =
  var cc = IrContents(kind: IrCall, binop: true)

  ctx.binOpReplace("/", "__slash__")
  ctx.binOpReplace("*", "__star__")
  ctx.binOpReplace("+", "__plus__")
  ctx.binOpReplace("-", "__minus__")
  ctx.binOpReplace("%", "__percent__")
  ctx.binOpReplace("shl", "__shl__")
  ctx.binOpReplace("shr", "__shr__")
  ctx.binOpReplace("div", "__div__")
  ctx.binOpReplace("&", "__bitand__")
  ctx.binOpReplace("|", "__bitor__")
  ctx.binOpReplace("^", "__bitxor__")
  ctx.binOpReplace("shl", "__shl__")
  ctx.binOpReplace("shr", "__shr__")
  ctx.boolOpReplace("<",   "__lt__")
  ctx.boolOpReplace("<=",  "__lte__")
  ctx.boolOpReplace(">",   "__gt__")
  ctx.boolOpReplace(">=",  "__gte__")
  ctx.boolOpReplace("!=",  "__ne__")
  ctx.boolOpReplace("==",  "__eq__")

proc convertBinaryOp(ctx: var CompileCtx): IrNode =
  result          = ctx.irNode(IrBinary)
  result.contents.bOp   = ctx.getText()
  result.contents.bLhs  = ctx.downNode(0)
  result.contents.bRhs  = ctx.downNode(1)

  result.tid = result.bLhs.contents.tid.unify(result.bRhs.contents.tid)

  if result.tid == TBottom and result.bLhs.isNumericBuiltin() and
     result.bRhs.isNumericBuiltin():
    result.tid = resultingNumType(result.bLhs.tid, result.bRhs.tid, warn)

    if result.tid == TFloat and result.bOp in notFloatOps:
      result.tid = TBottom

  if result.tid == TBottom:
    result.irError(BinaryOperandsNotCompatable)
  else:
    case result.contents.bOp
    of "/":
      if result.tid.isNumericBuiltin():
        result.tid == TFloat
      else:
        ctx.replaceBinOpWithCall(result)
    of "+", "-", "*":
      if not result.tid.isNumericBuiltin():
        ctx.replaceBinOpWithCall(result)
    of "shl", "shr", "div", "&", "|", "^", "%":
      if result.tid == TFloat or not result.tid.isNumericBuiltin():
        ctx.replaceBinOpWithCall(result)
    of ">", "<", ">=", "<=", "!=", "==":
      result.tid = TBool
      if not result.tid.isNumericBuiltin():
        ctx.replaceBinOpWithCall(result)

proc parseTreeToIr(ctx: var CompileCtx): IrNode =
  case ctx.pt.kind
  of NodeModule:
    ctx.moduleScope.initScope()
    ctx.findExplicitDeclarations()
    for (name, sym) in ctx.globalScope.table.items():
      for impl in sym.fimpls:
        ctx.funcScope = impl.fnScope
        ctx.pt        = impl.rawImpl
        ctx.parent    = nil
        ctx.parseTreeToIr() # This should be a body node.
      if sym.pInfo != nil and sym.pinfo.defaultIr.isSome():
        ctx.funcScope = nil
        ctx.pt        = ctx.pinfo.defaultIr.get()
        ctx.parent    = nil
        ctx.parseTreeToIr() # This should be some sort of expression node.
    result = ctx.statementsToIr()
  of NodeBody:
    result = ctx.statementsToIr()
  of NodeAttrAssign:
    result = ctx.convertAttrAssignment()
  of NodeAttrSetLock:
    result      = ctx.downNode(0)
    result.lock = true
  of NodeUnpack, NodeVarAssign:
    result = ctx.convertVarAssignment()
  of NodeSection:
    result = ctx.convertSection()
  of NodeIfStmt, NodeElifStmt:
    result = ctx.convertConditional()
  of NodeElseStmt:
    result = ctx.downNode(0)
  of NodeForStmt:
    result = ctx.convertForStmt()
  of NodeWhileStmt:
    result = ctx.convertWhileStmt()
  of NodeBreakStmt:
    result = ctx.loopExit(true)
  of NodeContinueStmt:
    result = ctx.loopExit(false)
  of NodeReturnStmt:
    result = ctx.convertReturn()
  of NodeType:
    result = ctx.convertTypeLit()
  of NodeStringLit:
    result = ctx.convertLit(StStrQuotes)
  of NodeIntLit:
    result = ctx.convertLit(STBase10)
  of NodeHexLit:
    result = ctx.convertLit(StHex)
  of NodeFloatLit:
    result = ctx.convertLit(STFloat)
  of NodeBoolLit:
    result = ctx.convertLit(StBoolLit)
  of NodeOtherLit:
    result = ctx.convertLit(StOther)
  of NodeCharLit:
    result = ctx.convertCharLit()
  of NodeDictLit:
    result = ctx.convertDictLit()
  of NodeListLit:
    result = ctx.convertListLit()
  of NodeTupleLit:
    result = ctx.convertTupleLit()
  of NodeCallbackLit:
    result = ctx.convertCallbackLit()
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt,
     NodeMod, NodeMul, NodeDiv:
    result = ctx.convertBinaryOp()
  of NodePlus, NodeMinus:
    if ctx.numKids == 2:
      result = ctx.convertBinaryOp()
    else:
      result = ctx.convertUnaryOp()
  of NodeNot:
    result = ctx.convertUnaryOp()
  of NodeMember:
    result = ctx.convertMember()
  of NodeLiteral, NodeParenExpr, NodeExpression:
    result = ctx.downNode(0)
  of NodeIndex:
    result = ctx.convertIndex()
  of NodeCall:
    result = ctx.convertCall()
  of NodeUseStmt:
    result = ctx.convertUseStmt()
  of NodeIdentifier:
    result = ctx.downNode(0)
  else:
    unreachable
]#

proc toIr*(ctx: var CompileCtx): bool {.discardable.} =
  ctx.globalScope.initScope()
  ctx.moduleScope.initScope()
  ctx.pt = ctx.root
  ctx.findExplicitDeclarations()

  # Build IR; first the top-level module, then walk the module symbol table
  # to find functions and parameters (where only the setting of a default
  # value is evaluated as an expression)

  ctx.pt     = ctx.root
  ctx.irRoot = ctx.parseTreeToIr()

  for (name, sym) in ctx.moduleScope.table.items():
    for impl in sym.fimpls:
      ctx.funcScope = impl.fnScope
      ctx.pt        = impl.rawImpl
      impl.implementation = ctx.parseTreeToIr() # This should be a body node.
    if sym.pInfo != nil and sym.pinfo.defaultParse.isSome():
      ctx.funcScope = nil
      ctx.pt        = sym.pinfo.defaultParse.get()
      ctx.current   = nil
      let paramIr   = ctx.parseTreeToIr()

      sym.pinfo.defaultIr = some(paramIr)
      ctx.typeCheck(sym, paramIr.tid)

  return ctx.errors.canProceed()
