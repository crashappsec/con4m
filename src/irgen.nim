## The IR is essentially the graph representation on which we do all checking.
## Ideally, we will keep refining the nodes until we can use them essentially
## as fat VM instructions that we can directly marshal.

import parse, scope, cbox, strutils
export parse, scope, cbox

template getTid*(n: untyped): TypeId =
  n.tid.followForwards()

proc lockFn*(impl: FuncInfo) =
  impl.frozen = true
  let to      = impl.tid.idToTypeRef()
  to.isLocked = true

proc unlockFn*(impl: FuncInfo) =
  impl.frozen = false
  let to      = impl.tid.idToTypeRef()
  to.isLocked = true

template withFnLock(fi: FuncInfo, code: untyped) =
  let
    tobj = fi.tid.idToTypeRef()
    lock = tobj.isLocked

  tobj.isLocked = true

  try:
    code
  finally:
    tobj.isLocked = lock

proc beginFunctionResolution*(ctx: Module, n: IrNode, name: string,
                              callType: TypeId): FuncInfo =
  var
    symOpt   = ctx.moduleScope.table.lookup(name)
    sym:     SymbolInfo
    matches: seq[FuncInfo]
    fails:   seq[FuncInfo]

  if symOpt.isSome():
    sym = symOpt.get()
    if sym.isFunc:
      for fimpl in sym.fimpls:
        withFnLock(fimpl):
          if callType.copyType().typeId.unify(fimpl.tid) == TBottom:
            fails.add(fimpl)
          else:
            matches.add(fimpl)

  # All our functions generally will already be in the global scope,
  # so there's a bit of redundancy we could remove here.
  if matches.len() == 0:
    symOpt = ctx.globalScope.table.lookup(name)
    if symOpt.isSome():
      sym = symOpt.get()
      if sym.isFunc:
        for fimpl in sym.fimpls:
          withFnLock(fimpl):
            if callType.copyType().typeId.unify(fimpl.tid) == TBottom:
              fails.add(fimpl)
            else:
              matches.add(fimpl)

  if matches.len() == 1:
    result = matches[0]

    withFnLock(matches[0]):
      discard matches[0].tid.unify(callType)
  else:
    # Try again after functions have had a chance to be fully inferred.
    # Even if there's no match, it'll allow us to be more accurate
    # with type errors.
    ctx.funcsToResolve.add((n, callType, matches, fails))

proc getTypeIdFromSyntax*(ctx: Module, st: SyntaxType,
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
  of IrSection:
    var sec: string = ctx.contents.prefix

    if sec == "":
      sec = ctx.contents.sectName
    else:
      sec &= "." & ctx.contents.sectName

    return (fmt("Section", sec, ctx.contents.instance), @[ctx.contents.blk])
  of IrLoop:
    if ctx.contents.label != nil:
      moreinfo = "label: " & ctx.contents.label.getText()
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
    return (fmt("AttrAssign", "", moreinfo, ctx.getTid()),
            @[ctx.contents.attrLhs, ctx.contents.attrRhs])
  of IrVarAssign:
    return (fmt("VarAssign", "", "", ctx.getTid()),
            @[ctx.contents.varlhs, ctx.contents.varrhs])
  of IrConditional:
    return (fmt("Conditional"), @[ctx.contents.predicate,
                                  ctx.contents.trueBranch,
                                  ctx.contents.falseBranch])
  of IrJump:
    if ctx.contents.exitLoop:
      moreinfo = "break"
    else:
      moreinfo = "continue"
    if ctx.contents.targetNode.contents.label != nil:
      let labelStr = ctx.contents.targetNode.contents.label.getText()
      result = (fmt("Jump", moreinfo, "label: " & labelStr), @[])
    else:
      result = (fmt("Jump", moreinfo), @[])
  of IrRet:
    return (fmt("Return", "", "", ctx.getTid()), @[ctx.contents.retVal])
  of IrLit:
    if ctx.getTid().isBasicType():
      moreinfo = ctx.reprBasicLiteral()
    return (fmt("Literal", "", moreinfo, ctx.getTid()), ctx.contents.items)
  of IrFold:
    return (fmt("Folded", "", ctx.reprBasicLiteral(), ctx.getTid()), @[])
  of IrNop:
    return (fmt("Nop"), @[])
  of IrMember:
    return (fmt("Member", ctx.contents.name, "", ctx.getTid()), @[])
  of IrIndex:
    return (fmt("Index"), @[ctx.contents.indexStart, ctx.contents.indexEnd])
  of IrCall:
    descriptor = ctx.contents.fname
    if ctx.contents.module != "":
      descriptor = ctx.contents.module & "::" & descriptor
    if ctx.contents.replacement:
      moreinfo = "converted op to call"
    return (fmt("Call", descriptor, moreinfo, ctx.getTid()),
            ctx.contents.actuals)
  of IrUse:
    return (fmt("Use", ctx.contents.targetModule, ctx.contents.targetLoc), @[])
  of IrUminus:
    return (fmt("Uminus", "", "", ctx.getTid()), @[ctx.contents.uRhs])
  of IrNot:
    return (fmt("Not"), @[ctx.contents.uRhs])
  of IrBinary:
    return (fmt("BinaryOp", ctx.contents.bOp, "", ctx.getTid()),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrBool:
    return (fmt("BooleanOp", ctx.contents.bOp, "", ctx.getTid()),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrLogic:
    return (fmt("LogicOp", ctx.contents.bOp, "", ctx.getTid()),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrLhsLoad:
    return (fmt("LhsLoad", ctx.contents.symbol.name, "", ctx.getTid()), @[])
  of IrLoad:
    return (fmt("Load", ctx.contents.symbol.name, "", ctx.getTid()), @[])

proc toRope*(ctx: IrNode): Rope =
  if ctx == nil:
    return em("Null node")
  return ctx.quickTree(irWalker)

proc getLitMod(ctx: Module): string =
  return ctx.pt.token.litType

proc irNode(ctx: Module, kind: IrNodeType): IrNode =
  let payload = IrContents(kind: kind)
  result = IrNode(parseNode: ctx.pt, contents: payload, parent: ctx.current)
  ctx.current = result

proc parseTreeToIr(ctx: Module): IrNode

proc downNode(ctx: Module, which: int): IrNode =
  var saved   = ctx.current
  var savedpt = ctx.pt
  ctx.pt      = ctx.pt.children[which]
  result      = ctx.parseTreeToIr()
  ctx.pt      = savedpt
  ctx.current = saved

proc downNode(ctx: Module, kid, grandkid: int): IrNode =
  var saved = ctx.current
  var savedpt = ctx.pt

  ctx.pt      = ctx.pt.children[kid].children[grandkid]
  result      = ctx.parseTreeToIr()
  ctx.pt      = savedpt
  ctx.current = saved

template independentSubtree(code: untyped) =
  var saved = ctx.current

  ctx.current = nil
  code
  ctx.current = saved

template getText(ctx: Module): string =
  ctx.pt.getText()

template getText(ctx: Module, which: int): string =
   ctx.pt.children[which].getText()

template getText(ctx: Module, kidIx: int, grandIx: int): string =
  ctx.pt.children[kidIx].children[grandIx].getText()

template numKids(ctx: Module): int =
  ctx.pt.children.len()

template kidKind(ctx: Module, i: int): Con4mNodeKind =
  ctx.pt.children[i].kind

template kidKind(ctx: Module, i, j: int): Con4mNodeKind =
  ctx.pt.children[i].children[j].kind

template numGrandKids(ctx: Module, i: int): int =
  ctx.pt.children[i].children.len()

template parseKid(ctx: Module, i: int): Con4mNode =
  ctx.pt.children[i]

template parseGrandKid(ctx: Module, i, j: int): Con4mNode =
  ctx.pt.children[i].children[j]

proc isConstant*(n: IrNode): bool =
  # Doesn't capture everything that's constant, just things we
  # are currently folding.
  return n.getTid() != TBottom and n.getTid().isBuiltinType() and
                                                       n.value.isSome()

const
  ntLoops        = [ NodeForStmt, NodeWhileStmt ]
  ntConditionals = [ NodeIfStmt, NodeElifStmt ]

proc addFalseBranch(conditional: IrNode, falseBranch: IrNode) =
  var n = conditional
  while n.contents.falseBranch != nil:
    n = n.contents.falseBranch

  n.contents.falseBranch = falseBranch

proc checkLabelDupe(ctx: Module, label: string) =
  var
    n       = ctx.current.parent

  while n != nil:
    if n.contents.kind == IrLoop and n.contents.label != nil and
       n.contents.label.getText() == label:
      ctx.irWarn("LabelDupe", n, @[label])
      return
    n = n.parent

proc convertEnum*(ctx: Module) =
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
        if not v.getTid().isIntType():
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

      if itemName[0] == '$':
        ctx.irError("$assign", w = ctx.parseGrandKid(i, 0))

      var
        symOpt = ctx.scopeDeclare(ctx.moduleScope, itemName, false, TInt,
                  declnode = ctx.parseGrandkid(i, 0))

      if symOpt.isSome():
        let sym = symOpt.get()
        sym.constValue = some(newCbox(value, TInt))
        sym.immutable = true

proc convertIdentifier(ctx: Module): IrNode =
  var name = ctx.pt.getText()
  var stOpt: Option[SymbolInfo]

  if ctx.lhsContext:
    if ctx.attrContext and ctx.curSecPrefix != "":
      name = ctx.curSecPrefix & "." & name

    result = ctx.irNode(IrLhsLoad)
    stOpt  = ctx.addDef(name, result, tVar())

  else:
    result = ctx.irNode(IrLoad)
    stOpt = ctx.addUse(name, result, tVar())


  result.contents.symbol = stOpt.getOrElse(nil)

  if result.contents.symbol != nil:
    result.tid = result.contents.symbol.getTid()

proc convertAttrAssignment(ctx: Module): IrNode =
  result                  = ctx.irNode(IrAttrAssign)
  ctx.lhsContext          = true
  ctx.attrContext         = true

  if ctx.pt.getText() == "=":
    ctx.ambigAssign = true

  result.contents.attrLhs = ctx.downNode(0)
  ctx.attrContext         = false
  ctx.lhsContext          = false
  ctx.ambigAssign         = false
  result.contents.attrRhs = ctx.downNode(1)

  result.tid = ctx.typeCheck(result.contents.attrRhs.getTid(),
                             result.contents.attrLhs.getTid())

  if result.contents.attrLhs.contents.kind notin [IrMember, IrLhsLoad]:
    ctx.irError("AttrLhs", w = result.contents.attrLhs)

proc convertMember(ctx: Module): IrNode =
  result = ctx.irNode(IrMember)
  var parts: seq[string]
  for i, kid in ctx.pt.children:
    case kid.kind:
    of NodeIdentifier:
      parts.add(kid.getText())
    else:
      if i != ctx.pt.children.len() - 1:
        ctx.irError("MemberTop", w = ctx.pt)
        return
      result.contents.subaccess = ctx.downnode(i)

  result.contents.name = parts.join(".")

  if ctx.attrContext:
    if ctx.curSecPrefix != "":
      result.contents.name = ctx.curSecPrefix & "." & result.contents.name

  let sym = ctx.addAttrDef(result.contents.name, result, tVar(), false)

  if sym.isSome():
    result.contents.attrSym = sym.get()
    result.tid              = result.contents.attrSym.getTid()

proc convertVarAssignment(ctx: Module): IrNode =
  result = ctx.irNode(IrVarAssign)
  ctx.lhsContext = true
  let lhsIr = ctx.downNode(0)
  ctx.lhsContext = false
  let rhsIr = ctx.downNode(1)

  if lhsIr.contents.kind == IrLit:
    for node in lhsIr.contents.items:
      if node.contents.kind != IrLhsLoad:
        ctx.irError("TupleLhs", w = node)

  result.tid = ctx.typeCheck(lhsIr.getTid(), rhsIr.getTid())

  result.contents.varLhs = lhsIr
  result.contents.varRhs = rhsIr

proc extractSymInfo(ctx: Module, scope: var Scope, isConst = false,
                    checkdollar = false): SymbolInfo {.discardable.} =
  # Returns the last symbol; useful for convertParamBlock where
  # it only accepts one symbol.
  var
    toAdd: seq[(string, TypeId, Con4mNode)]

  for varNode in ctx.pt.children:
    if varNode.kind in [NodeGlobalStmt, NodeVarStmt, NodeConstStmt]:
      continue
    var
      varNames:  seq[string]
      foundType: TypeId = TBottom
    for kid in varNode.children:
      if kid.kind == NodeIdentifier:
        let name = kid.getText()
        if name[0] == '$':
          ctx.irError("$assign", w = kid)
        varNames.add(name)
      else:
        foundType = kid.buildType()
    for i, oneVarName in varNames:
      if foundType == TBottom:
        toAdd.add((oneVarName, tVar(), varnode.children[i]))
      else:
        toAdd.add((oneVarName, foundType.copyType().typeId,
                   varnode.children[i]))

  for i, (name, tid, kid) in toAdd:
    let symOpt = ctx.scopeDeclare(scope, name, false, tid.getTid(), isConst,
                                  declnode = kid)
    if i == toAdd.len() - 1 and symOpt.isSome():
      result = symOpt.get()

proc convertVarStmt(ctx: Module) =
  var
    symbolsAreConst = false
    sym: SymbolInfo

  if ctx.pt.children[^1].kind == NodeConstStmt:
    symbolsAreConst = true

  if ctx.funcScope != nil:
    sym = ctx.extractSymInfo(ctx.funcScope, symbolsAreConst,
                               checkdollar = true)
  else:
    sym = ctx.extractSymInfo(ctx.moduleScope, symbolsAreConst,
                             checkdollar = true)

proc convertGlobalStmt(ctx: Module) =
  var symbolsAreConst = false

  if ctx.pt.children.len() == 0:
    return

  if ctx.pt.children[^1].kind == NodeConstStmt:
    symbolsAreConst = true

  ctx.extractSymInfo(ctx.globalScope, symbolsAreConst, checkdollar = true)

proc convertConstStmt(ctx: Module) =
  var scope: Scope

  if ctx.pt.children.len() == 0:
    return

  if ctx.pt.children[^1].kind == NodeGlobalStmt:
    scope = ctx.globalScope
  elif ctx.funcScope == nil:
    scope = ctx.moduleScope
  else:
    scope = ctx.funcScope

  ctx.extractSymInfo(scope, true, checkdollar = true)


proc convertParamBody(ctx: Module, sym: var SymbolInfo) =
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
          ctx.irError("ParamType", @["validator", "callback"],
                      ctx.pt.children[i])
        let irNode = ctx.downNode(i, 1)
        ctx.typeCheck(irNode.tid, tFunc(@[sym.tid, TString]))
        let cb              = toVal[Callback](irNode.value.get())
        paramInfo.validator = some(cb)
        gotValid            = true
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

proc convertParamBlock(ctx: Module) =
  var sym: SymbolInfo

  if ctx.pt.children[0].kind == NodeMember:

    independentSubtree:
      let memberIr = ctx.downNode(0)
      sym = ctx.scopeDeclare(ctx.usedAttrs, memberIr.contents.name, false,
                             tVar()).getOrElse(nil)
  else: # will be something we can call extractSymInfo on.
    var savedPt = ctx.pt
    ctx.pt = ctx.pt.children[0]
    sym = ctx.extractSymInfo(ctx.moduleScope, checkdollar = true)
    ctx.pt = savedPt

  var savedPt = ctx.pt
  ctx.pt = ctx.pt.children[1]
  ctx.convertParamBody(sym)
  ctx.pt = savedPt

proc convertFormal(info: FuncInfo, n: Con4mNode) =
  var formalInfo = FormalInfo()

  if n.children[0].kind == NodeIdentifier:
    formalInfo.name = n.children[0].getText()
    formalInfo.loc  = n.children[0]

  else:
    formalInfo.va   = true
    formalInfo.name = n.children[0].children[0].getText()
    formalInfo.loc  = n.children[0].children[0]

  if n.children.len() == 2:
    formalInfo.tid = n.children[1].buildType()
  else:
    formalInfo.tid = tVar()

  info.params.add(formalInfo)
  info.paramNames.add(formalInfo.name) # TODO: redundant w/ formalInfo.name

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
    actTypes.add(actual.getTid())

  actTypes.add(info.retVal.getTid())

  info.tid = tFunc(actTypes, va)

  if va == true:
    # From the body of the function, the named parameter collects
    # all arguments of the given type into a list. This sets the
    # ACTUAL type that we'll add to the function's symbol table.
    info.params[^1].tid = tList(info.params[^1].getTid())

proc handleFuncdefSymbols(ctx:   Module,
                          info:  FuncInfo) =
  var
    symOpt: Option[SymbolInfo]
    allParams = info.params & @[info.retVal]


  info.fnScope = initScope()

  for i, item in allParams:
    symOpt = ctx.scopeDeclare(info.fnScope, item.name, false, item.getTid(),
                              declNode = item.loc)
    allParams[i].sym = symOpt.getOrElse(nil)

  symOpt = ctx.scopeDeclare(ctx.moduleScope, info.name, true)


  if symOpt.isSome():
    symOpt.get().fimpls.add(info)

proc findDeclarations(ctx: Module, n: Con4mNode)

proc convertFuncDefinition(ctx: Module) =
  ## This converts the declaration portion, NOT the body. For now, the
  ## body just goes into the FuncInfo `rawImpl` parameter. Once we have
  ## found all explicitly declared symbols, we then come back to
  ## convert the tree into IR.
  var
    funcName   = ctx.getText(0)
    info       = FuncInfo(defModule: ctx)
    returnType = Con4mNode(nil)

  # Params are in the second node, and the last item there might
  # have the varargs marker, which changes what we insert into the symbol
  # table for that thing.
  for item in ctx.pt.children[1].children:
    info.convertFormal(item)

  if ctx.numKids() == 4:
    returnType = ctx.pt.children[2].children[0]

  info.setupTypeSignature(returnType)
  info.name = funcName
  ctx.handleFuncdefSymbols(info)

  info.rawImpl = ctx.pt.children[^1]

  ctx.funcScope = info.fnScope
  ctx.findDeclarations(info.rawImpl)
  ctx.funcScope = nil
  info.lockFn()

  # Below, we also 'lift' this function into the global scope, unless
  # there is a global variable that currently conflicts.
  #
  # In the future we may add 'private' functions that stop them from
  # being lifted into the global symbol table.
  var sym: SymbolInfo
  let symOpt = ctx.globalScope.table.lookup(funcName)
  if symOpt.isSome():
    sym = symOpt.get()
    if not sym.isFunc:
      ctx.irWarn("CantLiftFunc", @[funcName], w = info.rawImpl)
      return
  else:
    sym = ctx.scopeDeclare(ctx.globalScope, funcName, true).get()

  sym.fimpls.add(info)

proc processUseStmt(ctx: Module) =
  ## Since currently use statements both import symbols and cause
  ## execution, we want to import symbols early, but we'll also leave
  ## this in the tre, and process it in the 2nd pass w/ convertUseStmt()
  var
    moduleName = ctx.getText(0)
    moduleLoc  = if ctx.numKids() == 2: ctx.getText(1) else: ""

  ctx.usedModules.add((moduleLoc, moduleName))

proc findAndLoadModule(ctx: var CompileCtx, location, fname, ext: string):
                       Option[Module] {.importc, cdecl.}

proc convertUseStmt(ctx: Module): IrNode =
  var
    modName = ctx.getText(0)
    loc     = ""
  result                       = ctx.irNode(IrUse)
  result.contents.targetModule = modName
  if ctx.numKids() == 2:
    loc                       = ctx.getText(1)
    result.contents.targetLoc = loc

  let possibleModule = ctx.compileCtx.findAndLoadModule(loc, modName, "")
  if possibleModule.isSome():
    result.contents.moduleObj = possibleModule.get()
    if result.contents.moduleObj notin ctx.imports:
      ctx.imports.add(result.contents.moduleObj)

proc findDeclarations(ctx: Module, n: Con4mNode) =
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
    of NodeConstStmt:
      ctx.convertConstStmt()
    of NodeParamBlock:
      ctx.convertParamBlock()
    of NodeUseStmt:
      ctx.processUseStmt()
    else:
      ctx.findDeclarations(kid)

  ctx.pt = saved

proc findDeclarations*(ctx: Module) =
  ctx.pt = ctx.root

  # First, pull declarations from the toplevel scope.
  let root = ctx.pt

  ctx.findDeclarations(ctx.pt)

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
      ctx.findDeclarations(item)

proc statementsToIr(ctx: Module): IrNode =
  result = ctx.irNode(IrBlock)

  for i, item in ctx.pt.children:

    case item.kind
    of NodeBreakStmt, NodeContinueStmt, NodeReturnStmt:
      # For these, we're going to process them when we descend;
      # But we check for dead code here.
      if i != ctx.numKids() - 1:
        ctx.irWarn("DeadCode", @[$(item)], w = item)
        # Do process this one statement.
        result.contents.stmts.add(ctx.downNode(i))
        return # Don't process that dead code.
    of NodeLabelStmt:
      if i != ctx.numKids() - 1 and ctx.parseKid(i + 1).kind in ntLoops:
        ctx.labelNode = ctx.parseGrandKid(i, 0)
        ctx.checkLabelDupe(ctx.labelNode.getText())
        continue
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
    of NodeEnumStmt, NodeFuncDef, NodeParamBlock, NodeVarStmt, NodeGlobalStmt,
         NodeConstStmt:
      continue
    else:
      discard  # An expression.

    result.contents.stmts.add(ctx.downNode(i))

proc convertLit(ctx: Module, st: SyntaxType): IrNode =
  var
    err: string
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)

  result.contents.syntax = st

  result.tid = ctx.getTypeIdFromSyntax(st, lmod)

  if result.tid != TBottom:
    let val = parseLiteral(cast[int](result.getTid()), ctx.getText(), err, st)
    if err != "":
      ctx.irError(err)
    else:
      result.value = some(val)

proc convertCharLit(ctx: Module): IrNode =
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
      val       = initializeCharLiteral(cast[int](result.getTid()), codepoint,
                                        err)
    if err != "":
      ctx.irError(err)
    else:
      result.value = some(val)

proc convertTypeLit(ctx: Module): IrNode =
  var
    tvars: Dict[string, TypeId]
    tinfo: TypeId

  tvars.initDict()
  result        = ctx.irNode(IrLit)
  result.tid    = tTypeSpec()
  tinfo         = ctx.pt.buildType(tvars)
  result.value  = some(tinfo.toMixed())

proc convertListLit(ctx: Module): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STList
  result.contents.litmod = ctx.getLitMod()
  var
    itemType             = tVar()

  for i in 0 ..< ctx.numKids:
    let oneItem = ctx.downNode(i)
    ctx.typeCheck(itemType, oneItem.getTid(), ctx.parseKid(i), "TyDiffListItem")
    result.contents.items.add(oneItem)

  if itemType != TBottom:
    result.tid = tList(itemType)

proc convertDictLit(ctx: Module): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STDict
  result.contents.litmod = ctx.getLitMod()
  var
    keyType              = tVar()
    itemType             = tVar()

  for i in 0 ..< ctx.numKids():
    let oneKey = ctx.downNode(i, 0)
    ctx.typeCheck(keyType, oneKey.getTid(), ctx.parseGrandKid(i, 0),
                  "TyDiffKey")
    let oneVal = ctx.downNode(i, 1)
    ctx.typeCheck(itemType, oneVal.getTid(), ctx.parseGrandKid(i, 1),
                  "TyDiffVal")
    result.contents.items.add(oneKey)
    result.contents.items.add(oneVal)

  if itemType != TBottom:
    result.tid = tDict(keyType, itemType)

proc convertTupleLit(ctx: Module): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STTuple
  result.contents.litmod = ctx.getLitMod()
  var
    types:    seq[TypeId]
    gotBottom = false

  for i in 0 ..< ctx.numKids():
    let oneItem = ctx.downNode(i)
    types.add(oneItem.getTid())
    if oneItem.getTid() == TBottom:
      gotBottom = true
    result.contents.items.add(oneItem)

  if not gotBottom:
    result.tid = tTuple(types)

proc convertCallbackLit(ctx: Module): IrNode =
  var cb: Callback

  result = ctx.irNode(IrLit)
  cb.name = ctx.getText(0)
  if ctx.numKids() == 2:
    var
      tvars: Dict[string, TypeId]
    cb.tid = ctx.pt.children[1].buildType(tvars)
    result.tid = cb.getTid()
  else:
    result.tid = tFunc(@[])

  cb.impl = ctx.beginFunctionResolution(result, cb.name, result.tid)

proc convertForStmt(ctx: Module): IrNode =
  result                    = ctx.irNode(IrLoop)
  result.contents.label     = ctx.labelNode
  ctx.labelNode             = nil
  var scope: Scope

  scope = initScope()

  result.contents.scope = scope
  ctx.blockScopes = @[scope] & ctx.blockScopes

  var
    kt   = tVar()
    vt   = tVar()
    dict = false

  if ctx.numKids() == 3:
    result.contents.keyVar = ctx.getText(0, 0)

    discard ctx.scopeDeclare(scope, result.contents.keyVar, false, kt, true)
    if ctx.numGrandKids(0) == 2:
      result.contents.valVar = ctx.getText(0, 1)
      discard ctx.scopeDeclare(scope, result.contents.valVar, false, vt, true)


    # Declare phantom variables.
    discard ctx.scopeDeclare(scope, "$i", false, TInt, true)
    if result.contents.label != nil:
      discard ctx.scopeDeclare(scope, "$i_" & result.contents.label.getText(),
                                              false, TInt, true)

    result.contents.condition = ctx.downNode(1)

    if dict:
      ctx.typeCheck(result.contents.condition.tid, tDict(kt, vt))
    else:
      ctx.typeCheck(result.contents.condition.tid, tList(kt))

    result.contents.loopBody  = ctx.downNode(2)
  else:
    result.contents.keyVar   = ctx.getText(0, 0)
    discard ctx.scopeDeclare(scope, result.contents.keyVar, false, kt, true)
    result.contents.startIx  = ctx.downNode(1)
    result.contents.endIx    = ctx.downNode(2)
    result.contents.loopBody = ctx.downNode(3)

  if ctx.blockScopes.len() > 1:
    ctx.blockScopes = ctx.blockScopes[1 .. ^1]
  else:
    ctx.blockScopes = @[]

proc convertWhileStmt(ctx: Module): IrNode =
  result                    = ctx.irNode(IrLoop)
  result.contents.label     = ctx.labelNode
  ctx.labelNode             = nil
  result.contents.condition = ctx.downNode(0)
  result.contents.loopBody  = ctx.downNode(1)

  if unify(TBool, result.contents.condition.getTid()) == TBottom:
    if result.contents.condition.getTid().canCastToBool():
      ctx.irWarn("BoolAutoCast",
                 @[result.contents.condition.getTid().toString()],
                 w = ctx.parseKid(0))
    else:
      ctx.irError("NoBoolCast",
                  @[result.contents.condition.getTid().toString()],
                  w = ctx.parseKid(0))

proc loopExit(ctx: Module, loopExit: bool): IrNode =
  result                   = ctx.irNode(IrJump)
  result.contents.exitLoop = loopExit
  if ctx.numKids() != 0:
    let label = ctx.getText(0)
    var n = result.parent
    while n != nil:
      if n.contents.kind == IrLoop:

        if n.contents.label != nil and n.contents.label.getText() == label:
          result.contents.targetNode = n
          return
      n = n.parent
    ctx.irError("BadLoopExit", @[ctx.getText(), label])
  else:
    var n = result.parent
    while n != nil:
      if n.contents.kind == IrLoop:
        result.contents.targetNode = n
        return
      n = n.parent

proc convertConditional(ctx: Module): IrNode =
  result                      = ctx.irNode(IrConditional)
  result.contents.predicate   = ctx.downNode(0)
  result.contents.trueBranch  = ctx.downNode(1)

  if ctx.numKids() == 3:
    result.contents.falseBranch = ctx.downNode(2)

  if unify(TBool, result.contents.predicate.getTid()) == TBottom:
    if result.contents.predicate.getTid().canCastToBool():
      ctx.irWarn("BoolAutoCast",
                 @[result.contents.predicate.getTid().toString()],
                 w = ctx.parseKid(0))
    else:
      ctx.irError("NoBoolCast",
                  @[result.contents.predicate.getTid().toString()],
                  w = ctx.parseKid(0))

proc convertReturn(ctx: Module): IrNode =
  result = ctx.irNode(IrRet)
  if ctx.numKids() == 1:
    let rv = ctx.downNode(0)
    result.contents.retVal = rv
    discard ctx.addVarDef("result", result, rv.getTid())

proc convertUnaryPlus(ctx: Module): IrNode =
  result = ctx.downNode(0)

proc convertUnaryMinus(ctx: Module): IrNode =
  result               = ctx.irNode(IrUMinus)
  result.contents.uRhs = ctx.downNode(0)
  result.tid           = result.contents.uRhs.getTid()

proc convertNotOp(ctx: Module): IrNode =
  result               = ctx.irNode(IrNot)
  result.contents.uRhs = ctx.downNode(0)
  result.tid           = TBool


proc convertBooleanOp(ctx: Module): IrNode =
  # Comparison operators.

  result                = ctx.irNode(IrBool)
  result.contents.bOp   = ctx.getText()
  let
    bLhs                = ctx.downNode(0)
    bRhs                = ctx.downNode(1)
  result.contents.bLhs  = bLhs
  result.contents.bRhs  = bRhs

  var operandType = bLhs.getTid().unify(bRhs.getTid())

  if operandType == TBottom and bLhs.getTid().isNumericBuiltin() and
     bRhs.getTid().isNumericBuiltin():
    operandType = ctx.resultingNumType(bLhs.getTid(), bRhs.getTid())

  if operandType == TBottom and bLhs.getTid() != TBottom and
    bRhs.getTid() != TBottom:
    ctx.irError("BinaryOpCompat",
                @[bLhs.getTid().toString(), bRhs.getTid().toString()])

  result.tid = TBool

const notFloatOps = ["<<", ">>", "and", "or", "^", "&", "|", "div", "%"]

proc convertBinaryOp(ctx: Module): IrNode =
  result                = ctx.irNode(IrBinary)
  result.contents.bOp   = ctx.getText()

  let
    bLhs                = ctx.downNode(0)
    bRhs                = ctx.downNode(1)

  result.contents.bLhs  = bLhs
  result.contents.bRhs  = bRhs

  result.tid = bLhs.getTid().unify(bRhs.getTid())

  if result.getTid() == TBottom and bLhs.getTid().isNumericBuiltin() and
     bRhs.getTid().isNumericBuiltin():
    result.tid = ctx.resultingNumType(bLhs.getTid(), bRhs.getTid())

    if result.getTid() == TFloat and result.contents.bOp in notFloatOps:
        result.tid = TBottom

  if result.getTid() == TBottom and bLhs.getTid() != TBottom and
    bRhs.getTid() != TBottom:
    ctx.irError("BinaryOpCompat", @[bLhs.getTid().toString(),
                                    bRhs.getTid().toString()])
  elif result.contents.bOp == "/":
    if bLhs.getTid().intBits() == 128 or bRhs.getTid().intBits() == 128:
      ctx.irError("128BitLimit", @["Float division"])
      result.tid = TBottom
    elif bLhs.getTid() == TUint or bRhs.getTid() == TUint:
      ctx.irError("U64Div")
      result.tid = TBottom

proc convertLogicOp(ctx: Module): IrNode =
  result                = ctx.irNode(IrLogic)
  result.contents.bOp   = ctx.getText()
  let
    bLhs                = ctx.downNode(0)
    bRhs                = ctx.downNode(1)
  result.contents.bLhs  = bLhs
  result.contents.bRhs  = bRhs

  if unify(TBool, bLhs.getTid()) == TBottom:
    if bLhs.getTid().canCastToBool():
      ctx.irWarn("BoolAutoCast", @[bLhs.getTid().toString()],
                 w = ctx.parseKid(0))
    else:
      ctx.irError("NoBoolCast", @[bLhs.getTid().toString()],
                  w = ctx.parseKid(0))
      result.tid = TBottom
      return

  if unify(TBool, bRhs.getTid()) == TBottom:
    if bRhs.getTid().canCastToBool():
      ctx.irWarn("BoolAutoCast", @[bRhs.getTid().toString()],
                 w = ctx.parseKid(1))
      result.tid = TBool
    else:
      ctx.irError("NoBoolCast", @[bRhs.getTid().toString()],
                  w = ctx.parseKid(1))
      result.tid = TBottom
  else:
      result.tid = TBool

proc convertSection(ctx: Module): IrNode =
  var
    haveSpec     = ctx.attrSpec != nil
    savedSecSpec = ctx.curSecSpec
  result                   = ctx.irNode(IrSection)
  result.contents.prefix   = ctx.curSecPrefix
  result.contents.sectName = ctx.getText(0)

  var savedInSection = ctx.secDefContext
  ctx.secDefContext  = true

  if haveSpec:
    let specOpt = ctx.attrSpec.secSpecs.lookup(result.contents.sectName)
    if specOpt.isNone():
      ctx.irError("BadSectionType", @[result.contents.sectName])
    else:
     if savedSecSpec != nil and
        result.contents.sectName notin savedSecSpec.allowedSections:
        ctx.irError("SecNotAllowed", @[result.contents.sectName])

     ctx.curSecSpec = specOpt.get()

     if ctx.curSecSpec.singleton:
       if ctx.pt.children.len() == 3:
         ctx.irError("NotASingleton", @[result.contents.sectName])
     elif ctx.pt.children.len() == 2:
       ctx.irError("IsASingleton", @[result.contents.sectName])

  if ctx.curSecPrefix != "":
    ctx.curSecPrefix &= "." & result.contents.sectName
  else:
    ctx.curSecPrefix = result.contents.sectName

  if ctx.pt.children.len() == 3:
    result.contents.instance = ctx.getText(1)
    ctx.curSecPrefix &= "." & result.contents.instance
    result.contents.blk = ctx.downNode(2)
  else:
    result.contents.blk = ctx.downNode(1)

  ctx.curSecPrefix  = result.contents.prefix
  ctx.curSecSpec    = savedSecSpec
  ctx.secDefContext = savedInSection

# When we are indexing into a tuple, we go ahead and constant-fold the
# index immediately, instead of waiting till the folding pass.
proc foldDown*(ctx: Module, newNode: IrNode) {.importc, cdecl.}

proc convertIndex(ctx: Module): IrNode =
  result = ctx.irNode(IrIndex)
  var
    toIx = ctx.downNode(0)
    brak = ctx.pt

  var
    ixStart = ctx.downNode(1)
    ixEnd: IrNode

  if toIx.contents.kind == IrIndex:
    ctx.irError("NDim", w = brak)
    return
  elif toIx.contents.kind == IrMember:
    result.contents.toIxSym = toIx.contents.attrSym
  elif toIx.contents.kind != IrCall:
    result.contents.toIxSym = toIx.contents.symbol

  result.tid = tVar()

  if ctx.numKids() == 2:
    let tobj = toIx.getTid().idToTypeRef()
    case tobj.kind
    of C4List:
      if TInt.unify(ixStart.getTid()) == TBottom:
        if not ixStart.getTid().isIntType():
          ctx.irError("ListIx")
      ctx.typeCheck(toIx.getTid(), tList(result.tid))
    of C4Dict:
      let expected = tDict(ixStart.getTid(), result.tid)
      ctx.typeCheck(toIx.getTid(), expected)

    of C4Tuple:
      ctx.foldDown(ixStart)
      if not ixStart.isConstant():
        ctx.irError("TupleConstIx", w = ixStart)
        return
      let v = toVal[int](ixStart.value.get())
      if v < 0 or v >= tobj.items.len():
        ctx.irError("TupleIxBound", w = ixStart)
        return
      let to = toIx.getTid().idToTypeRef().items[v]
      ctx.typeCheck(result.tid, to)
    of C4TVar:
      # Currently, we do not try to infer the container type;
      # we instead just error that the type needs to be known
      # to index it.
      ctx.irError("ContainerType", w = toIx)
    else:
      if tobj.typeId == TBottom:
        return
      ctx.irError("NotIndexible", @[tobj.typeId.toString()])
  else:  # SLICE operator. *must* be a list.
    ixEnd = ctx.downNode(2)
    result.tid = ctx.typeCheck(tList(tVar()), toIx.getTid())

  result.contents.toIx       = toIx
  result.contents.indexStart = ixStart
  result.contents.indexEnd   = ixEnd

proc convertCall(ctx: Module): IrNode =
  result                = ctx.irNode(IrCall)
  result.contents.fname = ctx.getText(0)
  result.tid            = tVar()
  result.parseNode      = ctx.parseKid(0)
  var
    fArgs: seq[TypeId]

  for i in 0 ..< ctx.numGrandKids(1):
    let oneActual = ctx.downNode(1, i)
    result.contents.actuals.add(oneActual)
    fArgs.add(oneActual.tid)

  fArgs.add(result.tid)
  result.contents.toCall =
      ctx.beginFunctionResolution(result, result.contents.fname, tFunc(fArgs))

proc fmtImplementationList(fname: string, fns: seq[FuncInfo],
                           t: TypeId, extra: seq[string] = @[]): string =
  var
    row:   seq[string]
    cells: seq[seq[string]]
    cur:   string
    pad:   string
    w = 0

  for num, item in fns:
    cur = "  <strong>"
    cur &= fname & "("
    for i, param in item.params:
      cur &= item.paramNames[i]
      cur &= ": "
      if i + 1 == item.params.len() and param.va:
        cur &= "*"
      cur &= param.tid.toString()
      if i + 1 < item.params.len():
        cur &= ", "
    cur &= ") -> "
    cur &= item.retval.tid.toString()
    cur &= "</strong>"

    row.add(cur)
    if cur.len() > w:
      w = cur.len()

    cur = item.defModule.modname & ":"
    cur &= $(item.rawImpl.token.lineNo) & ":"
    cur &= $(item.rawImpl.token.lineOffset) & "("
    cur &= item.defModule.where & ") "
    if extra.len() != 0:
      cur &= extra[num]

    row.add(cur)
    cells.add(row)
    row = @[]

  pad = `$`(Rune(' ').repeat(w))
  w += 2

  result = "<ol>"
  for row in cells:
    result &= "<li>" & row[0]
    result &= pad[0 .. (w - len(row[0]))]
    result &= row[1]
    result &= "</li>"
  result &= "</ol>"


proc showCallMistakes(fname: string, fns: seq[FuncInfo], t: TypeId): string =
  return fmtImplementationList(fname, fns, t)

proc resolveDeferredSymbols*(ctx: Module) =
  for (n, t, origmatch, origfail) in ctx.funcsToResolve:
    var
      matches: seq[FuncInfo]
      fails = origfail

    if origmatch.len() == 0 and origfail.len() == 0:
      ctx.irError("NoImpl", n, @[n.contents.fname, t.toString()])
      return

    for possibility in origmatch:
      withFnLock(possibility):
        if possibility.tid.unify(t.copyType().typeId) == TBottom:
          fails.add(possibility)
        else:
          matches.add(possibility)

    if matches.len() == 1:
      n.contents.toCall = matches[0]
      withFnLock(matches[0]):
        discard matches[0].tid.unify(t)
      continue
    elif matches.len() == 0:
      let info = showCallMistakes(n.contents.fname, fails, t)
      ctx.irError("BadSig", n, @[n.contents.fname, t.toString(), info, "call"])
    else:
      let info = fmtImplementationList(n.contents.fname, matches, t)
      ctx.irError("CallAmbig",n,
                  @[n.contents.fname, t.toString(), info, "call"])

  ctx.funcsToResolve = @[]

  # Also take a pass looking for overlapping signatures.
  for (name, sym) in ctx.moduleScope.table.items():
    if sym.isFunc and sym.fimpls.len() > 1:
      for i in 0 ..< (sym.fimpls.len() - 1):
        let oneFuncType = sym.fimpls[i].tid
        for j in i + 1 .. (sym.fimpls.len() - 1):
          if oneFuncType.unify(sym.fimpls[j].tid) != TBottom:
            let
              t1 = sym.fimpls[i].tid.toString()
              t2 = sym.fimpls[j].tid.toString()
              l1 = $(sym.fimpls[i].implementation.parseNode.token.lineNo)
            ctx.irError("SigOverlap", w = sym.fimpls[j].implementation,
                                      @[name, t1, t2, l1])


proc parseTreeToIr(ctx: Module): IrNode =
  case ctx.pt.kind:
    of NodeModule, NodeBody:
      result = ctx.statementsToIr()
    of NodeVarAssign:
      result = ctx.convertVarAssignment()
    of NodeIdentifier:
      result = ctx.convertIdentifier()
    of NodeExpression, NodeLiteral, NodeElseStmt, NodeParenExpr:
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
    of NodeType:
      result = ctx.convertTypeLit()
    of NodeDictLit:
      result = ctx.convertDictLit()
    of NodeListLit:
      result = ctx.convertListLit()
    of NodeTupleLit:
      result = ctx.convertTupleLit()
    of NodeCallbackLit:
      result = ctx.convertCallbackLit()
    of NodeUseStmt:
      result = ctx.convertUseStmt()
    of NodeForStmt:
      result = ctx.convertForStmt()
    of NodeWhileStmt:
      result = ctx.convertWhileStmt()
    of NodeBreakStmt:
      result = ctx.loopExit(true)
    of NodeContinueStmt:
      result = ctx.loopExit(false)
    of NodeIfStmt, NodeElifStmt:
      result = ctx.convertConditional()
    of NodeAttrSetLock:
      result               = ctx.downNode(0)
      result.contents.lock = true
    of NodeReturnStmt:
      result = ctx.convertReturn()
    of NodeAttrAssign:
      result = ctx.convertAttrAssignment()
    of NodeSection:
      result = ctx.convertSection()
    of NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt:
      result = ctx.convertBooleanOp()
    of NodeOr, NodeAnd:
      result = ctx.convertLogicOp()
    of NodeMod, NodeMul, NodeDiv, NodeBitOr, NodeBitXor, NodeBitAnd,
       NodeShl, NodeShr:
      result = ctx.convertBinaryOp()
    of NodePlus:
      if ctx.numKids == 2:
        result = ctx.convertBinaryOp()
      else:
        result = ctx.convertUnaryPlus()
    of NodeMinus:
      if ctx.numKids == 2:
        result = ctx.convertBinaryOp()
      else:
        result = ctx.convertUnaryMinus()
    of NodeNot:
      result = ctx.convertNotOp()
    of NodeMember:
      discard
      result = ctx.convertMember()
    of NodeIndex:
      discard
      result = ctx.convertIndex()
    of NodeCall:
      discard
      result = ctx.convertCall()
    else:
      unreachable

proc finishFunctionProcessing(ctx: Module, impl: FuncInfo) =
  # If a result was never set inside the function,
  # then we need to change the type to void.
  #
  # If the return variable was never used, this should not pose a
  # problem. However, if there are any uses of the return value, the
  # error will get picked up when we do our def/use analysis after
  # the folding pass, so we ignore any type error for now.

  let resSym = impl.fnScope.table["result"]
  if resSym.defs.len() == 0:
    discard resSym.tid.unify(TVoid)

  # Next, we go ahead and 're-lock' the function, to be safe. This
  # ensures we, from this point forward, won't accidentally modify the
  # type.
  impl.lockFn()

proc toIr*(ctx: Module): bool {.discardable.} =
  ctx.pt        = ctx.root
  ctx.funcScope = nil
  ctx.ir        = ctx.parseTreeToIr()

  for (name, sym) in ctx.moduleScope.table.items():
    for impl in sym.fimpls:
      impl.unlockFn()
      ctx.funcScope = impl.fnScope
      ctx.pt        = impl.rawImpl
      impl.implementation = ctx.parseTreeToIr() # This should be a body node.
      ctx.finishFunctionProcessing(impl)

    if sym.pInfo != nil and sym.pinfo.defaultParse.isSome():
      ctx.funcScope = nil
      ctx.pt        = sym.pinfo.defaultParse.get()
      ctx.current   = nil
      let paramIr   = ctx.parseTreeToIr()

      sym.pinfo.defaultIr = some(paramIr)
      ctx.typeCheck(sym, paramIr.getTid())

  result = ctx.errors.canProceed()
