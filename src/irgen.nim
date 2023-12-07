## The IR is essentially the graph representation on which we do all checking.
## Ideally, we will keep refining the nodes until we can use them essentially
## as fat VM instructions that we can directly marshal.

import parse, typesystem, types, typeinfo, options, strutils, typebuiltins

type 
  IrNodeType* = enum
    IrBlock, IrLoop, IrAttrAssign, IrVarAssign, IrSectionScope, IrConditional,
    IrJump, IrRet, IrLit, IrMember, IrIndex, IrCall, IrUse, IrUnary, IrBinary,
    IrEnum, IrEnumItem

  IrNode* = ref object
    parseNode*: Con4mNode
    tid*:       Option[TypeId]
    value*:     Option[Any]
    parent*:    IrNode
    case kind*: IrNodeType
    of IrBlock:
      stmts*: seq[IrNode]
    of IrLoop:
      label*:     Con4mNode # Any type of loop.
      keyVar*:    string    # Both types of for loops.
      valVar*:    string    # for k, v in dict
      startIx*:   IrNode    # for x from 0 to 10
      endIx*:     IrNode    # for x from 0 to 10
      condition*: IrNode    # While loop condition or object to iterate over.
      loopBody*:  IrNode
    of IrAttrAssign:
      attrlhs*: string
      attrrhs*: IrNode
      lock*:    bool
    of IrVarAssign:
      varlhs*: seq[IrNode]
      varrhs*: IrNode
    of IrSectionScope:
      secType*: string
      secName*: string
      secBody*: IrNode
    of IrConditional:
      condition*:   IrNode
      trueBranch*:  IrNode
      falseBranch*: IrNode
    of IrJump:
      exitLoop*:   bool
      targetNode*: IrNode
    of IrRet:
      retVal*: IrNode
    of IrLit:
      syntax*: SyntaxType
      litmod*: string # Only for containers.
      items*:  seq[IrNode] # For dicts, [k1, v1, k2, v2]
    of IrMember:
      name*: string
    of IrIndex:
      indexStart*: IrNode
      indexEnd*:   IrNode
    of IrCall:
      module*:  string   # For explicit module specifier
      fname*:   string
      actuals*: seq[IrNode]
    of IrUse:
      targetModule*: string
      targetLoc*: string
    of IrUnary:
      uOp*: string
      uRhs*: IrNode
    of IrBinary:
      bOp*:  string
      bLhs*: IrNode
      bRhs*: IrNode
    of IrEnumItem:
      enumItemName*: string
      customValue*:  IrNode
    of IrEnum:
      enumItems*: seq[IrNode]

  SymbolKind* = enum SymVar, SymAttr, SymFunc, SymParam, SymEnum

  SymbolInfo* = ref object
    name*:       string
    kind*:       SymbolKind
    tid*:        TypeId
    uses*:       seq[IrNode]
    defs*:       seq[IrNode]
    exportIt*:   bool
    fimpl*:      FuncDecl
    pInfo*:      ParamInfo
    shortdoc*:   Option[string]
    doc*:        Option[string]
    validator*:  Option[Callback]
    startValue*: Option[Any]

  IrErrInfo = object
    msg*:     Rope
    err*:     bool
    node*:    Con4mNode
    prevLoc*: IrNode

  IrGenCtx = object
    pt*:          Con4mNode
    parent*:      IrNode
    curFunc*:     SymbolInfo
    params*:      Dict[string, ParamInfo]
    errors*:      seq[IrErrInfo]
    moduleScope*: Dict[string, SymbolInfo]
    enums*:       seq[IrNode]
    lhsContext*:  bool

  # After pass 1, check to see if declared variables are used, and
  # warn if not.

    
template irError*(ctx: var IrGenCtx, msg: string, prevLoc: IrNode = nil) =
  ctx.errors.add(IrrErrInfo(msg: msg.text(), err: true, node: ctx.pt))

template irWarn*(ctx: var IrGenCtx, msg: string, prevLoc: IrNode = nil) =
  ctx.errors.add(IrrErrInfo(msg: msg.text(), err: false, node: ctx.pt))

template irError*(ctx: var IrGenCtx, msg: Rope, prevLoc: IrNode = nil) =
  ctx.errors.add(IrrErrInfo(msg: msg, err: true, node: ctx.pt))

template irWarn*(ctx: var IrGenCtx, msg: Rope, prevLoc: IrNode = nil) =
  ctx.errors.add(IrrErrInfo(msg: msg, err: false, node: ctx.pt))

proc getLitMod(ctx: var IrGenCtx): string =
  return ctx.pt.token.get().litType

proc irNode(ctx: var IrGenCtx, kind: IrNodeType) =
  return IrNode(parseNode: ctx.pt, kind: kind)

proc parseTreeToIr(ctx: var IrGenCtx): IrNode

proc downNode(ctx: var IrGenCtx, which: int): IrNode =
  var savedParent = ctx.parent
  ctx.parent      = ctx.pt
  ctx.pt          = ctx.pt.children[which]
  result          = ctx.parseTreeToIr()
  ctx.pt          = ctx.parent
  ctx.parent      = savedParent

proc downNode(ctx: var IrGenCtx, kid, grandkid: int): IrNode =
  var savedParent = ctx.parent
  ctx.parent = ctx.pt.children[kid]
  ctx.pt     = ctx.parent[grandkid]
  result     = ctx.parseTreeToIr()
  ctx.pt     = ctx.parent.parent
  ctx.parent = savedParent

template getText(ctx: var IrGenCtx): string =
  ctx.pt.getTokenText()

template getText(ctx: var IrGenCtx, which: int): string =
   ctx.pt.children[which].getTokenText() 

template getText(ctx: var IrGenCtx, kidIx: int, grandIx: int): string =
  ctx.pt.children[kidIx].children[grandIx].getTokenText()

template numKids(ctx: var IrGenCtx): int =
  ctx.pt.children.len()

template numGrandKids(ctx: var IrGenCtx, i: int): int =
  ctx.pt.children[i].children.len()

template parseKid(ctx: var IrGenCtx, i: int): Con4mNode =
  ctx.pt.children[i]

template parseGrandKid(ctx: var IrGenCtx, i, j: int): Con4mNode =
  ctx.pt.children[i].children[j]

const 
  ntLoops        = [ NodeForStmt, NodeWhileStmt ]
  ntConditionals = [ NodeIfStmt, NodeElifStmt ]

proc addFalseBranch(conditional: IrNode, falseBranch: IrNode) =
  var n = conditional
  while n.falseBranch != nil:
    n = n.falseBranch

  n.falseBranch = falseBranch

proc checkLabelDupe(ctx: var IrGen, label: string) =
  var n = ctx.parent
  while n != nil:
    if n.kind == IrLoop and n.label == label:
      result.irWarn("Nested loops have the same label (" & label & ")", n)
      return
    n = n.parent

proc ensureEnumValueUniqueness*(ctx: var IrGen, enumNode: IrNode): bool =
  let symOpt = ctx.moduleScope[enumNode.enumItemName]

    if symOpt.isSome():
      let sym = symOpt.get()
      case sym.kind
      of SymEnum:
        if sym.defs[0] != enumNode:
          result.irError("Enum value '" & name & "' has already been defined" &
                       " in an enum.", sym.defs[0])
          return false
      of SymFunc:
        result.irError("Enum value '" & name & "' conflicts with a function " &
          " definition.", sym.defs[0])
        return false
      of SymParam:
        result.irError("Enum value '" & name & "' shares a name with a " &
          "parameter; parameters must be variable; enums must be immutable.",
        sym.defs[0])
        return false
      of SymVar:
        if sym.defs.len() > 0 or (sym.defs.len() == 1 and 
                                  sym.defs[0] != enumNode):
          result.irError("Enum value '" & name & "' must be immutable, but " &
            "has been mutated elsewhere.", sym.defs[0])
          return false
      else:
        discard
    return true
 
proc convertEnum*(ctx: var IrGen) =
  # We skip adding the actual values to the symbol table here, because
  # we're not propogating type information in this pass, at least
  # not yet.
  var stmtNode = ctx.irNode(IrEnum)

  for i in 0 ..< ctx.numKids():
    
    var itemNode = IrNode(kind: rEnumItem, tid: tInt(), 
                          parseNode: ctx.pt.children[i],
                          enumItemName: ctx.getText(i, 0),
                          parent: stmtNode)
    
    if ctx.numKids(i) == 2:
      itemNode.customValue = itemNode.ctx.downNode(i, 1)

    stmtNode.enumItems.add(itemNode)
    if not ctx.ensureEnumValueUniqueness(itemNode):
      return

    let symOpt = ctx.moduleScope[itemNode.enumItemName]
    var sym: SymbolInfo

    if symOpt.isSome():
      sym = symOpt.get()
      sym.kind = SymEnum
      sym.defs.add(itemNode)
      sym.exportIt = true
      sym.tid = tInt()
    else:
      sym = SymbolInfo(name:     itemNode.enumItemName,
                       kind:     SymEnum,
                       tid:      tInt(),
                       defs:     @[itemNode],
                       exportIt: true)
      ctx.moduleScope[itemNode.enumItemName] = sym

proc convertFuncDefinition*(ctx: var IrGen) =
  discard

proc convertVarStmt*(ctx: var IrGen) =
  discard

proc convertGlobalStmt*(ctx: var IrGen) =
  discard

proc convertExportStmt*(ctx: var IrGen) =
  discard

proc convertParamBlock*(ctx: var IrGen) =
  discard

proc statementsToIr(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrBlock)
  var skipI = false

  for i in 0 ..< ctx.numKids():
    let item = ctx.parseKid(i)

    case item.kind
    of NodeBreak, NodeContinue, NodeReturn:
      if i != ctx.numKids() + 1:
        ctx.irWarn("Dead code after '" & ctx.getText() & "' statement.")
        result.stmts.add(ctx.downNode(i))
        return # Don't process that dead code.
    of NodeLabel:
      if i == ctx.numKids() - 1 and ctx.parseKid(i + 1).kind in ntLoops:
          let one   = ctx.downNode(i + 1)
          one.label = ctx.parseGrandKid(i, 0)
          skipI = true
          result.stmts.add(one)
          ctx.checkLabelDupe(one.label)
      else:
        ctx.irError("'label' statement must proceed a 'for' or 'while' loop")
        continue
    of NodeElifStmt:
      if i != 0:
        let prev = ctx.parseKid(i - 1)
        if prev.kind in ntConditionals:
          result.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("'elif' statement must follow an 'if' block or another " &
                  "'elif' block.")
      continue
    of NodeElseStmt:
      if i != 0:
        let prev = ctx.parseKid(i = 1)
        if prev.kind in ntConditionals:
          result.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("'else' statement must follow an 'if' block or another " &
                  "'elif' block.")
      continue
    of NodeEnumStmt:
      ctx.convertEnum()
      continue
    of NodeFuncDef:
      ctx.convertFuncDefinition()
      continue
    of NodeVarStmt:
      ctx.convertVarStmt()
      continue
    of NodeGlobalStmt:
      ctx.convertGlobalStmt()
    of NodeExportStmt:
      ctx.convertExportStmt()
    of NodeParamBlock:
      ctx.convertParamBlock()
    else:
      discard

    if skipI:
      skipI = false
      continue
    else:
      result.stmts.add(ctx.downNode(i))

proc convertAttrAssignment(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrAttrAssign)
  var 
    parts: seq[string] = @[]
    kid = ctx.parseKid(0)
  while true:
    parts.add(kid.getTokenText())
    if kid.children.len() == 0:
      break
    kid = kid.children[0]
  result.attrlhs = parts.join(".")
  result.attrrhs = ctx.downNode(1)
  result.tid     = result.attrrhs.tid

proc convertVarAssignment(ctx: var IrGenCtx): IrNode =
  ctx.convertVarAssignment()
  result = ctx.irNode(IrVarAssign)
  for i in 0 ..< ctx.children.len():
    result.varlhs.add(ctx.downNode(i))
  # The last item wasn't really part of the lhs.
  result.varrhs = result.varlhs.pop()

proc convertConditional(ctx: var IrGenCtx): IrNode =
  result            = ctx.irNode(IrConditional)
  result.condition  = ctx.downNode(0)
  result.trueBranch = ctx.downNode(1)

proc convertSection(ctx: var IrGenCtx): IrNode =
  result         = ctx.irNode(IrSectionScope)
  result.secType = ctx.getText(0)
  if ctx.numKids() == 3:
    result.secName = ctx.getText(1)
    result.body    = ctx.downNode(2)
  else:
    result.body    = ctx.downNode(1)

proc convertForStmt(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrLoop)
  if ctx.numKids() == 3:
    result.keyVar = ctx.getText(0, 0)
    if ctx.numGrandKids(0) == 2:
      result.valVar = ctx.getText(0, 1)

    result.condition = ctx.downNode(1)
    result.loopBody  = ctx.downNode(2)
  else:
    result.keyVar   = ctx.getText(0, 0)
    result.startIx  = ctx.downNode(1)
    result.endIx    = ctx.downNode(2)
    result.loopBody = ctx.downNode(3)

proc convertWhileStmt(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrLoop)
  result.condition = ctx.downNode(0)
  result.loopBody  = ctx.downNode(1)

proc loopExit(ctx: var IrGenCtx, loopExit: bool): IrNode =
  result = ctx.irNode(IrJump)
  result.exitLoop = loopExit
  if ctx.numKids() != 0:
    let label = ctx.getText(0)
    var n = ctx.parent
    while n != nil:
      if n.kind == IrLoop:
        if n.label == label:
          result.targetNode = n
          return
      n = n.parent
    ctx.irError(ctx.getText() & " to label '" & label & 
      "' is invalid, because it is not contained inside a loop that was " &
      "given that label.")

  var n = ctx.parent
  while n != nil:
    if n.kind == IrLoop:
      result.targetNode = n
      return
    n = n.parent

proc convertReturn(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrRet)
  if ctx.numKids() == 1:
    result.retVal = ctx.downNode(0)

proc convertTypeLit(ctx: var IrGenCtx): IrNode =
  var 
    tvars: Dict[string, TypeId]
    tinfo: TypeId

  result        = ctx.irNode(IrLit)
  result.tid    = some(tTypeSpec().typeId)
  tinfo         = ctx.pt.buildType(tvars)
  result.litVal = tinfo.toAny()

proc convertCharLit(ctx: var IrGenCtx): IrNode =
  let 
    err: string
    tid: int
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)

  if litMod != "":
    tid = getTypeIdFromSyntax(tid, lmod, STChrQuotes)
    if err != "":
      ctx.irError(err)
    else:
      let 
        codepoint = ctx.pt.token.get().codepoint
        val       = initializeCharLiteral(tid, codepoint, err)
      if err != "":
        ctx.irError(err)
      else:
        result.value = some(val)

proc convertLit(ctx: var IrGenCtx, st: SyntaxType): IrNode =
  var 
    err: string
    tid: int
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)

  result.syntax = st

  if lmod != "":
    tid = getTypeIdFromSyntax(st, lmod, err)
    if err != "":
      ctx.irError(err)
    else:
      let val = parseLiteral(tid, ctx.getText(), err, st)
      if err != "":
        ctx.irError(err)
      else:
        result.value = some(val)

proc convertListLit(ctx: var IrGenCtx): IrNode =
  result        = ctx.irNode(IrLit)
  result.syntax = STList
  result.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids:
    result.items.add(ctx.downNode(i))

proc convertDictLit(ctx: var IrGenCtx): IrNode =
  result        = ctx.irNode(IrLit)
  result.syntax = STDict
  result.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids():
    result.items.add(ctx.downNode(i, 0))
    result.items.add(ctx.downNode(i, 1))

proc convertTupleLit(ctx: var IrGenCtx): IrNode =
  result        = ctx.irNode(IrLit)
  result.syntax = STTuple
  result.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids():
    result.items.add(ctx.downNode(i))

proc convertCallbackLit(ctx: var IrGenCtx): IrNode =
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
    result.tid = tref(newFuncType(@[])

proc convertMember(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrMember)
  var parts: seq[string]
  for i in 0 ..< ctx.numKids():
    parts.add(ctx.getText(i))
  result.name = parts.join(".")

proc convertIndex(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrIndex)
  result.indexStart = ctx.downNode(0)
  if ctx.numKids() == 2:
    result.indexEnd = ctx.downNode(1)

proc convertCall(ctx: var IrGenCtx): IrNode =
  result       = ctx.irNode(IrCall)
  result.fname = ctx.getText(0)

  for i in 0 ..< ctx.numKids(1):
    result.actuals.add(ctx.downNode(1, i))

proc convertUseStmt(ctx: var IrGenCtx): IrNode =
  result              = ctx.irNode(IrUse)
  result.targetModule = ctx.getText(0)
  if ctx.numKids() == 2:
    result.targetLoc = ctx.getText(1)

proc convertUnaryOp(ctx: var IrGenCtx): IrNode =
  result      = ctx.irNode(IrUnary)
  result.uOp  = ctx.getText()
  result.uRhs = ctx.downNode(0)

proc convertBinaryOp(ctx: var IrGenCtx): IrNode =
  result      = ctx.irNode(IrBinary)
  result.bOp  = ctx.getText()
  result.bLhs = ctx.downNode(0)
  result.bRhs = ctx.downNode(1)

proc parseTreeToIr(ctx: var IrGenCtx): IrNode =
  case ctx.pt.kind
  of NodeModule, NodeBody:
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
  else:
    unreachable


