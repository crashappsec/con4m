## The IR is essentially the graph representation on which we do all checking.
## Ideally, we will keep refining the nodes until we can use them essentially
## as fat VM instructions that we can directly marshal.

import parse, typesystem, types, typeinfo, options, strutils, typebuiltins

type 
  IrNodeType* = enum
    IrBlock, IrLoop, IrAttrAssign, IrVarAssign, IrSectionScope, IrConditional,
    IrJump, IrRet, IrLit

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
      litVal*: Any
      
  IrGenCtx = object
    pt:     Con4mNode
    parent: IrNode
    errors: seq[string]
    
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
    result.irError(ctx.getText() & " to label '" & label & 
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
  var tvars: Dict[string, TypeId]

  result        = ctx.irNode(IrLit)
  result.tid    = some(tTypeSpec().typeId)
  result.litVal = ctx.pt.buildType(tvars).toAny()

proc convertStrLit(ctx: var IrGenCtx): IrNode =
  result     = ctx.irNode(IrLit)
  result.tid = some(ctx.getStringTypeFromLitMod(ctx.getLitMod())

proc convertIntLit(ctx: var IrGenCtx): IrNode

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
    result = ctx.convertStrLit()
  of NodeIntLit:
    result = ctx.convetIntLit()
  of NodeFloatLit:
    result = ctx.convertFloatLit()
  of NodeBoolLit:
    result = ctx.convertBoolLit()
  of NodeDictLit:
    result = ctx.convertDictLit()
  of NodeListLit:
    result = ctx.convertListLit()
  of NodeOtherLit:
    result = ctx.convertOtherLit()
  of NodeTupleLit:
    result = ctx.convertTupleLit()
  of NodeCharLit:
    result = ctx.convertCharLit()
  of NodeCallbackLit:
    result = ctx.convertCallbackLit()
  of NodeNoCallbackName:
    result = ctx.convertEmptyCallbackLit()
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt,
     NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    result = ctx.convertBinaryOp()
  of NodeNot:
    result = ctx.convertUnaryOp()
  of NodeMember:
    result = ctx.convertMember()
  of NodeLiteral, NodeParenExpr, NodeExpression:
    result = ctx.downNode(0)
  of NodeIndex:
    result = ctx.convertIndex()
  of NodeActuals:
    result = ctx.convertActuals()
  of NodeCall:
    result = ctx.convertCall()
  of NodeEnumStmt:
    result = ctx.convertEnum()
  of NodeFuncDef:
    result = ctx.convertFuncDefinition()
  of NodeVarStmt:
    result = ctx.convertVarStmt()
  of NodeGlobalStmt:
    result = ctx.convertGlobalStmt()
  of NodeExportStmt:
    result = ctx.convertExportStmt()
  of NodeUseStmt:
    result = ctx.convertUseStmt()
  of NodeParamBlock:
    result = ctx.convertParamBlock()
  else:
    unreachable


