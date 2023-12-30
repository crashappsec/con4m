import common

const STACKSIZE = 1 shl 12 # 8K stack size

type
  FrameInfo* = object
    m*: Module
    f*: FuncInfo
    n*: Con4mNode

  InterpreterState* = object
    modules*:     Dict[string, Module]
    varStack*:    array[STACKSIZE, pointer]
    framePtr*:    int
    stackPtr*:    int
    callstack*:   seq[FrameInfo]
    loopstack*:   seq[IrNode]
    attributes*:  Dict[string, pointer]
    destructors*: Dict[pointer, Deallocator]
    rtti*:        Dict[pointer, TypeId]
    lhs*:         int
    attrname*:    string
    ip*:          IrNode

template varPop(ctx: var InterpreterState): pointer =
  ctx.stackPtr -= 1
  return ctx.varStack[ctx.stackPtr]

template varPush(ctx: var InterpreterState, p: pointer) =
  ctx.varStack[ctx.stackPtr] = p
  ctx.stackPtr += 1

template varAddr(ctx: var InterpreterState, sym: SymbolInfo): pointer =
  if sym.heapAlloc:
    if sym.global:
      return addr ctx.globalHeap[sym.offset]
    else:
      return addr sym.module.moduleHeap[sym.offset]
  else:
    return addr ctx.varStack[ctx.framePtr + sym.offset]

template getRTTI(ctx: var InterpreterState, sym: SymbolInfo): TypeId =
  ctx.rtti.lookup(ctx.varAddr(sym)).getOrElse(TBottom)

template setRTTI(ctx: var InterpreterState, sym: SymbolInfo, t: TypeId) =
  ctx.rtti[ctx.varAddr(sym)] = t

template enterFrame(ctx: var InterpreterState, framesz: int) =
  ctx.varStack[ctx.stackPtr] = ctx.framePtr
  ctx.stackPtr += 1
  ctx.framePtr = ctx.stackPtr
  ctx.stackPtr += framesz / 8

template exitFrame(ctx: var InterpreterState) =
  ctx.stackPtr = ctx.framePtr - 1
  ctx.framePtr = ctx.varStack[ctx.stackPtr]

proc interpretCode(ctx: var InterpreterState, n: IrNode) =
  case n.kind
  of IrBlock:
    if n.scope != nil:
      ctx.enterFrame(n.scope.scopeSize)
      for item in n.stmts:
        ctx.interpretCode(item)
      ctx.exitFrame()
    else:
      for item in n.stmts:
        ctx.interpretCode(item)
  of IrSection:
    for item in n.blk:
      ctx.interpretCode(item)
  of IrLoop:
    if n.contents.whileLoop:
      discard
    elif n.contents.condition.contents.kind == IrRange:
      discard
    else:
      discard
  of IrAttrAssign:
    ctx.lhs = true
    ctx.interpretCode(n.contents.attrLhs)
    ctx.lhs = false
    ctx.interpretCode(n.contents.attrRhs)

    ctx.attributes[ctx.attrname] = ctx.varPop()
  of IrVarAssign:
    ctx.lhs = true
    ctx.interpretCode(n.contents.varrLhs)
    ctx.lhs = false
    ctx.interpretCode(n.contents.varRhs)
    let
      value   = ctx.varPop()
      dstaddr = cast[ptr pointer](ctx.varPop())

    let destOpt = ctx.destructors.lookup[dstaddr]
    if destOpt.isSome():
      let destructor = destOpt.get()
      if destructor != nil:
        # TODO: delete instead.
        destructor(dstaddr)
        ctx.destructors[dstaddr] = nil

    dstaddr[0] = value
  of IrConditional:
    ctx.interpretCode(n.contents.predicate)
    let val = cast[int](ctx.varPop())
    if val != 0:
      ctx.interpretCode(n.contents.trueBranch)
    else:
      ctx.interpretCode(n.contents.falseBranch)
  of IrSwitch:
    var doneWithSearch = false

    if ctx.typecase:
      let concrete = ctx.getRTTI(n.contents.targetSym)

      for branch in n.contents.branches:
        for condition in branch.contents.conditions:
          if concrete.unify(condition.branchSym.tid) != TBottom:
            doneWithSearch = true
            ctx.interpretCode(branch.contents.action)
    else:
      ctx.interpretCode(n.contents.switchTarget)
      let toMatch = ctx.varPop()


      for branch in n.contents.branches:
        for condition in branch.contents.conditions:
          ctx.interpretCode(condition)
          if condition.contents.kind == IrRange:
            let
              rhigh = cast[int](ctx.varPop())
              rlow  = cast[int](ctx.varPop())
              m     = cast[int](toMatch)

            if m >= rlow and m <= rhigh:
              doneWithSearch = true
              ctx.interpretCode(branch.contents.action)
              break
          elif ctx.valuesEq(ctx.varPop(), toMatch, n.switchTarget.tid):
            doneWithSearch = true
            ctx.interpretCode(branch.contents.action)
            break

        if doneWithSearch:
          break
  of IrRange:
    ctx.interpretCode(n.contents.rangeStart())
    ctx.interpretCode(n.contents.rangeEnd())
  of IrLit:
    if n.value.isSome():
      ctx.varPush(n.value.get().toPtr())
    elif n.contents.items.len() != 0:
      let start = addr ctx.varStack[ctx.stackPtr]
      for item in n.contents.items:
        ctx.interpretCode(item)
      ctx.callLitConstructor(start, n.contents.items.len())
    else:
      ctx.callLitConstructor(nil, 0)
  of IrMember:

  of IrIndex:
  of IrNot:
  of IrUminus:
  of IrBinary:
  of IrBool:
  of IrLogic:
  of IrLhsLoad:
  of IrLoad:
  of IrFold:
  of IrJump:
  of IrRet:
  of IrCall:
  of IrUse:
  of IrNop, IrNil, IrLit, IrSwitchBranch:
    discard


  if n.scope != nil:
    ctx.exitFrame()
