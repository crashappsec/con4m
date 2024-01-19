# This module is really the static evaluator.
# It should evaluate everything that *can* be evaluated statically, but
# right now it is primarily just focused on integers.
#
# We'll do more as time progresses.

import irgen
export irgen

proc fold[T](n: IrNode, val: T) =
  n.contents = IrContents(kind: IrFold)
  n.value    = some(cast[pointer](val))

proc foldIr(ctx: Module)

proc foldDown*(ctx: Module, newNode: IrNode) {.cdecl, exportc.} =
  let savedNode = ctx.current
  ctx.current   = newNode
  ctx.foldIr()
  ctx.current   = savedNode

proc loopFold(ctx: Module) =
  let n = ctx.current

  ctx.foldDown(n.contents.condition)
  ctx.foldDown(n.contents.loopBody)
  if n.contents.condition != nil and n.contents.condition.isConstant():
    var err: string

    let
      pre  = n.contents.condition.value.get()
      post = callCast(pre, n.contents.condition.tid, TBool, err)
      val  = cast[bool](post)

    if err != "":
      ctx.irError(err, w = n)
    elif not val:
      n.contents = IrContents(kind: IrNop)

proc conditionalFold(ctx: Module) =
  let n = ctx.current

  ctx.foldDown(n.contents.predicate)
  ctx.foldDown(n.contents.trueBranch)
  ctx.foldDown(n.contents.falseBranch)
  if n.contents.predicate.isConstant():
    var err: string

    let
      pre  = n.contents.predicate.value.get()
      post = callCast(pre, n.contents.predicate.tid, TBool, err)
      val  = cast[bool](post)

    if err != "":
      ctx.irError(err, w = n)
    elif val:
      n.contents = n.contents.trueBranch.contents
    elif n.contents.falseBranch != nil:
      n.contents = n.contents.falseBranch.contents
    else:
      n.contents = IrContents(kind: IrNop)

proc retFold(ctx: Module) =
  discard

#[
proc listLitFold(ctx: Module) =
  var
    values: seq[pointer]
    err:    string

  let n = ctx.current

  for item in n.contents.items:
    ctx.foldDown(item)
    if not item.isConstant():
      return
    values.add(item.value.get())

  let c = instantiateContainer(n.tid, STList, "", values, err)
  if err == "":
    n.fold(c)
  else:
    ctx.irError(err, w = n)

proc dictLitFold(ctx: Module) =
  discard
  # TODO... this, tuples, ...

proc tupLitFold(ctx: Module) =
  discard

proc litFold(ctx: Module) =
  let t = ctx.current.tid.idToTypeRef()
  case t.kind
  of C4List:
    ctx.listLitFold()
  of C4Dict:
    ctx.dictLitFold()
  of C4Tuple:
    ctx.tupLitFold()
  else:
    discard
]#

proc indexFold(ctx: Module) =
  ctx.foldDown(ctx.current.contents.indexStart)
  ctx.foldDown(ctx.current.contents.indexEnd)

proc callFold(ctx: Module) =
  for item in ctx.current.contents.actuals:
    ctx.foldDown(item)

proc replaceCall*(ctx: Module, n: IrNode, name: string, sig: TypeId,
                 actuals: seq[IrNode]) =
  var
    newcontents = IrContents(kind: IrCall, replacement: true)

  newcontents.fname   = name
  newcontents.toCall  = ctx.beginFunctionResolution(n, name, sig)
  newcontents.actuals = actuals

  n.contents = newcontents

proc uminusFold(ctx: Module) =
  let n = ctx.current

  if not n.tid.isBasicType() or (not n.tid.isIntType() and n.tid != TFloat):
    let
      actuals = @[n.contents.uRhs]
      sig = tFunc(@[n.tid, n.tid])

    ctx.replaceCall(n, "__uminus__", sig, actuals)
  else:
    if n.tid.isSigned() == false:
      ctx.irError("UnsignedUminus")
    elif n.contents.uRhs.isConstant():
      if n.tid.isIntType():
        let
          rhs = n.contents.uRhs
          v   = cast[int64](rhs.value.get())
        fold[int64](n, -v)
      else:
        let
          rhs = n.contents.uRhs
          v   = cast[float](rhs.value.get())
        fold[float](n, -v)

proc notFold(ctx: Module) =
  var err: string

  let n = ctx.current

  if n.contents.uRhs.tid.canCastToBool():
    if n.contents.uRhs.isConstant():
      let
        pre  = n.contents.uRhs.value.get()
        post = callCast(pre, n.contents.uRhs.tid, TBool, err)
      if err != "":
        ctx.irError(err)
      else:
        n.fold(not cast[bool](post))
  else:
    let
      sig     = tFunc(@[n.contents.uRhs.tid, TBool])
      actuals = @[n.contents.uRhs]

    ctx.replaceCall(n, "__not__", sig, actuals)

template opReplace(ctx: Module, o, f: string) =
  if n.contents.bOp == o:
    ctx.replaceCall(n, f, sig, actuals)
    return

proc replaceBinOpWithCall(ctx: Module, n: IrNode) =
  if n.contents.kind != IrBinary:
    return

  let
    sig     = tFunc(@[n.contents.bLhs.tid, n.contents.bRhs.tid, n.tid])
    actuals = @[n.contents.bLhs, n.contents.bRhs]

  ctx.opReplace("/",    "__fdiv__")
  ctx.opReplace("*",    "__mul__")
  ctx.opReplace("+",    "__plus__")
  ctx.opReplace("-",    "__minus__")
  ctx.opReplace("%",    "__mod__")
  ctx.opReplace("<<",   "__shl__")
  ctx.opReplace(">>",   "__shr__")
  ctx.opReplace("div",  "__idiv__")
  ctx.opReplace("&",    "__bitand__")
  ctx.opReplace("|",    "__bitor__")
  ctx.opReplace("^",    "__bitxor__")

proc replaceBoolOpWithCall(ctx: Module, n: IrNode) =
  if n.contents.kind != IrBool:
    return

  let
    sig     = tFunc(@[n.contents.bLhs.tid, n.contents.bRhs.tid, TBool])
    actuals = @[n.contents.bLhs, n.contents.bRhs]

  ctx.opReplace("<",   "__lt__")
  ctx.opReplace("<=",  "__lte__")
  ctx.opReplace(">",   "__gt__")
  ctx.opReplace(">=",  "__gte__")
  ctx.opReplace("!=",  "__ne__")
  ctx.opReplace("==",  "__eq__")

proc boolFold(ctx: Module) =
  let
    node        = ctx.current
    bLhs        = node.contents.bLhs
    bRhs        = node.contents.bRhs

  ctx.foldDown(bLhs)
  ctx.foldDown(bRhs)

  if node.tid.followForwards() == TBottom:
    return

  if not bLhs.tid.isNumericBuiltin():
    ctx.replaceBoolOpWithCall(node)

  elif bLhs.isConstant() and bRhs.isConstant():

    if node.tid.isIntType():
      if node.tid.isSigned():
        let
          v1 = cast[int64](bLhs.value.get())
          v2 = cast[int64](bRhs.value.get())
        case node.contents.bOp
        of "<":
          node.fold(v1 < v2)
        of ">":
          node.fold(v1 > v2)
        of "<=":
          node.fold(v1 <= v2)
        of ">=":
          node.fold(v1 >= v2)
        of "==":
          node.fold(v1 == v2)
        of "!=":
          node.fold(v1 != v2)
        else:
          unreachable
      else:
        let
          v1 = cast[uint64](bLhs.value.get())
          v2 = cast[uint64](bRhs.value.get())
        case node.contents.bOp
        of "<":
          node.fold(v1 < v2)
        of ">":
          node.fold(v1 > v2)
        of "<=":
          node.fold(v1 <= v2)
        of ">=":
          node.fold(v1 >= v2)
        of "==":
          node.fold(v1 == v2)
        of "!=":
          node.fold(v1 != v2)
        else:
          unreachable
    else:
      let
        v1 = cast[float](bLhs.value.get())
        v2 = cast[float](bRhs.value.get())
      case node.contents.bOp
      of "<":
        node.fold(v1 < v2)
      of ">":
        node.fold(v1 > v2)
      of "<=":
        node.fold(v1 <= v2)
      of ">=":
        node.fold(v1 >= v2)
      of "==":
        node.fold(v1 == v2)
      of "!=":
        node.fold(v1 != v2)
      else:
        unreachable

proc binFold(ctx: Module) =
  let
    node        = ctx.current
    bLhs        = node.contents.bLhs
    bRhs        = node.contents.bRhs

  ctx.foldDown(bLhs)
  ctx.foldDown(bRhs)

  if node.tid.followForwards() == TBottom:
    return

  case node.contents.bOp
  of "/":
    if node.tid.isNumericBuiltin():
      if bLhs.isConstant() and bRhs.isConstant():
        # This will currently be set to an int if the operands
        # were ints; we override it for division below. Thus,
        # we can just do this one check.
        if node.tid == TFloat:
          var lhf, rhf: float

          if bLhs.tid == TFloat:
            lhf = cast[float](bLhs.value.get())
          else:
            lhf = float(cast[int64](bLhs.value.get()))

          if bRhs.tid == TFloat:
            rhf = cast[float](bRhs.value.get())
          else:
            rhf = float(cast[int64](bRhs.value.get()))

          node.fold(lhf / rhf)
        let
          l = cast[int64](bLhs.value.get())
          r = cast[int64](bLhs.value.get())

        node.fold(l / r)
      node.tid = TFloat
    else:
      ctx.replaceBinOpWithCall(node)
  of "+", "-", "*":
    if not node.tid.isNumericBuiltin():
      ctx.replaceBinOpWithCall(node)
    elif bLhs.isConstant() and bRhs.isConstant():
      if node.tid == TFloat:
        var lhf, rhf: float

        if bLhs.tid == TFloat:
          lhf = cast[float](bLhs.value.get())
        else:
          lhf = float(cast[int64](bLhs.value.get()))
        if bRhs.tid == TFloat:
          rhf = cast[float](bRhs.value.get())
        else:
          rhf = float(cast[int64](bRhs.value.get()))

        case node.contents.bOp
        of "+":
          node.fold(lhf + rhf)
        of "-":
          node.fold(lhf - rhf)
        of "*":
          node.fold(lhf * rhf)
        else:
          unreachable
      elif node.tid.isSigned():
        let
          li = cast[int64](bLhs.value.get())
          ri = cast[int64](bRhs.value.get())
        case node.contents.bOp
        of "+":
          node.fold(li + ri)
        of "-":
          node.fold(li - ri)
        of "*":
          node.fold(li * ri)
        else:
          unreachable
      else:
        let
          li = cast[uint64](bLhs.value.get())
          ri = cast[uint64](bRhs.value.get())
        case node.contents.bOp
        of "+":
          node.fold(li + ri)
        of "-":
          node.fold(li - ri)
        of "*":
          node.fold(li * ri)
        else:
          unreachable

  of "<<", ">>", "div", "&", "|", "^", "%":
    if node.tid == TFloat or not node.tid.isNumericBuiltin():
      ctx.replaceBinOpWithCall(node)
    elif bLhs.isConstant() and bRhs.isConstant():
      if node.tid.isSigned():
        let
          li = cast[int64](bLhs.value.get())
          ri = cast[int64](bRhs.value.get())
        case node.contents.bop
             of "<<":
               node.fold(li shl ri)
             of ">>":
               node.fold(li shr ri)
             of "div":
               node.fold(li div ri)
             of "&":
               node.fold(li and ri)
             of "|":
               node.fold(li or ri)
             of "^":
               node.fold(li xor ri)
             of "%":
               node.fold(li mod ri)
             else:
               unreachable
      else:
        let
          li = cast[uint64](bLhs.value.get())
          ri = cast[uint64](bRhs.value.get())
        case node.contents.bop
             of "<<":
               node.fold(li shl ri)
             of ">>":
               node.fold(li shr ri)
             of "div":
               node.fold(li div ri)
             of "&":
               node.fold(li and ri)
             of "|":
               node.fold(li or ri)
             of "^":
               node.fold(li xor ri)
             of "%":
               node.fold(li mod ri)
             else:
               unreachable
  else:
    unreachable

proc logicFold(ctx: Module) =
  var
    err: string

  let
    node        = ctx.current
    operandType = node.contents.bOp
    bLhs        = node.contents.bLhs
    bRhs        = node.contents.bRhs

  ctx.foldDown(bLhs)
  ctx.foldDown(bRhs)

  if node.tid.followForwards() == TBottom:
    return

  if bLhs.isConstant():
    if cast[bool](callCast(bLhs.value.get(), bLhs.tid, TBool, err)) == false:
      if node.contents.bOp == "and":
        node.fold(false)
      elif bRhs.isConstant():

        node.fold(callCast(bRhs.value.get(), bRhs.tid, TBool, err))
        if err != "":
          ctx.irError(err)
      else:
        node.contents = bRhs.contents
    else:
      if node.contents.bOp == "or":
        node.fold(true)
      elif bRhs.isConstant():
        node.fold(callCast(bRhs.value.get(), bRhs.tid, TBool, err))
        if err != "":
          ctx.irError(err)
      else:
        node.contents = bRhs.contents
  elif bRhs.isConstant():
    let val = callCast(bRhs.value.get(), bRhs.tid, TBool, err)
    if node.contents.bOp == "and":
      if cast[bool](val) == false:
        node.fold(false)
      else:
        node.contents = bLhs.contents
    else:
      if cast[bool](val) == true:
        node.fold(true)
      else:
        node.contents = bLhs.contents

proc assignFold(ctx: Module) =
  ctx.foldDown(ctx.current.contents.assignLhs)
  ctx.foldDown(ctx.current.contents.assignLhs)
  # For now, we only fold this if the RHS is constant and the LHS is a
  # simple variable, not an array access (even if it's `m[4] = 12`, if
  # `m` is a global).
  if ctx.current.contents.assignRhs.isConstant():
    let lhs = ctx.current.contents.assignLhs
    if lhs.contents.kind == IrLhsLoad:
      let sym = lhs.contents.symbol
      if sym.immutable:
        # Previous pass would catch multiple assignments.
        sym.constValue       = ctx.current.contents.assignRhs.value
        ctx.current.contents = IrContents(kind: IrNop)

proc foldIr(ctx: Module) =
  if ctx.current == nil:
    return

  case ctx.current.contents.kind
  of IrBlock:
    # We could consider pruning this if it's empty (after subtracting
    # nops), but right now we count on it being here when generating
    # the CFG. Anyway, we skip the nops when building the CFG.
    for item in ctx.current.contents.stmts:
      ctx.foldDown(item)
  of IrLoop:
    ctx.loopFold()
  of IrAssign:
    ctx.assignFold()
  of IrConditional:
    ctx.conditionalFold()
  of IrRet:
    ctx.retFold()
  of IrLit:
    # ctx.litFold()
    # Currently, our code generation doesn't pre-initialize container types;
    # it builds them at runtime.  Then you can freeze them...
    discard
  of IrIndex, IrIndexLhs:
    ctx.indexFold()
  of IrCall:
    ctx.callFold()
  of IrUminus:
    ctx.uminusFold()
  of IrNot:
    ctx.notFold()
  of IrBinary:
    ctx.binFold()
  of IrBool:
    ctx.boolFold()
  of IrLogic:
    ctx.logicFold()
  of IrSection:
    ctx.foldDown(ctx.current.contents.blk)
  of IrCast:
    ctx.foldDown(ctx.current.contents.srcData)
  of IrAssert:
    # TODO-- warn if assertion is always true.
    discard
  of IrSwitch, IrSwitchBranch, IrRange:
    discard # TODO... fold down at least. Prune cases that can't be true.
  of IrNil:
    discard
  of IrLoad, IrLhsLoad, IrNop, IrFold, IrUse, IrJump, IrMember, IrMemberLhs:
    discard

proc foldingPass*(ctx: Module) =
  ctx.funcScope = nil
  ctx.current   = ctx.ir
  ctx.foldIr()

  for (name, sym) in ctx.moduleScope.table.items():
    for impl in sym.fimpls:
      ctx.funcScope = impl.fnScope
      ctx.current   = impl.implementation
      ctx.foldIr()
    if sym.pInfo != nil and sym.pinfo.defaultIr.isSome():
      ctx.funcScope = nil
      ctx.current = sym.pinfo.defaultIr.get()
      ctx.foldIr()
