# This module is really the static evaluator.
# It should evaluate everything that *can* be evaluated statically, but
# right now it is primarily just focused on integers.
#
# We'll do more as time progresses.

import irgen
export irgen

proc fold[T](n: IrNode, val: T) =
  n.contents = IrContents(kind: IrFold)
  n.value    = some(toMixed[T](val))

proc foldI128(n: IrNode, value: int128, tid: TypeId) =
  case tid.intBits()
  of 8:
    fold[int8](n, i128toI8(value))
  of 16:
    fold[int16](n, i128toI16(value))
  of 32:
    fold[int32](n, i128toI32(value))
  of 64:
    fold[int64](n, i128toI64(value))
  of 128:
    fold[int128](n, value)
  else:
    unreachable

proc foldU128(n: IrNode, value: uint128, tid: TypeId) =
  case tid.intBits()
  of 8:
    fold[uint8](n, u128toU8(value))
  of 16:
    fold[uint16](n, u128toU16(value))
  of 32:
    fold[uint32](n, u128toU32(value))
  of 64:
    fold[uint64](n, u128toU64(value))
  of 128:
    fold[uint128](n, value)
  else:
    unreachable

proc foldIr(ctx: Module)

proc foldDown*(ctx: Module, newNode: IrNode) {.cdecl, exportc.} =
  let savedNode = ctx.current
  ctx.current   = newNode
  ctx.foldIr()
  ctx.current   = savedNode

proc loopFold(ctx: Module) =
  let n = ctx.current

  ctx.foldDown(n.contents.startIx)
  ctx.foldDown(n.contents.endIx)
  ctx.foldDown(n.contents.condition)
  ctx.foldDown(n.contents.loopBody)
  if n.contents.condition != nil and n.contents.condition.isConstant():
    let
      valToCast = n.contents.condition.value.get()
      val       = valToCast.castToBool(n.contents.condition.tid).get()

    if not val:
      n.contents = IrContents(kind: IrNop)

proc conditionalFold(ctx: Module) =
  let n = ctx.current

  ctx.foldDown(n.contents.predicate)
  ctx.foldDown(n.contents.trueBranch)
  ctx.foldDown(n.contents.falseBranch)
  if n.contents.predicate.isConstant():
    let
      valToCast = n.contents.predicate.value.get()
      val       = valToCast.castToBool(n.contents.predicate.tid).get()

    if val:
      n.contents = n.contents.trueBranch.contents
    elif n.contents.falseBranch != nil:
      n.contents = n.contents.falseBranch.contents
    else:
      n.contents = IrContents(kind: IrNop)

proc retFold(ctx: Module) =
  discard

proc listLitFold(ctx: Module) =
  var values: seq[Mixed]

  for item in ctx.current.contents.items:
    ctx.foldDown(item)
    if not item.isConstant():
      return
    values.add(item.value.get())

  # TODO: finish this.

proc dictFold(ctx: Module) =
  discard

proc litFold(ctx: Module) =
  discard

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
          v   = rhs.value.get().castToI128(rhs.tid).get()
          neg = itoI128(-1) * v

        foldI128(n, neg, n.tid)
      else:
        let
          rhs = n.contents.uRhs
          v   = toVal[float](rhs.value.get())
        fold[float](n, -v)

proc notFold(ctx: Module) =
  let n = ctx.current

  if n.contents.uRhs.tid.canCastToBool():
    if n.contents.uRhs.isConstant():
      let
        box = n.contents.uRhs.value.get()
        v   = castToBool(box, n.contents.uRhs.tid).get()
      n.fold(not v)
  else:
    let
      sig     = tFunc(@[n.contents.uRhs.tid, TBool])
      actuals = @[n.contents.uRhs]

    ctx.replaceCall(n, "__not__", sig, actuals)

template opReplace(ctx: Module, o, f: string) =
  if n.contents.bOp == o:
    ctx.replaceCall(n, f, sig, actuals)
    return

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

  ctx.opReplace("/",    "__slash__")
  ctx.opReplace("*",    "__star__")
  ctx.opReplace("+",    "__plus__")
  ctx.opReplace("-",    "__minus__")
  ctx.opReplace("%",    "__percent__")
  ctx.opReplace("<<",   "__shl__")
  ctx.opReplace(">>",   "__shr__")
  ctx.opReplace("div",  "__div__")
  ctx.opReplace("&",    "__bitand__")
  ctx.opReplace("|",    "__bitor__")
  ctx.opReplace("^",    "__bitxor__")
  ctx.opReplace("shl",  "__shl__")
  ctx.opReplace("shr",  "__shr__")

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

  if not node.tid.isNumericBuiltin():
    ctx.replaceBoolOpWithCall(node)

  elif bLhs.isConstant() and bRhs.isConstant():

    if node.tid.isIntType():
      if node.tid.isSigned():
        let
          v1: int128 = bLhs.value.get().castToI128(bLhs.tid).get()
          v2: int128 = bRhs.value.get().castToI128(bRhs.tid).get()
        case node.contents.bOp
        of "<":
          node.fold(int128_t.`<`(v1, v2))
        of ">":
          node.fold(int128_t.`>`(v1, v2))
        of "<=":
          node.fold(int128_t.`<=`(v1, v2))
        of ">=":
          node.fold(int128_t.`>=`(v1, v2))
        of "==":
          node.fold(int128_t.`eq`(v1, v2))
        of "!=":
          node.fold(int128_t.`neq`(v1, v2))
        else:
          unreachable
      else:
        let
          v1 = bLhs.value.get().castToU128(bLhs.tid).get()
          v2 = bRhs.value.get().castToU128(bRhs.tid).get()
        case node.contents.bOp
        of "<":
          node.fold(int128_t.`<`(v1, v2))
        of ">":
          node.fold(int128_t.`>`(v1, v2))
        of "<=":
          node.fold(int128_t.`<=`(v1, v2))
        of ">=":
          node.fold(int128_t.`>=`(v1, v2))
        of "==":
          node.fold(int128_t.`eq`(v1, v2))
        of "!=":
          node.fold(int128_t.`neq`(v1, v2))
        else:
          unreachable
    else:
      let
        v1 = toVal[float](bLhs.value.get())
        v2 = toVal[float](bRhs.value.get())
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
            lhf = toVal[float](bLhs.value.get())
          else:
            let asI128 = bLhs.value.get().castToI128(bLhs.tid).get()
            lhf        = float(i128ToI64(asI128))

          if bRhs.tid == TFloat:
            rhf = toVal[float](bRhs.value.get())
          else:
            let asI128 = bRhs.value.get().castToI128(bRhs.tid).get()
            rhf = float(i128ToI64(asI128))

          node.fold(lhf / rhf)
        let
          l128 = bLhs.value.get().castToI128(bLhs.tid).get()
          r128 = bLhs.value.get().castToI128(bRhs.tid).get()
          l    = i128ToI64(l128)
          r    = i128ToI64(r128)
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
          lhf = toVal[float](bLhs.value.get())
        else:
          let asI128 = bLhs.value.get().castToI128(bLhs.tid).get()
          lhf = float(i128ToI64(asI128))
        if bRhs.tid == TFloat:
          rhf = toVal[float](bRhs.value.get())
        else:
          let asI128 = bRhs.value.get().castToI128(bRhs.tid).get()
          rhf = float(i128ToI64(asI128))

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
          l128 = bLhs.value.get().castToI128(bLhs.tid).get()
          r128 = bRhs.value.get().castToI128(bRhs.tid).get()
        case node.contents.bOp
        of "+":
          node.foldI128(int128_t.`+`(l128, r128), node.tid)
        of "-":
          node.foldI128(int128_t.`-`(l128, r128), node.tid)
        of "*":
          node.foldI128(int128_t.`*`(l128,  r128), node.tid)
        else:
          unreachable
      else:
        let
          l128 = bLhs.value.get().castToU128(bLhs.tid).get()
          r128 = bRhs.value.get().castToU128(bRhs.tid).get()
        case node.contents.bOp
        of "+":
          node.foldU128(l128 + r128, node.tid)
        of "-":
          node.foldU128(l128 - r128, node.tid)
        of "*":
          node.foldU128(l128 * r128, node.tid)
        else:
          unreachable

  of "<<", ">>", "div", "&", "|", "^", "%":
    if node.tid == TFloat or not node.tid.isNumericBuiltin():
      ctx.replaceBinOpWithCall(node)
    elif bLhs.isConstant() and bRhs.isConstant():
      if node.tid.isSigned():
        let
          l128 = bLhs.value.get().castToI128(bLhs.tid).get()
          r128 = bRhs.value.get().castToI128(bRhs.tid).get()
        case node.contents.bop
             of "<<":
               node.foldI128(l128 shl r128, node.tid)
             of ">>":
               node.foldI128(l128 shr r128, node.tid)
             of "div":
               node.foldI128(l128 div r128, node.tid)
             of "&":
               node.foldI128(l128 and r128, node.tid)
             of "|":
               node.foldI128(l128 or r128, node.tid)
             of "^":
               node.foldI128(l128 xor r128, node.tid)
             of "%":
               node.foldI128(l128 mod r128, node.tid)
             else:
               unreachable
      else:
        let
          l128 = bLhs.value.get().castToU128(bLhs.tid).get()
          r128 = bRhs.value.get().castToU128(bRhs.tid).get()
        case node.contents.bop
             of "<<":
               node.foldU128(l128 shl r128, node.tid)
             of ">>":
               node.foldU128(l128 shr r128, node.tid)
             of "div":
               node.foldU128(l128 div r128, node.tid)
             of "&":
               node.foldU128(l128 and r128, node.tid)
             of "|":
               node.foldU128(l128 or r128, node.tid)
             of "^":
               node.foldU128(l128 xor r128, node.tid)
             of "%":
               node.foldU128(l128 mod r128, node.tid)
             else:
               unreachable
  else:
    unreachable

proc logicFold(ctx: Module) =
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
    if bLhs.value.get().castToBool(bLhs.tid).get() == false:
      if node.contents.bOp == "and":
        node.fold(false)
      elif bRhs.isConstant():
        node.fold(toMixed(bRhs.value.get().castToBool(bRhs.tid).get()))
      else:
        node.contents = bRhs.contents
    else:
      if node.contents.bOp == "or":
        node.fold(true)
      elif bRhs.isConstant():
        node.fold(toMixed(bRhs.value.get().castToBool(bRhs.tid).get()))
      else:
        node.contents = bRhs.contents
  elif bRhs.isConstant():
    let val = bRhs.value.get().castToBool(bRhs.tid).get()
    if node.contents.bOp == "and":
      if val == false:
        node.fold(false)
      else:
        node.contents = bLhs.contents
    else:
      if val == true:
        node.fold(true)
      else:
        node.contents = bLhs.contents

proc varAssignFold(ctx: Module) =
  ctx.foldDown(ctx.current.contents.varlhs)
  ctx.foldDown(ctx.current.contents.varrhs)
  # For now, we only fold this if the RHS is constant and the LHS is a
  # simple variable, not an array access (even if it's `m[4] = 12`, if
  # `m` is a global).
  if ctx.current.contents.varrhs.isConstant():
    let lhs = ctx.current.contents.varlhs
    if lhs.contents.kind == IrLhsLoad:
      let sym = lhs.contents.symbol
      if sym.immutable:
        # Previous pass would catch multiple assignments.
        var box: Cbox
        box.t = ctx.current.contents.varlhs.tid
        box.v = ctx.current.contents.varrhs.value.get()
        sym.constValue = some(box)
        ctx.current.contents = IrContents(kind: IrNop)

proc attrAssignFold(ctx: Module) =
  ctx.foldDown(ctx.current.contents.attrLhs)
  ctx.foldDown(ctx.current.contents.attrRhs)

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
  of IrAttrAssign:
    ctx.attrAssignFold()
  of IrVarAssign:
    ctx.varAssignFold()
  of IrConditional:
    ctx.conditionalFold()
  of IrRet:
    ctx.retFold()
  of IrLit:
    ctx.litFold()
  of IrIndex:
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
  of IrLoad, IrLhsLoad, IrNop, IrFold, IrUse, IrJump, IrMember:
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
