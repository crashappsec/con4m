# This is a very, very simple set of analyses.  We will have to get
# more complicated when we add ref params.
#
# This analysis could definitely still give errors in some cases where
# a use-before-def is impossible. And, we're happy to set things to
# default values. So currently, we're tagging these as errors, but
# non-fatal ones; meaning, we might be willing to compile and run the
# code anyway.

import fold
export fold

proc gatherDefs(n: IrNode, defs: var seq[SymbolInfo]) =
  if n == nil:
    return

  case n.contents.kind
  of IrLhsLoad:
    if n.contents.symbol notin defs and n.contents.symbol != nil:
      defs.add(n.contents.symbol)
  of IrIndex:
    if n.contents.toIxSym notin defs and n.contents.toIxSym != nil:
      defs.add(n.contents.toIxSym)
    n.contents.toIx.gatherDefs(defs)
    # Things in the indexStart and indexEnd are uses only.
  of IrMember:
    if n.contents.attrSym notin defs and n.contents.attrSym != nil:
      defs.add(n.contents.attrSym)
    n.contents.subaccess.gatherDefs(defs)
  of IrFold, IrNop:
    return
  of IrCall:
    # The subnodes can only be uses.
    return
  # These should all be statement-level ONLY, and thus not ever in the
  # subtree we are searching.
  of IrBlock, IrSection, IrLoop, IrAttrAssign, IrVarAssign,
     IrConditional, IrJump, IrRet, IrUse:
    unreachable
  # These aren't statement level, but should never appear on a LHS,
  # except in the context of a use within an index or parameter,
  # and we do not descend into those.
  of IrLoad, IrBinary, IrBool, IrLogic, IrNot, IrUminus, IrLit:
    unreachable

proc gatherDefSideUses(n: IrNode, uses: var seq[SymbolInfo],
                       nodes: var seq[IrNode], defContext = false) =
  if n == nil:
    return

  case n.contents.kind
  of IrIndex:
    if not defContext:
      if n.contents.toIxSym notin uses and n.contents.toIxSym != nil:
        uses.add(n.contents.toIxSym)
        nodes.add(n)
    n.contents.indexStart.gatherDefSideUses(uses, nodes)
    n.contents.indexEnd.gatherDefSideUses(uses, nodes)
  of IrMember:
    if defContext:
      if n.contents.attrSym notin uses and n.contents.attrSym != nil:
        uses.add(n.contents.attrSym)
        nodes.add(n)
    n.contents.subaccess.gatherDefSideUses(uses, nodes, defContext)
  of IrCall:
    for actual in n.contents.actuals:
      actual.gatherDefSideUses(uses, nodes)
  of IrLoad:
    if n.contents.symbol != nil and n.contents.symbol notin uses:
      uses.add(n.contents.symbol)
      nodes.add(n)
  of IrBinary, IrBool, IrLogic:
    n.contents.bLhs.gatherDefSideUses(uses, nodes)
    n.contents.bRhs.gatherDefSideUses(uses, nodes)
  of IrNot, IrUminus:
    n.contents.uRhs.gatherDefSideUses(uses, nodes)
  of IrLit:
    # This is possible, but a bit crazy.  For example, you
    # could use a dict like so...
    # foo[{"a" : 10}["a"]] = 12
    for item in n.contents.items:
      item.gatherDefSideUses(uses, nodes)
  of IrFold, IrLhsLoad, IrNop:
    return
  of IrBlock, IrSection, IrLoop, IrAttrAssign, IrVarAssign,
     IrConditional, IrJump, IrRet, IrUse:
    unreachable

proc gatherUses(n: IrNode, uses: var seq[SymbolInfo],
                nodes: var seq[IrNode]) =
  if n == nil:
    return

  case n.contents.kind
  of IrLit:
    for item in n.contents.items:
      item.gatherUses(uses, nodes)
  of IrMember:
    if n.contents.attrSym notin uses and n.contents.attrSym != nil:
      uses.add(n.contents.attrSym)
      nodes.add(n)
    n.contents.subaccess.gatherUses(uses, nodes)
  of IrIndex:
    if n.contents.toIxSym notin uses and n.contents.toIxSym != nil:
      uses.add(n.contents.toIxSym)
      nodes.add(n)
    n.contents.indexStart.gatherUses(uses, nodes)
    n.contents.indexEnd.gatherUses(uses, nodes)
  of IrCall:
    for actual in n.contents.actuals:
      actual.gatherUses(uses, nodes)
  of IrNot, IrUminus:
    n.contents.uRhs.gatherUses(uses, nodes)
  of IrBinary, IrBool, IrLogic:
    n.contents.bLhs.gatherUses(uses, nodes)
    n.contents.bRhs.gatherUses(uses, nodes)
  of IrLoad:
    if n.contents.symbol notin uses and n.contents.symbol != nil:
      uses.add(n.contents.symbol)
      nodes.add(n)
  of IrFold, IrNop:
    return
  of IrBlock, IrSection, IrLoop, IrAttrAssign, IrVarAssign,
     IrConditional, IrJump, IrRet, IrUse, IrLhsLoad:
    unreachable

proc gatherDefs(n: IrNode): seq[SymbolInfo] =
  gatherDefs(n, result)

proc gatherUses(useNode: IrNode, defNode: IrNode = nil):
               seq[(SymbolInfo, Con4mNode)] =
  var
    uses:  seq[SymbolInfo]
    nodes: seq[IrNode]

  useNode.gatherUses(uses, nodes)
  if defNode != nil:
    gatherDefSideUses(defNode, uses, nodes, true)

  for i in 0 ..< uses.len():
    result.add((uses[i], nodes[i].parseNode))

proc checkUses(ctx: Module, nRhs: IrNode, nLhs: IrNode, bb: CfgNode) =
  for (sym, node) in gatherUses(nRhs, nLhs):

    if sym.immutable:
      continue

    if sym notin bb.defAtStart and sym notin bb.defInBlock:
      if sym notin bb.errorsInBlock:
        ctx.irNonFatal("UseBeforeDef", @[sym.name], w = node)
        bb.errorsInBlock.add(sym)

template checkUses(ctx: Module, nRhs: IrNode, bb: CfgNode) =
  checkUses(ctx, nRhs, nil, bb)

proc addDefs(ctx: Module, n: IrNode, bb: CfgNode) =
  for sym in gatherDefs(n):
    if sym notin bb.defAtStart and sym notin bb.defInBlock:
      bb.defInBlock.add(sym)

proc extractDefUseFromAssign(ctx: Module, lhs, rhs: IrNode, bb: CfgNode) =
  ctx.checkUses(rhs, lhs, bb)
  ctx.addDefs(lhs, bb)

proc startBlock(n: CfgNode, ir: IrNode, exit: CfgNode = nil): CfgNode =
  var exit = exit

  if n == nil:
    result = CfgNode(irNode: ir)
  else:
    result = CfgNode(pre: @[n], irNode: ir)
    result.defAtStart = n.defAtStart & n.defInBlock
    if n.nextBlock == nil:
      n.nextBlock = result
    elif n.nextBlock != exit:
      n.nextBlock = exit

    n.post.add(result)

  if exit == nil:
    exit = CfgNode(startNode: result)

  result.exitNode  = exit

proc finishBlock(cur: CfgNode, start: CfgNode): bool {.discardable.} =
  if cur != nil:
    if start.exitNode.pre.len() == 0:
      start.exitNode.defAtStart = cur.defAtStart & cur.defInBlock
      var
        newDefs: seq[SymbolInfo] = @[]
        curSet                   = cur.defAtStart & cur.defInBlock

      for item in start.exitNode.defAtStart:
        if item in curSet:
          newDefs.add(item)

      start.exitNode.defAtStart = newDefs

    if cur.nextBlock == nil:
      cur.nextBlock = start.exitNode

    if start.exitNode notin cur.post:
      cur.post.add(start.exitNode)

    if cur notin start.exitNode.pre:
      start.exitNode.pre.add(cur)

    return true

proc handleOneFunc(ctx: CompileCtx, f: FuncInfo, start: seq[SymbolInfo])
proc handleModule(ctx: CompileCtx, m: Module, start: seq[SymbolInfo])

proc hasExitToOuterBlock(loop: CfgNode): bool =
  # TODO: do this
  return true

proc handleOneNode(ctx: CompileCtx, m: Module, n: IrNode,
                   cur: CfgNode): CfgNode =
  result = cur

  if result.startnode != nil or result.exitnode != nil:
    # We got something after either a block start or
    # block end node got inserted into the graph. This goes
    # in what is essentially a 'contents' node, so create
    # that here.
    result = CfgNode(irNode: n, pre: @[cur])
    cur.nextBlock = result
    cur.post.add(result)


  case n.contents.kind
  of IrBlock:
    for i, item in n.contents.stmts:
      if item.contents.kind == IrNop:
        continue
      if item.contents.kind notin [IrBlock, IrSection]:
        result.stmts.add(item)
      if item.tid.getTid() notin [TVoid, TBottom]:
        if item.contents.kind notin [IrAttrAssign, IrVarAssign]:
          m.irWarn("ExprResult", @[item.tid.toString()], w = item.parseNode)

      result = ctx.handleOneNode(m, item, result)

      if result == nil:
        if i + 1 != n.contents.stmts.len():
          m.irWarn("DeadCode", @["statement"], w = item.parseNode)
        return # Skip dead code.

  of IrSection:
    let secStart = result.startBlock(n)
    result = ctx.handleOneNode(m, n.contents.blk, secStart)
    result.finishBlock(secStart)
    result = secStart.exitNode

  of IrLoop:
    m.checkUses(n.contents.startIx, result)
    m.checkUses(n.contents.endIx, result)

    let loopTop = result.startBlock(n)
    loopTop.loopIrNode = n

    # No scope for while loops, which have no loop variable.
    if n.contents.keyVar != "":
      loopTop.defInBlock &= n.contents.scope.allVars()

    m.checkUses(n.contents.condition, loopTop)

    # If the below is false then we will have already warned
    # about a loop that never exits and should be quiet I guess?
    if ctx.handleOneNode(m, n.contents.loopBody, loopTop).finishBlock(loopTop):
      result = loopTop.exitNode

      if n.contents.condition.isConstant():
        let
          asMixed = n.contents.condition.value.get()
          asBool  = toVal[bool](asMixed)
        # If the condition evaluates to true (e.g., while true {}),
        # and there's no path to exit outside this loop, then we will
        # complain.
        if asBool and not loopTop.hasExitToOuterBlock():
          m.irWarn("InfLoop", w = n.contents.condition.parseNode)
          result = nil
    else:
      result = nil

  of IrAttrAssign:
    m.extractDefUseFromAssign(n.contents.attrLhs, n.contents.attrRhs, result)
  of IrVarAssign:
    m.extractDefUseFromAssign(n.contents.varLhs, n.contents.varRhs, result)
  of IrConditional:
    var
      top    = result.startBlock(n)
      joiner = top.exitNode

    m.checkUses(n.contents.predicate, result)

    var
      l    = top.startBlock(n.contents.trueBranch, joiner)
      r    = top.startBlock(n.contents.falseBranch, joiner)
      endl = ctx.handleOneNode(m, n.contents.trueBranch, l)
      retl = endl.finishBlock(l)
      endr = ctx.handleOneNode(m, n.contents.falseBranch, r)
      retr = endr.finishBlock(r)

    l.label = "T"
    r.label = "F"

    if endl != nil:
      endl.nextBlock = nil
    if endr != nil:
      endr.nextBlock = nil

    if not retl and not retr:
      result = nil
    else:
      result = joiner

    assert top.nextBlock == joiner


  of IrJump:
    # Because our jumps are structured, we don't have to worry about
    # the jump point not being in our lineage.  We can just walk
    # ancestors until we find a block holding the target IR node that
    # matches ours, and there is guaranteed to be one.  Note that some
    # ancestor blocks might have multiple 'pre' nodes, meaning one
    # of two things:
    #
    # 1. There  was a conditional that later joined together.  In this
    #    case, we could walk up *any* single one of them, as they are
    #    all contained inside our enclosing loop and will share a common
    #    ancestor.
    # 2. There's an entry point from a break (which might not be ours).
    #    This cannot be a break statement from something lexically below
    #    us, as we will not have processed it yet. And, if it's above us,
    #    it will be enclosed in the same loop. So it's safe to walk up.
    # 3. There's a function call above us; the call graph then links
    #    to that function's call graph, which could have a lot of
    #    blocks in its 'post'.
    #
    # To make sure we handle the last case, whenever we see a function
    # call, we add ourselves as the *first* item in its pre-list.
    # Even that doesn't guarantee that when we do the scan from here,
    # we'll see something at the same level; the same call might
    # happen in a nested block above this node.
    #
    # However, it would be *after* where *we* saw that call, which
    # guarantees it'd be somewhere in an enclosing loop lexically
    # above us, but below the call site. So we'll get there, even
    # though it may be indirect.

    var search = result
    while search.loopIrNode != n.contents.targetNode:
      search = search.pre[0]
    # The post for a break is the end of the loop (the exit node).
    # For a continue, it's the top of the loop.
    if n.contents.exitLoop:
      result.exitType = CFXBreak
      result.post     = @[search.exitNode]
      search.exitNode.pre.add(result)
    else:
      result.exitType = CFXContinue
      if search notin result.post:
        result.post.add(search)
      if result notin search.pre:
        search.pre.add(result)

    result = nil

  of IrRet:
    result.exitType = CFXReturn

    if n.contents.retSym != nil:
      m.checkUses(n.contents.retVal, result)
      if n.contents.retSym notin result.defInBlock:
        result.defInBlock.add(n.contents.retSym)

    # Returns can happen from nested blocks... we jump to the true
    # exit.
    if ctx.topExitNode notin result.post:
      result.post.add(ctx.topExitNode)
    if result notin ctx.topExitNode.pre:
      ctx.topExitNode.pre.add(result)

    result = nil

  of IrCall:
    # Currently, we only check a call's 'pre' state the very first time
    # we run across it in our analysis. This could lead to cases
    # where a single branch controls what is live, and leads to a use-
    # before def problem on module state or function state.
    #
    # My intent is to handle this at some point, by killing stuff from
    # the set of available symbols, then updating the graph.  But it's
    # not done yet, and isn't in my high priority bucket.
    #
    # Note that because we're currently proceeding through this phase
    # even if there are errors, the toCall field might not be set,
    # in which case we will just pretend there was no impact,
    # and the call graph will NOT show this guy.

    if n.contents.toCall == nil:
      return

    # We go off and build the CFG for the function we're calling, if
    # it hasn't already been built. In the case where these things
    # are mutually recursive, the second call will not see any
    # 'post' state.
    #
    # Plus, we only care about new global / module state anyway, so
    # instead of re-analyzing until we find a fixed point, for the
    # moment we will end up just keeping the state we already have,
    # and don't even bother merging in global / module symbols that
    # weren't set when we were called.

    # We currently do not bother to remove symbols that aren't in scope
    # for the function we're calling; it's just junk that will
    # follow around the function, and will not impact any analysis.
    # But we should clean it up someday.
    ctx.handleOneFunc(n.contents.toCall, result.defAtStart & result.defInBlock)

    # When we add ourselves as a predecessor, we want to be first,
    # to ensure that break / continue statements can properly find their
    # ancestor.
    var
      returnTo = CfgNode(pre: @[n.contents.toCall.exitNode],
                         defAtStart: result.defAtStart & result.defInBlock)
      curPost  = n.contents.toCall.exitNode.post

    n.contents.toCall.exitNode.post = @[returnTo] & curPost

    # For when we're scanning *down* a series of statements looking
    # for the end of a block, we'll skip the entire function by
    # leveraging the 'nextBlock' linked list.
    result.nextBlock = returnTo

    # For future analysis (esp fixpoint analysis), when we get to this
    # node, make it easy to know that this block is a call site.
    # We currently are not using this.
    result.exitType = CFXCall

    result = returnTo

  of IrUse:
    # This basically acts identically to function calls; we're just
    # calling the module initialization 'function' (the top level
    # code) when we execute a use statement.
    if n.contents.moduleObj == nil:
      return

    ctx.handleModule(n.contents.moduleObj,
                     result.defAtStart & result.defInBlock)

    var
      returnTo = CfgNode(pre: @[n.contents.moduleObj.exitNode],
                         defAtStart: result.defAtStart & result.defInBlock)
      curPost  = n.contents.moduleObj.exitNode.post

    n.contents.moduleObj.exitNode.post = @[returnTo] & curPost
    result.nextBlock                   = returnTo
    result.exitType                    = CFXUse
    result                             = returnTo

  of IrLit, IrMember, IrIndex, IrNot, IrUminus,
     IrBinary, IrBool, IrLogic, IrLhsLoad, IrLoad, IrFold, IrNop:
       m.checkUses(n, result)

proc handleOneFunc(ctx: CompileCtx, f: FuncInfo, start: seq[SymbolInfo]) =
  # For now, handle recursion by checking to see if the function's cfg
  # field is set. We *should* at this point merge pre's and
  # intelligently analyze. But we'll do that when we get to converting
  # to a SSA form.

  if f.cfg != nil:
    return

  let m = f.defModule

  var top                = startBlock(nil, f.implementation)
  top.defAtStart         = start
  ctx.topExitNode        = top.exitNode

  # When we add default paramers, we will want to check parameters'
  # uses, via the module's def's. For now, we just add them in to the
  # list of already-defined symbols.
  for item in f.params:
    top.defInBlock.add(item.sym)

  ctx.handleOneNode(m, f.implementation, top).finishBlock(top)

proc handleModule(ctx: CompileCtx, m: Module, start: seq[SymbolInfo]) =
  # This can be called recursively, so don't re-process.
  if m.cfg != nil:
    return

  # Param statements execute before the first block.
  m.cfg            = startBlock(nil, m.ir)
  m.cfg.defInBlock = start
  ctx.topExitNode  = m.cfg.exitNode

  for sym in m.moduleScope.allVars():
    # We don't add the default statements to the statement list
    # because they may or may not be executed. We do, however, check
    # to make sure any symbols they use are defined globally.
    if sym.pInfo != nil:
      let irOpt = sym.pInfo.defaultIr
      if irOpt.isSome():
        m.checkUses(irOpt.get(), m.cfg)

  # `handleOneNode` returns the lexically last node, or nil if
  # there was an unconditional jump.
  # `finishBlock` will add the end node if it wasn't a jump, and
  # return true if it adds the block.
  if not ctx.handleOneNode(m, m.ir, m.cfg).finishBlock(m.cfg):
    var where: Con4mNode
    # Warn if the module doesn't exit out the bottom.
    # Right now that'd only happen if it ended w/ a clear infinite loop.
    # This probably doesn't matter if the module is the main module,
    # esp once we mark 'exit' as noreturn and track that.
    if m.ir == nil or m.ir.contents.kind != IrBlock or
       m.ir.contents.stmts.len() == 0:
      where = m.root
    else:
      where = m.ir.contents.stmts[^1].parseNode
    m.irWarn("DoesntExit", @["module"], w = where)

proc buildCfg*(ctx: CompileCtx, module: Module) =
  # Generally, this build the Control Flow Graph for a module under
  # the assumption that it's the entry point (and thus no global
  # variables are set).
  #
  # This *should* automatically pull in:
  # 1) Constants, including module constants.
  # 2) Anything we have statically determined is a constant in that
  #    it only gets assigned once, and to a constant.
  #
  # For #2, I'm not 100% sure that's the right thing; it could be
  # that, dynamically, the value is either the constant OR
  # `undefined`, and their code counts on that. For now, we ignore.
  ctx.handleModule(module, @[])

proc buildAllUnbuiltCfgs*(ctx: CompileCtx, module: Module) =
  # If any of these functions get called from w/in the main body, then
  # they will already be built. We come back so that we can do
  # checking on things that *aren't* yet called when doing our whole
  # program analysis.

  # Here, we currently make the fairly liberal assumption that all
  # vars in the module and global scope will be initialized before
  # calling the function.
  #
  # Eventually, we will do better than this, once we're willing to mix
  # analysis across multiple call sites.
  #
  # In the mean time, it's not much of an issue, because we will
  # also guarantee default values for module / global variables
  # at the start of the program.
  #
  # By the time we get to `ref` and 'maybe' types we will definitely
  # want to do better, as this analysis will effectively become part
  # of the type checking system, where we determine if something is
  # possibly nil. We will be able to start out inferencing types, then
  # any time we see a potential use-before-def, we can change the
  # inference, and force a null check. This will also allow us to have
  # 'undefined' (aka 'bottom') be the true default initialization
  # value for any type.

  for function in module.allFunctions():
    let startDefs = ctx.globalScope.allVars() & module.moduleScope.allVars()
    ctx.handleOneFunc(function, startDefs)

proc prepFirstGraph*(n: CfgNode, i: var int, fns, mods: var seq[CfgNode]) =
  if n == nil or n.nodeId != 0:
    return

  i        = i + 1
  n.nodeId = i

  case n.exitType
  of CFXCall:
    if n.post[0] notin fns:
      fns.add(n.post[0])
    n.nextBlock.prepFirstGraph(i, fns, mods)
  of CFXUse:
    if n.post[0] notin mods:
      mods.add(n.post[0])
    n.nextBlock.prepFirstGraph(i, fns, mods)
  of CFXNormal:
    for item in n.post:
      item.prepFirstGraph(i, fns, mods)
  of CFXStart:
    n.nextBlock.prepFirstGraph(i, fns, mods)
  of CFXBreak, CFXContinue, CFXReturn:
    return

proc fmtCfgLoc(n: Con4mNode, m: Con4mNode = nil): Rope =
  if m == nil:
    return text(" (line " & $(n.token.lineNo) & ") ")
  else:
    return text(" (lines " & $(n.token.lineNo) & " - " &
                           $(m.token.lineNo) & ") ")

proc getSeqDisplayKids(entry: CfgNode): seq[CfgNode] =
  var cur = entry

  while cur != nil:
    result.add(cur)
    cur = cur.nextBlock

proc cfgRopeWalk(cur: CfgNode): (Rope, seq[CfgNode]) =
  var
    r: Rope = em($(cur.nodeId)) + text(" ")
    kids: seq[CfgNode]

  if cur.exitType == CFXStart:
    r    = em("Root")
    kids = cur.nextBlock.getSeqDisplayKids()

    return (r, kids)

  if cur.loopIrNode != nil:
    r += fgColor("Loop", "atomiclime")

    if cur.loopIrNode.contents.label != nil:
      r += strong(cur.loopIrNode.contents.label.getText()) + text(" ")
    r += cur.loopIrNode.parseNode.fmtCfgLoc()
  elif cur.label != "":
    r += fgColor(cur.label, "atomiclime")
  elif cur.exitNode != nil:
    if cur.post.len() == 2:
      r += fgColor("If", "atomiclime")
    else:
      r += fgColor("Start", "atomiclime")
  elif cur.startNode != nil:
    if cur.startNode.post.len() <= 1:
      r += fgColor("End", "atomiclime")
    else:
      r += fgColor("Join", "atomiclime")
  else:
    r += fgColor("Code",  "atomiclime")
    case cur.stmts.len()
    of 0:
      if cur.irNode != nil:
        r += cur.irNode.parseNode.fmtCfgLoc()
    of 1:
      r += cur.stmts[0].parseNode.fmtCfgLoc()
    else:
      let
        firstNode = cur.stmts[0].parseNode
        lastNode  = cur.stmts[^1].parseNode
      r += fmtCfgLoc(firstNode, lastNode)

  case cur.exitType
  of CfxUse:
    let module = cur.irNode.parseNode.children[0].getText()
    r += fgcolor(" -> runs: " & module, "atomiclime")
  of CfxCall:
    let fname = cur.irNode.parseNode.children[0].getText()
    r += fgcolor(" -> calls: " & fname, "atomiclime")
  of CfxBreak:
    r += fgcolor(" -> breaks to #" & $(cur.post[0].nodeId), "atomiclime")
  of CfxContinue:
    r += fgcolor(" -> continues to #" & $(cur.post[0].nodeId), "atomiclime")
  of CfxReturn:
    r += fgcolor(" -> exit to #" & $(cur.post[0].nodeId), "atomiclime")
  else:
    if cur.post.len() == 1 and cur.post[0] != cur.nextBlock:
      r += fgcolor(" -> #" & $(cur.post[0].nodeId), "atomiclime")

  if cur.startNode != nil:
    r += fgColor(" for #" & $(cur.startNode.nodeId), "fandango")

  elif cur.exitNode != nil:
    r += fgColor(" to #" & $(cur.exitNode.nodeId), "fandango")

    if cur.post.len() > 1:
      for item in cur.post:
        item.loopTop = true
        kids.add(item)

  if cur.loopTop:
    kids = cur.nextBlock.getSeqDisplayKids()

  return (r, kids)


# DONT FORGET ABOUT THE TODO ITEM
proc toRope*(n: CfgNode, isModule: bool, inclCalls = true): Rope =
  var
    lastNum = 0
    fns:  seq[CfgNode]
    mods: seq[CfgNode]
    dummy = CfgNode(exitType: CFXStart, nextBlock: n, post: @[n])

  n.label          = "Enter"
  n.exitNode.label = "Exit"
  prepFirstGraph(n, lastnum, fns, mods)
  return dummy.quickTree(cfgRopeWalk)
