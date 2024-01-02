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

proc addUse(ctx: Module, sym: SymbolInfo, n: IrNode, bb: CfgNode) =
  if sym == nil or sym.immutable or sym in bb.defAtStart or
     sym in bb.defInBlock or sym in bb.errorsInBlock:
    return

  if not sym.isAttr or not sym.hasDefault:
    ctx.irNonFatal("UseBeforeDef", w = n, @[sym.name])
    bb.errorsInBlock.add(sym)

  if n notin sym.uses:
    sym.uses.add(n)

proc addDef(sym: SymbolInfo, n: IrNode, bb: CfgNode) =
  if sym == nil:
    return
  if sym notin bb.defAtStart and sym notin bb.defInBlock:
    bb.defInBlock.add(sym)

  if n notin sym.defs:
    sym.uses.add(n)

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

proc getSectionLabel(n: IrNode): string =
  let s = n.contents

  result = "Section "
  if s.prefix != "":
    result &= s.prefix & "."

  result &= s.sectName

  if s.instance != "":
    result &= s.instance

proc handleOneNode(ctx: CompileCtx, m: Module, n: IrNode,
                   cur: CfgNode): CfgNode =
  result = cur

  if n == nil:
    return

  if n.scope != nil:
    n.scope.calculateOffsets()

    if m.definingFn != nil:
      if n.scope.scopeSize > m.definingFn.maxOffset:
        m.definingFn.maxOffset = n.scope.scopeSize
    else:
      if n.scope.scopeSize > m.maxOffset:
        m.maxOffset = n.scope.scopeSize

  if result.startnode != nil or result.exitnode != nil:
    # We got something after either a block start or
    # block end node got inserted into the graph. This goes
    # in what is essentially a 'contents' node, so create
    # that here.
    #
    # The exception is if we're def about to create a new block
    # ourselves, which only happens for sections; loops and
    # conditionals get blocks made for them.
    if n.contents.kind != IrSection:
      let defs = result.defAtStart
      result = CfgNode(irNode: n, pre: @[cur])
      result.defAtStart = defs
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
          m.irWarn("ExprResult", item, @[item.tid.toString()])

      result = ctx.handleOneNode(m, item, result)

      if result == nil:
        if i + 1 != n.contents.stmts.len():
          m.irWarn("DeadCode", item, @["statement"])
        return # Skip dead code.

  of IrSection:
    let secStart = result.startBlock(n)
    result       = ctx.handleOneNode(m, n.contents.blk, secStart)
    result.finishBlock(secStart)
    result         = secStart.exitNode
    secstart.label = n.getSectionLabel()

  of IrLoop:
    let loopTop = result.startBlock(n)
    loopTop.loopIrNode = n

    # No scope for while loops, which have no loop variable.
    loopTop.defInBlock &= n.contents.loopVars

    let passDown = ctx.handleOneNode(m, n.contents.condition, loopTop)

    # If the below is false then we will have already warned
    # about a loop that never exits and should be quiet I guess?
    if ctx.handleOneNode(m, n.contents.loopBody, passDown).finishBlock(loopTop):
      result = loopTop.exitNode

      if n.contents.condition != nil and n.contents.condition.isConstant():
        let
          asMixed = n.contents.condition.value.get()
          asBool  = cast[bool](asMixed)
        # If the condition evaluates to true (e.g., while true {}),
        # and there's no path to exit outside this loop, then we will
        # complain.
        if asBool and not loopTop.hasExitToOuterBlock():
          m.irWarn("InfLoop", w = n.contents.condition)
          result = nil
    else:
      result = nil

  of IrAttrAssign:
    result = ctx.handleOneNode(m, n.contents.attrRhs, result)
    m.lhsContext = true
    result = ctx.handleOneNode(m, n.contents.attrLhs, result)
  of IrVarAssign:
    result = ctx.handleOneNode(m, n.contents.varRhs, result)
    m.lhsContext = true
    result = ctx.handleOneNode(m, n.contents.varLhs, result)
  of IrRange:
    result = ctx.handleOneNode(m, n.contents.rangeStart, result)
    result = ctx.handleOneNode(m, n.contents.rangeEnd, result)

  of IrConditional:
    var
      top    = result.startBlock(n)
      joiner = top.exitNode

    result = ctx.handleOneNode(m, n.contents.predicate, result)

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

  of IrSwitch:
    var
      top    = result.startBlock(n)
      joiner = top.exitNode

    if n notin n.contents.targetSym.uses:
      n.contents.targetSym.uses.add(n)

    if not n.contents.typeCase:
      result = ctx.handleOneNode(m, n.contents.switchTarget, result)

    for branch in n.contents.branches:
      var
        cfblock = top.startBlock(branch, joiner)
        cfend   = ctx.handleOneNode(m, branch, cfblock)
        cfret   = cfend.finishBlock(cfblock)

      if cfend != nil:
        cfend.nextBlock = nil

    result = joiner

  of IrSwitchBranch:
    for i, item in n.contents.conditions:
      result = ctx.handleOneNode(m, item, result)

    result = ctx.handleOneNode(m, n.contents.action, result)

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
      if search.pre.len() == 0:
        result = nil
        return
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
    result = ctx.handleOneNode(m, n.contents.retVal, result)

    result.exitType = CFXReturn

    n.contents.retSym.addDef(n, result)

    # Returns can happen from nested blocks... we jump to the true
    # exit.
    if ctx.topExitNode notin result.post:
      result.post.add(ctx.topExitNode)
    if result notin ctx.topExitNode.pre:
      ctx.topExitNode.pre.add(result)

    result = nil

  of IrCall:
    for actual in n.contents.actuals:
      result = ctx.handleOneNode(m, actual, result)
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
    # even if there are errors, we pretend the call doesn't happen
    # and the call graph will NOT show this guy.
    if n.contents.toCall == nil:
      return

    # For external functions, we create dummy entry and exit nodes
    # here.
    if n.contents.toCall.externInfo != nil:
      let
        dummyStart = CfgNode()
        dummyEnd   = CfgNode(pre: @[dummyStart])

      n.contents.toCall.cfg       = dummyStart
      dummyStart.exitNode         = dummyEnd
      dummyEnd.startNode          = dummyStart
      n.contents.toCall.cfg       = dummyStart
      n.contents.toCall.exitNode  = dummyEnd

    result.irNode = n

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

  of IrMember:
    if m.lhsContext:
      n.contents.attrSym.addDef(n, result)
    else:
      m.addUse(n.contents.attrSym, n, result)
    result = ctx.handleOneNode(m, n.contents.subaccess, result)
  of IrIndex:
    let savedLhs = m.lhsContext
    m.lhsContext = false
    result = ctx.handleOneNode(m, n.contents.indexStart, result)
    result = ctx.handleOneNode(m, n.contents.indexEnd, result)
    m.lhsContext = savedLhs
    result = ctx.handleOneNode(m, n.contents.toIx, result)
  of IrNot, IrUMinus:
    result = ctx.handleOneNode(m, n.contents.uRhs, result)
  of IrBinary, IrBool, IrLogic:
    result = ctx.handleOneNode(m, n.contents.bLhs, result)
    result = ctx.handleOneNode(m, n.contents.bRhs, result)
  of IrLoad:
    m.addUse(n.contents.symbol, n, result)
  of IrLhsLoad:
    n.contents.symbol.addDef(n, result)
  of IrLit, IrFold, IrNop, IrNil:
    discard

proc handleOneFunc(ctx: CompileCtx, f: FuncInfo, start: seq[SymbolInfo]) =
  # For now, handle recursion by checking to see if the function's cfg
  # field is set. We *should* at this point merge pre's and
  # intelligently analyze. But we'll do that when we get to converting
  # to a SSA form.

  if f.cfg != nil or f.externInfo != nil:
    return

  f.fnScope.calculateOffsets()
  f.maxOffset = f.fnScope.scopeSize # This can get bigger as we descend.

  let m = f.defModule

  var top                = startBlock(nil, f.implementation, CfgNode())
  top.defAtStart         = start
  ctx.topExitNode        = top.exitNode
  f.exitNode             = top.exitNode
  f.cfg                  = top

  # When we add default parameters, we will want to check parameters'
  # uses, via the module's def's. For now, we just add them in to the
  # list of already-defined symbols.
  for item in f.params:
    top.defAtStart.add(item.sym)

  ctx.handleOneNode(m, f.implementation, top).finishBlock(top)

proc handleModule(ctx: CompileCtx, m: Module, start: seq[SymbolInfo]) =
  # This can be called recursively, so don't re-process.
  if m.cfg != nil:
    return

  m.moduleScope.calculateOffsets()
  if m.globalScope.scopeSize == 0:
    m.globalScope.calculateOffsets()

  # Param statements execute before the first block.
  m.cfg            = startBlock(nil, m.ir, CfgNode())
  m.cfg.defInBlock = start
  ctx.topExitNode  = m.cfg.exitNode

  var cur = m.cfg

  for sym in m.moduleScope.allVars():
    # We don't want to add the default statements to the statement
    # list because they may or may not be executed. We do, however,
    # check to make sure any symbols they use are defined globally.
    # the code isn't in a statement block, it won't get added to a
    # statement list by handleOneNode.
    if sym.pInfo != nil:
      let irOpt = sym.pInfo.defaultIr
      if irOpt.isSome():
        cur = ctx.handleOneNode(m, irOpt.get(), cur)

  # `handleOneNode` returns the lexically last node, or nil if
  # there was an unconditional jump.
  # `finishBlock` will add the end node if it wasn't a jump, and
  # return true if it adds the block.
  if not ctx.handleOneNode(m, m.ir, cur).finishBlock(m.cfg):
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

  #for function in module.allFunctions():
  #  let startDefs = ctx.globalScope.allVars() & module.moduleScope.allVars()
  #  ctx.handleOneFunc(function, startDefs)

  #for m in module.imports:
  #  let startDefs = ctx.globalScope.allVars() & module.moduleScope.allVars()
  #  ctx.handleModule(m, startDefs)

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

    module.definingFn = function
    ctx.handleOneFunc(function, startDefs)
    module.definingFn = nil

proc getSeqDisplayKids(entry: CfgNode): seq[CfgNode] =
  if entry == nil:
    return

  var
    cur   = entry.nextBlock
    last  = entry.exitNode
    depth = 0

  while cur != last and cur != nil:
    if depth == 0:
      result.add(cur)
    if cur.exitNode != nil:
      depth += 1
    elif cur.startNode != nil:
      depth -= 1
      if depth == 0:
        result.add(cur)

    cur = cur.nextBlock

proc prepGraph(n: CfgNode, i: var int, fns: var seq[FuncInfo],
                mods: var seq[Module]) =
  var kids: seq[CfgNode]

  if n == nil or n.nodeId != 0:
    return

  i        = i + 1
  n.nodeId = i

  case n.exitType
  of CFXStart:
    n.nextBlock.prepGraph(i, fns, mods)
    n.nextBlock.exitNode.prepGraph(i, fns, mods)
  of CFXCall:
    if n.irNode.contents.toCall notin fns:
      fns.add(n.irNode.contents.toCall)
  of CFXUse:
    let m = n.post[0].irNode.contents.moduleObj
    if m notin mods:
      mods.add(m)
  else:
    discard

  if n.exitNode != nil:
    if n.post.len() > 1:
      kids.add(n.post)
    else:
      kids = n.getSeqDisplayKids()
    #if n.exitType notin [CfxCall, CfxUse]:
    #  n.exitNode.prepGraph(i, fns, mods)

  for item in kids:
    item.prepGraph(i, fns, mods)

  n.nextBlock.prepGraph(i, fns, mods)

proc fmtCfgLoc(n: Con4mNode, m: Con4mNode = nil): Rope =
  if m == nil:
    return text(" (line " & $(n.token.lineNo) & ") ")
  else:
    return text(" (lines " & $(n.token.lineNo) & " - " &
                           $(m.token.lineNo) & ") ")

proc cfgRopeWalk(cur: CfgNode): (Rope, seq[CfgNode]) =
  var
    r: Rope = em($(cur.nodeId)) + text(" ")
    kids: seq[CfgNode]

  if cur.exitType == CFXStart:
    r    = em("Root")
    kids = @[cur.nextBlock, cur.nextBlock.exitNode]
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
    r += fgcolor("-> runs: " & module, "atomiclime")
  of CfxCall:
    let fname = cur.irNode.contents.toCall.name
    r += fgcolor("-> calls: " & fname, "atomiclime")
  of CfxBreak:
    r += fgcolor("-> breaks to #" & $(cur.post[0].nodeId), "atomiclime")
  of CfxContinue:
    r += fgcolor("-> continues to #" & $(cur.post[0].nodeId), "atomiclime")
  of CfxReturn:
    r += fgcolor("-> exit to #" & $(cur.post[0].nodeId), "atomiclime")
  else:
    if cur.post.len() == 1 and cur.post[0] != cur.nextBlock:
      r += fgcolor("-> #" & $(cur.post[0].nodeId), "atomiclime")

  if cur.startNode != nil:
    r = fgColor("↑" & $(cur.startNode.nodeId) & " ", "fandango") + r

  elif cur.exitNode != nil:
    r = fgColor("↓" & $(cur.exitNode.nodeId) & " ", "fandango") + r

    if cur.post.len() > 1:
      kids.add(cur.post)
    else:
      kids = cur.getSeqDisplayKids()

  let innum  = $cur.defAtStart.len()
  let outnum = $(cur.defAtStart.len() + cur.defInBlock.len())
  r = r + text(" in: " & innum & "; out: "  & outnum)
  return (r, kids)

proc toRope*(n: CfgNode, isModule: bool, inclCalls = true): Rope =
  var
    lastNum = 0
    fns:   seq[FuncInfo]
    mods:  seq[Module]
    mouts: seq[Rope]
    fouts: seq[Rope]
    main:  Rope
    dummy = CfgNode(exitType: CfxStart, nextBlock: n, post: @[n])
    mname: string
    i: int

  n.label          = "Enter"
  n.exitNode.label = "Exit"
  prepGraph(n, lastnum, fns, mods)
  main = dummy.quickTree(cfgRopeWalk)

  while i < mods.len():
    let module = mods[i]
    i += 1
    prepGraph(module.cfg, lastnum, fns, mods)
    prepGraph(module.exitNode, lastnum, fns, mods)
    dummy = CfgNode(exitType: CfxStart, nextBlock: module.cfg,
                    post: @[module.cfg])
    mouts.add(dummy.quickTree(cfgRopeWalk))

  i = 0
  while i < fns.len():
    let fn = fns[i]
    i += 1
    prepGraph(fn.cfg, lastnum, fns, mods)
    prepGraph(fn.exitNode, lastnum, fns, mods)
    dummy = CfgNode(exitType: CfxStart, nextBlock: fn.cfg, post: @[fn.cfg])
    fouts.add(dummy.quickTree(cfgRopeWalk))

  if mouts.len() != 0 or fouts.len() != 0:
    result = h1("Entry point:")

  result += main

  for i, mo in mouts:
    let m = mods[i]
    if m.where.len() != 0:
      mname = m.modname & " (from: " & m.where & ")"
    else:
      mname = m.modname

    result += h2("Module: " & mname)
    result += mo

  for i, fo in fouts:
    let f = fns[i]
    result += h3("Function: " & f.name & f.tid.toString())
    result += fo
