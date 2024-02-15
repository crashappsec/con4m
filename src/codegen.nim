# This assumes the stack grows toward smaller addresses, so that
# we can call arguments by offsetting into the stack the proper
# number of arguments.

# To support being more dynamic, we give each module its own
# independent heap, instead of laying things out into just one
# heap. That's probably not necessary, but whatever.

# Note that we don't expect to see generic types loaded EVER. When
# functions are called, the prologue binds what should be concrete
# types to the variables.

# Currently, the compiler is NOT enforcing that top-level (and
# module-level variables) have a generic element to their type.
# For the time being, that should be a runtime error.

# That's because we intend to do a REPL, where it will be necessary to
# defer typing things at the top-level; this will be a subtlety we
# deal with when we get to doing the REPL.

import "."/irgen
import ztypes/api

proc findAndLoadModule(ctx: CompileCtx, location, fname, ext: string):
                      Option[Module] {.importc, cdecl.}

type
  CodeGenState = ref object
    minfo:           Dict[string, Module]
    cc:              CompileCtx
    zobj:            ZObjectFile
    mcur:            Module
    fcur:            FuncInfo
    curNode:         IrNode
    callBackpatches: seq[(FuncInfo, Module, int)]
    strCache:        Dict[string, int]
    memos:           Memos

var tcallReverseMap = ["repr()", "cast()", "==", "<", ">", "+", "-",
                       "*", "/", "//", "%", "<<", ">>", "&", "|", "^",
                       "[]", "dict[]", "[:]", "[]=", "dict[]=",
                       "[:]=", "loadlit()", "containerlit()", "copy_object()",
                       "len()", "+=", "ffi()", "init()", "cleanup()",
                       "newlit()", ">max<"]

proc toString*(ins: ZInstruction): string =
  result = $ins & ": " & ins.typeInfo.toString()

  if ins.op == ZTCall:
    result &= " (" & tcallReverseMap[ins.arg] & ")"


proc oneIrNode(ctx: CodeGenState, n: IrNode)

template getOffset(sym: SymbolInfo): int64 =
  int64(sym.offset)

proc addStaticObject(ctx: CodeGenState, p: pointer, l: int): int =

  let arr = cast[cstring](p)

  if l == 0:
    return 0

  result = ctx.zobj.staticData.len()

  for i in 0 ..< l:
    ctx.zobj.staticData.add(arr[i])

  # Let's 64-bit align.
  let overage = ctx.zobj.staticData.len() mod 8
  for i in 0 ..< ((8 - overage) mod 8):
    ctx.zobj.staticData.add('\x00')
  #dealloc(p)

proc addStaticObject(ctx: CodeGenState, s: string): int =
  if s.len() == 0:
    return 0

  let cacheOpt = ctx.strCache.lookup(s)
  if cacheOpt.isSome():
    return cacheOpt.get()

  result = ctx.zobj.staticData.len()

  ctx.zobj.staticData &= s
  ctx.zobj.staticData.add('\x00')

  let overage = (ctx.zobj.staticData.len() + 1) mod 8

  for i in 0 ..< ((8 - overage) mod 8):
    ctx.zobj.staticData.add('\x00')

  ctx.strCache[s] = result

proc getLocation(ctx: CodeGenState): int32 =
  let n = ctx.curNode
  if n == nil or n.parseNode == nil or n.parseNode.token == nil:
    return -1

  return int32(n.parseNode.token.lineNo)

proc emitInstruction(ctx: CodeGenState, op: ZOp, arg: int = 0,
                    immediate: int64 = 0,  tid: TypeId = TBottom,
                    moduleId: int = ctx.mcur.objInfo.moduleId) =
  var ins = ZInstruction(op: op, moduleId: int16(moduleId), arg: int32(arg),
                         immediate: immediate, lineNo: ctx.getLocation(),
                         typeInfo: tid.getTid())
  ctx.mcur.objInfo.instructions.add(ins)

proc findStringAt*(mem: string, offset: int): string =
  let endIx = mem.find('\0', offset)

  return mem[offset ..< endIx]

proc hex(x: int, minlen = 2): string =
  let bitlen = int(64 - clzll(cast[uint](x)))
  var outlen = ((bitlen + 7) div 8) * 2

  if outlen < minlen:
    outlen = minlen

  return "0x" & `$`(x.toHex(outlen).toLowerAscii())

proc rawReprInstructions*(module: ZModuleInfo, ctx: ZObjectFile, fn = 0): Rope =
  # This is really just for use during development.
  # Will do a more proper disassembler at some point.
  var
    mem = ctx.staticData
    cells: seq[seq[Rope]] = @[@[atom("Address"), atom("Op"), atom("Arg 1"),
                                atom("Arg 2"), atom("Type Info"), atom("MID"),
                                atom("Comment / label")]]
    row: seq[Rope]


  for i, item in module.instructions:
    var
      address = text(hex(i * sizeof(ZInstruction), 8))
      arg1 = text(" ")
      arg2 = text(" ")
      ty   = text(cast[TypeId](item.typeInfo).toString())
      lno  = item.lineNo
      mid  = text($(item.moduleId))
      lbl: Rope = nil

    if item.typeInfo == TBottom:
      ty = text("none")

    case item.op
    of ZNop:
      if item.arg != 0:
        var str = mem.findStringAt(item.immediate)

        if str.len > 30:
          str = str[0 .. 14] & " … " & str[^14 .. ^1]

        ty  = text(" ")
        lbl = strong(str)
    of ZLoadFromAttr:
      if item.arg != 0:
        # Right now, we do this when a container is getting indexed,
        # because assignment always expects us to push the storage
        # address, even if we're going to assign to an index, not an
        # entire object.
        #
        # This is only needed for index assignment right now, and
        # should be fixed by not requiring this, and not generating an
        # needless ref when we are going to end up taking an index.
        arg1 = text("& (new ref)")
    of ZFFICall:
      let p = addr ctx.staticData[ctx.ffiInfo[item.arg].nameOffset]
      arg1 = text(hex(item.arg))
      lbl  = em(" ffi(" & $(cast[cstring](p)) & ")")
    of ZTCall:
      arg1 = text(hex(item.arg))
      lbl  = em(tCallReverseMap[item.arg])
    of Z0Call:
      # Look up the symbol name by moduleId. It's always going to be a module,
      # so it's really about indexing into the right module in the
      # zobject state, which is offset by one, since moduleId 0 is the
      # global namespace.
      if item.arg <= 0:
        let moduleIx = item.moduleId - 1
        arg1 = text($(moduleIx))
        arg2 = text("module: " & ctx.moduleContents[moduleIx].modname)
      else:
        let
          moduleIx = item.moduleId - 1
          fnObj    = ctx.funcInfo[item.arg - 1]
          name     = ctx.moduleContents[moduleIx].codesyms[fnObj.offset]

        arg1 = text(hex(item.arg))
        arg2 = text("module id: " & $item.moduleId)
        lbl = em("-> " & name)
    of ZPushVal, ZPushPtr, ZPushSType, ZPushAddr:
      var
        prefix, vname: string

      if item.arg > 0:
        arg1 = text("+" & hex(item.arg))
      else:
        arg1 = text("-" & hex(-item.arg))

      if item.moduleId == -1:
        vname = ctx.funcInfo[fn].syms.lookup(item.arg).getOrElse("($...)")
      elif item.moduleId != 0:
        vname = module.datasyms.lookup(item.arg).getOrElse("($...)")
      else:
        vname = ctx.globals.lookup(item.arg).getOrElse("($...)")

      if item.op == ZPushAddr:
        prefix = "&"

      lbl = text(prefix & vname & " -> (stack)")
    of ZStoreTop, ZStoreImm:
      var
        tmp:     string
        tmpOpt:  Option[string]
        dstname: string = "($...)"

      arg1 = text("+" & hex(item.arg))

      if item.op in [ZStoreImm]:
        tmp  = $(item.immediate)
        arg2 = text(tmp)
      else:
        tmp = "(stack)"

      if item.moduleId == -1:
        tmpOpt = ctx.funcInfo[fn].syms.lookup(item.arg)
      elif item.moduleId != 0:
        tmpOpt = module.datasyms.lookup(item.arg)
      else:
        tmpOpt = ctx.globals.lookup(item.arg)

      if tmpOpt.isSome():
        dstName = tmpOpt.get()

      lbl = text(tmp & " -> " & dstName)
    of ZPushStaticPtr:
      arg1 = text("+" & hex(item.arg))
      lbl  = text(mem.findStringAt(item.arg))
    of ZPushRes:
      lbl = text("(ret) -> (stack)")
    of ZSetRes:
      arg1 = text("+" & hex(item.arg))
      lbl = text("result -> (ret)")
    of ZPushImm:
      arg2 = text($item.immediate)
      lbl  = text(`$`(item.immediate) & " -> (stack)")
    of ZAssignToLoc:
      lbl = text("popx2")
    of ZAssignAttr:
      lbl = text("popx2")
      if item.arg != 0:
        arg1 = text("+lock")
    of ZLockOnWrite:
      lbl = text("pop")
    of ZModuleRet , ZRet, ZHalt:
      ty = text(" ")
    of ZMoveSp:
      ty = text(" ")
      arg1 = text($item.arg)
    of ZJz, ZJnz, ZJ:
      if item.arg > 0:
        arg1 = text("+" & hex(item.arg))
      else:
        arg1 = text("-" & hex(-item.arg))
      lbl  = italic("-> " & hex(item.arg + (i * sizeof(ZInstruction))))
      ty = text(" ")

      if item.op != ZJ:
        lbl += text(" + pop")
    of ZSObjNew:
      arg1 = text($(item.arg))
      arg2 = text("+" & hex(item.immediate))

      var
        p = cast[cstring](addr ctx.staticData[int(item.immediate)])
        o = c4m_unmarshal(p, item.typeInfo)
        s = $(call_repr(o, cast[TypeId](item.typeInfo)))

      if "\n" in s:
        s = s.split("\n")[0]
      if s.len() > 20:
        s = s[0 ..< 18] & "..."

        lbl = em(s)
      else:
        lbl = em(s)
    of ZPop, ZAssert, ZSwap:
      ty = text(" ")
    of ZPushFfiPtr, ZPushVmPtr:
      arg1 = text(hex(item.arg))
      arg2 = em(ctx.staticData.findStringAt(int(item.immediate)))
    else:
      discard

    if lbl == nil or lbl.runeLength() == 0:
      lbl = text(" ")
    row = @[address, em($(item.op)), arg1, arg2, ty, mid, lbl]

    cells.add(row)

  result = quickTable(cells)
  result.colWidths([(12, true), (16, true), (14, true), (16, true),
                    (0, false), (0, false), (0, false)])

proc disassembly*(ctx: ZObjectFile, skip_nops = true): Rope =
    for item in ctx.moduleContents:
      if item.instructions.len() == 3 and skip_nops:
        if item.instructions[0].op == ZNop and
           item.instructions[1].op == ZModuleRet and
           item.instructions[2].op == ZNop:
          continue
      result = result + rawReprInstructions(item, ctx)

proc getSymbolArena(ctx: CodeGenState, sym: SymbolInfo): int =
  var sym = sym

  while sym.actualSym != nil:
    sym = sym.actualSym

  if sym.moduleId == 0 and not sym.global:
    # When we did our original scope scan on a function or module, we
    # didn't descend into block scopes.
    #
    # So here we we know the temporary is getting used, so ensure
    # there is name information for block scopes, but note that there
    # can be multiple blocks sharing the same memory cells on the
    # stack; right now, we just call any such variable "(tmp)"; we can
    # do something more sophisticated later...
    if ctx.fcur != nil:
      ctx.fcur.objInfo.syms[sym.offset] = "tmp@" & $(sym.offset)
    else:
      ctx.mcur.objInfo.datasyms[sym.offset] = "tmp@" & $(sym.offset)

    if sym.inFunc:
      return -1
    else:
      ctx.mcur.objInfo.moduleId
  else:
    return sym.moduleId

proc codeLoc(ctx: CodeGenState): int =
  return ctx.mcur.objInfo.instructions.len()

## Opcode generation
proc genPop(ctx: CodeGenState, sym: SymbolInfo = nil,
            tid: TypeId = TBottom) =
  var ntid: TypeId = tid.getTid()

  ## Pop should be as much as we know about the type we're popping.
  if sym == nil:
    ctx.emitInstruction(ZPop, tid = ntid)
  else:
    let moduleId = ctx.getsymbolArena(sym)

    if ntid == TBottom:
      ntid = sym.getTid()

    ctx.emitInstruction(ZStoreTop, sym.getOffset(), tid = ntid,
                        moduleId = moduleId)
    ctx.emitInstruction(ZPop, tid = ntid)

proc genNop(ctx: CodeGenState) =
  ctx.emitInstruction(ZNop)

proc genSwap(ctx: CodeGenState) =
  ctx.emitInstruction(ZSwap)

proc genUnmarshalStaticObject(ctx: CodeGenState, offset: int,
                         l: int, tid: TypeId) =
  ctx.emitInstruction(ZSObjNew, l, offset, tid, moduleId = -2)

proc genLabel(ctx: CodeGenState, label: string) =
  ctx.emitInstruction(ZNop, 1, ctx.addStaticObject(label))

proc genPush(ctx: CodeGenState, sym: SymbolInfo) =
  var
    moduleId = ctx.getSymbolArena(sym)
    op:   ZOp

  if sym.tid.isValueType():
    op = ZPushVal
  else:
    op = ZPushPtr

  ctx.emitInstruction(op, sym.getOffset(), tid = sym.getTid(),
                      moduleId = moduleId)

proc genPushImmediate(ctx: CodeGenState, immediate: int, tid = TInt) =
  ctx.emitInstruction(ZPushImm, immediate = immediate, tid = tid.getTid())

proc genPushImmediateF(ctx: CodeGenState, val: float) =
  let p = cast[pointer](val)
  ctx.emitInstruction(ZPushImm, immediate = cast[int](p), tid = TFloat)

proc genPushTypeOf(ctx: CodeGenState, sym: SymbolInfo) =
  ctx.emitInstruction(ZPushSType, sym.getOffset(),
                     moduleId = ctx.getSymbolArena(sym))

proc genDupTop(ctx: CodeGenState) =
  ctx.emitInstruction(ZDupTop)

proc genStore(ctx: CodeGenState, sym: SymbolInfo, tid = sym.tid) =
  ctx.emitInstruction(ZStoreTop, sym.getOffset(), tid = tid.getTid(),
                     moduleId = ctx.getSymbolArena(sym))

proc genStoreImmediate(ctx: CodeGenState, sym: SymbolInfo,
                       immediate: int, tid = sym.tid) =
  ctx.emitInstruction(ZStoreImm, sym.getOffset(), immediate, tid = tid.getTid(),
                     moduleId = ctx.getSymbolArena(sym))

proc genTCall(ctx: CodeGenState, callId: int, tid: TypeId = TBottom) =
  var dtid: TypeId

  if tid != TBottom:
    dtid = tid.getDataType().dtid

  # This is for generating calls to our internal API for data types.
  # Push parameters on last to first.
  ctx.emitInstruction(ZTCall, callId, tid = tid)

proc genNot(ctx: CodeGenState) =
  ctx.emitInstruction(ZNot)

proc genEqual(ctx: CodeGenState, tid = TBool) =
  ctx.genTCall(FEq, tid)

proc genAdd[T, V](ctx: CodeGenState, lhs: T, rhs: V, tid: TypeId) =
  when T is SymbolInfo:
    ctx.genPush(lhs)
  else:
    ctx.genPushImmediate(lhs)

  when V is SymbolInfo:
    ctx.genPush(rhs)
  else:
    ctx.genPushImmediate(rhs)

  ctx.genTCall(FAdd, tid)

proc startJz(ctx: CodeGenState, target: IrNode = nil,
             popit = true): int {.discardable.} =
  # Returns the ABSOLUTE offset of this instruction; you need to
  # then calculate an offset.
  result = ctx.mcur.objInfo.instructions.len()

  if not popit:
    ctx.emitInstruction(ZDupTop)

  ctx.emitInstruction(ZJz)

  if target != nil:
    ctx.mcur.backpatchLocs.add((target, result))

proc startJnz(ctx: CodeGenState, target: IrNode = nil,
             popit = true): int {.discardable.} =

  result = ctx.mcur.objInfo.instructions.len()

  if not popit:
    ctx.emitInstruction(ZDupTop)

  ctx.emitInstruction(ZJnz)

  if target != nil:
    ctx.mcur.backpatchLocs.add((target, result))

proc startJ(ctx: CodeGenState, target: IrNode = nil): int {.discardable.} =

  result = ctx.mcur.objInfo.instructions.len()
  ctx.emitInstruction(ZJ)

  if target != nil:
    ctx.mcur.backpatchLocs.add((target, result))

proc getJumpOffset(jAddr, targetAddr: int): int32 =
  return int32((targetAddr - jAddr) * sizeof(ZInstruction))

proc genBackwardsJump(ctx: CodeGenState, target: IrNode) =
  let curAddr = ctx.mcur.objInfo.instructions.len()

  for (n, loc) in ctx.mcur.loopLocs:
    if n == target:
      ctx.emitInstruction(ZJ, arg = getJumpOffset(curAddr, loc))
      return

  unreachable

proc genBackwardsJump(ctx: CodeGenState, target: int) =
  let curAddr = ctx.mcur.objInfo.instructions.len()
  ctx.emitInstruction(ZJ, arg = getJumpOffset(curAddr, target))

proc backPatch(ctx: CodeGenState, patchLoc: int) =
  var cur = ctx.mcur.objInfo.instructions[patchLoc]

  cur.arg = getJumpOffset(patchLoc, ctx.codeLoc())

  ctx.mcur.objInfo.instructions[patchLoc] = cur

proc genContainerIndex(ctx: CodeGenState, sym: SymbolInfo = nil,
                       immediate = 0) =
  # Container is expected to already be on the stack.
  if sym == nil:
    ctx.genPushImmediate(immediate)
  else:
    ctx.genPush(sym)

  if sym.tid.isDictType():
    ctx.genTCall(FDictIndex)
  else:
    ctx.genTCall(FIndex)

proc genPushStaticString(ctx: CodeGenState, name: string) =
  ctx.emitInstruction(ZPushStaticPtr, ctx.addStaticObject(name), tid = TString)

proc genLoadAttr(ctx: CodeGenState, tid: TypeId, mode = 0) =
  ctx.emitInstruction(ZLoadFromAttr, tid = tid, arg = mode)

proc genSetAttr(ctx: CodeGenState, tid: TypeId, lock: bool) =
  var boolVal = 0

  if lock:
    boolVal = 1

  ctx.emitInstruction(ZAssignAttr, tid = tid, arg = boolVal)

proc genAssign(ctx: CodeGenState, tid: TypeId) =
  ctx.emitInstruction(ZAssignToLoc, tid = tid)

proc genLoadFromIx(ctx: CodeGenState, tid: TypeId) =
  if tid.isDictType():
    ctx.genTCall(FDictIndex, tid = tid)
  else:
    ctx.genTCall(FIndex, tid = tid)

proc genLoadFromSlice(ctx: CodeGenState, tid: TypeId) =
  ctx.genTCall(FSlice, tid = tid)

proc genPrologue(ctx: CodeGenState, sz: int) =
  ctx.emitInstruction(ZMoveSp, arg = sz)

proc genReturnInstructions(ctx: CodegenState) =
  let fn = ctx.fcur

  if fn.retval != nil and fn.retval.tid.unify(TVoid) == TBottom:
    ctx.emitInstruction(ZSetRes, fn.retval.sym.getOffset(),
                       tid = fn.retval.sym.tid,
                       moduleId = ctx.getSymbolArena(fn.retval.sym))
  ctx.emitInstruction(ZRet)

## End opcode generation

proc genRangeLoopInit(ctx: CodeGenState) =
  let cur = ctx.curNode.contents

  ctx.oneIrNode(cur.condition)

  let
    cursym = cur.loopVars[0]
    maxsym = cur.loopVars[1]

  ctx.genPop(maxSym)
  ctx.genPop(curSym)

proc genContainerLoopInit(ctx: CodeGenState) =
  let
    n            = ctx.curNode
    curSym       = n.contents.loopVars[0]
    containerSym = n.contents.loopVars[^2]
    maxSym       = n.contents.loopVars[^1]

  # This will push the container onto the stack.
  ctx.oneIrNode(n.contents.condition)
  # Address of container should be at top of stack now.
  # We need to get its length, and we need to stash a copy in $container.
  # So let's first dupe the reference:
  ctx.genDupTop()
  ctx.genPop(containerSym)
  ctx.genTCall(FLen, n.contents.condition.tid)
  ctx.genPop(maxSym)
  ctx.genStoreImmediate(curSym, 0)

proc genWhileLoopInit(ctx: CodeGenState) =
  # TODO-- only generate these if $i get used.
  let cursym = ctx.curNode.contents.loopVars[0]
  ctx.genStoreImmediate(curSym, 0)

proc genIterationCheck(ctx: CodeGenState, n: IrNode) =
  let cur = n.contents

  if not cur.whileLoop:
    ctx.genPush(cur.loopVars[0])
    ctx.genPush(cur.loopVars[^1])
    ctx.genEqual()
    ctx.startJnz(target = n)

    if cur.condition.contents.kind != IrRange:
      ctx.genPush(cur.loopVars[^2]) # Container on stack.
      ctx.genContainerIndex(cur.loopVars[0])
      if cur.loopVars.len() == 4:
        # This is a for x in list, so there's no array to unpack,
        # we can just directly pop it in.
        ctx.genPop(cur.loopVars[1])
      else:
        let unpackLen = cur.loopVars.len() - 3
        for i in  0 ..< unpackLen:
          ctx.genContainerIndex(immediate = i)
          ctx.genPop(cur.loopVars[i + 1])
        # The index operation pops the container.
  else:
    ctx.oneIrNode(cur.condition)
    ctx.startJz(target = n)

proc genLoopFooter(ctx: CodeGenState, top: int) =
  ctx.genAdd(ctx.curNode.contents.loopVars[0], 1, TInt)
  ctx.genStore(ctx.curNode.contents.loopVars[0], TInt)
  ctx.genPop()
  ctx.genBackwardsJump(top)

proc genConditional(ctx: CodeGenState, cur: IrContents) =
  ctx.oneIrNode(cur.predicate)
  let patch1 = ctx.startJz()
  ctx.oneIrNode(cur.trueBranch)
  let patch2 = ctx.startJ()
  if cur.falseBranch != nil:
    ctx.oneIrNode(cur.falseBranch)
  ctx.backPatch(patch1)
  ctx.backPatch(patch2)

proc genLoop(ctx: CodeGenState, n: IrNode) =
  # Loop var 0 is the interation count, always.
  # For loops also have a $max, which is always $i.
  # TODO: we should only generate these if used.
  let
    m   = ctx.mcur
    cur = n.contents

  # TODO: debug info w/ label names.
  m.loopLocs.add((n, ctx.codeLoc()))
  let patchStackSz = m.backpatchLocs.len()
  if cur.whileLoop:
    ctx.genWhileLoopInit()
  else:
    if cur.condition.contents.kind == IrRange:
      ctx.genRangeLoopInit()
    else:
      ctx.genContainerLoopInit()

  let top = ctx.codeLoc()
  m.loopLocs.add((n, top))

  if cur.label != nil:
    ctx.genLabel(cur.label.getText() & ": ")
  ctx.genIterationCheck(n)
  ctx.oneIrNode(cur.loopBody)
  ctx.genLoopFooter(top)

  var savedLocs: seq[(IrNode, int)]

  for (node, loc) in m.backpatchLocs:
    if node == n:
      ctx.backpatch(loc)
    else:
      savedLocs.add((node, loc))

  m.backpatchLocs = savedLocs

proc genTypeCase(ctx: CodeGenState, cur: IrContents) =
  var
    locsToFillWithExit: seq[int]
    locsForBranches:    seq[seq[int]]

  ctx.genPushTypeOf(cur.targetSym)

  # First go through and set up the conditional checks, then line up
  # all the jump targets.
  for item in cur.branches:
    var successPatches: seq[int]

    let branch = item.contents

    if branch.conditions.len() == 0: # The else branch; jump unconditionally.
      successPatches.add(ctx.startJ())
    else:
      for item in branch.conditions:
        ctx.genDupTop()
        ctx.oneIrNode(item)
        ctx.genEqual()  # TODO: Add TTSpec
        successPatches.add(ctx.startJnz())

    locsForBranches.add(successPatches)

  # If the last node has 2 branches, it was not an *else* branch,
  # so anything that failed all the tests should jump to the exit.
  if cur.branches[^1].contents.conditions.len() == 2:
    locsToFillWithExit.add(ctx.startJ())

  # Now, we're going to go back through each branch and lay out the
  # 'true' bits. Once it's done, jump to the switch exit.
  for i, item in cur.branches:
    for patchLoc in locsForBranches[i]:
      ctx.backPatch(patchLoc)

    ctx.oneIrNode(item.contents.action)
    locsToFillWithExit.add(ctx.startJ())

  for item in locsToFillWithExit:
    ctx.backPatch(item)

  ctx.genPop() # Discard what we've been cmp-ing against.

proc genValueCase(ctx: CodeGenState, cur: IrContents) =
  # This is identical to genTypeCase except for what we are testing.
  var
    locsToFillWithExit: seq[int]
    locsForBranches:    seq[seq[int]]

  ctx.oneIrNode(cur.switchTarget)

  for item in cur.branches:
    var successPatches: seq[int]

    let branch = item.contents

    if branch.conditions.len() == 0:
      successPatches.add(ctx.startJ())
    else:
      for item in branch.conditions:
        ctx.genDupTop()
        ctx.oneIrNode(item)
        ctx.genEqual(cur.switchTarget.tid)
        successPatches.add(ctx.startJnz())

    locsForBranches.add(successPatches)

  if cur.branches[^1].contents.conditions.len() == 2:
    locsToFillWithExit.add(ctx.startJ())

  for i, item in cur.branches:
    for patchLoc in locsForBranches[i]:
      ctx.backPatch(patchLoc)

    ctx.oneIrNode(item.contents.action)
    locsToFillWithExit.add(ctx.startJ())

  for item in locsToFillWithExit:
    ctx.backPatch(item)

  ctx.genPop()

proc genJump(ctx: CodeGenState, cur: IrContents) =
  if cur.exitLoop:
    ctx.startJ(cur.targetNode)
  else:
    ctx.genBackwardsJump(cur.targetNode)

proc genCallbackLoad(ctx: CodegenState, cb: ptr Callback) =
  if cb.impl.externInfo != nil:
    # External function pointer.
    for i, item in ctx.zobj.ffiInfo:
      let
        p = cast[pointer](addr ctx.zobj.staticData[item.nameOffset])
        s = $(cast[cstring](p))

      if s == cb.impl.externName:
        ctx.emitInstruction(ZPushFfiPtr,
                            arg       = i,
                            immediate = item.nameOffset,
                            tid       = cb.tid)
        return

    unreachable
  else:
    let
      moduleKey = cb.impl.defModule.key
      minf      = ctx.minfo[moduleKey]
      p         = cast[pointer](ctx.addStaticObject(cb.impl.name))

    if cb.impl.codeOffset == 0:
      ctx.callBackpatches.add((cb.impl, ctx.mCur, ctx.codeLoc()))

    ctx.emitInstruction(ZPushVMPtr,
                        arg       = cb.impl.internalId,
                        immediate = cast[int](p),
                        tid       = cb.tid,
                        moduleId  = minf.objInfo.moduleId)

proc genLitLoad(ctx: CodeGenState, n: IrNode) =
  let
    cur   = n.contents
    dt    = n.tid.getDataType()
    dtid  = dt.dtid
    byVal = dt.byValue

  if dtid == TFunc:
    ctx.genCallbackLoad(cast[ptr Callback](n.value))
  elif dtid == TTSpec:
    ctx.genPushImmediate(cast[int64](n.value))
  else:
    if n.isConstant() or n.contents.kind == IrFold:
      if byVal:

        var toPush: int64
        toPush = cast[int64](n.value)
        ctx.genPushImmediate(toPush)
      else:
        let
          serialized = n.value.marshal(n.tid, ctx.memos)
          offset     = ctx.addStaticObject(serialized, serialized.len())

        ctx.genUnmarshalStaticObject(offset, serialized.len(), n.tid)
    else:
      for item in cur.items:
        ctx.oneIrNode(item)

      ctx.genPushImmediate(cur.items.len())
      ctx.genTCall(FContainerLit, n.tid)

proc genLockAttr(ctx: CodeGenState, sym: SymbolInfo) =
  ctx.genPushStaticString(sym.name)
  ctx.emitInstruction(ZLockOnWrite)

proc genMember(ctx: CodeGenState, cur: IrContents) =

  ctx.genPushStaticString(cur.attrSym.name)
  ctx.genLoadAttr(cur.attrSym.tid)

proc genMemberLhs(ctx: CodeGenState, cur: IrContents) =
  ctx.genPushStaticString(cur.attrSym.name)

proc genIndex(ctx: CodeGenState, n: IrNode) =
  let cur = n.contents

  ctx.oneIrNode(cur.toIx)
  ctx.oneIrNode(cur.indexStart)

  if cur.indexEnd != nil:
    ctx.oneIrNode(cur.indexEnd)
    ctx.genLoadFromSlice(cur.toIx.tid)
  else:
    ctx.genLoadFromIx(cur.toIx.tid)

proc genIndexLhs(ctx: CodeGenState, n: IrNode) =
  # The actual assignment is generated above us.
  # So this is the same as genIndex except without actually loading the
  # array value.
  let cur = n.contents

  ctx.oneIrNode(cur.toIx)
  ctx.oneIrNode(cur.indexStart)
  if cur.indexEnd != nil:
    ctx.oneIrNode(cur.indexEnd)

proc genExternCall(ctx: CodeGenState, cur: IrContents) =

  var
    i = cur.actuals.len()
    tid: TypeId
    found: bool

  while i != 0:
    i = i - 1
    ctx.oneIrNode(cur.actuals[i])


  tid = cur.toCall.getTid().idToTypeRef().items[^1].tCopy()

  for i, item in ctx.zobj.ffiInfo:
    let
      p = addr ctx.zobj.staticData[item.nameOffset]
      s = $(cast[cstring](p))

    if s == cur.toCall.externName:
      ctx.emitInstruction(ZFFICall, arg = i, tid = tid)
      found = true
      break

  if not found:
    unreachable

  ctx.emitInstruction(ZMoveSp, - cur.actuals.len())

  if tid != TVoid:
    ctx.emitInstruction(ZPushRes, tid = tid)

proc genRunCallback(ctx: CodeGenState, cur: IrContents) =
  var
    i = cur.actuals.len()
    tid: TypeId
    found: bool

  while i != 0:
    i = i - 1
    ctx.oneIrNode(cur.actuals[i])

  tid = cur.cbSymbol.getTid().idToTypeRef().items[^1].tCopy()

  ctx.genPush(cur.cbSymbol)
  ctx.emitInstruction(ZRunCallback, arg = i, tid = tid)
  ctx.emitInstruction(ZMoveSp, - cur.actuals.len())

  if tid != TVoid:
    ctx.emitInstruction(ZPushRes, tid = tid)

proc genCall(ctx: CodeGenState, cur: IrContents) =
  if cur.cbSymbol != nil:
    ctx.genRunCallback(cur)
    return

  if cur.toCall.defModule == nil:
    ctx.genExternCall(cur)
    return

  var
    moduleKey = cur.toCall.defModule.key
    minf      = ctx.minfo[moduleKey]
    i         = cur.actuals.len()
    va        = cur.toCall.tid.isVarargs()
    numFixed  = cur.toCall.tid.getNumFormals() - 1

  # If the function accepts multiple args, wpush the number of VARARG
  # parameters. What we do with this depends on whether we're
  # FFI-calling.  If we're not, we actually construct a list of
  # items and thus always pass a fixed numer of args internally.
  #
  # Since we push arguments on in reverse order, the list of
  # items should be the first thing pushed, which means we must
  # call construct-list after pushing just the variadic arguments.

  while i != 0:
    i = i - 1
    ctx.oneIrNode(cur.actuals[i])

  if cur.toCall.codeOffset == 0:
    ctx.callBackpatches.add((cur.toCall, ctx.mCur, ctx.codeLoc()))

  ctx.emitInstruction(Z0Call, arg = cur.toCall.internalId,
                     tid = cur.toCall.tid, moduleId = minf.objInfo.moduleId)

  # Now we have to pop what we pushed:
  ctx.emitInstruction(ZMoveSp, - cur.actuals.len())
  if cur.toCall.retVal.tid.followForwards != TVoid:
    ctx.emitInstruction(ZPushRes, tid = cur.toCall.retVal.tid)

proc genUse(ctx: CodeGenState, cur: IrContents) =
  # This is basically just a 'call' with no parameters to the
  # address of the module.

  # TODO-- somehow the pointer we had is getting dropped? Or isn't
  # being loaded in the first place?
  if cur.moduleObj == nil:
    let m = ctx.cc.findAndLoadModule(cur.targetLoc, cur.targetModule, "")

    cur.moduleObj = m.get()

  var
    moduleKey = cur.moduleObj.key
    minf      = ctx.minfo[moduleKey]

  ctx.emitInstruction(ZCallModule, minf.objInfo.moduleId, tid = TVoid)

proc genRet(ctx: CodeGenState, cur: IrContents) =
  # We could skip adding any value to the `result` variable if there's
  # an expression after the return, and go directly to the return
  # register, but for now we're keeping it simple.
  if cur.retVal != nil:
    ctx.oneIrNode(cur.retVal)
    ctx.genPop(ctx.fcur.retval.sym)

  ctx.genReturnInstructions()

proc genAssign(ctx: CodeGenState, n: IrNode) =
  # In the case of assigning to an index, we'll end up w/
  # an extra item on the stack, with the index pushed on after the
  # container's address.
  #
  # To make our lives easier, we just use a different instruction
  # for each of these cases.
  #
  # TODO: Don't push the container's memory storage address if we're
  # doing an index assign (right now the VM knows what to do).

  let cur = n.contents

  ctx.oneIrNode(cur.assignRhs)
  if cur.assignRhs.contents.kind != IrLit and
    not cur.assignRhs.tid.getDataType().byValue:
    ctx.genTCall(FCopy, n.tid)

  # We don't want to generate tuple unpack code yet, because
  # we need to ignore the literal, and do the reverse order.
  if cur.assignLhs.contents.kind != IrLit:
    ctx.oneIrNode(cur.assignLhs)

  if cur.assignLhs.contents.kind == IrIndexLhs:
    let
      ixNode = cur.assignLhs.contents

    case ixNode.toIx.contents.kind
    of IrLhsLoad:
      if ixNode.toIx.contents.symbol.isAttr:
        ctx.genSwap()
        ctx.genLoadAttr(ixNode.toIx.contents.symbol.tid, mode = 1)
        ctx.genSwap()
    of IrMemberLhs:
      ctx.genSwap()
      ctx.genLoadAttr(ixNode.toIx.contents.attrSym.tid, mode = 1)
      ctx.genSwap()
    else:
      discard
    if ixNode.indexEnd == nil:
      if ixNode.toIx.tid.isDictType():
        ctx.genTCall(FAssignDIx, n.tid)
      else:
        ctx.genTCall(FAssignIx, n.tid)
    else:
      ctx.genTCall(FAssignSlice, n.tid)
  elif cur.assignLhs.contents.kind == IrLit:
    # Tuple unpacking. First we 'stash' the tuple, which basically
    # entails sticking it in a register (since we don't support
    # nested unpacking).
    ctx.emitInstruction(ZTupleStash, tid = n.tid)

    # Now, we push the arguments on backward, so that we can more
    # easily unpack off the stack.
    var
      tuplhs = cur.assignLhs.contents
      l      = tuplhs.items.len()
      i      = l

    while i > 0:
      i -= 1
      ctx.oneIrNode(tuplhs.items[i])

    ctx.emitInstruction(ZUnpack, arg = l, tid = n.tid)

  else:
    var
      isAttr: bool
      tid:    TypeId

    if cur.assignLhs.contents.kind == IrLhsLoad:
      isAttr = cur.assignLhs.contents.symbol.isAttr
    else: # IrMemberLhs is below us
      isAttr = true

    if isAttr:
      ctx.genSetAttr(n.tid, n.lock)
    else:
      ctx.genAssign(n.tid)

proc genLoadStorageAddress(ctx: CodeGenState, sym: SymbolInfo) =
  if sym.isAttr:
    ctx.genPushStaticString(sym.name)
  else:
    ctx.emitInstruction(ZPushAddr, sym.getOffset(), tid = sym.tid,
                       moduleId = sym.moduleId)

proc genLoadSymbol(ctx: CodeGenState, sym: SymbolInfo) =
  if sym.isAttr:
    ctx.genPushStaticString(sym.name)
    ctx.genLoadAttr(sym.tid)
  else:
    ctx.genPush(sym)

proc genUminus(ctx: CodeGenState, cur: IrContents) =
  ctx.oneIrNode(cur.uRhs)
  if cur.uRhs.tid.isFloatType():
    ctx.genPushImmediateF(-1.0)
  else:
    ctx.genPushImmediate(-1)
  ctx.genTCall(FMul, cur.uRhs.tid)

proc genBinary(ctx: CodeGenState, n: IrNode) =
  let cur = n.contents

  ctx.oneIrNode(cur.bLhs)
  ctx.oneIrNode(cur.bRhs)

  if cur.opId > 128:
    ctx.genNot()

  ctx.genTCall(cur.opId and 0x7f, tid = n.tid)

proc genLogic(ctx: CodeGenState, cur: IrContents) =
  var backpatchLoc: int

  ctx.oneIrNode(cur.bLhs)

  if cur.opId == OpLogicOr:
    # If stack top is true, we want to jump past the next branch, and
    # leave true on the stack.
    backpatchLoc = ctx.startJnz(popit = false)
  else:
    # If the stack top is FALSE we want to jump past the next branch,
    # and we want to leave 'false' on the stack.
    backpatchLoc = ctx.startJz(popit = false)

  # If we didn't jump, we don't want the extra item on the stack; we'll
  # instead leave the one left by the right branch, which will hold the
  # result for us.
  ctx.genPop()
  ctx.oneIrNode(cur.bRhs)
  ctx.backPatch(backpatchLoc)

proc genLoadValue(ctx: CodeGenState, n: IrNode) =
  ctx.genLitLoad(n)

proc oneIrNode(ctx: CodeGenState, n: IrNode) =
  # Since accessing subcontents is somewhat verbose, if the op doesn't
  # require the tid / value fields at the top-level, I pass the
  # underlying IRContents object with all the op-specific fields, not
  # the outer IRNode.
  let
    saved = ctx.curNode
    cur   = n.contents

  ctx.curNode = n

  case cur.kind
  of IrBlock:
    for item in cur.stmts:
      ctx.oneIrNode(item)
  of IrSection:
   ctx.oneIrNode(cur.blk)
  of IrLoop:
    ctx.genLoop(n) # Need the full node in loop.
  of IrConditional:
    ctx.genConditional(cur)
  of IrSwitch:
    if cur.typeCase:
      ctx.genTypeCase(cur)
    else:
      ctx.genValueCase(cur)
  of IrRange:
    ctx.oneIrNode(cur.rangeStart)
    ctx.oneIrNode(cur.rangeEnd)
  of IrJump:
    ctx.genJump(cur)
  of IrRet:
    ctx.genRet(cur)
  of IrLit:
    ctx.genLitLoad(n)
  of IrMember:
    if n.lock:
      ctx.genLockAttr(cur.attrSym)
    else:
      ctx.genMember(cur)
  of IrMemberLhs:
    ctx.genMemberLhs(cur)
  of IrIndex:
    ctx.genIndex(n)
  of IrIndexLhs:
    ctx.genIndexLhs(n)
  of IrCall:
    ctx.genCall(cur)
  of IrUse:
    ctx.genUse(cur)
  of IrNot:
    ctx.oneIrNode(cur.uRhs)
    ctx.genNot()
  of IrUMinus:
    ctx.genUminus(cur)
  of IrBinary, IrBool:
    ctx.genBinary(n)
  of IrLogic:
    ctx.genLogic(cur)
  of IrLhsLoad:
    ctx.genLoadStorageAddress(cur.symbol)
  of IrLoad:
    if n.lock:
      ctx.genLockAttr(cur.symbol)
    else:
      ctx.genLoadSymbol(cur.symbol)
  of IrFold:
    ctx.genLoadValue(n)
  of IrNop:
    ctx.genNop()
  of IrNil:
    ctx.genPushImmediate(0, TVoid)
  of IrAssign:
    ctx.genAssign(n)
  of IrCast:
    ctx.oneIrNode(n.contents.srcData)
    ctx.genPushImmediate(cast[int](n.tid), TVoid)
    ctx.emitInstruction(ZTCall, FCastFn, tid = n.contents.srcData.tid)
  of IrAssert:
    ctx.oneIrNode(n.contents.assertion)
    ctx.emitInstruction(ZAssert)
  of IrSwitchBranch:
    # Handled from the parent node w/o descending.
    unreachable

  ctx.curNode = saved

proc addParameter(ctx: CodeGenState, symbol: SymbolInfo) =
  discard

proc stashSymbolInfo(scope: Scope, d: var Dict[int, string],
                     t: var seq[(int, TypeId)]) =
  for (name, sym) in scope.table.items(sort=true):
    if sym.isFunc:
      continue
    d[sym.offset] = name
    t.add((sym.offset, sym.getTid()))

proc genFunction(ctx: CodeGenState, fn: FuncInfo) =
  let
    fullname = fn.name & fn.tid.toString()
    zinfo    = ctx.zobj.funcInfo[fn.internalId - 1]

  ctx.genLabel(fullname)

  ctx.fcur      = fn
  fn.codeOffset = ctx.mcur.objInfo.instructions.len() * sizeof(ZInstruction)
  fn.objInfo    = zinfo
  zinfo.offset  = fn.codeOffset

  if fn.fnScope.scopeSize > 0:
    ctx.genPrologue(fn.fnScope.scopeSize - 1)
  ctx.oneIrNode(fn.implementation)
  # Todo: if this is unreachable, don't bother generating it.
  ctx.genReturnInstructions()

  # Keep track of the size of the function implementation.
  let endPos = ctx.mcur.objInfo.instructions.len() * sizeof(ZInstruction)
  zinfo.size = endPos - zinfo.offset
  ctx.fcur   = nil

  ctx.mcur.objInfo.codesyms[zinfo.offset] = fullname

proc genModule(ctx: CodeGenState, m: Module) =
  let curMod = ctx.minfo[m.key]
  if curMod.processed:
    return

  ctx.mcur = curMod

  ctx.genLabel("Module '" & m.modname & "' :")

  for (_, sym) in m.moduleScope.table.items(sort=true):
    if sym.pInfo != nil:
      ctx.addParameter(sym)

  ctx.oneIrNode(m.ir)
  ctx.emitInstruction(ZModuleRet)
  let numInstructions     = curMod.objInfo.instructions.len()
  curMod.objInfo.initSize = numInstructions  * sizeof(ZInstruction)
  ctx.genLabel("Functions: ")

  for (_, sym) in m.moduleScope.table.items(sort=true):
    for fn in sym.fimpls:
      if fn.externInfo != nil:
        continue
      ctx.genFunction(fn)

  curMod.processed = true

  let deps = ctx.minfo.items(sort = true)
  for (k, v) in deps:
    if not v.processed:
      ctx.genModule(v)

proc applyArenaId(ctx: CodeGenState, scope: Scope, moduleId: int,
                  curFnId: var int) =
  for (name, sym) in scope.table.items(sort=true):
    sym.moduleId = moduleId
    for fn in sym.fimpls:
      if fn.externInfo != nil:
        continue
      if fn.internalId == 0:
        fn.internalId = curFnId
        let fi = ZFnInfo(funcname: name)   # Info carried into the object file.
        ctx.zobj.funcInfo.add(fi)
        fn.objInfo = fi
        fi.syms.initDict()
        fn.fnscope.stashSymbolInfo(fi.syms, fi.symTypes)
        curFnId += 1
      ctx.applyArenaId(fn.fnScope, -1, curFnId)

proc addFfiInfo(ctx: CodeGenState, m: Module) =
  # Since we want people to be able to add modules to already existing
  # static objects that use new functions, we've got space reserved in
  # both the module and in the global state for this stuff. But, for
  # the moment, I'm only implementing the initial compilation /
  # linking, so we just plop everything in the global space.
  for (name, sym) in m.moduleScope.table.items(sort=true):
    if not sym.isFunc:
      continue

    for item in sym.fimpls:
      if item.externInfo == nil:
        continue # Not a FFI definition.

      var
        cinfo     = item.externInfo
        zinfo     = ZFFiInfo()
        typeinfo  = item.tid.idToTypeRef()
        dupeEntry = false

      zinfo.nameOffset = ctx.addStaticObject(item.externName)
      zinfo.localName  = ctx.addStaticObject(item.name)


      for entry in ctx.zobj.ffiInfo:
        if entry.nameOffset == zinfo.nameOffset:
          dupeEntry = true
          break

      if dupeEntry:
        continue

      for dll in cinfo.dlls:
        zinfo.dlls.add(ctx.addStaticObject(dll))

      if typeinfo.items.len() > cinfo.cArgTypes.len():
        lateError("ExternZArgCt", @[name, $(typeinfo.items.len()),
                                    $(cinfo.cArgTypes.len()),
                                    typeinfo.typeId.toString()])


      elif typeinfo.items.len() < cinfo.cArgTypes.len():
        lateError("ExternCArgCt", @[name, $(typeinfo.items.len()),
                                    $(cinfo.cArgTypes.len())])
      for i, param in cinfo.cArgTypes:
        var
          cArgType  = mapCArgType(param[0])
          ourType: int32

        let itemType = typeinfo.items[i].idToTypeRef()
        if itemType.kind == C4TVar:
          ourType = RTAsMixed
        else:
          let dsttype = typeinfo.items[i].followForwards()
          if dsttype.isBasicType():
            ourType = int32(dsttype)
          else:
            ourType = RTAsPtr

        if ourType < 0:
          cArgType = mapCArgType("ptr")

        var paramInfo = ZffiArgInfo(argType: cArgType, ourType: ourType)

        zinfo.argInfo.add(paramInfo)

      for n in cinfo.heldParams:
        zinfo.argInfo[n].held = true

      for n in cinfo.allocedParams:
        zinfo.argInfo[n].alloced = true

      if typeinfo.va:
        zinfo.va = true

      ctx.zobj.ffiInfo.add(zinfo)

proc extractSectionDocs(ctx: CodeGenState,
                        m:   Module,
                        d:   Dict[string, AttrDocs]) =
  # TODO: warn about multiple adds.
  for node in m.secDocNodes:
    var sec = node.contents.prefix
    if sec == "":
      sec = node.contents.sectName
    else:
      sec &= "." & node.contents.sectName

    sec &= "." & node.contents.instance

    if sec != "" and sec[^1] == '.':
      sec = sec[0 ..< ^1]

    let docObj = AttrDocs(shortdoc: node.shortdoc, longdoc: node.longdoc)
    d[sec] = docObj

proc setupModules(ctx: CodeGenState, d: Dict[string, AttrDocs]) =
  var
    curArena = 1
    curFnId  = 1

  # This applies a unique number to each module (an moduleId id)
  # and then applies a unique number to each function.
  ctx.applyArenaId(ctx.cc.globalScope, 0, curFnId)
  ctx.zobj.globals.initDict()
  ctx.cc.globalScope.stashSymbolInfo(ctx.zobj.globals, ctx.zobj.symTypes)
  ctx.zobj.globalScopeSz = ctx.cc.globalScope.scopeSize

  for (_, module) in ctx.cc.modules.items(sort=true):
    ctx.addFfiInfo(module)
    ctx.extractSectionDocs(module, d)


  for (_, module) in ctx.cc.modules.items(sort=true):
    var mi = ZModuleInfo(moduleId: curArena, modname: module.modname,
                         moduleVarSize: module.moduleScope.scopeSize)
    module.objInfo = mi
    ctx.applyArenaId(module.moduleScope, curArena, curFnId)
    curArena              = curArena + 1
    ctx.minfo[module.key] = module
    ctx.zobj.moduleContents.add(mi)
    mi.codesyms.initDict()
    mi.datasyms.initDict()

    if not module.system:
      mi.source = $(module.s.runes)

    module.moduleScope.stashSymbolInfo(mi.datasyms, mi.symTypes)
    mi.codesyms[0] = module.modname & ".__mod_run__()"

proc fillCallBackpatches(ctx: CodeGenState) =
  for (fn, dstmodule, patchloc) in ctx.callBackpatches:
    var cur = dstmodule.objInfo.instructions[patchloc]
    cur.arg = int32(fn.internalId)

    dstmodule.objInfo.instructions[patchloc] = cur

proc cacheAllTypeInfo(ctx: CodeGenState) =
  # For the moment, instead of keeping type IDs for only types we
  # output, we go ahead and keep any type that does not forward.  This
  # is far less error prone than trying to make sure we always
  # remember to save off type info, but it might end up causing too
  # much bloat. I doubt it, though! However, we definitely should
  # collect metrics on this.

  for (id, tObj) in typeStore.items(sort=true):
    if tObj.typeId != id:
      continue

    let
      typeName = id.toString()
      loc      = ctx.addStaticObject(typeName)

    discard ctx.zobj.tInfo.add(id, loc)

  var typeCache: seq[string]
  for (k, v) in ctx.zobj.tInfo.items(sort=true):
    typeCache.add($(cast[int](k)))
    typeCache.add($(cast[cstring](addr ctx.zobj.staticdata[v])))

proc generateCode*(cc: CompileCtx, nextId = -1): RuntimeState =
  var ctx = CodeGenState()

  result     = RuntimeState()
  result.obj = ZObjectFile()
  ctx.cc     = cc
  ctx.zobj   = result.obj
  ctx.memos  = Memos()

  result.sectionDocs.initDict()

  ctx.zobj.tinfo.initDict()
  ctx.memos.map.initDict()
  ctx.minfo.initDict()
  ctx.strCache.initDict()
  ctx.cacheAllTypeInfo()
  ctx.setupModules(result.sectionDocs)
  ctx.genModule(cc.entrypoint)
  ctx.fillCallBackpatches()

  result.obj.entrypoint     = int32(cc.entrypoint.objInfo.moduleId)
  result.obj.nextEntrypoint = int32(nextId)
  result.obj.spec           = ctx.cc.attrSpec

# TODO -- pushTypeOf needs to handle attributes.
# TODO -- varargs for func entry.
