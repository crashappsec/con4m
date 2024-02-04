# This is just a basic runtime with no thought to performance; we'll
# do one more focused on performance later, probably directly in C.
proc load_spec() {.cdecl, importc.}

import "."/[stchecks, attrstore]
export attrstore

const sz = sizeof(ZInstruction)

when USE_TRACE:
  import codegen # For instr.toString()
  var trace_on = true

  proc stackRepr(ctx: RuntimeState): Rope =
    var
      rowOne:  seq[Rope]
      rowTwo:  seq[Rope]
      rowThree: seq[Rope]

      diff = ctx.stack.len() - ctx.sp

    for i in 0 .. diff:
      var
        item: pointer
        toptxt: Rope
        bottxt: Rope
        offset = (ctx.sp + i) - ctx.fp


      if i != diff:
        item = ctx.stack[i + ctx.sp]
      else:
        item = nil

      toptxt = text((cast[uint64](item)).toHex().toLowerAscii())

      if offset == 0:
        toptxt = fgColor(toptxt, "jazzberry")
        bottxt = fgColor("fp", "jazzberry")
        rowthree.add(fgColor(toHex(ctx.sp + i).toLowerAscii(), "jazzberry"))
      elif offset < 0:
        bottxt = text("fp "  & $offset)
        rowthree.add(text(toHex(ctx.sp + i).toLowerAscii()))
      else:
        bottxt = text("fp + " & $offset)
        rowthree.add(text(toHex(ctx.sp + i).toLowerAscii()))


      rowOne.add(toptxt)
      rowTwo.add(bottxt)

    return quicktable(@[rowOne, rowTwo, rowThree], noHeaders = true)

  proc fnScopeRepr(ctx: RuntimeState): Rope =
    var rows: seq[seq[Rope]] = @[]

    rows.add(@[text("Variable"), text("Value"), text("Offset")])

    if ctx.numFrames != 0 and
       ctx.frameInfo[ctx.numFrames - 1].targetFunc != nil:
      let
        fnObj = ctx.frameInfo[ctx.numFrames - 1].targetFunc

      for (offset, name) in fnObj.syms.items(sort=true):
        let val = toHex(cast[int](ctx.stack[ctx.fp -
                               (2*offset)])).toLowerAscii()
        rows.add(@[text(name), text(val), text($(offset))])

      return rows.quickTable(title = "Fn Scope")

  proc moduleScopeRepr(ctx: RuntimeState): Rope =
    var rows: seq[seq[Rope]] = @[]

    rows.add(@[text("Variable"), text("Value"), text("Offset")])

    for (offset, name) in ctx.curModule.datasyms.items(sort=true):
      let
        raw = ctx.moduleIds[ctx.curModule.moduleId][offset * 2]
        val = toHex(cast[int](raw)).toLowerAscii()
      rows.add(@[text(name), text(val), text($(offset))])

    return rows.quickTable(title = "Module scope")

  proc traceExecution(ctx: RuntimeState, instr: ZInstruction) =
    if trace_on:
      if TRACE_STACK:
        print ctx.stackRepr()
      if TRACE_SCOPE:
        print ctx.fnScopeRepr()
        print ctx.moduleScopeRepr()
      if TRACE_INSTR:
        echo "Next instruction: ip = ", toHex(ctx.ip * sz),
         "; sp = ", ctx.sp.toHex().toLowerAscii(),
         "; fp = ", ctx.fp.toHex().toLowerAscii()

        echo instr.toString()
else:
  template traceExecution(ctx: RuntimeState, instr: ZInstruction) =
    discard

template getModName(ctx: RuntimeState): string =
  ctx.curModule.modName & ".c4m"

template getLineNo(ctx: RuntimeState): int =
  ctx.curModule.instructions[ctx.ip].lineNo

proc getSourceLoc*(ctx: RuntimeState): string =
  ## Decode the source location of the current runtime state from
  ## the current instruction.
  return ctx.getModName() & " (line #" & $(ctx.getLineNo()) & ")"


proc getStackTrace*(ctx: RuntimeState): Rope {.exportc, cdecl.} =
  var cells: seq[seq[string]] = @[@["Caller module", "Line #",
                                   "Call target"]]

  for i in 1 ..< ctx.numFrames:
    var
      frame = ctx.frameInfo[i]
      row: seq[string]

    row.add(frame.callModule.modname)
    if i == ctx.numFrames - 1:
      row.add($(ctx.getLineNo()))
    else:
      row.add($(frame.calllineno))

    if frame.targetfunc == nil:
      row.add(frame.targetmodule.modname & ".__mod_run__")
    else:
      row.add(frame.targetmodule.modname & "." & frame.targetfunc.funcname)

    cells.add(row)

  result = cells.quicktable(title =  "Stack trace",
                            caption = "Source location: " & ctx.getSourceLoc())
  result.colWidths([(17, true), (15, true), (20, true)])
  result.tpad(0, true).bpad(0, true)

proc printStackTrace*(ctx: RuntimeState) =
  print ctx.getStackTrace()

proc bailHere(ctx: RuntimeState, errCode: string, extra: seq[string] = @[]) =
  var
    errList: seq[Con4mError]
    runes = ctx.curModule.source.toRunes()

  errList.baseError(code = errCode,
                    cursor = StringCursor(runes: runes),
                    modname = ctx.getModName(),
                    line = ctx.getLineNo(), lineOffset = -1,
                    phase = ErrRuntime, severity = LlFatal,
                    extraContents = extra)
  print errlist.formatErrors()
  ctx.printStackTrace()
  quit(-1)

proc storageAddr(ctx: RuntimeState, x: ZInstruction,
                     p: int64): ptr pointer =
  if x.moduleId == -1:
    result = addr ctx.stack[ctx.fp - (p * 2)]
  elif x.moduleId == -2:
    # Static data, no type info assumed.
    result = cast[ptr pointer](addr ctx.obj.staticData[p])
  else:
    result = addr ctx.moduleIds[x.moduleId][p * 2]

template storageAddr(ctx: RuntimeState, x: ZInstruction): ptr pointer =
  ctx.storageAddr(x, int64(x.arg))

template leaveFrame() =
  # Move the stack pointer to the frame pointer.
  ctx.sp  = ctx.fp

  # The saved frame pointer is under this spot.
  ctx.fp  = cast[int](ctx.stack[ctx.sp])

  # Once we've restored it, we can pop it.
  ctx.sp += 1

  # Next word has the saved module ID and instruction pointer.
  # Recover them.
  let
    asUInt = cast[uint64](ctx.stack[ctx.sp])
    module = asUint and 0xffffffff'u64
    ip     = asUint shr 32
  ctx.ip        = int(ip)
  ctx.curModule = ctx.obj.moduleContents[module - 1]

  # Now, pop that off too.
  ctx.sp += 1

  # Finally, pop the call trace info.
  ctx.popFrame()

proc showAssertionFailure(ctx: RuntimeState, instr: ZInstruction) =
  let
    lineno  = instr.lineno
    module  = ctx.obj.moduleContents[instr.moduleId - 1]
    line    = module.source.split(Rune('\n'))[lineno - 1]
    modname = module.modname

  # TODO: we should re-parse and print the entire expression.
  print(text("Assertion ") + fgcolor("failed", "red") +
        text(" in module " & modname & " (line " & $(lineno) & ")"),
        file = stderr)
  print(strong(line), file = stderr)

proc runMainExecutionLoop(ctx: RuntimeState): int =
  result = 0 # Default exit

  while true:
    let instr = ctx.curModule.instructions[ctx.ip]
    ctx.traceExecution(instr)
    case instr.op
    of ZNop:
      discard
    of ZMoveSp:
      ctx.sp -= (instr.arg * 2) # 1 var == 2 words
    of ZPushImm:
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.immediate)
    of ZPushRes:
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = ctx.returnRegister
    of ZSetRes:
      ctx.returnRegister = ctx.stack[ctx.fp - 2]
      ctx.rrType         = ctx.stack[ctx.fp - 1]
    of ZPushStaticPtr:
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.arg)
    of ZPushVal, ZPushPtr:
      let val = ctx.storageAddr(instr)[]
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = val
    of ZPushAddr:
      let val = ctx.storageAddr(instr)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = val
    of ZDupTop:
      ctx.sp -= 1
      ctx.stack[ctx.sp] = ctx.stack[ctx.sp + 2]
      ctx.sp -= 1
      ctx.stack[ctx.sp] = ctx.stack[ctx.sp + 2]
    of ZPop:
      ctx.sp += 2
    of ZJz:
      if ctx.stack[ctx.sp] == nil:
        ctx.ip += instr.arg div sz
        continue
      ctx.sp += 2
    of ZJnz:
      if ctx.stack[ctx.sp] != nil:
        ctx.ip += instr.arg div sz
        continue
      ctx.sp += 2
    of ZJ:
      ctx.ip += instr.arg div sz
      continue
    of ZNot:
      if ctx.stack[ctx.sp] == nil:
        ctx.stack[ctx.sp] = cast[pointer](1)
      else:
        ctx.stack[ctx.sp] = nil
    of ZHalt:
      return
    of ZSwap:
      var tmptop: pointer

      tmptop                = ctx.stack[ctx.sp]
      ctx.stack[ctx.sp]     = ctx.stack[ctx.sp + 2]
      ctx.stack[ctx.sp + 2] = tmptop

      tmptop                = ctx.stack[ctx.sp + 1]
      ctx.stack[ctx.sp + 1] = ctx.stack[ctx.sp + 3]
      ctx.stack[ctx.sp + 3] = tmptop

    of ZLoadFromAttr:
      let
        zptr   = cast[int](ctx.stack[ctx.sp])
        eix    = ctx.obj.staticdata.find('\0', zptr)
        key    = ctx.obj.staticdata[zptr ..< eix]
        byAddr = if instr.arg == 0: false else: true

      var err = false

      let dst = cast[ptr TypeId](addr ctx.stack[ctx.sp + 1])

      ctx.stack[ctx.sp] = ctx.get(key, err, dst, byAddr)

      if err:
        ctx.bailHere("AttrUse", @[$(key)])

    of ZAssignAttr:
      let
        zptr  = cast[int](ctx.stack[ctx.sp])
        eix   = ctx.obj.staticdata.find('\0', zptr)
        key   = ctx.obj.staticdata[zptr ..< eix]

      ctx.sp += 2

      let value = ctx.stack[ctx.sp]
      ctx.sp += 1
      let vtype = instr.typeInfo
      ctx.sp += 1

      let lock = if instr.arg != 0: true else: false

      if not ctx.set(key, value, vType, lock):
        var
          olditem: pointer
          oldtype: TypeId
          err:     bool

        olditem = ctx.get(key, err, addr oldType)

        ctx.bailHere("LockedAttr",
                     @[$(key),
                       $(call_repr(olditem, oldtype)),
                       $(call_repr(value, vtype))])

    of ZLockOnWrite:
      let
        zptr  = cast[int](ctx.stack[ctx.sp])
        eix   = ctx.obj.staticdata.find('\0', zptr)
        key   = ctx.obj.staticdata[zptr ..< eix]

      ctx.sp += 2

      var
        newInfo = AttrContents(lockOnWrite: true)
        oldInfo: AttrContents

      let infOpt = ctx.attrs.lookup(key)
      if infOpt.isSome():
        oldInfo = infOpt.get()
        if oldInfo.locked:
            ctx.bailHere("AlreadyLocked", @[$(key)])
        newInfo.contents = oldInfo.contents
        newInfo.tid      = oldInfo.tid
        newInfo.isSet    = oldInfo.isSet

      ctx.attrs[key] = newInfo
      if oldinfo != nil:
        GC_unref(oldinfo)
    of ZStoreTop:
      let
        address  = ctx.storageAddr(instr)
        typeaddr = cast[ptr pointer](cast[uint](address) +
                                     uint(sizeof(pointer)))

      address[]  = ctx.stack[ctx.sp]
      typeaddr[] = ctx.stack[ctx.sp + 1]
    of ZStoreImm:
      let
        address  = ctx.storageAddr(instr)
        typeaddr = cast[ptr pointer](cast[uint](address) +
                                     uint(sizeof(pointer)))

      address[]  = cast[pointer](instr.immediate)
      typeaddr[] = cast[pointer](instr.typeInfo)
    of ZPushSType:
      let
        address  = ctx.storageAddr(instr)
        typeaddr = cast[ptr pointer](cast[uint](address) +
                                     uint(sizeof(pointer)))
        typeId   = typeaddr[]

      ctx.sp -= 1
      ctx.stack[ctx.sp] = typeId

      ctx.sp -= 1
      ctx.stack[ctx.sp] = typeId
    of ZTCall:
      # This really should be, index into a table and let those
      # functions use the stack directly.  However, until we are
      # really using the C ABI and the system stack, we're going to go
      # through this slight pain.
      #
      # For example, we can't just point at the start of args on the
      # stack and call, because we're also keeping type info on the
      # stack.
      var err: bool
      var strerr: string
      case instr.arg
      of FRepr:
        let
          arg   = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])

        ctx.stack[ctx.sp + 1] = cast[pointer](TString)
        # TODO: we're not managing this pointer. Needs to be tracked.
        # right now we're just leaking it.
        let s = call_repr(arg, argTy)
        ctx.stack[ctx.sp] = cast[pointer](s)
      of FCastFn:
        let
          srcType = instr.typeInfo
          dstType = cast[TypeId](ctx.stack[ctx.sp])
          obj     = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = call_cast(obj, srcType, dstType, strerr)
        ctx.stack[ctx.sp + 1] = cast[pointer](dstType)

        if strErr != "":
          ctx.bailHere(strErr, @[srcType.toString(), dstType.toString()])
      of FEq:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = cast[pointer](call_eq(arg1, arg2, argTy))
        ctx.stack[ctx.sp + 1] = cast[pointer](TBool)
      of FLt:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = cast[pointer](call_lt(arg1, arg2, argTy))
        ctx.stack[ctx.sp + 1] = cast[pointer](TBool)
      of FGt:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = cast[pointer](call_gt(arg1, arg2, argTy))
        ctx.stack[ctx.sp + 1] = cast[pointer](TBool)
      of FAdd:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_add(arg1, arg2, argTy)
      of FSub:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_sub(arg1, arg2, argTy)
      of FMul:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_mul(arg1, arg2, argTy)
      of FIDiv:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_idiv(arg1, arg2, argTy)
      of FFDiv:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_fdiv(arg1, arg2, argTy)
        ctx.stack[ctx.sp + 1] = cast[pointer](TFloat)
      of FMod:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_mod(arg1, arg2, argTy)

      of FShl:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_shl(arg1, arg2, argTy)

      of FShr:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_shr(arg1, arg2, argTy)

      of FBand:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_band(arg1, arg2, argTy)

      of FBor:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_bor(arg1, arg2, argTy)

      of FBxor:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp] = call_bxor(arg1, arg2, argTy)

      of FIndex:
        let
          ix = cast[int](ctx.stack[ctx.sp])
          c  = ctx.stack[ctx.sp + 2]
          ty = cast[TypeId](ctx.stack[ctx.sp + 3])
          to = ty.idToTypeRef()
          it = to.items[^1]

        var err: bool

        ctx.sp += 2
        ctx.stack[ctx.sp]     = call_index(c, ix, ty, err)
        ctx.stack[ctx.sp + 1] = cast[pointer](it)

        if err:
          ctx.bailHere("ArrayIxErr", @[ $(ix) ])
      of FSlice:
        let
          endIx = cast[int](ctx.stack[ctx.sp])
          stIx  = cast[int](ctx.stack[ctx.sp + 2])
          c     = ctx.stack[ctx.sp + 4]
          ty    = cast[TypeId](ctx.stack[ctx.sp + 5])

        var err: bool

        ctx.sp += 4
        # Type is already correct on the stack, since we're writing
        # over the container location and this is a slice.
        ctx.stack[ctx.sp] = call_slice(c, stIx, endIx, ty, err)

        # Currently, `err` will never be set.
      of FAssignIx:
        let
          ix = cast[int](ctx.stack[ctx.sp])
          cp = cast[ptr pointer](ctx.stack[ctx.sp + 2])
          c  = cp[]
          ty = cast[TypeId](ctx.stack[ctx.sp + 3])
          ob = ctx.stack[ctx.sp + 4]                 # Data to assign

        # Do I have to take the address of the container on assign, if
        # it's an attr? I think so; I think that's what's wrong.
        var err: bool

        ctx.sp += 6
        call_assign_ix(c, ob, ix, ty, err)

        if err:
          ctx.bailHere("ArrayIxErr", @[ $(ix) ])
      of FassignSlice:
        let
          endix = cast[int](ctx.stack[ctx.sp])
          start = cast[int](ctx.stack[ctx.sp + 2])
          cp    = cast[ptr pointer](ctx.stack[ctx.sp + 4])
          c     = cp[]
          ty    = cast[TypeId](ctx.stack[ctx.sp + 5])
          ob    = ctx.stack[ctx.sp + 6] # Data to assign

        var err: bool

        ctx.sp += 8
        call_assign_slice(c, ob, start, endix, ty, err)

        # No possible error right now.
      of FDictIndex:
        let
          ix = ctx.stack[ctx.sp]
          c  = ctx.stack[ctx.sp + 2]
          ty = cast[TypeId](ctx.stack[ctx.sp + 3])
          to = ty.idToTypeRef()
          it = to.items[^1]

        var err: bool

        ctx.sp += 2
        ctx.stack[ctx.sp]     = call_dict_index(c, ix, ty, err)
        ctx.stack[ctx.sp + 1] = cast[pointer](it)

        if err:
          ctx.bailHere("DictKeyErr", @[ $(call_repr(ix, to.items[0])) ])

      of FAssignDIx:
        let
          ix = ctx.stack[ctx.sp]
          cp = cast[ptr pointer](ctx.stack[ctx.sp + 2])
          c  = cp[]
          ty = cast[TypeId](ctx.stack[ctx.sp + 3])
          ob = ctx.stack[ctx.sp + 4]                 # Data to assign

        var err: bool

        ctx.sp += 6
        call_assign_dict_ix(c, ob, ix, ty, err)

      of FContainerLit:
        var
          num = cast[int](ctx.stack[ctx.sp])
          ty  = instr.typeInfo
          err:      string
          contents: seq[pointer]

        for i in 0 ..< num:
          ctx.sp += 2
          contents = @[ctx.stack[ctx.sp]] & contents

        ctx.stack[ctx.sp] = runtime_instantiate_container(ty, contents, err)

        if err != "":
          ctx.bailHere(err)
      of FCopy:
        let
          arg   = ctx.stack[ctx.sp]
          argTy = cast[TypeId](ctx.stack[ctx.sp + 1])

        # TODO: need to track this pointer.
        ctx.stack[ctx.sp] = call_copy(arg, argTy)
      of FLen:
        let
          arg   = ctx.stack[ctx.sp]
          argTy = instr.typeInfo

        ctx.stack[ctx.sp + 1] = cast[pointer](TInt)
        # TODO: we're not managing this pointer. Needs to be tracked.
        # right now we're just leaking it.
        let s             = cast[pointer](call_len(arg, argTy))
        ctx.stack[ctx.sp] = cast[pointer](s)

      of FPlusEqRef, FGetFFIAddr, FInitialize, FCleanup, FLoadLit:
        unreachable # Not implemented yet, or not called via ZTCall.
      else:
        ctx.bailHere("RT_badTOp", @[$(instr.arg)])

    of Z0Call:
      ctx.z_native_call(instr.arg, instr.moduleId, instr)
      continue
    of ZCallModule:
      let
        toSave = ctx.ip shl 32 or ctx.curModule.moduleId
        mobj   = ctx.obj.moduleContents[instr.arg - 1]

      ctx.sp -= 1

      ctx.stack[ctx.sp] = cast[pointer](toSave)
      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cast[pointer](ctx.fp)
      ctx.fp            = ctx.sp
      ctx.ip            = 0

      let nextInstruction = ctx.curModule.instructions[ctx.ip]
      ctx.pushFrame(ctx.curModule, instr.lineno, nextInstruction.lineno, nil,
                    mobj)

      ctx.curModule = mobj
    of ZRunCallback:
      let cbObj = cast[ptr ZCallback](ctx.stack[ctx.sp])

      ctx.sp += 2
      ctx.run_callback(cbObj)
    of ZRet:
      leaveFrame()
    of ZModuleRet:
      if ctx.numFrames <= 2:
        return
      leaveFrame()
    of ZFFICall:
      ctx.z_ffi_call(instr.arg)
    of ZPushFfiPtr:
      var cb        = ZCallback.create()
      cb.impl       = cast[pointer](instr.arg)
      cb.nameoffset = int(instr.immediate)
      cb.tid        = instr.typeInfo
      cb.ffi        = true

      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cast[pointer](cb.tid)
      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cb
    of ZPushVmPtr:
      var cb        = ZCallback.create()
      cb.impl       = cast[pointer](instr.arg)
      cb.nameoffset = int(instr.immediate)
      cb.tid        = instr.typeInfo
      cb.ffi        = false
      cb.mid        = instr.moduleId

      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cast[pointer](cb.tid)
      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cb
    of ZSObjNew:
      # TODO: memory management.
      let
        address = ctx.storageAddr(instr, instr.immediate)
        objlen  = instr.arg
        obj     = instantiate_literal(instr.typeInfo, address, objlen)

      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = obj
    of ZAssignToLoc:
      let
        address  = cast[ptr pointer](ctx.stack[ctx.sp])
        typeaddr = cast[ptr pointer](cast[uint](address) +
                                     uint(sizeof(pointer)))
      ctx.sp += 2
      address[] = ctx.stack[ctx.sp]
      ctx.sp += 1
      typeaddr[] = ctx.stack[ctx.sp]
      ctx.sp += 1
    of ZAssert:
      if ctx.stack[ctx.sp] != nil:
        ctx.sp += 2
      else:
        ctx.showAssertionFailure(instr)
        return -1

    ctx.ip += 1

proc setupArena(ctx: RuntimeState, typeInfo: seq[(int, TypeId)], moduleIdSz: int) =
  # 128 bits per item.
  var moduleId = newSeq[pointer](moduleIdSz * 2)

  for (offset, tid) in typeInfo:
    moduleId[offset*2 + 1] = cast[pointer](tid)

  ctx.moduleIds.add(moduleId)

proc setupFfi(ctx: var RuntimeState) =
  var
    obj = ctx.obj

  ctx.externCalls = newSeq[CallerInfo](obj.ffiInfo.len())
  ctx.externArgs  = newSeq[seq[FfiType]](obj.ffiInfo.len())
  ctx.externFps   = newSeq[pointer](obj.ffiInfo.len())

  for i, item in obj.ffiInfo:
    var
      dlls:    seq[string]
      fname:   string
      p:       cstring
      argp:    pointer = nil
      numargs: cuint

    if item.va:
      runtimeWarn("External variable argument functions are not yet supported.")
      continue

    p       = cast[cstring](addr obj.staticData[item.nameOffset])
    fname   = $(p)
    numArgs = cuint(item.argInfo.len() - 1)

    for dllNameOffset in item.dlls:
      p = cast[cstring](addr obj.staticData[dllNameOffset])
      dlls.add($p)

    let fptr = findSymbol(fname, dlls)

    if fptr == nil:
      runtimeWarn("MissingSym", @[fname])
      continue

    ctx.externFps[i] = fptr

    if numArgs > 0:
      for j in 0 ..< numArgs:
        # Do not do the return value.
        ctx.externArgs[i].add(ffiTypeNameMapping[item.argInfo[j].argType])
      argp = addr ctx.externArgs[i][0]

    var retType = ffiTypeNameMapping[item.argInfo[^1].argType]

    ffi_prep_cif(ctx.externCalls[i], ffiAbi, numargs, retType, argp)

proc applyDefaultAttributes(ctx: RuntimeState) =
  if ctx.obj.spec == nil:
    return

  ctx.applyOneSectionSpecDefaults("", ctx.obj.spec.rootSpec)

proc run_0c00l_vm*(ctx: RuntimeState): int {.exportc, cdecl.} =
  ctx.pushFrame(ctx.curModule, 0, 0, nil, ctx.curModule)
  ctx.running = true
  result = ctx.runMainExecutionLoop()
  ctx.running = false
  ctx.popFrame()

proc execute_object*(obj: ZObjectFile): int {.exportc, cdecl.} =
  ## This call is intended for first execution, not for save /
  ## resumption.
  var ctx = RuntimeState()

  currentRuntime = ctx
  ctx.obj        = obj
  ctx.curModule  = obj.moduleContents[obj.nextEntrypoint - 1]
  ctx.numFrames  = 1
  ctx.sp         = STACK_SIZE
  ctx.fp         = ctx.sp

  ctx.attrs.initDict()
  ctx.applyDefaultAttributes()
  # Add moduleIds.
  ctx.setupFfi()
  ctx.setupArena(obj.symTypes, obj.globalScopeSz)

  for item in obj.moduleContents:
    ctx.setupArena(item.symTypes, item.moduleVarSize)

  return ctx.run_0c00l_vm()
