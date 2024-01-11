# This is just a basic runtime with no thought to performance; we'll
# do one more focused on performance later, probably directly in C.

import common, nimutils, stchecks, strutils, ffi

const
  MAX_CALL_DEPTH = 200
  STACK_SIZE     = 1 shl 20
  sz             = sizeof(ZInstruction)
  USE_TRACE      = false


type
 StackFrame   = object
    callModule:   ZModuleInfo
    calllineno:   int32
    targetline:   int32
    targetfunc:   ZFnInfo
    targetmodule: ZModuleInfo # If not targetFunc

 RuntimeState = ref object
    obj:            ZObjectFile
    frameInfo:      array[MAX_CALL_DEPTH, StackFrame]
    numFrames:      int
    stack:          array[STACK_SIZE, pointer]
    curModule:      ZModuleInfo
    sp:             int
    fp:             int
    ip:             int      # Index into instruction array, not bytes.
    returnRegister: pointer
    rrType:         pointer
    arenas:         seq[seq[pointer]]
    attrs:          Dict[string, pointer]
    attrTypes:      Dict[string, TypeId]
    externCalls:    seq[CallerInfo]
    externArgs:     seq[seq[FfiType]]
    externFps:      seq[pointer]

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

      for (offset, name) in fnObj.syms.items():
        let val = toHex(cast[int](ctx.stack[ctx.fp - (2*offset)])).toLowerAscii()
        rows.add(@[text(name), text(val), text($(offset))])

      return rows.quickTable(title = "Fn Scope")

  proc moduleScopeRepr(ctx: RuntimeState): Rope =
    var rows: seq[seq[Rope]] = @[]

    rows.add(@[text("Variable"), text("Value"), text("Offset")])

    for (offset, name) in ctx.curModule.datasyms.items():
      let
        raw = ctx.arenas[ctx.curModule.arenaId][offset * 2]
        val = toHex(cast[int](raw)).toLowerAscii()
      rows.add(@[text(name), text(val), text($(offset))])

      return rows.quickTable(title = "Module scope")

  proc traceExecution(ctx: RuntimeState, instr: ZInstruction) =
    if trace_on:
      print ctx.stackRepr()
      print ctx.fnScopeRepr()
      print ctx.moduleScopeRepr()
      echo "Next instruction: ",
         "ip = ", toHex(ctx.ip * sz), "; sp = ", ctx.sp.toHex().toLowerAscii(),
         "; fp = ", ctx.fp.toHex().toLowerAscii()

      echo instr.toString()
else:
  template traceExecution(ctx: RuntimeState, instr: ZInstruction) =
    discard


proc getStackTrace*(ctx: RuntimeState): Rope =
  var cells: seq[seq[string]] = @[@["Caller module", "Caller line",
                                   "Call target", "Call start"]]

  for i in 1 ..< ctx.numFrames:
    var
      frame = ctx.frameInfo[i]
      row: seq[string]

    row.add(frame.callModule.modname)
    row.add($(frame.calllineno))

    if frame.targetfunc == nil:
      row.add(frame.targetmodule.modname & ".__mod_run__")
    else:
      row.add(frame.targetmodule.modname & "." & frame.targetfunc.funcname)

    row.add($(frame.targetline))
    cells.add(row)

  result = cells.quicktable(title = "Stack trace")

proc printStackTrace*(ctx: RuntimeState) =
  print ctx.getStackTrace()

template getModName(ctx: RuntimeState): string =
  ctx.curModule.modName

template getLineNo(ctx: RuntimeState): int =
  ctx.curModule.instructions[ctx.ip].lineNo

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
  quit(-1)

template pushFrame(ctx: RuntimeState, cm: ZModuleInfo, cl: int32,
                   tl: int32, tf: ZFnInfo, tm: ZModuleInfo) =
  ctx.frameInfo[ctx.numFrames].callmodule   = cm
  ctx.frameInfo[ctx.numFrames].calllineno   = cl
  ctx.frameInfo[ctx.numFrames].targetline   = tl
  ctx.frameInfo[ctx.numFrames].targetfunc   = tf
  ctx.frameInfo[ctx.numFrames].targetmodule = tm
  ctx.numFrames += 1

template popFrame(ctx: RuntimeState) =
  ctx.numFrames -= 1

proc storageAddr(ctx: RuntimeState, x: ZInstruction,
                     p: int64): ptr pointer =
  if x.arena == -1:
    result = addr ctx.stack[ctx.fp - (p * 2)]
  elif x.arena == -2:
    # Static data, no type info assumed.
    result = cast[ptr pointer](addr ctx.obj.staticData[p])
  else:
    result = addr ctx.arenas[x.arena][p * 2]

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
    module  = ctx.obj.moduleContents[instr.arena - 1]
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
    of ZLoadFromAttr:
      let
        zptr  = cast[int](ctx.stack[ctx.sp])
        eix   = ctx.obj.staticdata.find('\0', zptr)
        key   = ctx.obj.staticdata[zptr ..< eix]
        vopt  = ctx.attrs.lookup(key)
        val   = vopt.get()
        topt  = ctx.attrTypes.lookup(key)
        tval  = cast[pointer](topt.get())

      ctx.stack[ctx.sp]     = val
      ctx.stack[ctx.sp + 1] = tval
    of ZAssignAttr:
      let
        zptr  = cast[int](ctx.stack[ctx.sp])
        eix   = ctx.obj.staticdata.find('\0', zptr)
        key   = ctx.obj.staticdata[zptr ..< eix]

      ctx.sp += 2

      let value = ctx.stack[ctx.sp]
      ctx.sp += 1
      let vtype = cast[TypeId](ctx.stack[ctx.sp])
      ctx.sp += 1

      ctx.attrs[key]     = value
      ctx.attrTypes[key] = vtype
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
      var err: bool
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
        discard # Not implemented yet.
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
        discard
      of FDictIndex:
        discard
      of FSlice:
        discard
      of FAssignIx:
        discard
      of FAssignDIx:
        discard
      of FassignSlice:
        discard
      of FLoadLit:
        discard
      of FContainerLit:
        # TODO, get the output type precise. This isn't good enough.
        var
          num = cast[int](ctx.stack[ctx.sp])
          ty  = instr.typeInfo
          err:      string
          contents: seq[pointer]

        for i in 0 ..< num:
          ctx.sp += 2
          contents.add(ctx.stack[ctx.sp])

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

        ctx.stack[ctx.sp + 1] = cast[pointer](TString)
        # TODO: we're not managing this pointer. Needs to be tracked.
        # right now we're just leaking it.
        let s             = cast[pointer](call_repr(arg, argTy))
        ctx.stack[ctx.sp] = cast[pointer](s)

      of FPlusEqRef, FGetFFIAddr, FInitialize, FCleanup:
        discard # Not implemented yet.
      else:
        ctx.bailHere("RT_badTOp", @[$(instr.arg)])

    of Z0Call:
      # `toSave` will get pushed onto the stack, and it's the current
      # instruction pointer plus the module ID in one word.
      let
        toSave     = ctx.ip shl 32 or ctx.curModule.arenaId
        fobj       = ctx.obj.funcInfo[instr.arg - 1]
        oldModule  = ctx.curModule

      # push toSave.
      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cast[pointer](toSave)
      # push the old frame pointer.
      ctx.sp           -= 1
      ctx.stack[ctx.sp] = cast[pointer](ctx.fp)
      # The new frame pointer is wherever the stack is.
      ctx.fp            = ctx.sp
      # Calculate the new instruction ponter; the fobj has a byte offset,
      # but we're indexing by instruction.
      ctx.ip            = fobj.offset div sz

      ctx.curModule       = ctx.obj.moduleContents[instr.arena - 1]
      let nextInstruction = ctx.curModule.instructions[ctx.ip]

      ctx.pushFrame(oldmodule, instr.lineno, nextInstruction.lineno,
                    fobj, ctx.curModule)
    of ZCallModule:
      let
        toSave = ctx.ip shl 32 or ctx.curModule.arenaId
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

    of ZRet:
      leaveFrame()
    of ZModuleRet:
      if ctx.numFrames <= 2:
        return
      leaveFrame()
    of ZFFICall:
      var
        ffiObj  = ctx.obj.ffiInfo[instr.arg]
        p       = addr ctx.obj.staticData[instr.arg]
        s       = $(cast[cstring](p))
        n       = ffiObj.argInfo.len() - 1 # Last is the return value
        sp      = ctx.sp
        argAddr = pointer(nil)
        args: seq[pointer]

      # For each data type, we should be calling the type API to
      # handle translation, memory management, etc.
      # But for now, we'll just directly send stuff along and
      # assume this implementation.
      for i in 0 ..< n:
        args.add(addr ctx.stack[sp])
        sp += 2

      if n > 0:
        argAddr = addr args[0]

      ffi_call(ctx.externCalls[instr.arg], ctx.externFps[instr.arg],
               addr ctx.returnRegister, argAddr)
      ctx.rrType = cast[pointer](int64(ffiObj.argInfo[^1].ourType))

    of ZSObjNew:
      # TODO: memory management.
      let
        address = ctx.storageAddr(instr, instr.immediate)
        obj     = alloc0(instr.arg)

      copyMem(obj, address, instr.arg)
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
    of ZIndexedLoad, ZSliceLoad, ZAssignSlice, ZAssignToIx:
      discard

    ctx.ip += 1

proc setupArena(ctx: RuntimeState, typeInfo: seq[(int, TypeId)], arenaSz: int) =
  # 128 bits per item.
  var arena = newSeq[pointer](arenaSz * 16)

  for (offset, tid) in typeInfo:
    arena[offset + 1] = cast[pointer](tid)

  ctx.arenas.add(arena)

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

proc executeObject*(obj: ZObjectFile): int =
  var ctx = RuntimeState()

  ctx.obj       = obj
  ctx.curModule = obj.moduleContents[obj.nextEntrypoint - 1]
  ctx.numFrames = 1
  ctx.sp        = STACK_SIZE
  ctx.fp        = ctx.sp

  # Add arenas.
  ctx.setupFfi()
  ctx.setupArena(obj.symTypes, obj.globalScopeSz)

  for item in obj.moduleContents:
    ctx.setupArena(item.symTypes, item.arenaSize)
  ctx.attrs.initDict()
  ctx.attrTypes.initDict()
  ctx.pushFrame(ctx.curModule, 0, 0, nil, ctx.curModule)
  return ctx.runMainExecutionLoop()
