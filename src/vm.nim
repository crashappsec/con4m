# This is just a basic runtime with no thought to performance; we'll
# do one more focused on performance later, probably directly in C.
import "."/[stchecks, attrstore, modparams, vm_func]
export attrstore

proc load_spec() {.cdecl, importc.}
proc using_spec(ctx: RuntimeState): bool {.importc, cdecl.}

const sz = sizeof(ZInstruction)


proc bailHere*(ctx: RuntimeState, errCode: string, extra: seq[string] = @[]) =
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
  ctx.print_con4m_trace()
  quit(-1)

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

  proc moduleScopeRepr(ctx: RuntimeState, title = "Module scope"): Rope =
    var rows: seq[seq[Rope]] = @[]

    rows.add(@[text("Variable"), text("Value"), text("Offset")])

    for (offset, name) in ctx.curModule.datasyms.items(sort=true):
      let
        raw = ctx.moduleAllocations[ctx.curModule.moduleId][offset * 2]
        val = toHex(cast[int](raw)).toLowerAscii()
      rows.add(@[text(name), text(val), text($(offset))])

    return rows.quickTable(title = title)

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

proc get_current_instruction*(ctx: RuntimeState):
                            ptr ZInstruction {.exportc, cdecl.} =
  return addr ctx.curModule.instructions[ctx.ip]


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
  print_err(r("Assertion [red]failed[/] in module: " & modname &
            " (line [i]" & $(lineno) & "[/])"))
  print_err(bold(line))

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
      ctx.stack[ctx.sp] = cast[pointer](ctx.rrType)
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

      let dst = cast[ptr TypeSpec](addr ctx.stack[ctx.sp + 1])

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
          oldtype: TypeSpec
          err:     bool

        olditem = ctx.get(key, err, addr oldType)

        ctx.bailHere("LockedAttr",
                     @[$(key),
                       con4m_repr(olditem).toNimStr(),
                       con4m_repr(value).toNimStr()])

    of ZLockOnWrite:
      let
        zptr  = cast[int](ctx.stack[ctx.sp])
        eix   = ctx.obj.staticdata.find('\0', zptr)
        key   = ctx.obj.staticdata[zptr ..< eix]

      ctx.sp += 2

      var
        newInfo = AttrContents(lockOnWrite: true)
        oldInfo: AttrContents

      let infOpt = ctx.attrs.lookup(r(key))
      if infOpt.isSome():
        oldInfo = infOpt.get()
        if oldInfo.locked:
            ctx.bailHere("AlreadyLocked", @[$(key)])
        newInfo.contents = oldInfo.contents
        newInfo.tid      = oldInfo.tid
        newInfo.isSet    = oldInfo.isSet

      ctx.attrs[r(key)] = newInfo
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
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])

        ctx.stack[ctx.sp + 1] = cast[pointer](tspec_utf8())
        let s = con4m_repr(arg)
        ctx.stack[ctx.sp] = cast[pointer](s)
      of FCastFn:
        let
          srcType = instr.typeInfo
          dstType = cast[TypeSpec](ctx.stack[ctx.sp])
          obj     = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = con4m_cast(obj, srcType, dstType)
        ctx.stack[ctx.sp + 1] = cast[pointer](dstType)

      of FEq:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = cast[pointer](con4m_eq(argTy, arg1, arg2))
        ctx.stack[ctx.sp + 1] = cast[pointer](tspec_bool())
      of FLt:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = cast[pointer](con4m_lt(argTy, arg1, arg2))
        ctx.stack[ctx.sp + 1] = cast[pointer](tspec_bool())
      of FGt:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        ctx.stack[ctx.sp]     = cast[pointer](con4m_gt(argTy, arg1, arg2))
        ctx.stack[ctx.sp + 1] = cast[pointer](tspec_bool())
      of FAdd:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        if argTy.isIntBuiltin():
          ctx.stack[ctx.sp] = cast[pointer](cast[int](arg1) + cast[int](arg2))
        if argTy.isFloatBuiltin():
          ctx.stack[ctx.sp] = cast[pointer](cast[float64](arg1) +
                                            cast[float64](arg2))
        else:
          ctx.stack[ctx.sp] = con4m_add(arg1, arg2)
      of FSub:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2

        if argTy.isIntBuiltin():
          ctx.stack[ctx.sp] = cast[pointer](cast[int](arg1) - cast[int](arg2))
        if argTy.isFloatBuiltin():
          ctx.stack[ctx.sp] = cast[pointer](cast[float64](arg1) -
                                            cast[float64](arg2))
        else:
          ctx.stack[ctx.sp] = con4m_sub(arg1, arg2)
      of FMul:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2

        if argTy.isIntBuiltin():
          ctx.stack[ctx.sp] = cast[pointer](cast[int](arg1) * cast[int](arg2))
        if argTy.isFloatBuiltin():
          ctx.stack[ctx.sp] = cast[pointer](cast[float64](arg1) *
                                            cast[float64](arg2))
        else:
          ctx.stack[ctx.sp] = con4m_mul(arg1, arg2)

      of FIDiv:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2
        if argTy.isIntBuiltin():
          let
            res = cast[int](arg1) div cast[int](arg2)
          ctx.stack[ctx.sp] = cast[pointer](res)
        if argTy.isFloatBuiltin():
          let
            res = cast[float](arg1) / cast[float](arg2)
          ctx.stack[ctx.sp] = cast[pointer](res)
        else:
          ctx.stack[ctx.sp] = con4m_div(arg1, arg2)

      of FFDiv:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2

        let
          res = cast[int](arg1) / cast[int](arg2)

        ctx.stack[ctx.sp]     = cast[pointer](res)
        ctx.stack[ctx.sp + 1] = cast[pointer](tspec_f64())
      of FMod:
        let
          arg2  = ctx.stack[ctx.sp]
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = ctx.stack[ctx.sp + 2]

        ctx.sp += 2

        let
          res = cast[int](arg1) mod cast[int](arg2)
        ctx.stack[ctx.sp] = cast[pointer](res)

      of FShl:
        let
          arg2  = cast[int](ctx.stack[ctx.sp])
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = cast[int](ctx.stack[ctx.sp + 2])

        ctx.sp += 2
        ctx.stack[ctx.sp] = cast[pointer](arg1 shl arg2)

      of FShr:
        let
          arg2  = cast[int](ctx.stack[ctx.sp])
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = cast[int](ctx.stack[ctx.sp + 2])

        ctx.sp += 2
        ctx.stack[ctx.sp] = cast[pointer](arg1 shr arg2)

      of FBand:
        let
          arg2  = cast[int](ctx.stack[ctx.sp])
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = cast[int](ctx.stack[ctx.sp + 2])

        ctx.sp += 2
        ctx.stack[ctx.sp] = cast[pointer](arg1 and arg2)

      of FBor:
        let
          arg2  = cast[int](ctx.stack[ctx.sp])
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = cast[int](ctx.stack[ctx.sp + 2])

        ctx.sp += 2
        ctx.stack[ctx.sp] = cast[pointer](arg1 or arg2)

      of FBxor:
        let
          arg2  = cast[int](ctx.stack[ctx.sp])
          argTy = cast[TypeSpec](ctx.stack[ctx.sp + 1])
          arg1  = cast[int](ctx.stack[ctx.sp + 2])

        ctx.sp += 2
        ctx.stack[ctx.sp] = cast[pointer](arg1 xor arg2)

      of FIndex, FDictIndex:
        # TODO: wrap the exception handling to catch the error.

        let
          ix = ctx.stack[ctx.sp]
          c  = ctx.stack[ctx.sp + 2]
          r  = con4m_index_get(c, ix)
          t  = get_my_type(cast[C4Obj](c))

        ctx.sp += 2
        ctx.stack[ctx.sp]     = r
        ctx.stack[ctx.sp + 1] = t.get_param(t.num_params() - 1)

      of FSlice:
        let
          endIx = cast[int](ctx.stack[ctx.sp])
          stIx  = cast[int](ctx.stack[ctx.sp + 2])
          c     = ctx.stack[ctx.sp + 4]

        ctx.sp += 4
        # Type is already correct on the stack, since we're writing
        # over the container location and this is a slice.
        ctx.stack[ctx.sp] = con4m_slice_get(c, stIx, endIx)

      of FAssignIx, FAssignDIx:
        let
          ix = cast[pointer](ctx.stack[ctx.sp])
          cp = cast[ptr pointer](ctx.stack[ctx.sp + 2])
          c  = cp[]
          ob = ctx.stack[ctx.sp + 4]                 # Data to assign

        ctx.sp += 6
        con4m_index_set(c, ob, ix)

        #if err:
        #  ctx.bailHere("ArrayIxErr", @[ $(ix) ])
      of FassignSlice:
        let
          endix = cast[int](ctx.stack[ctx.sp])
          start = cast[int](ctx.stack[ctx.sp + 2])
          cp    = cast[ptr pointer](ctx.stack[ctx.sp + 4])
          c     = cp[]
          ob    = ctx.stack[ctx.sp + 6] # Data to assign

        ctx.sp += 8
        con4m_slice_set(c, start, endix, ob)

      of FContainerLit:
        var
          num = cast[int](ctx.stack[ctx.sp])
          ty  = instr.typeInfo
          err:      string
          contents: seq[pointer]

        for i in 0 ..< num:
          ctx.sp += 2
          contents = @[ctx.stack[ctx.sp]] & contents

        # TODO: push the litmod in codegen and use it here.
        ctx.stack[ctx.sp] = instantiate_container(ty, contents)

      of FCopy:
        let
          arg   = ctx.stack[ctx.sp]
          res   = con4m_copy(arg)
        ctx.stack[ctx.sp] = res
      of FLen:
        let
          arg   = ctx.stack[ctx.sp]
          argTy = instr.typeInfo

        ctx.stack[ctx.sp + 1] = cast[pointer](tspec_i64())
        # TODO: we're not managing this pointer. Needs to be tracked.
        # right now we're just leaking it.
        let s             = cast[pointer](con4m_len(arg))
        ctx.stack[ctx.sp] = cast[pointer](s)

      of FPlusEqRef, FGetFFIAddr, FInitialize, FCleanup, FLoadLit:
        unreachable # Not implemented yet, or not called via ZTCall.
      else:
        ctx.bailHere("RT_badTOp", @[$(instr.arg)])

    of Z0Call:
      ctx.z_native_call(instr.arg, addr instr)
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
      ctx.run_callback_internal(cbObj)
    of ZRet:
      leaveFrame()
    of ZModuleEnter:
      if instr.arg != 0:
        for i, param in ctx.curModule.parameters:
          var
            val: pointer
            t:   TypeSpec

          ## Fill in all parameter values now. If there's a validator,
          ## it will get called after this loop, along w/ a call to
          ## ZParamCheck.

          if param.attr.rich_len() != 0:
            let attropt = ctx.attrs.lookup(param.attr)
            if attropt.isNone():
              (val, t) = ctx.get_param_value(param)
              discard ctx.set(param.attr.toNimStr(), cast[pointer](val),
                              param.tid, lock = true,
                              internal = true)
          else:
            let
              p        = param.offset
              id       = ctx.curModule.moduleId
              a        = addr ctx.moduleAllocations[id][p * 2]
              address  = cast[ptr pointer](a)
              typeaddr = cast[ptr pointer](cast[uint](address) +
                                           uint(sizeof(pointer)))
            if typeaddr[] == nil: # TBottom
              (val, t)   = ctx.get_param_value(param)
              address[]  = val
              typeaddr[] = cast[pointer](t)

        ctx.module_lock_stack.add(int32(ctx.curModule.moduleId))
      else:
        if ctx.module_lock_stack.len() == 0:
          ctx.module_lock_stack.add(0)
        else:
           ctx.module_lock_stack.add(ctx.module_lock_stack[^1])

    of ZParamCheck:
      let s = cast[Rich](ctx.stack[ctx.sp])

      ctx.sp += 2

      if s != nil and s.len() != 0:
        var
          param   = ctx.curModule.parameters[instr.arg]
          modname = ctx.curModule.modname
          err     = s.toNimStr()
          name: string

        if param.attr.con4m_len() != 0:
          name = param.attr.toNimStr()
        else:
          name = cast[Rich](ctx.curModule.datasyms[param.offset]).toNimStr()

        ctx.runtimeError("ParamNotValid", @[name, modname, err])

    of ZModuleRet:
      if ctx.numFrames <= 2:
        return 0
      discard ctx.module_lock_stack.pop()
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
      var
        address = cast[cstring](addr ctx.obj.staticData[instr.immediate])

      ctx.sp -= 1
      ctx.stack[ctx.sp] = cast[pointer](instr.typeInfo)
      ctx.sp -= 1
      ctx.stack[ctx.sp] = unmarshal_obj(address, ctx.obj.staticData.len())
      raise newException(ValueError, "TODO: Fix the need for len()")

    of ZAssignToLoc:
      let
        address  = cast[ptr pointer](ctx.stack[ctx.sp])
        typeaddr = cast[ptr pointer](cast[uint](address) +
                                     uint(sizeof(pointer)))
      ctx.sp += 1
      typeaddr[] = ctx.stack[ctx.sp]
      ctx.sp += 1
      address[] = ctx.stack[ctx.sp]
      ctx.sp += 2
    of ZAssert:
      if ctx.stack[ctx.sp] != nil:
        ctx.sp += 2
      else:
        ctx.showAssertionFailure(instr)
        return -1
    of ZTupleStash:
      ctx.tupleStash = cast[CTuple](ctx.stack[ctx.sp])
      ctx.stashType  = ctx.stack[ctx.sp + 1]
      ctx.sp += 2

    of ZUnpack:
      let
        n  = instr.arg
        to = instr.typeInfo.followForwards()

      var
        err: bool
        tup      = ctx.tupleStash

      for i in 0 ..< n:
        var
          address  = cast[ptr pointer](ctx.stack[ctx.sp])
          typeaddr = cast[ptr pointer](cast[uint](address) +
                                       uint(sizeof(pointer)))
          itemType = to.get_param(cint(i))

        ctx.sp += 2
        let
          val = cast[ptr pointer](tup[i])

        address[]  = val
        typeaddr[] = cast[pointer](itemType)

    of ZBail:
      let s = cast[cstring](ctx.stack[ctx.sp])
      print(fgColor("error: ", "red") + text($s))
      return -1

    ctx.ip += 1

proc setupArena(ctx:        RuntimeState,
                moduleIdSz: int) =
  # 128 bits per item.
  var moduleAllocation = newSeq[pointer](moduleIdSz * 2)

  #for (offset, tid) in typeInfo:
  #  moduleAllocation[offset*2 + 1] = cast[pointer](tid)

  ctx.moduleAllocations.add(moduleAllocation)

proc setupFfi(ctx: var RuntimeState, compiling: bool) =
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


    p       = cast[cstring](addr obj.staticData[item.nameOffset])
    fname   = $(p)
    numArgs = cuint(item.argInfo.len() - 1)

    if item.va:
      ctx.objLoadWarn("ExternVarargs", args = @[fname])
      continue

    for dllNameOffset in item.dlls:
      p = cast[cstring](addr obj.staticData[dllNameOffset])
      dlls.add($p)

    let fptr = findSymbol(fname, dlls)

    if fptr == nil and not compiling:
      ctx.objLoadWarn("MissingSym", @[fname])
      continue

    ctx.externFps[i] = fptr

    if numArgs > 0:
      for j in 0 ..< numArgs:
        # Do not do the return value.
        ctx.externArgs[i].add(ffiTypeNameMapping[item.argInfo[j].argType])
      argp = addr ctx.externArgs[i][0]

    var retType = ffiTypeNameMapping[item.argInfo[^1].argType]

    ffi_prep_cif(ctx.externCalls[i], ffiAbi, numargs, retType, argp)

proc run_0c00l_vm*(ctx: RuntimeState): int {.exportc, cdecl.} =
  ctx.pushFrame(ctx.curModule, 0, 0, nil, ctx.curModule)
  ctx.running = true
  result = ctx.runMainExecutionLoop()
  ctx.running = false
  ctx.popFrame()

proc setup_first_execution*(ctx: var RuntimeState) {.exportc, cdecl.} =
  currentRuntime = ctx
  ctx.curModule  = ctx.obj.moduleContents[ctx.obj.entrypoint - 1]
  ctx.numFrames  = 1
  ctx.sp         = STACK_SIZE
  ctx.fp         = ctx.sp

  ctx.attrs.initDict()
  ctx.allSections.initDict()

  if ctx.using_spec():
    ctx.applyOneSectionSpecDefaults("", ctx.obj.spec.rootSpec)

  # Add module allocations.
  ctx.setupFfi(compiling = true)
  ctx.setupArena(ctx.obj.globalScopeSz)
  for i, item in ctx.obj.moduleContents:
    ctx.setupArena(item.moduleVarSize)


proc execute_object*(ctx: var RuntimeState): int {.exportc, cdecl.} =
  ## This call is intended for first execution, not for save /
  ## resumption.
  ctx.setup_first_execution()
  return ctx.run_0c00l_vm()

proc resume_object*(ctx: var RuntimeState): int {.exportc, cdecl.} =
  ## First unmarshal an object with unmarshal_runtime().
  currentRuntime = ctx

  var reentrypt  = ctx.obj.next_entrypoint

  if reentrypt - 1 >= ctx.obj.moduleContents.len() or reentrypt <= 0:
    print fgcolor("warning: ", "yellow") +
          text("No resumption module was specified; ") +
          text("Re-executing from the original entry point, ") +
          text("with ") + em("saved") + text(" state intact.")

    reentrypt = ctx.obj.entrypoint - 1

  ctx.curModule = ctx.obj.moduleContents[reentrypt]
  ctx.numFrames = 1
  ctx.sp        = STACK_SIZE
  ctx.fp        = ctx.sp

  ctx.setupFfi(compiling = false)

  return ctx.run_0c00l_vm()
