import "."/[common, err]

proc run_0c00l_vm*(ctx: RuntimeState): int {.importc, cdecl.}

template pushFrame*(ctx: RuntimeState, cm: ZModuleInfo, cl: int32,
                   tl: int32, tf: ZFnInfo, tm: ZModuleInfo) =
  if ctx.numFrames == MAX_CALL_DEPTH:
    ctx.runtimeError("StackOverflow", cm, cl)

  ctx.frameInfo[ctx.numFrames].callmodule   = cm
  ctx.frameInfo[ctx.numFrames].calllineno   = cl
  ctx.frameInfo[ctx.numFrames].targetline   = tl
  ctx.frameInfo[ctx.numFrames].targetfunc   = tf
  ctx.frameInfo[ctx.numFrames].targetmodule = tm
  ctx.numFrames += 1

template popFrame*(ctx: RuntimeState) =
  ctx.numFrames -= 1

proc toString(x: TypeSpec): string {.importc, cdecl.}

proc fn_repr(c: ptr ZCallback): cstring {.exportc, cdecl.} =
  let
    rt  = get_con4m_runtime()
    eix = rt.obj.staticdata.find('\0', c.nameoffset)
    n   = rt.obj.staticdata[c.nameOffset ..< eix]
    s   = c4str("func " & n & c.tid.toString())

  return cast[cstring](s)

proc fn_eq(c1: ptr ZCallback, c2: ptr ZCallback): bool {.exportc, cdecl.} =
  return c1.impl == c2.impl

proc fn_copy(c1: ptr ZCallback): ptr ZCallback {.exportc, cdecl.} =
  result = ZCallback.create()
  result.impl       = c1.impl
  result.nameoffset = c1.nameoffset
  result.tid        = c1.tid
  result.ffi        = c1.ffi

template z_native_call*(ctx: RuntimeState, funcid: int,
                        instr: ptr ZInstruction = nil) =
  ## This is the raw 0c00l call implementation for 0c00l byte code.
  ## It assumes the virtual machine state is set up, so should only
  ## be called internally.
  ##
  ## If your application used con4m to load a config, then you're calling
  ## con4m callbacks when execution finished, push arguments on and
  ## instead call `run_callback()`.

  # `toSave` will get pushed onto the stack, and it's the current
  # instruction pointer plus the module ID in one word.
  let
    toSave     = ctx.ip shl 32 or ctx.curModule.moduleId
    fobj       = ctx.obj.funcInfo[funcid - 1]
    module     = fobj.mid
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
  ctx.ip            = fobj.offset div sizeof(ZInstruction)

  ctx.curModule       = ctx.obj.moduleContents[module - 1]
  let nextInstruction = addr ctx.curModule.instructions[ctx.ip]
  let ln = if instr != nil: instr.lineno else: -1
  ctx.pushFrame(oldmodule, ln, nextInstruction.lineno, fobj, ctx.curModule)

template initForeignModule(): ZModuleInfo =
  ZModuleInfo(modname: "external call", moduleId: -2, initSize: 2,
              instructions: @[
                ZInstruction(op: Z0Call),
                ZInstruction(op: ZHalt)
                ])

proc run_validator(ctx: RuntimeState, startswith: string):
                   FlexArray[pointer] {.cdecl, importc.}

proc foreign_z_call*(ctx: RuntimeState, funcid: int):
                   pointer {.exportc, cdecl.} =
  # We use a dummy module with two instructions, a call and a halt,
  # then we re-start the VM using that as an entry point.
  # Any result is left in the return register.

  if ctx.foreignModule == nil:
    ctx.foreignModule = initForeignModule()

  ctx.foreignModule.instructions[0].arg = int32(funcid)

  ctx.curModule = ctx.foreignModule
  ctx.ip        = 0

  # TODO: if we get an exit code, we should deal with that; we
  # currently just ignore it here.
  discard ctx.run_0c00l_vm()

  result = ctx.returnRegister

  # We reset the sp to the bottom after a foreign call.
  ctx.sp        = STACK_SIZE

  # Run validation unilaterally for now.
  let validation_errors = ctx.run_validator("")

  if len(validation_errors) != 0:
    raise newException(ValueError, "Post-callback state validation failed!")


proc z_ffi_call*(ctx: RuntimeState, ix: int) {.exportc, cdecl.} =
  var
    ffiObj           = ctx.obj.ffiInfo[ix]
    # p                = addr ctx.obj.staticData[ffiObj.nameOffset]
    # s                = $(cast[cstring](p))
    n                = ffiObj.argInfo.len() - 1 # Last is the return value
    sp               = ctx.sp
    m: pointer
    argAddr: pointer = pointer(nil)
    args:    seq[pointer]

  # For each data type, we should be calling the type API to
  # handle translation, memory management, etc.
  # But for now, we'll just directly send stuff along and
  # assume this implementation.
  #
  # The only difference is if a local parameter is declared to be
  # generic, then we currently will box the parameter into a
  # Mixed object.

  for i in 0 ..< n:
    let t = cast[TypeSpec](ctx.stack[sp + 1]) # ffiObj.argInfo[i].ourType is wrong

    if ffiObj.argInfo[i].ourType == RTAsMixed:
      m = cast[pointer](ctx.stack[sp])
      args.add(addr m)
    else:
      args.add(addr ctx.stack[sp])
    sp += 2

  if n > 0:
    argAddr = addr args[0]

  ffi_call(ctx.externCalls[ix], ctx.externFps[ix],
           addr ctx.returnRegister, argAddr)
  ctx.rrType = cast[pointer](int64(ffiObj.argInfo[^1].ourType))


proc push_call_param*(ctx: RuntimeState, p: pointer,
                      t: TypeSpec) {.exportc, cdecl.} =
  ctx.sp -= 1
  ctx.stack[ctx.sp] = cast[pointer](t)
  ctx.sp -= 1
  ctx.stack[ctx.sp] = p

proc run_callback_internal*(ctx: RuntimeState, cb: ptr ZCallback,
                            args: openarray[(pointer, TypeSpec)] = []):
    pointer {.discardable, exportc, cdecl.} =
  ## Note that you pass args in the 'correct' order, and this will
  ## push them on backwards, because that's the convention.
  ##
  ## This call is agnostic to whether the VM is already running or
  ## not; the VM will use this to run callback objects, and you can
  ## also use it externally.
  ##
  ## However, it's important to note that the VM is only single
  ## threaded, so do not call this externally if running in parallel
  ## to the VM, until it has definitely completed.
  ##
  ## This only returns a value in cases where it's being called
  ## externally, and will be nil if the function called returns
  ## no value.

  var i = args.len()

  while i != 0:
    i -= 1
    let (p, t) = args[i]
    ctx.push_call_param(p, t)

  if cb.ffi:
    ctx.z_ffi_call(cast[int](cb.impl))
  elif ctx.running:
    # The generated code will, in this branch, push the result
    # if merited.
    let cur_ip = addr ctx.curModule.instructions[ctx.ip]

    ctx.z_native_call(cast[int](cb.impl), cur_ip)
  else:
    return ctx.foreign_z_call(cast[int](cb.impl))

proc find_string_at(mem: string, offset: int): string {.importc, cdecl.}

proc baseunify(id1, id2: TypeSpec): TypeSpec {.importc, cdecl.}

proc run_callback*(ctx:  RuntimeState,
                   name: string,
                   t:    TypeSpec,
                   args: openarray[(pointer, TypeSpec)] = [],
                  ffi = false): pointer {.discardable, exportc, cdecl.} =
  ## This version of run_callback finds the function to call, and
  ## creates the callback pointer for you, based on the name and
  ## signature.
  ##
  ## By default, it only searches native functions, but the optional
  ## 'ffi' flag will cause it to search both.
  ##
  ## This version will never get called by the runtime, so it expects
  ## that it is ONLY called when the VM is idle.

  var i = args.len()

  while i != 0:
    i -= 1
    let (p, t) = args[i]
    ctx.push_call_param(p, t)

  for i, item in ctx.obj.funcInfo:
    if item.funcname != name:
      continue
    if t == item.tid or not unify(t, item.tid).tspec_is_error():
      return ctx.foreign_z_call(i + 1)
