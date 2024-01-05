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

import ztypes/api, strutils, irgen

type
  ZOp* {.size: 1.} = enum
     # The first four pushes are all pushes from variables to the stack.
     # So if we were explicitly going through registers, this would be
     # load + push.
     #
     #
     #
     # The 64-bit argument to the instruction will contain the address
     # from which to load.
     ZPushO         = 0x10, # Push an ordinal type (no need to type check)
     ZPushF         = 0x11, # Push a float type
     ZPushVal       = 0x12, # Push some other primitive value that is
                            # passed by value (e.g., a size object)

     # Push a pointer relative to the offset of the scope start.  This
     # might be a data object pointer, or it might be an attribute
     # name, as we call this before assigning or loading attributes.
     ZPushPtr       = 0x13,

     # Push a pointer relative to our static data.
     ZPushStaticPtr = 0x14,

     # Push an immediate value onto the stack (the value coming from
     # the instruction itself).
     ZPushImm       = 0x15,

     # Pushes the type of a a heap object onto the stack
     # for purposes of runtime type comparisons.
     ZPushSType     = 0x16,

     # Push the address of a variable that lives either on the stack
     # or the heap (not an attribute).  The intent here is not to
     # access the contents, but to replace the object.  So if we do:
     # `x = y`, where `y` is an `int`, this loads the address of `x`,
     # not the value of `x`.
     #
     # For *object* types, there's no difference here, until we add
     # references to the language.
     ZPushAddr      = 0x17,

     # A lot of the below operations implicitly pop args from the
     # stack once they consume them. This allows us to keep an arg
     # around. Particularly, we use this for switch-style statements,
     # to keep the value we're testing against around (since our
     # current VM model is stack-only).
     ZDupTop        = 0x18,

     # When this instruction is called, the stack is popped, the value is
     # checked for a C-style string; that attribute is loaded.
     ZLoadFromAttr  = 0x19,

     # The top of the stack has a heap object and an index; replace
     # with the contents at that index. Note that the index might not
     # be directly on the stack, it might be a string, etc.  Check the
     # type field for this one.
     ZIndexedLoad   = 0x1a,

     # Load a slice. Here, the indicies are always ints.
     ZSliceLoad     = 0x1b,

     # Pop a value from the stack, discarding it.
     ZPop           = 0x20,

     # Store a copy of the top of the stack into a memory address
     # from a stack offset.
     ZStoreTop      = 0x21,

     # Store an immediate value (encoded in the instruction) into the
     # memory address, ignoring the stack.
     ZStoreImm      = 0x22,

     # Tests the top of the stack... if it's a zero value, it jumps to
     # the indicated instruction. The jump argument is a relative
     # offset, and is measured in BYTES, which obviously will
     # be a multiple of the opcode size.
     #
     # Note that, when we're backpatching jumps, we hold onto an
     # absolute index into the opcode table, not a byte index. We
     # don't convert to a relative offset until storing the info.
     #
     # No matter the value, the two conditional jumps pop the top of
     # the stack.
     ZJz            = 0x30,
     ZJnz           = 0x31,

     # No popping for an unconditional jump.
     ZJ             = 0x32,

     # This calls our internal data type API. If there's a return value,
     # it's left on the stack.
     ZTCall         = 0x33,

     # This calls a function, which can include the module's top-level
     # code, with the opcode encoding the arena (module id) and the
     # byte offset into that arena's code.
     #
     # It assumes arguments are pushed, but then:
     # 1. Pushes the return address
     # 2. Saves the base pointer to the stack.
     # 3. Sets the value of the base pointer to point to the saved
     #    base pointer. That means the first argument will live at -16.
     # 4. It jumps to the entry point.
     Z0Call         = 0x34,

     # This runs a FFI call.
     ZFFICall       = 0x35,

     # This maps to a 'use' statement; it calls the initialization code
     # of a module.
     ZCallModule    = 0x36,

     # Duh.
     ZNot           = 0x50,

     # Unmarshal stored data from the heap, instantiating an object.
     # the data in the heap should be considered read-only.
     ZSObjNew       = 0x60,

     # The stack contains a heap pointer and a value to assign at
     # that address.
     ZAssignToLoc   = 0x70,

     # The stack contains a heap pointer to a data object, an index
     # (which might be a non-int type like a pointer), and a value
     # to assign at the given index.
     ZAssignToIx    = 0x71,

     # Same as prev in concept, but with the extra index to indicate
     # the end of the slice.
     ZAssignSlice   = 0x72,

     # Whereas with `ZAssignToLoc`, the stack containsa pointer to a variable,
     # here it contains a pointer to a heap-alloc'd string that is the name
     # of the attribute.
     ZAssignAttr    = 0x73,

     # This just needs to restore the old base pointer, and jump to
     # the saved return address.
     ZRet           = 0x80,

     # This is a return, unless the module is the entry point, in
     # which case it is a halt.
     ZModuleRet     = 0x81,
     ZHalt          = 0x82,

     # SetRet should be called by any function before exiting, placing
     # the return value in the one user register in our 'machine'. The
     # top of the stack is popped and placed in that register, overwriting
     # anything there.
     ZSetRes        = 0x90,

     # This pushes whatever is in the return register onto the stack;
     # It does NOT clear the register.
     ZPushRes       = 0x91,

     # A no-op. These double as labels for disassembly too.
     ZNop           = 0xff

  ZProgParam* = object
    # `memloc` specifies where the parameter should be stored. This is
    # not a raw memory pointer. It's either an offset from a module's
    # arena, or a pointer to the global static data, where the
    # attribute name is kept.
    #
    # Which of the above applies is detemrined by the `arenaId` field;
    # the modules are all given positive indexes. If it's zero, then
    # it's an attribute.
    memloc*:    pointer
    paramName*: pointer # The source name of the variable or attribute.
    typePtr*:   pointer # The marshal'd type object.
    shortDoc*:  pointer # An offset to the short description start.
    longDoc*:   pointer
    validator*: pointer # An offset to validation code in the global arena.
    default*:   pointer # Depending on the type, either a value, or a ptr
    status*:    int32   # 0 for no value present.
    arenaId*:   int32

  ZInstruction* = object
    # Compilers should all pad this structure to 24 bytes, but just in case...
    op*:        ZOp     # 1 byte
    pad:        byte
    arena*:     int16   # Right now, 0 for global variables,
                        # -1 for stack, and then a positive number for
                        # modules. These don't really need to live in
                        # separate memory arenas, but we computed
                        # offsets on a per-module basis.
    srcId*:     int32   # Index of source info in the
    arg*:       int32   # An offset, call number, etc.
    immediate*: int64
    typeInfo*:  TypeId

  ZLoc* = object
    url*:     string
    modname*: string
    line*:    int
    col*:     int

  ZFFiAuxArgInfo* = object
    held*:    bool # Whether passing a pointer to the thing causes it
                   # to hold the pointer, in which case decref must
                   # be explicit.
    alloced*: bool # This passes a value back that was allocated
                   # in the FFI.
    argType*: uint16 # an index into the CTypeNames data structure in ffi.nim.
    ourType*: uint32 # To look up any FFI processing we do for the type.

  ZFFiInfo* = object
    baseInfo*:     CallerInfo
    externalName*: pointer
    dllName*:      pointer
    auxArgInfo*:   seq[ZffiAuxArgInfo]

  ZFnInfo* = ref object
    syms*:    Dict[int, string]
    offset*:  int
    size*:    int

    # Not used during runtime; a back-pointer for generation.
    fnRef*:   FuncInfo

  # This is all the data that will be in an "object" file; we'll
  # focus on being able to marshal and load these only.
  ZObject* = ref object
    zeroMagic*:      int = 0x0c001deac001dea
    zeroCoolVers*:   int = 0x01
    moduleContents*: Dict[string, ModuleInfo] # Order is important.
    types*:          Dict[int64, string]
    entrypoint*:     int64
    locInfo*:        seq[ZLoc]
    funcInfo*:       seq[ZFnInfo]
    globals*:        Dict[int, string]
    staticData*:     string

  ModuleInfo* = ref object
    syms*:           Dict[int, string]
    source*:         StringCursor
    arenaId*:        int

    # The above items get marshalled into the ZObject; the rest does not.
    processed*:      bool
    moduleRef*:      Module
    loopLocs*:       seq[(IrNode, int)]
    backpatchLocs*:  seq[(IrNode, int)]
    instructions*:   seq[ZInstruction]


  CodeGenState* = object
    cc*:             CompileCtx
    zobj*:           ZObject
    minfo*:          Dict[string, ModuleInfo]
    locMap*:         Dict[IrNode, ZLoc]
    mcur*:           ModuleInfo
    fcur*:           ZFnInfo
    curNode*:        IrNode


var tcallReverseMap = ["repr()", "cast()", "==", "<", ">", "+", "-",
                       "*", "/", "//", "%", "<<", ">>", "&", "|", "^",
                       "[]", "dict[]", "[:]", "[]=", "dict[]=",
                       "[:]=", "loadlit()", "containerlit()", "copy()",
                       "len()", "+=", "ffi()", "init()", "cleanup()",
                       "newlit()", ">max<"]

proc oneIrNode(ctx: var CodeGenState, n: IrNode)

template getOffset(sym: SymbolInfo): int64 =
  int64(sym.offset)

proc getLocationObj(ctx: var CodeGenState, n: IrNode): Zloc =
  let
    opt    = ctx.locMap.lookup(n)
    module = ctx.mcur.moduleRef

  if opt.isSome():
    return opt.get()

  if n == nil or n.parseNode == nil:
    result = ZLoc(url: module.url, modname: module.modname,
                  line: -1, col: -1)

  else:
    let tok  = n.parseNode.token

    result = ZLoc(url: module.url, modname: module.modname,
                  line: tok.lineNo,  col: tok.lineOffset)

  ctx.locMap[n] = result

proc addStaticObject(ctx: var CodeGenState, p: pointer, l: int): int =

  let arr = cast[cstring](p)

  if l == 0:
    return 0

  result = ctx.zobj.staticData.len()

  for i in 0 ..< l:
    ctx.zobj.staticData.add(arr[i])

  dealloc(p)

proc addStaticObject(ctx: var CodeGenState, s: string, addnil = true): int =
  if s.len() == 0:
    return 0

  result = ctx.zobj.staticData.len()

  ctx.zobj.staticData &= s
  if addnil:
    ctx.zobj.staticData.add('\x00')

proc getLocation(ctx: var CodeGenState): int32 =
  var
    obj = ctx.getLocationObj(ctx.curNode)
    ix  = ctx.zobj.locInfo.find(obj)

  if ix == -1:
    ix = ctx.zobj.locInfo.len()
    ctx.zobj.locInfo.add(obj)

  return int32(ix)

proc addInstruction(ctx: var CodeGenState, op: ZOp, arg: int = 0,
                    immediate: int64 = 0,  tid: TypeId = TBottom,
                    arena: int = ctx.mcur.arenaId) =
  var ins = ZInstruction(op: op, arena: int16(arena), arg: int32(arg),
                         immediate: immediate, srcId: ctx.getLocation(),
                         typeInfo: tid.followForwards())
  ctx.mcur.instructions.add(ins)

proc findStringAt(mem: string, offset: int): string =
  let endIx = mem.find('\0', offset)

  return mem[offset ..< endIx]

proc lookupLine*(ctx: ZObject, ins: ZInstruction): int =
  result = ctx.locInfo[ins.srcId].line

template hex(x: int): string =
  `$`((cast[int32](x)).toHex().toLowerAscii())


proc rawReprInstructions(module: ModuleInfo, ctx: ZObject, fn = 0): Rope =
  # This is really just for use during development.
  # Will do a more proper disassembler at some point.
  var
    mem = ctx.staticData
    cells: seq[seq[Rope]] = @[@[atom("Address"), atom("Op"), atom("Arg 1"),
                                atom("Arg 2"), atom("Tid"),
                                atom("Comment / label")]]
    row: seq[Rope]


  for i, item in module.instructions:
    var
      address = text(hex(i * sizeof(ZInstruction)))
      arg1 = text(" ")
      arg2 = text(" ")
      ty   = text(cast[TypeId](item.typeInfo).toString())
      lno  = ctx.lookupLine(item)
      line = text(if lno == -1: "" else: "#" & $(lno))

    if item.typeInfo == TBottom:
      ty = text("none")

    case item.op
    of ZNop:
      if item.arg != 0:
        var str = mem.findStringAt(item.immediate)

        if str.len > 30:
          str = str[0 .. 14] & " … " & str[^14 .. ^1]

        ty   = text(" ")
        line = strong(str)

    of ZTCall:
      arg1 = text(hex(item.arg))
      line = em(tCallReverseMap[item.arg])
    of ZPushO, ZPushF, ZPushVal, ZPushPtr, ZPushSType, ZPushAddr,
         ZStoreTop, ZStoreImm:
      arg1 = text("+" & hex(item.arg))

      if item.op in [ZStoreImm]:
        arg2 = text($(item.immediate))

      if item.arena == -1:
        line = em("-> " & ctx.funcInfo[fn].syms[item.arg])
      elif item.arena != 0:
        line = em("-> " & module.syms[item.arg])
      else:
        line = em("-> " & ctx.globals[item.arg])
    of ZPushStaticPtr:
      arg1 = text("+" & hex(item.arg))
      arg2 = text(mem.findStringAt(item.arg))
    of ZPushImm:
      arg2 = text($item.immediate)
    of ZModuleRet , ZRet, ZHalt:
      ty = text(" ")
    of ZJz, ZJnz, ZJ:
      if item.arg > 0:
        arg1 = text("+" & hex(item.arg))
      else:
        arg1 = text("-" & hex(-item.arg))
      line = italic("-> 0x" & hex(item.arg + (i * sizeof(ZInstruction))))
      ty = text(" ")
    of ZSObjNew:
      arg1 = text($(item.arg))
      arg2 = text("+" & hex(item.immediate))
    of ZPop:
      ty = text(" ")
    else:
      discard

    row = @[address, em($(item.op)), arg1, arg2, ty, line]

    cells.add(row)

  result = quickTable(cells)
  result.colWidths([(10, true), (15, true), (14, true), (16, true),
                    (0, false), (0, false)])

proc disassembly*(ctx: ZObject): Rope =
    for (_, item) in ctx.moduleContents.items():
      result = result + rawReprInstructions(item, ctx)

proc getSymbolArena(ctx: var CodeGenState, sym: SymbolInfo): int =
  var sym = sym

  while sym.actualSym != nil:
    sym = sym.actualSym

  # Here, the arena is always the module the symbol was defined in,
  # whether or not it's going on the stack.
  if sym.arena == 0 and not sym.global:
    # When we did our original scope scan on a function or module, we
    # didn't descend into block scopes.
    #
    # So here we we know the temporary is getting used, so ensure
    # there is name information for block scopes, but note that there
    # can be multiple blocks sharing the same memory cells on the
    # stack; right now, we just call any such variable "(tmp)"; we can
    # do something more sophisticated later...
    if ctx.fcur != nil:
      ctx.fcur.syms[sym.offset] = "(tmp)"
    else:
      ctx.mcur.syms[sym.offset] = "(tmp)"

    return ctx.mcur.arenaId
  else:
    return sym.arena

proc codeLoc(ctx: var CodeGenState): int =
  return ctx.mcur.instructions.len()

## Opcode generation
proc genPop(ctx: var CodeGenState, sym: SymbolInfo = nil,
            tid: TypeId = TBottom) =
  var ntid: TypeId = tid

  ## Pop should be as much as we know about the type we're popping.
  if sym == nil:
    ctx.addInstruction(ZPop, tid = ntid)
  else:
    let arena = ctx.getsymbolArena(sym)

    if ntid == TBottom:
      ntid = sym.tid

    ctx.addInstruction(ZStoreTop, sym.getOffset(), tid = ntid, arena = arena)
    ctx.addInstruction(ZPop, tid = ntid)

proc genNop(ctx: var CodeGenState) =
  ctx.addInstruction(ZNop)

proc genCopyStaticObject(ctx: var CodeGenState, offset: int,
                         l: int, tid: TypeId) =
  ctx.addInstruction(ZSObjNew, l, offset, tid)

proc genLabel(ctx: var CodeGenState, label: string) =
  ctx.addInstruction(ZNop, 1, ctx.addStaticObject(label))

proc genPush(ctx: var CodeGenState, sym: SymbolInfo) =
  var
    arena = ctx.getSymbolArena(sym)
    op:   ZOp

  if sym.tid.isValueType():
    if sym.tid.isIntType() or sym.tid.isBoolType():
      op = ZPushO # Ordinal type
    elif sym.tid.isFloatType():
      op = ZPushF
    else:
      op = ZPushVal
  else:
    op = ZPushPtr

  ctx.addInstruction(op, sym.getOffset(), tid = sym.tid, arena = arena)

proc genPushImmediate(ctx: var CodeGenState, immediate: int, tid = TInt) =
  ctx.addInstruction(ZPushImm, immediate = immediate, tid = tid)

proc genPushImmediateF(ctx: var CodeGenState, val: float) =
  let p = cast[pointer](val)
  ctx.addInstruction(ZPushImm, immediate = cast[int](p), tid = TFloat)

proc genPushTypeOf(ctx: var CodeGenState, sym: SymbolInfo) =
  ctx.addInstruction(ZPushSType, sym.getOffset(),
                     arena = ctx.getSymbolArena(sym))

proc genDupTop(ctx: var CodeGenState) =
  ctx.addInstruction(ZDupTop)

proc genStore(ctx: var CodeGenState, sym: SymbolInfo, tid = sym.tid) =
  ctx.addInstruction(ZStoreTop, sym.getOffset(), tid = tid,
                     arena = ctx.getSymbolArena(sym))

proc genStoreImmediate(ctx: var CodeGenState, sym: SymbolInfo,
                       immediate: int, tid = sym.tid) =
  ctx.addInstruction(ZStoreImm, sym.getOffset(), immediate, tid = tid,
                     arena = ctx.getSymbolArena(sym))

proc genTCall(ctx: var CodeGenState, callId: int, tid: TypeId = TBottom) =
  var dtid: TypeId

  if tid != TBottom:
    dtid = tid.getDataType().dtid

  # This is for generating calls to our internal API for data types.
  # Push parameters on last to first.
  ctx.addInstruction(ZTCall, callId, tid = dtid)

proc genNot(ctx: var CodeGenState) =
  ctx.addInstruction(ZNot)

proc genEqual(ctx: var CodeGenState, tid = TBool) =
  ctx.genTCall(FEq, tid)

proc genAdd[T, V](ctx: var CodeGenState, lhs: T, rhs: V, tid: TypeId) =
  when T is SymbolInfo:
    ctx.genPush(lhs)
  else:
    ctx.genPushImmediate(lhs)

  when V is SymbolInfo:
    ctx.genPush(rhs)
  else:
    ctx.genPushImmediate(rhs)

  ctx.genTCall(FAdd, tid)

proc startJz(ctx: var CodeGenState, target: IrNode = nil,
             popit = true): int {.discardable.} =
  # Returns the ABSOLUTE offset of this instruction; you need to
  # then calculate an offset.
  result = ctx.mcur.instructions.len()

  if not popit:
    ctx.addInstruction(ZDupTop)

  ctx.addInstruction(ZJz)

  if target != nil:
    ctx.mcur.backpatchLocs.add((target, result))

proc startJnz(ctx: var CodeGenState, target: IrNode = nil,
             popit = true): int {.discardable.} =

  result = ctx.mcur.instructions.len()

  if not popit:
    ctx.addInstruction(ZDupTop)

  ctx.addInstruction(ZJnz)

  if target != nil:
    ctx.mcur.backpatchLocs.add((target, result))

proc startJ(ctx: var CodeGenState, target: IrNode = nil): int {.discardable.} =

  result = ctx.mcur.instructions.len()
  ctx.addInstruction(ZJ)

  if target != nil:
    ctx.mcur.backpatchLocs.add((target, result))

proc getJumpOffset(jAddr, targetAddr: int): int32 =
  return int32((targetAddr - jAddr) * sizeof(ZInstruction))

proc genBackwardsJump(ctx: var CodeGenState, target: IrNode) =
  let curAddr = ctx.mcur.instructions.len()

  for (n, loc) in ctx.mcur.loopLocs:
    if n == target:
      ctx.addInstruction(ZJ, arg = getJumpOffset(curAddr, loc))
      return

  unreachable

proc genBackwardsJump(ctx: var CodeGenState, target: int) =
  let curAddr = ctx.mcur.instructions.len()
  ctx.addInstruction(ZJ, arg = getJumpOffset(curAddr, target))

proc backPatch(ctx: var CodeGenState, patchLoc: int) =
  var cur = ctx.mcur.instructions[patchLoc]

  cur.arg                         = getJumpOffset(patchLoc, ctx.codeLoc())
  ctx.mcur.instructions[patchLoc] = cur

proc genContainerIndex(ctx: var CodeGenState, sym: SymbolInfo = nil,
                       immediate = 0) =
  # Container is expected to already be on the stack.
  if sym == nil:
    ctx.genPushImmediate(immediate)
  else:
    ctx.genPush(sym)

  ctx.genTCall(FIndex)

proc genPushAttrName(ctx: var CodeGenState, name: string) =
  ctx.addInstruction(ZPushStaticPtr, ctx.addStaticObject(name), tid = TString)

proc genLoadAttr(ctx: var CodeGenState, tid: TypeId) =
  ctx.addInstruction(ZLoadFromAttr, tid = tid)

proc genAssign(ctx: var CodeGenState, tid: TypeId) =
  ctx.addInstruction(ZAssignToLoc, tid = tid)

proc genLoadFromIx(ctx: var CodeGenState, tid: TypeId) =
  ctx.addInstruction(ZIndexedLoad, tid = tid)

proc genLoadFromSlice(ctx: var CodeGenState, tid: TypeId) =
  ctx.addInstruction(ZSliceLoad, tid = tid)

proc genAssignToIx(ctx: var CodeGenState, tid: TypeId) =
  ctx.addInstruction(ZAssignToIx, tid = tid)

proc genAssignSlice(ctx: var CodeGenState, tid: TypeId) =
  ctx.addInstruction(ZAssignSlice, tid = tid)


proc genReturnInstructions(ctx: var CodegenState) =
  let fn = ctx.fcur.fnRef

  if fn.retval != nil and fn.retval.tid.unify(TVoid) == TBottom:
    ctx.addInstruction(ZSetRes, fn.retval.sym.getOffset(),
                       tid = fn.retval.sym.tid,
                       arena = ctx.getSymbolArena(fn.retval.sym))

  ctx.addInstruction(ZRet)

## End opcode generation

proc genRangeLoopInit(ctx: var CodeGenState) =
  let cur = ctx.curNode.contents

  ctx.oneIrNode(cur.condition)

  let
    cursym = cur.loopVars[0]
    maxsym = cur.loopVars[1]

  ctx.genPop(maxSym)
  ctx.genPop(curSym)

proc genContainerLoopInit(ctx: var CodeGenState) =
  let
    n            = ctx.curNode
    cursym       = n.contents.loopVars[0]
    maxsym       = n.contents.loopVars[1]
    containersym = n.contents.loopVars[2]

  ctx.oneIrNode(n.contents.condition)
  # Address of container should be at top of stack now.  When loaded,
  # it will already be marked as CG-traced.
  ctx.genTCall(FLen, n.contents.condition.tid)
  ctx.genPop(maxsym)
  ctx.genStoreImmediate(curSym, 0)
  ctx.genPop(containersym)

proc genWhileLoopInit(ctx: var CodeGenState) =
  # TODO-- only generate these if $i get used.
  let cursym = ctx.curNode.contents.loopVars[0]
  ctx.genStoreImmediate(curSym, 0)

proc genIterationCheck(ctx: var CodeGenState) =
  let
    cur = ctx.curNode.contents
    m   = ctx.mcur

  if not cur.whileLoop:
    ctx.genPush(cur.loopVars[0])
    ctx.genPush(cur.loopVars[1])
    ctx.genEqual()
    ctx.startJz(target = ctx.curNode)

    if cur.condition.contents.kind != IrRange:
      ctx.genPush(cur.loopVars[2]) # Container on stack.
      ctx.genContainerIndex(cur.loopVars[0])
      if cur.loopVars.len() == 3:
        # This is a for x in list, so there's no array to unpack,
        # we can just directly pop it in.
        ctx.genPop(cur.loopVars[2])
      else:
        for i in 3 ..< cur.loopVars.len():
          ctx.genContainerIndex(immediate = i - 3)
          ctx.genPop(cur.loopVars[i])
      ctx.genPop() # Pop the container, but it's already stored, so no arg.
  else:
    ctx.oneIrNode(cur.condition)
    ctx.startJz(target = ctx.curNode)

proc genLoopFooter(ctx: var CodeGenState, top: int) =
  ctx.genAdd(ctx.curNode.contents.loopVars[0], 1, TInt)
  ctx.genBackwardsJump(top)

proc genConditional(ctx: var CodeGenState, cur: IrContents) =
  ctx.oneIrNode(cur.predicate)
  let patch1 = ctx.startJz()
  ctx.oneIrNode(cur.trueBranch)
  let patch2 = ctx.startJ()
  if cur.falseBranch != nil:
    ctx.oneIrNode(cur.falseBranch)
  ctx.backPatch(patch1)
  ctx.backPatch(patch2)

proc genLoop(ctx: var CodeGenState, n: IrNode) =
  let
    m   = ctx.mcur
    cur = n.contents

  # TODO: debug info w/ label names.
  m.loopLocs.add((n, ctx.codeLoc()))
  let patchStackSz = m.backpatchLocs.len()
  if not cur.whileLoop:
    if cur.condition.contents.kind == IrRange:
      ctx.genRangeLoopInit()
    else:
      ctx.genContainerLoopInit()
  m.loopLocs.add((n, ctx.codeLoc()))
  let top = ctx.codeLoc()
  if cur.label != nil:
    ctx.genLabel(cur.label.getText() & ": ")
  ctx.genIterationCheck()
  ctx.oneIrNode(cur.loopBody)
  ctx.genLoopFooter(top)

  var savedLocs: seq[(IrNode, int)]
  while m.backpatchLocs.len() > patchStackSz:
    let (node, loc) = m.backpatchLocs.pop()
    if node == n:
      ctx.backPatch(loc)
    else:
      savedLocs.add((node, loc))

  m.backpatchLocs = savedLocs

proc genTypeCase(ctx: var CodeGenState, cur: IrContents) =
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
        ctx.genEqual( )  # TODO: Add TTSpec
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

proc genValueCase(ctx: var CodeGenState, cur: IrContents) =
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

proc genJump(ctx: var CodeGenState, cur: IrContents) =
  if cur.exitLoop:
    ctx.startJ(cur.targetNode)
  else:
    ctx.genBackwardsJump(cur.targetNode)

proc genLitLoad(ctx: var CodeGenState, n: IrNode) =
  let cur = n.contents

  if cur.items.len() != 0:
    for item in cur.items:
      ctx.oneIrNode(item)

    ctx.genPushImmediate(cur.items.len())

    ctx.genTCall(FContainerLit, n.tid)

  else:
    if cur.byVal:
      ctx.genPushImmediate(cast[int64](n.value.getOrElse(nil)))
    else:
      let p = ctx.addStaticObject(n.value.get(), cur.sz)
      ctx.genCopyStaticObject(p, cur.sz, n.tid)

proc genMember(ctx: var CodeGenState, cur: IrContents) =
  ctx.genPushAttrName(cur.attrSym.name)
  ctx.genLoadAttr(cur.attrSym.tid)

proc genMemberLhs(ctx: var CodeGenState, cur: IrContents) =
  ctx.genPushAttrName(cur.attrSym.name)

proc genIndex(ctx: var CodeGenState, n: IrNode) =
  let cur = n.contents

  ctx.oneIrNode(cur.toIx)
  ctx.oneIrNode(cur.indexStart)

  if cur.indexEnd != nil:
    ctx.oneIrNode(cur.indexEnd)
    ctx.genLoadFromSlice(n.tid)
  else:
    ctx.genLoadFromIx(n.tid)

proc genIndexLhs(ctx: var CodeGenState, n: IrNode) =
  # The actual assignment is generated above us.
  # So this is the same as genIndex except without actually loading the
  # array value.
  let cur = n.contents

  ctx.oneIrNode(cur.toIx)
  ctx.oneIrNode(cur.indexStart)
  if cur.indexEnd != nil:
    ctx.oneIrNode(cur.indexEnd)

proc genCall(ctx: var CodeGenState, cur: IrContents) =
  var
    moduleKey = cur.toCall.defModule.key
    minf      = ctx.minfo[moduleKey]
    i         = cur.actuals.len()

  # Push actuals in reverse order. This isn't ideal, since people expect
  # left-to-right evaluation. It's a TODO to fix that later.
  while i != 0:
    i = i - 1
    ctx.oneIrNode(cur.actuals[i])

  if cur.toCall.externInfo == nil:

    ctx.addInstruction(Z0Call, arg = cur.toCall.codeOffset,
                       tid = cur.toCall.tid, arena = minf.arenaId)

    if cur.toCall.retVal.tid.followForwards != TVoid:
      ctx.addInstruction(ZPushRes, tid = cur.toCall.retVal.tid)

  else:
    discard
    # TODO: FFI

proc genUse(ctx: var CodeGenState, cur: IrContents) =
  # This is basically just a 'call' with no parameters to the
  # address of the module.

  for (k, item) in ctx.minfo.items():
    echo "Have module: ", k

  assert cur.moduleObj != nil

  var
    moduleKey = cur.moduleObj.key
    minf      = ctx.minfo[moduleKey]

  ctx.addInstruction(Z0Call, arg = 0, tid = TVoid, arena = minf.arenaId)


proc genRet(ctx: var CodeGenState, cur: IrContents) =
  # We could skip adding any value to the `result` variable if there's
  # an expression after the return, and go directly to the return
  # register, but for now we're keeping it simple.
  if cur.retVal != nil:
    ctx.oneIrNode(cur.retVal)
    ctx.genPop(ctx.fcur.fnRef.retval.sym)

  ctx.genReturnInstructions()

proc genAttrAssign(ctx: var CodeGenState, n: IrNode) =
  # In the case of assigning to an index, we'll end up w/
  # an extra item on the stack, with the index pushed on after the
  # container's address.
  #
  # To make our lives easier, we just use a different instruction
  # for each of these cases.

  let cur = n.contents

  ctx.oneIrNode(cur.attrLhs)
  ctx.oneIrNode(cur.attrRhs)

  if cur.attrLhs.contents.kind == IrMemberLhs:
    if cur.attrLhs.contents.indexEnd == nil:
      ctx.genAssignToIx(cur.attrRhs.tid)
    else:
      ctx.genAssignSlice(cur.attrRhs.tid)
  else:
    ctx.genAssign(cur.attrRhs.tid)

proc genLoadStorageAddress(ctx: var CodeGenState, sym: SymbolInfo) =
  if sym.isAttr:
    ctx.genPushAttrName(sym.name)
  else:
    ctx.addInstruction(ZPushAddr, sym.getOffset(), tid = sym.tid,
                       arena = sym.arena)

proc genLoadSymbol(ctx: var CodeGenState, sym: SymbolInfo) =
  if sym.isAttr:
    ctx.genPushAttrName(sym.name)
    ctx.genLoadAttr(sym.tid)
  else:
    ctx.genPush(sym)

proc genUminus(ctx: var CodeGenState, cur: IrContents) =
  ctx.oneIrNode(cur.uRhs)
  if cur.uRhs.tid.isFloatType():
    ctx.genPushImmediateF(-1.0)
  else:
    ctx.genPushImmediate(-1)
  ctx.genTCall(FMul, cur.uRhs.tid)

proc genBinary(ctx: var CodeGenState, n: IrNode) =
  let cur = n.contents

  ctx.oneIrNode(cur.bLhs)
  ctx.oneIrNode(cur.bRhs)

  if cur.opId > 128:
    ctx.genNot()

  ctx.genTCall(cur.opId and 0x7f, tid = n.tid)

proc genLogic(ctx: var CodeGenState, cur: IrContents) =
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

proc genLoadValue(ctx: var CodeGenState, n: IrNode) =
  ctx.genPushImmediate(cast[int](n.value.get()), n.tid)

proc oneIrNode(ctx: var CodeGenState, n: IrNode) =
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
    ctx.genLoadSymbol(cur.symbol)
  of IrFold:
    ctx.genLoadValue(n)
  of IrNop:
    ctx.genNop()
  of IrNil:
    ctx.genPushImmediate(0, TVoid)
  of IrAttrAssign:
    ctx.genAttrAssign(n)
  of IrVarAssign, IrSwitchBranch:
    # The first shouldn't be produced anymore, and the second is handled
    # from the parent node w/o descending.
    unreachable

  ctx.curNode = saved

proc addParameter(ctx: var CodeGenState, symbol: SymbolInfo) =
  discard

proc addAllFfiInfo(ctx: var CodeGenState) =
  discard


proc stashSymbolInfo(scope: Scope, d: var Dict[int, string]) =
  for (name, sym) in scope.table.items():
    d[sym.offset] = name

proc genFunction(ctx: var CodeGenState, fn: FuncInfo) =
  ctx.genLabel(fn.name & fn.tid.toString())

  fn.codeOffset  = ctx.mcur.instructions.len() * sizeof(ZInstruction)
  ctx.fcur       = ctx.zobj.funcInfo[fn.internalId - 1]
  ctx.fcur.fnRef = fn

  ctx.oneIrNode(fn.implementation)
  # Todo: if this is unreachable, don't bother generating it.
  ctx.genReturnInstructions()

  ctx.fcur = nil

proc genModule(ctx: var CodeGenState, m: Module) =
  let curMod = ctx.minfo[m.key]
  if curMod.processed:
    return

  ctx.mcur = curMod

  ctx.genLabel("Module '" & m.modname & "' :")

  for (_, sym) in m.moduleScope.table.items():
    if sym.pInfo != nil:
      ctx.addParameter(sym)

  ctx.oneIrNode(m.ir)
  ctx.addInstruction(ZModuleRet)
  ctx.genLabel("Functions: ")

  for (_, sym) in m.moduleScope.table.items():
    for fn in sym.fimpls:
      ctx.genFunction(fn)

  curMod.processed = true

  let deps = ctx.minfo.items(sort = true)
  for (k, v) in deps:
    if not v.processed:
      ctx.genModule(v.moduleRef)

proc applyArenaId(ctx: var CodeGenState, scope: Scope, arenaId: int,
                  curFnId: var int) =
  for (_, sym) in scope.table.items():
    sym.arena = arenaId
    for fn in sym.fimpls:
      if fn.internalId == 0:
        fn.internalId = curFnId
        let fi = ZFnInfo()
        ctx.zobj.funcInfo.add(fi)
        fi.syms.initDict()
        fn.fnscope.stashSymbolInfo(fi.syms)
        curFnId += 1
      ctx.applyArenaId(fn.fnScope, -1, curFnId)

proc setupModules(ctx: var CodeGenState) =
  var
    curArena = 1
    curFnId  = 1

  # This applies a unique number to each module (an arena id)
  # and then applies a unique number to each function.
  ctx.applyArenaId(ctx.cc.globalScope, 0, curFnId)
  ctx.zobj.globals.initDict()
  ctx.cc.globalScope.stashSymbolInfo(ctx.zobj.globals)

  for (_, module) in ctx.cc.modules.items():
    var mi = ModuleInfo(arenaId: curArena, moduleRef: module)
    ctx.applyArenaId(module.moduleScope, curArena, curFnId)
    curArena                            = curArena + 1
    ctx.minfo[module.key]               = mi
    ctx.zobj.moduleContents[module.key] = mi
    mi.syms.initDict()
    module.moduleScope.stashSymbolInfo(mi.syms)

  ctx.addAllFfiInfo()

proc generateCode*(cc: CompileCtx): ZObject =
  var ctx: CodeGenState

  result   = ZObject()
  ctx.cc   = cc
  ctx.zobj = result

  result.moduleContents.initDict()
  result.types.initDict()

  ctx.locMap.initDict()
  ctx.minfo.initDict()
  ctx.setupModules()
  ctx.genModule(cc.entrypoint)


# TODO -- generation of function and module prologues.
# TODO -- call table for FFI info.
# TODO -- useful type comparison stuff.
# TODO -- pushTypeOf needs to handle attributes.
# TODO -- no attr assignment done yet.
# TODO -- global variables are not handled right.
