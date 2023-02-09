import types, streams, run, st, tables, nimutils, c42spec, strutils, dollars

type C4CSpecObj = ref object
  spec:  ConfigSpec
  state: ConfigState
  err:   string

# I need to spend more time investigating under the hood. I should
# just be able to GC_ref() the string a cstring was based on, and have
# it stick around. But I think there's perhaps some weird copying
# happening, related to string sharing.  So until I have time to
# investigate, I'm just keeping a table mapping the memory address to
# the object.  Basically, the same underlying string might end up w/ 2
# addresses pointing to it, and we want to keep each one live even if
# we deref the other.  Even w/ a C string, Nim's hash function uses
# the contents of the string, not the pointer.
var strPool = initTable[uint, cstring]()

proc exportStr(s: string): cstring {.inline.} =
  result = cstring(s)
  strPool[cast[uint](result)] = result

proc c4mStrDelete*(s: cstring) {.exportc.} =
  strPool.del(cast[uint](s))

proc c4mOneShot*(contents: cstring, fname: cstring): cstring {.exportc.} =
  try:
    let (ctx, res) = firstRun($(contents), $(fname))
    if not res:
      return nil
    return exportStr(ctx.attrs.scopeToJson())
  except:
    return exportStr(getCurrentExceptionMsg())

proc c4mFirstRun*(contents: cstring, fname: cstring, addBuiltIns: bool,
                  spec: C4CSpecObj, err: var cstring): ConfigState {.exportc.} =
  var
    c42Spec: ConfigSpec  = nil
    specCtx: ConfigState = nil

  if spec != nil:
    c42Spec = spec.spec
    specCtx = spec.state

  try:
    let (ctx, res) = firstRun($(contents),
                              $(fname),
                              c42Spec,
                              addBuiltIns,
                              evalCtx = specCtx)
    if not res:
      var cstr = exportStr(getCurrentExceptionMsg())
      err      = cstr
      return nil
    GC_ref(ctx)
    return ctx
  except:
    var cstr = exportStr("Unknown error")
    err      = cstr
    return nil

proc c4mStack*(state: ConfigState, contents: cstring, fname: cstring,
               spec: C4CSpecObj): cstring {.exportc.} =
  var specCtx: ConfigState = nil

  if spec != nil:
    specCtx = spec.state

  try:
    discard stackConfig(state, $(contents), $(fname), specCtx)
    return nil
  except:
    return exportStr(getCurrentExceptionMsg())


proc c4mSetAttrInt*(state: ConfigState, name: cstring, val: int):
                                                           int {.exportc.} =
  var
    n = $(name)
    b = pack[int](val)
    r = attrSet(state, n, b)
  return int(r.code)

proc c4mGetAttrInt*(state: ConfigState, name: cstring, ok: ptr int):
                                                           int {.exportc.} =
  var
    n = $(name)
    o = attrLookup(state, n)

  if o.isNone():
    ok[]     = int(0)
  else:
    ok[]     = int(1)
    let box  = o.get()
    result = unpack[int](box)

proc c4mSetAttrBool*(state: ConfigState, name: cstring, val: int):
                                                           int {.exportc.} =
  var
    n = $(name)
    b = pack[bool](if val != 0: true else: false)
    r = attrSet(state, n, b)
  return int(r.code)

proc c4mGetAttrBool*(state: ConfigState, name: cstring, ok: ptr int):
                                                           int {.exportc.} =
  var
    n = $(name)
    o = attrLookup(state, n)

  if o.isNone():
    ok[]     = int(0)
  else:
    ok[]     = int(1)
    let box  = o.get()
    result = if unpack[bool](box): 1 else: 0
    

proc c4mSetAttrStr*(state: ConfigState, name: cstring, val: cstring):
                                                           int {.exportc.} =
  var
    n = $(name)
    v = $(val)
    b = pack[string](v)
    r = attrSet(state, n, b)

  result = int(r.code)

proc c4mGetAttrStr*(state: ConfigState, name: cstring, ok: ptr int):
                                                       cstring {.exportc.} =
  var
    n = $(name)
    o = attrLookup(state, n)

  if o.isNone():
    ok[]      = int(0)
  else:
    ok[]      = int(1)
    let box   = o.get()
    let res   = unpack[string](box)
    result    = exportStr(res)

proc c4mSetAttrFloat*(state: ConfigState, name: cstring, val: float):
                                                          int {.exportc.} =
  var
    n = $(name)
    b = pack[float](val)
    r = attrSet(state, n, b)

  result = int(r.code)

proc c4mGetAttrFloat*(state: ConfigState, name: cstring, ok: ptr int):
                                                         float {.exportc.} =
  var
    n = $(name)
    o = attrLookup(state, n)

  if o.isNone():
    ok[]      = int(0)
  else:
    ok[]      = int(1)
    let box   = o.get()
    result    = unpack[float](box)

proc c4mSetAttr*(state: ConfigState, name: cstring, b: Box): int {.exportc.} =
  var
    n = $(name)
    r = attrSet(state, n, b)

  result = int(r.code)

proc c4mGetAttr*(state:   ConfigState,
                 name:    cstring,
                 boxType: ptr MixedKind,
                 ok:      ptr int): Box {.exportc.} =
  var
    n = $(name)
    o = attrLookup(state, n)

  if o.isNone():
    ok[]      = int(0)
  else:
    ok[]      = int(1)
    result    = o.get()
    boxType[] = result.kind
    GC_ref(result)

proc c4GetSections*(state: ConfigState,
                    name:  cstring,
                    arr:   var ptr cstring): int {.exportc.} =
  var res: seq[cstring] = @[]

  let
    parts = `$`(name).split(".")
    aOrE  = attrLookup(state.attrs, parts, 0, vlSecUse);

  if aOrE.isA(AttrErr):
    arr = nil
    return -1

  let aOrS = aOrE.get(AttrOrSub)

  if aOrS.isA(Attribute):
    arr = nil
    return -2

  let scope = aOrS.scope

  GC_ref(res)

  for k, v in scope.contents:
    if v.kind == false:
      res.add(cstring(k))

  arr = addr(res[0])

  return len(res)


proc c4GetFields*(state: ConfigState,
                  name:  cstring,
                  arr:   var ptr cstring): int {.exportc.} =
  var res: seq[cstring] = @[]

  let
    parts = `$`(name).split(".")
    aOrE  = attrLookup(state.attrs, parts, 0, vlSecUse);

  if aOrE.isA(AttrErr):
    arr = nil
    return -1

  let aOrS = aOrE.get(AttrOrSub)

  if aOrS.isA(Attribute):
    arr = nil
    return -2

  let scope = aOrS.scope

  GC_ref(res)

  for k, v in scope.contents:
    if v.isA(Attribute):
      res.add(cstring(k))
      res.add(cstring($(v.get(Attribute).tInfo)))

  arr = addr(res[0])

  return len(res)


proc c4EnumerateScope*(state: ConfigState,
                       name:  cstring,
                       arr:   var ptr cstring): int {.exportc.} =

  var res: seq[cstring] = @[]

  let
    parts = `$`(name).split(".")
    aOrE  = attrLookup(state.attrs, parts, 0, vlSecUse);

  if aOrE.isA(AttrErr):
    arr = nil
    return -1

  let aOrS = aOrE.get(AttrOrSub)

  if aOrS.isA(Attribute):
    arr = nil
    return -2

  let scope = aOrS.scope

  GC_ref(res)

  for k, v in scope.contents:
    res.add(cstring(k))
    if v.isA(Attribute):
      res.add(cstring($(v.get(Attribute).tInfo)))
    else:
      res.add(cstring("section"))

  arr = addr(res[0])

  return len(res)

proc c4mBoxType*(box: Box): MixedKind {.exportc.} =
  return box.kind

proc c4mClose*(state: ConfigState) {.exportc.} =
  GC_unref(state)

proc c4mUnpackInt*(box: Box): int {.exportc.} =
  result = unpack[int](box)

proc c4mPackInt*(i: int): Box {.exportc.} =
  result = pack(i)
  GC_ref(result)

proc c4mUnpackBool*(box: Box): int {.exportc.} =
  result = if unpack[bool](box): 1 else: 0

proc c4mPackBool*(i: int): Box {.exportc.} =
  result = if i == 0: pack(false) else: pack(true)
  GC_ref(result)
  
proc c4mUnpackFloat*(box: Box): float {.exportc.} =
  result = unpack[float](box)

proc c4mPackFloat*(f: float): Box {.exportc.} =
  result = pack(f)
  GC_ref(result)

proc c4UnpackString*(box: Box): cstring {.exportc.} =
  result = exportStr(unpack[string](box))

proc c4mPackString*(s: cstring): Box {.exportc.} =
  result = pack($(s))
  GC_ref(result)

proc c4mUnpackArray*(box: Box, arr: ref seq[Box]): int {.exportc.} =
  var items = unpack[seq[Box]](box)
  result    = len(items)
  arr[]     = items
  GC_ref(items)

proc c4mArrayDelete*(arr: var seq[Box]) {.exportc.} =
  GC_unref(arr)

proc c4mPackArray*(arr: UncheckedArray[Box], sz: int): Box {.exportc.} =
  var s: seq[Box] = @[]
  for i in 0 ..< sz:
    s.add(arr[i])
  result = pack(s)
  GC_ref(result)

proc c4mUnpackDict*(box: Box): OrderedTableRef[Box, Box] {.exportc.} =
  result = unpack[OrderedTableRef[Box, Box]](box)
  GC_ref(result)

proc c4mDictNew*(): OrderedTableRef[Box, Box] {.exportc.} =
  result = newOrderedTable[Box, Box]()
  GC_ref(result)

proc c4mDictDelete*(dict: OrderedTableRef[Box, Box]) {.exportc.} =
  GC_unref(dict)

proc c4mDictLookup*(tbl: OrderedTableRef[Box, Box], b: Box): Box {.exportc.} =
  if b in tbl:
    result = tbl[b]
    GC_ref(result)
  else:
    return nil

proc c4mDictSet*(tbl: OrderedTableRef[Box, Box], b: Box, v: Box) {.exportc.} =
  tbl[b] = v

proc c4mDictKeyDel*(tbl: OrderedTableRef[Box, Box], b: Box) {.exportc.} =
  if b in tbl:
    tbl.del(b)

proc c4mLoadSpec*(spec, fname: cstring, ok: ptr int): C4CSpecObj {.exportc.} =
  result = new(C4CSpecObj)
  
  try:
    let opt = c42Spec(newStringStream($(spec)), $(fname))

    if opt.isNone():
      ok[]         = int(0)
      result.spec  = nil
      result.state = nil
      result.err   = ""

    let (spec, evalCtx) = opt.get()

    result.spec  = spec
    result.state = evalCtx
    result.err   = ""
    ok[]         = int(1)
  except:
    result.spec  = nil
    result.state = nil
    result.err   = getCurrentExceptionMsg()
    ok[]         = int(0)

  GC_ref(result)


proc c4mGetSpecErr*(spec: var C4CSpecObj): cstring {.exportc.} =
  result = exportStr(spec.err)
  GC_unref(spec)

proc c4mSpecDelete*(spec: var C4CSpecObj) {.exportc.}  =
  GC_unref(spec)

proc c4mStateDelete*(state: var ConfigState) {.exportc.} =
  GC_unref(state)

proc c4mBoxDelete*(box: var Box) {.exportc.} =
  GC_unref(box)
