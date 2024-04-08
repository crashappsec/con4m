import "."/[common, attrstore]

proc symTypeXForm(st: seq[(int, TypeSpec)]): Dict[int, TypeSpec] =
  result.initDict()

  for (n, t) in st:
    result[n] = t

proc toST(s: Dict[int, TypeSpec]): seq[(int, TypeSpec)] =
   return s.items(sort = true)

proc marshal_modules(rt: RuntimeState) =
  let
    s = rt.marshalStream
    m = rt.memos
    a = addr rt.next_memoid

  marshal_i32(cint(rt.obj.moduleContents.len()), s)
  for module in rt.obj.moduleContents:
    con4m_sub_marshal(c4Str(module.modname), s, m, a)
    con4m_sub_marshal(c4Str(module.location), s, m, a)
    con4m_sub_marshal(c4Str(module.key), s, m, a)
    con4m_sub_marshal(c4Str(module.ext), s, m, a)
    con4m_sub_marshal(c4Str(module.url), s, m, a)
    con4m_sub_marshal(c4Str(module.version), s, m, a)
    con4m_sub_marshal(module.symTypes.symTypeXForm(), s, m, a)
    con4m_sub_marshal(module.codesyms, s, m, a)
    con4m_sub_marshal(module.datasyms, s, m, a)
    con4m_sub_marshal(c4Str(module.source), s, m, a)
    con4m_sub_marshal(c4Str(module.shortdoc), s, m, a)
    con4m_sub_marshal(c4Str(module.longdoc), s, m, a)
    marshal_i64(module.moduleId, s)
    marshal_i64(module.moduleVarSize, s)
    marshal_i64(module.initSize, s)
    marshal_i32(cint(module.parameters.len()), s)
    for param in module.parameters:
      con4m_sub_marshal(param.attr, s, m, a)
      marshal_i64(param.offset, s)
      con4m_sub_marshal(param.default, s, m, a)
      marshal_i32(param.vFnIx, s)
      marshal_bool(param.vNative, s)
      marshal_i32(param.iFnIx, s)
      marshal_bool(param.iNative, s)
      con4m_sub_marshal(param.userparam, s, m, a)
      con4m_sub_marshal(param.userType, s, m, a)
      con4m_sub_marshal(param.shortdoc, s, m, a)
      con4m_sub_marshal(param.longdoc, s, m, a)
    marshal_i32(cint(module.instructions.len()), s)
    for instruction in module.instructions:
      marshal_u8(cast[uint8](instruction.op), s)
      marshal_i16(instruction.moduleId, s)
      marshal_i32(instruction.lineNo, s)
      marshal_i32(instruction.arg, s)
      marshal_i64(instruction.immediate, s)
      con4m_sub_marshal(instruction.typeInfo, s, m, a)

proc unmarshal_modules(rt: RuntimeState): seq[ZModuleInfo] =
  let
    s = rt.marshalStream
    m = rt.memos
    l = s.unmarshal_i32()

  for i in 0 ..< l:
    var mi = ZModuleInfo()
    mi.modname  = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.location = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.key      = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.ext      = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.url      = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.version  = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.symTypes = con4m_sub_unmarshal[Dict[int, TypeSpec]](s, m).toST()
    mi.codesyms = con4m_sub_unmarshal[Dict[int, string]](s, m)
    mi.datasyms = con4m_sub_unmarshal[Dict[int, string]](s, m)
    mi.source   = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.shortdoc = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    mi.longdoc  = con4m_sub_unmarshal[Rich](s, m).toNimStr()

    mi.moduleId      = s.unmarshal_i64()
    mi.moduleVarSize = s.unmarshal_i64()
    mi.initSize      = s.unmarshal_i64()

    let nparams = s.unmarshal_i32()
    for j in 0 ..< nparams:
      var p       = ZParamInfo()
      p.attr      = con4m_sub_unmarshal[Rich](s, m)
      p.offset    = s.unmarshal_i64()
      p.default   = con4m_sub_unmarshal[pointer](s, m)
      p.vFnIx     = s.unmarshal_i32()
      p.vNative   = s.unmarshal_bool()
      p.iFnIx     = s.unmarshal_i32()
      p.iNative   = s.unmarshal_bool()
      p.userparam = con4m_sub_unmarshal[pointer](s, m)
      p.userType  = con4m_sub_unmarshal[TypeSpec](s, m)
      p.shortdoc  = con4m_sub_unmarshal[Rich](s, m)
      p.longdoc   = con4m_sub_unmarshal[Rich](s, m)
      mi.parameters.add(p)

    let ninstr = s.unmarshal_i32()
    for j in 0 ..< ninstr:
      var ins       = ZInstruction()
      ins.op        = cast[ZOp](s.unmarshal_u8())
      ins.moduleId  = s.unmarshal_i16()
      ins.lineNo    = s.unmarshal_i32()
      ins.arg       = s.unmarshal_i32()
      ins.immediate = s.unmarshal_i64()
      ins.typeInfo  = con4m_sub_unmarshal[TypeSpec](s, m)
      mi.instructions.add(ins)

    result.add(mi)

proc marshal_func_info(rt: RuntimeState) =
  let
    s = rt.marshalStream
    m = rt.memos
    a = addr rt.next_memoid

  marshal_u32(cuint(rt.obj.funcInfo.len()), s)
  for fn in rt.obj.funcInfo:
    con4m_sub_marshal(c4str(fn.funcname), s, m, a)
    con4m_sub_marshal(fn.syms, s, m, a)
    con4m_sub_marshal(fn.symTypes.symTypeXform(), s, m, a)
    con4m_sub_marshal(fn.tid, s, m, a)
    marshal_i32(fn.mid, s)
    marshal_i32(fn.offset, s)
    marshal_i32(fn.size, s)
    con4m_sub_marshal(fn.shortdoc, s, m, a)
    con4m_sub_marshal(fn.longdoc, s, m, a)

proc unmarshal_func_info(rt: RuntimeState): seq[ZFnInfo] =
  let
    s = rt.marshalStream
    m = rt.memos
    l = s.unmarshal_u32()

  for i in 0 ..< l:
    var info      = ZFnInfo()
    info.funcname = con4m_sub_unmarshal[Rich](s, m).toNimStr()
    info.syms     = con4m_sub_unmarshal[Dict[int, string]](s, m)
    info.symTypes = con4m_sub_unmarshal[Dict[int, TypeSpec]](s, m).toST()
    info.tid      = con4m_sub_unmarshal[TypeSpec](s, m)
    info.mid      = s.unmarshal_i32()
    info.offset   = s.unmarshal_i32()
    info.size     = s.unmarshal_i32()
    info.shortdoc = con4m_sub_unmarshal[Rich](s, m)
    info.longdoc  = con4m_sub_unmarshal[Rich](s, m)

proc marshal_ffi_info(rt: RuntimeState) =
  let
    s = rt.marshalStream
    m = rt.memos
    a = addr rt.next_memoid

  marshal_u32(uint32(rt.obj.ffiInfo.len()), s)
  for info in rt.obj.ffiInfo:
    marshal_i64(info.nameoffset, s)
    marshal_i64(info.localname, s)
    marshal_i32(info.mid, s)
    con4m_sub_marshal(info.tid, s, m, a)
    marshal_bool(info.va, s)
    con4m_sub_marshal(info.dlls.toXList(), s, m, a)
    marshal_u32(uint32(info.argInfo.len()), s)
    for arg in info.argInfo:
      marshal_bool(arg.held, s)
      marshal_bool(arg.alloced, s)
      marshal_i16(arg.argType, s)
      marshal_i32(arg.ourType, s)
      con4m_sub_marshal(c4Str(arg.name), s, m, a)
    con4m_sub_marshal(info.shortdoc, s, m, a)
    con4m_sub_marshal(info.longdoc, s, m, a)

proc unmarshal_ffi_info(rt: RuntimeState): seq[ZFfiInfo] =
  let
    s = rt.marshalStream
    m = rt.memos
    l = s.unmarshal_u32()

  for i in 0 ..< l:
    var info = ZFfiInfo()
    info.nameoffset     = s.unmarshal_i64()
    info.localname      = s.unmarshal_i64()
    info.mid            = s.unmarshal_i32()
    info.tid            = con4m_sub_unmarshal[TypeSpec](s, m)
    info.va             = s.unmarshal_bool()
    let arr: XList[int] = con4m_sub_unmarshal[XList[int]](s, m)
    info.dlls           = arr.toSeq()
    let nargs           = s.unmarshal_u32()
    for j in 0 ..< nargs:
      var arg = ZFfiArgInfo()
      arg.held    = s.unmarshal_bool()
      arg.alloced = s.unmarshal_bool()
      arg.argType = s.unmarshal_i16()
      arg.ourType = s.unmarshal_i32()
      arg.name    = con4m_sub_unmarshal[Rich](s, m).toNimStr()
      info.argInfo.add(arg)

    info.shortDoc = con4m_sub_unmarshal[Rich](s, m)
    info.longDoc  = con4m_sub_unmarshal[Rich](s, m)
    result.add(info)

proc marshal_one_field(rt: RuntimeState, name: Rich, f: FieldSpec) =
  let
    s          = rt.marshalStream
    m          = rt.memos
    a          = addr rt.next_memoid

  con4m_sub_marshal(name, s, m, a)
  con4m_sub_marshal(f.tid, s, m, a)
  marshal_bool(f.lockOnWrite, s)
  con4m_sub_marshal(f.defaultVal, s, m, a)

  marshal_u16(cast[uint16](f.validators.len()), s)

  for v in f.validators:
    con4m_sub_marshal(v.fn, s, m, a)
    con4m_sub_marshal(v.params, s, m, a)
    con4m_sub_marshal(v.paramType, s, m, a)

  marshal_bool(f.hidden, s)
  marshal_bool(f.required, s)
  con4m_sub_marshal(f.doc, s, m, a)
  con4m_sub_marshal(f.shortdoc, s, m, a)
  marshal_u32(cast[uint32](f.fieldKind), s)

  marshal_i64(f.errIx, s)
  con4m_sub_marshal(f.exclusions.toRichXlist(), s, m, a)
  con4m_sub_marshal(c4Str(f.deferredType), s, m, a)

proc unmarshal_one_field(rt: RuntimeState, name: Rich): FieldSpec =
  let
    s          = rt.marshalStream
    m          = rt.memos

  result = FieldSpec(name: name.toNimStr())

  result.tid         = con4m_sub_unmarshal[TypeSpec](s, m)
  result.lockOnWrite = s.unmarshal_bool()
  result.defaultVal  = con4m_sub_unmarshal[pointer](s, m)

  var n = uint32(s.unmarshal_u16())

  for i in 0 ..< n:
    var v       = Validator()
    v.fn        = con4m_sub_unmarshal[pointer](s, m)
    v.params    = con4m_sub_unmarshal[pointer](s, m)
    v.paramType = con4m_sub_unmarshal[TypeSpec](s, m)

    result.validators.add(v)

  result.hidden     = s.unmarshal_bool()
  result.required   = s.unmarshal_bool()
  result.doc        = con4m_sub_unmarshal[Rich](s, m)
  result.shortdoc   = con4m_sub_unmarshal[Rich](s, m)
  result.fieldKind  = cast[FsKind](s.unmarshal_u32())
  result.errIx      = s.unmarshal_i64()

  let
    exclusions = con4m_sub_unmarshal[XList[Rich]](s, m)

  result.exclusions   = exclusions.toSeqStr()
  result.deferredType = con4m_sub_unmarshal[Rich](s, m).toNimStr()

proc marshal_one_spec(rt: RuntimeState, name: Rich, sec: SectionSpec) =
  let
    s          = rt.marshalStream
    m          = rt.memos
    a          = addr rt.next_memoid
    field_view = sec.fields.items(sort = true)

  con4m_sub_marshal(name, s, m, a)
  marshal_i64(sec.minAllowed, s)
  marshal_i64(sec.maxAllowed, s)
  marshal_u16(uint16(field_view.len()), s)
  for (name, info) in field_view:
    marshal_one_field(rt, name, info)
  marshal_bool(sec.userDefOk, s)
  marshal_u16(uint16(sec.validators.len()), s)

  for v in sec.validators:
    con4m_sub_marshal(v.fn, s, m, a)
    con4m_sub_marshal(v.params, s, m, a)
    con4m_sub_marshal(v.paramType, s, m, a)

  marshal_bool(sec.hidden, s)
  con4m_sub_marshal(sec.doc, s, m, a)
  con4m_sub_marshal(sec.shortdoc, s, m, a)
  con4m_sub_marshal(sec.allowedSections.toXList(), s, m, a)
  con4m_sub_marshal(sec.requiredSections.toXlist(), s, m, a)

proc unmarshal_one_spec(rt: RuntimeState): SectionSpec =
  let
    s    = rt.marshalStream
    m    = rt.memos
    name = con4m_sub_unmarshal[Rich](s, m)
    sec  = SectionSpec(name: name.toNimStr())

  sec.minAllowed = s.unmarshal_i64()
  sec.maxAllowed = s.unmarshal_i64()

  let nfields = s.unmarshal_u16()
  sec.fields.initDict()

  for i in 0 ..< uint32(nFields):
    let name         = con4m_sub_unmarshal[Rich](s, m)
    sec.fields[name] = rt.unmarshal_one_field(name)

  sec.userDefOk = s.unmarshal_bool()

  let nvalid = s.unmarshal_u16()

  for i in 0 ..< uint32(nvalid):
    var v       = Validator()
    v.fn        = con4m_sub_unmarshal[pointer](s, m)
    v.params    = con4m_sub_unmarshal[pointer](s, m)
    v.paramType = con4m_sub_unmarshal[TypeSpec](s, m)

    sec.validators.add(v)

  sec.hidden   = s.unmarshal_bool()
  sec.doc      = con4m_sub_unmarshal[Rich](s, m)
  sec.shortdoc = con4m_sub_unmarshal[Rich](s, m)

  let
    allowedX  = con4m_sub_unmarshal[XList[Rich]](s, m)
    requiredX = con4m_sub_unmarshal[XList[Rich]](s, m)

  sec.allowedSections  = allowedX.toSeq()
  sec.requiredSections = requiredX.toSeq()

  return sec


proc marshal_spec(rt: RuntimeState) =
  let
    spec = rt.obj.spec
    view = spec.secSpecs.items(sort = true)

  var
    n: uint16 = uint16(view.len())

  if spec.rootSpec != nil:
    n += 1

  if spec.used == false or n == 0:
    marshal_u16(0, rt.marshalStream)
    return

  marshal_u16(n, rt.marshalStream)
  marshal_one_spec(rt, nil, spec.rootSpec)
  for (name, item) in view:
    marshal_one_spec(rt, name, item)

proc unmarshal_spec(rt: RuntimeState): ValidationSpec =
  result.secSpecs.initDict()

  for i in 0 ..< uint32(rt.marshalStream.unmarshal_u16()):
    if i == 0:
     result.rootSpec = rt.unmarshal_one_spec()
    else:
      let spec                      = rt.unmarshal_one_spec()
      result.secSpecs[r(spec.name)] = spec

proc marshal_object(rt: RuntimeState, nextmid: int32) =
  var nextmid = nextmid

  if nextmid == 0 and rt.obj.nextEntrypoint != 0:
    nextmid = rt.obj.nextEntrypoint

  let
    s = rt.marshalStream
    m = rt.memos
    a = addr rt.next_memoid

  marshal_u64(rt.obj.zeroMagic, s)
  marshal_i16(rt.obj.zcObjectVers, s)
  con4m_sub_marshal(c4str(rt.obj.staticData), s, m, a)
  con4m_sub_marshal(rt.obj.globals, s, m, a)
  con4m_sub_marshal(rt.obj.symTypes.symTypeXform(), s, m, a)
  marshal_i64(rt.obj.globalScopeSz, s)
  rt.marshal_modules()
  marshal_i32(rt.obj.entrypoint, s)
  marshal_i32(nextmid, s)
  rt.marshal_func_info()
  rt.marshal_ffi_info()
  rt.marshal_spec()

proc unmarshal_object(rt: RuntimeState) =
  let
    m   = rt.memos
    s   = rt.marshalStream
    obj = rt.obj

  obj.zeroMagic      = s.unmarshal_u64()
  obj.zcObjectVers   = s.unmarshal_i16()
  obj.staticData     = con4m_sub_unmarshal[Rich](s, m).toNimStr()
  obj.globals        = con4m_sub_unmarshal[Dict[int, string]](s, m)
  obj.symTypes       = con4m_sub_unmarshal[Dict[int, TypeSpec]](s, m).toST()
  obj.globalScopeSz  = s.unmarshal_i64()
  obj.moduleContents = rt.unmarshal_modules()
  obj.entrypoint     = s.unmarshal_i32()
  obj.nextEntryPoint = s.unmarshal_i32()
  obj.funcInfo       = rt.unmarshal_func_info()
  obj.ffiInfo        = rt.unmarshal_ffi_info()
  obj.spec           = rt.unmarshal_spec()

proc marshal_attrs(rt: RuntimeState) =
  let
    m = rt.memos
    s = rt.marshalStream
    a = addr rt.next_memoid

  marshal_bool(rt.usingAttrs, s)

  if not rt.usingAttrs:
    return

  let view = rt.attrs.items(sort = true)
  marshal_u32(uint32(view.len()), s)
  for (name, contents) in view:

    con4m_sub_marshal(name, s, m, a)
    con4m_sub_marshal(contents.tid, s, m, a)
    marshal_bool(contents.isSet, s)
    marshal_bool(contents.locked, s)
    marshal_bool(contents.lockOnWrite, s)
    marshal_i32(contents.moduleLock, s)
    marshal_bool(contents.override, s)
    con4m_sub_marshal(contents.contents, s, m, a)

  con4m_sub_marshal(rt.allSections, s, m, a)
  let docview = rt.sectionDocs.items(sort = true)
  marshal_u16(cast[uint16](docview.len()), s)
  for (secName, docContainer) in docview:
    con4m_sub_marshal(secname, s, m, a)
    con4m_sub_marshal(docContainer.shortdoc, s, m, a)
    con4m_sub_marshal(docContainer.longdoc, s, m, a)

proc unmarshal_attrs(rt: RuntimeState) =
  let
    m = rt.memos
    s = rt.marshalStream
    b = s.unmarshal_bool()

  if not b:
    return

  rt.attrs.initDict()
  var n = s.unmarshal_u32()
  for i in 0 ..< n:
    var
      name     = con4m_sub_unmarshal[Rich](s, m)
      contents = AttrContents()

    contents.tid         = con4m_sub_unmarshal[TypeSpec](s, m)
    contents.isSet       = s.unmarshal_bool()
    contents.locked      = s.unmarshal_bool()
    contents.lockOnWrite = s.unmarshal_bool()
    contents.moduleLock  = s.unmarshal_i32()
    contents.override    = s.unmarshal_bool()
    contents.contents    = con4m_sub_unmarshal[pointer](s, m)

    rt.attrs[name] = contents

  rt.allSections = con4m_sub_unmarshal[Dict[Rich, bool]](s, m)
  rt.sectionDocs.initDict()
  n = s.unmarshal_u16()
  for i in 0 ..< n:
    var
      name = con4m_sub_unmarshal[Rich](s, m)
      c    = DocsContainer()

    c.shortdoc = con4m_sub_unmarshal[Rich](s, m)
    c.longdoc  = con4m_sub_unmarshal[Rich](s, m)

    rt.sectionDocs[name] = c

proc marshal_runtime*(rt: RuntimeState, next_entry_point: int32): Buffer =
  ## The object must not be running when we save, and we assume the stack
  ## is empty (it is a programmer mistake to push things then save before
  ## calling).
  ##
  ## We do not save live FFI info, because for dynamically linked
  ## items, we cannot guarantee they'll resolve, so we need to do that
  ## each time we start back up.

  if rt.running:
    raise newException(ValueError, "Cannot marshal runtime while running.")

  result = new_buffer(16)

  rt.memos         = alloc_marshal_memos()
  rt.next_memoid   = 1
  rt.marshalStream = buffer_outstream(result)

  let
    m = rt.memos
    s = rt.marshalStream
    a = addr rt.next_memoid

  rt.marshal_object(int32(len(rt.obj.moduleContents)))
  marshal_type_environment(s, m, a)
  rt.marshal_attrs()
  marshal_u16(uint16(rt.moduleAllocations.len()), s)
  for item in rt.moduleAllocations:
    marshal_u16(cast[uint16](item.len()), s)
    for p in item:
      marshal_u64(cast[uint64](p), s)


proc unmarshal_runtime*(s: CStream): RuntimeState =
  result               = RuntimeState()
  result.obj           = ZObjectFile()
  result.memos         = alloc_unmarshal_memos()
  result.marshalStream = s

  let
    m = result.memos

  result.unmarshal_object()
  unmarshal_type_environment(s, m)
  result.unmarshal_attrs()
  var
    l = int32(s.unmarshal_u16())
    oneAlloc: seq[pointer]

  for i in 0 ..< l:
    let n = int32(s.unmarshal_u16())
    oneAlloc = @[]
    for j in 0 ..< n:
      oneAlloc.add(cast[pointer](s.unmarshal_u64()))
    result.moduleAllocations.add(oneAlloc)
