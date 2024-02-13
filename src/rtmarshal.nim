import "."/[common, attrstore]
import "ztypes"/[api, marshal]

proc marshal_int_list[T](l: seq[T]): C4Str =
  list_marshal_helper(l):
    toAdd.add(cast[pointer](item).marshal_64_bit_value())

proc unmarshal_int_list(p: var cstring): seq[int] =
  list_unmarshal_helper(p):
    let i = cast[int](p.unmarshal_64_bit_value())
    result.add(i)

proc marshal_ffi_arg_info(args: seq[ZFFiArgInfo]): C4Str =
  list_marshal_helper(args):
    toAdd.add(item.held.marshal_bool())
    toAdd.add(item.alloced.marshal_bool())
    toAdd.add(item.argType.marshal_16_bit_value())
    toAdd.add(item.ourType.marshal_32_bit_value())

proc unmarshal_ffi_arg_info(p: var cstring): seq[ZFFiArgInfo] =
  list_unmarshal_helper(p):
    var a = ZFFiArgInfo()

    a.held    = p.unmarshal_bool()
    a.alloced = p.unmarshal_bool()
    a.argtype = p.unmarshal_16_bit_value()
    a.ourtype = p.unmarshal_32_bit_value()

    result.add(a)

proc marshal_one_ffi_info_obj(f: ZFFiInfo): C4Str =
  basic_marshal_helper:
    toAdd.add(cast[pointer](f.nameOffset).marshal_64_bit_value())
    toAdd.add(cast[pointer](f.localName).marshal_64_bit_value())
    toAdd.add(f.va.marshal_bool())
    toAdd.add(f.dlls.marshal_int_list())
    toAdd.add(f.argInfo.marshal_ffi_arg_info())

proc unmarshal_one_ffi_info_obj(p: var cstring): ZFFiInfo =
  result = ZFFiInfo()

  result.nameOffset = int(p.unmarshal_64_bit_value())
  result.localName  = int(p.unmarshal_64_bit_value())
  result.va         = p.unmarshal_bool()
  result.dlls       = p.unmarshal_int_list()
  result.argInfo    = p.unmarshal_ffi_arg_info()

proc marshal_sym_types(l: var seq[(int, TypeId)]): C4Str =
  view_marshal_helper(l):
    toAdd.add(cast[pointer](k).marshal_64_bit_value())
    toAdd.add(cast[pointer](v).marshal_64_bit_value())

proc unmarshal_sym_types(p: var cstring): seq[(int, TypeId)] =
  list_unmarshal_helper(p):
    let
      k = int(p.unmarshal_64_bit_value())
      v = cast[TypeId](p.unmarshal_64_bit_value())

    result.add((k, v))

proc marshal_sym_names(d: Dict[int, string]): C4Str =
  dictionary_marshal_helper(d):
    toAdd.add(k.int32().marshal_32_bit_value())
    toAdd.add(v.marshal_nim_string())

proc unmarshal_sym_names(p: var cstring): Dict[int, string] =
  result.initDict()

  list_unmarshal_helper(p):
    let
      k = int(p.unmarshal_32_bit_value())
      v = p.unmarshal_nim_string()

    result[k] = v

proc marshal_one_func(f: ZFnInfo): C4Str =
  basic_marshal_helper:
    toAdd.add(f.funcName.marshal_nim_string())
    toAdd.add(f.syms.marshal_sym_names())
    toAdd.add(f.paramTypes.marshal_int_list())
    toAdd.add(f.symTypes.marshal_sym_types())
    toAdd.add(cast[pointer](f.offset).marshal_64_bit_value())
    toAdd.add(cast[pointer](f.size).marshal_64_bit_value())

proc unmarshal_one_func(p: var cstring): ZFnInfo =
  result = ZFnInfo()

  result.funcName   = p.unmarshal_nim_string()
  result.syms       = p.unmarshal_sym_names()
  result.paramTypes = cast[seq[TypeId]](p.unmarshal_int_list())
  result.symTypes   = p.unmarshal_sym_types()
  result.offset     = int(p.unmarshal_64_bit_value())
  result.size       = int(p.unmarshal_64_bit_value())

proc marshal_byte_code(bc: seq[ZInstruction]): C4Str =
  list_marshal_helper(bc):
    toAdd.add(marshal_8_bit_value(cast[uint8](item.op)))
    toAdd.add(marshal_8_bit_value(0'u8))
    toAdd.add(marshal_16_bit_value(item.moduleId))
    toAdd.add(marshal_32_bit_value(item.lineNo))
    toAdd.add(marshal_32_bit_value(item.arg))
    toAdd.add(cast[pointer](item.immediate).marshal_64_bit_value())
    toAdd.add(cast[pointer](item.typeInfo).marshal_64_bit_value())

proc unmarshal_byte_code(p: var cstring): seq[ZInstruction] =
  list_unmarshal_helper(p):
    var instr = ZInstruction()

    instr.op        = cast[ZOp](p.unmarshal_8_bit_value())
    discard           p.unmarshal_8_bit_value()
    instr.moduleId  = p.unmarshal_16_bit_value()
    instr.lineNo    = p.unmarshal_32_bit_value()
    instr.arg       = p.unmarshal_32_bit_value()
    instr.immediate = int64(p.unmarshal_64_bit_value())
    instr.typeInfo  = cast[TypeId](p.unmarshal_64_bit_value())
    result.add(instr)

proc marshal_one_module(rt: RuntimeState, m: ZModuleInfo): C4Str =
  basic_marshal_helper:
    toAdd.add(m.modname.marshal_nim_string())
    toAdd.add(m.location.marshal_nim_string())
    toAdd.add(m.version.marshal_nim_string())
    toAdd.add(m.symTypes.marshal_sym_types())
    toAdd.add(m.codesyms.marshal_sym_names())
    toAdd.add(m.datasyms.marshal_sym_names())

    toAdd.add(m.source.marshal_nim_string())
    toAdd.add(cast[pointer](m.moduleId).marshal_64_bit_value())
    toAdd.add(cast[pointer](m.moduleVarSize).marshal_64_bit_value())
    toAdd.add(cast[pointer](m.initSize).marshal_64_bit_value())
    toAdd.add(m.instructions.marshal_byte_code())

proc unmarshal_one_module(rt: RuntimeState, p: var cstring): ZModuleInfo =
  result = ZModuleInfo()

  result.modname       = p.unmarshal_nim_string()
  result.location      = p.unmarshal_nim_string()
  result.version       = p.unmarshal_nim_string()
  result.symTypes      = p.unmarshal_sym_types()
  result.codeSyms      = p.unmarshal_sym_names()
  result.dataSyms      = p.unmarshal_sym_names()
  result.source        = p.unmarshal_nim_string()
  result.moduleId      = int(p.unmarshal_64_bit_value())
  result.moduleVarSize = int(p.unmarshal_64_bit_value())
  result.initSize      = int(p.unmarshal_64_bit_value())
  result.instructions  = p.unmarshal_byte_code()

proc marshal_validators(rt: RuntimeState, vlist: seq[Validator]): C4Str =
  discard
  # TODO: there's some more work to do here.

proc unmarshal_validators(rt: RuntimeState, p: var cstring): seq[Validator] =
  discard

proc marshal_field_spec(rt: RuntimeState, fs: FieldSpec): C4Str =
  basic_marshal_helper:
    toAdd.add(fs.name.marshal_nim_string())
    toAdd.add(marshal_64_bit_value(cast[pointer](fs.tid)))
    toAdd.add(fs.lockOnWrite.marshal_bool())
    toAdd.add(fs.haveDefault.marshal_bool())
    if fs.haveDefault:
      toAdd.add(marshal(fs.defaultVal, fs.tid, rt.memos))
    #toAdd.add(rt.marshal_validators(fs.validators))
    toAdd.add(fs.hidden.marshal_bool())
    toAdd.add(fs.required.marshal_bool())
    toAdd.add(fs.doc.rope_marshal(TRich, rt.memos))
    toAdd.add(fs.shortdoc.rope_marshal(TRich, rt.memos))
    toAdd.add(marshal_64_bit_value(cast[pointer](fs.errIx)))
    toAdd.add(fs.exclusions.marshal_nimstr_list())
    toAdd.add(fs.deferredType.marshal_nim_string())

proc unmarshal_field_spec(rt: RuntimeState, p: var cstring): FieldSpec =
  result = FieldSpec()

  result.name        = p.unmarshal_nim_string()
  result.tid         = cast[TypeId](p.unmarshal_64_bit_value())
  result.lockOnWrite = p.unmarshal_bool()
  result.haveDefault = p.unmarshal_bool()

  if result.haveDefault:
    result.defaultVal = p.unmarshal(result.tid, rt.memos)

  #result.validators   = rt.unmarshal_validators(p)
  result.hidden       = p.unmarshal_bool()
  result.required     = p.unmarshal_bool()
  result.doc          = p.rope_unmarshal(TRich, rt.memos)
  result.shortdoc     = p.rope_unmarshal(TRich, rt.memos)
  result.errIx        = int(p.unmarshal_64_bit_value())
  result.exclusions   = p.unmarshal_nimstr_list()
  result.deferredType = p.unmarshal_nim_string()

proc marshal_fields(rt: RuntimeState, f: Dict[string, FieldSpec]): C4Str =
  if f == nil:
    return marshal_64_bit_value(nil)

  dictionary_marshal_helper(f):
    toAdd.add(k.marshal_nim_string())
    toAdd.add(rt.marshal_field_spec(v))

proc unmarshal_fields(rt: RuntimeState, p: var cstring):
                     Dict[string, FieldSpec] =
  result.initDict()

  list_unmarshal_helper(p):
    let
      k = p.unmarshal_nim_string()
      v = rt.unmarshal_field_spec(p)

    result[k] = v

proc marshal_one_section(rt: RuntimeState, sec: SectionSpec): C4Str =
  basic_marshal_helper:
    toAdd.add(sec.name.marshal_nim_string())
    toAdd.add(cast[pointer](sec.maxAllowed).marshal_64_bit_value())
    toAdd.add(rt.marshal_fields(sec.fields))
    toAdd.add(sec.userDefOk.marshal_bool())
    #toAdd.add(rt.marshal_validators(sec.validators))
    toAdd.add(sec.hidden.marshal_bool())
    toAdd.add(sec.doc.rope_marshal(TRich, rt.memos))
    toAdd.add(sec.shortdoc.rope_marshal(TRich, rt.memos))
    toAdd.add(sec.allowedSections.marshal_nimstr_list())
    toAdd.add(sec.requiredSections.marshal_nimstr_list())

proc unmarshal_one_section(rt: RuntimeState, p: var cstring): SectionSpec =
  result = SectionSpec()

  result.name             = p.unmarshal_nim_string()
  result.maxAllowed       = int(p.unmarshal_64_bit_value())
  result.fields           = rt.unmarshal_fields(p)
  result.userDefOk        = p.unmarshal_bool()
  #result.validators       = rt.unmarshal_validators(p)
  result.hidden           = p.unmarshal_bool()
  result.doc              = p.rope_unmarshal(TRich, rt.memos)
  result.shortdoc         = p.rope_unmarshal(TRich, rt.memos)
  result.allowedSections  = p.unmarshal_nimstr_list()
  result.requiredSections = p.unmarshal_nimstr_list()

proc marshal_sections(rt: RuntimeState,
                      secSpecs: Dict[string, SectionSpec]): C4Str =
  dictionary_marshal_helper(secSpecs):
    toAdd.add(k.marshal_nim_string())
    toAdd.add(rt.marshal_one_section(v))

proc unmarshal_sections(rt: RuntimeState, p: var cstring):
                       Dict[string, SectionSpec] =
  result.initDict()

  list_unmarshal_helper(p):
    let
      k = p.unmarshal_nim_string()
      v = rt.unmarshal_one_section(p)

    result[k] = v

proc marshal_spec(rt: RuntimeState, spec: ValidationSpec): C4Str =
  if spec == nil:
    return marshal_bool(false)

  basic_marshal_helper:
    toAdd.add(spec.used.marshal_bool())
    if spec.used:
      toAdd.add(rt.marshal_one_section(spec.rootSpec))
      toAdd.add(rt.marshal_sections(spec.secSpecs))

proc unmarshal_spec(rt: RuntimeState, p: var cstring): ValidationSpec =
  result = ValidationSpec()

  result.used = p.unmarshal_bool()
  if result.used:
    result.rootSpec = rt.unmarshal_one_section(p)
    result.secSpecs = rt.unmarshal_sections(p)

proc marshal_ffi_info(rt: RuntimeState, l: var seq[ZFfiInfo]): C4Str =
  list_marshal_helper(l):
    toAdd.add(item.marshal_one_ffi_info_obj())

proc unmarshal_ffi_info(rt: RuntimeState, p: var cstring): seq[ZffiInfo] =
  list_unmarshal_helper(p):
    result.add(p.unmarshal_one_ffi_info_obj())

proc marshal_func_info(rt: RuntimeState, l: var seq[ZFnInfo]): C4Str =
  list_marshal_helper(l):
    toAdd.add(item.marshal_one_func())

proc unmarshal_func_info(rt: RuntimeState, p: var cstring): seq[ZFnInfo] =
  list_unmarshal_helper(p):
    result.add(p.unmarshal_one_func())

proc marshal_modules(rt: RuntimeState, l: var seq[ZModuleInfo]): C4Str =
  list_marshal_helper(l):
    toAdd.add(rt.marshal_one_module(item))

proc unmarshal_modules(rt: RuntimeState, p: var cstring): seq[ZModuleInfo] =
  lisT_unmarshal_helper(p):
    result.add(rt.unmarshal_one_module(p))

proc marshal_types(d: Dict[TypeId, int]): C4Str =
  dictionary_marshal_helper(d):
    toAdd.add(cast[pointer](k).marshal_64_bit_value())
    toAdd.add(cast[pointer](v).marshal_64_bit_value())

proc unmarshal_types(p: var cstring): Dict[TypeId, int] =
  result.initDict()
  list_unmarshal_helper(p):
    var
      k = cast[TypeId](p.unmarshal_64_bit_value())
      v = int(p.unmarshal_64_bit_value())

    result[k] = v

proc marshal_object(rt: RuntimeState, nextmid: int32): C4Str =
  basic_marshal_helper:
    toAdd.add(cast[pointer](rt.obj.zeroMagic).marshal_64_bit_value())
    toAdd.add(rt.obj.zcObjectVers.marshal_16_bit_value())
    toAdd.add(rt.obj.staticData.marshal_nim_string())
    toAdd.add(rt.obj.tInfo.marshal_types())
    toAdd.add(rt.obj.globals.marshal_sym_names())
    toAdd.add(rt.obj.symTypes.marshal_sym_types())
    toAdd.add(rt.obj.globalScopeSz.int32().marshal_32_bit_value())
    toAdd.add(rt.marshal_modules(rt.obj.moduleContents))
    toAdd.add(rt.obj.entryPoint.marshal_32_bit_value())
    toAdd.add(nextmid.marshal_32_bit_value())
    toAdd.add(rt.marshal_func_info(rt.obj.funcInfo))
    toAdd.add(rt.marshal_ffi_info(rt.obj.ffiInfo))
    toAdd.add(rt.marshal_spec(rt.obj.spec))

proc unmarshal_object(rt: RuntimeState, p: var cstring) =
  if p.unmarshal_64_bit_value() != rt.obj.zeroMagic:
    raise newException(ValueError, "Invalid magic (expect 0x0c001dea x2)")

  if p.unmarshal_16_bit_value() != rt.obj.zcObjectVers:
    raise newException(ValueError, "Invalid object version.")
  rt.obj.staticData     = p.unmarshal_nim_string()
  rt.obj.tInfo          = p.unmarshal_types()
  rt.obj.globals        = p.unmarshal_sym_names()
  rt.obj.symTypes       = p.unmarshal_sym_types()
  rt.obj.globalScopeSz  = int(p.unmarshal_32_bit_value())
  rt.obj.moduleContents = rt.unmarshal_modules(p)
  rt.obj.entrypoint     = p.unmarshal_32_bit_value()
  rt.obj.nextentrypoint = p.unmarshal_32_bit_value()
  rt.obj.funcInfo       = rt.unmarshal_func_info(p)
  rt.obj.ffiInfo        = rt.unmarshal_ffi_info(p)
  rt.obj.spec           = rt.unmarshal_spec(p)

proc marshal_attrs(rt: RuntimeState): C4Str =
  dictionary_marshal_helper(rt.attrs):
    toAdd.add(k.marshal_nim_string())
    toAdd.add(cast[pointer](v.tid).marshal_64_bit_value())
    toAdd.add(v.isSet.marshal_bool())
    if v.isSet:
      toAdd.add(v.locked.marshal_bool())
      toAdd.add(v.lockOnWrite.marshal_bool())
      toAdd.add(v.override.marshal_bool())
      toAdd.add(v.contents.marshal(v.tid, rt.memos))

proc unmarshal_attrs(rt: RuntimeState, p: var cstring) =
  rt.attrs.initDict()

  list_unmarshal_helper(p):
    var
      k    = p.unmarshal_nim_string()
      t    = cast[TypeId](p.unmarshal_64_bit_value())
      s    = p.unmarshal_bool()
      l    = false
      wrlk = false
      o    = false
      c    = pointer(nil)

    if s:
      l    = p.unmarshal_bool()
      wrlk = p.unmarshal_bool()
      o    = p.unmarshal_bool()
      c    = p.unmarshal(t, rt.memos)

      discard rt.set(k, c, t, l, o, internal = true, wrlk)

proc marshal_section_list(rt: RuntimeState): C4Str =
  let secs = rt.allSections.keys(sort=true)
  list_marshal_helper(secs):
    toAdd.add(item.marshal_nim_string())

proc unmarshal_section_list(rt: RuntimeState, p: var cstring) =
  rt.allSections.initDict()
  list_unmarshal_helper(p):
    rt.allSections[p.unmarshal_nim_string()] = true

proc marshal_one_arena(rt: RuntimeState, ix, sz: int): C4Str =
  let arenaBytes: uint64 = uint64(sz * (2 * sizeof(pointer)))

  basic_marshal_helper:
    var contents = newC4Str(int64(arenaBytes))
    if arenaBytes != 0:
      copyMem(contents, addr rt.moduleAllocations[ix][0], arenaBytes)

    toAdd.add(marshal_64_bit_value(cast[pointer](arenaBytes)))

    if arenaBytes != 0:
      toAdd.add(contents)

proc unmarshal_one_arena(p: var cstring): seq[pointer] =
  let l = p.unmarshal_32_bit_value() div (sizeof(pointer))

  if l == 0:
    return

  result = newSeq[pointer](l)

  for i in 0 ..< l:
    result.add(cast[pointer](p.unmarshal_64_bit_value()))

proc marshal_module_allocations(rt: RuntimeState): C4Str =
  basic_marshal_helper:
    toAdd.add(marshal_32_bit_value(int32(rt.obj.moduleContents.len() + 1)))
    toAdd.add(rt.marshal_one_arena(0, rt.obj.globalScopeSz))
    for i, item in rt.obj.moduleContents:
      toAdd.add(rt.marshal_one_arena(i + 1, item.moduleVarSize))

proc unmarshal_module_allocations(rt: RuntimeState, p: var cstring) =
  list_unmarshal_helper(p):
    rt.moduleAllocations.add(p.unmarshal_one_arena())

proc marshal_object_props(d: Dict[string, TypeId]): C4Str =
  dictionary_marshal_helper(d):
    toAdd.add(k.marshal_nim_string())
    toAdd.add(marshal_64_bit_value(cast[pointer](v.followForwards())))

proc unmarshal_object_props(p: var cstring): Dict[string, TypeId] =
  result.initDict()

  list_unmarshal_helper(p):
    let
      k = p.unmarshal_nim_string()
      v = cast[TypeId](p.unmarshal_64_bit_value())

    result[k] = v

proc marshal_type_store(): C4Str =
  let view  = typeStore.items()
  var count = 0'i32

  basic_marshal_helper:
    for (k, v) in view:
      if v.typeId != k:
        # This entry is forwarded.
        continue
      count += 1
      toAdd.add(marshal_64_bit_value(cast[pointer](k)))
      toAdd.add(marshal_bool(v.isLocked))

      var titems: seq[TypeId]
      for item in v.items:
        titems.add(item.followForwards())
      toAdd.add(marshal_int_list(titems))
      toAdd.add(marshal_32_bit_value(cast[int32](v.kind)))
      case v.kind
      of C4TVar:
        toAdd.add(marshal_64_bit_value(cast[pointer](v.tvarId)))
        let haveLocalName = v.localName.isSome()
        toAdd.add(haveLocalName.marshal_bool())
        if haveLocalName:
          toAdd.add(v.localName.get().marshal_nim_string())

      of C4Func:
        toAdd.add(v.va.marshal_bool())

      of C4Struct:
        toAdd.add(v.name.marshal_nim_string())
        toAdd.add(v.props.marshal_object_props())

      else:
        discard

    toAdd = @[marshal_32_bit_value(count)] & toAdd

proc unmarshal_type_store(p: var cstring) =
  typeStore = nil
  typeStore.initDict()

  list_unmarshal_helper(p):
    var
      k      = cast[TypeId](p.unmarshal_64_bit_value())
      lock   = p.unmarshal_bool()
      titems = cast[seq[TypeId]](p.unmarshal_int_list())
      kind   = cast[C4TypeKind](p.unmarshal_32_bit_value())
      to     = TypeRef(typeid: k, isLocked: lock, items: titems, kind: kind)

    case kind
    of C4TVar:
      to.tvarId = cast[TypeId](p.unmarshal_64_bit_value())
      if p.unmarshal_bool():
        to.localName = some(p.unmarshal_nim_string())

    of C4Func:
      to.va = p.unmarshal_bool()

    of C4Struct:
      to.name  = p.unmarshal_nim_string()
      to.props = p.unmarshal_object_props()

    else:
      discard

    typeStore[k] = to

proc marshal_runtime*(rt: RuntimeState, next_entry: int32): C4Str =
  ## The object must not be running when we save, and we assume the stack
  ## is empty (it is a programmer mistake to push things then save before
  ## calling).
  ##
  ## We do not save live FFI info, because for dynamically linked
  ## items, we cannot guarantee they'll resolve, so we need to do that
  ## each time we start back up.

  if rt.running:
    raise newException(ValueError, "Cannot marshal runtime while running.")

  rt.memos = Memos()
  rt.memos.map.initDict()

  basic_marshal_helper:
      toAdd.add(rt.marshal_object(next_entry))
      toAdd.add(marshal_64_bit_value(cast[pointer](getVarId())))
      toAdd.add(marshal_type_store())
      toAdd.add(rt.marshal_section_list())
      toAdd.add(rt.marshal_attrs())
      toAdd.add(rt.marshal_module_allocations())

proc unmarshal_runtime*(s: C4Str): RuntimeState =
  var
    l = s.len()
    p = cast[cstring](s)
    e = cast[cstring](s)

  e.pointer_add(l)  # Seek to the end
  result       = RuntimeState()
  result.obj   = ZObjectFile()
  result.memos = Memos()


  result.obj.globals.initDict()
  result.obj.tinfo.initDict()
  result.memos.map.initDict()

  result.unmarshal_object(p)
  setVarId(uint64(p.unmarshal_64_bit_value()))
  p.unmarshal_type_store()
  result.unmarshal_section_list(p)
  result.unmarshal_attrs(p)
  result.unmarshal_module_allocations(p)
