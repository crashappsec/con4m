import "."/[stchecks, attrstore, vm_func]

proc get_parameter_info*(ctx: RuntimeState): seq[ZParamExport] =
  ## Pass back the big list of all the parameters we know about, with
  ## status information.
  for module in ctx.obj.moduleContents:
    for i, param in module.parameters:
      var
        data = ZParamExport(modid:    int32(module.moduleId),
                            modname:  r(module.modname),
                            paramid:  int32(i),
                            private:  param.private,
                            shortdoc: param.shortdoc,
                            longdoc:  param.longdoc,
                            tid:      param.tid)

      if param.attr.rich_len() != 0:
        data.name = param.attr
        var
          err: bool
          tid: TypeSpec

        let val = ctx.get(data.name.toNimStr(), err, addr tid)
        if not err:
          data.default     = val
          data.tid         = tid
          continue
      else:
        data.name = cast[Rich](ctx.curModule.datasyms[param.offset])
        let
          p         = param.offset
          dataaddr  = addr ctx.moduleAllocations[module.moduleId][p]
          typeaddr  = cast[ptr TypeSpec](cast[uint](dataaddr) +
                                       uint(sizeof(pointer)))
        if typeaddr[] != tspec_error() and typeaddr[] != nil:
          data.default     = dataaddr[]
          data.tid         = typeaddr[]
          continue

      data.default = param.default

      if data.default == nil and param.iFnIx != -1:
        if param.iNative:
          ctx.z_ffi_call(cast[int](param.iFnIx))
          data.default = ctx.returnRegister
          data.tid     = cast[TypeSpec](ctx.rrType)
        else:
          data.default = ctx.foreign_z_call(cast[int](param.iFnIx))
          data.tid     = cast[TypeSpec](ctx.rrType)

proc get_current_instruction(ctx: RuntimeState):
                            ptr ZInstruction {.importc, cdecl.}

proc run_param_validator*(ctx: RuntimeState, p: ZParamInfo,
                          val: pointer, t: TypeSpec): string =

  if p.tid.tspec_compare(t) == false:
    return "Specified type for parameter was not compatable with the " &
           "stored type (" & t.toString() & ")"
  if p.vFnIx != -1:
    var cb = ZCallback(impl: cast[pointer](int64(p.vFnIx)))

    if not p.vNative:
      cb.ffi = true

    let s = ctx.run_callback_internal(addr cb, [(val, p.tid)])

    if s != nil and s.len() != 0:
      return cast[Rich](s).toNimStr()

proc set_user_param*(ctx: RuntimeState, mid: int, paramix: int,
                     value: pointer, t: TypeSpec): string {.exportc, cdecl.} =
  ## Sets the user parameter, and runs the validator. If it returns
  ## an error message, then it's feedback for the user, and try
  ## again. Otherwise, it succeeded.

  let
    module = ctx.obj.moduleContents[mid - 1]
    param  = module.parameters[paramix]

  result = ctx.run_param_validator(param, value, t)

  param.userType  = t
  param.userparam = value

proc get_param_name*(param: ZParamInfo, m: ZModuleInfo):
                   Rich {.exportc, cdecl.} =
  if param.attr.rich_len() != 0:
    return param.attr
  else:
    return cast[Rich](m.datasyms[param.offset])


proc get_param_value*(ctx: RuntimeState, param: ZParamInfo):
                    (pointer, TypeSpec) {.exportc, cdecl.} =
  if param.userType.tspec_compare(tspec_typevar()) == true:
    return (param.userparam, param.userType)

  if param.default != nil:
    return (param.default, param.tid)

  var name = param.get_param_name(ctx.curModule)

  ctx.runtimeError("ParamNotSet", @[name.toNimStr(), ctx.curModule.modName])
