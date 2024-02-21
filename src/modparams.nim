import "."/[stchecks, attrstore]

proc get_parameter_info*(ctx: RuntimeState): seq[ZParamExport] =
  ## Pass back the big list of all the parameters we know about, with
  ## status information.
  for module in ctx.obj.moduleContents:
    for i, param in module.parameters:
      var
        data = ZParamExport(modid:    int32(module.moduleId),
                            modname:  module.modname,
                            paramid:  int32(i),
                            private:  param.private,
                            shortdoc: param.shortdoc,
                            longdoc:  param.longdoc,
                            tid:      param.tid)

      if param.attr != "":
        data.name = param.attr
        var
          err: bool
          tid: TypeId

        let val = ctx.get(data.name, err, addr tid)
        if not err:
          data.havedefault = true
          data.default     = val
          data.tid         = tid
          continue
      else:
        data.name = ctx.curModule.datasyms[param.offset]
        let
          p         = param.offset
          dataaddr  = addr ctx.moduleAllocations[module.moduleId][p]
          typeaddr  = cast[ptr TypeId](cast[uint](dataaddr) +
                                       uint(sizeof(pointer)))
        if typeaddr[] != TBottom:
          data.havedefault = true
          data.default     = dataaddr[]
          data.tid         = typeaddr[]
          continue

      if param.havedefault:
        data.havedefault = true
        data.default     = param.default

      elif param.iFnIx != -1:
        if param.iNative:
          ctx.z_ffi_call(cast[int](param.iFnIx))
          data.default = ctx.returnRegister
          data.tid     = cast[TypeId](ctx.rrType)
        else:
          data.default = ctx.foreign_z_call(cast[int](param.iFnIx))
          data.tid     = cast[TypeId](ctx.rrType)

proc get_current_instruction(ctx: RuntimeState):
                            ptr ZInstruction {.importc, cdecl.}

proc run_param_validator*(ctx: RuntimeState, p: ZParamInfo,
                          val: pointer, t: TypeId): string =

  if p.tid.tCopy().unify(t) == TBottom:
    return "Specified type for parameter was not compatable with the " &
           "stored type (" & t.toString() & ")"
  if p.vFnIx != -1:
    var cb = ZCallback(impl: cast[pointer](int64(p.vFnIx)))

    if not p.vNative:
      cb.ffi = true

    let s = ctx.run_callback_internal(addr cb, [(val, p.tid)])

    if s != nil and s.len() != 0:
      return s.toNimStr()

proc set_user_param*(ctx: RuntimeState, mid: int, paramix: int,
                     value: pointer, t: TypeId): string {.exportc, cdecl.} =
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
                   string {.exportc, cdecl.} =
  if param.attr != "":
    return param.attr
  else:
    return m.datasyms[param.offset]


proc get_param_value*(ctx: RuntimeState, param: ZParamInfo):
                    (pointer, TypeId) {.exportc, cdecl.} =
  if param.userType != TBottom:
    return (param.userparam, param.userType)

  if param.haveDefault:
    return (param.default, param.tid)

  var name = param.get_param_name(ctx.curModule)

  ctx.runtimeError("ParamNotSet", @[name, ctx.curModule.modName])
