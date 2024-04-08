import "std"/os
import "."/[common, modparams]


template ensure_spec(ctx: RuntimeState) =
  if ctx.obj.spec == nil:
    raise newException(ValueError, "No specification exists.")

template lookup_section(ctx: RuntimeState, section: Rich): SectionSpec =
  ctx.ensure_spec()
  if section.rich_len() == 0:
    ctx.obj.spec.rootSpec
  else:
    let obj_opt = ctx.obj.spec.secSpecs.lookup(section)
    if obj_opt.isNone():
          raise newException(ValueError, "Invalid section name.")
    obj_opt.get()

proc get_section_docs*(ctx: RuntimeState, section: Rich):
                     DocsContainer {.exportc, cdecl.} =
  let sec_obj = ctx.lookup_section(section)

  return DocsContainer(shortdoc: sec_obj.shortdoc, longdoc: sec_obj.doc)

proc get_field_docs*(ctx: RuntimeState, section: Rich, field: Rich):
                   DocsContainer {.exportc, cdecl.} =
  let
    sec_obj   = ctx.lookup_section(section)
    field_opt = sec_obj.fields.lookup(field)

  if field_opt.isNone():
    raise newException(ValueError, "Field not spec'd for that section")

  let field = field_opt.get()

  return DocsContainer(shortdoc: field.shortdoc, longdoc: field.doc)

proc get_all_field_docs*(ctx: RuntimeState, section: Rich):
                  Dict[Rich, DocsContainer] {.exportc, cdecl.} =
  result.initDict()

  let sec_obj = ctx.lookup_section(section)

  for (k, v) in sec_obj.fields.items(sort = true):
    result[k] = DocsContainer(shortdoc: v.shortdoc, longdoc: v.doc)

proc get_all_section_docs*(ctx: RuntimeState):
                         Dict[Rich, DocsContainer] {.exportc, cdecl.} =
  result.initDict()
  let root = ctx.lookup_section(rich"")
  result[rich""] = DocsContainer(shortdoc: root.shortdoc, longdoc: root.doc)

  for (k, v) in ctx.obj.spec.secSpecs.items(sort = true):
    result[k] = DocsContainer(shortdoc: v.shortdoc, longdoc: v.doc)

proc base_get_module_docs(ctx: RuntimeState, ix: int):
                         DocsContainer {.exportc, cdecl.} =
  let zm = ctx.obj.moduleContents[ix]

  return DocsContainer(shortdoc: r(zm.shortdoc),
                       longdoc:  r(zm.longdoc))

proc get_all_module_docs*(ctx: RuntimeState):
                        Dict[Rich, DocsContainer] {.exportc, cdecl.} =
  result.initDict()

  for i, item in ctx.obj.moduleContents:
    result[r(item.key)] = DocsContainer(shortdoc: r(item.shortdoc),
                                        longdoc:  r(item.longdoc))

proc get_module_docs*(ctx: RuntimeState, n: Rich):
                    DocsContainer {.exportc, cdecl.} =
  for item in ctx.obj.moduleContents:
    if n.toNimStr() == item.key or n.toNimStr() == item.modname:
      return DocsContainer(shortdoc: r(item.shortdoc),
                           longdoc: r(item.longdoc))

proc get_attr_docs*(ctx: RuntimeState):
                  Dict[Rich, DocsContainer] {.exportc, cdecl.} =
  return ctx.sectionDocs


proc get_all_param_docs*(ctx: RuntimeState, module: Rich):
                   Dict[Rich, DocsContainer] {.exportc, cdecl.} =
  result.initDict()

  for mobj in ctx.obj.moduleContents:
    if mobj.key != module.toNimStr() and mobj.modname != module.toNimStr():
      continue

    for param in mobj.parameters:
      let
        container = DocsContainer(shortdoc: param.shortdoc,
                                  longdoc: param.longdoc)
        name      = param.get_param_name(mobj)

      if result.lookup(name).isSome():
        result[joinPath(mobj.location, name.toNimStr()).r()] = container
      else:
        result[name] = container


proc get_param_docs*(ctx: RuntimeState):
                   Dict[Rich, DocsContainer] {.exportc, cdecl.} =
  result.initDict()

  for module in ctx.obj.moduleContents:
    for param in module.parameters:
      let
        container = DocsContainer(shortdoc: param.shortdoc,
                                    longdoc: param.longdoc)
        name      = param.get_param_name(module)

      if result.lookup(name).isSome():
        result[joinPath(module.location, name.toNimStr()).r()] = container
      else:
        result[name] = container

proc get_function_info*(ctx: RuntimeState): seq[FuncDocs] {.exportc, cdecl.} =

  for fn in ctx.obj.ffiInfo:
    let
      endix   = ctx.obj.staticdata.find('\0', fn.nameoffset)
      name    = ctx.obj.staticdata[fn.nameoffset ..< endix]
      finfo   = FuncDocs(funcname: name, tid: fn.tid, mid: fn.mid,
                         shortdoc: fn.shortdoc, longdoc: fn.longdoc,
                         extern: true)

    var param_names: seq[string]
    # TODO: arg names aren't filled in yet so that kinda sucks.
    for i, item in fn.arginfo[0 ..< 1]:
      if item.name != "":
        param_names.add(item.name)
      else:
        param_names.add("param_" & $(i + 1))

    finfo.paramNames = param_names

    result.add(finfo)

  for fn in ctx.obj.funcInfo:
    let finfo = FuncDocs(funcname: fn.funcname, tid: fn.tid, mid: fn.mid,
                         shortdoc: fn.shortdoc, longdoc: fn.longdoc,
                         extern: false)

    var param_names: seq[string]
    for (i, item) in fn.syms.items():
      param_names.add(item)

    finfo.paramNames = param_names

    result.add(finfo)
