import ".."/common
import "."/cmd_base


proc get_basic_object_info*(obj: ZObjectFile): Rope =
  var cells: seq[seq[string]]

  cells.add(@["Magic Value", obj.zeroMagic.toHex().toLowerAscii()])
  cells.add(@["Object Version", $(obj.zcObjectVers)])
  cells.add(@["Global scope alloc'd size", $(obj.globalScopeSz)])
  cells.add(@["Entrypoint", obj.moduleContents[obj.entryPoint - 1].modname])

  if obj.nextEntryPoint < 1:
    cells.add(@["Next Entrypoint", "Unspecified"])
  else:
    cells.add(@["Next Entrypoint",
                obj.moduleContents[obj.nextEntryPoint - 1].modname])

  result = cells.quickTable(title = "Basic object info", verticalHeaders = true,
                                    caption = config_args[0].resolvePath())

proc format_module_params*(obj: ZObjectFile, params: seq[ZParamInfo]): Rope =
  var cells: seq[seq[string]] = @[@["Attr or offset",
                                    "Type",
                                    "Native func?",
                                    "Function index", "Short Doc", "Long Doc"]]
  for item in params:
    var row: seq[string] = @[]
    if item.attr != "":
      echo "Attr = ", item.attr
      row.add(item.attr)
    else:
      echo "offset = ", item.offset
      row.add($item.offset)
    row.add(item.tid.toString())
    if item.native:
      row.add("✓")
    else:
      row.add("✗")
    row.add($item.funcIx)
    row.add(item.shortdoc & " ")
    row.add(item.longdoc & " ")
    cells.add(row)

  return cells.quickTable(title   = "Module param info",
                          caption = "Sorry, haven't had the dump look up " &
                                    "local names or functions yet")

proc get_per_module_info*(obj: ZObjectFile): Rope =
  var cells: seq[seq[string]]

  if obj.symTypes.len() == 0:
    result = h2("No global symbol info")
  else:
    cells = @[@["Variable name", "Static offset", "Type"]]
    for (offset, tid) in obj.symTypes:
      cells.add(@[obj.globals[offset], $(offset), tid.toString()])

    result += cells.quickTable(title = "Global symbol info")

  for item in obj.moduleContents:
    result += h1(text("Module: ") + em(item.modname))

    cells = @[@["Module name", item.modname],
              @["Location",    item.location],
              @["Version",     item.version],
              @["ID",          $(item.moduleId)],
              @["Var storage", $(item.moduleVarSize)],
              @["Code size",   $(item.initSize)]]

    result += cells.quickTable(verticalHeaders = true)

    if item.symTypes.len() == 0:
      result += h2("No module symbols")
    else:
      cells = @[@["Symbol name", "Static offset", "Type"]]
      let view1 = item.datasyms.items(sort = true)

      if view1.len() == 0:
        result +=  h2("No symbol definitions")
      else:
        for (offset, name) in view1:
          for (o2, tid) in item.symTypes:
            if offset == o2:
              cells.add(@[name, offset.toHex().toLowerAscii(), tid.toString()])
              break
        result += cells.quickTable(title = item.modname & " Symbol info")

      let view2 = item.datasyms.items(sort = true)
      if view2.len() == 0:
        result += h2("Unused; bit; minor refactoring needed")
      else:
        for (offset, name) in view2:
          for (o2, tid) in item.symTypes:
            if offset == o2:
              cells.add(@[name, $(offset), tid.toString()])
              break
        result += cells.quickTable(title = item.modname & " data symbols")

    if item.source == "":
      result += h2("No stored source code.")
    else:
      result += h2(text("Source code for ") + em(item.modname))
      result += item.source.pretty()

    if item.parameters.len() == 0:
      result += h2("No stored parameters.")
    else:
      result += obj.format_module_params(item.parameters)

    result += h2(text("Disassembly for ") + em(item.modname))
    result += item.rawReprInstructions(obj)

proc get_obj_func_info*(obj: ZObjectFile): Rope =
  var cells: seq[seq[string]]

  if obj.funcInfo.len() == 0:
    result = h2("No native Con4m functions in binary")
  else:
    cells = @[]

    cells.add(@["Name", "Module Address", "Size", "Type",
                "Param names / offsets"])

    for item in obj.funcInfo:
      var
        paramInfo: seq[string] = @[]
        ptypes:    seq[TypeId] = @[]

      for (offset, tid) in item.symTypes:
        paramInfo.add(item.syms[offset] & ": " & $(offset))
        ptypes.add(tid)

      cells.add(@[item.funcname,
                  item.offset.toHex.toLowerAscii(),
                  $(item.size),
                  tFunc(ptypes).toString(),
                  paramInfo.join(", ")])

    result = cells.quickTable(title = "Con4m native function info")

proc get_obj_ffi_info*(obj: ZObjectFile): Rope =
  var cells: seq[seq[string]]

  if obj.ffiInfo.len() == 0:
    return h2("No foreign functions mapped")

  else:
    cells = @[@["Foreign Name", "Local Name", "Varargs?", "dll names",
              "Param Info"]]
    for item in obj.ffiInfo:
      var
        xname = findStringAt(obj.staticData, item.nameOffset)
        lname = findStringAt(obj.staticData, item.localName)
        vargs = if item.va: "✓" else: "✗"
        dlls: seq[string]
        pinf: seq[string] = @[]

      if item.dlls.len() == 0:
        dlls = @["none required"]
      else:
        for ix in item.dlls:
          dlls.add(findStringAt(obj.staticData, ix))

      for i, ainfo in item.argInfo:
        var s = ""

        if i == item.argInfo.len() - 1:
          s = "return: "

        case ainfo.ourType
        of RTAsMixed:
          s &= "generic +autobox"
        of RTAsPtr:
          s &= "ptr"
        else:
          s &= cast[TypeId](ainfo.ourType).toString()
          s &= " +by-value"

        if ainfo.held:
          s &= " +auto-incref"
        if ainfo.alloced:
          s &= " +no-gc"

        pinf.add(s)

      if pinf.len() == 0:
        pinf = @["void"]

      cells.add(@[xname, lname, vargs, dlls.join(", "),
                  "(" & pinf.join(", ") & ")"])

    return cells.quickTable(title = "Foreign Function Mappings")

proc get_low_level_heap_info*(varmap: Dict[int, string], arena: seq[pointer],
                              title: string): Rope =
  var cells = @[
    @["Variable", "Offset", "Type", "Value", "Repr"]
  ]

  for (offset, name) in varmap.items(sort = true):
    var
      vraw = arena[offset * 2]
      traw = arena[offset * 2 + 1]
      tid  = cast[TypeId](traw)
      val  = cast[int](vraw).toHex().toLowerAscii()
      r    = "(No value)"

    if tid != TBottom:
      r = $(call_repr(vraw, tid))

    cells.add(@[name, $offset, tid.toString, val, r])

  return cells.quickTable(title = title)

proc get_module_heap_info*(rt: RuntimeState, m: ZModuleInfo, title = ""): Rope =
  var
    title  = if title != "":
               title
             else:
               "Current state for module: " & m.modname
    arena  = rt.moduleAllocations[m.moduleId]
    varmap = m.datasyms

  return get_low_level_heap_info(varmap, arena, title)

proc get_entry_heap_info*(rt: RuntimeState, title = ""): Rope =
  return rt.get_module_heap_info(rt.obj.moduleContents[rt.obj.entrypoint - 1], title)

proc get_next_entry_heap_info*(rt: RuntimeState, title = ""): Rope =
  return rt.get_module_heap_info(rt.obj.moduleContents[rt.obj.nextEntrypoint - 1],
                                 title)

proc get_all_heap_info*(rt: RuntimeState): Rope =
  var cells: seq[seq[string]]

  for i, item in rt.module_allocations:
    var
      title:    string
      varmap:   Dict[int, string]

    if rt.moduleAllocations[i].len() == 0:
      continue

    if i == 0:
      var
        title = "Global variable state"
        arena = rt.moduleAllocations[0]

      result += rt.obj.globals.get_low_level_heap_info(arena, title)

    else:
      result += rt.get_module_heap_info(rt.obj.moduleContents[i - 1])

proc get_section_docs*(rt: RuntimeState): Rope =
  let view = rt.sectionDocs.items(sort = true)

  if view.len() == 0:
    return h2("No section documentation found.")

  var cells: seq[seq[Rope]] = @[@[text("Section"), text("Short Doc"),
                                text("Long Doc")]]
  for (k, v) in view:
    var l, s: Rope

    if v.shortdoc == nil:
      s = text("None provided.")
    else:
      s = v.shortdoc

    if v.longdoc == nil:
      l = text("None provided.")
    else:
      l = v.longdoc

    cells.add(@[em(k), s, l])

  return cells.quicktable(title = text("Section Documentation"))

proc cmd_objdump*() =
  var
    rt  = config_args[0].load_object_file()
    obj = rt.obj


  print obj.get_basic_object_info()
  print h2("Global static data")
  print pre(obj.staticData.strDump())
  print obj.get_per_module_info()
  print obj.get_obj_func_info()

  print(h2("Type Catalog"))
  obj.print_type_catalog()

  if rt.usingAttrs:
    if obj.spec.used:
      print(h2("Attribute Specification"))
      obj.spec.print_spec()

    else:
      print(h2("No attribute specification"))

    var secnames: seq[Rope]

    for item in rt.allSections.keys(sort=true):
      secnames.add(@[li(item)])

    if secnames.len() == 0:
      print(h2("No sections defined."))
    else:
      print(h2("Active attribute sections"))
      print(ul(secnames))

    print rt.get_section_docs()

    print(h2("Saved attribute information"))
    rt.print_attributes()
  else:
    print h2("Attributes are not being used.")


  print rt.get_all_heap_info()
