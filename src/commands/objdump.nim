import ".."/common
import "."/cmd_base


proc get_basic_object_info*(obj: ZObjectFile): Grid =
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

  result = cells.table(title = "Basic object info", header_rows = 0,
                               header_cols = 1,
                               caption = config_args[0].resolvePath())

proc format_module_params*(obj: ZObjectFile, params: seq[ZParamInfo]): Grid =
  var cells: seq[seq[string]] = @[@["Attr or offset",
                                    "Type",
                                    "Native Validator?",
                                    "Validator index", "Short Doc", "Long Doc"]]
  for item in params:
    var row: seq[string] = @[]
    if item.attr.rich_len() != 0:
      row.add(item.attr.toNimStr())
    else:
      row.add($item.offset)
    row.add(item.tid.toString())
    if item.vnative:
      row.add("✓")
    else:
      row.add("✗")
    row.add($item.vfnIx)
    row.add(item.shortdoc.toNimStr() & " ")
    row.add(item.longdoc.toNimStr() & " ")
    cells.add(row)

  return cells.table(title = "Module param info",
                     caption = "Sorry, haven't had the dump look up " &
                       "local names or functions yet")

proc get_per_module_info*(obj: ZObjectFile): Grid =
  var
    cells: seq[seq[string]]
    fseq:  seq[Grid]


  if obj.symTypes.len() == 0:
    fseq.add(cell("No global symbol info", "h2"))
  else:
    cells = @[@["Variable name", "Static offset", "Type"]]
    for (offset, tid) in obj.symTypes:
      cells.add(@[obj.globals[offset], $(offset), tid.toString()])

    fseq.add(cells.table(title = "Global symbol info"))

  for item in obj.moduleContents:
    fseq.add(cell(text("Module: ") + em(item.modname), "h1"))

    cells = @[@["Module name", item.modname],
              @["Location",    item.location],
              @["Version",     item.version],
              @["ID",          $(item.moduleId)],
              @["Var storage", $(item.moduleVarSize)],
              @["Code size",   $(item.initSize)]]

    fseq.add(cells.table(header_rows = 0, header_cols = 1))

    if item.symTypes.len() == 0:
      fseq.add(cell("No module symbols", "h2"))
    else:
      cells = @[@["Symbol name", "Static offset", "Type"]]
      let view1 = item.datasyms.items(sort = true)

      if view1.len() == 0:
        fseq.add(cell("No symbol definitions", "h2"))
      else:
        for (offset, name) in view1:
          for (o2, tid) in item.symTypes:
            if offset == o2:
              cells.add(@[name, offset.toHex().toLowerAscii(), tid.toString()])
              break
        fseq.add(cells.table(title = item.modname & " Symbol info"))

      let view2 = item.datasyms.items(sort = true)
      if view2.len() == 0:
        fseq.add(cell("Unused; bit; minor refactoring needed", "h2"))
      else:
        for (offset, name) in view2:
          for (o2, tid) in item.symTypes:
            if offset == o2:
              cells.add(@[name, $(offset), tid.toString()])
              break
        fseq.add(cells.table(title = item.modname & " data symbols"))

    if item.source == "":
      fseq.add(cell("No stored source code.", "h2"))
    else:
      fseq.add(cell(text("Source code for ") + em(item.modname), "h2"))
      fseq.add(cell(item.source)) # item.source.pretty()


    if item.parameters.len() == 0:
      fseq.add(cell("No stored parameters.", "h2"))
    else:
      fseq.add(obj.format_module_params(item.parameters))

    fseq.add(cell(text("Disassembly for ") + em(item.modname), "h2"))
    fseq.add(item.rawReprInstructions(obj))

    return fseq.flow()

proc get_obj_func_info*(obj: ZObjectFile): Grid =
  # Note: Took out func type for right now.
  var cells: seq[seq[string]]

  if obj.funcInfo.len() == 0:
    result = cell("No native Con4m functions in binary", "h2")
  else:
    cells = @[]

    cells.add(@["Name", "Module Address", "Size",
                "Param names / offsets"])

    for item in obj.funcInfo:
      var
        paramInfo: seq[string]   = @[]
        ptypes:    seq[TypeSpec] = @[]

      for (offset, tid) in item.symTypes:
        paramInfo.add(item.syms[offset] & ": " & $(offset))
        ptypes.add(tid)

      cells.add(@[item.funcname,
                  item.offset.toHex.toLowerAscii(),
                  $(item.size),
                  paramInfo.join(", ")])

    result = cells.table(title = "Con4m native function info")

proc get_obj_ffi_info*(obj: ZObjectFile): Grid =
  var cells: seq[seq[string]]

  if obj.ffiInfo.len() == 0:
    return cell("No foreign functions mapped", "h2")

  else:
    cells = @[@["Foreign Name", "Local Name", "Varargs?", "dll names",
              "Param Info"]]
    for item in obj.ffiInfo:
      var
        xname = find_string_at(obj.staticData, item.nameOffset)
        lname = find_string_at(obj.staticData, item.localName)
        vargs = if item.va: "✓" else: "✗"
        dlls: seq[string]
        pinf: seq[string] = @[]

      if item.dlls.len() == 0:
        dlls = @["none required"]
      else:
        for ix in item.dlls:
          dlls.add(find_string_at(obj.staticData, ix))

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
          s &= cast[TypeSpec](ainfo.ourType).toString()
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

    return cells.table(title = "Foreign Function Mappings")

proc get_low_level_heap_info*(varmap: Dict[int, string], arena: seq[pointer],
                              title: string): Grid =
  var cells = @[
    @["Variable", "Offset", "Type", "Value", "Repr"]
  ]

  for (offset, name) in varmap.items(sort = true):
    var
      vraw = arena[offset * 2]
      traw = arena[offset * 2 + 1]
      tid  = cast[TypeSpec](traw)
      val  = cast[int](vraw).toHex().toLowerAscii()
      r    = "(No value)"

    if tid != nil and not tid.is_type_error():
      r = con4m_repr(vraw, tid).toNimStr()

    cells.add(@[name, $offset, tid.toString, val, r])

  return cells.table(title = title)

proc get_module_heap_info*(rt: RuntimeState, m: ZModuleInfo, title = ""): Grid =
  var
    title  = if title != "":
               title
             else:
               "Current state for module: " & m.modname
    arena  = rt.moduleAllocations[m.moduleId]
    varmap = m.datasyms

  return get_low_level_heap_info(varmap, arena, title)

proc get_entry_heap_info*(rt: RuntimeState, title = ""): Grid =
  return rt.get_module_heap_info(rt.obj.moduleContents[rt.obj.entrypoint - 1],
                                 title)

proc get_next_entry_heap_info*(rt: RuntimeState, title = ""): Grid =
  return rt.get_module_heap_info(rt.obj.moduleContents[rt.obj.nextEntrypoint - 1],
                                 title)

proc get_all_heap_info*(rt: RuntimeState): Grid =
  var fcells: seq[Grid]

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

      fcells.add(rt.obj.globals.get_low_level_heap_info(arena, title))

    else:
      fcells.add(rt.get_module_heap_info(rt.obj.moduleContents[i - 1]))

  return fcells.flow()

proc get_section_docs*(rt: RuntimeState): Grid =
  let view = rt.sectionDocs.items(sort = true)

  if view.len() == 0:
    return cell("No section documentation found.", "h2")

  var cells: seq[seq[Rich]] = @[@[text("Section"), text("Short Doc"),
                                text("Long Doc")]]
  for (k, v) in view:
    var l, s: Rich

    if v.shortdoc == nil:
      s = text("None provided.")
    else:
      s = v.shortdoc

    if v.longdoc == nil:
      l = text("None provided.")
    else:
      l = v.longdoc

    cells.add(@[em(k), s, l])

  return cells.table(title = "Section Documentation")

proc cmd_objdump*() =
  var
    rt  = config_args[0].load_object_file()
    obj = rt.obj
    secnames: seq[Rich]



  print obj.get_basic_object_info()
  print h2("Global static data")
  print c4str(obj.staticData.strDump())
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

    for item in rt.allSections.keys(sort=true):
      secnames.add(@[item])

    if secnames.len() == 0:
      print(h2("No sections defined."))
    else:
      print(h2("Active attribute sections"))
      let l = ul(secnames)
      print(l)

    print rt.get_section_docs()

    print(h2("Saved attribute information"))
    rt.print_attributes()
  else:
    print h2("Attributes are not being used.")


  print rt.get_all_heap_info()
