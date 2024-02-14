import ".."/common
import "."/cmd_base


proc cmd_objdump*() =
  var
    rt  = config_args[0].load_object_file()
    obj = rt.obj
    cells: seq[seq[string]]

  cells.add(@["Magic Value", obj.zeroMagic.toHex().toLowerAscii()])
  cells.add(@["Object Version", $(obj.zcObjectVers)])
  cells.add(@["Global scope alloc'd size", $(obj.globalScopeSz)])
  cells.add(@["Entrypoint", obj.moduleContents[obj.entryPoint - 1].modname])

  if obj.nextEntryPoint < 1:
    cells.add(@["Next Entrypoint", "Unspecified"])
  else:
    cells.add(@["Next Entrypoint",
                obj.moduleContents[obj.nextEntryPoint - 1].modname])

  print(cells.quickTable(title = "Basic object info", verticalHeaders = true,
                         caption = config_args[0].resolvePath()))

  print(h2("Global static data"))
  print pre(obj.staticData.strDump())

  if obj.symTypes.len() == 0:
    print(h2("No global symbol info"))
  else:
    cells = @[@["Variable name", "Static offset", "Type"]]
    for (offset, tid) in obj.symTypes:
      cells.add(@[obj.globals[offset], $(offset), tid.toString()])

    print cells.quickTable(title = "Global symbol info")

  for item in obj.moduleContents:
    print(h1(text("Module: ") + em(item.modname)))

    cells = @[@["Module name", item.modname],
              @["Location",    item.location],
              @["Version",     item.version],
              @["ID",          $(item.moduleId)],
              @["Var storage", $(item.moduleVarSize)],
              @["Code size",   $(item.initSize)]]

    print cells.quickTable(verticalHeaders = true)

    if item.symTypes.len() == 0:
      print(h2("No module symbols"))
    else:
      cells = @[@["Symbol name", "Static offset", "Type"]]
      let view1 = item.codesyms.items(sort = true)

      if view1.len() == 0:
        print h2("No symbol definitions")
      else:
        for (offset, name) in view1:
          for (o2, tid) in item.symTypes:
            if offset == o2:
              cells.add(@[name, offset.toHex().toLowerAscii(), tid.toString()])
              break
        print cells.quickTable(title = item.modname & " Symbol info")

      let view2 = item.datasyms.items(sort = true)
      if view2.len() == 0:
        print(h2("Unused; bit; minor refactoring needed"))
      else:
        for (offset, name) in view2:
          for (o2, tid) in item.symTypes:
            if offset == o2:
              cells.add(@[name, $(offset), tid.toString()])
              break
        print cells.quickTable(title = item.modname & " data symbols")

    if item.source == "":
      print h2("No stored source code.")
    else:
      print h2(text("Source code for ") + em(item.modname))
      print item.source.pretty()

    print h2(text("Disassembly for ") + em(item.modname))
    print item.rawReprInstructions(obj)

  if rt.obj.funcInfo.len() == 0:
    print h2("No native Con4m functions in binary")
  else:
    cells = @[]

    cells.add(@["Name", "Module Address", "Size", "Type",
                "Param names / offsets"])

    for item in rt.obj.funcInfo:
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

    print cells.quickTable(title = "Con4m native function info")

  if rt.obj.ffiInfo.len() == 0:
    print h2("No foreign functions mapped")

  else:
    cells = @[@["Foreign Name", "Local Name", "Varargs?", "dll names",
              "Param Info"]]
    for item in rt.obj.ffiInfo:
      var
        xname = findStringAt(rt.obj.staticData, item.nameOffset)
        lname = findStringAt(rt.obj.staticData, item.localName)
        vargs = if item.va: "✓" else: "✗"
        dlls: seq[string]
        pinf: seq[string] = @[]

      if item.dlls.len() == 0:
        dlls = @["none required"]
      else:
        for ix in item.dlls:
          dlls.add(findStringAt(rt.obj.staticData, ix))

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

    print cells.quickTable(title = "Foreign Function Mappings")

  if obj.spec.used:
    print(h2("Attribute Specification"))
    obj.spec.print_spec()
  else:
    print(h2("No attribute specification"))

  print(h2("Type Catalog"))
  obj.print_type_catalog()

  var secnames: seq[Rope]
  for item in rt.allSections.keys(sort=true):
    secnames.add(@[li(item)])

  if secnames.len() == 0:
    print(h2("No sections defined."))
  else:
    print(h2("Active attribute sections"))
    print(ul(secnames))

  print(h2("Saved attribute information"))
  rt.print_attributes()

  for i, item in rt.module_allocations:
    var
      title:     string
      varmap:    Dict[int, string]
      m:         ZModuleInfo
      arena = rt.moduleAllocations[i]

    if rt.moduleAllocations[i].len() == 0:
      continue

    if i == 0:
      varmap  = rt.obj.globals
      title   = "Global variable state"
    else:
      m       = obj.moduleContents[i - 1]
      title   = "Current state for module: " & m.modname
      varmap  = m.datasyms

    cells   = @[@["Variable", "Offset", "Type", "Value", "Repr"]]

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

    print cells.quickTable(title = title)
