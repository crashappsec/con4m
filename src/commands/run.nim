import ".."/common
import "."/[cmd_base, objdump]

# Modfile is a param instead of checking the config variable, so that library
# invokations of this don't need to use our config runtime.
proc save_object_to_disk*(rt: var RuntimeState, modfile: string) {.exportc, cdecl.} =
  var
    objfile = rt.marshal_runtime(-3)
    out_fname = modfile
  if not out_fname.endswith(obj_file_extension):
    out_fname &= obj_file_extension

  if config_debug:
    print hex_dump(objfile, uint(objfile.len()), 0)

  try:
    var
      f       = open(outfname, fmWrite)
      l       = objfile.len()
      cur     = 0
      toWrite = l

    if f == nil:
      print fgColor("error: ", "red") + text("Could not write to ") +
      em(outfname)
    else:
      while toWrite > 0:
        let n = f.writeBytes(cast[ptr UncheckedArray[uint8]](objfile).
                                                toOpenArray(0, l - 1),
                             cur,
                             toWrite)
        cur     += n
        toWrite -= n

  except:
    print fgColor("error: ", "red") + text("Could not write to ") +
          em(outfname) + text(": ") + italic(getCurrentException().msg)
    if config_debug:
      echo getCurrentException().getStackTrace()

proc run_object*(rt: var RuntimeState, modfile: string, resumed = false) =
  var module_ix = -1 # -1 for default.

  if config_reentry_point != "":
    if config_reentry_point.endswith(".c4m"):
      config_reentry_point = config_reentry_point[0 ..< ^4]

    var allnames: seq[Rope]

    for i, m in rt.obj.moduleContents:
      if m.modname == config_reentry_point or
         m.location.join_path(m.modname) == config_reentry_point:
        module_ix = i
        break

      allnames.add(li(m.modname))

    if module_ix == -1:
      print fgColor("error: ", "red") + text("Desired entry point module ") +
      em(config_reentry_point) + text(" is not one of the available modules ") +
      text("in the object file. Found modules are:") + ul(allnames)
      quit(-143)

    else:
      # moduleids start at 1, not 0
      rt.obj.nextentrypoint = int32(module_ix + 1)

  if config_debug:
    print h1("Type info stored in object file: ")
    rt.obj.print_type_catalog()
    print h1("Disassembly")
    print rt.obj.disassembly()

  if config_debug or config_format:
    print h1("Executing...")

  let exitCode = if resumed: rt.resume_object() else: rt.executeObject()

  if config_debug or config_format:
    print h1("Execution completed.")
  if config_save_object:
    rt.save_object_to_disk(modfile)

  let validation_errors = validate_state()
  if len(validation_errors) != 0:
    print(h4("Post-execution validation failed!"))
    var bullets: seq[Rope]
    for item in validation_errors.items():
      bullets.add(cast[Rope](item))
    print(ul(bullets), file = stderr)

  if config_debug:
    print(h1("Any set attributes are below."))
    rt.print_attributes()

  if config_debug:
    rt.obj.spec.print_spec()

  quit(exitCode)

proc cmd_resume*() =
  var
    fname = config_args[0]
    rt    = fname.load_object_file()

  if config_format:
    print(h2("Original entry point source code"))
    let m = rt.obj.moduleContents[rt.obj.entryPoint - 1]
    print(pretty(m.source))

  rt.run_object(fname, resumed = true)

proc base_compile*(ctx: CompileCtx): RuntimeState =
  let
    entryname   = config_args[0]
    can_proceed = ctx.buildFromEntryPoint(entryname)
    entry       = ctx.entrypoint

  if not can_proceed:
    print(h4("Compilation failed."))

  if config_format and entry.root != nil:
    print(h2("Entrypoint source code"))

    print pretty(entry.root)

    if config_debug:
      print(h2("Raw source"))
      echo $(entry.s.runes)

  if config_debug:
    if entry.tokens.len() == 0:
      return nil

    print(h1("Tokens for module '" & entry.modname & "'"))
    entry.printTokens()

    if entry.root == nil:
      return nil

    print(h1("Parse tree for module '" & entry.modname & "'"))
    entry.printParseTree()

    if entry.ir == nil:
      return nil

    print(h1("IR for module '" & entry.modname & "'"))
    entry.printIr()

    if entry.usedAttrs.table.items().len() != 0:
      entry.printAttrsUsed()
      entry.printModuleScope()

    if entry.usedAttrs != nil and
       entry.usedAttrs.table.items().len() != 0:
      print(h1("Attributes used"))
      entry.printAttrsUsed()

    if entry.funcScope != nil and
       entry.funcScope.table.items().len() != 0:
      print(h1("Function scopes + IRs available from entry point"))
      ctx.printAllFuncScopes(entry)

    if entry.moduleScope != nil and
       entry.moduleScope.table.items().len() != 0:
      print(h1("Module scope"))
      entry.printModuleScope()

    if ctx.globalScope != nil and ctx.globalScope.table.items().len() != 0:
      print(h1("Global Scope"))
      ctx.printGlobalScope()

    if entry.cfg != nil:
      print(h1("Global CFG"))
      ctx.printProgramCfg()

  if not ctx.printErrors():
    return nil

  return ctx.generateCode()

proc cmd_compile*(ctx: CompileCtx) =
  var rt = ctx.base_compile()

  if rt == nil or rt.obj == nil:
    quit(-1)

  if config_reentry_point == "":
    config_reentry_point = ctx.entrypoint.modname

  rt.setup_first_execution()

  if config_debug:
    print rt.obj.disassembly()

  rt.save_object_to_disk(config_reentry_point)

proc cmd_run*(ctx: CompileCtx) =
  var
    rt = ctx.base_compile()

  if rt == nil or rt.obj == nil:
    quit(-1)

  var
    entryname = rt.obj.moduleContents[rt.obj.entrypoint - 1].modname

  if config_reentry_point == "":
    config_reentry_point = entryname

  if config_debug:
    print rt.obj.disassembly()


  let exitcode = rt.execute_object()

  if rt.obj.spec.used:
    let errors = rt.run_validator().items()
    var items: seq[Rope]

    if errors.len() != 0:
      for item in errors:
        items.add(li((cast[Rope](item))))

      print(h1("Execution validation failed:") + ol(items), file = stderr)
      quit(-1)

  if config_save_object:
    rt.save_object_to_disk(config_reentry_point)

  if config_debug:
    print em("Execution complete.")
    rt.obj.spec.print_spec()
    print h1("Ending attributes:")
    rt.print_attributes()

    print rt.get_entry_heap_info()

    if config_reentry_point != entryname:
      print rt.get_next_entry_heap_info()


  quit(exitcode)
