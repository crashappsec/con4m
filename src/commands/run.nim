import ".."/common
import "."/cmd_base

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

proc cmd_compile*(ctx: CompileCtx) =
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
      quit(1)
    print(h1("Tokens for module '" & entry.modname & "'"))
    entry.printTokens()

    if entry.root == nil:
      quit(1)

    print(h1("Parse tree for module '" & entry.modname & "'"))
    entry.printParseTree()

    if entry.ir == nil:
      quit(1)

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
    quit(1)

  var rt = ctx.generateCode()
  rt.run_object(entry.modname)
