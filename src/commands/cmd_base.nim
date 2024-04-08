## Basic commands; which is most of them. Objdump has a bit more
## going on, and gets its own file.
##
## A lot of the commands that don't produce object files are just
## variations on a theme, and share `cmd_stop_early`.

import ".."/[compile, codegen, vm, specs, err, rtmarshal]
export compile, codegen, vm, specs, err, rtmarshal

proc findAndLoadFromUrl*(ctx: CompileCtx, url: string): Option[Module]
    {.importc, cdecl.}

proc cmd_pretty*(ctx: CompileCtx) =

 print_err(rich"[h1]Pretty printing is currently disabled.")

 when false:
  var
    modules: seq[Module]
    show_header = false

  if config_args.len() > 1:
    show_header = true

  for item in config_args:
    if not item.endswith(".c4m"):
      print(fgColor("error: ", "red") + text("Not a con4m file: ") +
            em(item.resolvePath()) +
            text(" (The ") + em(".c4m") + text(" extension is required.)"))
      continue

    let modOpt = ctx.findAndLoadFromUrl(item)
    if modOpt.isNone():
      ctx.loadError("FileNotFound", item)
      continue

    let module = modOpt.get()
    if show_header:
      print h1("Module: " & module.key & module.ext)

    print pretty(module.root)

proc cmd_stop_early*(ctx: CompileCtx) =
  var
    modules: seq[Module]
    gotErrors = false

  for item in config_args:
    if not item.endswith(".c4m"):
      var err = r("[red]error:[/] Not a con4m file: [i]" & item.resolvePath() &
        "[/] (requires a [i].c4m[/] extension")
      print_err(err)
      continue

    let modOpt = ctx.findAndLoadFromUrl(item)
    if modOpt.isNone():
      ctx.loadError("FileNotFound", item)
      continue

    let module = modOpt.get()
    if config_cmd == "check":
      ctx.buildIr(module)
    modules.add(module)

  for module in modules:
    print r("[h1]Module: " & module.key & module.ext)

    if config_format:
      print_err(rich"[h2]Pretty printing is currently disabled.")
      #print pretty(module.root)

    if config_cmd == "lex" or config_debug:
      module.printTokens()
      if config_cmd == "lex":
        continue

    if config_cmd == "parse" or config_debug:
      module.printParseTree()
      if config_cmd == "parse":
        continue

    module.printIr()
    if config_debug:
      if module.usedAttrs != nil and
         module.usedAttrs.table.items().len() != 0:
        module.printAttrsUsed()
      if module.funcScope != nil and
         module.funcScope.table.items().len() != 0:
        ctx.printAllFuncScopes(module)
      if module.moduleScope != nil and
         module.moduleScope.table.items().len() != 0:
        module.printModuleScope()

  if ctx.errors.len() != 0:
    gotErrors = true
  else:
    for (_, m) in ctx.modules.items():
      if m.errors.len() != 0:
        gotErrors = true
        break

  if gotErrors:
    print(h1("All Errors"))
    discard ctx.printErrors(file = stdout)
  quit()

proc load_object_file*(fname: string): RuntimeState {.exportc, cdecl.} =
  try:
    var rawobj = c4Str(readFile(fname.resolvePath()))

    if config_debug:
      print c4str(hex_dump(rawobj, uint(rawobj.len()), 0))

    result = string_instream(rawobj).unmarshal_runtime()

  except:
    print r("[red]error:[/] Could not open [b]" & fname & ":[/] [i]" &
          getCurrentException().msg)
    if config_debug:
      echo getCurrentException().getStackTrace()
    quit()

proc cmd_disassemble*() =
  print config_args[0].load_object_file().obj.disassembly(not config_debug)
  quit()
