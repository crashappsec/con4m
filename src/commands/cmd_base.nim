## Basic commands; which is most of them. Objdump has a bit more
## going on, and gets its own file.
##
## A lot of the commands that don't produce object files are just
## variations on a theme, and share `cmd_stop_early`.

import ".."/[compile, codegen, vm, specs, pretty, err, rtmarshal]
export compile, codegen, vm, specs, pretty, err, rtmarshal

proc findAndLoadFromUrl*(ctx: CompileCtx, url: string): Option[Module]
    {.importc, cdecl.}

proc cmd_pretty*(ctx: CompileCtx) =
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
      print(fgColor("error: ", "red") + text("Not a con4m file: ") +
            em(item.resolvePath()) +
            text(" (requires a ") + em(".c4m") + text(" extension"))
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
    print h1("Module: " & module.key & module.ext)

    if config_format:
      print pretty(module.root)

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
    var rawobj = fname.newC4StrFromFile()


    if config_debug:
      print hex_dump(rawobj, uint(rawobj.len()), 0)

    result = rawobj.unmarshal_runtime()

  except:
    print fgColor("error: ", "red") + text("Could not open ") + em(fname) +
          text(": ") + italic(getCurrentException().msg)
    if config_debug:
      echo getCurrentException().getStackTrace()
    quit()

proc cmd_disassemble*() =
  print config_args[0].load_object_file().obj.disassembly(not config_debug)
  quit()
