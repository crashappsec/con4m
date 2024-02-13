# TODO:
#
# === High priority -- before Chalk integration ===
#
# Travel for Gerhard
# Scott

# - Apply component logic in runtime:
# a) Handle defaults, setting them on component entry if needed.
# b) Auto-lock attrs that *can* be set in a component, when the component
#   exits.
# - Checkpointing (and restoring) runtime state.
# - Fix up and test 'Other' data types
# - Documentation.
# - Add doc strings in for getopt, etc.
# - Adding defaults should NOT trigger a write lock.

# === Semi-high priority -- could ship internally w/ known issues ===
# - Add help and extra help topics back into getopt.
# - Rich improvements.
# - Fill in missing error messages.
# - Proper marshaling of types and callbacks.
# - Lit mods need to be available for runtime literal instantiation.
# - Capture location info for runtime attr def locations, and show
#   all def locations for things like spec errors (anything runtime).
# - Don't bother to issue a copy when we just instantiated a lit.
# - Remainder of needed stdlib stuff
# - Restrict the leading '$' properly.
# - Redo doc API / autogeneration
# - Typecheck c vs con4m api for ffi
# - Enums should be global by default.  Add a 'private' for enums,
#   funcs and, when they show up,
# - Some basic memory management in the runtime (dynamic alllocs are
#   currently just leaked).
# - Sort errors by file / line (they come out by phase in IR portion).
# - More Documentation.
# - When doing second pass for calls, add cast nodes where we could auto-cast.
# - Remove any remaining newRefVal / extractRef calls
# - Folding for lists should be generalized, instead of the one-off for
#   `choice` fields we have now.
# - Get control flow stuff working properly.

# == Medium -- before public release ==
# - attr.x for disambiguation of top-level attr vs var.
# - Comments in pretty(), and better spacing control based on original src.
# - Test for tuple unpacking; I think I probably haven't done it, and it may not
#   work at all.
# - Redo code gen for assignment to get rid of the extra ref/deref for index
#   ops
# - Redo the rich data type again; add a "container" type
# - Check attr to lock and attrs to access against spec statically.
# - Buffers should be mutable (like strings, they currently are not).
# - explicit casts -- to(obj, type) OR type(obj) (decide which)
# - In showCallMistakes(), show which functions have the wrong # of args,
# - Possibly allow generating a C API based on the spec.
#   and which parameters are right / wrong.
# - finish hasExitToOuterBlock in CFG.
# - Have `const` items move to module-specific static storage.
# - Allow assignment inside var / global / const statements.
# - Use No-side-effect prop for funcs to allow calling functions at compile
#   time (and mark native f() no-side-effect if they do not use external
#   state).
#   Lots more folding work should be done.
# - Fold container literals wherever possible.
# - Access controls around extern and extensibility features.
# - ~ operator possibly should be renamed to 'lock' and not require an
#   assignment, but if there's no assignment it should error / warn if
#   one might be locking something that isn't assigned.
# - Be able to lock an entire section.
# - Warning when your declared type is more generic than the inferred type.
# - Warning when (in non-REPL-land) module vars / global vars are generic.
# - Implement _ as a 'discard' variable.
# - C api and bindings to other languages.
# - dlclose stuff.
# - Redo the CFG for SSA (cleaning up the existing bug)
# - Add some sort of mixed type
# - // for integer division.
# - REPL
# - Error if any variables not within a function do not have a
#   concrete type (after REPL is done)
# - Give names to enums / turn them into int subtypes.
# - Fold for list indexes when the length is fixed size and the list is static.
# - Add 'error' to functions.
# - string enums
# - some sort of module scope operator; perhaps root::, module::,
#   local:: I think? Or, rethink the whole import system :)
# - Keyword arguments for functions.
# - Some sort of debugger?
# - Add maybe / null checking
# - Should add variable aliases for $i and $label
# - Implement FFI Varargs.
# - Add objects, with typevars that can bind to all fields...
# - Add oneof
# - Add ref
# - Other hatrack data types
# - Code playground
# - LSP server
# - Only generate loop variables if they're used.
# - Ideally, target LLVM

# == Lower priority ==
# - For errors, make it easy to see "previous instance", and remove
#   table for 2nd line
# - Error msg squelching and colating
# - GUI for repl; show trees, etc.
# - let all the IO stuff be themeable
# - for x in <container>: generate a call to items() if the object is
#   not one of the built-in types.
# - Index for non-base types should generate a rewrite func
# - Validation routines need routines to validate their inputs.
# - TODO: should there be an option to leave functions in the module scope?
#   If so, what's the syntax?
# - Macro-style compile time rewriting and an aspect system
# - Allow arbitrary blocks within statements?
# - Check for object field is never used (or set and not read)
# - Config limit for how many errors we print.
# - Use tracking for function calls.
# - Package system.
# - Varargs for external calls.

## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022 - 2024

import std/os
import "."/[compile, codegen, vm, specs, pretty, err, getopts, rtmarshal]

const flag_spec = """
use getopt from "modules"

getopts {

  default_command:   "run"
  command_attribute: "command"
  arg_attribute:     "args"

  flag_yn debug {
      "Debug"
      "Turn debugging on, which will show intermediate phase output."
      field_to_set: "debug"
  }

  flag_yn pretty {
      "Pretty print file"
      "Output a pretty-printed version to stdout before executing"
      field_to_set: "pretty"
  }

      flag_yn save {
          "Save object state at end of execution"
          field_to_set: "save_object"
      }


  command lex {
      "Perform lexical analysis"
      "Pass any files given at the command line through the lexer (only)."
      args: (1, 0xffffffff)
  }

  command parse {
      "Parse files"
      "Print parse trees for files given at the command line, outputing trees."
      args: (1, 0xffffffff)
  }

  command check {
      "Parse and validate"
      "Print checked IR tree"
      args: (1, 0xffffffff)
  }

  command compile {
      "Compile a single program entry point, without running it."
      args: (1, 1)
  }

  command run {
      "Compile and run."
      args: (1, 1)

  }
}
"""

proc parse_command_line(): RuntimeState =
  let
    ctx  = newCompileContext(nil)
    spec = ctx.loadInternalModule("c4m_getopt", flag_spec)

  ctx.buildProgram(spec)

  var
    rt     = ctx.generateCode()
    exit   = rt.executeObject()
    params = commandLineParams()

  try:
    let pre = rt.runManagedGetopt(params)
    discard rt.finalizeManagedGetopt(pre)

    return rt

  except:
    print(fgColor("error: ", "red") + italic(getCurrentException().msg))
    if "--debug" in params:
      echo getStackTrace()
    quit()

proc findAndLoadFromUrl(ctx: CompileCtx, url: string): Option[Module] {.importc,
                                                                       cdecl.}
proc cmd_not_full_program(ctx: CompileCtx, cmd: string, args: seq[string],
                          fmt: bool, debug: bool) =
  var
    modules: seq[Module]
    gotErrors = false

  for item in args:
    let modOpt = ctx.findAndLoadFromUrl(item)
    if modOpt.isNone():
      ctx.loadError("FileNotFound", item)

    let module = modOpt.get()
    if cmd == "check":
      ctx.buildIr(module)
    modules.add(module)

  for module in modules:
    print h1("Module: " & module.key & module.ext)

    if fmt:
      print pretty(module.root)

    if cmd == "lex" or debug:
      module.printTokens()
      if cmd == "lex":
        continue

    if cmd == "parse" or debug:
      module.printParseTree()
      if cmd == "parse":
        continue

    module.printIr()
    if debug:
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

proc cmd_compile(ctx: CompileCtx, cmd, entryname: string, fmt, debug, saveObj: bool) =
  let
    can_proceed = ctx.buildFromEntryPoint(entryname)
    entry       = ctx.entrypoint


  if not can_proceed:
    print(h4("Compilation failed."))

  if fmt and entry.root != nil:
    print(h2("Entrypoint source code"))
    print pretty(entry.root)

    if debug:
      print(h2("Raw source"))
      echo $(entry.s.runes)

  if debug:
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

  if debug:
    print h1("Type info stored in object file: ")
    rt.obj.printTypeCatalog()
    print h1("Disassembly")
    print rt.obj.disassembly()
    print h1("Executing...")

  let exitCode = rt.executeObject()

  if debug:
    print h1("Execution completed.")
  if saveobj:
    let
      objfile = rt.marshal_runtime(-3)
      out_fname = entry.modname & ".0c00l"

    let r2 = objfile.unmarshal_runtime()

    if debug:
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

  let validation_errors = validate_state()
  if len(validation_errors) != 0:
    print(h4("Post-execution validation failed!"))
    var bullets: seq[Rope]
    for item in validation_errors.items():
      bullets.add(cast[Rope](item))
    print(ul(bullets), file = stderr)

  if debug:
    print(h1("Any set attributes are below."))
    rt.print_attributes()

  if debug and rt.obj.spec.used:
    print(h4("Specification used for validation: "))
    rt.obj.spec.print_spec()


  quit(exitCode)

when isMainModule:
  useCrashTheme()
  setupSignalHandlers()

  let
    argParse = parse_command_line()
    cmd      = lookup[C4Str](argParse, "command").get().toNimStr()
    args     = lookup[Array](argParse, "args").get().strlist()
    debug    = lookup[bool](argParse, "debug").get(false)
    format   = lookup[bool](argParse, "pretty").get(false)
    saveObj  = lookup[bool](argParse, "save_object").get(false)
    altPath  = $(getEnv("CON4M_PATH"))

  if cmd == "help":
    quit(1)

  var
    session = newCompileContext(nil)

  if altPath != "":
    session.modulePath = altPath.split(Rune(':'))

  if cmd in ["lex", "parse", "check"]:
    session.cmd_not_full_program(cmd, args, format, debug)
  else:
    session.cmd_compile(cmd, args[0], format, debug, saveObj)
