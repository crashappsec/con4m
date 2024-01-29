# TODO:
#
# === High priority -- before Chalk integration ===
#
# - Move the late error stuff around so we can get stack traces.
# - Test loading of default values at beginning of program and defaults.
# - con4m-accessible API for setting specs.
# - Apply component logic in runtime.
# - Re-implement standard library / wrappings.
# - Checkpointing runtime state.
# - Hook up getopts again.
# - Fix up and test 'Other' data types
# - Typecheck c vs con4m api for ffi
# - Enumerate function pointer literals and assume they're always live
#   and called as part of the entry point.
# - Doc strings
# - Documentation.
# - attr.x for disambiguation of top-level attr vs var.

# === Semi-high priority -- could ship internally w/ known issues ===
# - Capture location info for runtime attr def locations, and show
#   all def locations for things like spec errors.
# - Update the pretty printer.
# - Restrict the leading '$' properly.
# - Get callbacks working (eg ConvertCallbackLit type secolution)
# - Doc API.
# - Enums should be global by default.  Add a 'private' for enums,
#   funcs and, when they show up,
# - Some basic memory management in the runtime (dynamic alllocs are
#   currently just leaked).
# - Sort errors by file / line (they come out by phase in IR portion).
# - Documentation.
# - When doing second pass for calls, add cast nodes where we could auto-cast.
# - Remove any remaining newRefVal / extractRef calls
# - Be more forgiving in configspec properties; right now, most only
#   allow a direct value or a single var, not any expression that folds.
# - Folding for lists should be generalized, instead of the one-off for
#   `choice` fields we have now.

# == Medium -- before public release ==
# - Redo code gen for assignment to get rid of the extra ref/deref for index ops
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
#   time (and mark native f() no-side-effect if they do not use external state).
#   Lots more folding work should be done.
# - Fold container literals wherever possible.
# - Access controls around extern and extensibility features.
# - ~ operator should be renamed to 'lock' and not require an assignment,
#   but if there's no assignment it should error / warn if one might be
#   locking something that isn't assigned.
# - Be able to lock an entire section.
# - Issue w/ non-consistent views in hatrack?
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
#   local:: I think?
# - Keyword arguments
# - Some sort of debugger?
# - What's wrong w/ hatrack add??
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

import compile, codegen, vm, os
export compile

when isMainModule:
  useCrashTheme()

  var
    params = commandLineParams()
    debug  = false
    session = newCompileContext(nil)

  let altPath = $(getEnv("CON4M_PATH"))

  if altPath != "":
    session.modulePath = altPath.split(Rune(':'))

  if "--debug" in params:
    debug = true
    var newParams: seq[string]
    for item in params:
      if item != "--debug":
        newParams.add(item)
    params = newParams

  if params.len() != 1:
    print h2("Cannot con4m")
    print fgColor("error: ", "red") + text("For now, provide only one arg.")
    quit()

  discard session.buildFromEntryPoint(params[0])
  if debug:
    let allmods = session.modules.values(sort = true)

    for m in allmods:
      print(h1("Tokens for module '" & m.modname & "'"))
      m.printTokens()

    for m in allmods:
      print(h1("Parse tree for module '" & m.modname & "'"))
      m.printParseTree()

    for m in allmods:
      print(h1("IR for module '" & m.modname & "'"))
      m.printIr()

    for m in allmods:
      if m.usedAttrs.table.items().len() != 0:
        m.printAttrsUsed()

    for m in allmods:
      m.printModuleScope()

    print(h1("Function scopes + IRs available from entry point"))
    session.printAllFuncScopes(session.entrypoint)

    print(h1("Global Scope"))
    session.printGlobalScope()

    print(h1("Global CFG"))
    session.printProgramCfg()

  if session.printErrors():
    var generatedCode = session.generateCode()
    if debug:
      print generatedCode.disassembly()

    quit(generatedCode.executeObject())
  else:
    print h2("Program loading failed.")
    quit(1)
