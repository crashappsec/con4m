# TODO:
#
# === High priority -- before Chalk integration ===
#
# - Move the late error stuff around so we can get stack traces and line
#   numbers.
# - Finish validation.
# - Apply component logic in runtime.
# - Re-implement standard library / wrappings.
# - Checkpointing (and restoring) runtime state.
# - Hook up getopts again.
# - Fix up and test 'Other' data types
# - Enumerate function pointer literals and assume they're always live
#   and called as part of the entry point.
# - Documentation.

# === Semi-high priority -- could ship internally w/ known issues ===
# - Signal handler should print out runtime stack trace.
# - Capture location info for runtime attr def locations, and show
#   all def locations for things like spec errors.
# - Restrict the leading '$' properly.
# - attr.x for disambiguation of top-level attr vs var.
# - Doc API.
# - Typecheck c vs con4m api for ffi
# - Enums should be global by default.  Add a 'private' for enums,
#   funcs and, when they show up,
# - Some basic memory management in the runtime (dynamic alllocs are
#   currently just leaked).
# - Sort errors by file / line (they come out by phase in IR portion).
# - Documentation.
# - When doing second pass for calls, add cast nodes where we could auto-cast.
# - Remove any remaining newRefVal / extractRef calls
# - Folding for lists should be generalized, instead of the one-off for
#   `choice` fields we have now.
# - Get control flow stuff working properly.

# == Medium -- before public release ==

# - Implement a way to call function pointers; quite likely with good ol' ();
#   This really won't be too hard; the checker currently forbids it, and
#   there'd need to be some changes to code gen and the vm too, but it is all
#   minor; it's more about what syntax works for beginners, fp() or
#   maybe something like call(fp, ...)
# - Test for tuple unpacking; I think I probably haven't done it.
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

import compile, codegen, vm, os, pretty
export compile

when isMainModule:
  useCrashTheme()

  var
    params = commandLineParams()
    debug  = false
    format = false
    session = newCompileContext(nil)
    newParams: seq[string]

  let altPath = $(getEnv("CON4M_PATH"))

  if altPath != "":
    session.modulePath = altPath.split(Rune(':'))

  for item in params:
    case item
    of "--pretty":
      format = true
    of "--debug":
      debug = true
    else:
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

  if format:
    print pretty(session.entrypoint.root)

    if debug:
      print(h2("Raw source"))
      echo $(session.entrypoint.s.runes)

  if session.printErrors() and not format:
    var generatedCode = session.generateCode()
    if debug:
      print h1("Disassembly")
      print generatedCode.disassembly()
      print h1("Entrypoint Source")
      print pretty(session.entrypoint.root)
      print h1("Executing...")

    let exitCode = generatedCode.executeObject()

    if debug:
      print h1("Execution completed.")

    quit(exitCode)
  else:
    print h2("Program loading failed.")
    quit(1)
