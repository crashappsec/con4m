# TODO:
#
# === High priority -- before Chalk integration ===
#
# - Test suite.
# - Load default values at beginning of program.
# - Dictionary and tuple implementations
# - C-level interface to attributes
# - Spec checking after execution.
# - Saving the spec in the object file.
# - Checkpointing runtime state.
# - Swap in hatrack lists (and add rings?).
# - Print statement?
# - Component logic in runtime.
# - Get callbacks working (eg ConvertCallbackLit type secolution)
# - In showCallMistakes(), show which functions have the wrong # of args,
#   and which parameters are right / wrong.
# - Re-implement standard library / wrappings.
# - finish hasExitToOuterBlock in CFG.
# - Copy operations for all ref builtin types.
# - Handle negative indexes in call_...index
# - explicit casts
# - Hook up getopts again.
# - Update the pretty printer.
# - Possibly allow generating a C API based on the spec.
# - Doc API.
# - Default parameters
# - Add global enum
# - Enumerate function pointer literals and assume they're always live
#   and called as part of the entry point.
# - Add code gen for 'lock' operator.
# - The attr type info needs to be folded into the same dict as attrs.

# == Medium -- before public release ==
# - +=, -=, *=, etc.
# - Share dup'd strings when loading static data.
# - Have `const` items move to module-specific static storage.
# - Allow assignment inside var / global / const statements.
# - Use No-side-effect prop for funcs to allow calling functions at compile
#   time (and mark native f() no-side-effect if they do not use external state)
# - Merge var/attr assign nodes.
# - Fold container literals wherever possible.
# - Access controls around extern and extensibility features.
# - ~ operator should be renamed to 'lock' and not require an assignment,
#   but if there's no assignment it should error / warn if one might be
#   locking something that isn't assigned.
# - Be able to lock an entire section.
# - Warning when your declared type is more generic than the inferred type.
# - Warning when (in non-REPL-land) module vars / global vars are generic.
# - Doc strings
# - Implement _ as a 'discard' variable.
# - Rename to 0cool
# - C api and bindings to other languages.
# - dlclose stuff.
# - Redo the CFG for SSA (cleaning up the existing bug)
# - Add some sort of mixed type
# - Extra lines in error messages shouldn't get the huge table indent.
# - Sort errors by file / line (they come out by phase in IR portion).
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
# - Code playground

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

import compile, specs, codegen, vm, os
export compile

when isMainModule:
  useCrashTheme()

  proc buildTestSpec(): ValidationSpec =
    result   = newSpec()
    let
      root = result.getRootSection()
      q    = result.newSingleton("q")
      r    = result.newInstanceSection("r")
    root.addField("x", "int")
    result.allow(root, "q", "r")
    q.addField("z", "int", default = some(cast[pointer](12)))

  var
    params = commandLineParams()
    debug  = false
    spec   = buildTestSpec()
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
