# TODO:
#
# === High priority ===
# - No matching sig error for function calls
# - Offset isn't right for sym copies in a typecase. Check lineage.
# - Basic execution.
# - Load default values at beginning of program.
# - Dictionary and tuple implementations
# - C-level interface to attributes
# - Stack traces during execution.
# - Spec checking after execution.
# - Checkpointing runtime state.
# - Module state caching for re-linking when conf files change.
# - Swap in hatrack lists (and add rings?).
# - Use No-side-effect prop for funcs to allow calling functions at compile time.
# Builtin funcs are a bit wrong about data storage etc.
# - Print statement?
#
# - Keep spec around in object file.
# - Allow assignment inside var / global / const statements.
# - Component logic in runtime.
# - ConvertCallbackLit type secolution.
# - In showCallMistakes(), show which functions have the wrong # of args,
#   and which parameters are right / wrong.
# - Base types should be treated more like proper classes?
# - Re-implement standard library / wrappings.
# - finish hasExitToOuterBlock in CFG.
# - For errors, make it easy to see "previous instance", and remove
#   table for 2nd line
# - Auto-generation of type-checking C interface API?
# - Copy operations for all ref builtin types.
# - Handle negative indexes in call_...index
# - explicit casts
# - Callback objects
# - Warning when your declared type is more generic than the inferred type.

# == Medium ==
# - Merge var/attr assign nodes.
# - Access controls around extern and extensibility features
# - ~ operator should be renamed to 'lock' and not require an assignment,
#   but if there's no assignment it should error / warn if one might be
#   locking something that isn't assigned.
# - Built in print statement??
# - Doc strings
# - Implement _ as a 'discard' variable.
# - Rename to 0cool
# - C api and bindings to other languages.
# - Code generation.
# - Doc API.
# - dlclose stuff.
# - Redo the CFG for SSA (cleaning up the existing bug)
# - Add some sort of mixed type
# - Hook getopt back up.
# - Update the pretty printer.
# - Extra lines in error messages shouldn't get the huge table indent.
# - Sort errors by file / line (they come out by phase in IR portion).
# - REPL
# - Error if any variables not within a function do not have a concrete type (after REPL is done)
# - Default parameters
# - Litmods for common rope types
# - Add $len, $last
# - Give names to enums / turn them into int subtypes.
# - Fold for list indexes when the length is fixed size.
# - Add 'error' to functions.
# - Add global enum
# - string enums
# - :: module scope operator; root::, module::, local:: I think?
# - Keyword arguments
# - Move temporary compile state to a throw-away reference obj that's
#   carried in the module state.
# - Debug mode.
# - Enumerate function pointer literals and assume they're always live
#   and called as part of the entry point.
# - Properly handle
# - += and similar.
# - What's wrong w/ hatrack add??
# - Add maybe / null checking
# - Should add variable aliases for $i and $label

# == Lower priority ==
# - Error msg squelching and colating
# - GUI for repl; show trees, etc.
# - let all the IO stuff be themeable
# - Add objects, with typevars that can bind to all fields...
# - Add oneof
# - Add ref
# - for x in <container>: generate a call to items() if the object is
#   not one of the built-in types.
# - Todo-- index for non-base types should generate a rewrite func
# - Validation routines need routines to validate their inputs.
# - For dicts and lists: capture the value when possible and perform folding
# - Support litmods for containers.
# - TODO: should there be an option to leave functions in the module scope?
#   If so, what's the syntax?
# - Macros / aspects
# - Allow arbitrary blocks within statements?
# - Object field is never used (or set and not read)
# - Config limit for how many errors we print.
# - Use tracking for function calls.

## Makes it easy to build Apache-style configuration files with
## well-defined schemas, where you don't have to do significant work.
##
## And the people who write configuration files, can do extensive
## customization using the con4m language, which is built in a way
## that guarantees termination (e.g., no while loops, for loop index
## variables are immutible to the programmer).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import module, specs, codegen
export module

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
    var module = session.entrypoint

    module.printTokens()
    module.printParseTree()
    module.printIr()
    module.printAttrsUsed()
    module.printAllFuncScopes()
    module.printModuleScope()
    session.printGlobalScope()
    session.printProgramCfg()

  if session.printErrors():
    let generatedCode = session.generateCode()
    print generatedCode.disassembly()

  else:
    print h2("Program loading failed.")
