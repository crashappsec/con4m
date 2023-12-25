# TODO:
#
# === High priority ===
# - Second pass on function cfarg resolution before erroring:
#   When we're done w/ a module we need to look for overlapping function
#   definitions within the module.
# - Code generation to some basic form and execution from it.
# - Implement _ as a 'discard' variable.
# - C-level interface to attributes
# - Stack traces during execution.
# - Spec checking after execution.
# - Checkpointing runtime state.
# - Module state caching for re-linking when conf files change.
# - FFI
# - Swap in hatrack lists (and add rings?).
# - No-side-effect prop for funcs to allow calling functions at compile time.
# - Allow assignment inside var / global / const statements.
# - Component logic
# - ConvertCallbackLit type secolution.
# - In showCallMistakes(), show which functions have the wrong # of args,
#   and which parameters are right / wrong.
# - Base types should be treated more like proper classes?
# - Case statement / typecase
# - Re-implement standard library / wrappings.
# - finish hasExitToOuterBlock in CFG.
# - Merge views of global scope.
# - Issue with validators
# - For errors, make it easy to see "previous instance", and remove
#   table for 2nd line
# - Don't allow 'var' declarations or 'global' declarations if the
#   name is allowed in the top-level of attribute specs.

# == Medium ==
# - C api and bindings to other languages.
# - Doc API.
# - Merge mixed / cbox
# - Hook getopt back up.
# - Update the pretty printer.
# - Extra lines in error messages shouldn't get the huge table indent.
# - Sort errors by file / line (they come out by phase in IR portion).
# - REPL
# - Default parameters
# - Litmods for common rope types
# - Add $len, $last
# - Give names to enums / turn them into int subtypes.
# - Fold for list indexes when the length is fixed size.
# - Add 'error' to functions.
# - Add global enum
# - :: module scope operator; root::, module::, local:: I think?
# - Keyword arguments
# - Move temporary compile state to a throw-away reference obj that's
#   carried in the module state.
# - Debug mode.
# - Enumerate function pointer literals and assume they're always live
#   and called as part of the entry point.
# - Properly handle
# - += and similar.
# - Some sort of mixed type in the language itself.
# - What's wrong w/ hatrack add??

# == Lower priority ==
# - Error msg squelching and colating
# - GUI for repl; show trees, etc.
# - let all the IO stuff be themeable
# - Add objects, with typevars that can bind to all fields...
# - Add maybe
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

import module, specs
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
    q.addField("z", "int", default = some(toBox[int](12, TInt)))

  let
    params = commandLineParams()
    spec   = buildTestSpec()
    session = newCompileContext(spec)

  if len(params) != 1:
    print h2("Cannot con4m")
    print fgColor("error: ", "red") + text("For now, provide only one arg.")
    quit()

  discard session.buildFromEntryPoint(params[0])
  if true:
    var module = session.entrypoint

    module.printTokens()
    module.printParseTree()
    module.printIr()
    module.printAttrsUsed()
    module.printAllFuncScopes()
    module.printModuleScope()
    session.printGlobalScope()
    session.printProgramCfg()
    session.printErrors()
  else:
    print em("Could not find module 'ptest.c4m'")
