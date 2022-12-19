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

import con4m/parse
export fatal, parse.parse, Con4mError

## This operates on tokens, as already produced by lex().  It simply
## kicks off the parser by entering the top-level production (body),
## and prints out any error message that happened during parsing.

import con4m/treecheck
export checkTree

# Runtime bits
import con4m/types
export types

import con4m/eval
export evalTree, evalConfig

import con4m/box
export box

import con4m/builtins
export builtinIToS, builtinBToS, builtinFToS, builtInItoB, builtinFtoB,
       builtinStoB, builtinLToB, builtinDToB, builtinIToF, builtinFtoI,
       builtinSplit, builtinEcho, builtinEnv, builtinEnvExists,
       builtinEnvAll, builtinStrip, builtinContainsStrStr,
       builtinFindFromStart, builtinSlice, builtinSliceToEnd, builtInAbort,
       builtinCmd, newBuiltIn, addDefaultBuiltins


# Post-runtime (spec checking)
import con4m/spec
export newConfigSpec, addGlobalAttr, addSection, addAttr, validateConfig,
       newConfigState, getConfigVar, getAllSectionSTs, addSpec, lockConfigVar,
       setOverride

# Simplification via complexity
import con4m/codegen
export codegen.con4m, configDef

when defined(testCases):
  # There are really just exposed for our tests.  Should change that.
    import con4m/st
    export lookupAttr
    import con4m/dollars
    export dollars

when isMainModule:
  import os, parseopt, options, streams, con4m/dollars, json

  proc showResults(fstream: Stream,
                   scope:
                   Con4mScope,
                   s: string,
                   ascii: bool,
                   header: bool
                  ) =
    if header:
      fstream.write("********** Execution of " & s & " finished ************\n")
    if ascii:
      fstream.write($(scope))
    else:
      echo parseJson(scope.scopeTojson()).pretty()

  proc showHelp() {.noreturn.} =
    echo """
con4m [-ahv] [--help] [--verbose] [--ascii] [--o:outfile] first.config ...
      Evaluates your configuration file, dumping a JSON string with the results.

      Writes to standard output.  If multiple config files are provided, 
      they are executed 'stacked', in command-line order.
      The -a / --ascii flag outputs in text instead of JSON.

      Note that this interface currently does not support callbacks,
      and loads the default functions only. 
"""

    quit()

  let argv = commandLineParams()
  var
    outfilename: Option[string] = none(string)
    conffiles: seq[string] = @[]
    verbose: bool = false
    ascii: bool = false
    outstream: Stream


  for kind, k, v in getopt(argv,
                           {'h', 'v', 'a'},
                           @["help", "verbose", "ascii"]):
    case kind
    of cmdArgument:
      conffiles.add(k)
    of cmdLongOption, cmdShortOption:
      case k
      of "help", "h":
        showHelp()
      of "verbose", "v":
        verbose = true
      of "outfile", "o":
        if outfilename.isSome():
          echo "Error: multiple output files specified"
          showHelp()
        outfilename = some(v)
      of "ascii", "a":
        ascii = true
      else:
        echo "Unrecognized option: --" & v
    else:
      unreachable

  if len(conffiles) == 0:
    showHelp()

  if outfilename.isNone():
    outstream = newFileStream(stdout)
  else:
    outstream = newFileStream(outfilename.get(), fmWrite)
    if outstream.isNil():
      echo "con4m: error: Could not open output file: " & outfilename.get()
      quit(1)

  # Load the first config file, and get a context object.
  var
    state: ConfigState
    scope: Con4mScope
    opt = evalConfig(conffiles[0])

  if opt.isNone():
    echo "Failed to load: " & conffiles[0]
    quit()

  (state, scope) = opt.get()

  if verbose or len(conffiles) == 1:
    outstream.showResults(scope, conffiles[0], ascii, verbose)

  # Now, load the rest of the conf files, using the same scope.
  for filename in conffiles[1 .. ^1]:
    if verbose:
      echo "Loading config file: " & filename
    let scopeOpt = state.stackConfig(filename)
    scope = scopeOpt.get()

    if verbose or filename == conffiles[^1]:
      outstream.showResults(scope, filename, ascii, verbose)
