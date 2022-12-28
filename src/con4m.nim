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

import nimutils

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

import con4m/builtins
export builtins


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
  import os, parseopt, options, streams, strutils, strformat, json
  import con4m/dollars

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

  proc printCompilerError(fname: string, msg: string, debug: bool) =
    let
      parts = msg.split(":")
      me = getAppFileName().splitPath().tail
      f = msg.find("(thrown")

    var modmsg: string

    if len(parts) > 2 and f == -1:
      modmsg = msg
    else:
      let f2 = msg.find("): ") + 4

      modmsg = if (len(parts) > 2) and (f == -1): msg
               else: msg[0 ..< f].strip() & " " & msg[f2 .. ^1]

    stderr.writeLine(fmt"{me}:{fname}:{modmsg}")

    if len(parts) > 2:
      let
        line = parseInt(parts[0]) - 1
        offset = parseInt(parts[1])
        f = newFileStream(fname, fmRead)
        src = f.readAll()
        lines = src.split("\n")
        pad = repeat(' ', offset + 2)

      stderr.writeLine("  " & lines[line])
      stderr.writeline(pad & "^")
    if debug:
      raise

  proc showHelp() {.noreturn.} =
    echo """
con4m [flags] first.config ...
      Evaluates your configuration file, dumping a JSON string with the results.

      Writes to standard output by default.  If multiple config files are provided, 
      they are executed 'stacked', in command-line order.

      Note that this interface currently does not support callbacks,
      and loads the default functions only. 

      FLAGS:
      -a, --ascii              Output text instead of JSON
      -v, --verbose            Output (to stderr) additional information.
      -o:file, --outfile:file  Redirect JSON or ASCII output to a file.
      -d, --debug              Throw NIM exceptions instead of printing error messages
      -h, --help               This help message      
"""

    quit()

  let argv = commandLineParams()
  var
    outfilename: Option[string] = none(string)
    conffiles: seq[string] = @[]
    verbose: bool = false
    ascii: bool = false
    debug: bool = false
    outstream: Stream


  for kind, k, v in getopt(argv,
                           {'h', 'v', 'a', 'd'},
                           @["help", "verbose", "ascii", "debug"]):
    case kind
    of cmdArgument:
      conffiles.add(k)
    of cmdLongOption, cmdShortOption:
      case k
      of "help", "h":
        showHelp()
      of "debug", "d":
        debug = true
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
    opt: Option[(ConfigState, Con4mScope)]

  try:
    opt = evalConfig(conffiles[0])
  except:
    printCompilerError(conffiles[0], getCurrentExceptionMsg(), debug)

  if opt.isNone():
    echo "Failed to load: " & conffiles[0]
    quit()

  (state, scope) = opt.get()

  if verbose or len(conffiles) == 1:
    outstream.showResults(scope, conffiles[0], ascii, verbose)

  # Now, load the rest of the conf files, using the same scope.
  for filename in conffiles[1 .. ^1]:
    var scopeOpt: Option[Con4mScope]

    if verbose:
      echo "Loading config file: " & filename
    try:
      scopeOpt = state.stackConfig(filename)
    except:
      printCompilerError(filename, getCurrentExceptionMsg(), debug)

    if scopeOpt.isNone():
      echo "Failed to load: " & filename
      quit()

    scope = scopeOpt.get()

    if verbose or filename == conffiles[^1]:
      outstream.showResults(scope, filename, ascii, verbose)
