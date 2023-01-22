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

import nimutils

import con4m/[treecheck, types, eval, builtins, spec, codegen, st, dollars],
       con4m/errmsg
export treecheck, types, eval, builtins, spec, codegen, st, dollars, errmsg

when isMainModule:
  import os, parseopt, options, streams,json
  import con4m/dollars

  discard subscribe(con4mTopic, defaultCon4mHook)

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
    opt = evalConfig(conffiles[0], addBuiltins = true)
  except:
    discard
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
      discard

    if scopeOpt.isNone():
      echo "Failed to load: " & filename
      quit()

    scope = scopeOpt.get()

    if verbose or filename == conffiles[^1]:
      outstream.showResults(scope, filename, ascii, verbose)
