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

import con4m/[errmsg, types, lex, parse, st, builtins, treecheck, typecheck]
import con4m/[eval, dollars, spec, run, c42spec]
export errmsg, types, lex, parse, st, builtins, treecheck, typecheck
export eval, dollars, spec, run, c42spec


when isMainModule:
  setCon4mVerbosity(c4vShowLoc)
  import nimutils, nimutils/help
  import json, tables, options
  const helpPath   = staticExec("pwd") & "/help/"
  const helpCorpus = newOrderedFileTable(helpPath)


  discard subscribe(con4mTopic, defaultCon4mHook)


  var showFuncs = false
  proc setShowFuncs() =
    showFuncs = true

  var
    specFile = none(string)
    useC42   = false


  proc setSpecFile(fname: string) =
    if useC42:
      fatal("Can't use c42 spec and an external spec together")
    specFile = some(fname)

  proc setC42() =
    if specFile.isSome():
      fatal("Can't use c42 spec and an external spec together")
    useC42 = true

  proc getConfigSpec(): ConfigSpec =
    if specFile.isNone():
      if useC42:
        return buildC42Spec()
      return nil
    let `spec?` = c42Spec(specfile.get())

    if `spec?`.isNone():
      fatal("Config spec file failed to load.")
    return `spec?`.get()

  var attrOutStyle = "json"
  proc setOutStyle(style: string) =
    attrOutStyle = style

  proc styleOutput(ctx: ConfigState, final: bool) =
    if attrOutStyle == "pretty":
      if not final:
        echo $(ctx.attrs)
    if attrOutStyle == "json":
      if final:
        stderr.writeLine(toAnsiCode([acBRed]))
        stderr.writeLine("Results:" & toAnsiCode([acUnbold, acCyan]))
        echo parseJson(ctx.attrs.scopeToJson()).pretty()
        stderr.writeLine(toAnsiCode([acReset]))

  let
    phaseOps = ["tokenize", "parse", "check", "eval"]
    outOps   = ["json", "pretty", "none"]
    argParser  = newArgSpec().addArgs(min=0).
                 addFlagWithStrArg('s', "spec", setSpecFile).
                 addChoiceFlag('p', "phase", phaseOps, true, setStopPhase).
                 addChoiceFlag('a', "attr-output", outOps, true, setOutStyle).
                 addBinaryFlag('k', "show-tokens", setDumpToks).
                 addBinaryFlag('t', "show-parse-tree", setShowParse).
                 addBinaryFlag('x', "show-checked-tree", setShowChecked).
                 addBinaryFlag('F', "show-funcs", setShowFuncs).
                 addBinaryFlag('4', "c42", setC42).
                 addPairedFlag('c', 'C', "color", setShowColors).
                 addBinaryFlag('h',"help").
                 addBinaryFlag('d', "debug", setCTrace)
  try:
    let
      state = argParser.parse()
      args  = state.getArgs()
      flags   = state.getFlags()

    state.commit()

    if "help" in flags or (len(args) == 1 and args[0] == "help"):
      if len(args) != 0:
        echo getHelp(helpCorpus, args)
      else:
        echo getHelp(helpCorpus, @["help"])
      quit(1)
    if len(args) == 0:
      raise newException(ValueError, "Not enough arguments given.")
    var
      spec      = getConfigSpec()
      (ctx, ok) = firstRun(args[0], spec)
    if ok:
      ctx.styleOutput(false)
      for arg in args[1 .. ^1]:
        discard ctx.stackConfig(arg)
        ctx.styleOutput(false)
      ctx.styleOutput(true)
      if showFuncs:
        stderr.write($(ctx.funcTable))
    else:
      stderr.writeLine(toAnsiCode(acBold) & "Compilation failed.")
      quit(1)
  except ValueError:
    echo toAnsiCode(acBRed) & "con4m: " & toAnsiCode(acReset) &
      getCurrentExceptionMsg()
    if getCon4mVerbosity() == c4vMax:
      echo getCurrentException().getStackTrace()

    quit(1)
  except:
    echo "See con4m --help for help on usage."
    quit(1)
