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


when defined(CAPI):
  import con4m/capi
  export capi
elif isMainModule:
  setCon4mVerbosity(c4vShowLoc)
  import nimutils, nimutils/help, con4m/codegen
  import json, tables, options, streams
  const helpPath   = staticExec("pwd") & "/help/"
  const helpCorpus = newOrderedFileTable(helpPath)

  discard subscribe(con4mTopic, defaultCon4mHook)


  var
    specFile     = none(string)
    showFuncs    = false
    genOutLang   = none(string)
    genOutFile   = none(string)
    currentCmd   = ""
    attrOutStyle = "default"

  proc setShowFuncs() =
    showFuncs = true

  proc setGenOutLang(s: string) =
    genOutLang = some(s)

  proc setGenOutFile(fname: string) =
    genOutFile = some(fname)

  proc setSpecFile(fname: string) =
    specFile = some(fname)

  proc setOutStyle(style: string) =
    attrOutStyle = style

  proc styleOutput(ctx: ConfigState, final: bool) =
    if attrOutStyle == "default":
      if currentCmd == "compile":
        attrOutStyle = "json"
      # Else, we don't show this stuff for specs by default.

    case attrOutStyle
    of "pretty":
      if not final:
        echo $(ctx.attrs)
    of "json":
      if final:
        stderr.writeLine(toAnsiCode([acBRed]))
        stderr.writeLine("Results:" & toAnsiCode([acUnbold, acCyan]))
        echo parseJson(ctx.attrs.scopeToJson()).pretty()
        stderr.writeLine(toAnsiCode([acReset]))
    else:
      discard
  let
    phaseOps  = ["tokenize", "parse", "check", "eval"]
    outOps    = ["json", "pretty", "none"]
    argParser =
      newCmdLineSpec().
        addChoiceFlag("attr-output", outOps, true, ["a"], callback=setOutStyle).
        addBinaryFlag("show-tokens", ["k"], callback = setDumpToks).
        addBinaryFlag("show-parse-tree", ["t"], callback = setShowParse).
        addBinaryFlag("show-checked-tree", ["x"], callback = setShowChecked).
        addBinaryFlag("show-funcs", ["F"], callback = setShowFuncs).
        addBinaryFlag("debug", ["d"], callback = setCTrace).
        addBinaryFlag("help", ["h"]).
        addYesNoFlag("color", some('c'), some('C'), callback = setShowColors)

  argParser.addCommand("compile", ["c"]).
    addArgs(min=1).
    addFlagWithArg("spec", ["s"], callback = setSpecFile).
    addChoiceFlag("phase", phaseOps, true, ["p"], callback = setStopPhase)

  argParser.addCommand("specgen", ["spec", "c42", "genspec"]).
    addFlagWithArg("language", ["l"], callback = setGenOutLang).
    addFlagWithArg("output-file", ["o"], callback = setGenOutFile).
    addArgs(min=1, max=1)


  argParser.addCommand("help", ["h"]).addArgs(min=0)

  try:
    let
      parse = argParser.parse(defaultCmd = some("compile"))
      flags = parse.flags

    currentCmd = parse.getCommand()
    var args   = parse.getArgs(currentCmd).get()
    if "help" in flags:
      if len(args) == 0: args = @["help"]
      echo getHelp(helpCorpus, args)
      quit(1)

    case currentCmd
    of "compile":
      var
        spec:    ConfigSpec  = nil
        specCtx: ConfigState = nil
      if specFile.isSome():
        let `spec?` = c42Spec(specfile.get())
        if `spec?`.isNone():
          fatal("Config spec file failed to load.")
        else:
          (spec, specCtx) = `spec?`.get()
      var (ctx, ok) = firstRun(args[0], spec, evalCtx = specCtx)
      if ok:
        ctx.styleOutput(false)
        for arg in args[1 .. ^1]:
          discard ctx.stackConfig(arg)
          ctx.styleOutput(false)
        ctx.styleOutput(true)
        if showFuncs:
          stderr.write($(ctx.funcTable))
        quit(0)
    of "specgen":
      var (ctx, ok) = firstRun(args[0], buildC42Spec())
      if ok:
          ctx.styleOutput(false)
          ctx.styleOutput(true)
          if showFuncs:
            stderr.write($(ctx.funcTable))
          if not genOutLang.isSome():
            raise newException(ValueError,
                               "Must supply a language to generate with " &
                                 "--language, or --language=none for no output")
          let lang = genOutLang.get()

          if lang == "none":
            stderr.writeLine(toAnsiCode(acBold) &
              "Spec successfully validated.")
            quit(0)
          var genOutput = generateCode(ctx, lang)
          if genOutFile.isNone():
            echo genOutput
          else:
            var
              fullPath = resolvePath(genOutFile.get())
              s        = newFileStream(fullPath, fmWrite)

            s.write(genOutput)
            stderr.writeLine(toAnsiCode(acBold) & "Output to: " & fullPath)
          quit(0)
    of "help":
      if len(args) == 0: args = @["help"]
      echo getHelp(helpCorpus, args)
      quit(1)
    else:
      unreachable

    stderr.writeLine(toAnsiCode(acBold) & "Compilation failed.")
    quit(1)
  except ValueError:
    echo toAnsiCode(acBRed) & "error: " & toAnsiCode(acReset) &
      getCurrentExceptionMsg()
    if getCon4mVerbosity() == c4vMax:
      echo getCurrentException().getStackTrace()
    quit(1)
  except:
    echo perLineWrap(toAnsiCode(acBRed) & "error: " & toAnsiCode(acReset) &
                     getCurrentExceptionMsg(),
                    firstHangingIndent = len("error: con4m: "),
                    remainingIndents = 0)
    if getCon4mVerbosity() == c4vMax:
      echo getCurrentException().getStackTrace()
    echo "See con4m help for help on usage."
    quit(1)
