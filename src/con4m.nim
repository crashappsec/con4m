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
    phaseOps = ["tokenize", "parse", "check", "eval"]
    outOps   = ["json", "pretty", "none"]
    argParser  = newArgSpec(defaultCmd = true).
                 addChoiceFlag('a', "attr-output", outOps, true, setOutStyle).
                 addBinaryFlag('k', "show-tokens", setDumpToks).
                 addBinaryFlag('t', "show-parse-tree", setShowParse).
                 addBinaryFlag('x', "show-checked-tree", setShowChecked).
                 addBinaryFlag('F', "show-funcs", setShowFuncs).
                 addPairedFlag('c', 'C', "color", setShowColors).
                 addBinaryFlag('d', "debug", setCTrace).
                 addBinaryFlag('h',"help")

  argParser.addCommand("compile", ["c"]).addArgs(min=1).
               addFlagWithStrArg('s', "spec", setSpecFile).
               addChoiceFlag('p', "phase", phaseOps, true, setStopPhase)

  argParser.addCommand("specgen", ["spec", "c42", "genspec"]).addArgs(min=1, max=1).
               addFlagWithStrArg('l', "language", setGenOutLang).
               addFlagWithStrArg('o', "output-file", setGenOutFile)

  argParser.addCommand("help", ["h"]).addArgs(min=0)

  try:
    let (parsed, done) = argParser.mostlyParse(topHasDefault = true)

    if parsed.getCurrentCommandName().isNone():
      applyDefault(parsed, "compile")

    let flags   = parsed.getFlags()
    parsed.commit()

    currentCmd = parsed.getCurrentCommandName().get()

    var args = parsed.getSubCommand().get().getArgs()
    if "help" in flags:
      if len(args) == 0: args = @["help"]
      echo getHelp(helpCorpus, args)
      quit(1)

    case currentCmd
    of "compile":
      var
        spec:    ConfigSpec = nil
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
            stderr.writeLine(toAnsiCode(acBold) & "Spec successfully validated.")
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
