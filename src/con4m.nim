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
import con4m/[eval, dollars, spec, run]
export errmsg, types, lex, parse, st, builtins, treecheck, typecheck
export eval, dollars, spec, run


when isMainModule:
  setCon4mVerbosity(c4vShowLoc)
  import nimutils/filetable
  const helpPath   = staticExec("pwd") & "/help/"
  const helpCorpus = newOrderedFileTable(helpPath)
    
  import nimutils, nimutils/help
  import os, streams, json, tables
  
  discard subscribe(con4mTopic, defaultCon4mHook)

  let argParser = newArgSpec().addArgs(min=1).
                               addBinaryFlag('h',"help").
                               addBinaryFlag('c', "no-color").               
                               addBinaryFlag('p', "parse").
                               addBinaryFlag('t', "type").
                               addBinaryFlag('k', "dump-tokens", setDumpToks).
                               addBinaryFlag('s', "show-table").
                               addBinaryFlag('J', "no-json"). 
                               addBinaryFlag('d', "debug", setCTrace)
  try:
    let
      state = argParser.parse()
      flags = state.getFlags()
      args  = state.getArgs()

    state.commit()

    if existsEnv("NO_COLOR") or "no-color" in flags:
      setShowColors(false)
      
    if "help" in flags:
      echo getHelp(helpCorpus, args)
      quit()
    if len(args) == 0:
      echo getHelp(helpCorpus, @["help"])
    else:
      if "parse" in flags and "type" in flags:
        echo("Error: specified multiple passes to stop at.")
        quit()
      if "parse" in flags:
        for arg in args:
          let tree = parse(newFileStream(arg), arg)
          echo $(tree)
      elif "type" in flags:
        for arg in args:
          var
            tree  = parse(newFileStream(arg), arg)
            state = newConfigState(tree)
          tree.checkTree(state)
          echo ($tree)
      else:
        var (ctx, ok) = firstRun(args[0])
        if "show-table" in flags:
          echo $(ctx.attrs)
        for arg in args[1 .. ^1]:
          discard ctx.stackConfig(arg)
          if "show-table" in flags:
            echo $(ctx.attrs)
        if "no-json" notin flags:
          stderr.writeLine(toAnsiCode([acBRed]))
          stderr.writeLine("Results:" & toAnsiCode([acUnbold, acCyan]))
          echo parseJson(ctx.attrs.scopeToJson()).pretty()
          stderr.writeLine(toAnsiCode([acReset]))
  except:
    echo "See con4m --help for help on usage."
    
