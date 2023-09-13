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
import con4m/[eval, dollars, spec, run, c42spec, getopts, stack, legacy, doc]
export errmsg, types, lex, parse, st, builtins, treecheck, typecheck
export eval, dollars, spec, run, c42spec, getopts, stack, legacy, doc
import streams, nimutils, os

const compilerC42FileName = "con4m/c4m/compiler-config.c42spec"
const compilerConfigFName = "con4m/c4m/c4m-cmdline.c4m"
const c4mc42Contents      = staticRead(compilerC42FileName)
const c4mconfigContents   = staticRead(compilerConfigFName)

when defined(CAPI):
  import con4m/capi
  export capi

elif isMainModule:
  let
    specf    = newStringStream(c4mc42Contents)
    cfgf     = newStringStream(c4mconfigContents)
    conf     = resolvePath("~/.config/con4m/con4m.conf")
    c4mstack = newConfigStack().
                 addSystemBuiltins().
                 addGetoptSpecLoad().
                 addSpecLoad(compilerC42FileName, specf).
                 addConfLoad(compilerConfigFName, cfgf).
                 addStartGetOpts().
                 addFinalizeGetOpts()

  discard subscribe(con4mTopic, defaultCon4mHook)

  if conf.fileExists():
    try:
      let stream = newFileStream(conf)
      discard c4mstack.addConfLoad(conf, stream)
    except:
      stderr.write("Error: could not open external config file " &
                   "(permissions issue?)")
      raise

  c4mstack.run(backtrace = true)

  let
    command   = c4mstack.getCommand()
    config    = c4mstack.getAttrs().get()
    args      = c4mstack.getArgs()
    colorOpt  = getOpt[bool](config, "color")
    specs     = get[seq[string]](config, "specs")

  if colorOpt.isSome(): setShowColors(colorOpt.get())

  setConfigState(config)
  if command == "run":
    con4mRun(args, specs)
  elif command == "gen":
    specgenRun(args)
  else:
    echo "Unknown command: ", command
