import tables, options, streams, strutils, sequtils, sugar, os, json
import lex, types, errmsg, parse, treecheck, eval, spec, builtins, dollars,
       getopts, c42spec, st, typecheck

import nimutils/ansi

const getOptsSpec = staticRead("c4m/getopts.c42spec")

type
  ActionKind* = enum
    akInitState, akCallback, akC42SpecLoad, akConfLoad, akValidate,
    akStartGetOpts, akFinalizeGetOpts, akSetErrHandler

  StackCallback*     = (ConfigState) -> void
  ErrorCallback*     = (string, string) -> bool
  StackRunTimeScope* = enum srsConfig, srsValidation, srsBoth
  ConfigStep*        = ref object
    stageName*: string
    case kind*: ActionKind
    of akInitState:
      addBuiltins*: bool
      customFuncs*: seq[(string, BuiltinFn)]
      exclusions*:  seq[string]
      which*:       StackRuntimeScope
    of akCallback:
      callback*:    StackCallback
    of akC42SpecLoad, akConfLoad, akValidate:
      fileName*:      string
      stream*:        Stream
      genSpec*:       bool
      run*:           bool
      doPreCheck*:    bool
      doPostCheck*:   bool
      showToks*:      bool
      showParsed*:    bool
      showTyped*:     bool
      showPretty*:    bool
      showJson*:      bool
    of akStartGetOpts, akFinalizeGetOpts:
      args*:          seq[string]
      resPath*:       string
    of akSetErrHandler:
      handler*:       ErrorCallback

  ConfigStack* = ref object
    steps*:               seq[ConfigStep]
    ix*:                  int
    specValidationState*: ConfigState
    validationState*:     ConfigState
    configState*:         ConfigState
    lastUsed*:            ConfigState
    c42SpecObj*:          ConfigSpec
    errorCb*:             ErrorCallback
    errored*:             bool
    getOptOptions*:       seq[ArgResult]
    finalOpt*:            ArgResult
    bt*:                  bool
    getoptStarted*:       bool
    debug*:               bool

proc clearError*(s: ConfigStack) =
  s.errored = false
proc getRuntime*(s: ConfigStack): ConfigState = s.configState
  
proc newConfigStack*(): ConfigStack = ConfigStack()
proc addSystemBuiltins*(stack:      ConfigStack,                        
                        exclusions: openarray[string] = @[],
                        which = srsBoth): ConfigStack {.discardable.} =
  result   = stack
  var step = ConfigStep(kind: akInitState, addBuiltins: true, which: which,
                        exclusions: exclusions.toSeq())
  stack.steps.add(step)

proc addCustomBuiltins*(stack: ConfigStack,
                        fns:   openarray[(string, BuiltinFn)],
                        which = srsBoth):  ConfigStack {.discardable.} =
  result   = stack
  var step = ConfigStep(kind: akInitState, customFuncs: fns.toSeq(),
                        which: which)

  stack.steps.add(step)

proc addCallback*(stack:     ConfigStack,
                  callback:  StackCallback,
                  stageName: string = ""): ConfigStack {.discardable.} =
  result   = stack
  var step = ConfigStep(kind: akCallback, callback: callback,
                        stageName: stageName)

  stack.steps.add(step)

proc setErrorHandler*(stack: ConfigStack, cb: ErrorCallback):
                    ConfigStack {.discardable.} =
  result    = stack
  var step = ConfigStep(kind: akSetErrHandler, handler: cb)
  stack.steps.add(step)

proc addSpecLoad*(stack:        ConfigStack,
                  fileName:     string,
                  stream:       Stream,
                  preValidate:  bool = false,
                  postValidate: bool = false,
                  genSpec:      bool = true,
                  stageName:    string = fileName):
                    ConfigStack {.discardable.} =
  result   = stack
  var step = ConfigStep(kind: akC42SpecLoad, fileName: fileName, run: true,
                        doPreCheck: preValidate, doPostCheck: postValidate,
                        stageName: stageName, stream: stream, genSpec: genSpec)
  
  stack.steps.add(step)

proc addGetoptSpecLoad*(stack:     ConfigStack,
                        stageName: string = "getopts-specload"):
                          ConfigStack {.discardable.} =
  result = stack
  var step = ConfigStep(kind: akC42SpecLoad, filename: "getopts-spec",
                        run: true, doPreCheck: false, doPostCheck: false,
                        stageName: stageName, genSpec: true,
                        stream: newStringStream(getOptsSpec))
  stack.steps.add(step)

proc addConfLoad*(stack:        ConfigStack,
                  fileName:     string,
                  stream:       Stream,
                  preValidate:  bool   = true,
                  postValidate: bool   = true,
                  stageName:    string = fileName):
                    ConfigStack {.discardable.} =
  result   = stack
  var step = ConfigStep(kind: akConfLoad, fileName: fileName, run: true,
                        doPreCheck: preValidate, doPostCheck: postValidate,
                        stageName: stageName, stream: stream)
  stack.steps.add(step)
  
proc addValiate*(stack:       ConfigStack,
                 doPreCheck:  bool = true,
                 doPostCheck: bool = true,                    
                 stageName:   string = ""):
                   ConfigStack {.discardable.} = 
  result   = stack
  var step = ConfigStep(kind: akValidate, doPreCheck: doPreCheck, run: false,
                        doPostCheck: doPostCheck)
  stack.steps.add(step)
  
proc addStartGetOpts*(stack:     ConfigStack,
                      resPath:   string = "getopts",
                      stageName: string = "start-getopts",
                      args:      seq[string] = @[]):
                        ConfigStack {.discardable.} =
  result   = stack

  var argv = if len(args) > 0: args else: commandLineParams()
  var step = ConfigStep(kind: akStartGetOpts, args: argv, stageName: stageName,
                        resPath: resPath)
  stack.getoptStarted = true
  stack.steps.add(step)

proc addFinalizeGetOpts*(stack:     ConfigStack,
                         resPath:   string = "getopts",
                         stageName: string = "finalize-getopts"):
                           ConfigStack {.discardable.} =
  if not stack.getOptStarted:
    raise newException(ValueError, "startGetOpt must have been done before " &
      "you can add a finalize step")
  result   = stack

  var step = ConfigStep(kind: akFinalizeGetOpts, stageName: stageName,
                        resPath: resPath)
  stack.steps.add(step)
  
proc createEmptyRuntime(): ConfigState =
   result = ConfigState(attrs:
                AttrScope(parent: none(AttrScope), name: "<<root>>"))
   result.attrs.config = result

template initializeSpec(specRuntime: ConfigState) =
  specRuntime.spec = some(buildC42Spec())
    
proc runOneConf(stack: ConfigStack, conf, spec: ConfigState) =
  let step = stack.steps[stack.ix]
  var tree: Con4mNode
  
  try:
    if step.run:
      if step.stream == nil:
        fatal("Unable to open '" & step.fileName & "' for reading")
        step.stream.setPosition(0)
      let
        (valid, tokens) = step.stream.lex()

      if not valid:
        let msg = case tokens[^1].kind
        of ErrorTok:         "Invalid character found"
        of ErrorLongComment: "Unterminated comment"
        of ErrorStringLit:   "Unterminated string"
        of ErrorOtherLit:    "Unterminated literal"
        else:                "Unknown error" # Not be possible w/o a lex bug
        fatal(msg)

      if step.showToks:
        for i, token in tokens:
          stderr.writeLine($i & ": " & $token)
        
      tree          = tokens.parse(step.fileName)
      tree.varScope = VarScope(parent: none(VarScope))

      if step.showParsed:
        stderr.writeLine($tree)
        
      conf.secondPass = false
      tree.checkTree(conf)

      if step.showTyped:
        stderr.write(toAnsiCode(acBCyan) & "Entry point:\n" &
                     toAnsiCode(acReset))
        stderr.writeLine($tree)
        for item in conf.moduleFuncDefs:
          let typeStr = `$`(item.tInfo)
          stderr.write(toAnsiCode(acBCyan))
          stderr.writeLine("Function: " & item.name & typeStr)
          stderr.write(toAnsiCode(acReset))
          stderr.writeLine($item.impl.get())

    if step.doPreCheck and spec != nil:
      if conf.spec.isNone():
        if stack.c42SpecObj == nil:
          fatal("Spec runtime exists, but no spec object generated from it.")
        conf.spec = some(stack.c42SpecObj)
      conf.preEvalCheck(spec)

    if step.run: # Set up the runtime stack.
      var topFrame = RuntimeFrame()
      for k, sym in tree.varScope.contents:
        if k notin topFrame:
          topFrame[k] = sym.value

      conf.frames = @[topFrame]
  
      ctrace(step.fileName & ": Beginning evaluation.")
      tree.evalNode(conf)
      ctrace(step.fileName & ": Evaluation done.")
  finally:
    if step.run:
      conf.numExecutions += 1
      
      # Clean up the runtime stack and stash exported global state.
      if conf.frames.len() > 0:
        for k, v in conf.frames[0]:
          if k in conf.keptGlobals:
            conf.keptGlobals[k].value = v
        conf.frames = @[]
        
  if step.doPostCheck and spec != nil:
    conf.validateState(spec)

  if step.showPretty:
    echo conf.attrs

  if step.showJson:
    stderr.writeLine(toAnsiCode([acBRed]))
    stderr.writeLine("Results:" & toAnsiCode([acUnbold, acCyan]))
    echo parseJson(conf.attrs.scopeToJson()).pretty()
    stderr.writeLine(toAnsiCode([acReset]))

proc oneInit(s: ConfigState, step: ConfigStep) =
  if step.addBuiltins:
    for item in defaultBuiltins:
      let
        (sig, impl) = item
        ix          = sig.find('(')
        name        = sig[0 ..< ix].strip()
        tInfo       = sig[ix .. ^1].toCon4mType()

      if s.funcTable.contains(name):
        for existing in s.funcTable[name]:
          # Check to see if it's already loaded.
          if not isBottom(copyType(tInfo), copyType(existing.tInfo)): continue
      
      s.newCoreFunc(sig, impl)
      
    
  for item in step.customFuncs:
    let (sig, impl) = item
    s.newCoreFunc(sig, impl)

  for sig in step.exclusions:
    let
      ix    = sig.find('(')
      name  = sig[0 ..< ix].strip()
      tInfo = sig[ix .. ^1].toCon4mType()

    if s.funcTable.contains(name):
      var canRemain: seq[FuncTableEntry] = @[]
      for item in s.funcTable[name]:
        if isBottom(copyType(tInfo), copyType(item.tInfo)):
          canRemain.add(item)
      if len(canRemain) != 0:
        s.funcTable[name] = canRemain
      else:
        s.funcTable.del(name)
    
proc doInit(stack: ConfigStack) =
  let step = stack.steps[stack.ix]

  if step.which != srsConfig:
    if stack.validationState == nil:
      stack.validationState = createEmptyRuntime()
    stack.validationState.oneInit(step)
    stack.lastUsed = stack.validationState
    
  if step.which != srsValidation:
    if stack.configState == nil:
      stack.configState = createEmptyRuntime()
      if stack.c42SpecObj != nil:
        stack.configState.spec = some(stack.c42SpecObj)
    stack.configState.oneInit(step)
    stack.lastUsed = stack.configState

proc doCallback(stack: ConfigStack) {.inline.} = 
  # If it's nil, *shrug*
  stack.steps[stack.ix].callback(stack.lastUsed)

proc doSpecLoad(stack: ConfigStack) =
  let step = stack.steps[stack.ix]
  if stack.validationState == nil:
    stack.validationState = createEmptyRuntime()
      
  if stack.specValidationState == nil and (step.doPreCheck or step.doPostCheck):
    stack.specValidationState  = createEmptyRuntime()
    stack.validationState.spec = some(buildC42Spec())
  # load the file.
      
  stack.runOneConf(stack.validationState, stack.specValidationState)
  
  if step.genSpec:
    stack.c42SpecObj = stack.validationState.generateC42Spec()
    if stack.configState != nil:
      stack.configState.spec = some(stack.c42SpecObj)
      
  stack.lastUsed  = stack.validationState

proc doConfLoad(stack: ConfigStack) =
  if stack.configState == nil:
    stack.configState = createEmptyRuntime()
    if stack.c42SpecObj != nil:
      stack.configState.spec = some(stack.c42SpecObj)
  if stack.validationState == nil:
    let step = stack.steps[stack.ix]
    step.doPreCheck  = false
    step.doPostcheck = false
  stack.runOneConf(stack.configState, stack.validationState)
  stack.lastUsed = stack.configState

proc doValidate(stack: ConfigStack) =
  if stack.lastUsed == nil:
      raise newException(ValueError, "Can't validate before running anything")
  elif stack.lastUsed == stack.configState:
    if stack.configState.spec.isNone():
      raise newException(ValueError, "Can't validate: no spec loaded yet.")
    # If we're here, they loaded a spec obj programatically.
    # It won't be possible to use a runtime in such a case, but we might
    # need one later, and they're cheap, so just make one already.
    if stack.validationState == nil:
      stack.validationState = createEmptyRuntime()
      
    stack.runOneConf(stack.configState, stack.validationState)
  else:
    stack.runOneConf(stack.validationState, stack.specValidationState)

proc doStartGetOpts(stack: ConfigStack) =
  var res: seq[ArgResult]
  
  if stack.configState == nil:
    stack.configState = createEmptyRuntime()
  if stack.validationState == nil:
    raise newException(ValueError, "No spec to read getopts section from")
  let step = stack.steps[stack.ix]
  res = runManagedGetopt(stack.configState, step.args, step.resPath)
  if len(res) == 1:
    stack.finalOpt = res[0]
  else:
    stack.getOptOptions = res
    
proc doFinalizeGetOpts(s: ConfigStack) =
  if len(s.getOptOptions) < 2: return
  s.finalOpt      = finalizeManagedGetopt(s.configState, s.getOptOptions)
  s.getOptOptions = @[]

proc doSetErrorHandler(stack: ConfigStack) =
  let step = stack.steps[stack.ix]
  stack.errorCb = step.handler

proc getArgResult*(stack: ConfigStack): ArgResult = stack.finalOpt
  
proc run*(stack: ConfigStack, backtrace = false, debug = false):
        Option[ConfigState] {.discardable.} =
  result = none(ConfigState)
  if stack.errored: return
  stack.bt    = backtrace
  stack.debug = debug
  
  while stack.ix < len(stack.steps):
    let step = stack.steps[stack.ix]
    if stack.debug:
      stderr.writeLine("Begin step " & $(stack.ix) & "; kind = " & $step.kind)
    try:
      case step.kind
      of akInitState:       stack.doInit()
      of akCallback:        stack.doCallback()
      of akC42SpecLoad:     stack.doSpecLoad()
      of akConfLoad:        stack.doConfLoad()
      of akValidate:        stack.doValidate()
      of akStartGetOpts:    stack.doStartGetOpts()
      of akFinalizeGetOpts: stack.doFinalizeGetOpts()
      of akSetErrHandler:   stack.doSetErrorHandler()
      if stack.debug:
        stderr.writeLine("End step " & $(stack.ix))      
    except:
      if stack.debug:
        stderr.writeLine("Error in step " & $(stack.ix))      
      
      if stack.errorCb != nil:
        stack.errored = stack.errorCb(getCurrentExceptionMsg(),
                                      getCurrentException().getStackTrace())
      else:
        stack.errored = true      
        if stack.bt:
          echo getCurrentExceptionMsg()
          echo getCurrentException().getStackTrace()
      if stack.errored:
        raise
    finally:
      stack.ix = stack.ix + 1
