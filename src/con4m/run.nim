## Highest-level API for executing con4m. The macros provide more
## abstraction for stuff written in Nim.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022, 2023, Crash Override, Inc.

import options, types, eval, st, spec, builtins

proc newConfigState*(node:        Con4mNode,
                     spec:        ConfigSpec     = nil,
                     addBuiltins: bool           = true,
                     exclude:     openarray[int] = []): ConfigState =
  node.attrScope = AttrScope()
  node.varScope  = VarScope(parent: none(VarScope))

  ## Return a new `ConfigState` object, optionally setting the `spec`
  ## object, and, if requested via `addBuiltins`, installs the default
  ## set of builtin functions.
  if spec != nil:
    result = ConfigState(attrs:   node.attrScope,
                         globals: RuntimeFrame(),
                         spec:    some(spec))
  else:
    result = ConfigState(attrs: node.attrScope, globals: RuntimeFrame())

  if addBuiltins:
    result.addDefaultBuiltins(exclude)

proc initRun*(n: Con4mNode, s: ConfigState) {.inline.} =
  var topFrame = s.globals

  if topFrame == nil:
    topFrame = RuntimeFrame()

  # TODO: validate lock state of globals across runs.
  for k, sym in n.varScope.contents:
    if k notin topFrame:
      topFrame[k] = sym.value

  s.frames = @[topFrame]

proc postRun(state: ConfigState) =
  state.globals = state.frames[0]
  state.frames  = @[]

proc runBase(state: ConfigState, tree: Con4mNode): bool =
  if tree == nil: return false
  state.errors = @[]
  tree.checkTree(state)
  tree.initRun(state)
  try:
    tree.evalNode(state)
  finally:
    state.postRun()

  if s.spec.isSome():
    return s.validateConfig()
    
  return true
  
proc firstRun*(stream:      Stream,
               fileName:    string,
               spec:        ConfigSpec = nil,
               addBuiltins: bool = true, 
               customFuncs: openarray[(string, BuiltinFn, string)] = [],
               exclude:     openarray[int] = [],
               callbacks:   openarray[(string, string)] = []):
                 (ConfigState, bool) =
    setCurrentFileName(fileName)  
    # Parse throws an error if it doesn't succeed.
    var
      tree   = parse(stream, filename)
       state = newConfigState(tree, spec, addBuiltins, exclude)

    for (name, fn, tinfo) in customFuncs:
      state.newBuiltIn(name, fn, tinfo)

    for (name, tinfo) in callbacks:
      state.newCallback(name, tinfo)

    if state.runBase(tree):
      return (state, true)
    else:
      return (state, false)

proc firstRun*(contents:    string,
               fileName:    string,
               spec:        ConfigSpec = nil,
               addBuiltins: bool = true, 
               customFuncs: openarray[(string, BuiltinFn, string)] = [],
               exclude:     openarray[int] = [],
               callbacks:   openarray[(string, string)] = []):
                 (ConfigState, bool) =
  return firstRun(newStringStream(contents), fileName, spec, addBuiltins,
                  customFuncs, exclude, callbacks)

proc firstRun*(fileName:    string,
               spec:        ConfigSpec = nil,
               addBuiltins: bool = true, 
               customFuncs: openarray[(string, BuiltinFn, string)] = [],
               exclude:     openarray[int] = [],
               callbacks:   openarray[(string, string)] = []):
                 (ConfigState, bool) =
  return firstRun(newFileStream(fileName, fmRead), fileName, spec,
                  addBuiltins, customFuncs, exclude, callbacks)
  
proc stackConfig*(s:        ConfigState,
                  stream:   Stream,
                  fileName: string): Option[Con4mScope] =
  setCurrentFileName(fileName)
  runBase(s, parse(stream, fileName))

proc stackConfig*(s:        ConfigState,
                  contents: string,
                  filename: string): Option[Con4mScope] =
  setCurrentFileName(filename)
  runBase(s, parse(newStringStream(filename), filename))

proc stackConfig*(s: ConfigState, filename: string): Option[Con4mScope] =
  setCurrentFileName(filename)
  runBase(s, parse(newFileStream(filename), filename))

proc runCallback*(s:     ConfigState,
                  name:  string,
                  args:  seq[Box],
                  tinfo: Option[Con4mType] = none(Con4mType)): Option[Box] =
  if tinfo.isSome():
    return s.sCall(name, args, tinfo.get())
  if not s.funcTable.contains(name):
    # User did not supply the callback.
    return
  if len(s.funcTable[name]) > 0:
    raise newException(ValueError,
                       "When supporting callbacks with multiple signatures, " &
                       "you must supply the type when calling runCallback()")
  let impl = s.funcTable[name][0].impl
  return s.sCallUserDef(name, args, callback = true, nodeOpt = impl)
