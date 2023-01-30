## Highest-level API for executing con4m. The macros provide more
## abstraction for stuff written in Nim.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022, 2023, Crash Override, Inc.

import tables, options, streams, nimutils, strformat
import errmsg, types, parse, treecheck, eval, spec, builtins

proc newConfigState*(node:        Con4mNode,
                     spec:        ConfigSpec     = nil,
                     addBuiltins: bool           = true,
                     exclude:     openarray[int] = []): ConfigState =
  let attrRoot   = AttrScope(parent: none(AttrScope), name: "<<root>>")
  node.attrScope = attrRoot
  node.varScope  = VarScope(parent: none(VarScope))

  let specOpt = if spec == nil: none(ConfigSpec) else: some(spec)
  result      = ConfigState(attrs:   attrRoot,
                            globals: RuntimeFrame(),
                            spec:    specOpt)

  node.attrScope.config = result
  
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
  tree.checkTree(state)
  tree.initRun(state)
  try:
    ctrace(fmt"{getCurrentFileName()}: Beginning evaluation.")
    tree.evalNode(state)
    ctrace(fmt"{getCurrentFileName()}: Evaluation done.")
  finally:
    state.postRun()

  if state.spec.isSome():
    state.validateState()
    
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
      tree  = parse(stream, filename)
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
  
proc stackConfig*(s: ConfigState, stream: Stream, fileName: string): bool =
  setCurrentFileName(fileName)
  return runBase(s, parse(stream, fileName))

proc stackConfig*(s: ConfigState, contents: string, filename: string): bool =
  setCurrentFileName(filename)
  return runBase(s, parse(newStringStream(filename), filename))

proc stackConfig*(s: ConfigState, filename: string): bool =
  setCurrentFileName(filename)
  return runBase(s, parse(newFileStream(filename), filename))

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
