##  This module actually walks the tree, directly executing the
##  operations encoded in it (as opposed to generating code that we
##  would then run).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import types
import st
import parse
import treecheck
import typecheck
import nimutils
import nimutils/box

import options
import tables
import strformat

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

const breakMsg = "b"
const continueMsg = "c"

proc pushRuntimeFrame*(s: ConfigState) {.inline.} =
  s.frames.add(RuntimeFrame())

proc popRuntimeFrame*(s: ConfigState): RuntimeFrame {.inline.} =
  return s.frames.pop()

proc runtimeVarLookup*(frames: VarStack,
                       name: string,
                       scope: Con4mScope): Box =

  var
    n = frames.len()
    s = scope

  while n != 0:
    n = n - 1
    let frame = frames[n]
    # TODO: you are here. The second half of this shouldn't be necessary.
    # There should be code injecting this for nodeVarAssign, no??
    # But neither place we're assigning is working :/
    if name in s.entries or (name in frame):
      if name in frame:
        return frame[name]
      else:
        let entry = s.entries[name]
        if entry.value.isSome():
          return entry.value.get()
        else:
          raise newException(Con4mError,
              fmt"Variable {name} used before being defined")
    if s.parent.isSome():
      s = s.parent.get()
    else:
      break
  echo "couldn't lookup ", name
  unreachable

proc runtimeVarLookup(s: ConfigState,
                      name: string,
                      scope: Con4mScope): Box {.inline.} =
  return runtimeVarLookup(s.frames, name, scope)


proc runtimeVarSet*(state: ConfigState,
                    scope: Con4mScope,
                    name: string,
                    val: Box) =
  var
    n = state.frames.len()
    s = scope

  while n != 0:
    n = n - 1
    let frame = state.frames[n]
    if name in s.entries:
      frame[name] = val
      return
    if s.parent.isSome():
      s = s.parent.get()
    else:
      break

  raise newException(Con4mError, fmt"{name} not found in scope at runtime")

proc getFuncBySig*(s: ConfigState,
                   name: string,
                   t: Con4mType): Option[FuncTableEntry] =
  if not s.funcTable.contains(name):
    return

  let candidates = s.funcTable[name]

  for item in candidates:
    if not isBottom(copyType(t), copyType(item.tinfo)):
      return some(item)

proc evalFunc(s: ConfigState, args: seq[Box], node: Con4mNode): Option[Box]

proc sCallUserDef(s: ConfigState,
                  name: string,
                  a1: seq[Box],
                  callback: bool,
                  nodeOpt: Option[Con4mNode]): Option[Box] =

  try:
    if nodeOpt.isNone():
      if callback:
        return # Indicates the callback wasn't provided by the user.
        # user function, no implementation, wouldn't pass the checker.
      unreachable
    return s.evalFunc(a1, nodeOpt.get())
  except Con4mError:
    fatal(getCurrentExceptionMsg(), nodeOpt.get())
  except:
    echo getCurrentException().getStacktrace()
    echo getCurrentExceptionMsg()
    fatal(fmt"Unhandled error when running builtin call: {name}",
          nodeOpt.get())

proc sCallBuiltin(s: ConfigState,
                  name: string,
                  a1: seq[Box],
                  fInfo: FuncTableEntry,
                  node: Con4mNode): Option[Box] =
  var
    attrs, vars: Con4mScope
    scopeOpt = node.scopes
    n: Con4mNode = node

  while scopeOpt.isNone():
    n = n.parent.get()
    scopeOpt = n.scopes

  if n.scopes.isSome():
    attrs = n.scopes.get().attrs
    vars = n.scopes.get().vars
  else:
    attrs = nil
    vars = nil

  try:
    var x = fInfo.builtin(a1, attrs, s.frames, vars)
    return x
  except Con4mError:
    fatal(getCurrentExceptionMsg(), node)
  except:
    echo getCurrentException().getStacktrace()
    echo getCurrentExceptionMsg()
    fatal("Unhandled error when running builtin call: {name}".fmt(), node)

proc sCall*(s: ConfigState,
            name: string,
            a1: seq[Box],
            tinfo: Con4mType,
            nodeOpt: Option[Con4mNode] = none(Con4mNode)
           ): Option[Box] =
  ## This is not meant to be exposed outside this module, except to
  ## the evaluator.  This runs a builtin call, callback or
  ## user-defined function.
  ##
  ## The node parameter will be the node from the caller's scope, if
  ## present.

  let optFunc = s.getFuncBySig(name, tinfo)

  if not optFunc.isSome():
    fatal(fmt"Function {reprSig(name, tinfo)} not found")

  let fInfo = optFunc.get()

  case fInfo.kind
  of FnBuiltIn:
    var x = s.sCallBuiltin(name, a1, fInfo, nodeOpt.get())
    return x
  else:
    let callback = fInfo.kind == FnCallback
    return s.sCallUserDef(name, a1, callback, fInfo.impl)

proc runCallback*(s: ConfigState,
                  name: string,
                  args: seq[Box],
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

proc evalNode*(node: Con4mNode, s: ConfigState)

proc evalKids(node: Con4mNode, s: ConfigState) {.inline.} =
  for item in node.children:
    item.evalNode(s)

template cmpWork(typeWeAreComparing: typedesc, op: untyped) =
  ## This just factors out repetitive code to apply standard
  ## comparison operators like >=.  We unbox, apply the operator, then
  ## box the result.
  let
    v1 = unpack[typeWeAreComparing](node.children[0].value)
    v2 = unpack[typeWeAreComparing](node.children[1].value)
  if op(v1, v2):
    node.value = pack(true)
  else:
    node.value = pack(false)

template binaryOpWork(typeWeAreOping: typedesc,
                      returnType: typedesc,
                      op: untyped) =
  ## Similar thing, but with binary operators like +, -, /, etc.
  let
    v1 = unpack[typeWeAreOping](node.children[0].value)
    v2 = unpack[typeWeAreOping](node.children[1].value)

  var ret: returnType = cast[returnType](op(v1, v2))

  node.value = pack(ret)

proc evalNode*(node: Con4mNode, s: ConfigState) =
  ## This does the bulk of the work.  Typically, it will descend into
  ## the tree to evaluate, and then take whatever action is
  ## apporpriate after, if any.
  case node.kind
  of NodeFuncDef:
    unreachable
  of NodeReturn:
    if node.children.len() != 0:
      node.children[0].evalNode(s)
      s.runtimeVarSet(node.getVarScope(), "result", node.children[0].value)
    raise newException(Con4mError, "return")
  of NodeActuals, NodeBody:
    node.evalKids(s)
    node.value = pack(false)
  of NodeElse:
    s.pushRuntimeFrame()
    try:
      node.evalKids(s)
      node.value = pack(false)
    finally:
      discard s.popRuntimeFrame()
  of NodeSimpLit, NodeEnum, NodeFormalList:
    # Values for these were all assigned when we checked the tree, so we
    # don't have to do anything on this pass.
    return
  of NodeBreak:
    # We implement `break` and `continue` by having the for loop use a
    # `try` block. If we want to do either thing, we want to stop
    # executing the current list of statements in the tree, so be
    # raise an exception.  The for loop looks at the value, and
    # decides whether to start another iteration, or bail, based on that
    # exception message.
    raise newException(ValueError, breakMsg)
  of NodeContinue:
    raise newException(ValueError, continueMsg)
  of NodeSection:
    s.pushRuntimeFrame()
    try:
      node.children[^1].evalNode(s)
    finally:
      discard s.popRuntimeFrame()
  of NodeAttrAssign:
    # We already created scope objects in the tree checking phase.
    # And, we only allow identifiers on the LHS right now. So we just
    # have to evaluate the RHS, then try to update the symbol's value
    # in the symbol table.
    #
    # Note that we also allow fields to be "locked", essentially
    # turned into const-- basically, you should be able to save
    # attribute state across runs, so that you can layer
    # configurations. In such a case, an admin's config might specify
    # not to change a value, which is the idea behind this.
    node.children[1].evalNode(s)
    let
      name = node.children[0].getTokenText()
      scope = node.getAttrScope()
      entry = scope.lookup(name).get()

    if entry.locked:
      fatal("Attempt to assign to a read-only field", node)

    entry.value = some(node.children[1].value)
  of NodeVarAssign:
    # Variables are specific to the run, so this is a little simpler.
    node.children[1].evalNode(s)

    let
      name = node.children[0].getTokenText()
      scope = node.getVarScope()

    s.runtimeVarSet(scope, name, node.children[1].value)
  of NodeUnpack:
    node.children[^1].evalNode(s)
    let
      boxedTup = node.children[^1].value
      tup = unpack[seq[Box]](boxedTup)
      scope = node.getVarScope()

    # Each item is still a Box; we did not unbox all layers, only one.
    for i, item in tup:
      let name = node.children[i].getTokenText()

      s.runtimeVarSet(scope, name, item)
  of NodeIfStmt:
    # This is the "top-level" node in an IF statement.  The nodes
    # below it will all be of kind NodeConditional NodeElse.  We march
    # through them one by one, and stop once a branch evaluates to
    # true.
    for n in node.children:
      n.evalNode(s)
      if unpack[bool](n.value):
        return
  of NodeConditional:
    # First, evaluate the conditional expression.  If it's true, then
    # go ahead and run the body. We don't want to execute branches
    # where the conditional evaluates to false.
    node.children[0].evalNode(s)
    node.value = node.children[0].value
    if unpack[bool](node.value):
      s.pushRuntimeFrame()
      try:
        node.children[1].evalNode(s)
      finally:
        discard s.popRuntimeFrame()
  of NodeFor:
    # This is pretty straightforward, other than the fact that we use
    # exceptions to implement `break` / `continue` per the above
    # comments.
    let
      scope = node.getVarScope()
      name = node.children[0].getTokenText()
    var
      incr, start, stop, i: int

    node.children[1].evalNode(s)
    node.children[2].evalNode(s)

    start = unpack[int](node.children[1].value)
    stop = unpack[int](node.children[2].value)

    if start < stop:
      incr = 1
      stop = stop - 1
    else:
      incr = -1
      start = start - 1

    i = start
    s.pushRuntimeFrame()
    while i != (stop + incr):
      s.runtimeVarSet(scope, name, pack(i))
      i = i + incr
      try:
        node.children[3].evalNode(s)
      except ValueError:
        case getCurrentExceptionMsg()
        of breakMsg:
          discard s.popRuntimeFrame()
          return
        of continueMsg:
          # continue breaks out of the current statement block,
          # not the overall for loop.
          continue
        else:
          raise
    discard s.popRuntimeFrame()
  of NodeUnary:
    # The only unary ops we support are + and -, and only on numerics,
    # so + actually is a noop.
    node.evalKids(s)

    let
      sign = node.getTokenText()
      bx = node.children[0].value

    if sign[0] != '-':
      node.value = bx
      return

    case node.children[0].typeinfo.kind
    of TypeInt:
      node.value = pack(-unpack[int](bx))
    of TypeFloat:
      node.value = pack(-unpack[float](bx))
    else:
      unreachable
  of NodeNot:
    node.evalKids(s)

    let bx = node.children[0].value

    node.value = pack(not unpack[bool](bx))
  of NodeMember:
    # Unreachable, because I haven't implemented it yet.
    unreachable
  of NodeIndex:
    # The node on the left's value will resolve to the object we need
    # to index.  We have to take action based on the type (and w/
    # dicts we only allow ints and strings as keys currently).
    #
    # Eventually, this will need to be two seprate items, one for
    # looking up values in an expression, and one for handling LHS
    # indexing (which resolves to a storage address, not a value).
    #
    # However, right now, we explicitly are not supporting indexing
    # on the LHS of an assignment.

    node.evalKids(s)
    let
      containerBox = node.children[0].value
      indexBox = node.children[1].value

    case node.children[0].getBaseType()
    of TypeTuple:
      let
        l = unpack[seq[Box]](containerBox)
        i = unpack[int](indexBox)
      if i >= l.len() or i < 0:
        fatal("Runtime error in config: array index out of bounds", node)
      node.value = l[i]
    of TypeList:
      let
        l = unpack[seq[Box]](containerBox)
        i = unpack[int](indexBox)

      if i >= l.len() or i < 0:
        fatal("Runtime error in config: array index out of bounds", node)
      node.value = l[i]
    of TypeDict:
      let
        kt = node.children[0].typeInfo.keyType
        containerBox = node.children[0].value
        indexBox = node.children[1].value
      case kt.kind
      of TypeInt:
        let
          d = unpack[Con4mDict[int, Box]](containerBox)
          i = unpack[int](indexBox)
        if not d.contains(i):
          fatal(fmt"Runtime error in config: dict key {i} not found.", node)
        node.value = d[i]
      of TypeString:
        let
          d = unpack[Con4mDict[string, Box]](containerBox)
          s = unpack[string](indexBox)

        if not d.contains(s):
          fatal("Runtime error in config: dict key {s} not found.".fmt(), node)
        node.value = d[s]
      else:
        unreachable
    else:
      unreachable

  of NodeCall:
    # We package up the arguments into a sequence, and then invoke
    # sCall; we checked for the function's existence already.

    node.children[1].evalNode(s)
    let
      fname = node.children[0].getTokenText()
      funcSig = node.children[1].typeInfo

    var args: seq[Box] = @[]

    for kid in node.children[1].children:
      args.add(kid.value)

    var ret = s.sCall(fname, args, funcSig, some(node.children[0]))
    if ret.isSome():
      node.value = ret.get()
  of NodeDictLit:
    node.evalKids(s)
    if node.typeInfo.kind == TypeTVar:
      node.value = pack(newCon4mDict[Box, Box]())
    else:
      case node.typeInfo.keyType.kind
      of TypeString, TypeInt, TypeBool, TypeFloat:
        var dict = newCon4mDict[Box, Box]()
        for kvpair in node.children:
          let
            boxedKey = kvpair.children[0].value
            boxedVal = kvpair.children[1].value
          dict[boxedKey] = boxedVal
          node.value = pack(dict)
      else:
        unreachable # Should already be restricted to primitive types
  of NodeKVPair:
    node.evalKids(s)
  of NodeListLit:
    node.evalKids(s)
    var l: seq[Box]
    for item in node.children:
      l.add(item.value)
    node.value = pack[seq[Box]](l)
  of NodeTupleLit:
    node.evalKids(s)
    var l: seq[Box]
    for kid in node.children:
      l.add(kid.value)
    node.value = pack[seq[Box]](l)
  of NodeOr:
    node.children[0].evalNode(s)
    node.value = node.children[0].value
    if not unpack[bool](node.value):
      node.children[1].evalNode(s)
      node.value = node.children[1].value
  of NodeAnd:
    node.children[0].evalNode(s)
    node.value = node.children[0].value
    if unpack[bool](node.value):
      node.children[1].evalNode(s)
      node.value = node.children[1].value
  of NodeNe:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt: cmpWork(int, `!=`)
    of TypeFloat: cmpWork(float, `!=`)
    of TypeBool: cmpWork(bool, `!=`)
    of TypeString: cmpWork(string, `!=`)
    else: unreachable
  of NodeCmp:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt: cmpWork(int, `==`)
    of TypeFloat: cmpWork(float, `==`)
    of TypeBool: cmpWork(bool, `==`)
    of TypeString: cmpWork(string, `==`)
    else:
      unreachable
  of NodeGte:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt: cmpWork(int, `>=`)
    of TypeFloat: cmpWork(float, `>=`)
    of TypeString: cmpWork(string, `>=`)
    else: unreachable
  of NodeLte:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt: cmpWork(int, `<=`)
    of TypeFloat: cmpWork(float, `<=`)
    of TypeString: cmpWork(string, `<=`)
    else: unreachable
  of NodeGt:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt: cmpWork(int, `>`)
    of TypeFloat: cmpWork(float, `>`)
    of TypeString: cmpWork(string, `>`)
    else: unreachable
  of NodeLt:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt: cmpWork(int, `<`)
    of TypeFloat: cmpWork(float, `<`)
    of TypeString: cmpWork(string, `<`)
    else: unreachable
  of NodePlus:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt: binaryOpWork(int, int, `+`)
    of TypeFloat: binaryOpWork(float, float, `+`)
    of TypeString: binaryOpWork(string, string, `&`)
    else: unreachable
  of NodeMinus:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt: binaryOpWork(int, int, `-`)
    of TypeFloat: binaryOpWork(float, float, `-`)
    else: unreachable
  of NodeMod:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt: binaryOpWork(int, int, `mod`)
    else: unreachable
  of NodeMul:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt: binaryOpWork(int, int, `*`)
    of TypeFloat: binaryOpWork(float, float, `*`)
    else: unreachable
  of NodeDiv:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt: binaryOpWork(int, float, `/`)
    of TypeFloat: binaryOpWork(float, float, `/`)
    else: unreachable
  of NodeIdentifier:
    let
      name = node.getTokenText()
      scopes = node.getBothScopes()
      attrEntry = scopes.attrs.lookup(name)
    try:
      if attrEntry.isSome():
        node.value = attrEntry.get().value.get()
      else:
        node.value = s.runtimeVarLookup(name, scopes.vars)
    except:
      fatal("Variable was referenced before assignment", node)

template evalTreeBase(node: untyped, param: untyped): untyped =
  let state = param
  
  if node == nil:
    return

  state.pushRuntimeFrame()
  try:
    node.evalNode(state)
  finally:
    discard state.popRuntimeFrame()

  return some(state)
  
proc evalTree*(node:         Con4mNode,
               addBuiltins = false): Option[ConfigState] {.inline.} =
  ## This runs the evaluator on a tree that has already been parsed
  ## and type-checked.
  evalTreeBase(node):
      node.checkTree(addBuiltins)

proc evalTree*(node:    Con4mNode,
               fns:     openarray[(string, BuiltinFn, string)] = [],
               exclude: openarray[int] = []): Option[ConfigState] {.inline.} =
  ## This is the same as above, but always has checkTree() add the
  ## default builtins, minus explicitly excluded ones, and
  ## additionally allows for installing custom ones.
  evalTreeBase(node):
      node.checkTree(fns, exclude)
               
proc evalConfig*(filename: string,
                 addBuiltins = false): Option[(ConfigState, Con4mScope)] =
  ## Given the config file as a string, this will load and parse the
  ## file, then execute it, returning both the state object created,
  ## as well as the top-level symbol table for attributes, both
  ## assuming the operation was successful.

  let
    tree = parse(filename)
    confOpt = tree.evalTree(addBuiltins)

  if confOpt.isSome():
    let
      state = confOpt.get()
      scopes = tree.scopes.get()

    return some((state, scopes.attrs))

proc evalFunc(s: ConfigState, args: seq[Box], node: Con4mNode): Option[Box] =

  if args.len() != node.children[1].children.len():
    raise newException(Con4mError, "Incorrect number of arugments")

  let
    scope = node.getVarScope()
    savedFrames = s.frames

  s.frames = @[]
  s.pushRuntimeFrame()

  for i, idNode in node.children[1].children:
    let name = idNode.getTokenText()
    s.runtimeVarSet(scope, name, args[i])

  try:
    node.children[2].evalNode(s)
  except Con4mError:
    discard # Clean return

  let frame = s.popRuntimeFrame()

  s.frames = savedFrames

  if "result" in frame:
    node.value = frame["result"]
    return some(node.value)
  else:
    return none(Box)

