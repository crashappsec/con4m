##  This module actually walks the tree, directly executing the
##  operations encoded in it (as opposed to generating code that we
##  would then run).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options, tables, strformat, dollars
import types, st, parse, treecheck, typecheck, nimutils, errmsg

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

const breakMsg    = "b"
const continueMsg = "c"

proc evalNode*(node: Con4mNode, s: ConfigState)

# Right now, we are not properly generating code, we are just evaluating
# directly out of the tree. We still need a stack for execution, which
# is the first parameter, here.
#
# Were we generating code, we would have generated offsets from
# the start of the stack frame for the stack variables we reference.
# However, we have done no such thing, and we need to get the 'right'
# version of masked variables (it can happen in conform, such as with
# loop index variables).
#
# Two obvious strategies for dealing with this:
# 1) When we push a scope, we can pre-load the runtime values with
#    default (zero'd out) values for any new declarations the
#    symbol table shows.
#
# 2) We can consult the symbol table as we look up variables.
#
# Either of these is fine, and the default value is not even very
# important, since we are statically checking for def before use.
# Still, we take a belt-and-suspenders approach, for the inevitable
# compiler implementation bugs, so we do runtime checking, and
# make the symbol table hold options to make life easy.
#
# In the first version of con4m we went with door #2. Now
# that we're redoing the symbol table, I'm going w/ door #1.
#
# I'm sure before too long we'll be generating at least some code for
# a dumb stack machine, so all of this will be moot.

proc pushRuntimeFrame*(s: ConfigState, n: Con4mNode) {.inline.} =
  var newFrame = RuntimeFrame()

  for k, sym in n.varScope.contents:
    newFrame[k] = sym.value

  s.frames.add(newFrame)

proc popRuntimeFrame*(s: ConfigState): RuntimeFrame {.inline.} =
  return s.frames.pop()

proc runtimeVarLookup(s: ConfigState, name: string): Box {.inline.} =
  return runtimeVarLookup(s.frames, name)

proc getFuncBySig*(s:    ConfigState,
                   name: string,
                   t:    Con4mType): Option[FuncTableEntry] =
  let candidates = s.findMatchingProcs(name, t)

  if len(candidates) == 1:
    return some(candidates[0])

proc evalFunc(s: ConfigState, args: seq[Box], node: Con4mNode): Option[Box] =
  if args.len() != node.children[1].children.len():
    raise newException(Con4mError, "Incorrect number of arugments")

  let savedFrames = s.frames

  if len(savedFrames) > 0:
    s.frames = @[savedFrames[0]]
  else:
    var newFrame = RuntimeFrame()
    s.frames     = @[newFrame]
    for k, v in s.keptGlobals:
      newFrame[k] = v.value


  s.pushRuntimeFrame(node)

  for i, idNode in node.children[1].children:
    let name = idNode.getTokenText()
    s.runtimeVarSet(name, args[i])

  try:
    node.children[2].evalNode(s)
  except Con4mError:
    if getCurrentExceptionMsg() != "return":
      raise
  if len(s.frames) > 0:
    for k, v in s.frames[0]:
      if k in s.keptGlobals:
        s.keptGlobals[k].value = v

  try:
    result = some(runtimeVarLookup(s, "result"))
  except:
    result = none(Box)

  s.frames = savedFrames

proc sCallUserDef*(s:        ConfigState,
                   name:     string,
                   a1:       seq[Box],
                   callback: bool,
                   nodeOpt:  Option[Con4mNode]): Option[Box] =

  try:
    if nodeOpt.isNone():
      if callback:
        return # Indicates the callback wasn't provided by the user.
        # user function, no implementation, wouldn't pass the checker.
      unreachable
    return s.evalFunc(a1, nodeOpt.get())
  except ValueError:
    let msg = getCurrentExceptionMsg()
    if getCon4mVerbosity() == c4vMax:
      echo getCurrentException().getStackTrace()
    fatal(fmt"Unhandled error when running builtin call '{name}': {msg}",
          nodeOpt.get())

proc sCallBuiltin(s:     ConfigState,
                  name:  string,
                  a1:    seq[Box],
                  fInfo: FuncTableEntry,
                  node:  Con4mNode): Option[Box] =
  s.nodeStash = node
  try:
    return fInfo.builtin(a1, s)
  except Con4mError:
    fatal(getCurrentExceptionMsg(), node)
  except:
    fatal("Unhandled error when running builtin call: " & name & "\n" &
          "error is: " & getCurrentExceptionMsg(), node)


proc sCall*(s:       ConfigState,
            fInfo:   FuncTableEntry,
            a1:      seq[Box],
            node:    Con4mNode): Option[Box] =
  ## This is not really meant to be exposed outside this module,
  ## except to the evaluator.  This runs a builtin call, callback or
  ## user-defined function.
  ##
  ## The node parameter will be the node from the caller's scope.
  case fInfo.kind
  of FnBuiltIn:
    return s.sCallBuiltin(fInfo.name, a1, fInfo, node)
  else:
    return s.sCallUserDef(fInfo.name, a1, true, fInfo.impl)

proc sCall*(s:       ConfigState,
            name:    string,
            a1:      seq[Box],
            tinfo:   Con4mType,
            nodeOpt: Option[Con4mNode] = none(Con4mNode)): Option[Box] =

  let optFunc = s.getFuncBySig(name, tinfo)

  if not optFunc.isSome():
    if tinfo.kind == TypeFunc and tinfo.retType != bottomType:
      return none(Box)
    else:
      fatal(fmt"Function '{reprSig(name, tinfo)}' not found")

  return s.sCall(optFunc.get(), a1, nodeOpt.getOrElse(nil))

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
                      returnType:     typedesc,
                      op:             untyped) =
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
  # These are explicit just to make sure I don't end up w/ implementation
  # errors that baffle me.
  of NodeFuncDef, NodeType, NodeVarDecl, NodeExportDecl, NodeVarSymNames,
       NodeCallbackLit:
    return # Nothing to do, everything was done in the check phase.
  of NodeReturn:
    if node.children.len() != 0:
      node.children[0].evalNode(s)
      s.runtimeVarSet("result", node.children[0].value)
    raise newException(Con4mError, "return")
  of NodeActuals, NodeBody:
    node.evalKids(s)
    node.value = pack(false)
  of NodeElse:
    s.pushRuntimeFrame(node)
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
    s.pushRuntimeFrame(node)
    try:
      node.children[^1].evalNode(s)
    finally:
      discard s.popRuntimeFrame()
  of NodeAttrAssign, NodeAttrSetLock:
    # We already created scope objects in the tree checking phase,
    # and cached a reference to the attribute location where any
    # result should be stored.
    #
    # So we just have to update the RHS and then assign.
    node.children[1].evalNode(s)
    let err = attrSet(node.attrRef, node.children[1].value)

    case err.code
    of errCantSet:
      when defined(errOnOverride):
        fatal(err.msg, node)
    of errOk:
      if node.kind == NodeAttrSetLock:
        node.attrRef.locked = true
    else:
      unreachable

  of NodeVarAssign:
    node.children[1].evalNode(s)

    let name  = node.children[0].getTokenText()

    s.runtimeVarSet(name, node.children[1].value)

  of NodeUnpack:
    node.children[^1].evalNode(s)
    let
      boxedTup = node.children[^1].value
      tup = unpack[seq[Box]](boxedTup)

    # Each item is still a Box; we did not unbox all layers, only one.
    for i, item in tup:
      let name = node.children[i].getTokenText()
      s.runtimeVarSet(name, item)

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
      s.pushRuntimeFrame(node)
      try:
        node.children[1].evalNode(s)
      finally:
        discard s.popRuntimeFrame()
  of NodeFor:
    # This is pretty straightforward, other than the fact that we use
    # exceptions to implement `break` / `continue` per the above
    # comments.
    let name  = node.children[0].getTokenText()
    var incr, start, stop, i: int

    node.children[1].evalNode(s)
    node.children[2].evalNode(s)

    start = unpack[int](node.children[1].value)
    stop  = unpack[int](node.children[2].value)

    if start < stop:
      incr = 1
      stop = stop - 1
    else:
      incr = -1
      start = start - 1

    i = start
    s.pushRuntimeFrame(node)
    while i != (stop + incr):
      s.runtimeVarSet(name, pack(i))
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
      bx   = node.children[0].value
      typ  = node.children[0].getType()

    if sign[0] != '-':
      node.value = bx
      return

    case typ.kind
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
    if node.attrRef.value.isNone():
      fatal("Attribute used before it was set", node)

    node.value = node.attrRef.value.get()
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
      indexBox     = node.children[1].value
      containerTy  = node.children[0].getType()
      ixTy         = node.children[1].getType()

    case containerTy.kind
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
        echo node
        fatal("Runtime error in config: array index out of bounds", node)
      node.value = l[i]
    of TypeDict:
      let
        kt           = containerTy.keyType.resolveTypeVars()
        containerBox = node.children[0].value
        indexBox     = node.children[1].value
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
          fatal(fmt"Runtime error in config: dict key {s} not found.", node)
        node.value = d[s]
      else:
        unreachable
    else:
      unreachable

  of NodeCall:
    # We package up the arguments into a sequence, and then invoke
    # sCall; we checked for the function's existence already.
    node.children[1].evalNode(s)
    var
      args: seq[Box] = @[]

    for kid in node.children[1].children:
      args.add(kid.value)

    assert node.procRef != nil
    var ret = s.sCall(node.procRef, args, node)
    if ret.isSome():
      node.value = ret.get()

  of NodeDictLit:
    node.evalKids(s)
    let nodeType = node.getType()
    var dict     = newCon4mDict[Box, Box]()

    for kvpair in node.children:
      let
        boxedKey = kvpair.children[0].value
        boxedVal = kvpair.children[1].value
      dict[boxedKey] = boxedVal

    node.value = pack(dict)
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
    of TypeInt, TypeDuration, TypeSize: cmpWork(int, `!=`)
    of TypeFloat: cmpWork(float, `!=`)
    of TypeBool: cmpWork(bool, `!=`)
    of TypeString, TypeIPAddr, TypeCIDR, TypeDate, TypeTime, TypeDateTime:
      cmpWork(string, `!=`)
    else: unreachable
  of NodeCmp:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt, TypeDuration, TypeSize: cmpWork(int, `==`)
    of TypeFloat: cmpWork(float, `==`)
    of TypeBool: cmpWork(bool, `==`)
    of TypeString, TypeIPAddr, TypeCIDR, TypeDate, TypeTime, TypeDateTime:
      cmpWork(string, `==`)
    else:
      unreachable
  of NodeGte:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt, TypeDuration, TypeSize: cmpWork(int, `>=`)
    of TypeFloat: cmpWork(float, `>=`)
    of TypeString, TypeIPAddr, TypeDate, TypeTime, TypeDateTime:
      cmpWork(string, `>=`)
    else: unreachable
  of NodeLte:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt, TypeDuration, TypeSize: cmpWork(int, `<=`)
    of TypeFloat: cmpWork(float, `<=`)
    of TypeString, TypeIPAddr, TypeDate, TypeTime, TypeDateTime:
      cmpWork(string, `<=`)
    else: unreachable
  of NodeGt:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt, TypeDuration, TypeSize: cmpWork(int, `>`)
    of TypeFloat: cmpWork(float, `>`)
    of TypeString, TypeIPAddr, TypeDate, TypeTime, TypeDateTime:
      cmpWork(string, `>`)
    else: unreachable
  of NodeLt:
    node.evalKids(s)
    case node.children[0].getBaseType()
    of TypeInt, TypeDuration, TypeSize: cmpWork(int, `<`)
    of TypeFloat: cmpWork(float, `<`)
    of TypeString, TypeIPAddr, TypeDate, TypeTime, TypeDateTime:
      cmpWork(string, `<`)
    else: unreachable
  of NodePlus:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt, TypeDuration: binaryOpWork(int, int, `+`)
    of TypeFloat: binaryOpWork(float, float, `+`)
    of TypeString: binaryOpWork(string, string, `&`)
    else: unreachable
  of NodeMinus:
    node.evalKids(s)
    case node.getBaseType()
    of TypeInt, TypeDuration: binaryOpWork(int, int, `-`)
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
    if node.attrRef != nil:
      if node.attrRef.value.isNone():
        fatal("Attribute {node.getTokenText()} referenced before assignment")
      else:
        node.value = node.attrRef.value.get()
    else:
      node.value = s.runtimeVarLookup(node.getTokenText())

proc runCallback*(s:     ConfigState,
                  name:  string,
                  args:  seq[Box],
                  tinfo: Option[Con4mType] = none(Con4mType)): Option[Box] =
  if tinfo.isSome():
    return s.sCall(name, args, tinfo.get())
  if not s.funcTable.contains(name):
    # User did not supply the callback.
    return none(Box)
  if len(s.funcTable[name]) > 0:
    raise newException(ValueError,
                       "When supporting callbacks with multiple signatures, " &
                       "you must supply the type when calling runCallback()")
  let impl = s.funcTable[name][0].impl
  return s.sCallUserDef(name, args, callback = true, nodeOpt = impl)

proc runCallback*(s:    ConfigState,
                  fptr: CallbackObj,
                  args: seq[Box]): Option[Box] =
  if fptr.tInfo == nil:
    return s.runCallback(fptr.name, args)
  else:
    return s.runCallback(fptr.name, args, some(fptr.tInfo))
