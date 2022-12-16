##  This module actually walks the tree, directly executing the
##  operations encoded in it (as opposed to generating code that we
##  would then run).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import ./types
import st
import box
import parse
import treecheck
import typecheck
import dollars

import options
import tables
import strformat

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

const breakMsg = "b"
const continueMsg = "c"

proc getFuncBySig(s: ConfigState,
                     name: string,
                     t: Con4mType): Option[FuncTableEntry] =
  if not s.funcTable.contains(name):
    return

  let candidates = s.funcTable[name]

  for item in candidates:
    if not isBottom(t, item.tinfo):
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
    let scopes = n.scopes.get()
    attrs = scopes.attrs
    vars = scopes.vars
  else:
    attrs = nil
    vars = nil

  try:
    return fInfo.builtin(a1, attrs, vars)
  except Con4mError:
    fatal(getCurrentExceptionMsg(), node)
  except:
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
    fatal(fmt"Function {name}{`$`(tinfo)[1 .. ^1]} not found")

  let fInfo = optFunc.get()


  case fInfo.kind
  of FnBuiltIn:
    return s.sCallBuiltin(name, a1, fInfo, nodeOpt.get())
  else:
    let callback = fInfo.kind == FnCallback
    return s.sCallUserDef(name, a1, callback, fInfo.impl)

proc evalNode*(node: Con4mNode, s: ConfigState)

proc evalKids(node: Con4mNode, s: ConfigState) {.inline.} =
  for item in node.children:
    item.evalNode(s)

template cmpWork(typeWeAreComparing: typedesc, op: untyped) =
  ## This just factors out repetitive code to apply standard
  ## comparison operators like >=.  We unbox, apply the operator, then
  ## box the result.
  let
    v1 = unbox[typeWeAreComparing](node.children[0].value)
    v2 = unbox[typeWeAreComparing](node.children[1].value)
  if op(v1, v2):
    node.value = box(true)
  else:
    node.value = box(false)

template binaryOpWork(typeWeAreOping: typedesc,
                      returnType: typedesc,
                      op: untyped) =
  ## Similar thing, but with binary operators like +, -, /, etc.
  let
    v1 = unbox[typeWeAreOping](node.children[0].value)
    v2 = unbox[typeWeAreOping](node.children[1].value)

  var ret: returnType = cast[returnType](op(v1, v2))

  node.value = box(ret)

proc evalNode*(node: Con4mNode, s: ConfigState) =
  ## This does the bulk of the work.  Typically, it will descend into
  ## the tree to evaluate, and then take whatever action is
  ## apporpriate after, if any.
  case node.kind
  of NodeFuncDef:
    # Exception handling here is just a mechanism for returns
    # to break linear control flow
    try:
      node.children[2].evalNode(s)
    except:
      return
  of NodeReturn:
    if node.children.len() != 0:
      node.children[0].evalNode(s)
      let
        scope = node.getVarScope()
        entry = scope.getEntry("result").get()
      entry.value = some(node.children[0].value)
  of NodeBody, NodeElse, NodeActuals:
    node.evalKids(s)
    node.value = box(false)
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
    node.children[^1].evalNode(s)
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
      entry = scope.lookup(name).get()

    entry.value = some(node.children[1].value)

  of NodeUnpack:
    node.children[^1].evalNode(s)
    let
      boxedTup = node.children[^1].value
      tup = unboxList[Box](boxedTup)
      scope = node.getVarScope()

    for i, item in tup:
      let
        name = node.children[i].getTokenText()
        entry = scope.lookup(name).get()

      entry.value = some(item)

  of NodeIfStmt:
    # This is the "top-level" node in an IF statement.  The nodes
    # below it will all be of kind NodeConditional NodeElse.  We march
    # through them one by one, and stop once a branch evaluates to
    # true.
    for n in node.children:
      n.evalNode(s)
      if unbox[bool](n.value):
        return
  of NodeConditional:
    # First, evaluate the conditional expression.  If it's true, then
    # go ahead and run the body. We don't want to execute branches
    # where the conditional evaluates to false.
    node.children[0].evalNode(s)
    node.value = node.children[0].value
    if unbox[bool](node.value):
      node.children[1].evalNode(s)
  of NodeFor:
    # This is pretty straightforward, other than the fact that we use
    # exceptions to implement `break` / `continue` per the above
    # comments.
    let
      scope = node.getVarScope()
      name = node.children[0].getTokenText()
    var
      entry = scope.lookup(name).get()
      incr, start, stop, i: int

    node.children[1].evalNode(s)
    node.children[2].evalNode(s)

    start = unbox[int](node.children[1].value)
    stop = unbox[int](node.children[2].value)
    i = start

    if start < stop:
      incr = 1
    else:
      incr = -1

    # Do this at runtime, just to ensure this works in all cases.
    # Particularly, we might serialize this info, and if it gets
    # editable, we still need this locked to prevent infinite loops.
    entry.locked = true
    while i != (stop + incr):
      entry.value = some(box(i))
      i = i + incr
      try:
        node.children[3].evalNode(s)
      except ValueError:
        case getCurrentExceptionMsg()
        of breakMsg:
          return
        of continueMsg:
          # continue breaks out of the current statement block,
          # not the overall for loop.
          continue
        else:
          raise
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
      node.value = box(-unbox[int](bx))
    of TypeFloat:
      node.value = box(-unbox[float](bx))
    else:
      unreachable
  of NodeNot:
    node.evalKids(s)

    let bx = node.children[0].value

    node.value = box(not unbox[bool](bx))
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

    case node.children[0].typeInfo.kind
    of TypeTuple:
      let
        l = unboxList[Box](containerBox)
        i = unbox[int](indexBox)
      if i >= l.len() or i < 0:
        fatal("Runtime error in config: array index out of bounds", node)
      node.value = l[i]
    of TypeList:
      let
        l = unboxList[Box](containerBox)
        i = unbox[int](indexBox)

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
          d = unbox[TableRef[int, Box]](containerBox)
          i = unbox[int](indexBox)

        if not d.contains(i):
          fatal("Runtime error in config: dict key {i} not found.".fmt(), node)
        node.value = d[i]
      of TypeString:
        let
          d = unbox[TableRef[string, Box]](containerBox)
          s = unbox[string](indexBox)

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

    var args: seq[Box]

    for kid in node.children[1].children:
      args.add(kid.value)

    let ret = s.sCall(fname, args, funcSig, some(node.children[0]))

    if ret.isSome():
      node.value = ret.get()
  of NodeDictLit:
    # 1. create a table ref here.
    # 2. Let each of the KVPair nodes do the insertion.
    if node.children[0].typeInfo.kind == TypeString:
      # Right now, we only allow string and int keys.
      # We could give boxes a hash, but we won't for now.
      var lit = newTable[string, Box]()
      node.value = boxDict[string, Box](lit)
    else:
      var lit = newTable[int, Box]()
      node.value = boxDict[int, Box](lit)

    node.evalKids(s)
  of NodeKVPair:
    node.evalKids(s)
    let
      boxedKey = node.children[0].value
      boxedValue = node.children[1].value
      boxedDict = node.parent.get().value

    if node.typeInfo.kind == TypeString:
      let
        dict = unbox[TableRef[string, Box]](boxedDict)
        k = unbox[string](boxedKey)
      dict[k] = boxedValue
    else:
      let
        dict = unbox[TableRef[int, Box]](boxedDict)
        k = unbox[int](boxedKey)
      dict[k] = boxedValue
  of NodeListLit:
    node.evalKids(s)
    var l: seq[Box]
    for item in node.children:
      l.add(item.value)
      node.value = boxList[Box](l)
  of NodeTupleLit:
    node.evalKids(s)
    var l: seq[Box]
    for kid in node.children:
      l.add(kid.value)
    node.value = boxList[Box](l)
  of NodeOr:
    node.children[0].evalNode(s)
    node.value = node.children[0].value
    if not unbox[bool](node.value):
      node.children[1].evalNode(s)
      node.value = node.children[1].value
  of NodeAnd:
    node.children[0].evalNode(s)
    node.value = node.children[0].value
    if unbox[bool](node.value):
      node.children[1].evalNode(s)
      node.value = node.children[1].value
  of NodeNe:
    node.evalKids(s)
    case node.children[0].typeInfo.kind
    of TypeInt: cmpWork(int, `!=`)
    of TypeFloat: cmpWork(float, `!=`)
    of TypeBool: cmpWork(bool, `!=`)
    of TypeString: cmpWork(string, `!=`)
    else: unreachable
  of NodeCmp:
    node.evalKids(s)
    case node.children[0].typeInfo.kind
    of TypeInt: cmpWork(int, `==`)
    of TypeFloat: cmpWork(float, `==`)
    of TypeBool: cmpWork(bool, `==`)
    of TypeString: cmpWork(string, `==`)
    else: unreachable
  of NodeGte:
    node.evalKids(s)
    case node.children[0].typeInfo.kind
    of TypeInt: cmpWork(int, `>=`)
    of TypeFloat: cmpWork(float, `>=`)
    of TypeString: cmpWork(string, `>=`)
    else: unreachable
  of NodeLte:
    node.evalKids(s)
    case node.children[0].typeInfo.kind
    of TypeInt: cmpWork(int, `<=`)
    of TypeFloat: cmpWork(float, `<=`)
    of TypeString: cmpWork(string, `<=`)
    else: unreachable
  of NodeGt:
    node.evalKids(s)
    case node.children[0].typeInfo.kind
    of TypeInt: cmpWork(int, `>`)
    of TypeFloat: cmpWork(float, `>`)
    of TypeString: cmpWork(string, `>`)
    else: unreachable
  of NodeLt:
    node.evalKids(s)
    case node.children[0].typeInfo.kind
    of TypeInt: cmpWork(int, `<`)
    of TypeFloat: cmpWork(float, `<`)
    of TypeString: cmpWork(string, `<`)
    else: unreachable
  of NodePlus:
    node.evalKids(s)
    case node.typeInfo.kind
    of TypeInt: binaryOpWork(int, int, `+`)
    of TypeFloat: binaryOpWork(float, float, `+`)
    of TypeString: binaryOpWork(string, string, `&`)
    else: unreachable
  of NodeMinus:
    node.evalKids(s)
    case node.typeInfo.kind
    of TypeInt: binaryOpWork(int, int, `-`)
    of TypeFloat: binaryOpWork(float, float, `-`)
    else: unreachable
  of NodeMod:
    node.evalKids(s)
    case node.typeInfo.kind
    of TypeInt: binaryOpWork(int, int, `mod`)
    else: unreachable
  of NodeMul:
    node.evalKids(s)
    case node.typeInfo.kind
    of TypeInt: binaryOpWork(int, int, `*`)
    of TypeFloat: binaryOpWork(float, float, `*`)
    else: unreachable
  of NodeDiv:
    node.evalKids(s)
    case node.typeInfo.kind
    of TypeInt: binaryOpWork(int, float, `/`)
    of TypeFloat: binaryOpWork(float, float, `/`)
    else: unreachable
  of NodeIdentifier:
    let
      scopes = node.getBothScopes()
      name = node.getTokenText()
      attrEntry = scopes.attrs.lookup(name)
      varEntry = scopes.vars.lookup(name)
    try:
      let
        ent = if attrEntry.isSome(): attrEntry.get() else: varEntry.get()
        optVal = ent.value
      node.value = optVal.get()
    except:
      fatal("Variable was referenced before assignment", node)


proc evalTree*(node: Con4mNode): Option[ConfigState] {.inline.} =
  ## This runs the evaluator on a tree that has already been parsed
  ## and type-checked.

  let state = node.checkTree()

  if node == nil:
    return

  node.evalNode(state)

  return some(state)

proc evalConfig*(filename: string): Option[(ConfigState, Con4mScope)] =

  ## Given the config file as a string, this will load and parse the
  ## file, then execute it, returning both the state object created,
  ## as well as the top-level symbol table for attributes, both
  ## assuming the operation was successful.

  let
    tree = parse(filename)
    confOpt = tree.evalTree()

  if confOpt.isSome():
    let
      state = confOpt.get()
      scopes = tree.scopes.get()

    return some((state, scopes.attrs))

proc evalFunc(s: ConfigState, args: seq[Box], node: Con4mNode): Option[Box] =
  if args.len() != node.children[1].children.len():
    raise newException(Con4mError, "Incorrect number of arugments")

  let scope = node.getVarScope()

  for i, idNode in node.children[1].children:
    let
      name = idNode.getTokenText()
      entry = scope.lookup(name).get()

    entry.value = some(args[i])

  node.evalNode(s)

  let entry = scope.lookup("result").get()
  return entry.value
