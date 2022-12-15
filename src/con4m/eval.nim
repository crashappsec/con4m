##  This module actually walks the tree, directly executing the
##  operations encoded in it (as opposed to generating code that we
##  would then run).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import ./types
import st
import builtins
import box
import parse
import treecheck

import options
import tables
import strformat

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]: off.}

const breakMsg = "b"
const continueMsg = "c"

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
  of NodeBody, NodeElse, NodeActuals:
    node.evalKids(s)
    node.value = box(false)
  of NodeSimpLit, NodeEnum:
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
    # Calls are always to built-in functions, at least for now.  So
    # all we need to is package up the arguments into a sequence, and
    # then call; we checked for the function's existence already.

    node.children[1].evalNode(s)
    let
      fname = node.children[0].getTokenText()
      funcSig = node.children[1].typeInfo

    var args: seq[Box]

    for kid in node.children[1].children:
      args.add(kid.value)

    let ret = s.sCall(fname, args, funcSig, node.children[0])

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

