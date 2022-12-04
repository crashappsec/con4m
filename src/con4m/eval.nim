import con4m_types
import st
import builtins
import box
import parse
import treecheck

import options
import tables

when (NimMajor, NimMinor) >= (1, 7):
  {.warning[CastSizes]:off.}

const breakMsg = "b"
const continueMsg = "c"

proc evalNode(node: Con4mNode, s: ConfigState)

proc evalKids(node: Con4mNode, s: ConfigState) {.inline.} =
  for item in node.children:
    item.evalNode(s)

template cmpWork(typeWeAreComparing: typedesc, op: untyped) =
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
  let
    v1 = unbox[typeWeAreOping](node.children[0].value)
    v2 = unbox[typeWeAreOping](node.children[1].value)
    
  var ret: returnType = cast[returnType](op(v1, v2))

  node.value = box(ret)

proc modFunc(a, b: int): int {.inline.} =
  a mod b

proc evalNode(node: Con4mNode, s: ConfigState) =
  case node.kind
  of NodeBody, NodeElse, NodeActuals:
    node.evalKids(s)
  of NodeSimpLit, NodeEnum: # Values were all assigned when we checked the tree.
    return
  of NodeBreak:
    raise newException(ValueError, breakMsg)
  of NodeContinue:
    raise newException(ValueError, continueMsg)
  of NodeSection:
    node.children[^1].evalNode(s)
  of NodeAttrAssign:
    node.children[1].evalNode(s)
    let
      name = node.children[0].getTokenText()
      scope = node.getAttrScope()
      entry = scope.lookup(name).get()

    if entry.locked:
      raise newException(ValueError, "Attempt to assign to a read-only field")

    node.value = node.children[1].value
    entry.value = some(node.value)
    
  of NodeVarAssign:
    node.children[1].evalNode(s)
    let
      name = node.children[0].getTokenText()
      scope = node.getVarScope()
      entry = scope.lookup(name).get()

    node.value = node.children[1].value
    entry.value = some(node.value)
  of NodeIfStmt:
    for n in node.children:
      n.evalNode(s)
      if unbox[bool](n.value):
        return
  of NodeConditional:
    node.evalKids(s)
    node.value = node.children[0].value
  of NodeFor:
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
    unreachable
  of NodeIndex:
    node.evalKids(s)
    let
      containerBox = node.children[0].value
      indexBox = node.children[1].value

    case node.children[0].typeInfo.kind
    of TypeList:
      let
        l = unbox[seq[Box]](containerBox)
        i = unbox[int](indexBox)

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
        node.value = d[i]
      of TypeString:
        let
          d = unbox[TableRef[string, Box]](containerBox)
          i = unbox[string](indexBox)
        node.value = d[i]
      else:
        unreachable
    else:
      unreachable

  of NodeCall:
    node.children[1].evalNode(s)
    let
      fname = node.children[0].getTokenText()
      funcSig = node.children[1].typeInfo

    var args: seq[Box]

    for kid in node.children[1].children:
      args.add(kid.value)

    let ret = s.sCall(fname, args, funcSig)

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

    node.value = box[Box](l)
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
    of TypeInt: binaryOpWork(int, int, modFunc)
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
      ent = if attrEntry.isSome(): attrEntry.get() else: varEntry.get()
      optVal = ent.value

    node.value = optVal.get()

proc evalTree*(node: Con4mNode, s: ConfigState) {.inline.} =
  node.evalNode(s)

proc evalConfig*(filename: string): (ConfigState, Option[Con4mScope]) =
  let tree = parse(filename)

  if tree == nil:
    return

  let state = tree.checkTree()
  tree.evalTree(state)

  let scopes = tree.scopes.get()
  
  return (state, some(scopes.attrs))

