import math
import options
import strformat
import strutils

import con4m_types
import st
import box
import typerepr
import typecheck
import builtins

proc checkNode(node: Con4mNode)

proc checkKids(node: Con4mNode) {.inline.} =
  for item in node.children:
    item.checkNode()

template typeError(msg: string) =
  raise newException(ValueError, msg)

template parseError(msg: string) =
  raise newException(ValueError, msg)

template pushVarScope() =
  let scopeOpt = node.scopes
  var scopes = scopeOpt.get()

  scopes.vars = Con4mScope(parent: some(scopes.vars))
  node.scopes = some(scopes)

proc getVarScope*(node: Con4mNode): Con4mScope =
  let scopes = node.scopes.get()
  return scopes.vars

proc getAttrScope*(node: Con4mNode): Con4mScope =
  let scopes = node.scopes.get()
  return scopes.attrs

proc getBothScopes*(node: Con4mNode): CurScopes =
  return node.scopes.get()

proc getTokenText*(node: Con4mNode): string {.inline.} =
  let token = node.token.get()

  return $(token)

proc getTokenType(node: Con4mNode): Con4mTokenKind {.inline.} =
  let token = node.token.get()

  return token.kind

proc getLineNo(node: Con4mNode): int {.inline.} =
  let token = node.token.get()

  return token.lineNo

proc checkNode(node: Con4mNode) =
  if node.scopes.isNone():
    node.scopes = node.parent.get().scopes

  case node.kind
  of NodeBody:
    node.checkKids()
  of NodeBreak, NodeContinue:
    discard
  of NodeSection:
    var scopes = node.scopes.get()
    var scope = scopes.attrs

    for kid in node.children[0 ..< ^1]:
      let
        secname = kid.getTokenText()
        maybeEntry = scope.lookup(secname, scopeOk=true)
      if maybeEntry.isSome():
        let entry = maybeEntry.get()
        if not entry.subscope.isSome():
          parseError("Section declaration conflicted with a variable from another section")
        scope = entry.subscope.get()
      else:
        let
          loc = node.getLineNo()
          entry = scope.addEntry(secname, loc, subscope = true)
        scope = entry.get().subscope.get()
          
    scopes.attrs = scope
    node.scopes = some(scopes)

    node.children[^1].checkNode()
  of NodeAttrAssign:
    if node.children[0].kind != NodeIdentifier:
      parseError("Dot assignments for attributes currently not supported.")

    node.children[1].checkNode()
    let
      name = node.children[0].getTokenText()
      loc = node.getLineNo()
      scopes = node.getBothScopes()
      tinfo = node.children[1].typeinfo

    if scopes.vars.lookup(name).isSome():
      parseError("Attribute {name} conflicts with existing user variable".fmt())

    let existing = scopes.attrs.lookup(name)

    if not existing.isSome():
      discard scopes.attrs.addEntry(name, loc, tinfo)
    else:
      let
        entry = existing.get()
        t = unify(tinfo, entry.tinfo)
      if t.isBottom():
        typeError("Type of value assigned conflicts with the exiting type")
      else:
        entry.tinfo = t
        entry.defLocs.add(loc)

  of NodeVarAssign:
    if node.children[0].kind != NodeIdentifier:
      parseError("Dot assignments for variables currently not supported.")

    node.children[1].checkNode()
    let
      name = node.children[0].getTokenText()
      loc = node.getLineNo()
      scopes = node.getBothScopes()
      tinfo = node.children[1].typeinfo

    if scopes.attrs.lookup(name).isSome():
      parseError("Variable {name} conflicts with existing attribute".fmt())

    let existing = scopes.vars.lookup(name)

    if not existing.isSome():
      discard scopes.vars.addEntry(name, loc, tinfo)
    else:
      let
        entry = existing.get()
        t = unify(tinfo, entry.tinfo)

      if entry.locked:
        typeError("Cannot assign to loop index variables.")
        
      if t.isBottom():
        typeError("Type of value assigned conflicts with the exiting type")
      else:
        entry.tinfo = t
        entry.defLocs.add(loc)
  of NodeIfStmt, NodeElse:
    pushVarScope()
    node.checkKids()
  of NodeConditional:
    pushVarScope()
    node.checkKids()
    if isBottom(node.children[0], boolType):
      typeError("If conditional must evaluate to a boolean")
    node.typeInfo = boolType # Unneeded.
  of NodeFor:
    pushVarScope()
    let
      scope = node.getVarScope()
      name = node.children[0].getTokenText()
      loc = node.getLineNo()
      entry = scope.addEntry(name, loc, intType).get()

    entry.locked = true
    
    for i in 1 ..< node.children.len():
      node.children[i].checkNode()
  of NodeSimpLit:
    case node.getTokenType()
    of TTStringLit:
      node.typeInfo = stringType
      var s = node.getTokenText()
      node.value = box(s)
    of TTIntLit:
      node.typeInfo = intType
      try:
        node.value = box(node.getTokenText().parseInt())
      except:
        parseError("Number too large for int type.")
    of TTFloatLit:
      node.typeInfo = floatType

      let
        txt = node.getTokenText()
        dotLoc = txt.find('.')
      var
        value: float
        eLoc = txt.find('e')
        intPartS: string
        intPartI: int
        floatPartS: string
        expPartS: string
        expPartI: int = 1
        eSignIsMinus: bool

      if eLoc == -1:
        eLoc = txt.find('E')

      if dotLoc != -1:
        intPartS = txt[0 ..< dotLoc]
        if eLoc != -1:
          floatPartS = txt[dotLoc+1 ..< eLoc]
        else:
          floatPartS = txt[dotLoc+1 .. ^1]
      else:
        intPartS = txt[0 ..< eLoc]

      if eLoc != -1:
        eLoc = eLoc + 1
        case txt[eLoc]
        of '+':
          eLoc = eLoc + 1
        of '-':
          eLoc = eLoc + 1
          eSignIsMinus = true
        else:
          discard
        expPartS = txt[eLoc .. ^1]

      try:
        intPartI = intPartS.parseInt()
      except:
        parseError("Integer piece is too large for an int; use 'e' notation.")

      if expPartS != "":
        try:
          expPartI = expPartS.parseInt()
        except:
          parseError("Exponent piece of float overflows integer type")

      # Fow now, we just truncate floating point digits if there are
      # $(high(int)).len() digits or more.

      if floatPartS != "":
        const
          maxAsString = $(high(int))
          maxLen = maxAsString.len()

        if floatPartS.len() >= maxLen:
          floatPartS = floatPartS[0 ..< maxLen]

        value = floatPartS.parseInt() / (10 ^ floatPartS.len())

      value = value + float(intPartI)
      value = value * pow(10.0, float(expPartI))

      node.value = box(value)
    of TtTrue:
      node.typeInfo = boolType
      node.value = box(true)
    of TtFalse:
      node.typeInfo = boolType
      node.value = box(false)
    of TtNull:
      node.typeInfo = newTypeVar()
    else:
      unreachable
  of NodeUnary:
    node.checkKids()
    let t = node.children[0].typeInfo
    case t.kind
    of TypeInt, TypeFloat: node.typeInfo = t
    else:
      typeError("Unary operator only works on numeric types")
  of NodeNot:
    node.checkKids()
    if node.children[0].isBottom():
      typeError("There's nothing to 'not' here")
    node.typeInfo = boolType
  of NodeMember:
    node.checkKids()
    typeError("Member access is currently not supported.")
  of NodeIndex:
    node.checkKids()
    case node.children[0].typeInfo.kind
    of TypeList:
      if isBottom(node.children[1], intType):
        parseError("Invalid list index (numbers only)")
      node.typeInfo = node.children[0].typeInfo.itemType
    of TypeDict:
      let
        kt = node.children[0].typeInfo.keyType
        vt = node.children[0].typeInfo.valType
      case kt.kind
      of TypeString, TypeInt:
        node.typeInfo = vt
      else:
        parseError("Dict indicies can only be strings or ints")
    else:
      echo $node
      parseError("Currently only support indexing on dicts and lists")
  of NodeCall:
    node.children[1].checkNode()

    if node.children[0].kind != NodeIdentifier:
      parseError("Data objects cannot currently have methods.")

    let
      fname = node.children[0].getTokenText()
      builtinOpt = getBuiltinBySig(fname, node.children[1].typeInfo)

    if builtinOpt.isNone():
      if fname.isBuiltin():
        typeError("No signature with that type found.")
      else:
        typeError("Function {fname} doesn't exist".fmt())

    # Could stash for when we execute, but currently will just
    # look it up on the execute pass.
    let builtin = builtinOpt.get()
    node.typeInfo = builtin.tInfo.retType
  of NodeActuals:
    var t: seq[Con4mType]
    node.checkKids()

    for item in node.children:
      t.add(item.typeInfo)

    node.typeInfo = newProcType(t, newTypeVar())
  of NodeDictLit:
    node.checkKids()
    var ct = newTypeVar()
    for item in node.children:
      ct = unify(ct, item.typeinfo)
      if ct.isBottom():
        typeError("Dict items must all be the same type.")
    node.typeInfo = ct
  of NodeKVPair:
    node.checkKids()
    if node.children[0].isBottom() or node.children[1].isBottom():
      typeError("Invalid type for dictionary keypair")
    node.typeInfo = newDictType(node.children[0].typeInfo,
                                node.children[1].typeInfo)
  of NodeListLit:
    node.checkKids()
    var ct = newTypeVar()
    for item in node.children:
      ct = unify(ct, item.typeinfo)
      if ct.isBottom():
        typeError("List items must all be the same type")
    node.typeInfo = newListType(ct)
  of NodeOr, NodeAnd:
    node.checkKids()
    if isBottom(node.children[0], boolType) or
       isBottom(node.children[1], boolType):
      typeError("Each side of || and && expressions must eval to a bool")
    node.typeinfo = boolType
  of NodeNe, NodeCmp:
    node.checkKids()
    let t = unify(node.children[0], node.children[1])

    case t.kind
    of TypeInt, TypeFloat, TypeString, TypeBool:
      node.typeinfo = boolType
    of TypeBottom:
      typeError("Types to comparison operators must match")
    else:
      typeError("== and != currently do not work on lists or dicts")
  of NodeGte, NodeLte, NodeGt, NodeLt:
    node.checkKids()
    let t = unify(node.children[0], node.children[1])

    case t.kind
    of TypeInt, TypeFloat, TypeString:
      node.typeinfo = boolType
    of TypeBottom:
      typeError("Types to comparison operators must match")
    else:
      typeError("Comparison ops only work on numbers and strings")
  of NodePlus:
    node.checkKids()
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      typeError("Types of left and right side of binary operators must match")
    of TypeInt, TypeFloat, TypeString:
      node.typeInfo = t
    else:
      typeError("Invalid type for bianry operator")
  of NodeMinus, NodeMul:
    node.checkKids()
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      typeError("Types of left and right side of binary operators must match")
    of TypeInt, TypeFloat:
      node.typeInfo = t
    else:
      typeError("Invalid type for bianry operator")
  of NodeMod:
    node.checkKids()
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      typeError("Types of left and right side of binary operators must match")
    of TypeInt:
      node.typeInfo = t
    else:
      typeError("Invalid type for bianry operator")
  of NodeDiv:
    node.checkKids()
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      typeError("Types of left and right side of binary operators must match")
    of TypeInt, TypeFloat:
      node.typeInfo = floatType
    else:
      typeError("Invalid type for bianry operator")
  of NodeIdentifier:
    # This gets used in cases where the symbol is looked up in the
    # current scope, not when it has to be in a specific scope (i.e.,
    # member access).
    let
      scopes = node.getBothScopes()
      name = node.getTokenText()
      attrEntry = scopes.attrs.lookup(name)
      varEntry = scopes.vars.lookup(name)

    if attrEntry.isNone() and varEntry.isNone():
      parseError("Variable {name} used before definition".fmt())

    let ent = if attrEntry.isSome(): attrEntry.get() else: varEntry.get()

    node.typeInfo = ent.tinfo

proc checkTree*(node: Con4mNode) =
  node.scopes = some(newRootScope())
  for item in node.children:
    item.checkNode()

when isMainModule:
  proc tT(s: string): Con4mType =
    return s.toCon4mType()

  let t = newProcType(@[newListType(stringType),
                        newDictType(boolType, floatType),
                        intType],
                      bottomType, true)
  echo $(t)

  echo $("[ string ]".toCon4mType())
  echo $("[[string ]]".toCon4mType())
  echo $("{int : [[string]]}".toCon4mType())
  echo $("([string], { bool : float }, *int) -> `x".toCon4mType())
  echo $("[ string ]".tT().unify("`x".tT()))
  echo $("(int, int, int, *string)".tT())
  echo $"(int, int, int, *string) -> `x".tT().unify(
      "(int, int, int, *string)".tT())
  echo $"(int, int, int, string) -> `x".tT().unify("(int, int, int, string)".tT())
  echo $"(int, int, int, *string) -> `x".tT().unify(
      "(int, int, int, string, string)".tT())
  echo $"(int, int, int, *bool) -> `x".tT().unify("(int, int, int)".tT())
  echo $"(int, int, int, *`t) -> `x".tT().unify("(int, int, int, float)".tT())
  echo $"(int, int, int, *`t) -> `x".tT().unify("(int, int, int)".tT())
