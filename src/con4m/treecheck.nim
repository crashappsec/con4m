## This module is doing three things:
## 
## 1) Type-checking the program, using the standard unifcation
##    algorithm, which itself lives in typecheck.nim
##
## 2) Putting symbol tables into each node, inserting variables (and
##    constants) into the proper symbol tables as it goes.
##
## 3) Setting values from string literals.  Dict/list literals are 
##    done at eval time.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import math
import options
import strformat
import strutils
import unicode
import streams

import con4m_types
import st
import box
import dollars
import typecheck
import builtins
import spec
import parse # just for fatal()

proc checkNode(node: Con4mNode, s: ConfigState)

proc checkKids(node: Con4mNode, s: ConfigState) {.inline.} =
  for item in node.children:
    item.checkNode(s)

template pushVarScope() =
  let scopeOpt = node.scopes
  var scopes = scopeOpt.get()

  scopes.vars = Con4mScope(parent: some(scopes.vars))
  node.scopes = some(scopes)

proc getVarScope*(node: Con4mNode): Con4mScope =
  ## Internal. Returns just the current variables scope, when we
  ## already know it exists.
  let scopes = node.scopes.get()
  return scopes.vars

proc getAttrScope*(node: Con4mNode): Con4mScope =
  ## Internal. Returns just the current attributes scope, when we
  ## already know it exists.
  let scopes = node.scopes.get()
  return scopes.attrs

proc getBothScopes*(node: Con4mNode): CurScopes =
  ## Internal. Returns both scopes when we know they exist.
  return node.scopes.get()

proc checkStringLit(node: Con4mNode) =
  # Note that we do NOT accept hex or octal escapes, since
  # strings theoretically should always be utf-8 only.
  let token = node.token.get()

  var
    flag: bool
    remaining: int
    codepoint: int
    raw: string = newStringOfCap(token.endPos - token.startPos)
    res: string = newStringOfCap(token.endPos - token.startPos)

  token.stream.setPosition(token.startPos)
  raw = token.stream.readStr(token.endPos - token.startPos)

  for r in raw.runes():
    if remaining > 0:
      codepoint = codepoint shl 4
      let o = ord(r)
      case o
      of int('0') .. int('9'):
        codepoint = codepoint and (o - int('0'))
      of int('a') .. int('f'):
        codepoint = codepoint and (o - int('a') + 10)
      of int('A') .. int('F'):
        codepoint = codepoint and (o - int('A') + 10)
      else:
        fatal("Invalid unicode escape in string literal", node)
      remaining -= 1
      if remaining == 0:
        res.add(Rune(codepoint))
        codepoint = 0
    elif flag:
      case r
      of Rune('n'):
        res.add('\n')
        flag = false
      of Rune('r'):
        res.add('\r')
        flag = false
      of Rune('a'):
        res.add('\a')
        flag = false
      of Rune('b'):
        res.add('\b')
        flag = false
      of Rune('f'):
        res.add('\f')
        flag = false
      of Rune('t'):
        res.add('\t')
        flag = false
      of Rune('\\'):
        res.add('\\')
        flag = false
      of Rune('u'):
        flag = false
        remaining = 4
      of Rune('U'):
        flag = false
        remaining = 8
      else:
        res.add(r)
    else:
      case r
      of Rune('\\'):
        flag = true
      else:
        res.add(r)

  if flag or (remaining != 0):
    fatal("Unterminated escape sequence in string literal", node)

  token.unescaped = res

proc getTokenText*(node: Con4mNode): string {.inline.} =
  ## This returns the raw string associated with a token.  Internal.
  let token = node.token.get()

  case token.kind
  of TtStringLit:
    return token.unescaped
  else:
    return $(token)

proc getTokenType(node: Con4mNode): Con4mTokenKind {.inline.} =
  let token = node.token.get()

  return token.kind

proc checkNode(node: Con4mNode, s: ConfigState) =
  # We take our scope from the parent by default.  If we're going to
  # have different scopes, the parent will tell us what our scope is.
  if node.scopes.isNone():
    node.scopes = node.parent.get().scopes

  case node.kind
  of NodeBody:
    node.checkKids(s)
  of NodeBreak, NodeContinue:
    discard
  of NodeEnum:
    let
      scopes = node.scopes.get()
      tinfo = intType
    for i, kid in node.children:
      let
        name = node.children[i].getTokenText()

      if scopes.attrs.lookupAttr(name).isSome():
        fatal("Enum Value {name} conflicts w/ a toplevel attr".fmt(), kid)
      if scopes.vars.lookup(name).isSome():
        fatal("Enum Value {name} conflicts w/ an existing variable".fmt(), kid)
      let entry = scopes.vars.addEntry(name, some(kid), tinfo).get()

      entry.value = some(box(i))
      entry.locked = true
      # Setting locked ensures that the user cannot assign to an enum;
      # it will pass the syntax check, but fail when we look at the
      # assignment.

  of NodeSection:
    var scopes = node.scopes.get()
    var scope = scopes.attrs
    var entry: STEntry
    var scopename = "<root>"

    for kid in node.children[0 ..< ^1]:
      if kid.kind == NodeSimpLit:
        kid.checkNode(s)
      let
        secname = kid.getTokenText()
        maybeEntry = scope.lookupAttr(secname, scopeOk = true)
      scopename = scopename & "." & secname
      if maybeEntry.isSome():
        entry = maybeEntry.get()
        if not entry.subscope.isSome():
          fatal("Section declaration conflicted with a variable " &
            "from another section", kid)
      else:
        entry = scope.addEntry(secname, some(kid), subscope = true).get()
        scope = entry.subscope.get()

      scope = entry.subscope.get()
      scopes.attrs = scope

    scopes.attrs = scope
    node.scopes = some(scopes)

    node.children[^1].checkNode(s)
  of NodeAttrAssign:
    if node.children[0].kind != NodeIdentifier:
      fatal("Dot assignments for attributes currently not supported.",
            node.children[0])

    node.children[1].checkNode(s)
    let
      name = node.children[0].getTokenText()
      scopes = node.getBothScopes()
      tinfo = node.children[1].typeinfo

    if scopes.vars.lookupAttr(name).isSome():
      fatal("Attribute {name} conflicts with existing user variable".fmt(),
            node.children[0]
      )

    let existing = scopes.attrs.lookupAttr(name)

    if not existing.isSome():
      discard scopes.attrs.addEntry(name, some(node), tinfo)
    else:
      let
        entry = existing.get()
        t = unify(tinfo, entry.tinfo)
      if t.isBottom():
        fatal("Type of value assigned conflicts with the exiting type",
              node.children[1])
      else:
        entry.tinfo = t

  of NodeVarAssign:
    if node.children[0].kind != NodeIdentifier:
      fatal("Dot assignments for variables currently not supported.",
            node.children[0])

    node.children[1].checkNode(s)
    let
      name = node.children[0].getTokenText()
      scopes = node.getBothScopes()
      tinfo = node.children[1].typeinfo

    if scopes.attrs.lookupAttr(name).isSome():
      fatal("Variable {name} conflicts with existing attribute".fmt(),
            node.children[0])

    let existing = scopes.vars.lookup(name)

    if not existing.isSome():
      discard scopes.vars.addEntry(name, some(node), tinfo)
    else:
      let
        entry = existing.get()
        t = unify(tinfo, entry.tinfo)

      if entry.locked:
        # Could be a loop index, enum, or something a previous
        # run locked.
        fatal("You cannot assign to this value.", node.children[0])

      if t.isBottom():
        fatal("Type of value assigned conflicts with the exiting type",
              node.children[1])
      else:
        entry.tinfo = t
  of NodeIfStmt, NodeElse:
    pushVarScope()
    node.checkKids(s)
  of NodeConditional:
    pushVarScope()
    node.checkKids(s)
    if isBottom(node.children[0], boolType):
      fatal("If conditional must evaluate to a boolean", node.children[0])
    node.typeInfo = boolType # Unneeded.
  of NodeFor:
    pushVarScope()
    let
      scope = node.getVarScope()
      name = node.children[0].getTokenText()
      entry = scope.addEntry(name, some(node.children[0]), intType).get()

    entry.locked = true

    for i in 1 ..< node.children.len():
      node.children[i].checkNode(s)
  of NodeSimpLit:
    case node.getTokenType()
    of TTStringLit:
      node.typeInfo = stringType
      node.checkStringLit()
      var s = node.getTokenText()
      node.value = box(s)
    of TTIntLit:
      node.typeInfo = intType
      try:
        node.value = box(node.getTokenText().parseInt())
      except:
        fatal("Number too large for int type.", node)
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
        fatal("Integer piece is too large for an int; use 'e' notation.", node)

      if expPartS != "":
        try:
          expPartI = expPartS.parseInt()
        except:
          fatal("Exponent piece of float overflows integer type", node)

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
    node.checkKids(s)
    let t = node.children[0].typeInfo
    case t.kind
    of TypeInt, TypeFloat: node.typeInfo = t
    else:
      fatal("Unary operator only works on numeric types", node)
  of NodeNot:
    node.checkKids(s)
    if node.children[0].isBottom():
      fatal("There's nothing to 'not' here", node)
    node.typeInfo = boolType
  of NodeMember:
    node.checkKids(s)
    fatal("Member access is currently not supported.", node)
  of NodeIndex:
    node.checkKids(s)
    case node.children[0].typeInfo.kind
    of TypeList:
      if isBottom(node.children[1], intType):
        fatal("Invalid list index (numbers only)", node.children[1])
      node.typeInfo = node.children[0].typeInfo.itemType
    of TypeDict:
      let
        kt = node.children[0].typeInfo.keyType
        vt = node.children[0].typeInfo.valType
      case kt.kind
      of TypeString, TypeInt:
        node.typeInfo = vt
      else:
        fatal("Dict indicies can only be strings or ints", node.children[1])
    else:
      fatal("Currently only support indexing on dicts and lists", node)
  of NodeCall:
    node.children[1].checkNode(s)

    if node.children[0].kind != NodeIdentifier:
      fatal("Data objects cannot currently have methods.", node.children[0])

    let
      fname = node.children[0].getTokenText()
      builtinOpt = s.getBuiltinBySig(fname, node.children[1].typeInfo)

    if builtinOpt.isNone():
      if s.isBuiltin(fname):
        fatal("No signature with that type found.", node.children[0])
      else:
        fatal("Function {fname} doesn't exist".fmt(), node.children[0])

    # Could stash for when we execute, but currently will just
    # look it up on the execute pass.
    let builtin = builtinOpt.get()
    node.typeInfo = builtin.tInfo.retType
  of NodeActuals:
    var t: seq[Con4mType]
    node.checkKids(s)

    for item in node.children:
      t.add(item.typeInfo)

    node.typeInfo = newProcType(t, newTypeVar())
  of NodeDictLit:
    node.checkKids(s)
    var ct = newTypeVar()
    for item in node.children:
      ct = unify(ct, item.typeinfo)
      if ct.isBottom():
        fatal("Dict items must all be the same type.", node)
    node.typeInfo = ct
  of NodeKVPair:
    node.checkKids(s)
    if node.children[0].isBottom() or node.children[1].isBottom():
      fatal("Invalid type for dictionary keypair", node)
    node.typeInfo = newDictType(node.children[0].typeInfo,
                                node.children[1].typeInfo)
  of NodeListLit:
    node.checkKids(s)
    var ct = newTypeVar()
    for item in node.children:
      ct = unify(ct, item.typeinfo)
      if ct.isBottom():
        fatal("List items must all be the same type", node)
    node.typeInfo = newListType(ct)
  of NodeOr, NodeAnd:
    node.checkKids(s)
    if isBottom(node.children[0], boolType) or
       isBottom(node.children[1], boolType):
      fatal("Each side of || and && expressions must eval to a bool", node)
    node.typeinfo = boolType
  of NodeNe, NodeCmp:
    node.checkKids(s)
    let t = unify(node.children[0], node.children[1])

    case t.kind
    of TypeInt, TypeFloat, TypeString, TypeBool:
      node.typeinfo = boolType
    of TypeBottom:
      fatal("Types to comparison operators must match", node)
    else:
      fatal("== and != currently do not work on lists or dicts", node)
  of NodeGte, NodeLte, NodeGt, NodeLt:
    node.checkKids(s)
    let t = unify(node.children[0], node.children[1])

    case t.kind
    of TypeInt, TypeFloat, TypeString:
      node.typeinfo = boolType
    of TypeBottom:
      fatal("Types to comparison operators must match", node)
    else:
      fatal("Comparison ops only work on numbers and strings", node)
  of NodePlus:
    node.checkKids(s)
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      fatal("Types of left and right side of binary operators must match", node)
    of TypeInt, TypeFloat, TypeString:
      node.typeInfo = t
    else:
      fatal("Invalid type for bianry operator", node)
  of NodeMinus, NodeMul:
    node.checkKids(s)
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      fatal("Types of left and right side of binary operators must match", node)
    of TypeInt, TypeFloat:
      node.typeInfo = t
    else:
      fatal("Invalid type for bianry operator", node)
  of NodeMod:
    node.checkKids(s)
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      fatal("Types of left and right side of binary operators must match", node)
    of TypeInt:
      node.typeInfo = t
    else:
      fatal("Invalid type for bianry operator", node)
  of NodeDiv:
    node.checkKids(s)
    let t = unify(node.children[0], node.children[1])
    case t.kind
    of TypeBottom:
      fatal("Types of left and right side of binary operators must match", node)
    of TypeInt, TypeFloat:
      node.typeInfo = floatType
    else:
      fatal("Invalid type for bianry operator", node)
  of NodeIdentifier:
    # This gets used in cases where the symbol is looked up in the
    # current scope, not when it has to be in a specific scope (i.e.,
    # member access).
    let
      scopes = node.getBothScopes()
      name = node.getTokenText()
      attrEntry = scopes.attrs.lookupAttr(name)
      varEntry = scopes.vars.lookup(name)

    if attrEntry.isNone() and varEntry.isNone():
      fatal("Variable {name} used before definition".fmt(), node)

    let ent = if attrEntry.isSome(): attrEntry.get() else: varEntry.get()

    node.typeInfo = ent.tinfo

proc checkTree*(node: Con4mNode, s: ConfigState) =
  ## Checks a parse tree rooted at `node` for static errors (i.e.,
  ## anything we can easily find before execution).  This version
  ## accepts an "old" `ConfigState` object, so that you can keep an
  ## old symbol table around, layering new choices on top of the old
  ## ones.
  for item in node.children:
    item.checkNode(s)

proc checkTree*(node: Con4mNode): ConfigState =
  ## Checks a parse tree rooted at `node` for static errors (i.e.,
  ## anything we can easily find before execution).  This version
  ## returns a new `ConfigState` object, that can be used for
  ## querying, dumped to a data structure (via our macros), or sent
  ## back to checkTree when loading a file being layered on top of
  ## what we've already read.
  node.scopes = some(newRootScope())

  result = newConfigState(node.scopes.get().attrs)

  node.checkTree(result)

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
  echo $("([string], { bool : float }, *int) -> @x".toCon4mType())
  echo $("[ string ]".tT().unify("@x".tT()))
  echo $("(int, int, int, *string)".tT())
  echo $"(int, int, int, *string) -> @x".tT().unify(
      "(int, int, int, *string)".tT())
  echo $"(int, int, int, string) -> @x".tT(
    ).unify("(int, int, int, string)".tT())
  echo $"(int, int, int, *string) -> @x".tT().unify(
      "(int, int, int, string, string)".tT())
  echo $"(int, int, int, *bool) -> @x".tT().unify("(int, int, int)".tT())
  echo $"(int, int, int, *@t) -> @x".tT().unify("(int, int, int, float)".tT())
  echo $"(int, int, int, *@t) -> @x".tT().unify("(int, int, int)".tT())
