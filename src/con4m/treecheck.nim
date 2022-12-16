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
import tables

import ./types
import st
import box
import dollars
import typecheck
import spec
import parse # just for fatal()

proc checkNode(node: Con4mNode, s: ConfigState)

proc checkKids(node: Con4mNode, s: ConfigState) {.inline.} =
  for item in node.children:
    item.checkNode(s)

template pushVarScope() =
  if not s.secondPass:
    let scopeOpt = node.scopes
    var scopes = scopeOpt.get()

    scopes.vars = Con4mScope(parent: some(scopes.vars))
    node.scopes = some(scopes)

proc getTokenText*(node: Con4mNode): string {.inline.} =
  ## This returns the raw string associated with a token.  Internal.
  let token = node.token.get()

  case token.kind
  of TtStringLit:
    return token.unescaped
  else:
    return $(token)

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

proc cycleDetected(stack: var seq[FuncTableEntry]) =
  let
    toMatch = stack[^1]
  var
    msg = "Function call cycle detected (disallowed): "
    parts: seq[string]

  for item in stack:
    if len(parts) == 0:
      if item != toMatch:
        continue
    let
      name = item.name
      sig = `$`(item.tinfo)[1 .. ^1]
    parts.add(fmt"{name}{sig}")

  msg = msg & parts.join(" calls ")
  raise newException(Con4mError, msg)

proc cyCheckOne(s: ConfigState, n: Con4mNode, stack: var seq[FuncTableEntry])

proc cycleDescend(s: ConfigState,
                  n: Con4mNode,
                  stack: var seq[FuncTableEntry]) =
  if n.kind != NodeCall:
    for kid in n.children:
      cycleDescend(s, kid, stack)
  else:
    let fname = n.children[0].getTokenText()
    if fname in s.funcTable:
      let entries = s.funcTable[fname]
      for item in entries:
        if item.kind == FnBuiltIn:
          return
        let t = unify(n.children[1].typeInfo, item.tinfo)
        if not t.isBottom():
          stack.add(item)
          # This must be present if checking passed.
          let node = item.impl.get()
          cyCheckOne(s, node, stack)

proc cyCheckOne(s: ConfigState, n: Con4mNode, stack: var seq[FuncTableEntry]) =
  if stack[^1].onStack:
    cycleDetected(stack)

  stack[^1].onStack = true
  cycleDescend(s, n, stack)
  stack[^1].onStack = false
  stack[^1].cannotCycle = true
  discard stack.pop()

  # We are going to add functions that get defined in this module to
  # the list of things to check for cycles.  We use the
  # "onStack" field to track all possible call stacks, and if
  # we go to push a function that is already on the stack, we
  # know there's a cycle.

  # Note that, when we allow function definitions to be shadowed, it
  # is possible to introduce cross-config file cycles that aren't
  # detectable just by looking at one file.
  #
  # To deal with such a case, we need to peer into the implementations
  # of called functions from other config files we've loaded.  But, we
  # only need to do that when they are called locally, we don't need
  # to use them as starting points, just the functions defined
  # locally.
  #
  # We also assume that built-in functions do NOT call back directly
  # into config code. That would defeat the purpose of callbacks. If
  # you break that rule, then all bets are off!
proc cycleCheck(s: ConfigState) =
  var stack: seq[FuncTableEntry]

  for item in s.moduleFuncDefs:
    if item.cannotCycle:
      continue
    else:
      stack = @[item]
      # If we got here, item.impl must exist.
      s.cyCheckOne(item.impl.get(), stack)

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
  of NodeBreak, NodeContinue, NodeFormalList:
    discard
  of NodeReturn:
    if not s.funcOrigin:
      fatal("Return not allowed outside function or callback definition")
    if len(node.children) == 0:
      let
        scope = node.getVarScope()
        entry = scope.getEntry("result").get()
      if entry.firstDef.isNone():
        entry.tinfo = bottomType
      elif entry.tinfo != bottomType:
        fatal("Return without a value when expecting a value", node)
    else:
      node.children[0].checkKids(s)
      let
        scope = node.getVarScope()
        entry = scope.getEntry("result").get()
      if entry.firstDef.isNone():
        entry.tinfo = node.children[0].typeInfo
        entry.firstDef = some(node)
      else:
        let t = unify(entry.tinfo, node.children[0].typeInfo)
        if t.isBottom():
          fatal("Inconsistent return type for function", node.children[0])
        entry.tinfo = t
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

    if s.funcOrigin:
      fatal("Cannot introduce a section within a func or callback", node)

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
    if s.funcOrigin:
      fatal("Cannot assign to attributes within functions or callbacks.", node)
    if node.children[0].kind != NodeIdentifier:
      fatal("Dot assignments for attributes currently not supported.",
            node.children[0])

    node.children[1].checkNode(s)
    let
      name = node.children[0].getTokenText()
      scopes = node.getBothScopes()
      tinfo = node.children[1].typeinfo

    if name == "result":
      fatal("'result' is a reserved word in con4m, and cannot be used " &
        "for attributes", node)

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

    if name == "result" and not s.funcOrigin:
      fatal("Cannot assign to special variable 'result' outside a function " &
        "definition.")
    if scopes.attrs.lookupAttr(name).isSome():
      fatal(fmt"Variable {name} conflicts with existing attribute",
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
        fatal(fmt"You cannot assign to the (locked) value {name}.",
              node.children[0])

      if t.isBottom():
        fatal(fmt"Type of value assigned to {name} conflicts with " &
                 "the exiting type",
              node.children[1])
      else:
        entry.tinfo = t

  of NodeUnpack:
    let
      tup = node.children[^1]
      scopes = node.getBothScopes()

    tup.checkNode(s)
    if tup.typeInfo.kind != TypeTuple:
      fatal("Trying to unpack a value that is not a tuple.", tup)
    if tup.typeInfo.itemTypes.len() != node.children.len() - 1:
      fatal("Trying to unpack a tuple of the wrong size.", tup)

    for i, tv in tup.typeInfo.itemTypes:
      let name = node.children[i].getTokenText()
      if scopes.attrs.lookupAttr(name).isSome():
        fatal("Variable {name} conflicts with existing attribute".fmt(),
              node.children[0])

      let existing = scopes.vars.lookup(name)
      if not existing.isSome():
        discard scopes.vars.addEntry(name, some(node), tv)
      else:
        let
          entry = existing.get()
          t = unify(tv, entry.tinfo)

        if entry.locked:
          fatal(fmt"Cannot unpack to (locked) variable {name}.",
                node.children[i])
        if t.isBottom():
          fatal(fmt"Type of value assigned to {name} conflicts with " &
            "the exiting type",
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
    of TypeTuple:
      if isBottom(node.children[1], intType):
        fatal("Invalid tuple index (numbers only)", node.children[1])
      node.typeInfo = newTypeVar()
      let v = node.children[1].value
      if v == nil:
        fatal("Tuple index must be an integer literal for " &
              "static type checking", node.children[1])
      let i = unbox[int](v)

      if i < 0 or i >= node.children[0].typeInfo.itemTypes.len():
        fatal("Tuple index out of bounds", node.children[1])
      node.typeInfo = node.children[0].typeInfo.itemTypes[i]
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
  of NodeFuncDef:
    let
      rootscope = node.getVarScope()
      name = node.children[0].getTokenText()
      callback = if node.getTokenText() == "callback": true else: false

    if rootscope.getEntry(name).isSome():
      fatal(fmt"Attempted to redefine {name} as a function", node)

    if callback:
      var scopes = node.scopes.get()
      scopes.vars = Con4mScope(parent: none(Con4mScope))
      node.scopes = some(scopes)
    else:
      pushVarScope()

    # Remove these for execution; the executor will more or less treat
    # them like top-level nodes.
    #
    # The if statement is due to the fact that, if there are forward
    # references, we will end up doing multiple type checking passes.
    if node.parent.isSome():
      s.moduleFuncImpls.add(node)
      var l = node.parent.get().children
      l.delete(l.find(node))
      node.parent = none(Con4mNode)

    let scope = node.getVarScope()

    # Add a "result" variable.
    discard scope.addEntry("result")

    for node in node.children[1].children:
      let
        varname = node.getTokenText()
        optEntry = scope.addEntry(varname)
      if optEntry.isNone():
        fatal("Duplicate parameter name when defining function", node)


    s.funcOrigin = true
    node.children[2].checkNode(s)
    s.funcOrigin = false

    # Type check parameters, add the function w/ signature in the fn table.
    # We look them back up in the symbol table.
    # Note that, for now anyway, we are not accepting varargs functions.
    let
      tinfo = Con4mType(kind: TypeProc, va: false)
      entry = scope.getEntry("result").get()

    # If the return variable was never set, firstDef will be nothing,
    # and this method returns void.
    if entry.firstDef.isNone():
      tinfo.retType = bottomType
    else:
      tinfo.retType = entry.tinfo

    for node in node.children[1].children:
      let
        varname = node.getTokenText()
        entry = scope.getEntry(varname).get()

      tinfo.params.add(entry.tinfo)

    # Now, that we have type info, we want to update the function table.
    if not s.funcTable.contains(name):
      if callback:
        fatal(fmt"Callback {name} is not a known callback you can define",
              node)
      let f = FuncTableEntry(kind: FnUserDefined,
                             tinfo: tinfo,
                             name: name,
                             onStack: true,
                             cannotCycle: false,
                             locked: false,
                             impl: some(node))
      s.moduleFuncDefs.add(f)
      s.funcTable[name] = @[f]
      return

    var fnList = s.funcTable[name]
    for item in fnList:
      # Look for already installed info about functions of this name.
      # If we find one, perform any checks we need, and if we are allowed
      # to overwrite the old implementation, do so (and return).
      #
      # If we don't find a match, then we are adding a new signature
      # with a name that is already defined.
      case item.kind
      of FnBuiltIn:
        fatal(fmt"Function name conflicts with a builtin function", node)
      of FnCallback:
        if not callback:
          fatal("User-defined function has the same name as a callback." &
            "If you intended for this to be a callback implementation, " &
            "use 'callback' instead of 'func'", node)
        elif not isBottom(tinfo, item.tinfo):
          if item.onStack:
            fatal("Multiple callback defs in the same config file", node)
          elif item.locked:
            fatal("A callback is already installed, and is locked " &
                  "(i.e., has been marked so that it can not be replaced)",
                  node)
          else:
            item.onStack = true
            item.impl = some(node) # Write over the old implementation.
            s.moduleFuncDefs.add(item)
            return
      of FnUserDefined:
        if not isBottom(tinfo, item.tinfo):
          if item.onStack:
            fatal("Multiple defs of function in one config file", node)
          elif item.locked:
            fatal("An implementation of this function exists and is " &
              "locked (i.e., has been marked so that it can't be replaced)")
          else:
            item.onStack = true
            item.impl = some(node)
            s.moduleFuncDefs.add(item)
            return
    # If we get here, the name was already defined, and none of the existing
    # definitions match our signature.  If it's a user-defined function, that's
    # great, add ourselves in.
    #
    # However, if it's a callback, then we've wasted everybody's
    # time and should be sent packing.
      if item.kind == FnCallback:
        fatal("Callback implementation does not match expected callback " &
          "signature")
    let f = FuncTableEntry(kind: FnUserDefined,
                           tinfo: tinfo,
                           name: name,
                           impl: some(node),
                           onStack: true,
                           cannotCycle: false,
                           locked: false)

    fnList.add(f)
    s.moduleFuncDefs.add(f)
    s.funcTable[name] = fnList

  of NodeCall:
    # Because we accept function calls for functions that aren't defined
    # yet in the source when the call appaears, we need to insert a dummy
    # type variable here, just in case we cannot resolve the type yet. This
    # will keep typechecking above this node from failing due to no info
    # about this node.
    #
    # However, if we do find a type, we will replace this value.
    #
    # Note that we do *not* allow the use of global variables before
    # they are defined, even in a function.

    node.typeInfo = newTypeVar()

    node.children[1].checkNode(s)

    if node.children[0].kind != NodeIdentifier:
      fatal("Data objects cannot currently have methods.", node.children[0])

    let fname = node.children[0].getTokenText()

    if fname in s.funcTable:
      var entries = s.funcTable[fname]

      for item in entries:
        # If we have real type info here, we use it now.  Otherwise,
        # we'll have to mark ourselves as waiting on a future
        # definition, and there will need to be a second pass of the
        # tree :(
        if not item.tinfo.isBottom():
          let t = unify(node.children[1].typeInfo, item.tinfo)
          if t.isBottom():
            # Actually, it didn't match this signature. Go back to
            # hoping there's a forward reference.
            continue
          else:
            # Here, it did match the signature, but check to see
            # if there's an implementation, or if it's just a stub
            # like a callback, or some user-defined function we're
            # expecting to see based on previous code but haven't.
            if not (item.kind == FnBuiltIn) and item.impl.isNone():
              if not ((fname, item) in s.callBeforeDef):
                s.callBeforeDef.add((fname, item))
            node.typeInfo = t.retType
            return

      # After looping through all entries and not finding it, we hope
      # there is a future definition, but only if the existing items
      # are of type FnUserDefined (otherwise, the system should have
      # given us a signature).
      if entries[0].kind != FnUserDefined:
        fatal("No signature with that type found.", node.children[0])

      let f = FuncTableEntry(kind: FnUserDefined,
                             tinfo: bottomType,
                             name: fname,
                             impl: none(Con4mNode),
                             onStack: false,
                             cannotCycle: false,
                             locked: false)
      entries.add(f)
      s.funcTable[fname] = entries
      s.callBeforeDef.add((fname, f))
      s.waitingForTypeInfo = true
      return
    else:
      # The call wasn't in the function table, so we hope it's defined
      # later in the config.
      let f = FuncTableEntry(kind: FnUserDefined,
                             tinfo: bottomType,
                             name: fname,
                             impl: none(Con4mNode),
                             onStack: false,
                             cannotCycle: false,
                             locked: false)
      s.funcTable[fname] = @[f]
      s.callBeforeDef.add((fname, f))
      s.waitingForTypeInfo = true
      return

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
  of NodeTupleLit:
    node.checkKids(s)
    var itemTypes: seq[Con4mType]

    for item in node.children:
      itemTypes.add(item.typeInfo)
    node.typeInfo = Con4mType(kind: TypeTuple, itemTypes: itemTypes)
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
  ##
  ## It does need to create a new variable scope though!
  node.scopes = some(CurScopes(vars: Con4mScope(), attrs: s.st))

  s.funcOrigin = false
  s.moduleFuncDefs = @[]
  s.moduleFuncImpls = @[]
  s.errors = @[]

  for item in node.children:
    item.checkNode(s)

  # When this condition is true, some function call we saw was unable
  # to find an implementation with the right signature.
  #
  # We now look to see if the function got provided.  If we see any
  # call table entries where the function was supposed to be user
  # defined and still hasn't been seen, then we know those functions
  # have not been defined.  Otherwise, we now have complete
  # information for function implementations. But, if we had been
  # waiting on a user-defined function, we will have incomplete type
  # information. Unfortunately, in such a case, need to run the type
  # checker a second time before we can move on to the next step,
  # which is detecting (and disallowing) recursive calls.

  var badList: seq[string]
  for (name, item) in s.callBeforeDef:
    if item.impl.isNone():
      badList.add(fmt"{name}{`$`(item.tinfo)[1 .. ^1]}")

  if len(badList) != 0:
    fatal(fmt"""func(s) {badList.join(", ")}() called but not defined.""")

    # At least one of the call-before-def situations was to a function
    # where we didn't have the type, but now we DO have the type. As a
    # result, we have to make a second typecheck pass.
    #
    # Note that we've de-rooted any functions by this point, so to be
    # able to re-check them, we kept them around.
    #
    # The secondPass flag tells the checker to skip anything not type
    # related.
    if s.waitingForTypeInfo:
      s.waitingForTypeInfo = false
      s.secondPass = true
      for item in node.children:
        item.checkNode(s)
      for funcRoot in s.moduleFuncImpls:
        funcRoot.checkNode(s)

  # This cycle check waits until we are sure all forward references
  # are resolved.
  s.cycleCheck()

proc checkTree*(node: Con4mNode): ConfigState =
  ## Checks a parse tree rooted at `node` for static errors (i.e.,
  ## anything we can easily find before execution).  This version
  ## returns a new `ConfigState` object, that can be used for
  ## querying, dumped to a data structure (via our macros), or sent
  ## back to checkTree when loading a file being layered on top of
  ## what we've already read.
  ##
  ## Adds all default builtins.
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
