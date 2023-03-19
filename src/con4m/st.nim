## The Con4m symbol table, scopes, and things that support it.  This
## includes the helper functions for instantiating type objects.
##
## The methods are all meant to be internal; the end user shouldn't
## directly deal with the symbol table objects.
##
## The external interface should either be via macros of
## `getConfigVar()`, which lives in spec.nim just due to
## cross-file dependencies.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import tables, options, strutils, strformat, nimutils, types, algorithm,
       typecheck, dollars

proc newVarSym(name: string): VarSym =
  return VarSym(name:     name,
                tInfo:    newTypeVar(),
                value:    none(Box),
                firstDef: none(Con4mNode))

## Symbol table lookups for variables start in a given scope, and then
## check up the tree, to see if the variable is defined in the current
## scope. It's all static scoping; parent scopes have no sense of
## child scopes.  The child basically is generally going to be looking
## to see whether there is already a variable in scope, and if not,
## and if the variable is the left hand side of an assignment, then
## it'll be defined in the new scope.
##
## For the most part, we always prefer reusing a variable that's in
## scope, with the exception of index variables on for loops, which
## are scoped to the loop.  Currently there is no other case where
## you'd be able to mask a variable name that's in scope.
##
## So there are 3 kinds of lookups we might want to do:
##
## 1) A "def" lookup, where we are looking to find the symbol, and
##    want to define it in the local scope if it's not in a parent
##    scope, or else use the one that's there.
## 2) A "use" lookup, where we exepct the variable is there, and if it
##    isn't, there's definitely an error.
## 3) A "mask" lookup for constructs that will mask the variable, but
##    if the *current* scope has the variable, that should be a problem
##    (and, in fact, until we add to the language, that will not be
##    possible).
##
## The first will always return a symbol; the second two will return
## either a symbol or an error.  Since there is only a max of one error
## condition for each type of lookup, we model this with an Option.

proc varLookup*(scope: VarScope,
                name:  string,
                op:    VLookupOp,
                node:  Con4mNode = nil): Option[VarSym] =
  if name in scope.contents:
    case op
    of vlDef:
      let sym = scope.contents[name]
      if node != nil and node notin sym.defs: sym.defs.add(node)

      return some(sym)
    of vlUse:
      let sym = scope.contents[name]
      if node != nil and node notin sym.uses: sym.uses.add(node)
      return some(sym)
    of vlMask:
      let sym = scope.contents[name]
      if node != nil and node notin sym.defs: sym.defs.add(node)
      return some(sym)
    of vlFormal:
      let sym = scope.contents[name]
      if not sym.persists: return none(VarSym)
      # else fall through and stick in a new symbol.

  case op
  of vlMask, vlFormal:
    var sym              = newVarSym(name)
    result               = some(sym)
    scope.contents[name] = sym
    if node != nil:
      sym.firstDef = some(node)
      sym.defs     = @[node]
  of vlDef:
    if scope.parent.isSome():
      # it's a def lookup in OUR scope, but a use lookup in
      # parent scopes, if we have to recurse.
      let maybe = scope.parent.get().varLookup(name, vlUse)
      if maybe.isSome(): return maybe

    var sym              = newVarSym(name)
    result               = some(sym)
    scope.contents[name] = sym
    if node != nil:
      sym.firstDef = some(node)
      sym.defs     = @[node]
  of vlUse:
    if scope.parent.isSome(): return scope.parent.get().varLookup(name, vlUse)
    else:                     return none(VarSym)

proc varUse*(node: Con4mNode, name: string): Option[VarSym] =
  return varLookup(node.varScope, name, vlUse, node)

proc addVariable*(node: Con4mNode, name: string): VarSym =
  result = varLookup(node.varScope, name, vlDef).get()

  if result.firstDef.isNone(): result.firstDef = some(node)
  if node notin result.defs:   result.defs.add(node)

## With var scopes, we have a single name, where we are always
## searching back up a stack when we need to search.
##
## Attributes are top-down.  We start at some node in the tree, and if
## we have more names, we keep descending.  We pass an index into the
## dotted lookup array, so that if there's an error we can use the
## topic ssytem to publish an appropriate error.
##
## Attributes do not support masking. But they do have more error
## conditions!
proc attrLookup*(scope: AttrScope,
                 parts: openarray[string],
                 ix:    int,
                 op:    ALookupOp): AttrOrErr =

  if ix == 0 and (len(parts) == 0 or (len(parts) == 1 and parts[0] == "")):
    return either(scope)

  if ix >= len(parts):
    return AttrErr(code: errNoAttr)

  let name = parts[ix]
  if ix == len(parts)-1:
      if name in scope.contents:
        let item = scope.contents[name]
        case op
        of vlExists:
          return item
        of vlSecDef, vlSecUse:
          if item.isA(Attribute):
            let dotted = parts.join(".")
            return AttrErr(code: errBadSec,
                           msg:  fmt"{dotted}: should be a section, but " &
                                 "already have it as an attr")
          return item
        of vlAttrDef, vlAttrUse:
          if item.isA(Attribute):
            return item
          else:
            let dotted = parts.join(".")
            return AttrErr(code: errBadAttr,
                           msg:  fmt"{dotted}: should be an attr, but " &
                                    "already have it as a section")
      else:
        case op
        of vlSecDef:
          let sub     = AttrScope(name:     name,
                                  parent:   some(scope),
                                  config:   scope.config,
                                  contents: default(Table[string, AttrOrSub]))
          scope.contents[name] = either(sub)

          return scope.contents[name]
        of vlAttrDef:
          let attrib           = Attribute(name: name, scope: scope)
          scope.contents[name] = either(attrib)

          return scope.contents[name]
        else:
          return AttrErr(code: errNoAttr)
  else:
    var newScope: AttrScope

    if name in scope.contents:
      let item = scope.contents[name]
      if item.isA(Attribute):
        let dotted = parts[0 .. ix].join(".")
        return AttrErr(code: errBadSec,
                       msg:  fmt"{dotted}: should be a section, but already "&
                         "have it as an attr")
      newScope = item.get(AttrScope)
    else:
      newScope = AttrScope(name:     name,
                           parent:   some(scope),
                           config:   scope.config,
                           contents: default(Table[string, AttrOrSub]))

      scope.contents[name] = either(newScope)

    return newScope.attrLookup(parts, ix + 1, op)

proc attrExists*(scope: AttrScope, parts: openarray[string]): bool =
  return scope.attrLookup(parts, 0, vlExists).isA(AttrOrSub)

proc attrToVal*(attr: Attribute): Option[Box] =
  let
    `val?`  = attr.value
    `over?` = attr.override

  if `over?`.isSome():
    return `over?`
  elif `val?`.isSome():
    return `val?`
  return none(Box)

proc attrLookup*(attrs: AttrScope, fqn: string): Option[Box] =
  ## This is the interface for actually lookup up values at runtime.
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(attrs, parts, 0, vlAttrUse)

  if possibleAttr.isA(AttrErr):
    return none(Box)

  let attr = possibleAttr.get(AttrOrSub).get(Attribute)

  return attrToVal(attr)

proc attrLookup*(ctx: ConfigState, fqn: string): Option[Box] =
  return attrLookup(ctx.attrs, fqn)

proc setOverride*(attrs: AttrScope, name: string, val: Option[Box]): bool =
  let possibleAttr = attrLookup(attrs, @[name], 0, vlAttrUse)

  if possibleAttr.isA(AttrErr):
    return false

  var attr      = possibleAttr.get(AttrOrSub).get(Attribute)
  attr.override = val

  return true

proc setOverride*(ctx: ConfigState, fqn: string, val: Option[Box]): bool =
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(ctx.attrs, parts, 0, vlAttrUse)

  if possibleAttr.isA(AttrErr):
    return false

  var attr      = possibleAttr.get(AttrOrSub).get(Attribute)
  attr.override = val

  return true

proc attrLookupFull*(attrs: AttrScope, fqn: string):
                   (AttrErrEnum, Option[Box]) =
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(attrs, parts, 0, vlAttrUse)

  if possibleAttr.isA(AttrErr):
    return (possibleAttr.get(AttrErr).code, none(Box))

  let attr = possibleAttr.get(AttrOrSub).get(Attribute)

  return (errOk, attrToVal(attr))


proc attrLookupFull*(ctx: ConfigState, fqn: string):
                   (AttrErrEnum, Option[Box]) =
  return attrLookupFull(ctx.attrs, fqn)

proc fullNameAsSeq*(scope: AttrScope): seq[string] =
  var sec = scope
  result  = @[]

  while sec.parent.isSome():
    result.add(sec.name)
    sec = sec.parent.get()
  result.reverse

proc fullNameAsSeq*(attr: Attribute): seq[string] =
  result = attr.scope.fullNameAsSeq()
  result.add(attr.name)

proc fullNameAsStr*(scope: AttrScope): string =
  return scope.fullNameAsSeq().join(".")

proc fullNameAsStr*(attr: Attribute): string =
  return attr.fullNameAsSeq().join(".")

proc attrSet*(attr: Attribute, value: Box, hook: AttrSetHook = nil): AttrErr =
  ## This version of attrSet is the lowest level, and actually does
  ## the setting. This applies our logic for overrides, attribute
  ## locks and user-defined hooking, so don't set Attribute object
  ## values directly!
  let
    `over?`   = attr.override
    nameparts = attr.fullNameAsSeq()
    n         = nameparts.join(".")

  if `over?`.isSome():

    return AttrErr(code: errCantSet,
                   msg:  fmt"{n} attr can't be set due to user override")
  if attr.locked:
    return AttrErr(code: errCantSet,
                   msg:  fmt"{n}: attribute is locked and can't be set")
  if hook != nil:
    if not hook(nameParts, value):
      return AttrErr(code: errCantSet,
                     msg:  fmt"{n}: The application prevented this " &
                              "attribute from being set")
  attr.value = some(value)

  if attr.lockOnWrite:
    attr.locked = true

  return AttrErr(code: errOk)

proc attrSet*(attrs: AttrScope, fqn: string, value: Box): AttrErr =
  ## This is the interface for setting values at runtime.
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(attrs, parts, 0, vlAttrDef)

  if possibleAttr.isA(AttrErr):
    return possibleAttr.get(AttrErr)

  let
    aOrS              = possibleAttr.get(AttrOrSub)
    attr              = aOrS.get(Attribute)

  return attr.attrSet(value, attrs.config.setHook)

proc attrSet*(ctx: ConfigState, fqn: string, val: Box): AttrErr =
  return attrSet(ctx.attrs, fqn, val)

proc nameUseContext*(node: Con4mNode, name: string, ctx: ConfigState): UseCtx =
  if name in ctx.funcTable:             return ucFunc
  if node.attrScope.attrExists([name]): return ucAttr
  if node.varUse(name).isSome():        return ucVar

  if node.attrScope != ctx.attrs and ctx.attrs.attrExists([name]):
    return ucAttr

  return ucNone

proc runtimeVarLookup*(frames: VarStack, name: string): Box =
  var n = frames.len()

  while n != 0:
    n         = n - 1
    let frame = frames[n]

    if name in frame:
      let optRet = frame[name]
      if not optRet.isSome():
        raise newException(ValueError,
                           fmt"Variable {name} used before assignment")
      return optRet.get()
  unreachable

proc runtimeVarSet*(state: ConfigState, name: string, val: Box) =
  var n = state.frames.len()

  while n != 0:
    n         = n - 1
    let frame = state.frames[n]

    if name in frame:
      frame[name] = some(val)
      return

  unreachable

proc lockAttribute*(attrs: AttrScope, fqn: string): bool =
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(attrs, parts, 0, vlAttrUse)

  if possibleAttr.isA(AttrErr):
    return false

  let attr    = possibleAttr.get(AttrOrSub).get(Attribute)
  attr.locked = true

proc lockAttribute*(state: ConfigState, fqn: string): bool =
  return state.attrs.lockAttribute(fqn)

const nullstr = "\"null\""

proc oneValToJson(box: Box, tInfo: Con4mType): string =
  case tInfo.kind
  of TypeInt:
    return $(unpack[int](box))
  of TypeFloat:
    return $(unpack[float](box))
  of TypeTuple:
    var
      x              = unpack[seq[Box]](box)
      l: seq[string] = @[]
    for i, item in x: l.add(item.oneValToJson(tInfo.itemTypes[i]))
    result = "[" & l.join(", ") & "]"
  of TypeList:
    var
      x              = unpack[seq[Box]](box)
      l: seq[string] = @[]
    for item in x: l.add(item.oneValToJson(tInfo.itemType))
    result = "[" & l.join(", ") & "]"
  of TypeDict:
    var
      x              = unpack[Con4mDict[Box, Box]](box)
      l: seq[string] = @[]
    for k, v in x:
      let
        kstr = k.oneValToJson(tInfo.keyType)
        vstr = v.oneValToJson(tInfo.valType)
      l.add(kstr & ":" & vstr)
    result = "{" & l.join(", ") & "}"
  else:
    result = "\"" & tInfo.oneArgToString(box) & "\""
    
proc scopeToJson*(scope: AttrScope): string =
  var kvpairs: seq[string] = @[]

  for k, v in scope.contents:
    if v.isA(Attribute):
      let
        attr   = v.get(Attribute)
        boxOpt = attr.attrToVal()
      if boxOpt.isSome():
        let
          val     = boxOpt.get().oneValToJson(attr.tInfo) 
          typeStr = fmt("\"type\": \"{$(attr.tInfo)}\"")
          valStr  = fmt("\"value\": {val}")
        
        kvpairs.add(fmt(""""{k}" : {{{typeStr}, {valStr}}}"""))
      else:
        kvpairs.add(fmt""""{k}" : {nullstr}""")
    else:
      kvpairs.add(fmt""""{k}" : {scopeToJson(v.get(AttrScope))}""")
  result = "{ " & kvpairs.join(", ") & "}"
