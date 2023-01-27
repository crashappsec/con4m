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


import tables, options, unicode, strutils, strformat, nimutils, types

var tVarNum: int

proc newListType*(contained: Con4mType): Con4mType =
  return Con4mType(kind: TypeList, itemType: contained)

proc newDictType*(keyType, valType: Con4mType): Con4mType =
  return Con4mType(kind: TypeDict, keyType: keyType, valType: valType)

proc newTypeVar*(constraints: set[Con4mTypeKind] = {}): Con4mType =
  tVarNum.inc()
  return Con4mType(kind: TypeTVar,
                   varNum: tVarNum,
                   link: none(Con4mType),
                   linksin: @[],
                   cycle: false,
                   constraints: constraints)

# This should only be called when we know that the type variable
# is going to be unique for the context.  It's mainly meant
# for compile-time usage.
proc newTypeVar*(num: int): Con4mType =
  return Con4mType(kind: TypeTVar, varNum: num)

proc newProcType*(params: seq[Con4mType],
                  retType: Con4mType,
                  va: bool = false): Con4mType =
  if params.len() != 0:
    return Con4mType(kind: TypeProc,
                    params: params,
                    va: va,
                    retType: retType)
  else:
    return Con4mType(kind: TypeProc, retType: retType)


##### New interface here.

proc newVarSym(name: string): VarSym =
  return VarSym(name: name, value: none(Box), firstDef: none(Con4mNode))
  
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
    
proc varLookup*(scope: VarScope, name: string, op: VLookupOp): Option[VarSym] =
  if name in scope.contents:
    case op
    of vlDef, vlUse:
      return some(scope.contents[name])
    of vlMask:
      return none(VarSym)
  else:
    case op
    of vlMask:
      var sym              = newVarSym(name)
      result               = some(sym)
      scope.contents[name] = sym
    of vlDef:
      if scope.parent.isSome():
        # it's a def lookup in OUR scope, but a use lookup in
        # parent scopes, if we have to recurse.
        let maybe = scope.parent.get().varLookup(name, vlUse)
        if maybe.isSome():
          return maybe

      var sym              = newVarSym(name)
      result               = some(sym)
      scope.contents[name] = sym
    of vlUse:
      if scope.parent.isSome():
        return scope.parent.get().varLookup(name, vlUse)
      else:
        return none(VarSym)

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
  if ix >= len(parts):
    return AttrErr(code: errNoAttr)

  let name = parts[ix]
  if ix == len(parts)-1:
      if name in scope.contents:
        let item = scope.contents[name]
        case op
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
          let sub     = AttrScope(contents: default(Table[string, AttrOrSub]))
          scope.contents[name] = either(sub)
          
          return scope.contents[name]
        of vlAttrDef:
          let attrib           = Attribute()
          scope.contents[name] = either(attrib)

          return scope.contents[name]
        else:
          return AttrErr(code: errNoAttr)
  else:
    let item = scope.contents[name]
    if item.isA(Attribute):
      let dotted = parts[0 .. ix].join(".")
      return AttrErr(code: errBadSec,
                     msg:  fmt"{dotted}: should be a section, but already "&
                               "have it as an attr")
    let newScope = item.get(AttrScope)
    return newScope.attrLookup(parts, ix + 1, op)
  
proc attrLookup*(attrs: AttrScope, fqn: string): Option[Box] =
  ## This is the interface for actually lookup up values at runtime.
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(attrs, parts, 0, vlAttrUse)

  if possibleAttr.isA(AttrErr):
    return none(Box)

  let
    aOrS    = possibleAttr.get(AttrOrSub)
    attr    = aOrS.get(Attribute)
    `val?`  = attr.value
    `over?` = attr.override
    
  if `over?`.isSome():
    return `over?`
  elif `val?`.isSome():
    return `val?`
  return none(Box)

proc attrLookup*(ctx: ConfigState, fqn: string): Option[Box] =
  return attrLookup(ctx.attrs, fqn)

proc attrSet*(attrs: AttrScope, fqn: string, value: Box): AttrErr =
  ## This is the interface for setting values at runtime.
  let
    parts        = fqn.split(".")
    possibleAttr = attrLookup(attrs, parts, 0, vlAttrUse)    

  if possibleAttr.isA(AttrErr):
    return possibleAttr.get(AttrErr)

  let
    aOrS              = possibleAttr.get(AttrOrSub)
    attr              = aOrS.get(Attribute)
    `over?`           = attr.override
    hook: AttrSetHook = attr.setHook

  if `over?`.isSome() or attr.locked:
    return AttrErr(code: errCantSet)

  attr.value = some(value)

  if hook != nil:
    hook(value)
  
  return AttrErr(code: errOk)

proc attrSet*(ctx: ConfigState, fqn: string, val: Box): AttrErr =
  return attrSet(ctx.attrs, fqn, val)

# This does not accept bottom, other than you can leave off the
# arrow and type to indicate no return.
#
proc toCon4mType(s: string, tv: TableRef): (Con4mType, string) =
  var n = unicode.strip(s).toLower()

  if n.startsWith("string"): return (Con4mType(kind: TypeString),
                                               n["string".len() .. ^1])
  if n.startsWith("bool"): return(Con4mType(kind: TypeBool),
                                  n["bool".len() .. ^1])
  if n.startsWith("int"): return (Con4mType(kind: TypeInt),
                                  n["int".len() .. ^1])
  if n.startsWith("float"): return (Con4mType(kind: TypeFloat),
                                              n["float".len() .. ^1])

  if n.len() == 0:
    raise newException(ValueError, "Cannot convert a null string to a type")
  if n[0] == '@':
    let vname = $n[1]
    if not tv.contains(vname): tv[vname] = newTypeVar(len(tv))
    return (tv[vname], n[2 .. ^1])
  elif n[0] == '[':
    var (contained, rest) = n[1 .. ^1].toCon4mType(tv)
    n = unicode.strip(rest)
    if n.len() == 0:
      raise newException(ValueError, "Unterminated list type")
    if n[0] != ']':
      raise newException(ValueError, "Unterminated list type")
    return (newListType(contained), n[1 .. ^1])
  elif n[0] == '{':
    var (keyT, rest) = n[1 .. ^1].toCon4mType(tv)
    n = unicode.strip(rest)
    if n[0] != ':':
      raise newException(ValueError, "Expected : in dict type")
    var (valT, rest2) = n[1 .. ^1].toCon4mType(tv)
    n = unicode.strip(rest2)
    if n.len() == 0:
      raise newException(ValueError, "Unterminated dict type")
    if n[0] != '}':
      raise newException(ValueError, "Unterminated dict type")
    return (newDictType(keyT, valT), n[1 .. ^1])
  elif n[0] == '(':
    var
      argtypes: seq[Con4mType]
      oneArgType: Con4mType

    n = unicode.strip(n[1 .. ^1])

    while true:
      (oneArgType, n) = n.toCon4mType(tv)
      argTypes.add(oneArgType)
      n = unicode.strip(n[0 .. ^1])
      if n.len() == 0:
        raise newException(ValueError, "Unterminated tuple type")
      case n[0]
      of ',':
        n = unicode.strip(n[1 .. ^1])
      of ')':
        if len(argTypes) < 2:
          raise newException(ValueError, "Tuples must have 2 or more items")
        n = unicode.strip(n[1 .. ^1])
        return (Con4mType(kind: TypeTuple, itemTypes: argTypes), n)
      else:
        raise newException(ValueError, "Invalid tuple type spec")
  elif n[0] == 'f':
    var
      params: seq[Con4mType]
      oneParam: Con4mType
      va: bool

    n = unicode.strip(n[1 .. ^1])

    if n[0] != '(':
      raise newException(ValueError, "Function types are written: f() -> ...")

    n = unicode.strip(n[1 .. ^1])
    if n.len() == 0:
      raise newException(ValueError, "Unterminated function parameter spec")
    if n[0] == ')':
      n = unicode.strip(n[1 .. ^1])
    else:
      while true:
        if va:
          raise newException(ValueError, "Can't have 2nd arg spec after *")

        if n[0] == '*':
          va = true
          n = n[1 .. ^1]

        (oneParam, n) = n.toCon4mType(tv)
        params.add(oneParam)
        n = unicode.strip(n[0 .. ^1])
        if n.len() == 0:
          raise newException(ValueError, "Unterminated function parameter spec")
        case n[0]
        of ',':
          if va:
            raise newException(ValueError, "Can't have 2nd arg spec after *")
          n = unicode.strip(n[1 .. ^1])
        of ')':
          n = unicode.strip(n[1 .. ^1])
          break
        else:
          raise newException(ValueError, "Invalid function type spec")

    if n.len() != 0:
      if n[0] != '-':
        return (newProcType(params, Con4mType(kind: TypeBottom), va), n)
      if (n.len() == 1) or (n[1] != '>'):
        raise newException(ValueError, "Expected > after - for proc return")
      n = unicode.strip(n[2 .. ^1])
      (oneParam, n) = n.toCon4mType(tv)
    else:
      oneParam = Con4mType(kind: TypeBottom)

    return (newProcType(params, oneParam, va), n)
  else:
    raise newException(ValueError, "Unknown character: {$(n[0])}".fmt())

proc toCon4mType*(s: string): Con4mType =
  ## Converts a string to a Con4m type object.
  var
    v: Con4mType
    n: string

  try:
    (v, n) = s.toCon4mType(newTable[string, Con4mType]())
  except:
    raise newException(ValueError,
                       "Incomplete type specification (no closing paren)")

  if unicode.strip(n).len() != 0:
    raise newException(ValueError,
                       "Extraneous text after parsed type: {n}".fmt())
  return v

  
