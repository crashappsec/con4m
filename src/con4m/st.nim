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


import tables
import options
import unicode
import strutils
import strformat
import parse # only for fatal()

import ./types


var tVarNum: int

template lookupError(m: string, symbol: string, entry: Option[STEntry]) =
  if entry.isNone() or entry.get().firstDef.isNone():
    let t = Con4mToken(kind: TTStringLit,
                       unescaped: symbol,
                       lineNo: -1,
                       lineOffset: -1)
    fatal("When looking up " & symbol & ": " & m, t)
  else:
    let t = entry.get().firstDef.get().token
    if not t.isSome():
      assert false, "Programmer error, no token provided"

    fatal("When looking up " & symbol & ": " & m, t.get())


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

proc newRootScope*(): CurScopes =
  result = CurScopes(vars: Con4mScope(), attrs: Con4mScope())

proc getEntry*(scope: Con4mScope, name: string): Option[STEntry] =
  if not scope.entries.contains(name): return
  return some(scope.entries[name])

proc addEntry*(scope: Con4mScope,
               name: string,
               firstDef: Option[Con4mNode] = none(Con4mNode),
               tinfo = newTypeVar(),
               subscope: bool = false): Option[STEntry] =
  if scope.entries.contains(name):
    return
  let e = STEntry(tinfo: tinfo, firstDef: firstDef)
  if subscope:
    let ss = Con4mScope(parent: some(scope))
    e.subscope = some(ss)
  scope.entries[name] = e
  return some(e)

# This version of symbol lookup is meant to be used when evaluating a
# non-dotted variable in a local scope. If a variable is not found in
# one scope, then we check the parent scope, and we don't give up
# until we get to the root scope and the item is still missing.
proc lookup*(scope: Con4mScope, name: string): Option[STEntry] =
  if scope.entries.contains(name):
    if scope.entries[name].subscope.isSome():
      lookupError("asked for a attribute, but this name refers to a section",
                  name,
                  some(scope.entries[name]))

    return some(scope.entries[name])
  if scope.parent.isSome():
    return scope.parent.get().lookup(name)

# This version of symbol lookup checks for an attribute in the local scope,
# i.e., without dot notation.  This does NOT inherit from previous scopes,
# they only do local or dot notation.
proc lookupAttr*(scope: Con4mScope,
                 name: string,
                 scopeOk: bool = false): Option[STEntry] =
  ## This method is exposed because it's used in the code generated
  ## automatically by our macro library.  It's lower level, operating
  ## on the scope data type that doesn't get exposed from the package
  ## by default.  Instead, use `getConfigVar()`.
  if scope.entries.contains(name):
    if not scopeOk and scope.entries[name].subscope.isSome():
      lookupError("asked for a attribute, but this name refers to a section",
                  name,
                  some(scope.entries[name]))

    return some(scope.entries[name])

# This version of symbol lookup is meant for dotted notation, when we
# are essentially doing object access.  But, we don't have objects,
# we have attribute scopes that can be nested.
proc dottedLookup*(scope: Con4mScope, dotted: seq[string]): Option[STEntry] =
  if dotted.len() == 0:
    return

  let
    name = dotted[0]
    rest = dotted[1 .. ^1]

  if not (name in scope.entries):
    return
  let entry = scope.entries[name]

  if rest.len() == 0:
    return some(entry)

  if not entry.subscope.isSome():
    return

  return dottedLookup(entry.subscope.get(), rest)

# This does NOT accept generics, as we don't support them across the
# call boundary. Except it currently does, for my own testing.
#
# Similarly, it does not accept bottom, other than you can leave off the
# arrow and type to indicate no return.

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

  if n[0] == '@':
    let vname = $n[1]
    if not tv.contains(vname): tv[vname] = newTypeVar(len(tv))
    return (tv[vname], n[2 .. ^1])
  elif n[0] == '[':
    var (contained, rest) = n[1 .. ^1].toCon4mType(tv)
    n = unicode.strip(rest)
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
      case n[0]
      of ',':
        n = unicode.strip(n[1 .. ^1])
      of ')':
        if len(argTypes) < 2:
          raise newException(ValueError, "Tuples must have 2 or more items")
        n = unicode.strip(n[1 .. ^1])
        return (Con4mType(kind: TypeTuple, itemTypes: argTypes), n)
      else:
        raise newException(ValueError, "Invalid function type spec")
  elif n[0] == 'f':
    var
      params: seq[Con4mType]
      oneParam: Con4mType
      va: bool

    n = unicode.strip(n[1 .. ^1])

    if n[0] != '(':
      raise newException(ValueError, "Function types are written: f() -> ...")

    n = unicode.strip(n[1 .. ^1])

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
    raise newException(ValueError, "Incomplete type specification")

  if unicode.strip(n).len() != 0:
    raise newException(ValueError,
                       "Extraneous text after parsed type: {n}".fmt())

  return v


