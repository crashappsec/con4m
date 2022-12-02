import tables
import options
import unicode
import strutils
import strformat

import con4m_types

# Symbol tables and scopes, and things that support it.
# This includes the helper functions for instantiating type objects.

var tVarNum: int

proc newListType*(contained: Con4mType): Con4mType =
  return Con4mType(kind: TypeList, itemType: contained)

proc newDictType*(keyType, valType: Con4mType): Con4mType =
  return Con4mType(kind: TypeDict, keyType: keyType, valType: valType)

proc newTypeVar*(): Con4mType =
  tvarNum.inc()
  return Con4mType(kind: TypeTVar, varNum: tVarNum)

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
               loc: int,
               tinfo = newTypeVar(),
               subscope: bool = false): Option[STEntry] =
  if scope.entries.contains(name):
    return
  let e = STEntry(tinfo: tinfo, defLocs: @[loc])
  if subscope:
    let ss = Con4mScope(parent: some(scope))
    e.subscope = some(ss)
  scope.entries[name] = e
  return some(e)

proc lookup*(scope: Con4mScope, name: string, scopeOk: bool = false): Option[STEntry] =
  if scope.entries.contains(name):
    if not scopeOk and scope.entries[name].subscope.isSome():
      raise newException(ValueError, "Attempting to look up attribute that " &
                                     "refers to a section -- " & name)

    return some(scope.entries[name])
  if scope.parent.isSome():
    return scope.parent.get().lookup(name, scopeOk)

# This does NOT accept generics, as we don't support them across the
# call boundary. Except it currently does, for my own testing.
#
# Similarly, it does not accept bottom, other than you can leave off the
# arrow and type to indicate no return.

proc toCon4mType(s: string, tv: TableRef): (Con4mType, string) =
  var n = unicode.strip(s).toLower()

  if n.startsWith("string"): return (stringType, n["string".len() .. ^1])
  if n.startsWith("bool"): return(boolType, n["bool".len() .. ^1])
  if n.startsWith("int"): return (intType, n["int".len() .. ^1])
  if n.startsWith("float"): return (floatType, n["float".len() .. ^1])

  if n[0] == '`':
    let vname = $n[1]
    if not tv.contains(vname): tv[vname] = newTypeVar()
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
      params: seq[Con4mType]
      oneParam: Con4mType
      va: bool

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
        return (newProcType(params, bottomType, va), n)
      if (n.len() == 1) or (n[1] != '>'):
        raise newException(ValueError, "Expected > after - for proc return")
      n = unicode.strip(n[2 .. ^1])
      (oneParam, n) = n.toCon4mType(tv)
    else:
      oneParam = bottomType

    return (newProcType(params, oneParam, va), n)
  else:
    raise newException(ValueError, "Unknown character: {$(n[0])}".fmt())

proc toCon4mType*(s: string): Con4mType =
  var
    v: Con4mType
    n: string

  try:
    (v, n) = s.toCon4mType(newTable[string, Con4mType]())
  except:
    raise newException(ValueError, "Incomplete type specification")

  if unicode.strip(n).len() != 0:
    raise newException(ValueError, "Extraneous text after parsed type")

  return v

when isMainModule:
  var
    ctx: CurScopes = newRootScope()
    s: Con4mScope = ctx.attrs
    entry = s.addEntry(name = "module", loc = 1, subscope = true).get()
    ss = entry.subscope.get()

  discard s.addEntry("foo", 1)
  discard ss.addEntry("bar", 1)

  assert s.lookup("foo").isSome()
  assert s.lookup("bar").isNone()
  assert ss.lookup("foo").isSome()
  assert ss.lookup("bar").isSome()
  assert ss.lookup("boz").isNone()
