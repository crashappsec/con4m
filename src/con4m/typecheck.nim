## Our implementation of the good ol' unifcation algorithm, with
## Con4m's current type rules codified.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import types
import st

import dollars
import options

proc resolveTypeVars(t: Con4mType): Con4mType =
  case t.kind
  of TypeTVar:
    if t.cycle:
      return bottomType
    if t.link.isSome():
      t.cycle = true
      result = resolveTypeVars(t.link.get())
      t.cycle = false
      return
    else:
      return t
  else:
    return t

proc linkTypeVar(t1: Con4mType, t2: Con4mType) =
  if t1 == t2:
    return
  if t2.kind == TypeTVar:
    t2.linksin.add(t1)
    t1.link = some(t2)
    for item in t1.linksin:
      item.link = some(t2)
      t2.linksin.add(item)
  else:
    t1.link = some(t2)
    for item in t1.linksin:
      item.link = some(t2)

  t1.linksin = @[]

proc getBaseType*(t: Con4mType): Con4mTypeKind =
  case t.kind
  of TypeTVar:
    if t.link.isSome():
      return t.link.get().getBaseType()
    return TypeTVar
  else:
    return t.kind

proc getBaseType*(node: Con4mNode): Con4mTypeKind =
  return node.typeInfo.getBaseType()

const allConstraints: set[Con4mTypeKind] = {
  TypeString, TypeBool, TypeInt, TypeFloat, TypeTuple, TypeList, TypeDict
  }

# Just in case someone manages to clone a singleton, we
# always check against the .kind field, instead of looking at
# object equivolence for singletons (e.g., int, bottom)

proc unify*(param1: Con4mType, param2: Con4mType): Con4mType {.inline.} =
  let
    t1 = param1.resolveTypeVars()
    t2 = param2.resolveTypeVars()

  if t2.kind == TypeTVar and t1.kind != TypeTVar:
    return t2.unify(t1)

  case t1.kind
  of TypeString, TypeBool, TypeInt, TypeFloat:
    if t2.kind == t1.kind: return t1
    return bottomType
  of TypeBottom: return bottomType
  of TypeProc:
    case t2.kind
    of TypeProc:
      var
        newParams: seq[Con4mType]
        newRet: Con4mType
        vaResult: bool

      # Actuals will never be varargs, so if we have two vararg
      # functions, it's only because we're trying to unify two formals.
      if ((not t1.va) and (not t2.va)) or (t1.va and t2.va):
        if t1.params.len() != t2.params.len(): return bottomType
        for i in 0 ..< t1.params.len():
          let p = t1.params[i].unify(t2.params[i])

          if p.kind == TypeBottom: return bottomType
          newParams.add(p)
        if t1.va: vaResult = true
      else:
        if t1.va:
          vaResult = true
          if t2.params.len() < t1.params.len() - 1: return bottomType
          for i in 0 ..< t1.params.len() - 1:
            let p = t1.params[i].unify(t2.params[i])
            if p.kind == TypeBottom: return bottomType
            newParams.add(p)
          var vargType: Con4mType = t1.params[^1]
          for i in t1.params.len()-1 ..< t2.params.len():
            vargType = vargType.unify(t2.params[i])
            if vargType.kind == TypeBottom: return bottomType
          newParams.add(vargType)
        else:
          return t2.unify(t1)

      newRet = t1.retType.unify(t2.retType)
      if newRet.kind == TypeBottom:
        if not (t1.retType.kind in [TypeBottom, TypeTVar]) or
           not (t2.retType.kind in [TypeBottom, TypeTVar]):
          return bottomType

      return newProcType(newParams, newRet, vaResult)

    else: return bottomType
  of TypeTuple:
    if t2.kind != TypeTuple: return bottomType
    if len(t2.itemTypes) != len(t1.itemTypes): return bottomType
    result = Con4mType(kind: TypeTuple, itemTypes: @[])
    for i, item in t1.itemTypes:
      let l = unify(item, t2.itemTypes[i])
      if l.kind == TypeBottom:
        return bottomType
      result.itemTypes.add(l)
    return
  of TypeList:
    if t2.kind != TypeList: return bottomType
    let containedType = t1.itemType.unify(t2.itemType)
    if containedType == bottomType: return bottomType
    return newListType(containedType)
  of TypeDict:
    if t2.kind != TypeDict: return bottomType
    let kt = t1.keyType.unify(t2.keyType)
    let vt = t1.valType.unify(t2.valType)
    if kt.kind == TypeBottom or vt.kind == TypeBottom: return bottomType
    return newDictType(kt, vt)
  of TypeTVar:
    if t2.kind == TypeTVar:
      let intersection = t1.constraints * t2.constraints
      if t1.constraints == {}:
        t1.linkTypeVar(t2)
        return t2
      elif t2.constraints == {}:
        t2.linkTypeVar(t1)
        return t1
      elif intersection == {}:
        return bottomType
      elif card(intersection) == 1:
        var newType: Con4mType
        if TypeBool in intersection:
          newType = boolType
        elif TypeString in intersection:
          newType = stringType
        elif TypeInt in intersection:
          newType = intType
        elif TypeFloat in intersection:
          newType = floatType
        elif TypeList in intersection:
          newType = Con4mType(kind: TypeList, itemType: newTypeVar())
        elif TypeDict in intersection:
          newType = Con4mType(kind: TypeDict,
                              keyType: newTypeVar(),
                              valType: newTypeVar())
        else:
          # This can only be TypeTuple.
          # If two type variables are somehow constrained so that they
          # could only possibly be tuples, we don't have any info on
          # number of elements, which we cannot generically represent,
          # so this is against our type rules.
          return bottomType
        # The above elif-branches all come down here
        t1.linkTypeVar(newType)
        t2.linkTypeVar(newType)
        return newType
      else: #card(intersection) > 1
        t1.constraints = intersection
        t2.constraints = intersection
        t1.linkTypeVar(t2)
        return t2
    else:
      if t1.constraints == {} or t2.kind in t1.constraints:
        t1.linkTypeVar(t2)
        return t2
      else:
        return bottomType

#proc debugunify*(param1: Con4mType, param2: Con4mType): Con4mType =
# If you want to use this, rename unify* above to unifyActual,
# Change the name of this to unify*, and then add a prototype
# for this function above unifyActual...
#
#  result = unifyActual(param1, param2)
#  echo fmt"{`$`(param1)} ⋃ {`$`(param2)} = {`$`(result)}"

proc isBottom*(t: Con4mType): bool =
  return t.kind == TypeBottom

proc isBottom*(t1, t2: Con4mType): bool =
  return unify(t1, t2).isBottom()

proc unify*(n2, n1: Con4mNode): Con4mType =
  return unify(n1.typeInfo, n2.typeInfo)

proc isBottom*(n: Con4mNode): bool =
  return n.typeInfo.isBottom()

proc isBottom*(n1, n2: Con4mNode): bool =
  return isBottom(n1.typeInfo, n2.typeInfo)

proc isBottom*(n: Con4mNode, t: Con4mType): bool =
  return isBottom(n.typeInfo, t)

proc hasTypeVar*(t: Con4mType): bool =
  case t.kind
  of TypeTVar:
    # Doesn't matter if it's a forward, return true and
    # get a clean copy!
    return true
  of TypeList:
    return t.itemType.hasTypeVar()
  of TypeDict:
    return t.keyType.hasTypeVar() or t.valType.hasTypeVar()
  of TypeTuple:
    for item in t.itemTypes:
      if item.hasTypeVar(): return true
  of TypeProc:
    for item in t.params:
      if item.hasTypeVar(): return true
    return t.retType.hasTypeVar()
  else:
    return false

proc copyType*(t: Con4mType): Con4mType =
  case t.kind
  of TypeTVar:
    if t.link.isSome():
      return copyType(t.link.get())
    else:
      result = newTypeVar(t.constraints)
      result.cycle = false
  of TypeList:
    if t.itemType.hasTypeVar():
      result = Con4mType(kind: TypeList)
      result.itemType = copyType(t.itemType)
    else:
      return t
  of TypeDict:
    # Right now, we constrain keys to string or int
    if t.hasTypeVar():
      result = Con4mType(kind: TypeDict)
      result.keyType = copyType(t.keyType)
      result.valType = copyType(t.valType)
    else:
      return t
  of TypeTuple:
    if t.hasTypeVar():
      result = Con4mType(kind: TypeTuple)
      for param in t.itemTypes:
        result.itemTypes.add(param.copyType())
    else:
      return t
  of TypeProc:
    if t.hasTypeVar():
      result = Con4mType(kind: TypeProc)
      for param in t.params:
        result.params.add(param.copyType())
      result.va = t.va
      result.retType = t.retType.copyType()
    else:
      return t
  else:
    return t

proc reprSig*(name: string, t: Con4mType): string =
  return name & (($(t))[1 .. ^1])

