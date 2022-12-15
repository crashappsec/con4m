## Our implementation of the good ol' unifcation algorithm, with
## Con4m's current type rules codified.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import ./types
import st

# Just in case someone manages to clone a singleton, we
# always check against the .kind field, instead of looking at
# object equivolence for singletons (e.g., int, bottom)
proc unify*(t1: Con4mType, t2: Con4mType): Con4mType =
  case t1.kind
  of TypeString, TypeBool, TypeInt, TypeFloat:
    if t2.kind == t1.kind: return t1
    if t2.kind == TypeTVar:
      return t1
    return bottomType
  of TypeTuple:
    if t2.kind == TypeTVar:
      return t1
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
    if t2.kind == TypeTVar:
      return t1
    if t2.kind != TypeList: return bottomType
    let containedType = t1.itemType.unify(t2.itemType)
    if containedType == bottomType: return bottomType
    return newListType(containedType)
  of TypeDict:
    if t2.kind == TypeTVar:
      return t1
    if t2.kind != TypeDict: return bottomType
    let kt = t1.keyType.unify(t2.keyType)
    let vt = t1.valType.unify(t2.valType)
    if kt.kind == TypeBottom or vt.kind == TypeBottom: return bottomType
    return newDictType(kt, vt)
  of TypeTVar: return t2
  of TypeBottom: return t1
  of TypeProc:
    case t2.kind
    of TypeTVar: return t1
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

proc isBottom*(t: Con4mType): bool =
  return t.kind == TypeBottom

proc isBottom*(t1, t2: Con4mType): bool =
  return unify(t1, t2).isBottom()

proc unify*(n1, n2: Con4mNode): Con4mType =
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
    return true
  # Proc types must be concrete.
  of TypeBool, TypeString, TypeInt, TypeFloat, TypeProc, TypeBottom:
    return false
  of TypeList:
    return hasTypeVar(t.itemType)
  of TypeTuple:
    for item in t.itemTypes:
      if hasTypeVar(item):
        return true
    return false
  of TypeDict:
    return hasTypeVar(t.keyType) or hasTypeVar(t.keyType)
