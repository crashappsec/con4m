import tables

import con4m_types

proc box*(value: string): Box {.inline.} =
  return Box(kind: TypeString, s: value)

proc box*(value: bool): Box {.inline.} =
  return Box(kind: TypeBool, b: value)

proc box*(value: int): Box {.inline.} =
  return Box(kind: TypeInt, i: value)

proc box*(value: float): Box {.inline.} =
  return Box(kind: TypeFloat, f: value)

proc boxList*[T](value: var seq[T]): Box {.inline.} =
  return Box(kind: TypeList, p: cast[pointer](addr(value)))

proc boxDict*[K, V](value: var TableRef[K, V]): Box {.inline.} =
  return Box(kind: TypeDict, p: cast[pointer](addr(value)))

proc unbox*[T](box: Box): T =
  case box.kind
  of TypeString: return cast[T](box.s)
  of TypeBool: return cast[T](box.b)
  of TypeInt: return cast[T](box.i)
  of TypeFloat: return cast[T](box.f)
  else:
    let p = cast[ptr T](box.p)
    return p[]

when isMainModule:
  var
    s = @[1, 2, 3, 4]
    d = {"foo": 32, "bar": 23}.newTable()
    b: Box


  b = boxSeq(s)
  echo unbox[seq[int]](b)

  b = boxDict(d)
  echo unbox[TableRef[string, int]](b)

  # This interface is gone, but I wanted to leave the comment for my
  # own reference, as it is one of NIM's biggest gotchas, right here.
  # Don't instantiate something generic where the LHS is dotted.
  #
  # echo getDict[string, int](n)





