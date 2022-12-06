import tables

import con4m_types

when (NimMajor, NimMinor) > (1, 7):
  {.warning[CastSizes]: off.}

proc box*(value: bool): Box =
  return Box(kind: TypeBool, b: value)

proc box*(value: string): Box =
  return Box(kind: TypeString, s: value)

proc box*(value: int): Box =
  return Box(kind: TypeInt, i: value)

proc box*(value: float): Box =
  return Box(kind: TypeFloat, f: value)

proc box*[T](value: var seq[T]): Box =
  return Box(kind: TypeList, p: cast[pointer](addr(value)))

proc box*[T](value: seq[T]): Box =
  var copy: seq[T] = value

  return Box(kind: TypeList, p: cast[pointer](addr(copy)))

proc boxList*[T](value: var seq[T]): Box =
  var res: seq[Box] = newSeq[Box]()

  when T is seq:
    if len(value) == 0:
      return Box(kind: TypeList, p: cast[pointer](addr(res)))
  else:
    for item in value:
      when T is int:
        res.add(Box(kind: TypeInt, i: item))
      elif T is string:
        res.add(Box(kind: TypeString, s: item))
      elif T is bool:
        res.add(Box(kind: TypeBool, b: item))
      elif T is float:
        res.add(Box(kind: TypeFloat, f: item))
      elif T is seq:
        raise newException("ValueError", "nested boxing not supported yet")

    return Box(kind: TypeList, p: cast[pointer](addr(res)))

proc box*[T](value: var TableRef[T, Box]): Box =
  return Box(kind: TypeDict, p: cast[pointer](addr(value)))

proc boxDict*[K, V](value: var TableRef[K, V]): Box =
  return Box(kind: TypeDict, p: cast[pointer](addr(value)))

proc unbox*[T](box: Box): T =
  case box.kind
  of TypeBool: return cast[T](box.b)
  of TypeInt: return cast[T](box.i)
  of TypeFloat: return cast[T](box.f)
  of TypeString:

    when (NimMajor, NimMinor) >= (1, 7):
      return cast[ptr T](addr(box.s))[]
    else:
      return cast[T](box.s)

  else: return cast[ptr T](box.p)[]

when isMainModule:
  var
    #l = @[1, 2, 3, 4]
    s = @["hi", "ho", "hi", "ho"]
    cray = @[s, s]
    d = {"foo": 32, "bar": 23}.newTable()
    b1, b2, b3, b4: Box
    ub, ub2: seq[Box]
    tub: seq[string] = @[]


  b1 = boxList(cray)
  ub = unbox[seq[Box]](b1)
  echo ub.len()
  for box in ub:
    #ub2 = unbox[seq[string]](box)
    tub = unbox[seq[string]](box)
    echo tub



  # This interface is gone, but I wanted to leave the comment for my
  # own reference, as it is one of NIM's biggest gotchas, right here.
  # Don't instantiate something generic where the LHS is dotted.
  #
  # echo getDict[string, int](n)





