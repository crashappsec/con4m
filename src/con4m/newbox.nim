# Thanks very much to ElegantBeef for helping me with my fight against
# type system recursion wonkiness... the lastType implementation was
# due to him.

import std/typetraits
import strutils
import tables

# This is the clever trick to get the type of the array/seq item...
# call lastType to decompose.
proc lastType[T](a: openarray[T]): auto =
  when T is (seq or array):
    default(T).lastType()
  else:
    default(T)

type
  MixedKind = enum
    MkInt, MkStr, MkObj, MkFloat, MkSeq, MkBool
  BoxAtom = string or int64 or float64 or RootRef or Box
  BoxHoldsAtoms = concept x
    lastType(x) is BoxAtom
  BoxArray = BoxHoldsAtoms and (array or seq)
  Box* = ref object
    case kind*: MixedKind
    of MkFloat: f: float64
    of MkInt: i: int64
    of MkBool: b: bool
    of MkStr: s: string
    of MkSeq: c: ListCrate
    of MkObj:
      o: RootRef
      typename: string
  SomeTableRef = TableRef or OrderedTableRef or CountTableRef

  ListCrate = ref object of RootObj
    s: seq[Box]
    typename: string

  TableCrate[T: SomeTableRef] = ref object of RootObj
    t: T

proc lastType(a: BoxAtom): BoxAtom = a

proc unpack*[T](box: BoxArray, result: var seq[T]) =
  for entry in box.items:
    when typeof(box[0]) is BoxAtom:
      result.add(entry)
    else:
      entry.unpack(result)

proc unpack*[T](box: BoxHoldsAtoms, result: var seq[T]) =
  result.add(box)

proc unpack*(box: BoxHoldsAtoms): auto =
  result = newSeq[typeof(box.lastType)]()
  box.unpack(result)

proc unpack*[T](box: Box, result: var T) =
  when typeof(result) is string:
    result = box.s
  elif typeof(result) is SomeFloat:
    result = cast[T](box.f)
  elif typeof(result) is SomeInteger:
    result = cast[T](box.i)
  elif typeof(result) is RootRef:
    result = cast[T](box.o)
  elif typeof(result) is Box:
    result = box
  elif typeof(result) is seq:
    result = newSeq[typeof(result.lastType)]()
    for item in box.c.s:
      var x: typeof(result.lastType)
      unpack[typeof(result.lastType)](item, x)
      result.add(x)
  elif typeof(result) is SomeTableRef:
    let tc: TableCrate[T] = cast[TableCrate[T]](box.o)
    result = tc.t
  else:
    raise newException(ValueError, "Bad type to unpack: " & $(T.type))

proc unpack*(box: BoxAtom): auto =
  result = box

proc pack*[T](x: T): Box =
  when T is SomeInteger:
    result = Box(kind: MkInt, i: cast[int64](x))
  elif T is SomeFloat:
    result = Box(kind: MkFloat, f: cast[float64](x))
  elif T is bool:
    result = Box(kind: MkBool, b: x)
  elif T is string:
    result = Box(kind: MkStr, s: x)
  elif T is RootRef:
    result = Box(kind: MkObj, o: x, typename: $(x.type))
  elif T is seq[Box]:
    let c = ListCrate(s: newSeq[Box](), typename: $(x.type))
    result = Box(kind: MkSeq, c: c)
    for item in x:
      c.s.add(item)
  elif T is SomeTableRef:
    result = Box(kind: MkObj,
                 o: TableCrate[typeof(x)](t: x),
                 typename: $(x.type))
  else:
    raise newException(ValueError, "Bad type to pack: " & $(T.type))
    

proc `$`*(x: Box): string =
  case x.kind
  of MkFloat:
    return $(x.f)
  of MkInt:
    return $(x.i)
  of MkObj:
    return x.typename
  of MkBool:
    return $(x.b)
  of MkStr:
    return x.s
  of MkSeq:
    var s: seq[string] = @[]
    for item in x.c.s:
      s.add($(item))
    return "mix[" & s.join(", ") & "]"

when isMainModule:
  var
    a = @[@[@[10i64, 20, 30], @[1i64, 2, 2]], @[@[1000i64, 3000, 4, 5]]]
    b = 100i64
    c = 300f64
    m1 = pack(100)
    m2 = pack(200)
    m3 = pack(@[m1, m2])
    m4 = pack(@[m2, m2, m3])
    w = @[[m4, m3, m2], [m4, m3, m1]]


  echo a.unpack()
  echo b.unpack()
  echo c.unpack()
  echo "m1 = ", m1
  echo "m2 = ", m2
  echo "m3 = ", m3
  echo "m4 = ", m4
  echo "w: ", w.unpack()
  echo typeof(w)
  echo typeof(w.unpack())
  echo typeof(m3)

  var outseq: seq[int]
  unpack(m3, outseq)

  echo outseq

  var myDict = newTable[string, int]()

  myDict["foo"] = 12

  let dictBox = pack(myDict)

  var newDict: TableRef[string, int]

  echo dictBox
  unpack(dictBox, newDict)

  echo newDict

  # import sugar
  # This shoul fail
  # var v: ()->int
  # unpack[()->int](m1, v)
