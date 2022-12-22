# Thanks very much to ElegantBeef for helping me with my fight against
# type system recursion wonkiness... the lastType implementation was
# due to him.

import std/typetraits
import std/hashes
import strutils
import tables
import json

# This is the clever trick to get the type of the array/seq item...
# call lastType to decompose.
proc lastType[T](a: openarray[T]): auto =
  when T is (seq or array):
    default(T).lastType()
  else:
    default(T)


type
  MixedKind* = enum
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
    of MkObj: # Only used for tables.
      o: RootRef
      baseType: string
      keyType: string
      valType: string
  SomeTableRef = TableRef or OrderedTableRef

  ListCrate = ref object of RootObj
    s: seq[Box]
    typeName: string
    itemType: string

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
    let tc = cast[TableCrate[OrderedTableRef[Box, Box]]](box.o)
    var
      genericParamDummy: genericParams(result.type)
      dstKey: genericParamDummy[0].type()
      dstVal: genericParamDummy[1].type()                                        
      
    when T is OrderedTableRef:
      result = newOrderedTable[genericParamDummy[0].type(),
                               genericParamDummy[1].type()]()
    else:
      result = newTable[genericParamDummy[0].type(),
                        genericParamDummy[1].type()]()
    for k, v in tc.t:
      unpack[genericParamDummy[0].type()](k, dstKey)
      unpack[genericParamDummy[1].type()](v, dstVal)
      result[dstKey] = dstVal
  else:
    raise newException(ValueError, "Bad type to unpack: " & $(T.type))

proc unpack*(box: BoxAtom): auto =
  result = box

proc hash*(box: Box): Hash =
  case box.kind
  of MkInt:
    var unboxed: int
    unpack[int](box, unboxed)
    return hash(unboxed)
  of MkStr:
    var unboxed: string
    unpack[string](box, unboxed)
    return hash(unboxed)
  of MkFloat:
    var unboxed: float
    unpack[float](box, unboxed)
    return hash(unboxed)
  of MkBool:
    var unboxed: bool
    unpack[bool](box, unboxed)
    return hash(unboxed)
  else:
    raise newException(ValueError, "Invalid type for hash key")
    
proc pack*[T](x: T): Box =
  when T is Box:
    result = x
  elif T is SomeInteger:
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
  elif T is seq:
    let c = ListCrate(s: newSeq[Box](), typename: $(x.type))
    result = Box(kind: MkSeq, c: c)
    for item in x:
      c.s.add(pack(item))
  elif T is SomeTableRef:
    let newdict: OrderedTableRef[Box, Box] = newOrderedTable[Box, Box]()
    var kt = ""
    var vt = ""      
    for k, v in x:
      if kt == "":
        kt = $(k.type)
        vt = $(v.type)
      newdict[pack(k)] = pack(v)
    result = Box(kind: MkObj,
                 o: TableCrate[OrderedTableRef[Box, Box]](t: newDict),
                 baseType: "TableRef",
                 keyType: kt,
                 valType: vt)
  else:
    raise newException(ValueError, "Bad type to pack: " & $(T.type))

proc `$`*(x: Box): string =
  case x.kind
  of MkFloat:
    return $(x.f)
  of MkInt:
    return $(x.i)
  of MkObj:
    let c = cast[TableCrate[OrderedTableRef[Box, Box]]](x.o)
    var addComma: bool = false

    result = "{"
    for k, val in c.t:
      if addComma: result = result & ", " else: addComma = true
      result = result & $(k) & " : " & $(val)
    
    result = result & "}"
  of MkBool:
    return $(x.b)
  of MkStr:
    return x.s
  of MkSeq:
    var s: seq[string] = @[]
    for item in x.c.s:
      s.add($(item))
    return "mix[" & s.join(", ") & "]"

proc boxToJson*(b: Box): string =
  var addComma: bool = false
  
  case b.kind
  of MkInt, MkFloat, MkBool:
    return $(b)
  of MkStr:
    return escapeJson($(b))
  of MkSeq:
    result = "["
    for item in b.c.s:
      if addComma: result = result & ", ": else: addComma = true
      result = result & item.boxToJSon()
    result = result & "]"
  of MkObj:
    let
      c = cast[TableCrate[OrderedTableRef[Box, Box]]](b.o)
    for k, val in c.t:
      if addComma: result = result & ", " else: addComma = true
      result = result & boxToJson(k) & " : " & boxToJson(val)
    result = result & "}"

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

  var myDict = newTable[string, seq[string]]()

  
  myDict["foo"] = @["a", "b"]
  myDict["bar"] = @["b"]
  myDict["boz"] = @["c"]
  myDict["you"] = @["d"]

  import streams
  
  let
    f = newFileStream("box.nim", fmRead)
    contents = f.readAll()[0 .. 20]

  myDict["file"] = @[contents]

  let dictBox = pack(myDict)

  let listbox = pack(a)

  var outList = unpack(listbox)

  echo "Here's the listbox: ", listbox
  echo "Here it is unpacked: ", outlist

  var newDict: TableRef[string, seq[string]]

  unpack(dictBox, newDict)

  echo "Here's the dictbox: ", dictBox

  echo "Here it is unpacked: ", newDict

  echo "Here it is, boxed, as Json: ", boxToJson(dictBox)
  

  # import sugar
  # This shoul fail
  # var v: ()->int
  # unpack[()->int](m1, v)
