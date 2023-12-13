import unicode

type
  MixedContainer[T] = ref object of RootRef
    item: T

  MixedKind = enum MixedValue, MixedReference

  Mixed* = object
    case kind: MixedKind
    of MixedValue:
      byVal: pointer
    of MixedReference:
      byRef: RootRef

proc toMixed*[T](item: T): Mixed =
  when T is SomeOrdinal or T is SomeFloat or T is Rune:
    result = Mixed(kind: MixedValue, byVal: cast[pointer](item))
  else:
    result = Mixed(kind: MixedReference, byRef: MixedContainer[T](item: item))

proc toVal*[T](item: Mixed): T =
  when T is SomeOrdinal or T is SomeFloat or T is Rune:
    result = cast[T](item.byVal)
  else:
    result = MixedContainer[T](item.byRef).item

when isMainModule:
  let
    f = toMixed("foo bar")
    g = toMixed(12703219)


  echo toVal[string](f)
  echo toVal[int](g)
