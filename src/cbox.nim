import common

proc newCbox*[T](item: T, tid: TypeId): CBox =
  result.v = toMixed[T](item)
  result.t = tid

template toBox*[T](item: T, tid: TypeId): CBox =
  newCBox[T](item, tid)

proc unBox*[T](b: CBox): T =
  # Currently unchecked.
  return toVal[T](b.v.toVal)

template getType*(x: CBox): TypeId =
  x.t.followForwards()
