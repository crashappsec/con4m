## A type to allow us to store any value we might need for con4m, and
## defer type checking on those values (generally not until runtime,
## generally until later in the process).
##
## I know that Nim does have an `Any` type, but this is simpler, and
## focused on the types that con4m allows.  Part of the reason I built
## my own here is because I intend to cache hash codes here
## eventually, so that dictionary keys can be arbitrary types.
##
## Plus, Nim Any types require some of the work here to avoid dangling
## references to sequences in particular.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import tables

import ./types


when (NimMajor, NimMinor) > (1, 7):
  {.warning[CastSizes]: off.}

proc box*(value: bool): Box =
  ## Converts a boolean value to a box object.
  return Box(kind: TypeBool, b: value)

proc box*(value: string): Box =
  ## Converts a string value to a box object.
  return Box(kind: TypeString, s: value)

proc box*(value: int): Box =
  ## Converts an int value to a box object.  Note that con4m only
  ## allows the natural int type (usually 64 bit signed).
  return Box(kind: TypeInt, i: value)

proc box*(value: float): Box =
  ## Converts a float value to a box object.
  return Box(kind: TypeFloat, f: value)

proc boxList*[T](value: seq[T]): Box =
  ## Converts a sequence to a box object.
  let empty = if len(value) != 0: false else: true
  var listbox = ListBox[T](contents: value, empty: empty)
  return Box(kind: TypeList, l: cast[RootRef](listbox))

proc boxDict*[K, V](value: TableRef[K, V]): Box =
  ## Converts a Tableref to a box object.  This has to be named,
  ## because Nim doesn't seem to be able to distinguish between this
  ## and box[T] with dictionaries, even though they're generic types
  ## w/ two type parameters :)
  let empty = if len(value) != 0: false else: true
  var dictbox = DictBox[K, V](contents: value, empty: empty)
  return Box(kind: TypeDict, d: cast[RootRef](dictbox))

proc unbox*[T](box: Box): T =
  ## Generically unboxes any data type.

  when T is Box:
    return box
  else:
    case box.kind
    of TypeBool: return cast[T](box.b)
    of TypeInt: return cast[T](box.i)
    of TypeFloat: return cast[T](box.f)
    of TypeString:
      when (NimMajor, NimMinor) >= (1, 7):
        return cast[ptr T](addr(box.s))[]
      else:
        return cast[T](box.s)
    of TypeTVar:
      return cast[T](box)
    else:
      raise newException(ValueError, "Invalid box type for unboxing")

proc unboxList*[T](box: Box): seq[T] =
  let l: ListBox[T] = cast[ListBox[T]](box.l)

  return l.contents

proc unboxDict*[K, V](box: Box): TableRef[K, V] =
  let d: DictBox[K, V] = cast[DictBox[K, V]](box.d)
  return d.contents

  # This interface is gone, but I wanted to leave the comment for my
  # own reference, as it is one of NIM's biggest gotchas, right here.
  # Don't instantiate something generic where the LHS is dotted.
  #
  # echo getDict[string, int](n)


