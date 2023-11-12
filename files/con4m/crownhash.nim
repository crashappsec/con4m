## This hash table does require any new thread to call registerThread().
##
## Note:
##
## This doesn't seem to work w/ Orc and I'm not in a hurry to make it
## work either.
##
## Memory management approach:
##
## For most primitive types, we store a 64-bit value that neither Nim
## nor the underlying implementation needs to memory manage. That includes
## all int and float types, bools, enums, etc.
##
## For objects (things that are references and should thus be heap
## allocated in nim) we can GC_ref() them and store them directly.
##
## Strings in nim are generally three-tier. There's a stack-allocated
## value that has the length, and a pointer to a heap-allocated value,
## which contains a capacity and a pointer to the actual c-string.
##
## To deal with this, we create our own heap-allocated data structure
## that keeps the data we need to re-assemble the string when we remove
## it. We also GC_ref() the heap-allocated payload (and unref it when
## ejecting).
##
## We could also ensure a heap-allocated string by sticking it inside
## of a ref object and copying it in, but it's an extra layer of
## indirection for computing the hash value... for strings, we want to
## do that by treating it as a null-terminated C string, not a pointer.
##
## With C strings, we currently bundle them in a Nim string to simplify
## memory management. This may change soon, so don't count on it. Note
## here that, in Nim's FFI, $(x) copies the string, but cstring(s) does
## not.  The 'sink' modifier passes 'ownership'.
##
## For everything else, we'll generally see it stack-allocated, and
## won't necessarily have access to a consistent storage location, even
## if it never leaves the stack. That makes such data objects
## unsuitable for being hash keys (though, we could support custom
## per-type hash functions in the future).
##
## However, we can store such things as *values* by wrapping them in a
## heap allocated object that we then incref. We'll then hold a copy of
## that object.
##
## When anything gets ejected from the hash table other than a
## primitive ordinal or float type, we GC_unref() if it was allocated
## from Nim, and currently ignore otherwise.


import sugar, os, macros, options

{.pragma: hatc, cdecl, importc.}

type
  Dict*[T, V] {. importc: "hatrack_dict_t", header: splitPath(currentSourcePath()).head & "/crownhash.h", nodecl.} = object
  DictRef*[T, V] = ref Dict[T, V]
  DictKeyType = enum
    KTInt, KTFloat, KtCStr, KtPtr, KtObjInt, KtObjReal, KtObjCstr,
    KtObjPtr, KtObjCustom, KtForce32Bits = 0x0fffffff
  StackBox[T] = ref object
    contents:   T
    ownedByNim: bool
  StrBoxObj = object
    data:       cstring
    str:        string
    ownedByNim: bool
  StrBox    = ref StrBoxObj
  StrPayloadCast = object
    cap: int
    data: UncheckedArray[char]
  StrCast = object
    len: int
    p:   ptr StrPayloadCast
  SomeString  = string | cstring # String box.
  SomeRef     = ref or pointer  # Not boxed.
  SomeNumber  = SomeOrdinal or SomeFloat
  RawItem     = object
    key:   pointer
    value: pointer


proc toStrBox(s: string): StrBox =
  var
    outer = cast[StrCast](s)
    inner = cast[ref StrPayloadCast](outer.p)

  new result

  result.data       = cstring(s)
  result.str        = s
  result.ownedByNim = true
  GC_ref(result)

template toStrBox(s: cstring): StrBox =
  toStrBox($(s))

proc unboxStr(s: StrBox): string =
  return s.str

proc ejectStrBox(s: StrBox) =
  if s.ownedByNim:
    GC_unref(s)

proc toStackBox[T](o: T): StackBox[T] =
  result = StackBox[T](contents: o, ownedByNim: true)
  GC_ref(result)

template unboxStackObj[T](box: StackBox[T]): T =
  box.contents

template ejectStackBox[T](s: StackBox[T]) =
  if s.ownedByNim:
    GC_unref(s)

proc hatrack_dict_init(ctx: var Dict, key_type: DictKeyType) {.hatc.}
proc hatrack_dict_delete(ctx: ptr Dict) {.hatc.}
proc hatrack_dict_set_consistent_views(ctx: var Dict, yes: bool) {.hatc.}
proc hatrack_dict_get_consistent_views(ctx: var Dict): bool {.hatc.}
proc hatrack_dict_set_hash_offset(ctx: var Dict, offset: cint) {.hatc.}
proc hatrack_dict_get(ctx: var Dict, key: pointer, found: var bool):
                     pointer {.hatc.}
proc hatrack_dict_put(ctx: var Dict, key: pointer,
                            value: pointer) {.
  hatc.}
proc hatrack_dict_replace(ctx: var Dict, key: pointer, value: pointer):
                     bool {.hatc.}
proc hatrack_dict_add(ctx: var Dict, key: pointer, value: pointer):
                     bool {.hatc.}
proc hatrack_dict_remove(ctx: var Dict, key: pointer): bool {.hatc.}
proc hatrack_dict_keys_sort(ctx: var Dict, n: ptr uint64):
                           pointer {.hatc.}
proc hatrack_dict_values_sort(ctx: var Dict, n: ptr uint64):
                             pointer {.hatc.}
proc hatrack_dict_items_sort(ctx: var Dict, n: ptr uint64):
                            pointer {.hatc.}
proc hatrack_dict_keys_nosort(ctx: var Dict, n: ptr uint64):
                             pointer {.hatc.}
proc hatrack_dict_values_nosort(ctx: var Dict, n: ptr uint64):
                               pointer {.hatc.}
proc hatrack_dict_items_nosort(ctx: var Dict, n: ptr uint64):
                              pointer {.hatc.}
proc hatrack_dict_set_free_handler[T, V](ctx: var Dict[T, V],
                       cb: (var Dict[T, V], ptr RawItem) -> void) {.hatc.}
proc register_thread() {.cdecl, importc: "mmm_register_thread" .}

proc decrefDictItems[T, V](dict: var Dict[T, V], p: ptr RawItem) =
  when T is SomeString:
    ejectStrBox(cast[StrBox](p[].key))
  elif T is ref:
    GC_unref(cast[T](p[].value))

  when V is SomeString:
    ejectStrBox(cast[StrBox](p[].value))
  elif V is ref:
    GC_unref(cast[V](p[].value))
  elif not (V is SomeNumber or V is pointer):
    ejectStackBox(cast[StackBox[V]](p[].value))

once:
  # Auto-register the main thread.
  registerThread()

proc initDict*[T, V](dict: var Dict[T, V]) =
  ## Initialize a Dict.
  when T is SomeOrdinal:
    hatrack_dict_init(dict, KtInt)
  elif T is SomeFloat:
    hatrack_dict_init(dict, KtFloat)
  elif T is SomeString:
    hatrack_dict_init(dict, KtObjCStr)
    hatrack_dict_set_hash_offset(dict, 0)
  elif T is SomeRef:
    hatrack_dict_init(dict, KtPtr)
  else:
    static:
      error("Cannot currently have keys of seq or object types")

  dict.hatrack_dict_set_consistent_views(true)
  when not (T is SomeNumber and V is SomeNumber):
    dict.hatrack_dict_set_free_handler(decrefDictItems)

proc `=destroy`*[T, V](x: Dict[T, V]) =
  ## Calls the underlying C cleanup routine to deallocate everything
  ## specifically allocated in C.
  hatrack_dict_delete(addr x)

proc `[]=`*[T, V](dict: var Dict[T, V], key: T, value: sink V) =
  ## This assigns, whether or not there was a previous value
  ## associated with the passed key.
  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)
  var p: pointer
  when T is SomeString:
    p = cast[pointer](key.toStrBox())
  else:
    p = cast[pointer](key)

  when V is SomeOrdinal:
    dict.hatrack_dict_put(p, cast[pointer](int64(value)))
  elif V is SomeFloat:
    dict.hatrack_dict_put(p, cast[pointer](float(value)))
  elif V is SomeString:
    dict.hatrack_dict_put(p, cast[pointer](value.toStrBox()))
  elif V is ref:
    GC_ref(value)
    dict.hatrack_dict_put(p, cast[pointer](value))
  elif V is pointer:
    dict.hatrack_dict_put(p, cast[pointer](value))
  else:
    dict.hatrack_dict_put(p, cast[pointer](value.toStackBox()))

template `[]=`*[T, V](dict: DictRef[T, V], key: T, value: sink V) =
  `[]=`(dict[], key, value)

proc replace*[T, V](dict: var Dict[T, V], key: T, value: sink V): bool =
  ## This replaces the value associated with a given key.  If the key
  ## has not yet been set, then `false` is returned and no value is
  ## set.
  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  var p: pointer
  when T is SomeString:
    p = cast[pointer](key.toStrBox())
  else:
    p = cast[pointer](key)

  when V is SomeOrdinal:
    return dict.hatrack_dict_replace(p, cast[pointer](int64(value)))
  elif V is SomeFloat:
    return dict.hatrack_dict_replace(p, cast[pointer](float(value)))
  elif V is SomeString:
    return dict.hatrack_dict_replace(p, cast[pointer](value.toStrBox()))
  elif V is ref:
    GC_ref(value)
    return dict.hatrack_dict_replace(p, cast[pointer](value))
  elif V is pointer:
    return dict.hatrack_dict_replace(p, cast[pointer](value))
  else:
    return dict.hatrack_dict_replace(p, cast[pointer](value.toStackBox()))

template replace*[T, V](dict: DictRef[T, V], key: T, value: sink V): bool =
  replace(dict[], key, value)

proc add*[T, V](dict: var Dict[T, V], key: T, value: sink V): bool =
  ## This sets a value associated with a given key, but only if the
  ## key does not exist in the hash table at the time of the
  ## operation.

  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  var p: pointer
  when T is SomeString:
    p = cast[pointer](key.toStrBox())
  else:
    p = cast[pointer](key)

  when V is SomeOrdinal:
    return dict.hatrack_dict_replace(p, cast[pointer](int64(value)))
  elif V is SomeFloat:
    return dict.hatrack_dict_replace(p, cast[pointer](float(value)))
  elif V is SomeString:
    return dict.hatrack_dict_replace(p, cast[pointer](value.toStrBox()))
  elif V is ref:
    GC_ref(value)
    return dict.hatrack_dict_replace(p, cast[pointer](value))
  elif V is pointer:
    return dict.hatrack_dict_replace(p, cast[pointer](value))
  else:
    return dict.hatrack_dict_replace(p, cast[pointer](value.toStackBox()))

template add*[T, V](dict: DictRef[T, V], key: T, value: sink V): bool =
  add(dict[], key, value)

proc lookup*[T, V](dict: var Dict[T, V], key: T): Option[V] =
  ## Retrieve the value associated with a key, wrapping it in
  ## an option. If the key isn't present, then returns `none`.
  ##
  ## See the [] operator for a version that throws an exception
  ## if the key is not present in the table.

  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  var
    found: bool
    p:     pointer

  when T is SomeString:
    p = cast[pointer](key.toStrBox())
  else:
    p = cast[pointer](key)

  var retp = dict.hatrack_dict_get(p, found)

  if found:
    when V is SomeOrdinal:
      var x: int64 = cast[int64](retp)
      result = some(V(x))
    elif V is SomeFloat:
      var x: float = cast[float](retp)
      result = some(V(x))
    elif V is string:
      var box = cast[StrBox](retp)
      result = some(box.unboxStr())
    elif V is cstring:
      var
        box = cast[StrBox](retp)
        str = box.unboxStr()
      result = some(cstring(str))
    elif V is SomeRef:
      # No need to worry about possible incref; the type will cause
      # the right thing to happen here.
      result = some(cast[V](retp))
    else:
      var box = cast[StackBox[V]](retp)
      result = some(box.contents)

proc contains*[T, V](dict: var Dict[T, V], key: T): bool =
  ## In a multi-threaded environment, this shouldn't be used when
  ## there might be any sort of race condition.  Use lookup() instead.
  return dict.lookup(key).isSome()

template contains*[T, V](dict: DictRef[T, V], key: T): bool =
  contains(dict[], key)

proc `[]`*[T, V](dict: var Dict[T, V], key: T) : V =
  ## Retrieve the value associated with a key, or else throws an erro
  ## if it's not present.
  ##
  ## See `lookup` for a version that returns an Option, and thus
  ## will not throw an error when the item is not found.

  var optRet: Option[V] = dict.lookup(key)

  if optRet.isSome():
    return optRet.get()
  else:
    raise newException(KeyError, "Dictionary key was not found.")

template `[]`*[T, V](dict: DictRef[T, V], key: T) : V =
 `[]`(dict[], key)

proc toDict*[T, V](pairs: openarray[(T, V)]): DictRef[T, V] =
  ## Use this to convert a nim {} literal to a Dict.
  result = DictRef[T, V]()
  initDict(result[])
  for (k, v) in pairs:
    result[k] = v

proc newDict*[T, V](): DictRef[T, V] =
  ## Heap-allocate a DictRef
  result = DictRef[T, V]()
  initDict[T, V](result[])

proc del*[T, V](dict: var Dict[T, V], key: T): bool {.discardable.} =
  ## Deletes any value associated with a given key.
  ##
  ## Note that this does *not* throw an exception if the item is not present,
  ## as multiple threads might be attempting parallel deletes. Instead,
  ## if you care about the op succeeding, check the return value.
  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  var
    p: pointer

  when T is SomeString:
    p = cast[pointer](key.toStrBox())
  else:
    p = cast[pointer](key)

  return dict.hatrack_dict_remove(p)

template delete*[T, V](dict: var Dict[T, V], key: T): bool =
  del[T, V](dict, key)

template del*[T, V](dict: DictRef[T, V], key: T): bool =
  del[T, V](dict[], key)

template delete*[T, V](dict: DictRef[T, V], key: T): bool =
  del[T, V](dict[], key)

proc keys*[T, V](dict: var Dict[T, V], sort = false): seq[T] =
  ## Returns a consistent view of all keys in a dictionary at some
  ## moment in time during the execution of the function.
  ##
  ## Note that this is *not* an iterator. This is intentional. The
  ## only way to get a consistent view in a parallel environment is to
  ## create a consistent copy; we already have the copy, so having an
  ## extra layer of cursor state is definitely not needed.
  ##
  ## Memory is cheap and plentyful; you'll survive.

  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  when T is SomeString:
    var p: ptr UncheckedArray[StrBox]
  elif T is SomeOrdinal:
    var p: ptr UncheckedArray[int64]
  elif T is SomeFloat:
    var p: ptr UncheckedArray[float]
  else:
    var p: ptr UncheckedArray[T]

  var
    n: uint64

  if sort:
    p = cast[typeof(p)](hatrack_dict_keys_sort(dict, addr n))
  else:
    p = cast[typeof(p)](hatrack_dict_keys_nosort(dict, addr n))

  for i in 0 ..< n:
    when T is string:
      result.add(unboxStr(p[i]))
    elif T is cstring:
      result.add(cstring(unboxStr(p[i])))
    else:
      result.add(T(p[i]))

template keys*[T, V](dict: DictRef[T, V], sort = false): seq[T] =
  keys[T, V](dict[], sort)

proc values*[T, V](dict: var Dict[T, V], sort = false): seq[V] =
  ## Returns a consistent view of all values in a dictionary at some
  ## moment in time during the execution of the function.
  ##
  ## Note that this is *not* an iterator. This is intentional. The
  ## only way to get a consistent view in a parallel environment is to
  ## create a consistent copy; we already have the copy, so having an
  ## extra layer of cursor state is definitely not needed.
  ##
  ## Memory is cheap and plentyful; you'll survive.

  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  when V is SomeOrdinal:
    var
      p: ptr UncheckedArray[int64]
  elif V is SomeFloat:
    var
      p: ptr UncheckedArray[float]
  elif V is SomeRef:
    var
      p: ptr UncheckedArray[V]
  elif V is SomeString:
    var
      p: ptr UncheckedArray[StrBox]
  else:
    var
      p: ptr UncheckedArray[StackBox[V]]

  var n: uint64

  if sort:
    p = cast[typeof(p)](hatrack_dict_values_sort(dict, addr n))
  else:
    p = cast[typeof(p)](hatrack_dict_values_nosort(dict, addr n))

  for i in 0 ..< n:
    when V is SomeOrdinal or V is SomeFloat or V is SomeRef:
      result.add(V(p[i]))
    elif V is string:
      result.add(unboxStr(p[i]))
    elif V is cstring:
      result.add(cstring(unboxStr(p[i])))
    else:
      result.add(unboxStackObj[V](p[i]))

template values*[T, V](dict: DictRef[T, V], sort = false): seq[V] =
  values[T, V](dict[], sort)

proc items*[T, V](dict: var Dict[T, V], sort = false): seq[(T, V)] =
  ## Returns a consistent view of all key, value pairs in a dictionary
  ## at some moment in time during the execution of the function.
  ##
  ## Note that this is *not* an iterator. This is intentional. The
  ## only way to get a consistent view in a parallel environment is to
  ## create a consistent copy; we already have the copy, so having an
  ## extra layer of cursor state is definitely not needed.
  ##
  ## Memory is cheap and plentyful; you'll survive.

  if not dict.hatrack_dict_get_consistent_views():
    initDict[T, V](dict)

  var
    p:    ptr UncheckedArray[RawItem]
    n:    uint64
    item: tuple[key: T, value: V]

  if sort:
    p = cast[typeof(p)](hatrack_dict_items_sort(dict, addr n))
  else:
    p = cast[typeof(p)](hatrack_dict_items_nosort(dict, addr n))

  for i in 0 ..< n:
    var uncast = p[i]

    when T is string:
      item.key = unboxStr(cast[StrBox](uncast.key))
    elif T is cstring:
      item.key = cstring(unboxStr(cast[StrBox](uncast.key)))
    elif T is SomeOrdinal:
      item.key = T(cast[int64](uncast.key))
    elif T is SomeFloat:
      item.key = T(cast[float](uncast.key))
    else: # T is SomeRef
      item.key = T(uncast.key)

    when V is string:
      item.value = unboxStr(cast[StrBox](uncast.value))
    elif V is cstring:
      item.value = cstring(unboxStr(cast[StrBox](uncast.value)))
    elif V is SomeOrdinal:
      item.value = V(cast[int64](uncast.value))
    elif V is SomeFloat:
      item.value = T(cast[float](uncast.value))
    elif V is SomeRef:
      item.value = cast[V](uncast.value)
    else:
      item.value = unboxStackObj[V](cast[StackBox[V]](uncast.value))

    result.add(item)

template items*[T, V](dict: DictRef[T, V], sort = false): seq[(T, V)] =
  items[T, V](dict[], sort)

proc `$`*[T, V](dict: var Dict[T, V]): string =
  let view = dict.items()
  result = "{ "
  for i, (k, v) in view:
    if i != 0:
      result &= ", "
    when T is SomeString:
      result &= "\"" & $(k) & "\" : "
    else:
      result &= $(k) & " : "
    when V is SomeString:
      result &= "\"" & $(v) & "\""
    else:
      result &= $(v)
  result &= " }"

template `$`*[T, V](dict: DictRef[T, V]): string =
  `$`[T, V](dict[])

proc deepEquals*[T, V](dict1: var Dict[T, V], dict2: var Dict[T, V]): bool =
  ## This operation doesn't make too much sense in most cases; we'll
  ## leave == to default to a pointer comparison (for dictrefs).
  let
    view1 = dict1.items[T, V](sort = true)
    view2 = dict2.items[T, V](sort = true)

  if view1.len() != view2.len():
    return false

  for i in 0 ..< view1.len():
    let
      (k1, v1) = view1[i]
      (k2, v2) = view2[i]
    if k1 != k2 or v1 != v2:
      return false

  return true

template deepEquals*[T, V](dict1: var Dict[T, V], dict2: DictRef[T, V]): bool =
  deepEquals[T, V](dict1, dict2[])

template deepEquals*[T, V](dict1: DictRef[T, V], dict2: var Dict[T, V]): bool =
  deepEquals[T, V](dict1[], dict2)

template deepEquals*[T, V](dict1: DictRef[T, V], dict2: DictRef[T, V]): bool =
  deepEquals[T, V](dict1[], dict2[])

when true:
  var x: DictRef[int, string]
  var y: Dict[int, string]
  x = {42: "bar", 1000 : "zork", 17 : "foo", 500: "boz"}.toDict()

  y[500]  = "boz"
  y[1000]  = "zork"
  y[17] = "foo"
  y[42] = "bar"

  echo x[42]
  echo x[17]
  x[17] = "blah"
  y[17] = "blah"
  echo x[17]
  for i in 1..1000:
    x[17] = $i
    y[17] = x[17]
    echo y[17]
    if i mod 2 == 1:
      x.del(17)
      y.del(17)

  echo x.keys()
  echo x.values()
  echo x.items()

  echo y.keys()
  echo y.values()
  echo y.items()
  var d2: DictRef[string, int] = newDict[string, int]()
  var seqstr = ["ay", "bee", "cee", "dee", "e", "eff", "gee", "h", "i", "j"]

  for i, item in seqstr:
    d2[item] = i
    echo d2[item]

  echo d2.keys()
  echo d2.values()
  echo d2.items()
  echo d2.keys(sort = true)
  echo d2.values(sort = true)
  echo d2.items(sort = true)

  var d3 = newDict[string, array[10, string]]()
  for item in seqstr:
    d3[item] = seqstr

  echo d3[seqstr[0]]

  echo x
  echo y
  echo d2
  echo d3
  quit()
