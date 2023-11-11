# Memory management approach:
#
# For most primitive types, we store a 64-bit value that neither Nim
# nor the underlying implementation needs to memory manage. That includes
# all int and float types, bools, enums, etc.
#
# For objects (things that are references and should thus be heap
# allocated in nim) we can GC_ref() them and store them directly.
#
# Strings in nim are generally three-tier. There's a stack-allocated
# value that has the length, and a pointer to a heap-allocated value,
# which contains a capacity and a pointer to the actual c-string.
#
# To deal with this, we create our own heap-allocated data structure
# that keeps the data we need to re-assemble the string when we remove
# it. We also GC_ref() the heap-allocated payload (and unref it when
# ejecting).
#
# We could also ensure a heap-allocated string by sticking it inside
# of a ref object and copying it in, but it's an extra layer of
# indirection for computing the hash value... for strings, we want to
# do that by treating it as a null-terminated C string, not a pointer.
#
# With C strings, we currently bundle them in a Nim string to simplify
# memory management. This may change soon, so don't count on it. Note
# here that, in Nim's FFI, $(x) copies the string, but cstring(s) does
# not.  The 'sink' modifier passes 'ownership'.
#
# For everything else, we'll generally see it stack-allocated, and
# won't necessarily have access to a consistent storage location, even
# if it never leaves the stack. That makes such data objects
# unsuitable for being hash keys (though, we could support custom
# per-type hash functions in the future).
#
# However, we can store such things as *values* by wrapping them in a
# heap allocated object that we then incref. We'll then hold a copy of
# that object.
#
# When anything gets ejected from the hash table other than a
# primitive ordinal or float type, we GC_unref().



import sugar, os

{.pragma: hatc, cdecl, importc.}

type
  DictObj*[T, V] {. importc: "hatrack_dict_t", header: splitPath(currentSourcePath()).head & "/crownhash.h", nodecl.} = object
  Dict*[T, V] = ref DictObj[T, V]
  DictKeyType = enum
    KTInt, KTFloat, KtCStr, KtPtr, KtForce32Bits = 0x0fffffff
  SomeBox[T] = ref object of RootRef
    contents:   T
    ownedByNim: bool
type
  StrBoxObj = object
    data:       ptr UncheckedArray[char]
    pldptr:     ptr StrPayloadCast
    len:        int
    ownedByNim: bool
  StrBox    = ptr StrBoxObj
  StrPayloadCast = object
    cap: int
    data: UncheckedArray[char]
  StrCast = object
    len: int
    p:   ptr StrPayloadCast

proc toStrBox(s: string): StrBox =
  var
    outer = cast[StrCast](s)
    inner = cast[ref StrPayloadCast](outer.p)

  result            = cast[StrBox](allocShared0(sizeof(StrBox)))
  result.data       = addr inner.data
  result.pldptr     = outer.p
  result.len        = outer.len
  result.ownedByNim = true
  GC_ref(inner)

proc unboxStr(s: ptr StrBoxObj): string =
  # Nim doesn't want to let us cast `addr result` directly to a
  # `ptr StrCast`, thus the temporary. Have tried a few differen
  # ways, and this is the only way that seems to work.
  var
    tmp:    ptr string  = addr result
    pre:    ptr StrCast = cast[ptr StrCast](tmp)

  pre[].len = s.len
  pre[].p   = s.pldptr

proc ejectStrBox(s: ptr StrBoxObj) =
  GC_unref(cast[ref StrPayloadCast](s[].pldptr))
  deallocShared(s)

var mmm = toStrBox("Hello, world!")
echo "Step 1 complete."
echo unboxStr(mmm)
ejectStrBox(mmm)




proc hatrack_dict_init(ctx: var DictObj, key_type: DictKeyType) {.hatc.}
proc hatrack_dict_delete(ctx: ptr DictObj) {.hatc.}
proc hatrack_dict_set_consistent_views(ctx: var DictObj, yes: bool) {.hatc.}
proc hatrack_dict_get(ctx: var DictObj, key: pointer, found: var bool):
                     pointer {.hatc.}
proc hatrack_dict_put(ctx: var DictObj, key: pointer,
                            value: pointer) {.
  hatc.}
proc hatrack_dict_replace(ctx: var DictObj, key: pointer, value: pointer):
                     bool {.hatc.}
proc hatrack_dict_add(ctx: var DictObj, key: pointer, value: pointer):
                     bool {.hatc.}
proc hatrack_dict_remove(ctx: var DictObj, key: pointer): bool {.hatc.}
proc hatrack_dict_keys_sort(ctx: var DictObj, n: ptr uint64):
                           pointer {.hatc.}
proc hatrack_dict_values_sort(ctx: var DictObj, n: ptr uint64):
                             pointer {.hatc.}
proc hatrack_dict_items_sort(ctx: var DictObj, n: ptr uint64):
                            pointer {.hatc.}
proc hatrack_dict_keys_nosort(ctx: var DictObj, n: ptr uint64):
                             pointer {.hatc.}
proc hatrack_dict_values_nosort(ctx: var DictObj, n: ptr uint64):
                               pointer {.hatc.}
proc hatrack_dict_items_nosort(ctx: var DictObj, n: ptr uint64):
                              pointer {.hatc.}
proc hatrack_dict_set_free_handler[T, V](ctx: var DictObj[T, V],
                       cb: (var DictObj[T, V], RootRef) -> void) {.hatc.}
proc register_thread() {.cdecl, importc: "mmm_register_thread" .}

proc decrefDictValue[T, V](dict: var DictObj[T, V], val: RootRef) =
  GC_unref(val)

once:
  registerThread()

proc initDict*[T, V](dict: var DictObj[T, V]) =
  when T is SomeOrdinal:
    hatrack_dict_init(dict, KtInt)
  elif T is SomeFloat:
    hatrack_dict_init(dict, KtFloat)
  elif T is string or T is cstring:
    hatrack_dict_init(dict, KtCStr)
  else:
    hatrack_dict_init(dict, KtPtr)

  dict.hatrack_dict_set_consistent_views(true)
  when V is SomeOrdinal or V is SomeFloat:
    discard
  else:
    dict.hatrack_dict_set_free_handler(decrefDictValue)

proc `=destroy`*[T, V](x: DictObj[T, V]) =
    hatrack_dict_delete(addr x)

proc `[]=`*[T, V](dict: var DictObj[T, V], key: T, value: sink V) =
    var p: pointer
    when T is string:
      p = cast[pointer](cstring(key))
    else:
      p = cast[pointer](key)

    when V is SomeOrdinal:
      dict.hatrack_dict_put(p, cast[pointer](int64(value)))
    elif V is SomeFloat:
      dict.hatrack_dict_put(p, cast[pointer](float(value)))
    elif V is cstring:
      dict.hatrack_dict_put(p, cast[pointer](value))
    else:
      var obj = SomeBox[V](contents: value)
      GC_ref(obj)
      dict.hatrack_dict_put(p, cast[pointer](obj))

template `[]=`*[T, V](dict: Dict[T, V], key: T, value: sink V) =
    `[]=`(dict[], key, value)

proc `[]`*[T, V](dict: var DictObj[T, V], key: T) : V =
  var
    found: bool
    p:     pointer

  when T is string:
    p = cstring(key)
  else:
    p = cast[pointer](key)

  var retp = dict.hatrack_dict_get(p, found)

  if found:
    when V is SomeOrdinal:
      var x: int64 = cast[int64](retp)
      result = V(x)
    elif V is SomeFloat:
      var x: float = cast[float](retp)
      result = V(x)
    elif V is cstring:
      result = cast[V](retp)
    else:
      var box = cast[SomeBox[V]](retp)
      result = box.contents
  else:
    raise newException(KeyError, "Dictionary key was not found.")

template `[]`*[T, V](dict: Dict[T, V], key: T) : V =
 `[]`(dict[], key)

proc toDict*[T, V](pairs: openarray[(T, V)]): Dict[T, V] =
  result = Dict[T, V]()
  initDict(result[])
  for (k, v) in pairs:
    result[k] = v

proc newDict*[T, V](): Dict[T, V] =
  result = Dict[T, V]()
  initDict[T, V](result[])

proc delete*[T, V](dict: var DictObj[T, V], key: T) =
  var
    p: pointer

  when T is string:
    p = cstring(key)
  else:
    p = cast[pointer](key)

  if not dict.hatrack_dict_remove(p):
    raise newException(KeyError, "Item was not in dictionary.")

template delete*[T, V](dict: Dict[T, V], key: T) =
  delete[T, V](dict[], key)

proc keys*[T, V](dict: var DictObj[T, V], sort = false): seq[T] =
  when T is string or T is cstring:
    var p: ptr UncheckedArray[cstring]
  elif T is SomeOrdinal:
    var p: ptr UncheckedArray[int64]
  elif T is SomeFloat:
    var p: ptr UncheckedArray[float]
  else:
    var p: ptr UncheckedArray[ptr T]

  var
    n: uint64
    o: T

  if sort:
    p = cast[typeof(p)](hatrack_dict_keys_sort(dict, addr n))
  else:
    p = cast[typeof(p)](hatrack_dict_keys_nosort(dict, addr n))

  for i in 0 ..< n:
    when T is string:
      var f: cstring = p[i]
      result.add($f)
    elif T is SomeOrdinal or T is SomeFloat or T is cstring:
      result.add(T(p[i]))
    else:
      var o = SomeBox[T](p[i])
      result.add(o.contentS)

template keys*[T, V](dict: var Dict[T, V], sort = false): seq[T] =
  keys[T, V](dict[], sort)

proc values*[T, V](dict: var DictObj[T, V], sort = false): seq[V] =
  when V is SomeOrdinal:
    var
      p: ptr UncheckedArray[int64]
      o: V
  elif V is SomeFloat:
    var
      p: ptr UncheckedArray[float]
      o: V
  elif V is ref or V is pointer:
    var
      p: ptr UncheckedArray[V]
      o: V
  else:
    var
      p: ptr UncheckedArray[SomeBox[V]]
      o: SomeBox[V]

  var n: uint64

  if sort:
    p = cast[typeof(p)](hatrack_dict_values_sort(dict, addr n))
  else:
    p = cast[typeof(p)](hatrack_dict_values_nosort(dict, addr n))

  for i in 0 ..< n:
    when V is SomeOrdinal or V is SomeFloat or V is cstring:
      result.add(V(p[i]))
    else:
      o = SomeBox[V](p[i])
      result.add(V(o.contents))

template values*[T, V](dict: var Dict[T, V], sort = false): seq[V] =
  values[T, V](dict[], sort)

type GenericItem = object
  key:   pointer
  value: pointer

proc items*[T, V](dict: var DictObj[T, V], sort = false): seq[(T, V)] =
  var
    p:    ptr UncheckedArray[GenericItem]
    n:    uint64
    item: tuple[key: T, value: V]

  if sort:
    p = cast[typeof(p)](hatrack_dict_items_sort(dict, addr n))
  else:
    p = cast[typeof(p)](hatrack_dict_items_nosort(dict, addr n))

  for i in 0 ..< n:
    var uncast = p[i]

    when T is string or T is cstring:
      item.key = $(cast[cstring](uncast.key))
    elif T is SomeOrdinal:
      var tmp: int64 = cast[int64](uncast.key)
      item.key = T(tmp)
    elif T is SomeFloat:
      var tmp: float = cast[float](uncast.key)
      item.key = T(tmp)
    else:
      item.key = T(uncast.key)

    when V is string or V is seq or V is ref or V is pointer:
      var box = cast[SomeBox[V]](uncast.value)
      item.value = box.contents
    elif V is SomeOrdinal:
      var x: int64 = cast[int64](uncast.value)
      item.value = V(x)
    elif V is SomeFloat:
      var x: float = cast[float](uncast.value)
      item.value = V(x)
    else:
      item.value = cast[V](uncast.value)

    result.add(item)

template items*[T, V](dict: var Dict[T, V], sort = false): seq[(T, V)] =
  items[T, V](dict[], sort)

when true:
  var x: Dict[int, string]
  var y: DictObj[int, string]
  x = {42: "bar", 1000 : "zork", 17 : "foo", 500: "boz"}.toDict()
  y.initDict()
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
      x.delete(17)
      y.delete(17)

  echo x.keys()
  echo x.values()
  echo x.items()

  echo y.keys()
  echo y.values()
  echo y.items()
  var d2: Dict[string, int] = newDict[string, int]()


  for i, item in ["ay", "bee", "cee", "dee", "e", "eff", "gee", "h", "i", "j"]:
    d2[item] = i

  echo d2.keys()
  echo d2.values()
  echo d2.items()
  echo d2.keys(sort = true)
  echo d2.values(sort = true)
  echo d2.items(sort = true)
  quit()
