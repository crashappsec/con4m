import ../common
export common

# These all assume we're using 64 bit storage. This won't work
# on smaller word sizes.


type
  ReprFn*     = proc(p: pointer): string {.cdecl.}
  GetCastFn*  = proc(dt: DataType, err: var string): pointer {.cdecl.}
  CastFn*     = proc(cur: pointer): pointer {.cdecl.}
  BoolRetFn*  = proc(l, r: pointer): bool {.cdecl.}
  BinOpFn*    = proc(l, r: pointer): pointer {.cdecl.}
  IndexFn*    = proc(c: pointer, i: int, err: var bool): pointer {.cdecl.}
  DIndexFn*   = proc(c, i: pointer, err: var bool): pointer {.cdecl.}
  SliceFn*    = proc(c: pointer, i, j: int): pointer {.cdecl.}
  AssignIxFn* = proc(c, v: pointer, i: int, err: var bool) {.cdecl.}
  SetDixFn*   = proc(c, v, i: pointer, err: var bool) {.cdecl.}
  ASliceFn*   = proc(c, v: pointer, i, j: int, err: var bool) {.cdecl.}
  NewLitFn*   = proc(s: pointer, st: SyntaxType, litmod: string,
                     err: var string): pointer {.cdecl.}
  ClitFn*     = proc(st: SyntaxType, litmod: string, t: TypeId,
                     contents: seq[pointer], err: var string):
                     pointer {.cdecl.}
  CopyFn*     = proc(a: pointer): pointer {.cdecl.}
  LenFn*      = proc(a: pointer): int {.cdecl.}
  PlusEqFn*   = proc(l, r: pointer) {.cdecl.}

  SyntaxInfo* = object
    primary*: DataType
    litmods*: seq[(string, DataType)]

var
  dataTypeInfo*: seq[DataType]
  dtNameMap*:    Dict[string, DataType]
  refCount*:     int
  syntaxInfo*:   array[int(StMax), SyntaxInfo]
  allBiNames =   @["dict", "tuple", "struct", "ref", "set", "maybe", "oneof",
                   "typespec"]
  numConcrete:   int

dtNameMap.initDict()


proc idFromTypeName*(n: string): TypeId =
  return TypeId(dtNameMap[n].dtid)

proc getAllBuiltinTypeNames*(): seq[string] =
  once:
    allBiNames &= dtNameMap.keys(sort=true)

  return allBiNames

proc addDataType*(name: string, concrete: bool, ops: seq[pointer],
                  byValue = false, intW = 0, signed = false, fTy = false,
                  isBool = false, aliases: seq[string] = @[],
                  ckind = C4None, signedVariant = TypeId(0)): TypeId {.cdecl.} =
  result = TypeId(dataTypeInfo.len())

  var v: DataType = nil

  if signedVariant != 0:
    v = dataTypeInfo[signedVariant]

  var dt = DataType(name: name, dtid: result, concrete: concrete,
                    isBool: isBool, intW: intW, signed: signed,
                    signedVariant: v, fTy: fTy, aliases: aliases,
                    byValue: byValue, ops: ops, ckind: ckind)

  dtNameMap[name] = dt

  if concrete:
    numConcrete += 1

  for alias in dt.aliases:
    dtNameMap[alias] = dt

  dataTypeInfo.add(dt)

template numBuiltinTypes*(): int =
  numConcrete

proc registerSyntax*(dtid: TypeId, syntax: SyntaxType, litmods: seq[string],
                     primary = false) =
  let dt = dataTypeInfo[dtid]

  if primary:
    syntaxInfo[int(syntax)].primary = dt

  syntaxInfo[int(syntax)].litmods.add((dt.name, dt))

  for item in litmods:
    syntaxInfo[int(syntax)].litmods.add((item, dt))

let
  TBottom* = addDataType("none (type error)", true, @[])
  TVoid*   = addDataType("void", true, @[])


proc newRefValue*[T](item: T, dtid: TypeId): pointer =
  let o = RefValue[T](dtInfo: dataTypeInfo[dtid], item: item,
                      refCount: 1)

  GC_ref(o)

  return cast[pointer](o)

proc extractRef*[T](item: pointer, decref = false): T =
  let o = cast[RefValue[T]](item)
  if decref:
    GC_unref(o)
  return o.item

proc incref*[T](item: T) =
  item.refCount += 1
  GC_ref(item)

proc decref*[T](item: T) =
  item.refCount -= 1
  GC_unref(item)

template newVTable*(): seq[pointer] =
  newSeq[pointer](int(FMax))

proc cast_identity*(pre: pointer): pointer {.cdecl, exportc.} =
  result = pre

proc cast_to_bool*(pre: pointer): pointer {.cdecl, exportc.} =
  if pre != nil:
    return cast[pointer](1)

proc value_eq*(a, b: pointer): bool {.cdecl.} =
  return a == b

proc value_lt*(a, b: pointer): bool {.cdecl.} =
  return a < b

proc value_gt*(a, b: pointer): bool {.cdecl.} =
  return a > b

proc parseInt128*(s: string, res: var uint128, sign: var bool): int {.cdecl.} =
  # When we're calling this from the lexer, sign will never appear,
  # because it treats that as a separate token.
  var
    i = 0
    last: uint128

  if s.len() == 0:
    return -1
  if s[0] == '-':
    sign = true
    i += 1
    if i == len(s):
      return -3
  else:
    sign = false

  while i < s.len():
    res *= 10
    let n = byte(s[i]) - byte('0')
    if n < 0 or n > 9:
      return -2
    res += iToU128(uint16(n))
    i += 1
    if res < last:  # overflow.
      return -1
    last = res

  if sign:
    if res > iToU128(high(int)) or (sign and res == iToU128(-low(int))):
      return 16
    elif res > iToU128(high(int32)) or (sign and res == iToU128(-low(int32))):
      return 8
    elif res > iToU128(high(int8)) or (sign and res == iToU128(-low(int8))):
      return 4
    else:
      return 1
  else:
    if res > high(uint):
      return 16
    elif res > high(uint32):
      return 8
    elif res > high(uint8):
      return 4
    else:
      return 1

proc tinfo*(t: TypeId): DataType =
  let
    n = int(t)

  if n < dataTypeInfo.len():
    return dataTypeInfo[n]

proc box*[T](item: T, t: TypeId): pointer =
  let info = tinfo(t)

  if info == nil or not info.byValue:
    return newRefValue[T](item, t)
  else:
    return cast[pointer](item)

proc call_repr*(value: pointer, t: TypeId): string =
  let
    info = tinfo(t)
    op   = cast[ReprFn](info.ops[FRepr])

  if op == nil:
    return "?"

  return op(value)


# proc call_get_val_for_ffi*(p: pointer, ffitype: int)

# TODO when we go back to Hatrack
# var
#  arrayOps: seq[pointer]
#  ringOps: seq[pointer]
#  stackOps: seq[pointer]
#  setOps: seq[pointer]

# let TArray* = addDataTypeInfo(DataType(name:     "array",
#                                        syntax:   sqBrOk,
#                                        litmods:  @["a"],
#                                        byValue:  false,
#                                        ops:      arrayOps))

# let TRing* = addDataTypeInfo(DataType(name:     "ring",
#                                       syntax:   sqBrOk,
#                                       litMods:  @["r"],
#                                       byValue:  false,
#                                       ops:      ringOps))

# let TStack* = addDataTypeInfo(DataType(name:     "stack",
#                                        syntax:   sqBrOk,
#                                        litMods:  @["s"],
#                                        byValue:  false,
#                                        ops:      stackOps))

# let TSet* =  addDataTypeInfo(DataType(name:     "set",
#                                       syntax:   curlBrOk,
#                                       litMods:  @["s"],
#                                       byValue:  false,
#                                       ops:      setOps))

# TODO:
# 1. Migrate instances of parseLiteral() and initializeCharLiteral() to
#    the above interfance.
# 3. intBits call in irgen?  Most of that should be rm'able?
# 4. Fold needs to get rid of the use of mixed.
# 6. Merge isBasicType with isBuiltinType
# 7. Redo the setIntVariants to user the alias info.
# 8. Merge (for types) tostr and tostring
