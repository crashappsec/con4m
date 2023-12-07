## Base for data types used across the project.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import nimutils, typeinfo
export nimutils, typeinfo

const
  noRepr*       = 0
  scalarOk*     = 1
  hexOk*        = 2
  floatOk*      = 4
  boolOk*       = 8
  strQuotesOk*  = 16
  charQuotesOk* = 32
  euroQuotesOk* = 64
  stdBoolKind*  = (boolOk)
  stdIntKind*   = (scalarOk or hexOk)
  stdFloatKind* = (scalarOk or floatOk)
  stdStrKind*   = strQuotesOk
  stdChrKind*   = (scalarOk or hexOk or charQuotesOk)
  stdOtherKind* = (strQuotesOk or euroQuotesOk)

type
  SyntaxType* = enum
    STBase10    = scalarOk,
    STHex       = hexOk,
    STFloat     = floatOk,
    STBoolLit   = boolOk,
    STStrQuotes = strQuotesOk,
    STChrQuotes = charQuotesOk,
    STOther     = euroQuotesOk,
    STList      = 128,
    STDict      = 256,
    STTuple     = 512

  C4TypeKind* = enum
    C4TVar, C4List, C4Dict, C4Tuple, C4TypeSpec, C4Ref, C4Maybe,
    C4Func, C4Struct, C4OneOf, C4Primitive

  TypeRef*  = ref object
    typeid*:   TypeId
    isLocked*: bool
    items*:    seq[TypeId]
    case kind*: C4TypeKind
    of C4TVar:
      tvarId*:    TypeId
      localName*: Option[string]
    of C4Func:
      va*: bool
    of C4Struct:
      name*:  string
      props*: Dict[string, TypeId]
    else:
      discard

  Callback* = object
    # Right now, this doesn't even stash a pointer; we could cache
    # this, but we accept callbacks that aren't provided, so we
    # currently just defer until runtime to look up the function
    # anyway.  Also helps make it easy to handle the case where a
    # function's entry is dynamically replaced via a stack.
    #
    # This probably should get moved to its own module.
    # It's a value for a ref to a function.
    name*: string
    tid*:  TypeId

  TypeConstructor* = proc (i0: string, i1: var Any, i2: SyntaxType):
                         string {.cdecl.}
  # i0 -> the starting type; modifiable.
  # i1 -> the literal to update.
  ContainerConstructor* = proc (i0: var TypeId, i1: var Any): string {.cdecl.}
  CharInitFn*           = proc (i0: uint, i1: var Any): string
  TypeId*               = uint64
  TypeInfo* = ref object
    name*:        string
    kind*:        uint
    litmods*:     seq[string]
    typeId*:      TypeId
    fromRawLit*:  TypeConstructor
    fromCharLit*: CharInitFn

var
  basicTypes*: seq[TypeInfo]
  tiMap*:      Dict[TypeId, TypeInfo]
  nameMap*:    Dict[string, TypeInfo]

proc addBasicType*(name:        string,
                   kind:        uint,
                   litmods:     seq[string] = @[],
                   fromRawLit:  TypeConstructor = nil,
                   fromCharLit: CharInitFn = nil): TypeId  =

  var tInfo = TypeInfo(name: name, kind: kind, litMods: litMods,
                       fromRawLit: fromRawLit, fromCharLit: fromCharLit)

  result        = TypeId(basicTypes.len())
  tinfo.typeId  = result
  tiMap[result] = tInfo
  nameMap[name] = tInfo

  basicTypes.add(tInfo)

proc getTypeIdFromSyntax*(st: SyntaxType, litMod: string,
                            err: var string): TypeId =
  ## Only should be called if a litmod is supplied w/ a literal;
  ## This uses the syntax type and litmod to find the type name.
  ## i.e., do not call it with an empty litmod.
  var
    bestErr = "Invalid literal modifier: '" & litMod & "'"

  for v in basicTypes:
    if litMod in v.litMods:
      if v.kind != 0 and uint(st) != 0:
        return v.typeId
      else:
        bestErr = "Literal modifier '" & litMod & "' is not valid for "
        case st
        of STBase10:
          besterr &= "integer literals"
        of STHex:
          besterr &= "hex literals"
        of STFloat:
          besterr &= "float literals"
        of STBoolLit:
          besterr &= "boolean literals"
        of STStrQuotes:
          besterr &= "string literals"
        of STChrQuotes:
          besterr &= "character literals"
        of STOther:
          besterr &= "specialized literals"
        else:
          unreachable # Not supposed to call this w/ complex types

  err = bestErr

proc getBuiltinTypeIdFromName*(name: string, err: var string): TypeId =
  let opt = nameMap.lookup(name)
  if opt.isNone():
    err = "Unknown built-in type: '" & name & "'"
  else:
    result = opt.get().typeId

var
  listMods: Dict[string, ContainerConstructor]
  dictMods: Dict[string, ContainerConstructor]
  tupMods:  Dict[string, ContainerConstructor]

listMods.initDict()
dictMods.initDict()
tupMods.initDict()

proc registerListMod*(name: string, cons: ContainerConstructor) =
  listMods[name] = cons

proc registerDictMod*(name: string, cons: ContainerConstructor) =
  dictMods[name] = cons

proc registerTupMod*(name: string, cons: ContainerConstructor) =
  tupMods[name] = cons

proc listModExists*(name: string): bool =
  return listMods.lookup(name).isSome()

proc dictModExists*(name: string): bool =
  return dictMods.lookup(name).isSome()

proc tupModExists*(name: string): bool =
  return tupMods.lookup(name).isSome()

proc applyListMod*(name: string, tinfo: var TypeId, val: var Any): string =
  return listMods[name](tinfo, val)

proc applyDictMod*(name: string, tinfo: var TypeId, val: var Any): string =
  return dictMods[name](tinfo, val)

proc applyTupMod*(name: string, tinfo: var TypeId, val: var Any): string =
  return tupMods[name](tinfo, val)

let
  TBottom* = addBasicType(name = "bottom",
                          kind = noRepr)
  TVoid*   = addBasicType(name = "void",
                          kind = noRepr)

proc parseLiteral*(typeId: int, raw: string, err: var string,
                  st: SyntaxType): Any =
  ## Returns the parsed literal wrapped in an `Any`.
  ## Assumes you know your typeId definitively by now.
  ##
  ## If `err` is not empty, then ignore the return value.
  var ti = basicTypes[typeId]

  err = ti.fromRawLit(raw, result, st)

proc initializeCharLiteral*(typeId: int, cp: uint, err: var string): Any =
  ## Returns the initialized, error-checked literal wrapped in an `Any`.
  ## Assumes you know your typeId definitively by now.
  ##
  ## If `err` is not empty, then ignore the return value.

  var ti = basicTypes[typeId]

  err = ti.fromCharLit(cp, result)

var
  biTypeNames: seq[string]
  allTypeIds:  seq[string]

proc typeNameFromId*(id: TypeId): string =
  return biTypeNames[int(id)]

proc idFromTypeName*(n: string): TypeId =
  return TypeId(biTypeNames.find(n))

proc getAllBuiltinTypeNames*(): seq[string] =
  if biTypeNames.len() > 0:
    return biTypeNames

  for item in basicTypes:
    biTypeNames.add(item.name)

  allTypeIds = biTypeNames
  allTypeIds &= ["list", "dict", "tuple", "struct", "ref", "set",
                 "maybe", "oneof", "typespec"]

  return biTypeNames

proc getAllTypeIdentifiers*(): seq[string] =
  if allTypeIds.len() == 0:
    discard getAllBuiltinTypeNames()

  return allTypeIds

proc numBuiltinTypes*(): int =
  return biTypeNames.len()
