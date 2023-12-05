# Types that accept << >> literals automatically accept 
# string literals with their type name as a lit mod.
#
# Also, << >> should ideally be able to unambiguously parse across
# multiple literal types, but the priority field takes care of that.

import nimutils, unicode, typeinfo

const hardcodedTypeIds* = [
  "bottom", "void", "bool", "int", "uint", "int32", "uint32", "char", "byte",
  "float", "string", "buffer", "utf8", "utf32", "Duration", "Ipv4", "Ipv6", 
  "Cidr", "Size", "Date", "Time", "DateTime", "Path"
]

type 
  TypeConstructor* = proc (i0: var string, i1: var Any): bool {.cdecl.}

  TypeInfo* = ref object
    # First two fields are only indended for int types.
    bits*:       int   # If signed, 0x100 is set. If a char type, 0x200 is set.
    alias*:      string
    litmods*:    seq[string]
    typeId*:     int
    fromRawLit*: TypeConstructor


  C4PrimitiveTypes* = enum 
    TBottom = 0, TVoid = 1, TBool = 2, TInt = 3, TUint = 4, TInt32 = 5, 
    TUint32 = 6, TChar = 7, TByte = 8, TFloat = 9, TString = 10,
    TBuffer = 11, TUtf8 = 12, TUtf32 = 13, TDuration = 14, TIpv4 = 15, 
    TIpv6 = 16, TCidr = 17, TSize = 18, TDate = 19, TTime = 20,
    TDateTime = 21, TPath = 22


proc constructRope(s: var string, outObj: var Any) : bool {.cdecl.}  =
  var r: Rope = text(s)
  GC_ref(r)
  outObj = r.toAny()
  return true
  
proc constructUtf32(s: var string, outObj: var Any): bool {.cdecl.} =
  var runes: seq[Rune]
  try:
    runes  = s.toRunes()
    outObj = runes.toAny()
    return true
  except:
    return false

proc constructUtf8(s: var string, outObj: var Any): bool {.cdecl.} =
  result = s.validateUtf8() == -1
  outObj = s.toAny()

proc constructBuf(s: var string, outObj: var Any): bool {.cdecl.}  =
  outObj = s.toAny()
  return true

proc constructBool(s: var string, outObj: var Any): bool {.cdecl.} =
  case s
  of "True", "true":
    outObj = toAny(true)
  of "False", "false":
    outObj = toAny(false)
  else:
    return false
  return true

proc constructInt(s: var string, outObj: var Any): bool {.cdecl.} =
  
    
var 
  typeInfo: Dict[string, TypeInfo] = { 
    "bool":     TypeInfo(typeId: int(TBool), fromRawLit: constructBool),
    "int" :     TypeInfo(bits: 0x40, alias: "int64",
                        litmods: @["i64", "int64"], typeId: int(TInt)),
    "uint":     TypeInfo(bits: 0x140, alias: "uint64", 
                        litmods: @["u64", "'uint64"], typeId: int(TUint)),
    "int32":    TypeInfo(bits: 0x20, litmods: @["i32", "int32"], 
                         typeId: int(TInt32)),
    "uint32":   TypeInfo(bits: 0x120, litmods: @["u32", "uint32"], 
                          typeId: int(TUint32)),
    "char":     TypeInfo(bits: 0x220, litmods: @["char", "c"],
                        typeId: int(TChar)),
    "byte":     TypeInfo(bits: 0x208, litmods: @["byte", "b"], 
                        typeId: int(TByte)),
    "float":    TypeInfo(typeId: int(TFloat),
    "string":   TypeInfo(litMods: @["r"], typeId: int(TString),
                          fromRawLit: constructRope),
    "buffer":   TypeInfo(litMods: @["b"], typeId: int(TBuffer),
                          fromRawLit: constructBuf),
    "utf8":     TypeInfo(litMods: @["u", "u8", "utf8"], typeId: int(TUtf8),
                          fromRawLit: constructUtf8),
    "utf32":    TypeInfo(litMods: @["u32", "utf32"], typeId: int(TUtf32),
                        fromRawLit: constructUtf32),
    "path":     TypeInfo(litMods: @["p", "path"], typeId: int(TPath),
                          fromRawLit: constructBuf),
    "Duration": TypeInfo(litMods: @["Duration", "duration"], 
                              typeId: int(TDuration)),
    "Ipv4":     TypeInfo(litMods: @["ipv4"], typeId: int(TIpv4)),
    "Ipv6":     TypeInfo(litMods: @["ipv6"], typeId: int(TIpv6)),
    "Cidr":     TypeInfo(litMods: @["cidr"], typeId: int(TCidr)),
    "Size":     TypeInfo(litMods: @["size"], typeId: int(TSize)),
    "Date":     TypeInfo(litMods: @["date"], typeId: int(TDate)),
    "Time":     TypeInfo(litMods: @["time"], typeId: int(TTime)),
    "DateTime": TypeInfo(litMods: @["datetime"], typeId: int(TDateTime))
    }.toDict()[]

proc getTypeFromLitMod*(litMod: string): int =
  if litMod == "":
    return int(TString)

  let objOpt = typeInfo.lookup(litMod)
  if objOpt.isSome():
    return objOpt.get().typeId

  for (k, v) in typeInfo.items():
    if litMod in v.litmods:
      return v.typeId

  return int(TBottom)
 
proc getTypeFromName*(n: string): int =
  let objOpt = typeInfo.lookup(n)
  if objOpt.isSome():
    return objOpt.get().typeId

  for (k, v) in typeInfo.items():
    if n == v.alias:
      return v.typeId
    
  return int(TBottom)

proc getValueFromRawStr*(tid: int, n: string): Any =
  
  
