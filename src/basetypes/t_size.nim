import parseutils, ../common

type   Size* = uint64

proc constructSize(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.} =
  var
    letterix = 0
    multiple = 1'u64

  if len(s) == 0:
    return "Invalid value for Size data type."

  for i in 0 ..< len(s):
    if s[i] notin '0' .. '9':
      letterix = i
      break
  if letterix < 1:
    return "Invalid value for Size data type."
  case s[letterix .. ^1].strip()
  of "b", "B", "Bytes", "bytes":
    multiple = 1
  of "k", "K", "kb", "Kb", "KB":
    multiple = 1000
  of "ki", "Ki", "kib", "KiB", "KIB":
    multiple = 1024
  of "m", "M", "mb", "Mb", "MB":
    multiple = 1000000
  of "mi", "Mi", "mib", "MiB", "MIB":
    multiple = 1048576
  of "g", "G", "gb", "Gb", "GB":
    multiple = 1000000000
  of "gi", "Gi", "gib", "GiB", "GIB":
    multiple = 1073741824
  of "t", "T", "tb", "Tb", "TB":
    multiple = 1000000000000'u64
  of "ti", "Ti", "tib", "TiB", "TIB":
    multiple = 1099511627776'u64
  else:
    return "Invalid value for Size data type."
  try:
    var
      intpart = s[0 ..< letterix]
      sz: int
    discard intpart.parseInt(sz, 0)
    var value: Size = uint64(sz) * multiple
    outObj = value.toMixed()
  except:
    return "Invalid value for Size data type."

proc repr(id: TypeId, m: Mixed): string {.cdecl.} =
  # TODO: do better!
  return $(toVal[uint64](m)) & " bytes"

let
  TSize*     = addBasicType(name        = "size",
                            repr        = repr,
                            kind        = stdOtherKind,
                            litMods     = @["size"],
                            fromRawLit  = constructSize)
