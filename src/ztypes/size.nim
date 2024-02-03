import std/parseutils
import "."/base

type Size* = uint64

var szOps = newVTable()

proc repr_size(m: pointer): string {.cdecl.} =
  # TODO: do better!
  return $(cast[uint64](m)) & " bytes"

proc new_size(s: string, st: SyntaxType, lmod: string,
              l: var int, err: var string): pointer {.exportc, cdecl.} =
  l = 8

  var
    letterix = 0
    multiple = 1'u64

  if len(s) == 0:
    err = "BadSize"
    return

  for i in 0 ..< len(s):
    if s[i] notin '0' .. '9':
      letterix = i
      break
  if letterix < 1:
    err = "BadSize"
    return


  case unicode.strip(s[letterix .. ^1])
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
    err = "BadSize"
    return
  try:
    var
      intpart = s[0 ..< letterix]
      sz: int
    discard intpart.parseInt(sz, 0)
    var value: Size = uint64(sz) * multiple
    return cast[pointer](value)
  except:
    err = "BadSize"
    return

szOps[FRepr]   = cast[pointer](repr_size)
szOps[FEq]     = cast[pointer](value_eq)
szOps[FLt]     = cast[pointer](value_lt)
szOps[FGt]     = cast[pointer](value_gt)
szOps[FNewLit] = cast[pointer](new_size)

let TSize* = addDataType(name = "size", concrete = true, byValue = true,
                         ops = szOps)

registerSyntax(TSize, STOther, @["sz"])
registerSyntax(TSize, STStrQuotes, @["sz"])
