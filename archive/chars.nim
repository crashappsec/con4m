import std/unicode
import ".."/ffi

proc char_is_num(arg: uint32): bool {.exportc, cdecl.} =
  if arg >= uint32('0') and arg <= uint32('9'):
    return true
  else:
    return false

proc char_is_alpha_num(arg: Rune): bool {.exportc, cdecl.} =
  return cast[uint32](arg).char_is_num() or arg.isAlpha()

proc char_is_space(arg: Rune): bool {.exportc, cdecl.} =
  return unicode.isSpace($arg)

proc char_is_alpha(arg: Rune): bool {.exportc, cdecl.} =
  return unicode.isAlpha(arg)

# The data type is mostly in ordinal.nim for the moment.
addStaticFunction("char_rune_len", unicode.size)
addStaticFunction("char_is_lower", unicode.isLower)
addStaticFunction("char_is_upper", unicode.isUpper)
addStaticFunction("char_is_combining", unicode.isCombining)
addStaticFunction("char_is_space", char_is_space)
addStaticFunction("char_is_alpha", char_is_alpha)
addStaticFunction("char_is_num", char_is_num)
addStaticFunction("char_is_alpha_num", char_is_alpha_num)
