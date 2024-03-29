import std/strutils

proc find_string_at*(mem: string, offset: int): string =
  let endIx = mem.find('\0', offset)
  return mem[offset ..< endIx]
