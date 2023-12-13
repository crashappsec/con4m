# I was using a Stream abstraction here, but streams won't marshall
# and we will need them to to be able to support suspension and
# resumption.
#
# Plus, I'd prefer to keep UTF32 instead of UTF8.
import common
export common

proc newStringCursor*(s: string): StringCursor =
  result = StringCursor(runes: s.toRunes(), i: 0)

template peek*(cursor: StringCursor, skip = 0): Rune =
  if cursor.i + skip >= cursor.runes.len():
    Rune(0)
  else:
    cursor.runes[cursor.i + skip]

proc read*(cursor: StringCursor): Rune =
  if cursor.i >= cursor.runes.len():
    return Rune(0)
  else:
    result = cursor.runes[cursor.i]
    cursor.i += 1

template advance*(cursor: StringCursor) =
  cursor.i += 1

template getPosition*(cursor: StringCursor): int = cursor.i

proc setPosition*(cursor: StringCursor, i: int) =
  cursor.i = i

template slice*(cursor: StringCursor, startIx, endIx: int): seq[Rune] =
  cursor.runes[startIx ..< endIx]

proc `$`*(s: StringCursor): string =
  echo s.i
  echo s.runes.len()
  echo s.runes
  result = $(s.runes)

proc toRope*(s: StringCursor): Rope =
  return paragraph(Rope(kind: RopeAtom, length: s.runes.len(), text: s.runes))
