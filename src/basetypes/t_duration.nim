import posix, parseutils, ../common

type Duration* = Timeval

proc constructDuration(s: string, outObj: var Mixed, st: SyntaxType):
                      string {.cdecl.} =
  const err = "Invalid duration literal."
  var
    parts: seq[(string, string)] = @[]
    s                            = s.strip()
    startix                      = 0
    ix                           = 0
    duration                     = 0'u64  # In microseconds.
    seenUsec                     = false
    seenMsec                     = false
    seenSec                      = false
    seenMin                      = false
    seenHr                       = false
    seenDay                      = false
    seenWeek                     = false
    seenYear                     = false
    numPart: string
    parsedInt: int

  while ix < len(s):
    startix = ix
    while ix < len(s):
      if s[ix] notin '0'..'9':
        break
      ix += 1
    if startix == ix:
      return err
    numPart = s[startix ..< ix]
    while ix < len(s) and s[ix] == ' ': ix += 1
    if ix == len(s):
      return err
    startix = ix
    while ix < len(s):
      case s[ix]
      of 'a'..'z', 'A'..'Z':
        ix = ix + 1
      else:
        break
    if startix == ix:
      return err
    parts.add((numPart, s[startix ..< ix]))
    while ix < len(s):
      case s[ix]
      of ',':
        ix = ix + 1
      of '0'..'9', ' ':
        break
      else:
        return err
    if startix == ix:
      return err
    while ix < len(s) and s[ix] == ' ':
      ix = ix + 1
  if len(parts) == 0:
    return err
  for (numAsString, unitStr) in parts:
    try:
      discard numAsString.parseInt(parsedInt, 0)
    except:
      return err
    case unitStr.toLower()
    of "us", "usec", "usecs":
      if seenUsec:
        return err
      else:
        seenUsec = true
      duration += uint64(parsedInt)
    of "ms", "msec", "msecs":
      if seenMsec:
        return err
      else:
        seenMSec = true
      duration += uint64(parsedInt * 1000)
    of "s", "sec", "secs", "seconds":
      if seenSec:
        return err
      else:
        seenSec = true
      duration += uint64(parsedInt * 1000000)
    of "m", "min", "mins", "minutes":
      if seenMin:
        return err
      else:
        seenMin = true
      duration += uint64(parsedInt * 1000000 * 60)
    of "h", "hr", "hrs", "hours":
      if seenHr:
        return err
      else:
        seenHr = true
      duration += uint64(parsedInt * 1000000 * 60 * 60)
    of "d", "day", "days":
      if seenDay:
        return err
      else:
        seenDay = true
      duration += uint64(parsedInt * 1000000 * 60 * 60 * 24)
    of "w", "wk", "wks", "week", "weeks":
      if seenWeek:
        return err
      else:
        seenWeek = true
      duration += uint64(parsedInt * 1000000 * 60 * 60 * 24 * 7)
    of "y", "yr", "yrs", "year", "years":
      if seenYear:
        return err
      else:
        seenYear = true
      duration += uint64(parsedInt * 1000000 * 60 * 60 * 24 * 365)
    else:
      return err

  var res: Duration

  res.tv_usec = int32(duration mod 1000000)
  res.tv_sec  = Time(duration div 1000000)

  outObj = res.toMixed()

proc repr(id: TypeId, m: Mixed): string {.cdecl.} =
  # TODO: do better.
  var dur = toVal[Duration](m)

  result = $(cast[int32](dur.tv_sec)) & " sec"

  if dur.tv_usec != 0:
    result &= " " & $(dur.tv_usec) & " usec"

proc durEq(a, b: CBox): bool {.cdecl.} =
  var
    d1 = toVal[Duration](a.v)
    d2 = toVal[Duration](b.v)

  let r = memcmp(cast[pointer](d1), cast[pointer](d2), csize_t(sizeof(Timeval)))

  return r == 0

let
  TDuration* = addBasicType(name        = "duration",
                            repr        = repr,
                            kind        = stdOtherKind,
                            fromRawLit  = constructDuration,
                            eqFn        = durEq)
