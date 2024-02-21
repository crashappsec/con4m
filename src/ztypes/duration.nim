import std/[posix, parseutils]
import "."/base

type Duration* = ref Timeval

var durOps = newVTable()

proc repr_duration(dur: Duration): string {.cdecl.} =
  # TODO: do better.
  result = $(cast[int32](dur.tv_sec)) & " sec"

  if dur.tv_usec != 0:
    result &= " " & $(dur.tv_usec) & " usec"

proc eq_duration(d1, d2: Duration): bool {.cdecl.} =
  let
    r = memcmp(cast[pointer](d1), cast[pointer](d2), csize_t(sizeof(Timeval)))

  return r == 0

proc new_duration(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer {.cdecl.} =
  var
    parts: seq[(string, string)] = @[]
    s                            = unicode.strip(s)
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
      err = "BadDuration"
      return
    numPart = s[startix ..< ix]
    while ix < len(s) and s[ix] == ' ': ix += 1
    if ix == len(s):
      err = "BadDuration"
      return
    startix = ix
    while ix < len(s):
      case s[ix]
      of 'a'..'z', 'A'..'Z':
        ix = ix + 1
      else:
        break
    if startix == ix:
      err = "BadDuration"
      return
    parts.add((numPart, s[startix ..< ix]))
    while ix < len(s):
      case s[ix]
      of ',':
        ix = ix + 1
      of '0'..'9', ' ':
        break
      else:
        err = "BadDuration"
        return
    if startix == ix:
      err = "BadDuration"
      return
    while ix < len(s) and s[ix] == ' ':
      ix = ix + 1
  if len(parts) == 0:
    err = "BadDuration"
    return
  for (numAsString, unitStr) in parts:
    try:
      discard numAsString.parseInt(parsedInt, 0)
    except:
      err = "BadDuration"
      return
    case unitStr.toLower()
    of "us", "usec", "usecs":
      if seenUsec:
        err = "BadDuration"
        return
      else:
        seenUsec = true
      duration += uint64(parsedInt)
    of "ms", "msec", "msecs":
      if seenMsec:
        err = "BadDuration"
        return
      else:
        seenMSec = true
      duration += uint64(parsedInt * 1000)
    of "s", "sec", "secs", "seconds":
      if seenSec:
        err = "BadDuration"
        return
      else:
        seenSec = true
      duration += uint64(parsedInt * 1000000)
    of "m", "min", "mins", "minutes":
      if seenMin:
        err = "BadDuration"
        return
      else:
        seenMin = true
      duration += uint64(parsedInt * 1000000 * 60)
    of "h", "hr", "hrs", "hours":
      if seenHr:
        err = "BadDuration"
        return
      else:
        seenHr = true
      duration += uint64(parsedInt * 1000000 * 60 * 60)
    of "d", "day", "days":
      if seenDay:
        err = "BadDuration"
        return
      else:
        seenDay = true
      duration += uint64(parsedInt * 1000000 * 60 * 60 * 24)
    of "w", "wk", "wks", "week", "weeks":
      if seenWeek:
        err = "BadDuration"
        return
      else:
        seenWeek = true
      duration += uint64(parsedInt * 1000000 * 60 * 60 * 24 * 7)
    of "y", "yr", "yrs", "year", "years":
      if seenYear:
        err = "BadDuration"
        return
      else:
        seenYear = true
      duration += uint64(parsedInt * 1000000 * 60 * 60 * 24 * 365)
    else:
      err = "BadDuration"
      return

  result  = alloc(sizeof(TimeVal))
  var res = cast[Duration](result)

  res.tv_usec = int32(duration mod 1000000)
  res.tv_sec  = Time(duration div 1000000)

durOps[FRepr]   = cast[pointer](repr_duration)
durOps[FEq]     = cast[pointer](eq_duration)
durOps[FNewLit] = cast[pointer](new_duration)

let TDuration* = addDataType(name = "duration", concrete = true, ops = durOps)
registerSyntax(TDuration, STOther, @[])
registerSyntax(TDuration, STStrQuotes, @[])
