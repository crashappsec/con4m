# For dates, we assume that it might make sense for people to only
# provide one of the three items, and possibly two. Year and day of
# month without the month probably doesn't make sense often, but
# whatever.
#
# But even the old ISO spec doesn't accept all variations (you can't
# even do year by itself. When the *year* is omitted, we use the *old*
# ISO format, in hopes that it will be recognized by most software.
# Specifically, depending on a second omission, the format will be:
# --MM-DD
# --MM
# ---DD
#
# However, if the year is provided, we will instead turn omitted
# numbers into 0's, because for M and D that makes no semantic sense
# (whereas it does for Y), so should be unambiguous and could give the
# right results depending on the checking native libraries do when
# parsing.
#
# Note that we also go the ISO route and only accept 4-digit
# dates. And, we don't worry about negative years. They might hate me
# in the year 10,000, but I don't think there are enough cases where
# someone needs to specify "200 AD" in a config file to deal w/ the
# challenges with not fixing the length of the year field.
import std/[posix, parseutils]
import "."/base

type
  DTFlags* = enum
    DTHaveTime, DtHaveSecond, DTHaveFracSec, DTHaveMonth, DTHaveYear,
    DTHaveDay, DTHaveDoy, DTHaveDST, DTHaveOffset

  DateTimeObj* = object
    dt*:       Tm
    fracsec*:  uint
    tzoffset*: int
    flags*:    set[DTFlags]

  DateTime* = ref DateTimeObj

proc usWrittenDate(lit: string, res: var DateTime): bool =
  var
    monthPart = ""
    dayPart   = ""
    yearPart  = ""
    s         = lit
    ix        = 0
    startix   = 0
    day, year: int

  if len(s) == 0:
    return false
  while ix < len(s):
    case s[ix]
    of 'a' .. 'z', 'A' .. 'Z':
      ix += 1
    else:
      break
  if ix == startix:
    return false

  monthPart = s[startix ..< ix]
  while ix < len(s):
    if s[ix] == ' ':
      ix += 1
    else:
      break
  startix = ix
  while ix < len(s):
    case s[ix]
    of '0' .. '9':
      ix += 1
    of ' ', ',':
      break
    else:
      return false
  if startix != ix:
    dayPart = s[startIx ..< ix]
  if ix < len(s) and s[ix] == ',':
    ix += 1
  while ix < len(s):
    if s[ix] != ' ':
      break
    ix += 1
  startix = ix
  while ix < len(s):
    if s[ix] notin '0'..'9':
      break
    ix += 1
  if ix != len(s):
    return false
  yearPart = s[startix ..< ix]
  if len(daypart) == 4 and len(yearpart) == 0:
    yearpart = daypart
    daypart  = ""

  case monthpart.toLower()
  of "jan", "january":
    res.dt.tm_mon = 0;
  of "feb", "february":
    res.dt.tm_mon = 1;
  of "mar", "march":
    res.dt.tm_mon = 2;
  of "apr", "april":
    res.dt.tm_mon = 3;
  of "may":
    res.dt.tm_mon = 4;
  of "jun", "june":
    res.dt.tm_mon = 5;
  of "jul", "july":
    res.dt.tm_mon = 6;
  of "aug", "august":
    res.dt.tm_mon = 7;
  of "sep", "sept", "september":
    res.dt.tm_mon = 8;
  of "oct", "october":
    res.dt.tm_mon = 9;
  of "nov", "november":
    res.dt.tm_mon = 10
  of "dec", "december":
    res.dt.tm_mon = 11
  else:
    return false
  res.flags = res.flags + { DTHaveMonth }

  try:
    if len(daypart) != 0:
      discard daypart.parseInt(day, 0)
      res.flags = res.flags + { DTHaveDay }
      res.dt.tm_mday = int32(day)
    if len(yearpart) != 0:
      discard yearpart.parseInt(year, 0)
      res.flags = res.flags + { DTHaveYear }
      res.dt.tm_year = int32(year - 1900)
  except:
    return false

  if day > 31:
    return false
  if res.dt.tm_mon == 00 and day >= 30:
    return false
  if day == 31 and res.dt.tm_mon in [3, 5, 8, 10]:
    return false

  return true

proc otherWrittenDate(lit: string, res: var DateTime): bool =
  var
    dayPart   = ""
    s         = lit
    ix        = 0
    startix   = 0

  while ix < len(s):
    if s[ix] notin '0'..'9':
      break
    ix += 1
  if ix == 0:
    return false
  dayPart = s[0 ..< ix]
  while ix < len(s):
    if s[ix] != ' ':
      break
    ix += 1
  startix = ix
  while ix < len(s):
    if s[ix] notin 'a' .. 'z' and s[ix] notin 'A' .. 'Z':
      break
    ix += 1
  if startix == ix:
    return false

  return usWrittenDate(s[startix ..< ix] & " " & dayPart & s[ix .. ^1], res)

proc isoDateTime(lit: string, res: var DateTime): bool =
  var
    s = lit
    m = 0
    d = 0
    y = 0

  if len(s) == 4:
    if s[0] != '-' or s[1] != '-':
      return false
    if s[2] notin '0' .. '1' or s[3] notin '0' .. '9':
      return false
    m = (int(s[2]) - int('0')) * 10 + int(s[3]) - int('0')
    if m > 12:
      return false
    res.dt.tm_mon = int32(m - 1)
    res.flags = res.flags + { DTHaveMonth }
    return true

  elif len(s) == 7:
    if s[0] != '-' or s[1] != '-' or s[4] != '-':
      return false
    if s[2] notin '0' .. '1' or s[3] notin '0' .. '9':
      return false
    m = (int(s[2]) - int('0')) * 10 + int(s[3]) - int('0')
    if m > 12:
      return false
    if s[5] notin '0' .. '3' or s[6] notin '0' .. '9':
      return false
    d = (int(s[5]) - int('0')) * 10 + int(s[6]) - int('0')
    if d > 31:
      return false
    if m == 2 and d > 29:
      return false
    if d == 31 and m in [0, 6, 9, 11]:
      return false
    res.dt.tm_mon  = int32(m - 1)
    res.dt.tm_mday = int32(d)
    return true

  elif len(s) == 8:
    # This should be the more rare case, and the format we output.
    s = s[0 .. 3] & '-' & s[4 .. 5] & '-' & s[6 .. 7]
  elif len(s) != 10:
    return false

  if s[4] != '-' or s[7] != '-':
    return false
  for i in 0 .. 3:
    if s[0] notin '0' .. '9':
      return false
  if s[5] notin '0' .. '1' or s[6] notin '0' .. '9':
    return false
  m = (int(s[5]) - int('0')) * 10 + int(s[6]) - int('0')
  if m > 12:
    return false
  if s[8] notin '0' .. '3' or s[9] notin '0' .. '9':
    return false
  d = (int(s[8]) - int('0')) * 10 + int(s[9]) - int('0')
  if d > 31:
    return false
  if m == 2 and d > 29:
    return false
  if d == 31 and m in [0, 6, 9, 11]:
    return false

  discard s[0 .. 3].parseInt(y, 0)

  res.dt.tm_mday = int32(d)
  res.dt.tm_mon  = int32(m - 1)
  res.dt.tm_year = int32(y - 1900)
  res.flags = res.flags + { DTHaveMonth, DTHaveDay, DTHaveYear }
  return true

proc otherLitToNativeDate*(lit: string, res: var DateTime): bool =
  result = lit.isoDateTime(res)
  if not result:
    result = lit.usWrittenDate(res)
  if not result:
    result = lit.otherWrittenDate(res)

proc otherLitToNativeTime*(lit: string, res: var DateTime): bool =
  var
    hr, min, sec:   int
    s:              string = lit
    n:              int

  # We'll tolerate missing leading zeros, but only for the hours field.
  if len(s) < 2 or s[1] == ':':
    s = "0" & s
  if len(s) < 5:
    return false
  if s[0] notin '0' .. '2':
    return false
  if s[1] notin '0' .. '9' or s[2] != ':':
    return false
  if s[3] notin '0' .. '5' or s[4] notin '0' .. '9':
    return false
  hr  = (int(s[0]) - int('0')) * 10 + int(s[1]) - int('0')
  min = (int(s[3]) - int('0')) * 10 + int(s[4]) - int('0')
  res.flags = res.flags + { DTHaveTime }

  sec = 0
  block iso:
    if len(s) > 5:
      res.flags = res.flags + { DTHaveSecond }
      s = s[5 .. ^1]
      case s[0]
      of ':':
        s = s[1 .. ^1]
        if len(s) == 0: return false
        elif len(s) == 1: s = "0" & s
        if s[0] notin '0' .. '6' or s[1] notin '0' .. '9':
          return false
        sec = (int(s[0]) - int('0')) * 10 + int(s[1]) - int('0')
        s   = s[2 .. ^1]
        if len(s) != 0:
          # We will accept EITHER a real standard fractsec OR a more
          # coloquial AM/PM, but not both.
          case s[0]
          of 'a', 'A', 'p', 'P':
            break iso
          of '.':
            if len(s) == 1:
              return false
            res.flags = res.flags + { DTHaveFracSec }
            s = s[1 .. ^1]
            var fs: uint = 0
            while len(s) > 0 and s[0] in '0' .. '9':
              fs *= 10
              fs += (uint(s[0]) - uint('0'))
              s = s[1 .. ^1]
            res.fracsec = fs
          else:
            discard
          if len(s) != 0:
            if len(s) == 1:
              if s[0] notin ['Z', 'z']:
                return false
              res.flags = res.flags + { DTHaveOffset }
              s      = ""
            elif len(s) != 6:
              return false
            elif s[0] notin ['+', '-']:
              return false
            elif s[3] != ':':
              return false
            elif s[4] notin '0' .. '5' or s[5] notin '0' .. '9':
              return false
            elif s[1] notin '0' .. '2' or s[2] notin '0' .. '9':
              return false
            else:
              n = (int(s[1]) - int('0')) * 10 + int(s[2]) - int('0')
              if n > 23: return false
              if s[0] == '-':
                res.tzoffset = -1 * n
              else:
                res.tzoffset = n
              res.flags = res.flags + { DTHaveOffset }
              s = ""
      else:
        case s[0]
        of 'Z', 'z':
          s = s[1 .. ^1]
        of '+', '-':
          if len(s) != 6:
            return false
          elif s[3] != ':':
            return false
          elif s[4] notin '0' .. '5' or s[5] notin '0' .. '9':
            return false
          elif s[1] notin '0' .. '2' or s[2] notin '0' .. '9':
            return false
          else:
            n = (int(s[1]) - int('0')) * 10 + int(s[2]) - int('0')
            if n > 23: return false
            if s[0] == '-':
              res.tzoffset = -1 * n
            else:
              res.tzoffset = n
            res.flags = res.flags + { DTHaveOffset }
            s = ""
        else:
          return false
    else:
      s = ""
  if len(s) > 0:
    if len(s) != 2 or s[1] notin ['m', 'M']:
      return false
    case s[0]
    of 'p', 'P':
      hr += 12
    of 'a', 'A':
      discard
    else:
      return false
  if hr > 23 or min > 59 or sec > 60:
    return false

  res.dt.tm_hour = int32(hr)
  res.dt.tm_min  = int32(min)
  res.dt.tm_sec  = int32(sec)

  return true

proc otherLitToNativeDateTime*(lit: string, res: var DateTime): bool =
  var
    ix0 = lit.find('T')
    ix1 = lit.find('t')

  if ix0 == ix1:
    return false
  if ix0 >= 0 and ix1 >= 0:
    return false
  if ix1 > ix0:
    ix0 = ix1
  if ix0 == len(lit) - 1:
      return false
  let
    datePart = lit[0 ..< ix0]
    timePart = lit[ix0 + 1 .. ^1]
    dateRes  = datePart.otherLitToNativeDate(res)
    timeRes  = timePart.otherLitToNativeTime(res)

  if not dateRes or not timeRes:
    return false

  return true

proc new_date_time(s: string, st: SyntaxType, lmod: string,
                   l: var int, err: var string):
                      pointer {.cdecl.}


proc new_date(s: string, st: SyntaxType, lmod: string,
              l: var int, err: var string):
                      pointer {.cdecl.}

proc new_time(s: string, st: SyntaxType, lmod: string,
              l: var int, err: var string):
                      pointer {.cdecl.}


proc baseDtRepr(fmt: string, dt: var DateTime): string =
  var buf = cast[cstring](alloc(128))

  discard strftime(buf, 128, cstring(fmt), dt.dt)

  return $buf

proc repr_date_time(m: pointer): string {.cdecl.} =
  var dt = extractRef[DateTime](m)
  return baseDtRepr("%Y-%m-%dT%H:%M:%S", dt)

proc repr_date(m: pointer): string {.cdecl.} =
  var dt = extractRef[DateTime](m)

  return baseDtRepr("%Y-%m-%d", dt)

proc repr_time(m: pointer): string {.cdecl.} =
  var dt = extractRef[DateTime](m)

  return baseDtRepr("%H:%M:%S", dt)

proc eq_dt(a, b: pointer): bool {.cdecl.} =
  var
    d1 = extractRef[DateTime](a)
    d2 = extractRef[DateTime](b)

  let r = memcmp(cast[pointer](d1.dt),
                 cast[pointer](d2.dt),
                 csize_t(sizeof(Tm)))

  return r == 0

var
  dtOps = newVtable()
  dOps  = newVtable()
  tOps  = newVtable()


proc new_date_time(s: string, st: SyntaxType, lmod: string,
                   l: var int, err: var string):
                      pointer =
  l      = sizeof(DateTimeObj)
  result = alloc(l)

  var dt: DateTime = cast[DateTime](result)

  if not otherLitToNativeDateTime(unicode.strip(s), dt):
    err = "BadDateTime"
    dealloc(result)
    return nil

proc new_date(s: string, st: SyntaxType, lmod: string,
              l: var int, err: var string):
                      pointer =
  l      = sizeof(DateTimeObj)
  result = alloc(l)

  var dt: DateTime = cast[DateTime](result)

  if not otherLitToNativeDateTime(unicode.strip(s), dt):
    err = "BadDate"
    dealloc(result)
    return nil

proc new_time(s: string, st: SyntaxType, lmod: string,
              l: var int, err: var string):
                      pointer =
  l      = sizeof(DateTimeObj)
  result = alloc(l)

  var dt: DateTime = cast[DateTime](result)

  if not otherLitToNativeDateTime(unicode.strip(s), dt):
    err = "BadTime"
    dealloc(result)
    return nil

dtOps[FRepr]   = cast[pointer](repr_date_time)
dtOps[Feq]     = cast[pointer](eq_dt)
dtOps[FNewLit] = cast[pointer](new_date_time)
dOps[FRepr]    = cast[pointer](repr_date)
dOps[Feq]      = cast[pointer](eq_dt)
dOps[FNewLit]  = cast[pointer](new_date)
tOps[FRepr]    = cast[pointer](repr_time)
tOps[Feq]      = cast[pointer](eq_dt)
tOps[FNewLit]  = cast[pointer](new_time)

let
  TDateTime* = addDataType(name = "datetime", concrete = true, ops = dtOps)
  TDate*     = addDataType(name = "date", concrete = true, ops = dOps)
  TTime*     = addDataType(name = "time", concrete = true, ops = tOps)

registerSyntax(TDateTime, STOther, @[])
registerSyntax(TDateTime, STStrQuotes, @[])
registerSyntax(TDate, STOther, @[])
registerSyntax(TDate, STStrQuotes, @[])
registerSyntax(TTime, STOther, @[])
registerSyntax(TTime, STStrQuotes, @[])
