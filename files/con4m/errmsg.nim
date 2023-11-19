## We're going to use the nimutils topic outputting system, publishing
## everything to a "con4m" topic.  By default, we'll use the nimutils
## log-level system to decide what to publish.

import tables, strutils, strformat, os, unicode, nimutils, nimutils/logging,
       types
export getOrElse

type
  InstInfo*    = tuple[filename: string, line: int, column: int]
  C4Verbosity* = enum c4vBasic, c4vShowLoc, c4vTrace, c4vMax
  Con4mError*  = object of CatchableError

let
  con4mTopic*  = registerTopic("con4m")
  `hook?`       = configSink(getSinkImplementation("stderr").get(),
                             "con4m-default",
                             filters = @[MsgFilter(logLevelFilter),
                                         MsgFilter(logPrefixFilter)])
  defaultCon4mHook* = `hook?`.get()

var
  publishParams = { "loglevel" : $(llError) }.newOrderedTable()
  verbosity     = c4vShowLoc
  curFileName: string

proc setupTopStyle() =

  let s = newStyle(bgColor = "darkslategray", lpad=1)
  let r = newStyle(align = AlignC)

  perClassStyles["plain"]  = s
  perClassStyles["woo"]    = r
  styleMap["table"]        = newStyle(borders=[BorderNone])
  styleMap["thead"]        = s
  styleMap["tbody"]        = s
  styleMap["tfoot"]        = s
  styleMap["td"]           = s
  styleMap["tr"]           = s
  styleMap["tr.even"]      = s
  styleMap["tr.odd"]       = s
  styleMap["th"]           = s
  styleMap["caption"] = mergeStyles(styleMap["caption"], newStyle(bpad=2))


proc setupBottomStyle() =
  let s = newStyle(bgColor = "mediumpurple")
  perClassStyles["plain"]  = s
  styleMap["thead"]        = s
  styleMap["table"]        = newStyle(align = AlignC)
  styleMap["tbody"]        = s
  styleMap["tfoot"]        = s
  styleMap["td"]           = s
  styleMap["tr"]           = s
  styleMap["tr.even"]      = s
  styleMap["tr.odd"]       = s
  styleMap["th"]           = newStyle(fgColor = "atomiclime", tpad=1)


proc formatTb(tb, throwinfo: string): string =
  var
    nimbleDirs: OrderedTable[string, string]
    row:        string


  setupTopStyle()

  let lines = strutils.split(tb, "\n")
  for i, line in lines:
    if i == 0:
      result  = "<div class=woo><h2 class=woo>" & line & "</h2></div>\n"
      result &= "<table><colgroup><col width=70><col width=30></colgroup>"
      result &= "<tbody>"
      continue
    if len(line) == 0:
      continue
    let parts = line.split("/")
    if line[0] == '/' and "/.nimble/pkgs2/" in line:
      for j, item in parts:
        if item == "pkgs2":
          if parts[j+2] notin nimbleDirs:
            nimbleDirs[parts[j+2]] = parts[0 .. j+1].join("/")
          row = parts[j+2 ..< ^1].join("/") & "/<em>" & parts[^1] & "</em>"
          break
    else:
      row = parts[0 ..< ^1].join("/") & "/<em>" & parts[^1] & "</em>"

    let ix = row.find(' ')
    result &= "<tr><td>" & row[0 ..< ix] & "</em></td><td><em>" &
      row[ix + 1 .. ^1] & "</td></tr>"

  result &= "</tbody>"

  if throwinfo != "":
    result &= "</table><h2 class=woo><bg-black>" & throwinfo &
      "</bg-black></h2>"
  else:
    result &= "</table>"

  result = $(html(result))

  if len(nimbleDirs) > 0:
    setupBottomStyle()
    var t2 = "<p><center><table><colgroup><col width=20><col width=60>" &
      "</colgroup><thead><tr>" &
      "<th>Package</th><th>Location</th>" &
      "</tr></thead><tbody>"
    for k, v in nimbleDirs:
      t2 &= "<tr><td>" & k &
        "</td><td>" & v & "</td></tr>"
    t2 &= "</tbody><table><caption>Nimble packages used</caption></table></center>"
    result &= $(html(t2))

proc split*(str: seq[Rune], ch: Rune): seq[seq[Rune]] =
  var start = 0

  for i in 0 ..< len(str):
    if str[i] == ch:
      result.add(str[start ..< i])
      start = i + 1

  result.add(str[start .. ^1])

proc setCon4mVerbosity*(level: C4Verbosity) =
  verbosity = level

proc getCon4mVerbosity*(): C4Verbosity =
  return verbosity

proc setCurrentFileName*(s: string) =
  curFileName = s

proc getCurrentFileName*(): string =
  return curFileName

proc formatCompilerError*(msg: string,
                          t:   Con4mToken,
                          tb:  string = "",
                          ii:  InstInfo): string =
  let
    me = getAppFileName().splitPath().tail

  result =  $color(me, "red") & ": " & $color(curFileName, "jazzberry") & ": "

  if t != nil:
    result &= fmt"{t.lineNo}:{t.lineOffset+1}: "
  result &= "\n" & $(text(msg))

  if t != nil and verbosity in [c4vShowLoc, c4vMax]:
    let
      line   = t.lineNo - 1
      offset = t.lineOffset + 1
      src    = t.cursor.runes
      lines  = src.split(Rune('\n'))
      pad    = repeat((' '), offset + 1)

    result &= "\n" & "  " & $(lines[line]) & "\n"
    result &= $(pad) & "^"

  if verbosity in [c4vTrace, c4vMax]:
    if tb != "":
      var throwinfo = ""
      if ii.line != 0:
        throwinfo &= "Exception thrown at: "
        throwinfo &= ii.filename & "(" & $(ii.line) & ":" & $(ii.column) & ")"
      result &= "\n" & formatTb(tb, throwinfo)

proc rawPublish(level: LogLevel, msg: string) {.inline.} =
  publishParams["loglevel"] = $(level)
  discard publish(con4mTopic, msg & "\n", publishParams)

proc fatal*(baseMsg: string,
            token:   Con4mToken,
            st:      string   = "",
            ii:      InstInfo = default(InstInfo)) =
  # 'Fatal' from con4m's perspective is throwing an exception that
  # returns to the caller.
  var msg: string

  if token == nil:
    msg = baseMsg
  elif token.lineNo == -1:
    msg = "(in code called by con4m): " & baseMsg
  else:
    msg = baseMsg

  raise newException(Con4mError, formatCompilerError(msg, token, st, ii))

template fatal*(msg: string, node: Con4mNode = nil) =
  var st   = ""

  when not defined(release):
    st = getStackTrace()

  if node == nil:
    fatal(msg, Con4mToken(nil), st)
  else:
    fatal(msg, node.token.getOrElse(nil), st, instantiationInfo())

proc setCTrace*() =
  setLogLevel(llTrace)
  setCon4mVerbosity(c4vMax)
  rawPublish(llTrace, "debugging on.")

proc ctrace*(msg: string) =
  if verbosity == c4vMax:
    rawPublish(llTrace, msg)
