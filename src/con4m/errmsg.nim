## We're going to use the nimutils topic outputting system, publishing
## everything to a "con4m" topic.  By default, we'll use the nimutils
## log-level system to decide what to publish.

import tables, streams, strutils, strformat, os
import nimutils, nimutils/logging, types


type
  InstInfo*    = tuple[filename: string, line: int, column: int]
  C4Verbosity* = enum c4vBasic, c4vShowLoc, c4vTrace, c4vMax
  Con4mError*  = object of CatchableError

let
  con4mTopic*       = registerTopic("con4m")
  `hook?`           = configSink(getSink("stderr").get(),
                                 filters = @[MsgFilter(logLevelFilter),
                                             MsgFilter(logPrefixFilter)])
  defaultCon4mHook* = `hook?`.get()

var
  publishParams = { "loglevel" : $(llError) }.newOrderedTable()
  verbosity     = c4vMax
  curFileName: string


proc setCon4mVerbosity*(level: C4Verbosity) =
  verbosity = level

proc getCon4mVerbosity*(): C4Verbosity =
  return verbosity

proc setCurrentFileName*(s: string) =
  curFileName = s

proc getCurrentFileName*(): string =
  return curFileName

proc formatCompilerError(msg: string,
                         t:   Con4mToken,
                         tb:  string = "",
                         ii:  InstInfo): string =
  let
    me    = getAppFileName().splitPath().tail
    red   = toAnsiCode(acRed)
    reset = toAnsiCode(acReset)
    bold  = toAnsiCode(acBold)

  result = fmt"{red}{me}{reset}: {curFileName}: "
  if t != nil:
    result &= fmt"{t.lineNo}:{t.lineOffset+1}: "
  result &= msg

  if verbosity in [c4vShowLoc, c4vMax]:
    let f = newFileStream(curFileName, fmRead)

    if t != nil and f != nil:
      let
        line   = t.lineNo - 1
        offset = t.lineOffset + 1
        src    = f.readAll()
        lines  = src.split("\n")
        pad    = repeat(' ', offset + 1)

      result &= "\n  " & lines[line] & "\n"
      result &= pad & bold & "^" & reset

  if verbosity in [c4vTrace, c4vMax]:
    if tb != "":
      result &= "\n" & toAnsiCode(@[acBold]) & tb
      if ii.line != 0:
        result &= "Exception thrown at: "
        result &= ii.filename & "(" & $(ii.line) & ":" & $(ii.column) & ")\n"
      result &= toAnsiCode(@[acReset])

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
