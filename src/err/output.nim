import "std"/terminal
import ".."/common
import "."/[messages, backtrace]

proc lookupMsg(code: string): string =
  for (k, v) in errorMsgs:
    if k == code:
      return v

  return "<em>Unknown error code:</em> " & code

proc performSubs(extra: seq[string], s: var string) =
  for i, item in extra:
    s = s.replace("$" & `$`(i + 1), item)

proc oneErrToRopeList(err: Con4mError, s: string): seq[Rope] =
  case err.severity
  of LlErr, LlFatal:
    result.add(fgColor("error:", "red").td().overflow(OTruncate))
  of LlWarn:
    result.add(fgColor("warn:", "yellow").td().overflow(OTruncate))
  of LLInfo:
    result.add(fgColor("info:", "atomiclime").td().overflow(OTruncate))
  of LlNone:
    unreachable

  if err.modname.len() != 0:
    let modname = fgColor(err.modname, "jazzberry") + text(":")
    result.add(modname.overflow(OTruncate))
  else:
    result.add(text(""))
  if err.line >= 1:
    let offset = td(text(`$`(err.line) & ":" & `$`(err.offset + 1) & ":"))
    result.add(offset.overflow(OTruncate))
  else:
    result.add(text(""))

  result.add(s.htmlStringToRope(markdown = false, add_div = false))

proc getVerboseInfo(err: Con4mError): Rope =
  var
    noLoc = false

  if err.cursor == nil:
    return nil

  elif err.offset < 0:
    noLoc = true

  let
    src     = $(err.cursor.runes)
    lines   = src.split("\n")

  if lines.len() == 0 or err.line <= 0:
    return nil

  result = text(lines[err.line - 1]) + newBreak()

  if not noLoc:
    result += em(repeat((' '), err.offset) & "^") + newBreak()

  if err.detail != nil:
    result = result + err.detail

proc getLocWidth(errs: seq[Con4mError]): int =
  for err in errs:
    let r = 2 + `$`(err.line).len() + `$`(err.offset + 1).len()

    if r > result:
      result = r

proc getModuleWidth(errs: seq[Con4mError]): int =
  for err in errs:
    let l = err.modname.len()
    if l != 0 and (l + 1) > result:
      result = l + 1

proc dupeLocationCheck(err: Con4mError, locs: var seq[(int, int)]): bool =
  for (l, c) in locs:
    if err.line == l and err.offset == c:
        return true

  locs.add((err.line, err.offset))

proc formatErrors*(errs: seq[Con4mError], verbose = true): Rope =
  var
    errList: seq[seq[Rope]]
    locs:    seq[(int, int)]

  let
    mw = errs.getModuleWidth() + 1
    lw = errs.getLocWidth() + 1

  for i, error in errs:
    if error.dupeLocationCheck(locs):
      continue
    if i == 30: # TODO; make this a configurable limit.
      break
    var msg = error.code.lookupMsg() & "<i> (" & error.code & ")</i>"
    error.extra.performSubs(msg)
    errList.add(error.oneErrToRopeList(msg))

  if not verbose:
    let table = quickTable[Rope](errList, noHeaders = true,
                                 borders = BorderNone)
    var one: Rope
    result = table.colWidths([(7, true), (mw, true), (lw, true), (0, false)])
    result = result.lpad(0, true).rpad(0, true)
    result = result.bpad(0, true).tpad(0, true)
  else:
    for i, item in errlist:
      var table = quickTable(@[item], noHeaders = true, borders = BorderNone)
      table = table.colWidths([(7, true), (mw, true), (lw, true), (0, false)])
      table = table.lpad(0, true).rpad(0, true).bpad(0, true).tpad(0, true)
      var one = table + container(errs[i].getVerboseInfo())
      result += one

proc find_string_at(mem: string, offset: int): string {.importc, cdecl.}
proc toString(x: TypeId): string {.importc, cdecl.}

proc location_from_instruction*(ctx: RuntimeState,
                                ins: ptr ZInstruction): (string, int) =
  return (ctx.obj.moduleContents[ins.moduleId - 1].modname,
          int(ins.lineno))

proc print_con4m_trace*(ctx: RuntimeState) {.exportc, cdecl.} =
  print(ctx.get_stack_trace(), file = stderr)

proc formatLateError(err: string, severity: Con4mSeverity, location: string,
                     args: seq[string], verbose = true): Rope =
  # `location` should be an indication of the instruction if we are executing,
  # attribute information if we're validating, and whatever is appropriate
  # if it's some other error.
  var
    msg = lookupMsg(err)
    row: seq[Rope]

  performSubs(args, msg)

  case severity
  of LlErr, LlFatal:
    row.add(fgColor("error: ", "red").td().lpad(0))
  of LlWarn:
    row.add(fgColor("warn: ", "yellow").td().overflow(OTruncate))
  of LLInfo:
    row.add(fgColor("info: ", "atomiclime").td().overflow(OTruncate))
  of LlNone:
    unreachable

  row.add(italic(location & ": "))
  row.add(markdown(msg))
  row.add(italic("(" & err & ")"))

  var
    width_used = 11 + location.len() + err.len()
    remains    = terminalWidth() - width_used
    msg_width  = row[2].runeLength() + 1
    msg_col: int

  if msg_width < remains:
    msg_col = msg_width
  else:
    msg_col = remains

  result = @[row].quickTable(noheaders = true, borders = BorderNone)
  result.colWidths([(7, true), (location.len() + 2, true),
                    (msg_col, true), (err.len() + 2, true)])
  result.lpad(0, true).rpad(0, true).bpad(0, true).tpad(0, true)

proc assemble_validation_msg(ctx: RuntimeState, path: string, msg: Rope,
                             code: string, other: Rope = nil): Rope =
    var
      nim_path = path
      last_touch: Rope

    if nim_path.len() != 0:
      if nim_path[0] == '.':
        nim_path = nim_path[1 .. ^1]

      if nim_path[^1] == '.':
        nim_path = nim_path[0 ..< ^1]

      let attrOpt = ctx.attrs.lookup(nim_path)
      if attrOpt.isSome():
        let record = attrOpt.get()
        if record.lastset == nil:
          last_touch = text("Attribute has not been set this execution.")
        else:
          let (module, line) = ctx.location_from_instruction(record.lastset)

          last_touch = text("Attribute last set at: ") +
                       em(module & ":" & $line)

    if nim_path.len() == 0:
      nim_path = "root attribute section"

    result  = text("Validation for ") + em(nim_path) + text(" failed: ")
    result += italic(msg)
    result += last_touch

    if other != nil:
      result += text(" ") + other

    result += italic(" (" & code & ")")
    GC_ref(result)

proc formatValidationError*(ctx: RuntimeState, attr: string, err: string,
                            args: seq[string]): Rope =
  var msg = err.lookupMsg()
  performSubs(args, msg)

  let asRope = htmlStringToRope(msg, markdown = false, add_div = false)

  return ctx.assemble_validation_msg(attr, asRope, err)

proc formatValidationError*(ctx: RuntimeState, attr: C4Str, err: string,
                            args: seq[string]): Rope =
  return ctx.formatValidationError(attr.toNimStr(), err, args)

proc customValidationError*(ctx: RuntimeState, path: C4Str, usrmsg: C4Str,
                            cb: ptr ZCallback): Rope =
  let
    asRope         = htmlStringToRope(usrmsg.toNimStr(), markdown = false,
                                      add_div = false)
    cbName         = find_string_at(ctx.obj.staticData, cb.nameOffset)
    validator_info = text("Validation function: ") +
                     em(cbname & cb.tid.toString())

  return ctx.assemble_validation_msg(path.toNimStr(), asRope,
                                     "CustomValidator", validator_info)

proc runtimeIssue(ctx: RuntimeState, err: string, args: seq[string],
                  severity = LLFatal) =
  if severity < config_log_level:
    return

  let
    instr   = addr ctx.curModule.instructions[ctx.ip]
    (m, l)  = ctx.location_from_instruction(instr)
    extra   = if l == -1: "" else: ":" & $(l)
    loc     = "When executing " & m & extra

  print(err.formatLateError(severity, loc, args), file = stderr)

proc runtimeWarn*(ctx: RuntimeState, err: string, args: seq[string] = @[]) =
  if config_debug:
    ctx.print_con4m_trace()
  ctx.runtimeIssue(err, args, LlWarn)

proc runtimeError*(ctx: RuntimeState, err: string, args: seq[string] = @[]) =
  ctx.print_con4m_trace()
  ctx.runtimeIssue(err, args)
  quit(-1)

proc runtimeError*(ctx: RuntimeState, err: string, file: ZModuleInfo, line: int,
                   args: seq[string] = @[]) =
  var extra: string = ""
  if line != -1:
    extra = ":" & $(line)

  let loc = "When executing " & file.modname & extra

  ctx.print_con4m_trace()
  print(err.formatLateError(LlErr, loc, args), file = stderr)
  quit(-1)

proc codeGenError*(err: string, args: seq[string] = @[]) =
  # TODO: the module / function info needs to show up here.
  print(err.formatLateError(LlErr, "When generating code", args),
        file = stderr)
  quit(-1)

proc objLoadWarn*(ctx: RuntimeState, err: string, args: seq[string] = @[]) =
  if config_log_level > LlWarn:
    return

  # TODO: the module / function info needs to show up here.
  print(err.formatLateError(LlWarn, "When loading object file", args),
        file = stderr)
  quit(-1)
