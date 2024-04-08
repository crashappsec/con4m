import "std"/terminal
import ".."/common
import "."/[messages, backtrace]


proc lookupMsg(code: string): string =
  for (k, v) in errorMsgs:
    if k == code:
      return v

  return "[i]Unknown error code: [/i]" & code

proc performSubs(extra: seq[string], s: var string) =
  for i, item in extra:
    s = s.replace("$" & `$`(i + 1), item)

proc oneErrToCells(err: Con4mError, s: string): seq[Rich] =

  case err.severity
  of LlErr, LlFatal:
    result.add(rich"[red]error:[/]")
  of LlWarn:
    result.add(rich"[yellow]warn:[/]")
  of LLInfo:
    result.add(rich"[atomic lime]info:[/]")
  of LlNone:
    unreachable

  if err.modname.len() != 0:
    result.add(r("[jazzberry]" & err.modname & "[/]:"))

  else:
    result.add(rich"")
  if err.line >= 1:
    result.add(r(`$`(err.line) & ":" & `$`(err.offset + 1) & ":"))
  else:
    result.add(rich"")

  result.add(r(s))

proc getVerboseInfo(err: Con4mError): Rich =
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

  result = r(lines[err.line - 1] & "\n")

  if not noLoc:
    result += r(repeat((' '), err.offset) & "^\n")

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

proc setColWidths(g: Grid, mw, lw: int) =
  var
    col0 = new_render_style()
    col1 = new_render_style()
    col2 = new_render_style()

  col0.set_absolute_size(7)
  col1.set_absolute_size(mw)
  col2.set_absolute_size(lw)

  g.set_col_props(0, col0)
  g.set_col_props(1, col1)
  g.set_col_props(2, col2)

proc formatErrors*(errs: seq[Con4mError]): Grid =
  var
    errList: seq[seq[Rich]]
    locs:    seq[(int, int)]

  let
    mw = errs.getModuleWidth() + 1
    lw = errs.getLocWidth() + 1

  for i, error in errs:
    if error.dupeLocationCheck(locs):
      continue
    if i == 30: # TODO; make this a configurable limit.
      break
    var msg = error.code.lookupMsg() & "[i] (" & error.code & ")[/i]"
    error.extra.performSubs(msg)
    errList.add(error.oneErrToCells(msg))

  var flow_items: seq[Grid]
  for i, item in errlist:
    let table = table[Rich](@[item], header_rows = 0, borders = false)
    table.setColWidths(mw, lw)

    flow_items.add(table)

  return flow(flow_items)


proc find_string_at(mem: string, offset: int): string {.importc, cdecl.}

proc location_from_instruction*(ctx: RuntimeState,
                                ins: ptr ZInstruction): (string, int) =
  return (ctx.obj.moduleContents[ins.moduleId - 1].modname,
          int(ins.lineno))

proc print_con4m_trace*(ctx: RuntimeState) {.exportc, cdecl.} =
  print_err(ctx.get_stack_trace())

proc formatLateError(err: string, severity: Con4mSeverity, location: string,
                     args: seq[string], verbose = true): Grid =
  # `location` should be an indication of the instruction if we are executing,
  # attribute information if we're validating, and whatever is appropriate
  # if it's some other error.
  var
    msg = lookupMsg(err)
    row: seq[Rich]

  performSubs(args, msg)

  case severity
  of LlErr, LlFatal:
    row.add(rich"[red]error:[/] ")
  of LlWarn:
    row.add(rich"[yellow]warn:[/] ")
  of LLInfo:
    row.add(rich"[atomic lime]info:[/] ")
  of LlNone:
    unreachable

  row.add(em(location & ": "))
  row.add(r(msg))
  row.add(em("(" & err & ")"))

  var
    width_used = 11 + location.len() + err.len()
    remains    = terminalWidth() - width_used
    msg_width  = row[2].toNimStr().runeLength() + 1
    msg_col: int

  if msg_width < remains:
    msg_col = msg_width
  else:
    msg_col = remains

  result = @[row].table(header_rows = 1, borders = false)
  result.setColWidths(location.len() + 2, msg_col);

proc assemble_validation_msg(ctx: RuntimeState, path: string, msg: Rich,
                             code: string, other: Rich = nil): Rich =
    var
      nim_path = path
      last_touch: Rich

    if nim_path.len() != 0:
      if nim_path[0] == '.':
        nim_path = nim_path[1 .. ^1]

      if nim_path[^1] == '.':
        nim_path = nim_path[0 ..< ^1]

      let attrOpt = ctx.attrs.lookup(r(nim_path))
      if attrOpt.isSome():
        let record = attrOpt.get()
        if record.lastset == nil:
          last_touch = rich"Attribute has not been set this execution."
        else:
          let (module, line) = ctx.location_from_instruction(record.lastset)

          last_touch = rich"Attribute last set at: " +
                       em(module & ":" & $line)

    if nim_path.len() == 0:
      nim_path = "root attribute section"

    result  = rich"Validation for " + em(nim_path) + rich" failed: "
    result += em(msg)
    result += last_touch

    if other != nil:
      result += rich" " + other

    result += em(" (" & code & ")")

proc formatValidationError*(ctx: RuntimeState, attr: string, err: string,
                            args: seq[string]): Rich =
  var msg = err.lookupMsg()
  performSubs(args, msg)

  return ctx.assemble_validation_msg(attr, r(msg), err)

proc formatValidationError*(ctx: RuntimeState, attr: Rich, err: string,
                            args: seq[string]): Rich =
  return ctx.formatValidationError(attr.toNimStr(), err, args)

proc customValidationError*(ctx: RuntimeState, path: Rich, usrmsg: Rich,
                            cb: ptr ZCallback): Rich =
  let
    cbName         = find_string_at(ctx.obj.staticData, cb.nameOffset)
    to_style       = r(cbname) + con4m_repr(cb.tid)
    validator_info = rich"Validation function: " + em(to_style)


  return ctx.assemble_validation_msg(path.toNimStr(), usrmsg,
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

  print_err(err.formatLateError(severity, loc, args))

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
  print_err(err.formatLateError(LlErr, loc, args))
  quit(-1)

proc codeGenError*(err: string, args: seq[string] = @[]) =
  # TODO: the module / function info needs to show up here.
  print_err(err.formatLateError(LlErr, "When generating code", args))
  quit(-1)

proc objLoadWarn*(ctx: RuntimeState, err: string, args: seq[string] = @[]) =
  if config_log_level > LlWarn:
    return

  # TODO: the module / function info needs to show up here.
  print_err(err.formatLateError(LlWarn, "When loading object file", args))
  quit(-1)
