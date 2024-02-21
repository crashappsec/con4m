import ".."/common
import "."/messages

## If we have to bail on control flow, we'll do so here.
proc newCon4mException*(errs: seq[Con4mError] = @[]): Con4mException =
  result = new(Con4mException)
  result.errors = errs

proc newCon4mLongJmp*(msg = ""): Con4mLongJmp =
  result = new(Con4mLongJmp)
  result.msg = msg

template con4mLongJmp*(msg = "") =
  raise newCon4mLongJmp(msg)

proc baseError*(list: var seq[Con4mError], code: string, cursor: StringCursor,
                modname: string, line: int, lineOffset: int,
                phase: Con4mErrPhase, severity = LlFatal,
                extraContents: seq[string] = @[], detail: Rope = nil,
                trace: string = "", ii: Option[InstantiationInfo] =
                                  none(InstantiationInfo)) =
  if severity < config_log_level:
    return
  var err = Con4mError(phase: phase, severity: severity, code: code,
                       cursor: cursor, modname: modname, line: line,
                       offset: lineOffset, extra: extraContents, detail: detail)

  when not defined(release):
    err.trace = trace
    if ii.isSome():
      err.ii  = ii.get()

    var foundCode = false
    for (k, v) in errorMsgs:
      if code == k:
        foundCode = true
        break
    if not foundCode:
      print fgColor("error: ", "red") + text("Error code '") + em(code) +
      text("' was not found.")

  list.add(err)

proc baseError*(list: var seq[Con4mError], code: string, tok: Con4mToken,
                modname: string, phase: Con4mErrPhase, severity = LlFatal,
                extra: seq[string] = @[], detail: Rope = nil, trace = "",
                ii = none(InstantiationInfo)) =
  list.baseError(code, tok.cursor, modname, tok.lineNo, tok.lineOffset,
                 phase, severity, extra, detail, trace, ii)

proc lexBaseError*(ctx: Module, basemsg: string, t: Con4mToken = nil,
                  subs: seq[string] = @[]) =
  var t = t

  if t == nil:
    t = ctx.tokens[^1]

  ctx.errors.baseError(basemsg, t.cursor, ctx.modname & ctx.ext, t.lineNo,
                       t.lineOffset, ErrLex, LlFatal, subs)

proc lexFatal*(ctx: Module, basemsg: string, t: Con4mToken = nil) =
  ctx.lexBaseError(basemsg, t)
  raise newCon4mException()

template lexError*(msg: string, t: Con4mToken = nil) =
  ctx.lexFatal(msg, t)

proc baseError*(list: var seq[Con4mError], code: string, node: ParseNode,
                modname: string, phase: Con4mErrPhase, severity = LlFatal,
                extra = seq[string](@[]), detail: Rope = nil, trace = "",
                ii = none(InstantiationInfo)) =
  if node == nil:
    list.baseError(code, nil, modname, 0, 0, phase, severity, extra,
                   detail, trace, ii)
    return

  if node.err and severity != LlInfo:
    return
  if severity in [LlErr, LlFatal]:
    node.err = true
  list.baseError(code, node.token, modname, phase, severity, extra,
                 detail, trace, ii)

template irError*(ctx: Module, msg: string, extra: seq[string] = @[],
                  w = ParseNode(nil), detail = Rope(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlFatal,
                                                 extra, detail)

template irError*(ctx: Module, msg: string, w: IrNode,
                  extra: seq[string] = @[], detail: Rope = nil) =
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlFatal,
                                                 extra, detail)

template irNonFatal*(ctx: Module, msg: string, extra: seq[string] = @[],
                w = ParseNode(nil)) =
  # Things we consider errors, but we may end up allowing. Currently, this
  # is just for use-before-def errors.
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlErr,
                                                 extra)

template irNonFatal*(ctx: Module, msg: string, w: IrNode,
                     extra: seq[string] = @[]) =
  # Things we consider errors, but we may end up allowing. Currently, this
  # is just for use-before-def errors.
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlErr,
                                                 extra)

template irWarn*(ctx: Module, msg: string, extra: seq[string] = @[],
                w = ParseNode(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlWarn,
                                                 extra)

template irWarn*(ctx: Module, msg: string, w: IrNode,
                 extra: seq[string] = @[]) =
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlWarn,
                                                 extra)

template irInfo*(ctx: Module, msg: string, extra: seq[string] = @[],
                w = ParseNode(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlInfo,
                                                 extra)

template irInfo*(ctx: Module, msg: string, w: IrNode,
                 extra: seq[string] = @[]) =
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname & ctx.ext, ErrIrGen, LlInfo,
                                                 extra)

template loadError*(ctx: CompileCtx, msg: string, modname: string,
                    extra: seq[string] = @[]) =
  # TODO: don't hardcode the extension.
  ctx.errors.baseError(msg, ParseNode(nil), modname & ".c4m", ErrLoad,
                                                      LlFatal, extra)

template loadWarn*(ctx: CompileCtx, msg: string, modname: string,
                    extra: seq[string] = @[]) =
  ctx.errors.baseError(msg, ParseNode(nil), modname & ".c4m", ErrLoad, LlWarn,
                                                      extra)

proc canProceed*(errs: seq[Con4mError]): bool =
  for err in errs:
    if err.severity in [LlErr, LlFatal]:

      return false
  return true

template canProceed*(ctx: CompileCtx): bool =
  ctx.errors.canProceed()
