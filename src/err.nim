import nimutils, options, strutils, strcursor, unicode, tables

## If we have to bail on control flow, we'll do so here.
proc newCon4mException*(errs: seq[Con4mError] = @[]): Con4mException =
  result = new(Con4mException)
  result.errors = errs

proc newCon4mLongJmp*(msg = ""): Con4mLongJmp =
  result = new(Con4mLongJmp)
  result.msg = msg

template con4mLongJmp*(msg = "") =
  raise newCon4mLongJmp(msg)

const errorMsgs = [
  ("StrayCr",        "Carriage return (<em>\r</em>) without newline."),
  ("BadEscape",      "Unterminated escape sequence in literal."),
  ("UnicodeEscape",  "Invalid unicode escape character found in literal."),
  ("LitModExpected", "Expected a valid literal modifier"),
  ("CommentTerm",    "Unterminated C-style long comment."),
  ("BadFloat",       "Invalid character in float literal"),
  ("MissingDigits",  "Must have digits after the dot in a valid float literal"),
  ("BadExponent",    "Float exponent specified, with no exponent " &
                     "value provided"),
  ("BadChar",        "Invalid character literal"),
  ("StringNl",       "Unterminated single-quoted string."),
  ("StringTerm",     "Unterminated string literal"),
  ("CharTerm",       "Unterminated character literal"),
  ("OtherTerm",      "Unterminated literal"),
  ("BadChar",        "Invalid character found in token"),
  ("StmtEnd",        "Expected end of a statement after $1."),
  ("NotALit",        "Not a literal; literal modifier not allowed here."),
  ("BadLitMod",      "Unknown literal modifier: <em>$1</em>"),
  ("LitModTypeErr",  "Literal modifier <em>$1</em> can't be used with " &
                      "$2 literals"),
  ("MissingTok",     "Expected $1 here."),
  ("MemberSpec",     "<em>'.'</em> operator must have an identifier on " &
                     "both sides."),
  ("ItemExpected",   "Expected either another item or <em>'$1'</em>"),
  ("BadTuple",       "Tuple types must contain two or more items."),
  ("AccessExpr",     "Expected either an identifier or paren here."),
  ("ExprStart",      "Expected the start of an expression here, but got $1."),
  ("BinaryNot",      "<em>'not'<em> operator takes only one operand."),
  ("LitExpected",    "Expected a literal value here."),
  ("NotAttrAssign",  "Left hand side of assignment was an attribute, " &
                     "not a variable. Therefore, expected either " &
                     "<em>':'</em>  or <em>'='</em> for the assignment " &
                     "operator."),
  ("BadLock",        "Expected an attribute after <em>'~'</em> (the " &
                     "attribute lock operator.)"),
  ("ForFromIx",      "<em>for ... from</em> loops must have a single index " &
                     "variable."),
  ("NameInTypeSpec", "<em>'$1'</em> is not a builtin type. " &
                     "Use <em>struct[typename]</em> for user types."),
  ("BadOneOf",       "OneOf types must have multiple type options that " &
                     "are more constrained than a generic type variable."),
  ("BadObjType",     "Object types must provide either a name or a type " &
                     "variable if specifying an object where no fields " &
                     "will be referenced."),
  ("BadTypeDecl",    "Invalid syntax inside a type declaration."),
  ("BadFormalEnd",   "Expected either a closing parenthesis (<em>')'</em>" &
                     " or an additional parameter."),
  ("BadUseSyntax",   "<em>'use'</em> statement requires a string literal " &
                     " after <em>'from'</em>"),
  ("BadParamName",   "<em>'parameter'</em> keyword must be followed by " &
                     "an attribute (which can be dotted), or the " &
                     "<em>'var'</em> keyword followed by a local variable " &
                     "name. The variable can be optionally typed."),
  ("EofInBlock",     "Block was not closed when end-of-file was found."),
  ("TopLevelOnly",   "<em>'$1'</em> is only allowed at the top-level of a " &
                     "module."),
  ("InLoopsOnly",    "<em>'$1'</em> is only allowed inside loops."),
  ("RetOutOfFunc",   "<em>'return'</em> is only allowed inside functions."),
  ("SignToUnsign",   "Automatic conversion of unsigned value of type " &
                     "<em>$1</em> to a signed type can have unintended " &
                     "consequences, like making large numbers negative. " &
                     "Explicitly cast to another type if possible, if " &
                     "you want to avoid loss of precision."),
  ("SignChange",     "Operand type <em>$1</em> is signed, but <em>$2</em> " &
                     "is unsigned. Since $1 is larger, result will be " &
                     "signed. While no precision is lost, this might be " &
                     "unexpected (cast explictly to squelch message)"),
  ("TypeMismatch",   "<em>$1</em> and <em>$2</em> are incompatible types."),
  ("LoopVarAssign",  "Cannot assign to (or re-declare) loop iteration " &
                     "variables."),
  ("AlreadyAFunc",   "Variable names cannot have names that are identical to " &
                     "functions named in the same module's top-level."),
  ("AlreadyAVar",    "Already found a top-level variable with the same " &
                     "name as this function; this is not allowed within a " &
                     "module."),
  ("Immutable",      "Cannot modify immutable values."),
  ("VarRedef",       "Variable <em>$1</em> declared multiple times in " &
                     "the same scope."),
  ("LabelDupe",      "Nested loops have the same label (<em>$1</em>)."),
  ("LabelLoc",       "<em>label</em> statement must come immediately before " &
                     "either a <em>for</em> loop or a <em>while</em> loop."),
  ("DeadCode",       "Dead code after $1."),
  ("ElifLoc",        "<em>elif</em> statement must follow either an " &
                     "<em>if</em> block, or another <em>elif</em> block."),
  ("ElseLoc",        "<em>else<em> statement must follow either an " &
                     "<em>if</em> block or another <em>elif</em> block."),
  ("BadLoopExit",    "<em>$1</em> to label <em>$2</em> is invalid, because " &
                     "it is not contained inside a loop that have that label."),
  ("DupeParamProp",  "Duplicate parameter property for <em>$1</em>"),
  ("ParamType",      "Parameter property <em>$1</em> must be a $2."),
  ("BadParamProp",   "Invalid property for a parameter: <em>$1</em>"),
  ("EnumDeclConst",  "Enum value assignments must be constants"),
  ("EnumInt",        "Currently, enum values may only be <em>int<em> types"),
  ("EnumReuse",      "Value <em>$1</em> has already been used in this enum."),
  ("InternalErr1",   "Have a st entry where the parser shouldn't allow it?"),

 ]

proc baseError*(list: var seq[Con4mError], code: string, cursor: StringCursor,
                module: string, line: int, lineOffset: int,
                phase: Con4mErrPhase, severity = LlErr,
                extraContents: seq[string] = @[],
                trace: string = "", ii: Option[InstantiationInfo] =
                                  none(InstantiationInfo)) =
  var err = Con4mError(phase: phase, severity: severity, code: code,
                       cursor: cursor, module: module, line: line,
                       offset: lineOffset, extra: extraContents)

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
                module: string, phase: Con4mErrPhase, severity = LlErr,
                extra: seq[string] = @[], trace = "",
                ii = none(InstantiationInfo)) =
  list.baseError(code, tok.cursor, module, tok.lineNo, tok.lineOffset,
                 phase, severity, extra, trace, ii)

proc lexBaseError*(ctx: var CompileCtx, basemsg: string, t: Con4mToken = nil,
                  subs: seq[string] = @[]) =
  var t = t

  if t == nil:
    t = ctx.tokens[^1]

  ctx.errors.baseError(basemsg, t.cursor, ctx.module, t.lineNo,
                       t.lineOffset, ErrLex, LlErr, subs)

proc lexFatal*(ctx: var CompileCtx, basemsg: string, t: Con4mToken = nil) =
  ctx.lexBaseError(basemsg, t)
  raise newCon4mException()

template lexError*(msg: string, t: Con4mToken = nil) =
  ctx.lexFatal(msg, t)

template baseError*(list: var seq[Con4mError], code: string, node: Con4mNode,
                    module: string, phase: Con4mErrPhase, severity = LlErr,
                    extra = seq[string](@[]), trace = "",
                    ii = none(InstantiationInfo)) =
  list.baseError(code, node.token, module, phase, severity, extra,
                 trace, ii)

template irError*(ctx: var CompileCtx, msg: string, extra: seq[string] = @[],
                w = Con4mNode(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.module, ErrIrGen, LlErr, extra)

template irWarn*(ctx: var CompileCtx, msg: string, extra: seq[string] = @[],
                w = Con4mNode(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.module, ErrIrGen, LlWarn, extra)

proc canProceed*(errs: seq[Con4mError]): bool =
  for err in errs:
    if err.severity == LlErr:
      return false

  return true

proc lookupMsg(err: Con4mError): string =
  for (k, v) in errorMsgs:
    if k == err.code:
      return v

  return "<em>Unknown error code:</em> " & err.code

proc performSubs(err: Con4mError, s: var string) =
  for i, item in err.extra:
    s = s.replace("$" & `$`(i + 1), item)

proc oneErrToRopeList(err: Con4mError, s: string): seq[Rope] =
  case err.severity
  of LlErr:
    result.add(fgColor("error:", "red").td().overflow(OTruncate))
  of LlWarn:
    result.add(fgColor("warn:", "yellow").td().overflow(OTruncate))
  of LLInfo:
    result.add(fgColor("info:", "green").td().overflow(OTruncate))
  of LlNone:
    unreachable

  if err.module.len() != 0:
    let module = fgColor(err.module, "jazzberry") + text(":")
    let offset = td(text(`$`(err.line + 1) & ":" & `$`(err.offset + 1)))
    result.add(module.overflow(OTruncate))
    result.add(offset.overflow(OTruncate))
  else:
    result.add(text(""))
    result.add(text(""))

  result.add(s.htmlStringToRope(markdown = false, add_div = false))

proc getVerboseInfo(err: Con4mError): Rope =
  let
    src     = $(err.cursor.runes)
    lines   = src.split("\n")
    locator = em(repeat((' '), err.offset) & "^")

  result = text(lines[err.line]) + newBreak() + locator + newBreak()

proc getLocWidth(errs: seq[Con4mError]): int =
  for err in errs:
    let r = 2 + `$`(err.line + 1).len() + `$`(err.offset + 1).len()

    if r > result:
      result = r

proc getModuleWidth(errs: seq[Con4mError]): int =
  for err in errs:
    let l = err.module.len()
    if l != 0 and (l + 1) > result:
      result = l + 1

proc formatErrors*(errs: seq[Con4mError], verbose = true): Rope =
  var errList: seq[seq[Rope]]
  let
    mw = errs.getModuleWidth()
    lw = errs.getLocWidth()

  for error in errs:
    var msg = error.lookupMsg()
    error.performSubs(msg)
    errList.add(error.oneErrToRopeList(msg))

  if not verbose:
    let table = quickTable[Rope](errList, noHeaders = true,
                                 borders = BorderNone)

    result = table.colWidths([(7, true), (mw, true), (lw, true), (0, false)])
    result = result.lpad(0, true).rpad(0, true)
    result = result.bpad(0, true).tpad(0, true)
  else:
    for i, item in errlist:
      var table = quickTable(@[item], noHeaders = true, borders = BorderNone)
      table = table.colWidths([(7, true), (mw, true), (lw, true), (0, false)])
      table = table.lpad(0, true).rpad(0, true).bpad(0, true).tpad(0, true)
      result += table
      result += container(errs[i].getVerboseInfo())
