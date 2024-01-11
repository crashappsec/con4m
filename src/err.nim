import nimutils, options, strutils, strcursor, unicode, tables, common

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
  ("FileNotFound",   "Module could not be located."),
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
  ("BadLitMod",      "Unknown literal modifier <em>$1</em> for <em>$2</em>" &
                     " syntax."),
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
                     "<em>':'</em> or <em>'='</em> for the assignment " &
                     "operator."),
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
  ("BadExternField", "Bad field for <em>'extern'</em> block."),
  ("NeedSig",        "Field requires a function type signature."),
  ("BadRCParam",     "Reference counting specs must either be identifiers " &
                     "that match the names given in the function signature, " &
                     "or the special value <em>return</em> (for incref only)"),
  ("BadCType",       "Invalid C type for external parameter: <em>$1</em>"),
  ("PureBool",       "The <em>'pure'</em> property for external functions " &
                     "must be a boolean value (<em>true</em> or " &
                     "<em>false</em>)"),
  ("DupeCTypeParam", "Duplicate parameter name for an external function " &
                     "specification (<em>$1</em>)"),
  ("DupeExtField",   "Duplicate field <em>$1</em> provided for an <em>" &
                     "extern</em> block."),
  ("DupeDllName",    "Duplicate DLL name <em>$1</em> provided for an " &
                     "external function (ignored)."),
  ("DupeVal",        "Duplicate parameter value <em>$1</em> in the " &
                     "<em>$2</em> property of the <em>extern</em> spec."),
  ("ExtNotSpecd",    "None of the external function parameters were given" &
                     " the name <em>$1</em>"),
  ("ExtAllocNHold",  "Extern function parameter <em>$1</em> cannot be " &
                     "spec'd to have the external function hold memory " &
                     "we pass, and to allocate that memory."),
  ("NoMemMan",       "Since <em>$1</em> is not a pointer or array type, " &
                     "memory management will not be performend, and this " &
                     "annotation will be ignored."),
  ("WontLink",       "Was not able to locate the external symbol <em>$1</em>" &
                     ", which may be required for running."),
  ("MissingSym",     "Was not able to locate the external symbol <em>$1</em>" &
                     "; program will crash if it is accessed, unless " &
                     "it is dynamically loaded first."),
  ("PurePlz",        "Please provide a value for the <em>pure</em> property " &
                     "for extern functions. Pure functions always " &
                     "return the same output for the same input, and do " &
                     "not do any I/O, allowing us to pre-execute."),
  ("EofInBlock",     "Block was not closed when end-of-file was found."),
  ("TopLevelOnly",   "<em>'$1'</em> is only allowed at the top-level of a " &
                     "module."),
  ("TopLevelPlural", "$1 are only allowed at the top-level of a module."),
  ("InLoopsOnly",    "<em>'$1'</em> is only allowed inside loops."),
  ("RetOutOfFunc",   "<em>'return'</em> is only allowed inside functions."),
  ("UToSSmaller",    "Conversion of unsigned value to a smaller signed type" &
                     "can lead to both sign changes and truncated values."),
  ("UToSSameSz",     "Conversion of same-typed unsized values" &
                     "will turn large numbers negative. Explicitly cast " &
                     "to a bigger type if possible to avoid this scenario."),
  ("CanTruncate",    "Conversion to a smaller type can result in values " &
                     "getting truncated."),
  ("StoU",           "Conversion of signed integer to unsigned type turns " &
                     "negative values into larger integers."),
  ("SToSmallerU",    "Conversion of signed integer to unsigned type turns " &
                     "negative values into larger integers, and the " &
                     "smaller type may truncate."),
  ("HexTooLarge",    "Hex number is too large for type <em>$1</em>"),
  ("IntTooLarge",    "Integer literal is too large for type <em>$1</em>"),
  ("FloatTooLarge",  "Integer portion of float is too large; " &
                     "use <em>'e'</em> notation."),
  ("ExpTooLarge",    "Exponent portion of float is too large."),
  ("BadHex",         "Invalid hex literal"),
  ("BadInt",         "Invalid int literal"),
  ("BadBool",        "Invalid <em>bool</em> literal."),
  ("BadByte",        "Invalid value for a byte (cannot be above 0xff)"),
  ("BadCodepoint",   "Invalid character; unicode values may not be " &
                     "above U+10FFFF."),
  ("BadCP2",         "Invalid character; the unicode standard does not " &
                     "allow values from U+D800 to U+DFFF"),
  ("LoseFormat",     "Conversion discards string formatting information."),
  ("TypeNotSigned",  "Literal is negative, but the specified type " &
                     "is unsigned."),
  ("TypeMismatch",   "<em>$1</em> and <em>$2</em> are incompatible types."),
  ("LoopVarAssign",  "Cannot assign to (or re-declare) loop iteration " &
                     "variables."),
  ("AlreadyAFunc",   "Variable names cannot have names that are identical to " &
                     "functions named in the same module's top-level."),
  ("AlreadyAVar",    "Already found a top-level variable with the same " &
                     "name as this function; this is not allowed within a " &
                     "module."),
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
  ("BinaryOpCompat", "LHS and RHS do not have compatable types " &
                     "(lhs: <em>$1</em>; rhs: <em>$2</em>)."),
  ("NoBoolCast",     "The condition cannot be automatically converted to " &
                     "a true / false value; provide a specific check. " &
                     "Type of condition is: <em>$1</em>"),
  ("CannotCast",     "Cannot convert from type <em>$1</em> to " &
                     "type <em>$2</em>"),
  ("BoolAutoCast",   "This condition (of type <em>$1</em>) is not a " &
                     "boolean value, but is being auto-cast."),
  ("TyDiffListItem", "List item type is not consistent with other items (" &
                     "Previous items were <em>$1</em>; this is a <em>$2</em>)"),
  ("TyDiffKey",      "Key type is not consistent with other keys (" &
                     "Previous keys were <em>$1</em>; this is a <em>$2</em>)"),
  ("TyDiffValue",    "Value type is not consistent with other values (" &
                     "Previous values were <em>$1</em>; this is a " &
                     "<em>$2</em>)"),
  ("VarInSecDef",    "In explicit section declarations, the `=` is " &
                     "expecting valid attributes only on the LHS, but " &
                     "<em>$1</em> is not allowed as a top-level attribute." &
                     "Assuming this is a variable, not an attribute. " &
                     "If it should be an attribute, fix the specification. " &
                     "Otherwise, to get rid of this warning, either move " &
                     "the assignment outside the section block, or " &
                     "Use the <em>=</em> assignment operator, which " &
                     "forces variable assignment."),
  ("TryVarAssign",   "The attribute specifcation doesn't allow this field. " &
                     "If you'd like a variable with this name, you can " &
                     "use the <em>=</em> operator, which creates a" &
                     "variable, even when there's an attribute of the " &
                     "same name."),
  ("AssignToSec",    "Cannot assign directly to <em>$1</em>; it is a " &
                     "section that supports multiple instances, not a " &
                     "field within a section."),
  ("AsgnSingleton",  "Cannot assign directly to <em>$1</em>; it is a single" &
                     "section that contains fields, not a field within " &
                     "a section."),
  ("AsgnInstance",   "Cannot assign directly to <em>$1</em>; the parent " &
                     "section supports multiple instances, so this name " &
                     "would be an instance, to which you can then add fields."),
  ("SecUnderField",  "Cannot assign; <em>$1</em> is a field, so may not " &
                     "contain sub-fields."),
  ("SectionNoSpec",  "While <em>$1</em> is a valid section, there is no " &
                     "known section type named <em>$2</em>."),
  ("RootNoSec",      "There is no allowed section or attribute named: " &
                     "<em>$1</em>"),
  ("SectionDenied",  "While <em>$2</em> is a known section type, it is not " &
                     "permitted within <em>$1</em>."),
  ("RootSecDenied",  "The root attribute scope doesn't allow <em>$1<em> " &
                     "sections."),
  ("AttrNotSpecd",   "<em>$1</em> is not an allowed attribute name."),
  ("BadSectionType", "There isn't an allowed section type named <em>$1</em>."),
  ("SecNotAllowed",  "A <em>$1</em> section is not allowed from within $2."),
  ("NotASingleton",  "The <em>$1</em> section expects an instance name."),
  ("IsASingleton",   "A <em>$1</em> section does not allow named instances;" &
                     " there is only one unnamed section."),
  ("TypeVsSpec",     "The type of this use (<em>$1</em>) is not compatible " &
                     "with the specified type (<em>$2</em>)"),
  ("UnsignedUMinus", "Unary minus would turn an unsigned value to a signed " &
                     "value; cast either to the same size (which you " &
                     "shouldn't do if the int value might be larger than the " &
                     "highest signed value), or cast to the next size up if " &
                     "possible."),
  ("128BitLimit",    "$1 is not currently supported for 128-bit integers."),
  ("U64Div",         "Division producing a float isn't currently defined for " &
                     "unsigned 64-bit integers."),
  ("TupleLhs",       "When unpacking a tuple, all items on the left hand " &
                     "side of the assignment must be valid variables, and " &
                     "cannot currently be attributes."),
  ("MemberTop",      "Attribute member access (.) can only be applied " &
                     "to attributes, not to values."),
  ("TupleConstIx",   "When indexing a tuple, the index must evaluate to " &
                     "a constant integer."),
  ("TupleIxBound",   "Constant index is out of bounds for the tuple being " &
                     "indexed."),
  ("ContainerType",  "Cannot distinguish what kind of container type this " &
                     "is (could be a dict, list, tuple, etc.) " &
                     " Please explicitly declare this type."),
  ("BadUrl",         "Invalid URL for loading con4m source code."),
  ("InsecureUrl",    "Warning: loading file from an insecure URL. " &
                     "The contents could be injected by an attacker."),
  ("NoImpl",         "Could not find any function implementations in scope " &
                     "named <em>$1</em>. Full signature: <em>$1$2</em>"),
  ("NotAFunc",       "There is a variable named <em>$1</em>, but there was " &
                     "Not a function in scope with the signature: " &
                     "<em>$1$2</em>"),
  ("BadSig",         "No implementation of <em>$1</em> matched this $3. " &
                     "The $3 had the type: <em>$1$2</em>"),
  ("CallAmbig",      "Found multiple functions matching <em>$1$2" &
                     "</em> Please disambiguate."),
  ("NotIndexible",   "Type <em>$1</em> is not indexible."),
  ("CantLiftFunc",   "Function <em>$1</em> has the same name as a " &
                     "global variable. Currently, other modules will have to " &
                     "explicitly qualify the module to call this function."),
  ("DoesntExit",     "Control does not reach the end of this $1."),
  ("ExprResult",     "Result of expression is unused. Expression result is " &
                     "of type <em>$1</em>. Please assign to _ to discard."),
  ("InfLoop",        "While loop does not exit."),
  ("UseBeforeDef",   "Likely use of <em>$1</em> before assignment;" &
                     " If you think this is wrong, set a default value " &
                     "at the top of this scope."),
  ("ConstReassign",  "This variable has been declared constant, and was " &
                     "previously assigned."),
  ("Immutable",      "Cannot modify immutable values."),
  ("DefWoUse",       "Variable <em>$1</em> is defined, but never used."),
  ("ConstNotSet",    "Constant <em>$1</em> was declared, but no value was " &
                     "set."),
  ("$assign",        "Variables starting with <em>$</em> are set by the " &
                     "system, and cannot be otherwise assigned."),
  ("SigOverlap",     "In this module, for the function name <em>$1</em>, " &
                     "implementations have overlapping signatures:<br>" &
                     "2. <strong>$3</strong><br>" &
                     "1. <strong>$2</strong> (line $4)."),
  ("NextCase",       "Statement seemed to end, and was expecting " &
                     "another <em>case</em> branch, an <em>else</em>, " &
                     " or <em>}</em> to end the cases."),
  ("CaseBodyStart",  "Case bodies may either be regular blocks (i.e., " &
                     "<em>{ ... }</em>) or can be a colon followed by a list " &
                     "of statements."),
  ("DeadTypeCase",   "Variable can never be of type <em>$1</em>; case cannot " &
                     "be taken."),
  ("BadIPv4",        "Invalid <em>IPv4</em> address."),
  ("BadPort",        "Invalid <em>port</em> number."),
  ("BadDuration",    "Invalid <em>duration</em> literal."),
  ("BadSize",        "Invalid <em>size</em> literal."),
  ("BadUrl",         "Invalid <em>URL</em> literal."),
  ("BadDateTime",    "Invalid literal value for <em>datetime</em> type."),
  ("BadDate",        "Invalid literal value for <em>date</em> type."),
  ("BadTime",        "Invalid literal value for <em>time</em> type."),
  ("InvalidOther",   "Invalid literal, did not match any known literal type."),
  ("OtherLit",       "Inferred type of literal is <em>$1</em>. If incorrect, " &
                     "place in quotes and provide an explicit literal " &
                     "modifier after (e.g., <em>\"2 gb\"'size</em> for " &
                     "a size literal.)"),
  ("OtherQuotes",    "The <em>:=</em> operator is used for values of special " &
                     "types, where the system takes all input till the end of " &
                     "the line, then tries to treat it as a primitive type. " &
                     "The quotes here are treated like they're inside the " &
                     "data you're providing, which may not be what you want."),
  ("OtherBrak",      "The <em>:=</em> operator does not process brackets " &
                     "or parentheses of any kind; this will be treated as " &
                     "a single string."),
  ("OtherNum",       "The <em>:=</em> operator does not result in numeric " &
                     "types; it asks the system to guess from a number of " &
                     "specialized types. Use <em>=</em> or add a modifier " &
                     "that's appropriate for the type of value you're trying " &
                     "to create."),
  ("AttrUse",        "Attempted to use an attribute <em>$1</em> that has not " &
                     "been set."),

  ("RT_BadTOp",      "Unknown function id for internal type API call " &
                     "(<em>$1</em>)"),
  ("Debug",          "Debug: $1 $2 $3"),
 ]

proc baseError*(list: var seq[Con4mError], code: string, cursor: StringCursor,
                modname: string, line: int, lineOffset: int,
                phase: Con4mErrPhase, severity = LlFatal,
                extraContents: seq[string] = @[], detail: Rope = nil,
                trace: string = "", ii: Option[InstantiationInfo] =
                                  none(InstantiationInfo)) =
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

  ctx.errors.baseError(basemsg, t.cursor, ctx.modname, t.lineNo,
                       t.lineOffset, ErrLex, LlFatal, subs)

proc lexFatal*(ctx: Module, basemsg: string, t: Con4mToken = nil) =
  ctx.lexBaseError(basemsg, t)
  raise newCon4mException()

template lexError*(msg: string, t: Con4mToken = nil) =
  ctx.lexFatal(msg, t)

proc baseError*(list: var seq[Con4mError], code: string, node: Con4mNode,
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
                  w = Con4mNode(nil), detail = Rope(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlFatal, extra,
                       detail)

template irError*(ctx: Module, msg: string, w: IrNode,
                  extra: seq[string] = @[], detail: Rope = nil) =
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlFatal, extra,
                       detail)

template irNonFatal*(ctx: Module, msg: string, extra: seq[string] = @[],
                w = Con4mNode(nil)) =
  # Things we consider errors, but we may end up allowing. Currently, this
  # is just for use-before-def errors.
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlErr, extra)

template irNonFatal*(ctx: Module, msg: string, w: IrNode,
                     extra: seq[string] = @[]) =
  # Things we consider errors, but we may end up allowing. Currently, this
  # is just for use-before-def errors.
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlErr, extra)

template irWarn*(ctx: Module, msg: string, extra: seq[string] = @[],
                w = Con4mNode(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlWarn, extra)

template irWarn*(ctx: Module, msg: string, w: IrNode,
                 extra: seq[string] = @[]) =
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlWarn, extra)

template irInfo*(ctx: Module, msg: string, extra: seq[string] = @[],
                w = Con4mNode(nil)) =
  var where = if w == nil: ctx.pt else: w
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlInfo, extra)

template irInfo*(ctx: Module, msg: string, w: IrNode,
                 extra: seq[string] = @[]) =
  var where = if w == nil: ctx.pt else: w.parseNode
  ctx.errors.baseError(msg, where, ctx.modname, ErrIrGen, LlInfo, extra)

template loadError*(ctx: CompileCtx, msg: string, modname: string,
                    extra: seq[string] = @[]) =
  ctx.errors.baseError(msg, Con4mNode(nil), modname, ErrLoad, LlFatal, extra)

template loadWarn*(ctx: CompileCtx, msg: string, modname: string,
                    extra: seq[string] = @[]) =
  ctx.errors.baseError(msg, Con4mNode(nil), modname, ErrLoad, LlWarn, extra)

proc canProceed*(errs: seq[Con4mError]): bool =
  for err in errs:
    if err.severity in [LlErr, LlFatal]:

      return false
  return true

template canProceed*(ctx: CompileCtx): bool =
  ctx.errors.canProceed()

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
    let offset = td(text(`$`(err.line) & ":" & `$`(err.offset + 1) & ":"))
    result.add(modname.overflow(OTruncate))
    result.add(offset.overflow(OTruncate))
  else:
    result.add(text(""))
    result.add(text(""))

  result.add(s.htmlStringToRope(markdown = false, add_div = false))

proc getVerboseInfo(err: Con4mError): Rope =
  var
    noSource = false
    noLoc    = false

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

proc formatErrors*(errs: seq[Con4mError], verbose = true): Rope =
  var errList: seq[seq[Rope]]
  let
    mw = errs.getModuleWidth() + 1
    lw = errs.getLocWidth() + 1

  for i, error in errs:
    if i == 30: # TODO; make this a configurable limit.
      break
    var msg = error.lookupMsg() & "<i> (" & error.code & ")</i>"
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

proc runtimeWarn*(err: string, args: seq[string] = @[]) =
  var
    rawText = err
    toOut: Rope

  for (k, v) in errorMsgs:
    if k == err:
      rawText = v
      break

  for i, item in args:
    rawText = rawText.replace("$" & `$`(i + 1), item)

  toOut = fgColor("Warning: ", "yellow") + markdown(rawText)
  print(err, file = stderr)
