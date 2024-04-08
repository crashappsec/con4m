const errorMsgs* = [
  ("FileNotFound",   "Module could not be located."),
  ("ModuleNotFound", "Module [em]$1[/] could not be located."),
  ("StrayCr",        "Carriage return ([em]\r[/]) without newline."),
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
  ("BadLitMod",      "Unknown literal modifier [em]$1[/] for [em]$2[/]" &
                     " syntax."),
  ("LitModTypeErr",  "Literal modifier [em]$1[/] can't be used with " &
                      "$2 literals"),
  ("MissingTok",     "Expected $1 here."),
  ("MemberSpec",     "[em]'.'[/] operator must have an identifier on " &
                     "both sides."),
  ("ItemExpected",   "Expected either another item or [em]'$1'[/]"),
  ("BadTuple",       "Tuple types must contain two or more items."),
  ("AccessExpr",     "Expected either an identifier or paren here."),
  ("ExprStart",      "Expected the start of an expression here, but got $1."),
  ("BinaryNot",      "[em]'not'[/] operator takes only one operand."),
  ("LitExpected",    "Expected a literal value here."),
  ("ForFromIx",      "[em]for ... from[/] loops must have a single index " &
                     "variable."),
  ("NameInTypeSpec", "[em]'$1'[/] is not a builtin type. " &
                     "Use [em]struct[typename][/] for user types."),
  ("BadOneOf",       "OneOf types must have multiple type options that " &
                     "are more constrained than a generic type variable."),
  ("BadObjType",     "Object types must provide either a name or a type " &
                     "variable if specifying an object where no fields " &
                     "will be referenced."),
  ("BadTypeDecl",    "Invalid syntax inside a type declaration."),
  ("BadFormalEnd",   "Expected either a closing parenthesis ([em]')'[/]" &
                     " or an additional parameter."),
  ("BadLock",        "Attribute Lock operator ([em]~[/]) must be " &
                     "followed either by an assignment statement or " &
                     "an attribute to lock."),
  ("BadUseSyntax",   "[em]'use'[/] statement requires a string literal " &
                     " after [em]'from'[/]"),
  ("BadParamName",   "[em]'parameter'[/] keyword must be followed by " &
                     "an attribute (which can be dotted), or the " &
                     "[em]'var'[/] keyword followed by a local variable " &
                     "name. The variable can be optionally typed."),
  ("BadExternField", "Bad field for [em]'extern'[/] block."),
  ("NeedSig",        "Field requires a function type signature."),
  ("BadRCParam",     "Reference counting specs must either be identifiers " &
                     "that match the names given in the function signature, " &
                     "or the special value [em]return[/] (for incref only)"),
  ("BadCType",       "Invalid C type for external parameter: [em]$1[/]"),
  ("PureBool",       "The [em]'pure'[/] property for external functions " &
                     "must be a boolean value ([em]true[/] or " &
                     "[em]false[/])"),
  ("DupeExtField",   "Duplicate field [em]$1[/] provided for an [em]" &
                     "extern[/] block."),
  ("DupeDllName",    "Duplicate DLL name [em]$1[/] provided for an " &
                     "external function (ignored)."),
  ("DupeVal",        "Duplicate parameter value [em]$1[/] in the " &
                     "[em]$2[/] property of the [em]extern[/] spec."),
  ("ExtNotSpecd",    "None of the external function parameters were given" &
                     " the name [em]$1[/]"),
  ("ExtAllocNHold",  "Extern function parameter [em]$1[/] cannot be " &
                     "spec'd to have the external function hold memory " &
                     "we pass, and to allocate that memory."),
  ("NoMemMan",       "Since [em]$1[/] is not a pointer or array type, " &
                     "memory management will not be performend, and this " &
                     "annotation will be ignored."),
  ("WontLink",       "Was not able to locate the external symbol [em]$1[/]" &
                     ", which may be required for running."),
  ("MissingSym",     "Was not able to locate the external symbol [em]$1[/]" &
                     "; program will crash if it is accessed, unless " &
                     "it is dynamically loaded first."),
  ("PurePlz",        "Please provide a value for the [em]pure[/] property " &
                     "for extern functions. Pure functions always " &
                     "return the same output for the same input, and do " &
                     "not do any I/O, allowing us to pre-execute."),
  ("EofInBlock",     "Block was not closed when end-of-file was found."),
  ("TopLevelOnly",   "[em]'$1'[/] is only allowed at the top-level of a " &
                     "module."),
  ("TopLevelPlural", "$1 are only allowed at the top-level of a module."),
  ("InLoopsOnly",    "[em]'$1'[/] is only allowed inside loops."),
  ("RetOutOfFunc",   "[em]'return'[/] is only allowed inside functions."),
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
  ("HexTooLarge",    "Hex number is too large for type [em]$1[/]"),
  ("IntTooLarge",    "Integer literal is too large for type [em]$1[/]"),
  ("FloatTooLarge",  "Integer portion of float is too large; " &
                     "use [em]'e'[/] notation."),
  ("ExpTooLarge",    "Exponent portion of float is too large."),
  ("BadHex",         "Invalid hex literal"),
  ("BadInt",         "Invalid int literal"),
  ("BadBool",        "Invalid [em]bool[/] literal."),
  ("BadByte",        "Invalid value for a byte (cannot be above 0xff)"),
  ("BadCodepoint",   "Invalid character; unicode values may not be " &
                     "above U+10FFFF."),
  ("BadCP2",         "Invalid character; the unicode standard does not " &
                     "allow values from U+D800 to U+DFFF"),
  ("LoseFormat",     "Conversion discards string formatting information."),
  ("TypeNotSigned",  "Literal is negative, but the specified type " &
                     "is unsigned."),
  ("TypeMismatch",   "[em]$1[/] and [em]$2[/] are incompatible types."),
  ("LoopVarAssign",  "Cannot assign to (or re-declare) loop iteration " &
                     "variables."),
  ("AlreadyAFunc",   "Variable names cannot have names that are identical to " &
                     "functions named in the same module's top-level."),
  ("AlreadyAVar",    "Already found a top-level variable with the same " &
                     "name as this function; this is not allowed within a " &
                     "module."),
  ("VarRedef",       "Variable [em]$1[/] declared multiple times in " &
                     "the same scope."),
  ("LabelDupe",      "Nested loops have the same label ([em]$1[/])."),
  ("LabelLoc",       "[em]label[/] statement must come immediately before " &
                     "either a [em]for[/] loop or a [em]while[/] loop."),
  ("DeadCode",       "Dead code after $1."),
  ("ElifLoc",        "[em]elif[/] statement must follow either an " &
                     "[em]if[/] block, or another [em]elif[/] block."),
  ("ElseLoc",        "[em]else[em] statement must follow either an " &
                     "[em]if[/] block or another [em]elif[/] block."),
  ("BadLoopExit",    "[em]$1[/] to label [em]$2[/] is invalid, because " &
                     "it is not contained inside a loop that have that label."),
  ("DupeParamProp",  "Duplicate parameter property for [em]$1[/]"),
  ("ParamType",      "Parameter property [em]$1[/] must be a $2."),
  ("BadParamProp",   "Invalid property for a parameter: [em]$1[/]"),
  ("EnumDeclConst",  "Enum value assignments must be constants"),
  ("EnumInt",        "Currently, enum values may only be [em]int[em] types"),
  ("EnumReuse",      "Value [em]$1[/] has already been used in this enum."),
  ("BinaryOpCompat", "LHS and RHS do not have compatable types " &
                     "(lhs: [em]$1[/]; rhs: [em]$2[/])."),
  ("CannotCast",     "Cannot convert from type [em]$1[/] to " &
                     "type [em]$2[/]"),
  ("BoolAutoCast",   "This condition (of type [em]$1[/]) is not a " &
                     "boolean value, but is being auto-cast."),
  ("TyDiffListItem", "List item type is not consistent with other items (" &
                     "Previous items were [em]$1[/]; this is a [em]$2[/])"),
  ("TyDiffKey",      "Key type is not consistent with other keys (" &
                     "Previous keys were [em]$1[/]; this is a [em]$2[/])"),
  ("TyDiffValue",    "Value type is not consistent with other values (" &
                     "Previous values were [em]$1[/]; this is a " &
                     "[em]$2[/])"),
  ("VarInSecDef",    "In explicit section declarations, the `=` is " &
                     "expecting valid attributes only on the LHS, but " &
                     "[em]$1[/] is not allowed as a top-level attribute." &
                     "Assuming this is a variable, not an attribute. " &
                     "If it should be an attribute, fix the specification. " &
                     "Otherwise, to get rid of this warning, either move " &
                     "the assignment outside the section block, or " &
                     "Use the [em]=[/] assignment operator, which " &
                     "forces variable assignment."),
  ("TryVarAssign",   "The attribute specifcation doesn't allow this field. " &
                     "If you'd like a variable with this name, you can " &
                     "use the [em]=[/] operator, which creates a" &
                     "variable, even when there's an attribute of the " &
                     "same name."),
  ("AssignToSec",    "Cannot assign directly to [em]$1[/]; it is a " &
                     "section that supports multiple instances, not a " &
                     "field within a section."),
  ("AsgnSingleton",  "Cannot assign directly to [em]$1[/]; it is a single" &
                     "section that contains fields, not a field within " &
                     "a section."),
  ("AsgnInstance",   "Cannot assign directly to [em]$1[/]; the parent " &
                     "section supports multiple instances, so this name " &
                     "would be an instance, to which you can then add fields."),
  ("SecUnderField",  "Cannot assign; [em]$1[/] is a field, so may not " &
                     "contain sub-fields."),
  ("SectionNoSpec",  "While [em]$1[/] is a valid section, there is no " &
                     "known section type named [em]$2[/]."),
  ("RootNoSec",      "There is no allowed section or attribute named: " &
                     "[em]$1[/]"),
  ("SectionDenied",  "While [em]$2[/] is a known section type, it is not " &
                     "permitted within [em]$1[/]."),
  ("RootSecDenied",  "The root attribute scope doesn't allow [em]$1[/] " &
                     "sections."),
  ("AttrNotSpecd",   "[em]$1[/] is not an allowed attribute name."),
  ("BadSectionType", "There isn't an allowed section type named [em]$1[/]."),
  ("SecNotAllowed",  "A [em]$1[/] section is not allowed from within $2."),
  ("NotASingleton",  "The [em]$1[/] section expects an instance name."),
  ("IsASingleton",   "A [em]$1[/] section does not allow named instances;" &
                     " there is only one unnamed section."),
  ("TypeVsSpec",     "The type of this use ([em]$1[/]) is not compatible " &
                     "with the specified type ([em]$2[/])"),
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
                     "named [em]$1[/]. Full signature: [em]$1$2[/]"),
  ("NotAFunc",       "There is a variable named [em]$1[/], but there was " &
                     "Not a function in scope with the signature: " &
                     "[em]$1$2[/]"),
  ("BadSig",         "No implementation of [em]$1[/] matched this $3. " &
                     "The $3 had the type: [em]$1$2[/]"),
  ("CallAmbig",      "Found multiple functions matching [em]$1$2" &
                     "[/] Please disambiguate."),
  ("NotIndexible",   "Type [em]$1[/] is not indexible."),
  ("CantLiftFunc",   "Function [em]$1[/] has the same name as a " &
                     "global variable. Currently, other modules will have to " &
                     "explicitly qualify the module to call this function."),
  ("DoesntExit",     "Control does not reach the end of this $1."),
  ("ExprResult",     "Result of expression is unused. Expression result is " &
                     "of type [em]$1[/]. Please assign to _ to discard."),
  ("InfLoop",        "While loop does not exit."),
  ("UseBeforeDef",   "Likely use of [em]$1[/] before assignment;" &
                     " If you think this is wrong, set a default value " &
                     "at the top of this scope."),
  ("ConstReassign",  "This variable has been declared constant, and was " &
                     "previously assigned."),
  ("Immutable",      "Cannot modify immutable values."),
  ("DefWoUse",       "Variable [em]$1[/] is defined, but never used."),
  ("UseWoDef",       "Definite use of [em]$1[/] without assignment."),
  ("ConstNotSet",    "Constant [em]$1[/] was declared, but no value was " &
                     "set."),
  ("$assign",        "Variables starting with [em]$[/] are set by the " &
                     "system, and cannot be otherwise assigned."),
  ("SigOverlap",     "In this module, for the function name [em]$1[/], " &
                     "implementations have overlapping signatures:\n" &
                     "2. [strong]$3[/]\n" &
                     "1. [strong]$2[/] (line $4)."),
  ("NextCase",       "Statement seemed to end, and was expecting " &
                     "another [em]case[/] branch, an [em]else[/], " &
                     " or [em]}[/] to end the cases."),
  ("CaseBodyStart",  "Case bodies may either be regular blocks (i.e., " &
                     "[em]{ ... }[/]) or can be a colon followed by a list " &
                     "of statements."),
  ("DeadTypeCase",   "Variable can never be of type [em]$1[/]; case cannot " &
                     "be taken."),
  ("BadIPv4",        "Invalid [em]IPv4[/] address."),
  ("BadPort",        "Invalid [em]port[/] number."),
  ("BadDuration",    "Invalid [em]duration[/] literal."),
  ("BadSize",        "Invalid [em]size[/] literal."),
  ("BadUrl",         "Invalid [em]URL[/] literal."),
  ("BadDateTime",    "Invalid literal value for [em]datetime[/] type."),
  ("BadDate",        "Invalid literal value for [em]date[/] type."),
  ("BadTime",        "Invalid literal value for [em]time[/] type."),
  ("InvalidOther",   "Invalid literal; this mechanism is currently disabled."),
    #Used to be: , did not match any known literal type
  ("OtherLit",       "Inferred type of literal is [em]$1[/]. If incorrect, " &
                     "place in quotes and provide an explicit literal " &
                     "modifier after (e.g., [em]\"2 gb\"'size[/] for " &
                     "a size literal.)"),
  ("OtherQuotes",    "The [em]:=[/] operator is used for values of special " &
                     "types, where the system takes all input till the end " &
                     " of the line, then tries to treat it as a primitive " &
                     "type. The quotes here are treated like they're inside " &
                     "the data you're providing, which may not be what you " &
                     "want."),
  ("OtherBrak",      "The [em]:=[/] operator does not process brackets " &
                     "or parentheses of any kind; this will be treated as " &
                     "a single string."),
  ("OtherNum",       "The [em]:=[/] operator does not result in numeric " &
                     "types; it asks the system to guess from a number of " &
                     "specialized types. Use [em]=[/] or add a modifier " &
                     "that's appropriate for the type of value you're trying " &
                     "to create."),
  ("AttrUse",        "Attempted to use an attribute [em]$1[/] that has not " &
                     "been set."),
  ("RT_BadTOp",      "Unknown function id for internal type API call " &
                     "([em]$1[/])"),
  ("NeedExName",     "Local function specs must include names for every " &
                     "parameter."),
  ("DupeExParam",    "Duplicate parameter name for local function: " &
                     "[em]$1[/]"),
  ("ExternZArgCt",   "Local function spec has more arguments than external " &
                     "function for [em]$1[/], which is currently not " &
                     "allowed. C function has $3 args, local has $2 ($4)."),
  ("ExternCArgCt",   "External function spec has more arguments than local " &
                     "function for [em]$1[/], which is currently not " &
                     "allowed. C function has $3 args, local has $2."),
  ("ArrayIxErr",     "Array index [em]$1[/] is not in bounds."),
  ("DictKeyErr",     "Could not find dictionary key: [em]$1[/]."),
  ("LockedAttr",     "Attempted to set attribute [em]$1[/], which is " &
                     "locked. Current value: [em]$2[/] Attempted value: " &
                     "[em]$3[/]"),
  ("AlreadyLocked",  "Tried to set lock-on-write for attribute [em]$1[/], " &
                     "but it is already locked."),
  ("DupeSection",    "Section [em]$1[/] cannot be specified twice"),
  ("SpecFieldType",  "Specification field [em]$1[/] was expected to be " &
                     "of type [em]$3[/], but was of type [em]$2[/]"),
  ("RequiredProp",   "Specified field [em]$1[/] does not have the " &
                     "required property [em]$2[/]."),
  ("SpecLock",       "Cannot make changes to the attribute specification; " &
                     "it is locked."),
  ("NoSecSpec",      "When processing [em]$1[/], there is no section " &
                     "specification for [em]$2[/]"),
  ("MissingField",   "Attribute [em]$1[/] must be set, but was not " &
                     "provided."),
  ("NoSecType",      "In the top level of a [em]confspec[/] block, got " &
                     "[em]$1[/] but only allowed contents are: " &
                     "[em]root[/], [em]singleton[/] or [em]named[/]" &
                     " blocks."),
  ("InstSecStart",   "Was expecting this to start an instantiation " &
                     "of a [em]$1[/] section named [em]$2[/], but " &
                     "would need a [em]{[/] here."),
  ("RootOverwrite",  "Overwriting an existing root $1."),
  ("MissingSec",     "Spec tries to [em]$2[/] a section of type [em]$1[/]" &
                     ", but no section named [em]$1[/] has been spec'd yet."),

  ("BadRange",       "When validating [em]$1[/], an invalid value, " &
                     "[em]$2[/], was chosen. But the value must be no less " &
                     "than [em]$3[/] and no greater than [em]$4[/]."),
  ("BadChoice",      "When validating [em]$1[/], an invalid value, " &
                     "[em]$2[/], was chosen. Valid choices are: [em]$3[/]"),
  ("MissingSection", "When validating [em]$1[/], expected a required " &
                     "section named [em]$2[/], which was not provided."),
  ("MissingField",   "When validating [em]$1[/], expected a required " &
                     "field named [em]$2[/], but it was not found."),
  ("BadSection",     "When validating [em]$1[/], the section [em]$2[/] " &
                     "is not allowed."),
  ("NotTSpec",       "When attempting to determine the type of the " &
                     "value [em]$1$2[/], the field [em]$3[/] was " &
                     "expected to specify the value's type, but " &
                     "that field did not contain a type specification."),
  ("BadField",       "When validating [em]$1[/], found the field " &
                     "[em]$2[/], which is not a valid field name for a " &
                     "[em]$3[/] section."),
  ("FieldMutex",     "When validating [em]$1[/], found the field " &
                     "[em]$2[/], which is specified to not be able " &
                     "to appear together in a section with the field " &
                     "[em]$3[/] (which was also present)."),
  ("ExternVarargs",  "External variable argument functions are not yet " &
                     " supported (external function [em]$1[/])"),
  ("StackOverflow",  "Exceeded the maximum stack depth."),
  ("NoSpecForSec",   "Used a section [em]$1[/], which is not defined " &
                     "by the attribute specification."),
  ("InvalidStart",   "Found the start of an attribute assignment that " &
                     "is invalid according to the spec, at: [em]$1[/]"),
  ("NoInstance",     "This section must have an [i]instance[/], meaning " &
                     "you must specify named sections underneath it."),
  ("DupeProp",       "Property [em]$1[/] cannot appear twice in one " &
                     "item spec"),
  ("NotBool",        "Property [em]$1[/] is reuquired to be bool, " &
                     "but here is [em]$2[/])"),
  ("RangeAndChoice", "Cannot have both [em]range[/] and [em]choice[/] " &
                     "constraints for the same field."),
  ("TyDiffListItem", "Inconsistent item type for choice. Previously " &
                     "it type was [em]$1[/], but here it was [em]$2[/]"),
  ("BadRangeSpec",   "Start value for a range must be less than the end " &
                     "value."),
  ("SpecWhenLocked", "Cannot add [em]confspec[/] fields; the " &
                     "specification has already been locked."),
  ("DupeSpecField",  "Duplicate specification for field [em]$1[/]."),
  ("TCaseOverlap",   "Type cases have overlapping types."),
  ("DupeExclusion",  "Exclusion duplicated for [em]$1[/]."),
  ("DupeAllow",      "Section [em]$1[/] appears multiple times in " &
                     "[i]allow[/] list"),
  ("AllowInReq",     "Section [em]$1[/] appears in both [i]require[/]" &
                     "and [i]allow[/]; suggest removing from [i]allow[/]"),
  ("DupeRequire",    "Section [em]$1[/] appears multiple times in " &
                     "[i]require[/] list"),
  ("DupeRootSpec",   "Should not have duplicate [em]root[/] section" &
                     "in one module"),
  ("NotConst",       "Value must be a constant at compile time."),
  ("ParamValParTy",  "The validation function for this parameter takes an " &
                     "argument that is inconsistent with the type we have " &
                     "for the parameter. Previously, we had [em]$1[/], " &
                     "But the function takes a $2."),
  ("ParamValNArgs",  "Validation functions for parameters must take a " &
                     "single argument, where a value to validate will be " &
                     "passed. It must return a string; the empty string " &
                     "indicates no validation error. Otherwise, the return " &
                     "value should be an error message."),
  ("ParamValRetTy",  "Validation functions for parameters must return " &
                     "a string, which represents any error message to " &
                     "give as feedback."),
  ("NoCbMatch",      "Could not find a function to match to the callback " &
                     "[em]$1$2[/]"),
  ("ParamNotSet",    "Module parameter [em]$1[/] was not set when entering " &
                     "module [em]$2[/]."),
  ("ParamNotValid",  "Module parameter [em]$1[/] was set when entering " &
                     "module [em]$2[/], but was not valid: [em]$3[/]"),
  ("DefaultMutex",   "Module parameters cannot provide both a " &
                     "[em]default[/] value and an [em]initialize[/]. " &
                     "The initializer is intended for computing a default " &
                     "value when needed."),
  ("InitArg",        "Parameter initializer callbacks do not take take " &
                     "any arguments; they only return a value used to " &
                     "initialize the parameter."),
  ("LitRequired",    "Currently, $1 must be a $2 literal value, " &
                     "and cannot be computed or taken from a variable."),
  ("CantInitialize", "Parameter cannot be initialized if it is private, " &
                     "because no default value has been set, and no " &
                     "[em]initialize[/] callback has been set. If this " &
                     "value is going to be initialized every time in " &
                     "the module, then it shouldn't be a parameter."),
 ]
