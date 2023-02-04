## Data types used across the project.  We generally try to keep types
## out of this file if they're only used in one module (particularly
## true in macros.nim, which is all compile-time).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022


import streams, tables, options, sugar, macros, nimutils

type
  ## Enumeration of all possible lexical tokens. Should not be exposed
  ## outside the package.
  Con4mTokenKind* = enum
    TtWhiteSpace, TtSemi, TtNewLine, TtLineComment, TtLockAttr, TtExportVar,
    TtPlus, TtMinus, TtMul, TtLongComment, TtDiv, TTMod, TtLte, TtLt, TtGte,
    TtGt, TtNeq, TtNot, TtLocalAssign, TtColon, TtAttrAssign, TtCmp, TtComma,
    TtPeriod, TtLBrace, TtRBrace, TtLBracket, TtRBracket, TtLParen, TtRParen,
    TtAnd, TtOr, TtIntLit, TtFloatLit, TtStringLit, TtTrue, TtFalse, TtNull,
    TTIf, TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtReturn,
    TtEnum, TtIdentifier, TtFunc, TtCallback, TtVar, TtSof, TtEof, ErrorTok,
    ErrorLongComment, ErrorStringLit

  Con4mToken* = ref object
    ## Lexical tokens. Should not be exposed outside the package.
    case kind*:   Con4mTokenKind
    of TtStringLit:
      unescaped*: string
    else:  nil
    stream*:      Stream
    startPos*:    int
    endPos*:      int
    lineNo*:      int
    lineOffset*:  int

  Con4mNodeKind* = enum
    ## Parse tree nodes types. Really no reason for these to be
    ## exposed either, other than the fact that they're contained in
    ## state objects that are the primary object type exposed to the
    ## user.
    NodeBody, NodeAttrAssign, NodeAttrSetLock, NodeVarAssign, NodeVarSetExport,
    NodeUnpack, NodeSection, NodeIfStmt, NodeConditional, NodeElse, NodeFor,
    NodeBreak, NodeContinue, NodeReturn, NodeSimpLit, NodeUnary, NodeNot,
    NodeMember, NodeIndex, NodeActuals, NodeCall, NodeDictLit, NodeKVPair,
    NodeListLit, NodeTupleLit, NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte,
    NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv,
    NodeEnum, NodeIdentifier, NodeFuncDef, NodeFormalList, NodeTypeDict,
    NodeTypeList, NodeTypeTuple, NodeTypeString, NodeTypeInt, NodeTypeFloat,
    NodeTypeBool, NodeVarDecl, NodeVarSymNames

  Con4mTypeKind* = enum
    ## The enumeration of possible top-level types in Con4m
    TypeString, TypeBool, TypeInt, TypeFloat, TypeTuple, TypeList, TypeDict,
    TypeProc, TypeTVar, TypeBottom

  Con4mType* = ref object
    ## The internal representation of a type.  Generally, you should
    ## write strings, and con4m will parse them.
    case kind*:     Con4mTypeKind
    of TypeTuple:
      itemTypes*:   seq[Con4mType]
    of TypeList:
      itemType*:    Con4mType
    of TypeDict:
      keyType*:     Con4mType
      valType*:     Con4mType
    of TypeProc:
      params*:      seq[Con4mType]
      va*:          bool
      retType*:     Con4mType
    of TypeTVar:
      varNum*:      int
      link*:        Option[Con4mType]
      linksin*:     seq[Con4mType]
      cycle*:       bool
      constraints*: set[Con4mTypeKind]

    else: discard

  # So I can switch between ordered and not without hardship.
  Con4mDict*[K, V] = TableRef[K, V]

  ## At any point in a Con4m program, there are two different scopes,
  ## variable scopes (which change whenever we enter a new block
  ## like in a for loop), and attribute scopes, which nest based on
  ## sections.
  ##
  ## Conceptually, the program that loads the configuration file is
  ## expected to only make use of the attributes; the variables are
  ## private to the config file's execution.
  ##
  ## This helps make it easy for users to do computation, without
  ## polluting the runtime namespace, or making validation more
  ## challenging.
  ##
  AttrScope* = ref object
    name*:     string
    parent*:   Option[AttrScope]
    config*:   ConfigState
    contents*: Table[string, AttrOrSub]

  AttrOrSub* = object
    case kind*: bool
    of true:
      attr*: Attribute
    of false:
      scope*: AttrScope

  AttrOrErr* = object
    case kind*: bool
    of true:
      aos*: AttrOrSub
    of false:
      err*: AttrErr

  AttrSetHook* = (seq[string], Box) -> bool

  Attribute* = ref object
    name*:        string
    scope*:       AttrScope
    tInfo*:       Con4mType
    value*:       Option[Box]
    override*:    Option[Box]
    locked*:      bool
    lockOnWrite*: bool
    firstDef*:    Option[Con4mNode]

  VarSym*    = ref object
    name*:     string
    tInfo*:    Con4mType
    value*:    Option[Box]
    persists*: bool
    locked*:   bool
    firstDef*: Option[Con4mNode]
    defs*:     seq[Con4mNode]

  VLookupOp*   = enum vlDef, vlUse, vlMask, vlFormal
  ALookupOp*   = enum vlSecDef, vlAttrDef, vlSecUse, vlAttrUse, vlExists
  UseCtx*      = enum ucNone, ucFunc, ucAttr, ucVar
  AttrErrEnum* = enum
    errNoAttr, errBadSec, errBadAttr, errCantSet, errOk

  AttrErr* = object
    code*:     AttrErrEnum
    msg*:      string

  VarScope*  = ref object
    parent*:    Option[VarScope]
    contents*:  Table[string, VarSym]

  ## Frame for holding local variables.  In a call, the caller
  ## does the pushing and popping.
  RuntimeFrame*  = TableRef[string, Option[Box]]
  VarStack*      = seq[RuntimeFrame]
  Con4mSectInfo* = seq[(string, AttrScope)]

  Con4mNode* = ref object
    ## The actual parse tree node type.  Should generally not be exposed.
    id*:           int
    kind*:         Con4mNodeKind
    token*:        Option[Con4mToken] # Set on terminals, and some non-terminals
    children*:     seq[Con4mNode]
    parent*:       Option[Con4mNode] # Root is nil
    typeInfo*:     Con4mType
    varScope*:     VarScope
    attrScope*:    AttrScope
    value*:        Box
    attrRef*:      Attribute
    procRef*:      FuncTableEntry

  BuiltInFn* = ((seq[Box], ConfigState) -> Option[Box])
  ## The Nim type signature for builtins that can be called from Con4m.
  ## VarStack is defined below, but is basically just a seq of tables.

  FnType* = enum
    FnBuiltIn, FnUserDefined, FnCallback

  FuncTableEntry* = ref object
    tinfo*:       Con4mType
    name*:        string # Need for cycle check error message.
    onStack*:     bool
    cannotCycle*: bool
    locked*:      bool
    case kind*:   FnType
    of FnBuiltIn:
      builtin*:   BuiltInFn
    of FnUserDefined, FnCallback:
      impl*:      Option[Con4mNode]

  ExtendedTypeKind* = enum
    TypePrimitive, TypeSection, TypeC4TypeSpec, TypeC4TypePtr

  ExtendedType* = ref object
    validator*: string   # A con4m call used in fields, not sections.
    case kind*: ExtendedTypeKind
    of TypePrimitive:
      tinfo*:      Con4mType
      range*:      tuple[low: int, high: int] # Only for int types; INCLUSIVE.
      itemCount*:  tuple[low: int, high: int] # Should reuse (TODO)
      intChoices*: seq[int]
      strChoices*: seq[string]
    of TypeSection:
      sinfo*: Con4mSectionType
    of TypeC4TypePtr:
      fieldRef*: string
    of TypeC4TypeSpec:
      discard

  FieldSpec* = ref object
    extType*:      ExtendedType
    minRequired*:  int
    maxRequired*:  int
    lock*:         bool
    stackLimit*:   int
    default*:      Option[Box]
    exclusions*:   seq[string] # Fields that obviate us.

  Con4mSectionType* = ref object
    typeName*:      string
    singleton*:     bool
    fields*:        Table[string, FieldSpec]
    backref*:       ConfigSpec

  ConfigSpec* = ref object
    secSpecs*:      Table[string, Con4mSectionType]
    rootSpec*:      Con4mSectionType

  ConfigState* = ref object
    ## The top-level representation of a configuration's runtime
    ## state. The symbols are in here, the specs we apply, etc.
    ## Still, the end user should not need to access the members,
    ## except via API.
    numExecutions*:      int
    setHook*:            AttrSetHook
    attrs*:              AttrScope
    keptGlobals*:        Table[string, VarSym]
    frames*:             VarStack
    spec*:               Option[ConfigSpec]
    funcTable*:          Table[string, seq[FuncTableEntry]]
    funcOrigin*:         bool
    waitingForTypeInfo*: bool
    moduleFuncDefs*:     seq[FuncTableEntry] # Typed.
    moduleFuncImpls*:    seq[Con4mNode] # Passed from the parser.
    secondPass*:         bool
    nodeStash*:          Con4mNode # Tracked during builtin func calls, for
                                   # now, just for the benefit of format()
  Con4mPhase* = enum phTokenize, phParse, phCheck, phEval, phValidate

let
  # These are just shared instances for types that aren't
  # parameterized, instead of having to instantiate multiple
  # instances.  Should not be exposed to the user.
  stringType* = Con4mType(kind: TypeString)
  boolType*   = Con4mType(kind: TypeBool)
  intType*    = Con4mType(kind: TypeInt)
  floatType*  = Con4mType(kind: TypeFloat)
  bottomType* = Con4mType(kind: TypeBottom)

proc newCon4mDict*[K, V](): Con4mDict[K, V] {.inline.} =
  return newTable[K, V]()

type
  LookupErr* = enum
    errBadSubscope, errNotFound, errBadSpec, errAlreadyExists
  LookupKind* = enum
    # luMask is only for variables; luExpectAttr
    # luDeclareOnly is only for attrs.
    luExpect, luFindOrDeclare, luMask, luDeclareOnly

template isA*(aos: AttrOrSub, t: typedesc): bool =
  when t is Attribute:
    aos.kind
  elif t is AttrScope:
    not aos.kind
  else:
    static:
      error("isA(AttrOrSub, t): t must be an Attribute or AttrScope")
    false

template get*(aos: AttrOrSub, t: typedesc): untyped =
  when t is Attribute:
    aos.attr
  elif t is AttrScope:
    aos.scope
  else:
    static:
      error("get(AttrOrSub, t): t must be an Attribute or AttrScope")
    nil

template isA*(aoe: AttrOrErr, t: typedesc): bool =
  when t is AttrOrSub:
    aoe.kind
  elif t is AttrErr:
    not aoe.kind
  else:
    static:
      error("isA(AttrOrErr, t): t must be an AttrOrSub or AttrErr")
    false

template get*(aoe: AttrOrErr, t: typedesc): untyped =
  when t is AttrOrSub:
    aoe.aos
  elif t is AttrErr:
    aoe.err
  else:
    static:
      error("get(AttrOrErr, t): t must be an AttrOrSub or AttrErr")
    nil

proc either*(attr: Attribute): AttrOrSub =
  result = AttrOrSub(kind: true, attr: attr)

proc either*(sub: AttrScope): AttrOrSub =
  result = AttrOrSub(kind: false, scope: sub)

proc either*(aos: AttrOrSub): AttrOrErr =
  return AttrOrErr(kind: true, aos: aos)

proc either*(err: AttrErr): AttrOrErr =
  return AttrOrErr(kind: false, err: err)

converter attrToAttrOrSub*(attr: Attribute): AttrOrSub =
  either(attr)

converter subToAttrOrSub*(sub: AttrScope): AttrOrSub =
  either(sub)

converter attrToAttrOrErr*(aos: AttrOrSub): AttrOrErr =
  either(aos)

converter errToAttrOrErr*(err: AttrErr): AttrOrErr =
  either(err)

converter secToExt*(sec: Con4mSectionType): ExtendedType =
  return ExtendedType(kind: TypeSection, sinfo: sec)

converter c4mToExt*(tinfo: Con4mType): ExtendedType =
  return ExtendedType(kind: TypePrimitive, tinfo: tinfo)
