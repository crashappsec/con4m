## Data types used across the project.  We generally try to keep types
## out of this file if they're only used in one module (particularly
## true in macros.nim, which is all compile-time).
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import unicode, tables, options, sugar, nimutils

type
  StringCursor* = ref object
    runes*: seq[Rune]
    i*:     int

  ## Enumeration of all possible lexical tokens. Should not be exposed
  ## outside the package.
  Con4mTokenKind* = enum
    TtWhiteSpace, TtSemi, TtNewLine, TtLineComment, TtLockAttr, TtExportVar,
    TtPlus, TtMinus, TtMul, TtLongComment, TtDiv, TTMod, TtLte, TtLt, TtGte,
    TtGt, TtNeq, TtNot, TtLocalAssign, TtColon, TtAttrAssign, TtCmp, TtComma,
    TtPeriod, TtLBrace, TtRBrace, TtLBracket, TtRBracket, TtLParen, TtRParen,
    TtAnd, TtOr, TtIntLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse,
    TTIf, TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtReturn,
    TtEnum, TtIdentifier, TtFunc, TtVar, TtOtherLit, TtBacktick, TtArrow,
    TtObject, TtWhile, TtSof, TtEof, ErrorTok,
    ErrorLongComment, ErrorStringLit, ErrorCharLit, ErrorOtherLit, ErrorLitMod

  Con4mToken* = ref object
    ## Lexical tokens. Should not be exposed outside the package.
    case kind*:   Con4mTokenKind
    of TtStringLit:
      unescaped*: string
    of TtCharLit:
      codepoint*: int
    else:  nil
    cursor*:      StringCursor
    id*:          int
    startPos*:    int
    endPos*:      int
    lineNo*:      int
    lineOffset*:  int
    litType*:     string
    adjustment*:  int

  TokenBox* = ref object
    tokens*: seq[Con4mToken]

  Con4mNodeKind* = enum
    ## Parse tree nodes types. Really no reason for these to be
    ## exposed either, other than the fact that they're contained in
    ## state objects that are the primary object type exposed to the
    ## user.
    NodeModule, NodeBody, NodeAttrAssign, NodeAttrSetLock, NodeVarAssign, 
    NodeUnpack, NodeSection, NodeIfStmt, NodeElifStmt, NodeElseStmt, 
    NodeForStmt, NodeWhileStmt, NodeBreakStmt,
    NodeContinueStmt, NodeReturnStmt, NodeStringLit, NodeIntLit, NodeFloatLit, NodeBoolLit, NodeNot,
    NodeMember, NodeLiteral,
    NodeIndex, NodeActuals, NodeCall, NodeDictLit, NodeKVPair, NodeListLit, NodeOtherLit,
    NodeTupleLit, NodeCharLit, NodeCallbackLit, NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte,
    NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv,
    NodeEnumStmt, NodeIdentifier, NodeFuncDef, NodeFormalList, NodeTypeSpec,
    NodeType, NodeTypeVar, NodeTypeFunc, NodeTypeTuple, NodeTypeList,
    NodeTypeDict, NodeTypeObj, NodeTypeRef, NodeTypeTypeSpec, NodeTypeBuiltin, NodeReturnType, NodeTypeVararg, NodeParenExpr,
    NodeVarStmt, NodeExportStmt, NodeVarSymNames, NodeUseStmt, NodeParamBlock, NodeExpression, NodeFormal, NodeNoCallbackName

  Con4mTypeKind* = enum
    ## The enumeration of possible top-level types in Con4m
    TypeString, TypeBool, TypeInt, TypeFloat, TypeTuple, TypeList, TypeDict,
    TypeChar, TypeDuration, TypeIPAddr, TypeCIDR, TypeSize, TypeDate, TypeTime,
    TypeDateTime, TypeTypeSpec, TypeFunc, TypeTVar, TypeBottom

  Con4mType* = ref object of RootRef
    case kind*:     Con4mTypeKind
    of TypeTuple:
      itemTypes*:   seq[Con4mType]
    of TypeList:
      itemType*:    Con4mType
    of TypeDict:
      keyType*:     Con4mType
      valType*:     Con4mType
    of TypeFunc:
      params*:      seq[Con4mType]
      va*:          bool
      retType*:     Con4mType
      noSpec*:      bool   # Was a sig there? For callbacks, it is optional.
    of TypeTypeSpec:
      binding*:     Con4mType
    of TypeTVar:
      varNum*:      int
      localName*:   Option[string]
      link*:        Option[Con4mType]
      linksin*:     seq[Con4mType]
      cycle*:       bool
      components*:  seq[Con4mType]
    else: discard

  Con4mDuration* = uint64
  Con4mSize*     = uint64
  Con4mIPAddr*   = string
  Con4mCIDR*     = string
  Con4mDate*     = string # Stored as an ISO 8601 date
  Con4mTime*     = string # Stored as an ISO 8601 time
  Con4mDateTime* = string # Stored as an ISO 8601 date/time

  # So I can switch between ordered and not without hardship.
  Con4mDict*[K, V] = OrderedTableRef[K, V]

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
    contents*: OrderedTable[string, AttrOrSub]

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
    defs*:        seq[Con4mNode]
    uses*:        seq[Con4mNode]
    lastUse*:     Option[Con4mNode]

  VarSym*    = ref object
    name*:     string
    tInfo*:    Con4mType
    value*:    Option[Box]
    persists*: bool
    locked*:   bool
    firstDef*: Option[Con4mNode]
    defs*:     seq[Con4mNode]
    uses*:     seq[Con4mNode]

  VLookupOp*   = enum vlDef, vlUse, vlMask, vlFormal
  ALookupOp*   = enum vlSecDef, vlAttrDef, vlSecUse, vlAttrUse, vlExists
  UseCtx*      = enum ucNone, ucFunc, ucAttr, ucVar
  AttrErrEnum* = enum
    errOk, errNoAttr, errBadSec, errBadAttr, errCantSet, errBadType

  AttrErr* = object
    code*:     AttrErrEnum
    msg*:      string

  VarScope*  = ref object
    parent*:    Option[VarScope]
    contents*:  OrderedTable[string, VarSym]

  ## Frame for holding local variables.  In a call, the caller
  ## does the pushing and popping.
  RuntimeFrame*  = OrderedTableRef[string, Option[Box]]
  VarStack*      = seq[RuntimeFrame]
  Con4mSectInfo* = seq[(string, AttrScope)]

  Con4mNode* = ref object
    ## The actual parse tree node type.  Should generally not be exposed.
    id*:           int
    prevTokenIx*:  int
    depth*:        int
    kind*:         Con4mNodeKind
    token*:        Option[Con4mToken] # Set on terminals, and some non-terminals
    children*:     seq[Con4mNode]
    allTokens*:    TokenBox

  BuiltInFn* = ((seq[Box], ConfigState) -> Option[Box])
  ## The Nim type signature for builtins that can be called from Con4m.
  ## VarStack is defined below, but is basically just a seq of tables.

  Con4mCustomBuiltinInfo* = (string, BuiltinFn, string, seq[string])
  ## What callers have to pass to make custom builtins available

  FnType* = enum
    FnBuiltIn, FnUserDefined

  FuncTableEntry* = ref object
    tinfo*:       Con4mType
    name*:        string
    onStack*:     bool
    cannotCycle*: bool
    locked*:      bool
    doc*:         Option[string]
    tags*:        seq[string]
    hidden*:      string          # Not yet implemented.
    case kind*:   FnType
    of FnBuiltIn:
      builtin*:   BuiltInFn
    of FnUserDefined:
      impl*:      Option[Con4mNode]

  CallbackObj* = ref object of RootRef
    # Right now, this doesn't even stash a pointer; we could cache
    # this, but we accept callbacks that aren't provided, so we
    # currently just defer until runtime to look up the function
    # anyway.  Also helps make it easy to handle the case where a
    # function's entry is dynamically replaced via a stack.
    name*:  string
    tInfo*: Con4mType

  ExtendedTypeKind* = enum
    TypePrimitive, TypeSection, TypeC4TypeSpec, TypeC4TypePtr

  ExtendedType* = ref object
    validator*: CallbackObj
    case kind*: ExtendedTypeKind
    of TypePrimitive, TypeC4TypeSpec:
      tInfo*:      Con4mType
      range*:      tuple[low: int, high: int] # Only for int types; INCLUSIVE.
      itemCount*:  tuple[low: int, high: int] # Should reuse (TODO)
      intChoices*: seq[int]
      strChoices*: seq[string]
    of TypeSection:
      sinfo*: Con4mSectionType
    of TypeC4TypePtr:
      fieldRef*: string

  FieldSpec* = ref object
    extType*:      ExtendedType
    minRequired*:  int
    maxRequired*:  int
    lock*:         bool
    stackLimit*:   int
    default*:      Option[Box]
    exclusions*:   seq[string]    # Fields that obviate us.
    shortdoc*:     Option[string] # Short doc about this section.
    doc*:          Option[string] # Long-form documentation about this section.
    hidden*:       bool           # Hide this field from documentation APIs.

  Con4mSectionType* = ref object
    typeName*:      string
    singleton*:     bool
    fields*:        OrderedTable[string, FieldSpec]
    backref*:       ConfigSpec
    shortdoc*:      Option[string]
    doc*:           Option[string] # Any doc to provide about this section.
    hidden*:        bool           # Hide this section from documentation APIs
    validator*:     CallbackObj

  ConfigSpec* = ref object
    secSpecs*:      OrderedTable[string, Con4mSectionType]
    rootSpec*:      Con4mSectionType

  ConfigState* = ref object
    ## The top-level representation of a configuration's runtime
    ## state. The symbols are in here, the specs we apply, etc.
    ## Still, the end user should not need to access the members,
    ## except via API.
    numExecutions*:      int
    setHook*:            AttrSetHook
    attrs*:              AttrScope
    keptGlobals*:        OrderedTable[string, VarSym]
    frames*:             VarStack
    spec*:               Option[ConfigSpec]
    funcTable*:          OrderedTable[string, seq[FuncTableEntry]]
    funcOrigin*:         bool
    waitingForTypeInfo*: bool
    moduleFuncDefs*:     seq[FuncTableEntry] # Typed.
    moduleFuncImpls*:    seq[Con4mNode] # Passed from the parser.
    secondPass*:         bool
    lockAllAttrWrites*:  bool
    nodeStash*:          Con4mNode # Tracked during builtin func calls, for
                                   # now, just for the benefit of format()
    currentComponent*:   ComponentInfo
    programRoot*:        ComponentInfo
    components*:         Table[string, ComponentInfo]

  Con4mPhase*   = enum phTokenize, phParse, phCheck, phEval, phValidate
  FieldColType* = enum
    fcName, fcFullName, fcType, fcDefault, fcValue, fcShort, fcLong, fcProps

  ComponentInfo* = ref object
    url*:             string
    version*:         (int, int, int)
    desc*:            string
    doc*:             string
    hash*:            string
    source*:          string
    typed*:           bool
    cycle*:           bool
    savedGlobals*:    RuntimeFrame
    varParams*:       Table[string, ParameterInfo]
    attrParams*:      Table[string, ParameterInfo]
    componentsUsed*:  seq[ComponentInfo]
    beenChecked*:     seq[ConfigState] # Which contexts have we been checked in
    alreadyRunning*:  bool # Breaks cycles at runtime.
    entrypoint*:      Con4mNode

  ParameterInfo* = ref object
    name*:          string
    shortdoc*:      Option[string] # Short description
    doc*:           Option[string] # Long description
    validator*:     Option[CallbackObj]
    default*:       Option[Box]
    defaultType*:   Con4mType
    defaultCb*:     Option[CallbackObj]
    value*:         Option[Box]

let
  # These are just shared instances for types that aren't
  # parameterized, instead of having to instantiate multiple
  # instances.  Should not be exposed to the user.
  stringType*   = Con4mType(kind: TypeString)
  boolType*     = Con4mType(kind: TypeBool)
  intType*      = Con4mType(kind: TypeInt)
  charType*     = Con4mType(kind: TypeChar)
  floatType*    = Con4mType(kind: TypeFloat)
  durationType* = Con4mType(kind: TypeDuration)
  ipAddrType*   = Con4mType(kind: TypeIPAddr)
  cidrType*     = Con4mType(kind: TypeCIDR)
  sizeType*     = Con4mType(kind: TypeSize)
  dateType*     = Con4mType(kind: TypeDate)
  timeType*     = Con4mType(kind: TypeTime)
  dateTimeType* = Con4mType(kind: TypeDateTime)
  bottomType*   = Con4mType(kind: TypeBottom)

proc resolveTypeVars*(t: Con4mType): Con4mType =
  result = t
  if t.kind == TypeTVar:
    if t.cycle: return bottomType
    if t.link.isSome():
      t.cycle = true
      result  = t.link.get().resolveTypeVars()
      t.cycle = false

var tVarNum: int

proc newTypeVar*(constraints: seq[Con4mType] = @[]): Con4mType =
  tVarNum.inc()
  return Con4mType(kind:        TypeTVar,
                   varNum:      tVarNum,
                   link:        none(Con4mType),
                   linksin:     @[],
                   cycle:       false,
                   components:  constraints)
