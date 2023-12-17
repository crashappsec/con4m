## Base for data types used across the project.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import unicode, nimutils, mixed, options
export unicode, nimutils, mixed, options

const
  noRepr*       = 0
  scalarOk*     = 1
  hexOk*        = 2
  floatOk*      = 4
  boolOk*       = 8
  strQuotesOk*  = 16
  charQuotesOk* = 32
  euroQuotesOk* = 64
  stdBoolKind*  = (boolOk)
  stdIntKind*   = (scalarOk or hexOk)
  stdFloatKind* = (scalarOk or floatOk)
  stdStrKind*   = strQuotesOk
  stdChrKind*   = (scalarOk or hexOk or charQuotesOk)
  stdOtherKind* = (strQuotesOk or euroQuotesOk)

type
  StringCursor* = ref object
    runes*: seq[Rune]
    i*:     int

  SyntaxType* = enum
    STBase10    = scalarOk,
    STHex       = hexOk,
    STFloat     = floatOk,
    STBoolLit   = boolOk,
    STStrQuotes = strQuotesOk,
    STChrQuotes = charQuotesOk,
    STOther     = euroQuotesOk,
    STList      = 128,
    STDict      = 256,
    STTuple     = 512

  C4TypeKind* = enum
    C4TVar, C4List, C4Dict, C4Tuple, C4TypeSpec, C4Ref, C4Maybe,
    C4Func, C4Struct, C4OneOf, C4Primitive

  TypeRef*  = ref object
    typeid*:   TypeId
    isLocked*: bool
    items*:    seq[TypeId]
    case kind*: C4TypeKind
    of C4TVar:
      tvarId*:    TypeId
      localName*: Option[string]
    of C4Func:
      va*: bool
    of C4Struct:
      name*:  string
      props*: Dict[string, TypeId]
    else:
      discard

  Callback* = object
    # Right now, this doesn't even stash a pointer; we could cache
    # this, but we accept callbacks that aren't provided, so we
    # currently just defer until runtime to look up the function
    # anyway.  Also helps make it easy to handle the case where a
    # function's entry is dynamically replaced via a stack.
    #
    # This probably should get moved to its own module.
    # It's a value for a ref to a function.
    name*: string
    tid*:  TypeId

  TypeConstructor* = proc (i0: string, i1: var Mixed, i2: SyntaxType):
                         string {.cdecl.}
  # i0 -> the starting type; modifiable.
  # i1 -> the literal to update.
  ContainerConstructor* = proc (i0: var TypeId, i1: var Mixed): string {.cdecl.}
  CharInitFn*           = proc (i0: uint, i1: var Mixed): string {.cdecl.}
  ReprFn*               = proc (i0: TypeId, i1: Mixed): string {.cdecl.}
  BoolCastFn*           = proc (i0: Mixed): bool {.cdecl.}
  U128CastFn*           = proc (i0: Mixed): uint128 {.cdecl.}
  I128CastFn*           = proc (i0: Mixed): int128 {.cdecl.}
  EqFn*                 = proc (i0: CBox, i2: Cbox): bool {.cdecl.}

  TypeId*               = uint64
  TypeInfo* = ref object
    name*:        string
    kind*:        uint
    litmods*:     seq[string]
    typeId*:      TypeId
    intBits*:     int
    signed*:      bool
    signVariant*: TypeId
    isLocked*:    bool
    repr*:        ReprFn
    fromRawLit*:  TypeConstructor
    fromCharLit*: CharInitFn
    castToBool*:  BoolCastFn
    castToU128*:  U128CastFn
    castToI128*:  I128CastFn
    eqFn*:        EqFn

  CBox* = object
    v*: Mixed
    t*: TypeId

  Con4mErrPhase* = enum ErrLex, ErrParse, ErrIrgen
  Con4mSeverity* = enum LlNone, LlInfo, LlWarn, LlErr
  InstantiationInfo* = tuple[filename: string, line: int, column: int]
  Con4mError* = object
    phase*:    Con4mErrPhase
    severity*: Con4mSeverity
    cursor*:   StringCursor
    code*:     string
    module*:   string
    line*:     int
    offset*:   int
    extra*:    seq[string]
    when not defined(release):
      trace*:  string
      ii*:     InstantiationInfo

  Con4mException* = ref object of ValueError
    errors*: seq[Con4mError]

  Con4mLongJmp* = ref object of ValueError

  ## Enumeration of all possible lexical tokens. Should not be exposed
  ## outside the package.
  Con4mTokenKind* = enum
    TtWhiteSpace, TtSemi, TtNewLine, TtLineComment, TtLockAttr,
    TtPlus, TtMinus, TtMul, TtLongComment, TtDiv, TTMod, TtLte, TtLt, TtGte,
    TtGt, TtNeq, TtNot, TtLocalAssign, TtColon, TtAttrAssign, TtCmp, TtComma,
    TtPeriod, TtLBrace, TtRBrace, TtRBraceMod, TtLBracket, TtRBracket,
    TtRBracketMod, TtLParen, TtRParen, TtRParenMod, TtAnd, TtOr, TtIntLit,
    TtHexLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse, TTIf,
    TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtReturn,
    TtEnum, TtIdentifier, TtFunc, TtVar, TtGlobal, TtOtherLit, TtBacktick,
    TtArrow, TtObject, TtWhile, TtIn, TtBitAnd, TtBitOr, TtBitXor, TtShl,
    TtShr, TtSof, TtEof, ErrorTok

  Con4mToken* = ref object
    ## Lexical tokens. Should not be exposed outside the package.
    case kind*:   Con4mTokenKind
    of TtStringLit:
      unescaped*: string
    of TtCharLit:
      codepoint*: uint
    else:  nil
    cursor*:      StringCursor
    id*:          int
    startPos*:    int
    endPos*:      int
    lineNo*:      int
    lineOffset*:  int
    litType*:     string
    adjustment*:  int

  Con4mNode* = ref object
    ## The actual parse tree node type.  Should generally not be exposed.
    id*:           int
    prevTokenIx*:  int
    depth*:        int
    kind*:         Con4mNodeKind
    token*:        Con4mToken
    children*:     seq[Con4mNode]
    parent*:       Con4mNode
    commentLocs*:  seq[int]

  Con4mNodeKind* = enum
    ## Parse tree nodes types. Really no reason for these to be
    ## exposed either, other than the fact that they're contained in
    ## state objects that are the primary object type exposed to the
    ## user.
    NodeModule, NodeBody, NodeAttrAssign, NodeAttrSetLock, NodeVarAssign,
    NodeUnpack, NodeSection, NodeIfStmt, NodeElifStmt, NodeElseStmt,
    NodeForStmt, NodeWhileStmt, NodeBreakStmt,  NodeContinueStmt,
    NodeReturnStmt, NodeStringLit, NodeIntLit, NodeHexLit, NodeFloatLit,
    NodeBoolLit, NodeNot, NodeMember, NodeLiteral, NodeIndex, NodeActuals,
    NodeCall, NodeDictLit, NodeKVPair, NodeListLit, NodeOtherLit, NodeTupleLit,
    NodeCharLit, NodeCallbackLit, NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte,
    NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv,
    NodeEnumStmt, NodeEnumItem, NodeIdentifier, NodeFuncDef, NodeFormalList,
    NodeVarargsFormal, NodeTypeOneOf, NodeTypeMaybe, NodeTypeVar, NodeTypeFunc,
    NodeTypeTuple, NodeTypeList, NodeTypeDict, NodeTypeObj, NodeTypeRef,
    NodeTypeTypeSpec, NodeTypeBuiltin, NodeReturnType, NodeTypeVararg,
    NodeType, NodeParenExpr, NodeGlobalStmt, NodeVarStmt, NodeVarSymInfo,
    NodeUseStmt, NodeParamBlock, NodeExpression, NodeFormal, NodeLabelStmt,
    NodeBitOr, NodeBitXor, NodeBitAnd, NodeShl, NodeShr

  IrNodeType* = enum
    IrBlock, IrLoop, IrAttrAssign, IrVarAssign, IrSectionScope, IrConditional,
    IrJump, IrRet, IrLit, IrMember, IrIndex, IrCall, IrUse, IrUMinus, IrNot,
    IrBinary, IrBool, IrLogic, IrLoad, IrLhsLoad, IrFold, IrNop, IrSection
    #IrCast

  IrNode* = ref object
    parseNode*: Con4mNode
    tid*:       TypeId
    value*:     Option[Mixed]
    parent*:    IrNode
    contents*:  IrContents

  IrContents* = ref object
    case kind*: IrNodeType
    of IrBlock:
      stmts*: seq[IrNode]
    of IrSection:
      prefix*:   string
      sectName*: string
      instance*: string
      blk*:      IrNode
      spec*:     SectionSpec
    of IrLoop:
      label*:      Con4mNode # Any type of loop.
      keyVar*:     string    # Both types of for loops.
      valVar*:     string    # for k, v in dict
      startIx*:    IrNode    # for x from 0 to 10
      endIx*:      IrNode    # for x from 0 to 10
      condition*:  IrNode    # While loop condition or object to iterate over.
      loopBody*:   IrNode
      scope*:      Scope
    of IrAttrAssign:
      attrlhs*: IrNode
      attrrhs*: IrNode
      lock*:    bool
    of IrVarAssign:
      varlhs*: seq[IrNode]
      varrhs*: IrNode
    of IrSectionScope:
      secType*:  string
      secName*:  string
      secBody*:  IrNode
      prevPath*: string
    of IrConditional:
      predicate*:   IrNode
      trueBranch*:  IrNode
      falseBranch*: IrNode
    of IrJump:
      exitLoop*:   bool
      targetNode*: IrNode
    of IrRet:
      retVal*: IrNode
    of IrLit:
      syntax*: SyntaxType
      litmod*: string
      items*:  seq[IrNode] # For dicts, [k1, v1, k2, v2]
    of IrMember:
      name*: string
    of IrIndex:
      indexStart*: IrNode
      indexEnd*:   IrNode
    of IrCall:
      binop*:   bool     # When we, e.g., replace + with __add__()
      module*:  string   # For explicit module specifier
      fname*:   string
      actuals*: seq[IrNode]
    of IrUse:
      targetModule*: string
      targetLoc*:    string
    of IrNot, IrUminus:
      uRhs*: IrNode
    of IrBinary, IrBool, IrLogic:
      bOp*:  string
      bLhs*: IrNode
      bRhs*: IrNode
    of IrLhsLoad, IrLoad:
      symbol*: SymbolInfo
    of IrFold, IrNop:
      discard

  FormalInfo* = ref object
    name*: string
    tid*:  TypeId
    va*:   bool

  FuncInfo* = ref object
    # One module can have multiple instantiations of a function, as
    # long as the type signatures of the parameters are not
    # overlapping.
    #
    # So for functions, we ignore SymbolInfo's tid field and
    # come here.
    name*:           string
    rawImpl*:        Con4mNode
    implementation*: IrNode
    tid*:            TypeId
    params*:         seq[FormalInfo]
    retval*:         FormalInfo
    fnScope*:        Scope

  ParamInfo*  = ref object
    shortdoc*:      Option[string]
    doc*:           Option[string]
    validator*:     Option[Callback]
    defaultParse*:  Option[Con4mNode]
    defaultIr*:     Option[IrNode]
    startValue*:    Option[Mixed]

  SymbolInfo* = ref object
    name*:         string
    isFunc*:       bool
    tid*:          TypeId
    uses*:         seq[IrNode]
    defs*:         seq[IrNode]
    declaredType*: bool
    immutable*:    bool
    fimpls*:       seq[FuncInfo]
    pInfo*:        ParamInfo
    constValue*:   Option[CBox]

  AttrDict*      = Dict[string, CBox]

  # Some specification info is checked during compilation, but most of
  # it is validation done at points where the validation is supposed
  # to be consistent... for instance, after a config executes, and
  # then after subsequent callbacks.
  #
  # Validator gets passed the attr dict, the attribute, the value at
  # the end of execution, and then any parameters (like a set of valid
  # options).
  #
  # Each validation routine should indicate what params it can accept.
  ValidationFn* = proc (i0: AttrDict, i1: string, i2: Option[CBox],
                        i3: seq[CBox]): Rope {.cdecl.}

  Validator*     = object
    fn*:          ValidationFn
    params*:      seq[CBox]

  FieldSpec* = ref object
    name*:                 string
    tid*:                  TypeId
    section*:              bool
    lockOnWrite*:          bool
    defaultVal*:           Option[CBox]
    addDefaultsBeforeRun*: bool = true
    validators*:           seq[Validator]
    hidden*:               bool
    doc*:                  Rope
    shortdoc*:             Rope

  SectionSpec* = ref object
    ## This specification is only applied to attributes.  It is used
    ## lightly at compile time, if present, to statically detect any
    ## obvious mistakes. It's also kept around for runtime, and then
    ## automatically run at completion for any fields changed.

    name*:             string
    singleton*:        bool
    fields*:           Dict[string, FieldSpec]
    userDefOk*:        bool
    validators*:       seq[Validator]
    hidden*:           bool
    doc*:              Rope
    shortdoc*:         Rope
    allowedSections*:  seq[string]

  ValidationSpec* = ref object
    rootSpec*: SectionSpec
    secSpecs*: Dict[string, SectionSpec]

  Scope* = ref object
    table*:  Dict[string, SymbolInfo]
    attr*:   bool

  CompileCtx* = object
    # This is the compilation context for a single module. It includes
    # fields to support all phases; many fields are unused when the
    # appropriate phamse is done.
    #
    # When we cache info after a successful transformation of the thing
    # into what is essentially our byte code, we copy over only
    # what we need to keep around into the Module object.
    errors*:      seq[Con4mError]
    module*:      string
    s*:           StringCursor      # Sourcee
    tokens*:      seq[Con4mToken]
    root*:        Con4mNode         # Parse tree root
    irRoot*:      IrNode

    # The next set of variables don't show up till first IR pass, but
    # have a long life.
    #
    # globalScope should consist of:
    # 1) Stuff explicitly declared global
    # 2) Functions that are called but not defined in the local module.
    # 3) Any variables that are used without a def (done at the end
    #    of the IR generation).
    #
    # moduleScope and funcScope should be somewhat obvious. usedAttrs
    # keeps track of the attributes that a module explicitly accesses
    # (so far as they can be determined statically; library calls may
    # allow dynamic access). These are type checked against other
    # uses, and against the type in the validation spec, if it's
    # available (stashed in attrSpec).

    globalScope*: Scope
    moduleScope*: Scope
    funcScope*:   Scope
    usedAttrs*:   Scope
    attrSpec*:    ValidationSpec

    # Used in lexical analysis only.
    nextId*:    int = 1
    lineNo*:    int = 1
    lineStart*: int = 0

    # Used in parsing only.
    curTokIx*:       int
    prevTokIx*:      int
    curNodeId*:      int
    nesting*:        int
    skipOneNewLine*: bool
    inFunc*:         bool
    literalDepth*:   int
    loopDepth*:      int
    cachedComments*: seq[int]
    prevNode*:       Con4mNode

    # Used for IR generation only.
    pt*:           Con4mNode
    current*:      IrNode
    blockScopes*:  seq[Scope]   # Stack of loop iteration vars.
    lhsContext*:   bool         # To determine when this might be a def.
    attrContext*:  bool         # True when we are def processing an attr.
    curSym*:       SymbolInfo
    usedModules*:  seq[(string, string)]
    funcRefs*:     seq[(string, TypeId, IrNode)]
    labelNode*:    Con4mNode
    curSecPrefix*: string
    curSecSpec*:   SectionSpec

var
  basicTypes*: seq[TypeInfo]
  tiMap*:      Dict[TypeId, TypeInfo]
  nameMap*:    Dict[string, TypeInfo]

proc lockType*(tid: TypeId) =
  tiMap[tid].isLocked = true

proc unlockType*(tid: TypeId) =
  tiMap[tid].isLocked = false

proc addBasicType*(name:        string,
                   kind:        uint,
                   litmods:     seq[string] = @[],
                   intBits:     int = 0,
                   signed:      bool = false,
                   repr:        ReprFn,
                   castToBool:  BoolCastFn = nil,
                   castToU128:  U128CastFn = nil,
                   castToI128:  I128CastFn = nil,
                   fromRawLit:  TypeConstructor = nil,
                   fromCharLit: CharInitFn = nil,
                   eqFn:        EqFn = nil): TypeId  =

  var tInfo = TypeInfo(name: name, repr: repr, kind: kind, litMods: litMods,
                       intBits: intBits, signed: signed, castToBool: castToBool,
                       castToU128: castToU128, castToI128: castToI128,
                       fromRawLit: fromRawLit, fromCharLit: fromCharLit,
                       eqFn: eqFn)

  result        = TypeId(basicTypes.len())
  tinfo.typeId  = result
  tiMap[result] = tInfo
  nameMap[name] = tInfo

  basicTypes.add(tInfo)

proc getBuiltinTypeIdFromName*(name: string, err: var string): TypeId =
  let opt = nameMap.lookup(name)
  if opt.isNone():
    err = "Unknown built-in type: '" & name & "'"
  else:
    result = opt.get().typeId

var
  listMods: Dict[string, ContainerConstructor]
  dictMods: Dict[string, ContainerConstructor]
  tupMods:  Dict[string, ContainerConstructor]

listMods.initDict()
dictMods.initDict()
tupMods.initDict()

proc registerListMod*(name: string, cons: ContainerConstructor) =
  listMods[name] = cons

proc registerDictMod*(name: string, cons: ContainerConstructor) =
  dictMods[name] = cons

proc registerTupMod*(name: string, cons: ContainerConstructor) =
  tupMods[name] = cons

proc listModExists*(name: string): bool =
  return listMods.lookup(name).isSome()

proc dictModExists*(name: string): bool =
  return dictMods.lookup(name).isSome()

proc tupModExists*(name: string): bool =
  return tupMods.lookup(name).isSome()

proc applyListMod*(name: string, tinfo: var TypeId, val: var Mixed): string =
  return listMods[name](tinfo, val)

proc applyDictMod*(name: string, tinfo: var TypeId, val: var Mixed): string =
  return dictMods[name](tinfo, val)

proc applyTupMod*(name: string, tinfo: var TypeId, val: var Mixed): string =
  return tupMods[name](tinfo, val)

proc basicRepr(tid: TypeId, m: Mixed): string {.cdecl.} =
  return "error: type has no representation"

let
  TBottom* = addBasicType(name = "none (type error)",
                          repr = basicRepr,
                          kind = noRepr)
  TVoid*   = addBasicType(name = "void",
                          repr = basicRepr,
                          kind = noRepr)

proc parseLiteral*(typeId: int, raw: string, err: var string,
                  st: SyntaxType): Mixed =
  ## Returns the parsed literal wrapped in an `Mixed`.
  ## Assumes you know your typeId definitively by now.
  ##
  ## If `err` is not empty, then ignore the return value.
  var ti = basicTypes[typeId]

  err = ti.fromRawLit(raw, result, st)

proc initializeCharLiteral*(typeId: int, cp: uint, err: var string): Mixed =
  ## Returns the initialized, error-checked literal wrapped in an `Mixed`.
  ## Assumes you know your typeId definitively by now.
  ##
  ## If `err` is not empty, then ignore the return value.

  var ti = basicTypes[typeId]

  err = ti.fromCharLit(cp, result)

var
  biTypeNames: seq[string]
  allTypeIds:  seq[string]

proc typeNameFromId*(id: TypeId): string =
  return biTypeNames[int(id)]

proc idFromTypeName*(n: string): TypeId =
  return TypeId(biTypeNames.find(n))

proc getAllBuiltinTypeNames*(): seq[string] =
  if biTypeNames.len() > 0:
    return biTypeNames

  for item in basicTypes:
    biTypeNames.add(item.name)

  allTypeIds = biTypeNames
  allTypeIds &= ["list", "dict", "tuple", "struct", "ref", "set",
                 "maybe", "oneof", "typespec"]

  return biTypeNames

proc getAllTypeIdentifiers*(): seq[string] =
  if allTypeIds.len() == 0:
    discard getAllBuiltinTypeNames()

  return allTypeIds

proc numBuiltinTypes*(): int =
  return biTypeNames.len()

proc getTypeInfoObject*(id: TypeId): TypeInfo =
  return basicTypes[int(id)]

proc normalSizeIntToBool*(n: Mixed): bool {.cdecl.} =
  return toVal[uint64](n) != 0

proc normalSizeIntToU128*(n: Mixed): uint128 {.cdecl.} =
  return iToU128(toVal[uint64](n))

proc normalSizeIntToI128*(n: Mixed): int128 {.cdecl.} =
  return iToI128(toVal[int64](n))

proc basicEq*(a, b: CBox): bool {.cdecl.} =
  # Only meant to work for by-value storage.
  return toVal[int64](a.v) == toVal[int64](b.v)

proc pointerEq*(a, b: CBox): bool {.cdecl.} =
  # Should work with anything stored by reference in Nim.
  # So NOT seqs, etc.
  return toVal[pointer](a.v) == toVal[pointer](b.v)

proc memcmp*(a, b: pointer, size: csize_t): cint {.importc,
                                                   header: "<string.h>",
                                                   noSideEffect.}
