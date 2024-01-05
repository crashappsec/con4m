## Base for data types used across the project.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import unicode, nimutils, options, ffi
export unicode, nimutils, options, ffi

const
  FRepr*         = 0
  FCastFn*       = 1
  FEq*           = 2
  FLt*           = 3
  FGt*           = 4
  FAdd*          = 5
  FSub*          = 6
  FMul*          = 7
  FFDiv*         = 8
  FIDiv*         = 9
  FMod*          = 10
  FShl*          = 11
  FShr*          = 12
  FBand*         = 13
  FBor*          = 14
  FBxor*         = 15
  FIndex*        = 16
  FDictIndex*    = 17
  FSlice*        = 18
  FAssignIx*     = 19
  FAssignDIx*    = 20
  FassignSlice*  = 21
  FLoadLit*      = 22
  FContainerLit* = 23
  FCopy*         = 24
  FLen*          = 25
  FPlusEqRef*    = 26
  FGetFFIAddr*   = 27 # Of what? Already don't rememebr.
  FInitialize*   = 28 # Stuff we're not using yet.
  FCleanup*      = 29

  # This is not meant for the IR, just for the compiler / interpreter.
  FNewLit*       = 30
  FMax*          = 31



  # These don't generate function calls but get used in a slot for
  # operator numbers, so they are distinct from the above #'s.
  OpLogicOr*     = 33
  OpLogicAnd*    = 34

  # These also do not generate ops directly, they generate a NOT and
  # the corresponding op. They're the same number as their negation,
  #  except with 128 added. Since gte means "not less than" we
  # get OpGte by adding to the value associated w/ 'less-than'.

  OpNeq*         = 130
  OpGte*         = 131
  OpLte*         = 132



type
  SyntaxType* = enum
    STNone      = -1,
    STBase10    = 0,
    STHex       = 1,
    STFloat     = 2,
    STBoolLit   = 3,
    STStrQuotes = 4,
    STChrQuotes = 5,
    STOther     = 6,
    STList      = 7,
    STDict      = 8,
    STTuple     = 9,
    StNull      = 10,
    StMax       = 11

  DTFunc* = range[0 .. FMax]

  DataType* = ref object
    name*:          string
    dtid*:          TypeId
    concrete*:      bool
    isBool*:        bool
    ckind*:         C4TypeKind
    intW*:          int
    signed*:        bool
    signedVariant*: DataType
    fTy*:           bool        # Float type
    strTy*:         bool        # A string type.
    aliases*:       seq[string]
    byValue*:       bool
    ops*:           seq[pointer]

  RefValue*[T] = ref object of RootRef
    refcount*:  int
    staticVal*: bool
    dtInfo*:    DataType
    fullType*:  TypeId
    item*:      T

  StringCursor* = ref object
    runes*: seq[Rune]
    i*:     int

  C4TypeKind* = enum
    C4None, C4TVar, C4List, C4Dict, C4Tuple, C4TypeSpec, C4Ref, C4Maybe,
    C4Func, C4Struct, C4OneOf, C4Primitive

  TypeId*   = uint64

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
    name*: string
    tid*:  TypeId
    impl*: FuncInfo

  Con4mErrPhase* = enum ErrLoad, ErrLex, ErrParse, ErrIrgen
  Con4mSeverity* = enum LlNone, LlInfo, LlWarn, LlErr, LlFatal
  InstantiationInfo* = tuple[filename: string, line: int, column: int]
  Con4mError* = object
    phase*:    Con4mErrPhase
    severity*: Con4mSeverity
    cursor*:   StringCursor
    code*:     string
    modname*:  string
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
    TtHexLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse, TtNil, TTIf,
    TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtReturn,
    TtEnum, TtIdentifier, TtFunc, TtVar, TtGlobal, TtConst, TtOtherLit,
    TtBacktick, TtArrow, TtObject, TtWhile, TtIn, TtBitAnd, TtBitOr, TtBitXor,
    TtShl, TtShr, TtTypeOf, TtValueOf, TtCase, TtSof, TtEof, ErrorTok

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
    err*:          bool

  Con4mNodeKind* = enum
    ## Parse tree nodes types. Really no reason for these to be
    ## exposed either, other than the fact that they're contained in
    ## state objects that are the primary object type exposed to the
    ## user.
    NodeModule, NodeBody, NodeAttrAssign, NodeAttrSetLock, NodeVarAssign,
    NodeSection, NodeIfStmt, NodeElifStmt, NodeElseStmt, NodeTypeOfStmt,
    NodeValueOfStmt, NodeForStmt, NodeWhileStmt, NodeBreakStmt,
    NodeContinueStmt, NodeReturnStmt, NodeStringLit, NodeIntLit, NodeHexLit,
    NodeFloatLit, NodeBoolLit, NodeNot, NodeMember, NodeLiteral, NodeIndex,
    NodeActuals, NodeCall, NodeDictLit, NodeKVPair, NodeListLit, NodeOtherLit,
    NodeTupleLit, NodeCharLit, NodeCallbackLit, NodeOr, NodeAnd, NodeNe,
    NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod,
    NodeMul, NodeDiv, NodeEnumStmt, NodeEnumItem, NodeIdentifier, NodeFuncDef,
    NodeFormalList, NodeVarargsFormal, NodeTypeOneOf, NodeTypeMaybe,
    NodeTypeVar, NodeTypeFunc, NodeTypeTuple, NodeTypeList, NodeTypeDict,
    NodeTypeObj, NodeTypeRef, NodeTypeTypeSpec, NodeTypeBuiltin,
    NodeReturnType, NodeTypeVararg, NodeType, NodeParenExpr, NodeGlobalStmt,
    NodeConstStmt, NodeVarStmt, NodeVarSymInfo, NodeUseStmt, NodeParamBlock,
    NodeExternBlock, NodeExternSig, NodeExternParam, NodeExternLocal,
    NodeExternDll, NodeExternPure, NodeExternHolds, NodeExternAllocs,
    NodeExternReturn, NodeExpression, NodeFormal, NodeLabelStmt, NodeBitOr,
    NodeBitXor, NodeBitAnd, NodeShl, NodeShr, NodeNilLit, NodeCase,
    NodeCaseCondition, NodeRange, NodeDocString

  IrNodeType* = enum
    IrBlock, IrLoop, IrAttrAssign, IrVarAssign, IrConditional,
    IrJump, IrRet, IrLit, IrMember, IrMemberLhs, IrIndex, IrIndexLhs, IrCall,
    IrUse, IrUMinus, IrNot, IrBinary, IrBool, IrLogic, IrLoad, IrLhsLoad,
    IrFold, IrNop, IrSection, IrNil, IrSwitch, IrSwitchBranch, IrRange
    #IrCast

  IrNode* = ref object
    parseNode*: Con4mNode
    tid*:       TypeId
    value*:     Option[pointer]
    parent*:    IrNode
    contents*:  IrContents
    scope*:     Scope

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
      loopVars*:   seq[SymbolInfo]
      whileLoop*:  bool
      condition*:  IrNode    # For loops, this is the range or a container.
                             # For while loops, the loop condition.
      loopBody*:   IrNode
    of IrAttrAssign:
      attrlhs*: IrNode
      attrrhs*: IrNode
      lock*:    bool
    of IrVarAssign:
      varlhs*: IrNode
      varrhs*: IrNode
    of IrConditional:
      predicate*:   IrNode
      trueBranch*:  IrNode
      falseBranch*: IrNode
    of IrSwitch:
      typeCase*:     bool
      switchTarget*: IrNode
      targetSym*:    SymbolInfo
      branches*:     seq[IrNode]
    of IrSwitchBranch:
      conditions*: seq[IrNode]
      action*:     IrNode
      branchSym*:  SymbolInfo
    of IrRange:
      rangeStart*: IrNode
      rangeEnd*:   IrNode
    of IrJump:
      exitLoop*:   bool
      targetNode*: IrNode
    of IrRet:
      retVal*: IrNode
      retSym*: SymbolInfo
    of IrLit:
      items*:  seq[IrNode] # For dicts, [k1, v1, k2, v2]
      byVal*:  bool
      sz*:     int
    of IrMember, IrMemberLhs:
      name*:      string
      subaccess*: IrNode
      attrSym*:   SymbolInfo
    of IrIndex, IrIndexLhs:
      toIx*:       IrNode
      indexStart*: IrNode
      indexEnd*:   IrNode
      toIxSym*:    SymbolInfo
    of IrCall:
      replacement*: bool     # When we, e.g., replace + with __add__()
      module*:      string   # For explicit module specifier
      fname*:       string
      actuals*:     seq[IrNode]
      toCall*:      FuncInfo
    of IrUse:
      targetModule*: string
      targetLoc*:    string
      moduleObj*:    Module
    of IrNot, IrUminus:
      uRhs*: IrNode
    of IrBinary, IrBool, IrLogic:
      bOp*:  string
      opId*: int
      bLhs*: IrNode
      bRhs*: IrNode
    of IrLhsLoad, IrLoad:
      symbol*: SymbolInfo
    of IrFold, IrNop, IrNil:
      discard

  FormalInfo* = ref object
    name*: string
    tid*:  TypeId
    va*:   bool
    sym*:  SymbolInfo
    loc*:  Con4mNode

  ExternFnInfo* = ref object
    externName*:    string
    cArgTypes*:     seq[seq[string]]
    binPtr*:        pointer
    cParamNames*:   seq[string]
    heldParams*:    seq[int]
    allocedParams*: seq[int]
    dll*:           string
    ffiArgTypes*:   array[100, FfiType] # Will store arg info.
    ffiInfo*:       CallerInfo

  FuncInfo* = ref object
    # One module can have multiple instantiations of a function, as
    # long as the type signatures of the parameters are not
    # overlapping.
    #
    # So for functions, we ignore SymbolInfo's tid field and
    # come here.
    externInfo*:     ExternFnInfo
    name*:           string
    rawImpl*:        Con4mNode
    implementation*: IrNode
    tid*:            TypeId
    params*:         seq[FormalInfo]
    paramNames*:     seq[string] # In prep for future keyword args.
    retval*:         FormalInfo
    fnScope*:        Scope
    frozen*:         bool
    defModule*:      Module
    cfg*:            CfgNode
    exitNode*:       CfgNode
    pure*:           bool
    doc1*:           string
    doc2*:           string
    maxOffset*:      int
    internalId*:     int
    codeOffset*:     int # Measured in bytes.

  ParamInfo*  = ref object
    ## Module parameters.
    shortdoc*:      Option[string]
    doc*:           Option[string]
    validator*:     Option[Callback]
    defaultParse*:  Option[Con4mNode]
    defaultIr*:     Option[IrNode]

  SymbolInfo* = ref object
    name*:         string
    isFunc*:       bool
    isAttr*:       bool
    defaultVal*:   Option[pointer] # Only for attributes.
    declaredType*: bool
    immutable*:    bool
    tid*:          TypeId
    uses*:         seq[IrNode]
    defs*:         seq[IrNode]
    fimpls*:       seq[FuncInfo]
    pInfo*:        ParamInfo
    constValue*:   Option[pointer]
    module*:       Module # Ignored for non-func global vars and attrs.
    global*:       bool
    err*:          bool
    formal*:       bool
    declNode*:     Con4mNode
    # heapalloc is only true for global variables and
    # indicates that the offset generated won't be relative to the
    # stack, but from some start offset associated with the module
    # the variable lives in.
    heapAlloc*:    bool  # The below are not used for attributes.
    arena*:        int   # The index into modules; the set of offsets
                         # is calculated per-'memory arena', but
                         # during compilation, we don't care how
                         # that's implemented.
    offset*:       int   # offset within the arena.
    size*:         int   # Size that needs to be allocated. Currently,
                         # this should always be 8 bytes, because we do
                         # not yet support non-64-bit aligned layout, and
                         # we implement all container types as pointers
                         # in this arena, with no sense of where the
                         # backing store is (we might do more optimized
                         # arrays when the size is known later).
    # As we implement SSA and type casing, these are for managing
    # different instances of the same logical symbol. The 'actual'
    # symbol will have actualSym be `nil`, and the count will hand out
    # IDs to sub-symbols.
    #
    # For now, we're just going to use this for typecase, because what
    # I want to do is more of a rework than I've got time for.
    actualSym*:    SymbolInfo
    variantId*:    int

  CfgExitType* = enum
    CFXNormal, CFXCall, CFXUse, CFXBreak, CFXContinue, CFXReturn, CFXStart

  CfgNode* = ref object
    pre*:            seq[CfgNode]
    stmts*:          seq[IrNode]
    post*:           seq[CFGNode]
    defAtStart*:     seq[SymbolInfo]
    defInBlock*:     seq[SymbolInfo]
    errorsInBlock*:  seq[SymbolInfo]
    nextBlock*:      CfgNode
    loopIrNode*:     IrNode
    exitNode*:       CfgNode
    startNode*:      CfgNode # Exit node points to the start.
    splits*:         bool
    exitType*:       CfgExitType
    # Fields to assist in graph repr.
    # The node ID is not set until we go to print.
    # Don't print subgraphs in parallel, as node numberings could
    # go berzerk.
    nodeId*:         int
    irNode*:         IrNode
    makeChildren*:   bool
    label*:          string

  AttrDict*      = Dict[string, pointer]

  # Some specification info is checked during compilation, but most of
  # it is validation done at points where the validation is supposed
  # to be consistent... for instance, after a config executes, and
  # then after subsequent callbacks.
  #
  # Validator gets passed the attr dict, the attribute, the type, the
  # value at the end of execution, and then any parameters (like a set
  # of valid options).
  #
  # Each validation routine should indicate what params it can accept.
  ValidationFn* = proc (i0: AttrDict, i1: string, i2: TypeId,
                        i3: Option[pointer], i4: seq[pointer]): Rope {.cdecl.}

  Validator*     = object
    fn*:          ValidationFn
    params*:      seq[pointer]

  FsKind* = enum
    FsField, FsObjectType, FsSingleton, FsUserDefField, FsObjectInstance,
    FsErrorNoSpec, FsErrorSecUnderField, FsErrorNoSuchsec,
    FsErrorSecNotAllowed, FsErrorFieldNotAllowed

  FieldSpec* = ref object
    name*:                 string
    tid*:                  TypeId
    lockOnWrite*:          bool
    defaultVal*:           Option[pointer]
    addDefaultsBeforeRun*: bool = true
    validators*:           seq[Validator]
    hidden*:               bool
    doc*:                  Rope
    shortdoc*:             Rope
    fieldKind*:            FsKind
    errIx*:                int

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
    table*:     Dict[string, SymbolInfo]
    scopeSize*: int
    attr*:      bool
    numSyms*:   int
    parent*:    Scope

  Module* = ref object
    # This is the compilation context for a single module. It includes
    # fields to support all phases; many fields are unused when the
    # appropriate phamse is done.
    #
    # When we cache info after a successful transformation of the thing
    # into what is essentially our byte code, we copy over only
    # what we need to keep around into the Module object.
    url*:         string
    where*:       string
    modname*:     string
    key*:         string
    ext*:         string
    errors*:      seq[Con4mError]
    s*:           StringCursor      # Source
    tokens*:      seq[Con4mToken]
    root*:        Con4mNode         # Parse tree root
    ir*:          IrNode
    cfg*:         CfgNode
    exitNode*:    CfgNode
    compileCtx*:  CompileCtx        # An unfortunate temporary during IR pass.
    imports*:     seq[Module]

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

    # First two fields are refs to the one in the compile context.
    globalScope*: Scope
    attrSpec*:    ValidationSpec

    moduleScope*:    Scope
    funcScope*:      Scope
    usedAttrs*:      Scope
    fatalErrors*:    bool
    processed*:      bool

    # Used in lexical analysis only.
    # This should move to a tmp object.
    nextId*:    int = 1
    lineNo*:    int = 1
    lineStart*: int = 0

    # Used in parsing only.
    # This should move to a tmp object.
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
    # This should move to a tmp object.
    pt*:             Con4mNode
    definingFn*:     FuncInfo
    current*:        IrNode
    blockScopes*:    seq[Scope]   # Stack of loop iteration vars.
    lhsContext*:     bool         # To determine when this might be a def.
    attrContext*:    bool         # True when we are def processing an attr.
    ambigAssign*:    bool
    secDefContext*:  bool
    curSym*:         SymbolInfo
    usedModules*:    seq[(string, string)]
    funcsToResolve*: seq[(IrNode, TypeId, seq[FuncInfo], seq[FuncInfo])]
    labelNode*:      Con4mNode
    curSecPrefix*:   string
    curSecSpec*:     SectionSpec
    didFoldingPass*: bool # Might not be using this anymore.
    maxOffset*:      int


  CompileCtx* = ref object
    modulePath*:  seq[string] = @[".", "https://chalkdust.io/"]
    defaultExt*:  string      = ".c4m"
    attrSpec*:    ValidationSpec
    errors*:      seq[Con4mError]
    globalScope*: Scope
    usedAttrs*:   Scope
    entrypoint*:  Module
    fatal*:       bool
    topExitNode*: CfgNode # Used when building CFG
    modules*:     Dict[string, Module]

proc memcmp*(a, b: pointer, size: csize_t): cint {.importc,
                                                   header: "<string.h>",
                                                   noSideEffect.}
