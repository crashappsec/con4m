## Base for data types used across the project.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import unicode, nimutils, options, ffi
export unicode, nimutils, options, ffi

# These consts are for calls in our internal Type API. It's meant to be
# used both during compilation and interpretation.
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
  FStaticRepr*   = 31 # Only used for by-ref types.
  FMax*          = 32

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

  MAX_CALL_DEPTH* = 200
  STACK_SIZE*     = 1 shl 20
  USE_TRACE*      = false # Also requires one of the below to be true
  TRACE_INSTR*    = true
  TRACE_STACK*    = true
  TRACE_SCOPE*    = true
  RTAsMixed*      = -2
  RTAsPtr*        = -1

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

  Con4mErrPhase* = enum
    ErrLoad, ErrLex, ErrParse, ErrIrgen, ErrCodeGen, ErrRuntime
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
    detail*:   Rope
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
    TtGt, TtNeq, TtNot, TtColon, TtAssign, TtCmp, TtComma,
    TtPeriod, TtLBrace, TtRBrace, TtRBraceMod, TtLBracket, TtRBracket,
    TtRBracketMod, TtLParen, TtRParen, TtRParenMod, TtAnd, TtOr, TtIntLit,
    TtHexLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse, TtNil, TTIf,
    TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtReturn,
    TtEnum, TtIdentifier, TtFunc, TtVar, TtGlobal, TtConst, TtOtherLit,
    TtBacktick, TtArrow, TtObject, TtWhile, TtIn, TtBitAnd, TtBitOr, TtBitXor,
    TtShl, TtShr, TtTypeOf, TtValueOf, TtCase, TtPlusEq, TTMinusEq, TtMulEq,
    TtDivEq, TtModEq, TtBitAndEq, TtBitOrEq, TtBitXOrEq, TtShlEq, TtShrEq,
    TtSof, TtEof, ErrorTok

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
    NodeModule, NodeBody, NodeAssign, NodeAttrSetLock, NodeCast,
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
    NodeCaseCondition, NodeRange, NodeDocString, NodeAssert

  IrNodeType* = enum
    IrBlock, IrLoop, IrAssign, IrConditional, IrCast,
    IrJump, IrRet, IrLit, IrMember, IrMemberLhs, IrIndex, IrIndexLhs, IrCall,
    IrUse, IrUMinus, IrNot, IrBinary, IrBool, IrLogic, IrLoad, IrLhsLoad,
    IrFold, IrNop, IrSection, IrNil, IrSwitch, IrSwitchBranch, IrRange,
    IrAssert

  IrNode* = ref object
    parseNode*: Con4mNode
    tid*:       TypeId
    value*:     pointer
    haveVal*:   bool
    parent*:    IrNode
    contents*:  IrContents
    scope*:     Scope
    lock*:      bool


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
    of IrAssign:
      assignlhs*:   IrNode
      assignrhs*:   IrNode
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
    of IrAssert:
      assertion*: IrNode
    of IrCast:
      srcData*: IrNode
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
    dlls*:          seq[string]
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
    externName*:     string   # Somehow the string inside eI is getting lost?
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
    objInfo*:        ZFnInfo

  ParamInfo*  = ref object
    ## Module parameters.
    shortdoc*:      Option[string]
    doc*:           Option[string]
    validator*:     Option[Callback]
    defaultParse*:  Option[Con4mNode]
    defaultIr*:     Option[IrNode]

  SymbolInfo* = ref object
    name*:         string
    tid*:          TypeId
    module*:       Module # Ignored for non-func global vars and attrs.
    uses*:         seq[IrNode]
    defs*:         seq[IrNode]
    fimpls*:       seq[FuncInfo]
    pInfo*:        ParamInfo
    defaultVal*:   pointer
    constValue*:   pointer
    isFunc*:       bool
    inFunc*:       bool
    isAttr*:       bool
    declaredType*: bool
    immutable*:    bool
    haveDefault*:  bool
    haveConst*:    bool
    global*:       bool
    err*:          bool
    formal*:       bool
    declNode*:     Con4mNode
    # heapalloc is only true for global variables and
    # indicates that the offset generated won't be relative to the
    # stack, but from some start offset associated with the module
    # the variable lives in.
    heapAlloc*:    bool  # The below are not used for attributes.
    moduleId*:     int   # The index into modules; the set of offsets
                         # is calculated per-'memory moduleId', but
                         # during compilation, we don't care how
                         # that's implemented.
    offset*:       int   # offset within the moduleId.
    size*:         int   # Size that needs to be allocated. Currently,
                         # this should always be 8 bytes, because we do
                         # not yet support non-64-bit aligned layout, and
                         # we implement all container types as pointers
                         # in this moduleId, with no sense of where the
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

  SecValidator* =    proc(i0: RuntimeState, i1: string, i2: seq[string],
                          i3: seq[pointer]): Rope {.cdecl.}
  FieldValidator* =  proc(i0: RuntimeState, i1: string, i2: pointer,
                          i3: TypeId, i4:seq[pointer]): Rope {.cdecl.}

  Validator*     = object
    fn*:          pointer
    params*:      seq[pointer]

  FsKind* = enum
    FsField, FsObjectType, FsSingleton, FsUserDefField, FsObjectInstance,
    FsErrorNoSpec, FsErrorSecUnderField, FsErrorNoSuchsec,
    FsErrorSecNotAllowed, FsErrorFieldNotAllowed

  FieldSpec* = ref object
    name*:                 string
    tid*:                  TypeId
    lockOnWrite*:          bool   # Enforced at runtime.
    defaultVal*:           pointer
    haveDefault*:          bool
    validators*:           seq[Validator] # Applied on request.
    hidden*:               bool
    required*:             bool
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
    minAllowed*:       int # Not implemented yet; currently always 0
    maxAllowed*:       int # Right now, this is just high() if it's not a
                           # singleton.
    fields*:           Dict[string, FieldSpec]
    userDefOk*:        bool
    validators*:       seq[Validator]
    hidden*:           bool
    doc*:              Rope
    shortdoc*:         Rope
    allowedSections*:  seq[string]
    cycle*:            bool # Private, used to avoid populating cyclic defs.

  ValidationSpec* = ref object
    rootSpec*: SectionSpec
    secSpecs*: Dict[string, SectionSpec]
    locked*:   bool

  Scope* = ref object
    table*:       Dict[string, SymbolInfo]
    scopeSize*:   int
    attr*:        bool
    fnScope*:     bool
    numSyms*:     int
    parent*:      Scope
    childScopes*: seq[Scope]
    sized*:       bool

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
    funcsToResolve*: seq[(IrNode, TypeId, string)]
    labelNode*:      Con4mNode
    curSecPrefix*:   string
    curSecSpec*:     SectionSpec
    didFoldingPass*: bool # Might not be using this anymore.
    maxOffset*:      int

    # Used in code generation.
    processed*:     bool
    loopLocs*:      seq[(IrNode, int)]
    backpatchLocs*: seq[(IrNode, int)]
    objInfo*:       ZmoduleInfo

  CompileCtx* = ref object
    modules*:     Dict[string, Module]
    defaultExt*:  string      = ".c4m"
    attrSpec*:    ValidationSpec
    globalScope*: Scope
    usedAttrs*:   Scope
    entrypoint*:  Module
    fatal*:       bool
    topExitNode*: CfgNode # Used when building CFG
    modulePath*:  seq[string] = @[".", "https://chalkdust.io/"]
    sysdir*:      string
    errors*:      seq[Con4mError]

  # The remaining data structures are used in code generation and / or
  # runtime. Any type here with a Z in front is intended to get
  # marshaled into the object file.

  ZOp* {.size: 1.} = enum
     # The first pushes are all pushes from variables to the stack.
     # So if we were explicitly going through registers, this would be
     # load + push.
     #
     # The 64-bit argument to the instruction will contain the address
     # from which to load.
     #
     # These first item indicates that the operand is definitely typed
     # to one of the integer types, and is being held in a 64-bit
     # bounary.
     #
     # This way, when we're building a runtime, we can minimize work
     # in figuring out how to implement ZTCalls (internal calls) if
     # we don't
     ZPushVal       = 0x10, # Push a primitive value that is
                            # passed by value (e.g., a size object)

     # Push a pointer relative to the offset of the scope start.  This
     # might be a data object pointer, or it might be an attribute
     # name, as we call this before assigning or loading attributes.
     ZPushPtr       = 0x11,

     # Push a pointer relative to our static data.
     ZPushStaticPtr = 0x12,

     # Push an immediate value onto the stack (the value coming from
     # the instruction itself).
     ZPushImm       = 0x13,

     # Pushes the type of a a heap object onto the stack
     # for purposes of runtime type comparisons.
     ZPushSType     = 0x14,

     # Push the address of a variable that lives either on the stack
     # or the heap (not an attribute).  The intent here is not to
     # access the contents, but to replace the object.  So if we do:
     # `x = y`, where `y` is an `int`, this loads the address of `x`,
     # not the value of `x`.
     #
     # For *object* types, there's no difference here, until we add
     # references to the language.
     ZPushAddr      = 0x15,

     # A lot of the below operations implicitly pop args from the
     # stack once they consume them. This allows us to keep an arg
     # around. Particularly, we use this for switch-style statements,
     # to keep the value we're testing against around (since our
     # current VM model is stack-only).
     ZDupTop        = 0x16,

     # When this instruction is called, the stack is popped, the value
     # is checked for a C-style string; that attribute is
     # loaded. Right now, there's no dynamic checking on this; the
     # null / 0 value gets loaded if there's a lookup failure.
     ZLoadFromAttr  = 0x17,

     # Swap the top two stack items.
     ZSwap          = 0x18,

     # Pop a value from the stack, discarding it.
     ZPop           = 0x20,

     # Store a copy of the top of the stack into a memory address
     # from a stack offset. Does not pop.
     ZStoreTop      = 0x21,

     # Store an immediate value (encoded in the instruction) into the
     # memory address, ignoring the stack.
     ZStoreImm      = 0x22,

     # Tests the top of the stack... if it's a zero value, it jumps to
     # the indicated instruction. The jump argument is a relative
     # offset, and is measured in BYTES, which obviously will
     # be a multiple of the opcode size.
     #
     # Note that, when we're backpatching jumps, we hold onto an
     # absolute index into the opcode table, not a byte index. We
     # don't convert to a relative offset until storing the info.
     #
     # No matter the value, the two conditional jumps pop the top of
     # the stack.
     ZJz            = 0x30,
     ZJnz           = 0x31,

     # No popping for an unconditional jump.
     ZJ             = 0x32,

     # This calls our internal data type API. If there's a return value,
     # it's left on the stack.
     ZTCall         = 0x33,

     # This calls a function, which can include the module's top-level
     # code, with the opcode encoding the moduleId (module id) and the
     # byte offset into that moduleId's code.
     #
     # It assumes arguments are pushed, but then:
     # 1. Pushes the return address
     # 2. Saves the base pointer to the stack.
     # 3. Sets the value of the base pointer to point to the saved
     #    base pointer. That means the first argument will live at -16.
     # 4. It jumps to the entry point.
     Z0Call         = 0x34,

     # This runs a FFI call.
     ZFFICall       = 0x35,

     # This maps to a 'use' statement; it calls the initialization code
     # of a module.
     ZCallModule    = 0x36,

     # Logical not. Currently there's no bitwise not.
     ZNot           = 0x50,

     # Unmarshal stored data from the heap, instantiating an object.
     # the data in the heap should be considered read-only.
     ZSObjNew       = 0x60,

     # The stack contains a heap pointer and a value to assign at
     # that address. This pops its arguments.
     ZAssignToLoc   = 0x70,

     # Whereas with `ZAssignToLoc`, the stack contains a pointer to a
     # variable, here it contains a pointer to a heap-alloc'd string
     # that is the name of the attribute.
     #
     # If there is an argument provided, will lock the attribute,
     # preventing additional writes.
     ZAssignAttr    = 0x71,

     # This just needs to shrink the stack back to the base pointer,
     # restore the old base pointer, and jump to the saved return
     # address (popping it as well).
     ZRet           = 0x80,

     # This is a return, unless the module is the entry point, in
     # which case it is a halt.
     ZModuleRet     = 0x81,
     ZHalt          = 0x82,

     # SetRet should be called by any function before exiting, where
     # that function is non-void. It moves the value at fp - 2
     # into the return register.
     ZSetRes        = 0x90,

     # This pushes whatever is in the return register onto the stack;
     # It does NOT clear the register.
     ZPushRes       = 0x91,

     # This jumps the stack pointer to add or shrink by the size of
     # the variables in the current scope.  The scope size to add or
     # sub is in the `immediate` field and must be 64-bit aligned.
     ZMoveSP        = 0x92,

     # Halts the program if the stack top is false. Pops the stack.
     ZAssert        = 0xa0,

     # Sets the 'lock-on-write' bit for an attribute whose name is at
     # the top of the stack. The only way to lock immediately is
     # through a ZAssignAttr.
     ZLockOnWrite   = 0xb0,

     # A no-op. These double as labels for disassembly too.
     ZNop           = 0xff

  ZProgParam* = object
    # `memloc` specifies where the parameter should be stored. This is
    # not a raw memory pointer. It's either an offset from a module's
    # moduleId, or a pointer to the global static data, where the
    # attribute name is kept.
    #
    # Which of the above applies is detemrined by the `moduleId` field;
    # the modules are all given positive indexes. If it's zero, then
    # it's an attribute.
    memloc*:    pointer
    paramName*: pointer # The source name of the variable or attribute.
    typePtr*:   pointer # The marshal'd type object.
    shortDoc*:  pointer # An offset to the short description start.
    longDoc*:   pointer
    validator*: pointer # An< offset to validation code in the global moduleId.
    default*:   pointer # Depending on the type, either a value, or a ptr
    status*:    int32   # 0 for no value present.
    moduleId*:   int32

  ZInstruction* = object
    # Compilers should all pad this structure to 24 bytes, but just in case...
    op*:        ZOp     # 1 byte
    pad:        byte
    moduleId*:     int16   # Right now, 0 for global variables,
                        # -1 for stack, and then a positive number for
                        # modules. These don't really need to live in
                        # separate memory moduleIds, but we computed
                        # offsets on a per-module basis.
    lineNo*:    int32   # Line # in the current module this was gen'd at.
    arg*:       int32   # An offset, call number, etc.
    immediate*: int64   # Anything that must be 64-bits.
    typeInfo*:  TypeId  # TypeID associated w/ a data object in the source
                        # code. Will not always be concrete, but is there
                        # to facilitate run-time type info without having
                        # to do more accounting than needed.

  ZFFiArgInfo* = ref object
    held*:    bool   # Whether passing a pointer to the thing causes it
                     # to hold the pointer, in which case decref must
                     # be explicit.
    alloced*: bool   # This passes a value back that was allocated
                     # in the FFI.
    argType*: int16  # an index into the CTypeNames data structure in ffi.nim.
    ourType*: int32  # To look up any FFI processing we do for the type.

  ZFFiInfo* = ref object
    nameOffset*: int
    localName*:  int
    va*:         bool
    dlls*:       seq[int]
    argInfo*:    seq[ZffiArgInfo]

  ZFnInfo* = ref object
    # This field maps offsets to the name of the field. Frame
    # temporaries do not get name information here.
    funcname*:   string
    syms*:       Dict[int, string]
    # Type IDs at compile time. Particularly for debugging info, but
    # will be useful if we ever want to, from the object file, create
    # optimized instances of functions based on the calling type, etc.
    # Parameters are held seprately from stack symbols, and like w/
    # syms, we only capture variables scoped to the entire function.
    # We'll probably address that later.
    paramTypes*: seq[TypeId]
    # symTypes has offsets mapped to the compile-time type.
    # At run-time, the type will always need to be concrete.
    symTypes*:   seq[(int, TypeId)]
    # Offset in bytes into the module's generated code where this
    # function begins.
    offset*:     int
    size*:       int

  # This is all the data that will be in an "object" file; we'll
  # focus on being able to marshal and load these only.
  #
  # When we move on to storing object files, the data will be
  # marshal'd into the object's static data segment, and even the
  # dictionaries will be laid out so that we can just load them into
  # hatrack data structures w/o having to re-insert k/v pairs.
  #
  # Also, the object file will have room for runtime save state, a
  # resumption point (i.e., a replacement entry point), and a chalk
  # mark.
  ZObjectFile* = ref object
    zeroMagic*:      int = 0x0c001dea0c001dea
    zcObjectVers*:   int = 0x01
    staticData*:     string
    globals*:        Dict[int, string]
    tInfo*:          Dict[TypeId, int]   # Index into static Data for repr
    symTypes*:       seq[(int, TypeId)]  # address : TypeId
    globalScopeSz*:  int
    moduleContents*: seq[ZModuleInfo]
    entrypoint*:     int32  # A module ID
    nextEntrypoint*: int32  # A module ID
    funcInfo*:       seq[ZFnInfo]
    ffiInfo*:        seq[ZFfiInfo]
    spec*:           ValidationSpec

  ZModuleInfo* = ref object
    modname*:        string
    location*:       string
    version*:        string
    symTypes*:       seq[(int, TypeId)]
    codesyms*:       Dict[int, string]
    datasyms*:       Dict[int, string]
    source*:         string
    moduleId*:       int
    moduleVarSize*:  int
    instructions*:   seq[ZInstruction]
    initSize*:       int # size of init code before functions begin.

  ZList* = ref object
    l*:   seq[pointer]
    tid*: TypeId

  # Used in vm.nim
  StackFrame*   = object
    callModule*:   ZModuleInfo
    calllineno*:   int32
    targetline*:   int32
    targetfunc*:   ZFnInfo
    targetmodule*: ZModuleInfo # If not targetFunc

  AttrContents* = ref object
   contents*:    pointer
   tid*:         TypeId
   isSet*:       bool
   locked*:      bool
   lockOnWrite*: bool

  RuntimeState* = ref object
    obj*:            ZObjectFile
    frameInfo*:      array[MAX_CALL_DEPTH, StackFrame]
    numFrames*:      int
    stack*:          array[STACK_SIZE, pointer]
    curModule*:      ZModuleInfo
    sp*:             int
    fp*:             int
    ip*:             int      # Index into instruction array, not bytes.
    returnRegister*: pointer
    rrType*:         pointer
    moduleIds*:      seq[seq[pointer]]
    attrs*:          Dict[string, AttrContents]
    externCalls*:    seq[CallerInfo]
    externArgs*:     seq[seq[FfiType]]
    externFps*:      seq[pointer]
    specLock*:       bool

  MixedObj* = object
    t*:     TypeId
    value*: pointer

  Mixed* = ptr MixedObj

proc memcmp*(a, b: pointer, size: csize_t): cint {.importc,
                                                   header: "<string.h>",
                                                   noSideEffect.}

template debug*(x: string) =
  print(h1("Debug") + text(x), file = stderr)

template debug*(r: Rope) =
  print(h1("Debug") + r, file = stderr)

template debug*(s: string, s2: string, moreargs: varargs[string]) =
  var
    cells: seq[seq[string]]
    i = 0
    args = @[s, s2]

  for item in moreargs:
    args.add(item)

  while i < args.len():
    var row: seq[string]
    row.add(args[i])
    i += 1
    row.add(args[i])
    i += 1
    cells.add(row)

  cells.add(@["trace", getStackTrace()])

  debug(cells.quickTable(verticalHeaders = true))
