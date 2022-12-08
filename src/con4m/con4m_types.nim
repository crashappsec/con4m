import streams
import tables
import options
import sugar

type
  Con4mTokenKind* = enum
    TtWhiteSpace, TtSemi, TtNewLine, TtLineComment, TtPlus, TtMinus, TtMul,
    TtLongComment, TtDiv, TTMod, TtLte, TtLt, TtGte, TtGt, TtNeq, TtNot,
    TtLocalAssign, TtColon, TtAttrAssign, TtCmp, TtComma, TtPeriod,
    TtLBrace, TtRBrace, TtLBracket, TtRBracket, TtLParen, TtRParen,
    TtAnd, TtOr, TtIntLit, TtFloatLit, TtStringLit, TtTrue, TtFalse, TtNull,
    TTIf, TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtEnum,
    TtIdentifier, TtSof, TtEof, ErrorTok, ErrorLongComment, ErrorStringLit

  Con4mToken* = ref object
    case kind*: Con4mTokenKind
    of TtStringLit:
      unescaped*: string
    else:
      nil
    startPos*, endPos*, lineNo*, lineOffset*: int
    stream*: Stream

  Con4mNodeKind* = enum
    NodeBody, NodeAttrAssign, NodeVarAssign, NodeSection, NodeIfStmt,
    NodeConditional, NodeElse, NodeFor, NodeBreak, NodeContinue, NodeSimpLit,
    NodeUnary, NodeNot, NodeMember, NodeIndex, NodeActuals, NodeCall,
    NodeDictLit, NodeKVPair, NodeListLit, NodeOr, NodeAnd, NodeNe, NodeCmp,
    NodeGte, NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul,
    NodeDiv, NodeEnum, NodeIdentifier

  Con4mTypeKind* = enum
    TypeString, TypeBool, TypeInt, TypeFloat, TypeList, TypeDict, TypeProc,
    TypeTVar, TypeBottom

  Con4mType* = ref object
    case kind*: Con4mTypeKind
    of TypeList:
      itemType*: Con4mType
    of TypeDict:
      keyType*, valType*: Con4mType
    of TypeProc:
      params*: seq[Con4mType]
      va*: bool
      retType*: Con4mType
    of TypeTVar:
      varNum*: int
    else: discard

  Box* = ref object
    case kind*: Con4mTypeKind
    of TypeBool: b*: bool
    of TypeString: s*: string
    of TypeInt: i*: int
    of TypeFloat: f*: float
    else:
      p*: pointer

  STEntry* = ref object
    tInfo*: Con4mType
    value*: Option[Box]
    subscope*: Option[Con4mScope]
    defLocs*: seq[int]
    useLocs*: seq[int] # TODO?
    locked*: bool

  Con4mScope* = ref object
    parent*: Option[Con4mScope]
    entries*: OrderedTable[string, STEntry]

  Con4mSectInfo* = seq[(string, Con4mScope)]

  CurScopes* = object
    vars*: Con4mScope
    attrs*: Con4mScope

  Con4mNode* = ref object
    kind*: Con4mNodeKind
    token*: Option[Con4mToken] # Set on terminals, and some non-terminals
    children*: seq[Con4mNode]
    parent*: Option[Con4mNode] # Root is nil
    typeInfo*: Con4mType
    scopes*: Option[CurScopes]
    value*: Box

  BuiltInFn* = (seq[Box] -> Option[Box])

  BuiltInInfo* = ref object
    fn*: BuiltInFn
    tinfo*: Con4mType

  FieldValidator* = (seq[string], Box) -> bool

  AttrSpec* = ref object
    doc*: string
    attrType*: string
    validator*: Option[FieldValidator]
    defaultVal*: Option[Box]
    lockOnWrite*: bool
    required*: bool

  FieldAttrs* = OrderedTable[string, AttrSpec]

  SectionSpec* = ref object
    requiredSubsections*: seq[string]
    allowedSubsections*: seq[string]
    predefinedAttrs*: FieldAttrs
    customAttrs*: bool
    doc*: string
    associatedSpec*: ConfigSpec ## Don't use this, it's only temporary to
                                ## support having *some* code in place
                                ## for seprately typed subsections.

  ConfigSpec* = ref object
    builtins*: OrderedTable[string, seq[BuiltInInfo]]
    secSpecs*: OrderedTable[string, SectionSpec]
    globalAttrs*: FieldAttrs
    customTopLevelOk*: bool

  SectionState* = ref object
    isLocked*: bool
    substateObjs*: OrderedTable[string, SectionState]
    beenSeen*: bool

  ConfigState* = ref object
    stateObjs*: OrderedTable[string, SectionState]
    st*: Con4mScope
    spec*: Option[ConfigSpec]
    errors*: seq[string]
    builtins*: Table[string, seq[BuiltInInfo]]

let
  stringType* = Con4mType(kind: TypeString)
  boolType* = Con4mType(kind: TypeBool)
  intType* = Con4mType(kind: TypeInt)
  floatType* = Con4mType(kind: TypeFloat)
  bottomType* = Con4mType(kind: TypeBottom)

template unreachable*() =
  doAssert(false, "Reached code the programmer thought was unreachable :(")


