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
    TTIf, TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue,
    TtIdentifier, TtSof, TtEof, ErrorTok, ErrorLongComment, ErrorStringLit

  Con4mToken* = ref object
    kind*: Con4mTokenKind
    startPos*, endPos*, lineNo*, lineOffset*: int
    stream*: Stream

  Con4mNodeKind* = enum
    NodeBody, NodeAttrAssign, NodeVarAssign, NodeSection, NodeIfStmt,
    NodeConditional, NodeElse, NodeFor, NodeBreak, NodeContinue, NodeSimpLit,
    NodeUnary, NodeNot, NodeMember, NodeIndex, NodeActuals, NodeCall,
    NodeDictLit, NodeKVPair, NodeListLit, NodeOr, NodeAnd, NodeNe, NodeCmp,
    NodeGte, NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul,
    NodeDiv, NodeIdentifier

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

  Box* = object
    case kind*: Con4mTypeKind
    of TypeBool: b*: bool
    of TypeString: s*: string
    of TypeInt: i*: int
    of TypeFloat: f*: float
    of TypeList, TypeDict: p*: pointer
    else: nil

  STEntry* = ref object
    tInfo*: Con4mType
    value*: Option[Box]
    subscope*: Option[Con4mScope]
    defLocs*: seq[int]
    useLocs*: seq[int]

  Con4mScope* = ref object
    parent*: Option[Con4mScope]
    entries*: Table[string, STEntry]

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

let stringType* = Con4mType(kind: TypeString)
let boolType* = Con4mType(kind: TypeBool)
let intType* = Con4mType(kind: TypeInt)
let floatType* = Con4mType(kind: TypeFloat)
let bottomType* = Con4mType(kind: TypeBottom)

template unreachable*() =
  doAssert(false, "Reached code the programmer thought was unreachable :(")
