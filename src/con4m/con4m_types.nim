## Data types used across the project.  We generally try to keep types
## out of this file if they're only used in one module (particularly
## true in macros.nim, which is all compile-time).
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022


import streams
import tables
import options
import sugar

type
  ## Enumeration of all possible lexical tokens. Should not be exposed
  ## outside the package.
  Con4mTokenKind* = enum
    TtWhiteSpace, TtSemi, TtNewLine, TtLineComment, TtPlus, TtMinus, TtMul,
    TtLongComment, TtDiv, TTMod, TtLte, TtLt, TtGte, TtGt, TtNeq, TtNot,
    TtLocalAssign, TtColon, TtAttrAssign, TtCmp, TtComma, TtPeriod,
    TtLBrace, TtRBrace, TtLBracket, TtRBracket, TtLParen, TtRParen,
    TtAnd, TtOr, TtIntLit, TtFloatLit, TtStringLit, TtTrue, TtFalse, TtNull,
    TTIf, TTElIf, TTElse, TtFor, TtFrom, TtTo, TtBreak, TtContinue, TtEnum,
    TtIdentifier, TtSof, TtEof, ErrorTok, ErrorLongComment, ErrorStringLit

  Con4mToken* = ref object
    ## Lexical tokens. Should not be exposed outside the package.
    case kind*: Con4mTokenKind
    of TtStringLit:
      unescaped*: string
    else:
      nil
    startPos*, endPos*, lineNo*, lineOffset*: int
    stream*: Stream

  Con4mNodeKind* = enum
    ## Parse tree nodes types. Really no reason for these to be
    ## exposed either, other than the fact that they're contained in
    ## state objects that are the primary object type exposed to the
    ## user.
    NodeBody, NodeAttrAssign, NodeVarAssign, NodeSection, NodeIfStmt,
    NodeConditional, NodeElse, NodeFor, NodeBreak, NodeContinue, NodeSimpLit,
    NodeUnary, NodeNot, NodeMember, NodeIndex, NodeActuals, NodeCall,
    NodeDictLit, NodeKVPair, NodeListLit, NodeOr, NodeAnd, NodeNe, NodeCmp,
    NodeGte, NodeLte, NodeGt, NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul,
    NodeDiv, NodeEnum, NodeIdentifier

  Con4mTypeKind* = enum
    ## The enumeration of possible top-level types in Con4m
    TypeString, TypeBool, TypeInt, TypeFloat, TypeList, TypeDict, TypeProc,
    TypeTVar, TypeBottom

  Con4mType* = ref object
    ## The internal representation of a type.  Generally, you should
    ## write strings, and con4m will parse them.
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

  ListBox*[T] = ref object 
    contents*: seq[T]
    empty*: bool

  DictBox*[K, V] = ref object 
    contents*: TableRef[K, V]
    empty*: bool    
    
  Box* = ref object
    ## This type is used in cases where a specification allows
    ## multiple types to be stored.  This shouldn't happen often, but
    ## hey, here you go.
    ##
    ## The macros remove the need to explicitly use boxing when you do
    ## pre-specify types.
    case kind*: Con4mTypeKind
    of TypeBool: b*: bool
    of TypeString: s*: string
    of TypeInt: i*: int
    of TypeFloat: f*: float
    of TypeList: l*: RootRef
    of TypeDict: d*: RootRef
    else:
      nil

  STEntry* = ref object
    ## Internal; our symbol table data structure.
    tInfo*: Con4mType
    value*: Option[Box]
    override*: Option[Box] ## If a command-line flag or the program set this
                           ## value at runtime, then it will automatically be
                           ## re-set after the configuration file loads.
    subscope*: Option[Con4mScope]
    firstDef*: Option[Con4mNode]
    locked*: bool

  Con4mScope* = ref object
    ## Internal. It represents a single scope, only containing a
    ## dictionary plus a link to out parent scope, if any.
    parent*: Option[Con4mScope]
    entries*: OrderedTable[string, STEntry]

  Con4mSectInfo* = seq[(string, Con4mScope)]

  CurScopes* = object
    ## At any point in a Con4m program, there are to different scopes,
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
    attrs*: Con4mScope
    vars*: Con4mScope

  Con4mNode* = ref object
    ## The actual parse tree node type.  Should generally not be exposed.
    kind*: Con4mNodeKind
    token*: Option[Con4mToken] # Set on terminals, and some non-terminals
    children*: seq[Con4mNode]
    parent*: Option[Con4mNode] # Root is nil
    typeInfo*: Con4mType
    scopes*: Option[CurScopes]
    value*: Box

  BuiltInFn* = ((seq[Box], Con4mScope, Con4mScope) -> Option[Box])
  ## The Nim type signature for builtins that can be called from Con4m.

  BuiltInInfo* = ref object
    ## Internal; table entry for the registered builtin functions.
    fn*: BuiltInFn
    tinfo*: Con4mType

  FieldValidator* = (seq[string], Box) -> bool
  ## This isn't implemented fully yet, but will allow the program to
  ## specify additional value checking to be done on a field before
  ## accepting a configuration file.

  AttrSpec* = ref object
    ## Internal. This is the data structure holding specification data
    ## for individual attributes, used to check for well-formed config
    ## files, to plug in defaults, ...
    doc*: string
    attrType*: string
    validator*: Option[FieldValidator]
    defaultVal*: Option[Box]
    lockOnWrite*: bool
    required*: bool

  FieldAttrs* = OrderedTable[string, AttrSpec]
  ## Internal.  Alias for the table in a section specification maping
  ## its fields to attribute specifications.

  SectionSpec* = ref object
    ## Internal. This holds specification data for a top-level
    ## section.
    requiredSubsections*: seq[string]
    allowedSubsections*: seq[string]
    predefinedAttrs*: FieldAttrs
    customAttrs*: bool
    doc*: string
    associatedSpec*: ConfigSpec ## Don't use this, it's only temporary to
                                  ## support having *some* code in place
                                  ## for seprately typed subsections.

  ConfigSpec* = ref object
    ## The main user-level abstraction for holding specification data
    ## for a config file schema.  Fill it with calls to `addAttr()`,
    ## `addSection()`, etc (or, better yet, through the `con4m()`
    ## macro).
    ##
    ## The spec will be used to ensure the config file is well formed
    ## enough to work with, by comparing it against the results of
    ## execution.
    builtins*: OrderedTable[string, seq[BuiltInInfo]]
    secSpecs*: OrderedTable[string, SectionSpec]
    globalAttrs*: FieldAttrs
    customTopLevelOk*: bool

  SectionState* = ref object
    ## Internal. This holds the overall information about a single
    ## section's evauluation state, for use primarily in checking
    ## to make sure required sections are present.
    isLocked*: bool
    substateObjs*: OrderedTable[string, SectionState]
    beenSeen*: bool

  ConfigState* = ref object
    ## The top-level representation of a configuration's runtime
    ## state. The symbols are in here, the specs we apply, etc.
    ## Still, the end user should not need to access the members,
    ## except via API.
    stateObjs*: OrderedTable[string, SectionState]
    st*: Con4mScope
    spec*: Option[ConfigSpec]
    errors*: seq[string]
    builtins*: Table[string, seq[BuiltInInfo]]

let
  # These are just shared instances for types that aren't
  # parameterized, instead of having to instantiate multiple
  # instances.  Should not be exposed to the user.
  stringType* = Con4mType(kind: TypeString)
  boolType* = Con4mType(kind: TypeBool)
  intType* = Con4mType(kind: TypeInt)
  floatType* = Con4mType(kind: TypeFloat)
  bottomType* = Con4mType(kind: TypeBottom)

template unreachable*() =
  ## We use this to be explicit about case statements that are
  ## necessary to cover all cases, but should be impossible to
  ## execute.  That way, if they do execute, we know we made a
  ## mistake.
  doAssert(false, "Reached code the programmer thought was unreachable :(")


