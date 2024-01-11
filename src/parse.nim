## This is a recursive simple descent parser.  Note that I've explicitly
## factored the grammar for right recursion, so in the expression grammar
## there is a bit of tree jockeying to get the tree to look natural.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import lex, ztypes/api
export lex, api

proc tokenId(n: Con4mNode): Rope =
  return atom(" (tokid:" & `$`(n.token.id) & ")").fgColor("jazzberry")

proc ropeNt(n: Con4mNode, name: string): Rope =
  result = atom(name).italic().fgColor("atomiclime") + n.tokenId()

proc ropeT(n: Con4mNode, name: string): Rope =
  result = atom(name).fgColor("fandango") + n.tokenId()

proc ropeNtNamed(n: Con4mNode, name: string): Rope =
  result = atom(name).italic().fgColor("atomiclime") + n.tokenId() +
            atom(" ") + strong($n.token)

proc ropeWalker(n: Con4mNode): (Rope, seq[Con4mNode]) =
  var toPrint: Rope

  case n.kind
  of NodeModule:         toPrint = n.ropeNt("Module")
  of NodeBody:           toPrint = n.ropeNt("Body")
  of NodeDocString:      toPrint = n.ropeNt("DocString")
  of NodeParamBlock:     toPrint = n.ropeNt("ParamBlock")
  of NodeExternBlock:    toPrint = n.ropeNt("ExternBlock")
  of NodeExternSig:      toPrint = n.ropeNt("ExternSig")
  of NodeExternParam:    toPrint = n.ropeNt("ExternParam")
  of NodeExternLocal:    toPrint = n.ropeNt("ExternLocal")
  of NodeExternDll:      toPrint = n.ropeNt("ExternDll")
  of NodeExternPure:     toPrint = n.ropeNt("ExternPure")
  of NodeExternHolds:    toPrint = n.ropeNt("ExternHolds")
  of NodeExternAllocs:   toPrint = n.ropeNt("ExternAllocs")
  of NodeExternReturn:   toPrint = n.ropeNt("ExternReturn")
  of NodeAssign:         toPrint = n.ropeNt("Assign")
  of NodeAttrSetLock:    toPrint = n.ropeNt("AttrSetLock")
  of NodeSection:        toPrint = n.ropeNt("Section")
  of NodeIfStmt:         toPrint = n.ropeNt("If")
  of NodeTypeOfStmt:     toPrint = n.ropeNt("TypeOf")
  of NodeValueOfStmt:    toPrint = n.ropeNt("ValueOf")
  of NodeCaseCondition:  toPrint = n.ropeNt("CaseCondition")
  of NodeCase:           toPrint = n.ropeNt("Case")
  of NodeWhileStmt:      toPrint = n.ropeNt("While")
  of NodeElifStmt:       toPrint = n.ropeNt("Elif")
  of NodeElseStmt:       toPrint = n.ropeNt("Else")
  of NodeForStmt:        toPrint = n.ropeNt("For")
  of NodeRange:          toPrint = n.ropeNt("Range")
  of NodeReturnStmt:     toPrint = n.ropeNt("Return")
  of NodeNot:            toPrint = n.ropeNt("Not")
  of NodeMember:         toPrint = n.ropeNt("Member")
  of NodeIndex:          toPrint = n.ropeNt("Index")
  of NodeCall:           toPrint = n.ropeNt("Call")
  of NodeActuals:        toPrint = n.ropeNt("Actuals")
  of NodeParenExpr:      toPrint = n.ropeNt("ParenExpr")
  of NodeDictLit:        toPrint = n.ropeNt("DictLit")
  of NodeKVPair:         toPrint = n.ropeNt("KVPair")
  of NodeListLit:        toPrint = n.ropeNt("ListLit")
  of NodeTupleLit:       toPrint = n.ropeNt("TupleLit")
  of NodeOtherLit:       toPrint = n.ropeNt("Other")
  of NodeCharLit:        toPrint = n.ropeNt("Char")
  of NodeLiteral:        toPrint = n.ropeNt("Literal")
  of NodeEnumStmt:       toPrint = n.ropeNt("Enum")
  of NodeEnumItem:       toPrint = n.ropeNt("EnumItem")
  of NodeFormalList:     toPrint = n.ropeNt("Formals")
  of NodeType:           toPrint = n.ropeNt("Type")
  of NodeTypeVar:        toPrint = n.ropeNt("TypeVar")
  of NodeTypeFunc:       toPrint = n.ropeNt("TypeFunc")
  of NodeTypeTuple:      toPrint = n.ropeNt("TypeTuple")
  of NodeTypeList:       toPrint = n.ropeNt("TypeList")
  of NodeTypeDict:       toPrint = n.ropeNt("TypeDict")
  of NodeTypeObj:        toPrint = n.ropeNt("TypeObj")
  of NodeTypeRef:        toPrint = n.ropeNt("TypeRef")
  of NodeTypeTypeSpec:   toPrint = n.ropeNt("TypeTypeSpec")
  of NodeTypeBuiltin:    toPrint = n.ropeNt("TypeBuiltin")
  of NodeReturnType:     toPrint = n.ropeNt("TypeNodeReturnType")
  of NodeTypeVararg:     toPrint = n.ropeNt("TypeVararg")
  of NodeFormal:         toPrint = n.ropeNt("TypeFormal")
  of NodeVarStmt:        toPrint = n.ropeNt("VarStmt")
  of NodeGlobalStmt:     toPrint = n.ropeNt("GlobalStmt")
  of NodeVarSymInfo:     toPrint = n.ropeNt("VarSymInfo")
  of NodeConstStmt:      toPrint = n.ropeNt("ConstStmt")
  of NodeUseStmt:        toPrint = n.ropeNt("UseStmt")
  of NodeLabelStmt:      toPrint = n.ropeNt("LabelStmt")
  of NodeExpression:     toPrint = n.ropeNt("Expression")
  of NodeTypeOneOf:      toPrint = n.ropeNt("OneOf")
  of NodeTypeMaybe:      toPrint = n.ropeNt("Maybe")
  of NodeAssert:         toPrint = n.ropeNt("Assert")
  of NodeBreakStmt:      toPrint = n.ropeT("Break")
  of NodeContinueStmt:   toPrint = n.ropeT("Continue")
  of NodeVarargsFormal:  toPrint = n.ropeNt("Varargs")
  of NodeCallbackLit:    toPrint = n.ropeNtNamed("CallbackLit")
  of NodeStringLit:      toPrint = n.ropeNtNamed("String")
  of NodeIntLit:         toPrint = n.ropeNtNamed("Int")
  of NodeHexLit:         toPrint = n.ropeNtNamed("Hex")
  of NodeFloatLit:       toPrint = n.ropeNtNamed("Float")
  of NodeBoolLit:        toPrint = n.ropeNtNamed("Bool")
  of NodeNilLit:         toPrint = n.ropeNtNamed("Nil")
  of NodeFuncDef:        toPrint = n.ropeNtNamed("FuncDef")
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv,
     NodeBitOr, NodeBitXor, NodeBitAnd, NodeShl, NodeShr:
    toPrint = n.ropeNt($(n.token))
  of NodeIdentifier:   toPrint = n.ropeNtNamed("Identifier")

  result = (toPrint, n.children)

proc toRope*(n: Con4mNode): Rope =
  return n.quickTree(ropeWalker)

proc getText*(token: Con4mToken, adjust: static[bool] = false): string =
  when adjust:
    return $(token)
  else:
      if token.kind == TtStringLit:
        return token.unescaped
      elif token.kind == TtOtherLit:
        return $(token.cursor.slice(token.startPos, token.endPos))
      else:
        return $(token)

proc getText*(node: Con4mNode, adjust: static[bool] = false): string =
  ## This returns the raw string associated with a token.  Internal.
  return node.token.getText(adjust)

proc `$`*(n: Con4mNode): string =
  case n.kind
  of NodeModule:         "a module"
  of NodeBody:           "a block of statements"
  of NodeDocString:      "Documentation"
  of NodeParamBlock:     "a parameter block"
  of NodeExternBlock:    "an extern block"
  of NodeExternSig:      "an external function signature"
  of NodeExternParam:    "an external function parameter"
  of NodeExternLocal:    "the local interface to an external function"
  of NodeExternDll:      "an external function's expected DLL"
  of NodeExternPure:     "an external function's <em>pure</em> property"
  of NodeExternHolds:    "an external function's <em>holds</em> spec"
  of NodeExternAllocs:   "an external function's <em>allocs</em> spec"
  of NodeExternReturn:   "spec of the <em>return</em> for an external func"
  of NodeAssign:         "an assignment"
  of NodeAttrSetLock:    "a lock operation"
  of NodeSection:        "a section declaration"
  of NodeIfStmt:         "an <em>if</em> block"
  of NodeTypeOfStmt:     "a <em>typeof</em> statement"
  of NodeValueOfStmt:    "a <em>valueof</em> statement"
  of NodeCaseCondition:  "a <em>case</em> condition"
  of NodeCase:           "a <em>case</em> branch"
  of NodeRange:          "a numeric range"
  of NodeWhileStmt:      "a <em>while</em> loop"
  of NodeElifStmt:       "an <em>elif</em> block"
  of NodeElseStmt:       "an <em>else</em> block"
  of NodeForStmt:        "a <em>for</em> loop"
  of NodeReturnStmt:     "a <em>return</em> statement"
  of NodeNot:            "a <em>not</em> statement"
  of NodeMember:         "a member access"
  of NodeIndex:          "an indexing operation"
  of NodeCall:           "a function call"
  of NodeActuals:        "function parameters"
  of NodeParenExpr:      "a parenthesized expression"
  of NodeDictLit:        "a <em>dict</em> literal"
  of NodeKVPair:         "a key / value pair"
  of NodeListLit:        "a <em>list</em> literal"
  of NodeTupleLit:       "a <em>tuple</em> literal"
  of NodeCharLit:        "a character literal"
  of NodeLiteral:        "a literal"
  of NodeOtherLit:       "a literal"
  of NodeEnumStmt:       "an <em>enum</em> statement"
  of NodeEnumItem:       "an <em>enum</em> item"
  of NodeFormalList:     "func declaration formals"
  of NodeType:           "a type literal"
  of NodeTypeVar:        "a type variable"
  of NodeTypeFunc:       "a function type"
  of NodeTypeTuple:      "a <em>tuple</em> type"
  of NodeTypeList:       "a <em>list</em> type"
  of NodeTypeDict:       "a <em>dict</em> type"
  of NodeTypeObj:        "an <em>object</em> type"
  of NodeTypeRef:        "a <em>ref</em> type"
  of NodeTypeTypeSpec:   "a <em>typespec</em> type"
  of NodeTypeBuiltin:    ("a builtin type (" & n.getText() & ")")
  of NodeReturnType:     "a <em>return</em> type"
  of NodeTypeVararg:     "a <em>vararg</em> indicator"
  of NodeFormal:         "a formal parameter"
  of NodeVarStmt:        "a <em>var</em> statement"
  of NodeGlobalStmt:     "a <em>global</em> statement"
  of NodeConstStmt:      "a <em>const</em> statement"
  of NodeVarSymInfo:     "<em>var</em> statement symbol info"
  of NodeUseStmt:        "a <em>use</em> statement"
  of NodeLabelStmt:      "a <em>label</em> statement"
  of NodeExpression:     "an expression"
  of NodeTypeOneOf:      "a <em>oneof</em> type"
  of NodeTypeMaybe:      "a <em>maybe</em> type"
  of NodeBreakStmt:      "a <em>break</em> statement"
  of NodeContinueStmt:   "a <em>continue</em> statement"
  of NodeVarargsFormal:  "a <em>varargs</em> specifier"
  of NodeCallbackLit:    "a <em>callback</em> literal"
  of NodeStringLit:      "a <em>string</em> literal"
  of NodeIntLit:         "an <em>integer</em> literal"
  of NodeHexLit:         "a <em>hex<em> literal"
  of NodeFloatLit:       "a <em>float</em> literal"
  of NodeBoolLit:        "a <em>boolean</em> literal"
  of NodeNilLit:         "the <em>nil</em> value"
  of NodeFuncDef:        "a <em>function</em> declaration"
  of NodeAssert:         "an assert statement"
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv,
     NodeBitOr, NodeBitXor, NodeBitAnd, NodeShl, NodeShr:
    ("the <em>" & $(n.getText()) & "</em> operator")
  of NodeIdentifier:
    "an identifier <em>(" & $(n.getText()) & ")</em>"

proc addKid*(parent: Con4mNode, kid: Con4mNode) =
  parent.children.add(kid)
  kid.parent = parent

template tokAt*(ctx: Module, i: int): Con4mToken =
  ctx.tokens[i]

# Prototypes for things where we need the forward reference.
proc body(ctx: Module): Con4mNode
proc optionalBody(ctx: Module): Con4mNode
proc typeSpec(ctx: Module): Con4mNode
proc expressionStart(ctx: Module): Con4mNode
proc notExpr(ctx: Module): Option[Con4mNode]
proc accessExpr(ctx: Module): Con4mNode
proc typeOfStmt(ctx: Module): Con4mNode
proc valueOfStmt(ctx: Module): Con4mNode
proc forStmt(ctx: Module): Con4mNode
proc whileStmt(ctx: Module): Con4mNode
proc continueStmt(ctx: Module): Con4mNode
proc breakStmt(ctx: Module): Con4mNode
proc returnStmt(ctx: Module): Con4mNode
proc varStmt(ctx: Module): Con4mNode
proc globalStmt(ctx: Module): Con4mNode
proc constStmt(ctx: Module): Con4mNode
proc labelStmt(ctx: Module): Con4mNode
proc useStmt(ctx: Module): Con4mNode
proc parameterBlock(ctx: Module): Con4mNode
proc assign(ctx: Module, lhs: Con4mNode): Con4mNode
proc section(ctx: Module, lhs: Con4mNode): Con4mNode

proc inFunction(ctx: Module): bool {.inline.} =
  return ctx.inFunc

proc inLoop(ctx: Module): bool {.inline.} =
  return ctx.loopDepth != 0

proc curTok(ctx: Module): Con4mToken

const stmtStartList = [NodeAssign, NodeAttrSetLock,
                       NodeSection, NodeIfStmt, NodeElifStmt, NodeElseStmt,
                       NodeForStmt, NodeWhileStmt, NodeBreakStmt,
                       NodeTypeOfStmt, NodeValueOfStmt]

proc newNode(ctx: Module, kind: Con4mNodeKind): Con4mNode =
  ctx.curNodeId += 1

  result = Con4mNode(id: ctx.curNodeId, kind: kind, depth: ctx.nesting,
                     prevTokenIx: ctx.curTokIx)
  result.token       = ctx.curTok()
  if ctx.cachedComments.len() != 0 and kind in stmtStartList:
    if ctx.prevNode != nil:
      ctx.prevNode.commentLocs &= ctx.cachedComments
    else:
      result.commentLocs = ctx.cachedComments

    ctx.cachedComments = @[]

  ctx.prevNode = result

const alwaysSkip* = [TtSof, TtWhiteSpace, TtLineComment, TtLongComment]

# When outputting errors: return "" we might want to back up a token.
# need to skip ws when doing that.
proc unconsume(ctx: Module) =
  while true:
    ctx.curTokIx -= 1
    if not alwaysSkip.contains(ctx.curTok().kind):
      return

proc parseBaseError*(ctx: Module, code: string, backup: bool,
                     warn: bool, subs: seq[string], st: string,
                    ii: Option[InstantiationInfo]) =
  if backup:
    ctx.unconsume()

  let tok = ctx.curTok()
  let sev = if warn: LlWarn else: LlErr

  ctx.errors.baseError(code, tok, ctx.modname, ErrParse, sev, subs, nil, st, ii)

template parseError*(ctx: Module, msg: string,
                    extra: seq[string] = @[]) =
  when defined(debug):
    ctx.parseBaseError(msg, true, false, extra, getStackTrace(),
                       some(instantiationInfo()))
  else:
    ctx.parseBaseError(msg, true, false, extra, "", none(InstantiationInfo))

template parseErrorNoBackup*(ctx: Module, msg: string,
                            extra: seq[string] = @[]) =
  when defined(debug):
    ctx.parseBaseError(msg, false, false, extra, getStackTrace(),
                       some(instantiationInfo()))
  else:
    ctx.parseBaseError(msg, false, false, extra, "", none(InstantiationInfo))

template errSkipStmt*(ctx: Module, msg: string,
                     extra: seq[string] = @[]) =
  ctx.parseError(msg, extra)
  con4mLongJmp()

template errSkipStmtNoBackup*(ctx: Module, msg: string,
                             extra: seq[string] = @[]) =
  ctx.parseErrorNoBackup(msg, extra)
  con4mLongJmp()

template production(prodName: untyped,
                    nodeType: Con4mNodeKind, prodImpl: untyped) {.dirty.} =
  proc `prodName`(ctx: Module): Con4mNode =
    result = ctx.newNode(nodeType)
    prodImpl

  proc `parse prodName`*(ctx: Module): bool =
    if ctx.tokens.len() == 0:
      if not ctx.lex():
        return false

    ctx.root = ctx.`prodName`()

    return ctx.errors.canProceed()

  proc `parse prodName`*(s: string, errs: var seq[Con4mError],
                         modname = ""): Con4mNode =
    var ctx = Module()
    ctx.s       = s.newStringCursor()
    ctx.modname = modname

    if ctx.`parse prodName`():
      result =  ctx.root

    errs = ctx.errors

template idStmtProd(prodName: untyped, nodeType: Con4mNodeKind,
                    prodImpl: untyped) {.dirty.} =
  proc `prodName`(ctx: Module, lhs: Con4mNode): Con4mNode =
    result = ctx.newNode(nodeType)
    result.addKid(lhs)
    prodImpl

  proc `parse prodName`*(ctx: Module): bool =
    if ctx.tokens.len() == 0:
      if not ctx.lex():
        return false

    let x = ctx.expression()
    ctx.root = ctx.`prodName`(x)

    return ctx.errors.canProceed()

  proc `parse prodName`*(s: string, errs: var seq[Con4mError],
                         modname = ""): Con4mNode =
    var ctx: Module
    ctx.s      = s.newStringCursor()
    ctx.modname = modname

    if ctx.`parse prodName`():
      result =  ctx.root

    errs = ctx.errors

template exprProd(prodName, rhsName, chainNext, tokenType, nodeType: untyped) =
  proc prodName(ctx: Module): Option[Con4mNode]

  proc rhsName(ctx: Module): Con4mNode =
    var n = ctx.expressionStart()
    while true:
      let optExpr = ctx.prodName()
      if optExpr.isNone():
        return n
      else:
        var r = optExpr.get()
        if n != nil:
          r.children = @[n] & r.children
        n = r

  proc prodName(ctx: Module): Option[Con4mNode] =
    var res: Con4mNode

    if ctx.curKind() == tokenType:
      res = ctx.newNode(nodeType)
      ctx.advance()
      res.addKid(ctx.rhsName())
      result = some(res)
    else:
      result = ctx.chainNext()

template errBail(ctx: Module, msg: string) =
  ## When re-syncing is too likely to be error-prone, we bail from
  ## the rest of the parse.
  ctx.parseErrorNoBackup(msg)
  con4mLongJmp("BAIL")

# When it comes to whitespace in Con4m, we always skip whitespace
# tokens. There's no white space token sensitivity after the lexing
# phase.

# However, there is newline sensitivity. Since we don't require
# semicolons, we generally need newline to delimit "end-of-statement"
# to avoid some potentially ambiguous scenarios.
#
# While we could explicitly skip newlines everywhere except where
# there might be an ambiguity, we find it's best to add a little bit
# more structure.
#
# We definitely think you should be able to have an arbitrary amount
# of white space between statements. But in other places where we
# allow optional newlines, there's an advantage to it not being
# arbitrary.
#
# Specifically, consider a multi-line literal declaration, like
# `ignore1Nl` right below this comment.
#
# Of course, one should be able to move items to the next line, and
# even have a comment in the middle.
#
# But the programmer can easily forget to finish such a
# declaration. If we limit ourselves to not allowing two newlines in a
# row (ignoring spaces here, but not comments), then we'll often error
# out on this kind of issue right away. If we don't add this
# restriction, the parse could end up valid for quite a while.
#
# Basically, we try to not get in the way / be as intuitive as
# possible without being helpful, by applying the following rules:
#
# 1. We allow arbitrary newlines between statements.
# 2. For literal declarations in container types (lists, tuples, etc), we
#    allow a single newline after any single token. Comment lines are
#    additionally allowed; you basically aren't allowed to have two
#    newlines in a row (ignoring spaces/tabs).
# 3. For operators (all binary operators and the unary not), we also allow
#    newlines / comments. Comments are more dubious in this situation, but
#    it seemed better for regularity.
# 4. Putting parentheses around an expression similary allows for arbitrary
#    newlines as long as there are not two in a row.
# 5. Any time a '{' in the middle of a stement is expected to delinate a
#    block of code (e.g., for loops, sections, etc), you can put the brace
#    on the next line (or put comments before the brace; again, as long as
#    you don't have two newlines in a row).
#
# For #1, the body / toplevel productions just treat a bare newline as
# an empty statement, and generate no AST.
#
# For #2, when we open up a literal, we increment a 'literal' field
# that gets checked when calling curTok().  If curTok() sees this is
# non-zero, it will skip newlines until it sees two in a row. We use
# this field for #4 as well, even though it's not technically a
# literal (except in the context of tuples).
#
# For #3, curTok() also handles this, by checking the previous token
# agains tthe `ignore1Nl` list below.
#
# For #5, that's handled in the 'body' production by setting a field
# in the parse context, telling curTok() to skip as a one-off (since
# there's no nesting, the way there is with parens. curTok() clears
# this field when it's set.

const ignore1Nl = [ TtPlus, TtMinus, TtMul, TtDiv, TtMod, TtLte, TtLt,
                    TtGte, TtGt, TtNeq, TtNot, TtColon, TtAssign,
                    TtCmp, TtComma, TtPeriod, TtLBrace, TtLBracket,
                    TtLParen, TtAnd, TtOr, TTFrom, TtTo, TtArrow, TtIn,
                    TtBitAnd, TtBitOr, TtBitXor, TTShl, TtShr ]

const commentToks = [TtLineComment, TtLongComment]


proc doNewlineSkipping(ctx: Module) =
  var stopAtNextNewline = false
  while true:
    while ctx.tokens[ctx.curTokIx].kind in alwaysSkip:
      if ctx.tokens[ctx.curTokIx].kind in commentToks:
        ctx.cachedComments.add(ctx.curTokIx)
        stopAtNextNewLine = false
      ctx.curTokIx += 1

    if ctx.tokens[ctx.curTokIx].kind == TtNewline:
      ctx.curTokIx += 1
      if stopAtNextNewline:
        return
      else:
        stopAtNextNewline = true
    else:
      return

template skipNextNewline(ctx: Module) =
  ctx.skipOneNewline = true

proc skipOneNewlineIfAppropriate(ctx: Module) =

  if ctx.skipOneNewline:
    ctx.doNewlineSkipping()
    ctx.skipOneNewline = false
    return

  if ctx.curTokIx == 0:
    return
  if ctx.literalDepth == 0 and ctx.tokAt(ctx.curTokIx - 1).kind notin ignore1Nl:
    return

  ctx.doNewlineSkipping()

proc curTok(ctx: Module): Con4mToken =
  # when a token is consumed, we simply advance the index past it, and
  # then leave it to the next call to curTok() to advance.
  # First, we look at that last token; if it allows us to
  # skip one new line, we do so.
  #
  # We're also allowed to skip _one_ newline if we're in a literal,
  # after ANY token whatsoever.

  ctx.skipOneNewlineIfAppropriate()
  while true:
    let kind = ctx.tokAt(ctx.curTokIx).kind

    if kind notin alwaysSkip:
      break

    if kind in commentToks:
        ctx.cachedComments.add(ctx.curTokIx)

    ctx.curTokIx += 1

  result = ctx.tokAt(ctx.curTokIx)

template curKind(ctx: Module): Con4mTokenKind =
  ctx.curTok().kind

proc consume(ctx: Module): Con4mToken {.inline.} =
  result = ctx.curTok()
  ctx.curTokIx += 1

proc advance(ctx: Module) {.inline.} =
  discard ctx.curTok()
  ctx.curTokIx += 1

proc ignoreAllNewlines(ctx: Module) =
  while ctx.curKind() == TtNewline:
    ctx.advance()

proc lookAhead(ctx: Module, numToks: int = 1): Con4mToken =
  let cur = ctx.curTokIx
  var n = numToks

  while n != 0:
    ctx.advance()
    n = n - 1

  result = ctx.curTok()
  ctx.curTokIx = cur

proc atEndOfLine(ctx: Module): bool =
  let kind = ctx.curKind()
  if kind in [TtSemi, TtNewLine, TtRBrace, TtRParen, TtEOF]:
    return true

proc describeLastNode(ctx: Module): string =
  var n = ctx.prevNode
  if n.kind == NodeIdentifier and
     n.parent.kind in [NodeLiteral, NodeTypeBuiltin, NodeTypeVar,
                       NodeTypeMaybe, NodeTypeOneOf]:
    n = n.parent

  return $(n)

proc endOfStatement(ctx: Module, errIfNotThere = true) =
  if errIfNotThere and not ctx.atEndOfLine():
    ctx.errSkipStmtNoBackup("StmtEnd", @[ctx.describeLastNode()])
  else:
    while not ctx.atEndOfLine():
      ctx.advance() # Skip errors.
  while ctx.curKind() in [TtSemi, TtNewline]:
    ctx.advance()

proc expectOrErrConsuming(ctx: Module, kind: Con4mTokenKind) =
  let tok = ctx.consume()
  if tok.kind != kind:
    case kind
    of TtRBrace:
      if tok.kind == TtRBraceMod:
        ctx.parseError("NotALit")
        return
    of TtRBracket:
      if tok.kind == TtRBracketMod:
        ctx.parseError("NotALit")
        return
    of TtRParen:
      if tok.kind == TtRParenMod:
        ctx.parseError("NotALit")
        return
    else:
      discard
    ctx.errSkipStmt("MissingTok", @[$(kind)])

proc expectOrErr(ctx: Module, kind: Con4mTokenKind) =
  let foundKind = ctx.curKind()
  if foundKind != kind:
    case kind
    of TtRBrace:
      if foundKind == TtRBraceMod:
        ctx.parseError("NotALit")
        return
    of TtRBracket:
      if foundKind == TtRBracketMod:
        ctx.parseError("NotALit")
        return
    of TtRParen:
      if ctx.curKind() == TtRParenMod:
        ctx.parseError("NotALit")
        return
    else:
      discard
    ctx.errSkipStmtNoBackup("MissingTok", @[$(kind)])

template expect(ctx: Module, kind: Con4mTokenKind, consume = false) =
  if consume:
    ctx.expectOrErrConsuming(kind)
  else:
    ctx.expectOrErr(kind)

template adtLiteral(literalProduction: untyped) =
  ctx.nesting      += 1
  ctx.literalDepth += 1
  try:
    literalProduction
  finally:
    ctx.literalDepth -= 1
    ctx.nesting      -= 1

exprProd(shlExpr,    shlExprRHS,     notExpr,    TtShl,    NodeShl)
exprProd(shrExpr,    shrExprRHS,     shlExpr,    TtShr,    NodeShr)
exprProd(divExpr,    divExprRHS,     shrExpr,    TtDiv,    NodeDiv)
exprProd(mulExpr,    mulExprRHS,     divExpr,    TtMul,    NodeMul)
exprProd(modExpr,    modExprRHS,     mulExpr,    TtMod,    NodeMod)
exprProd(minusExpr,  minusExprRHS,   modExpr,    TtMinus,  NodeMinus)
exprProd(plusExpr,   plusExprRHS,    minusExpr,  TtPlus,   NodePlus)
exprProd(ltExpr,     ltExprRHS,      plusExpr,   TtLt,     NodeLt)
exprProd(gtExpr,     gtExprRHS,      ltExpr,     TtGt,     NodeGt)
exprProd(lteExpr,    lteExprRHS,     gtExpr,     TtLte,    NodeLte)
exprProd(gteExpr,    gteExprRHS,     lteExpr,    TtGte,    NodeGte)
exprProd(eqExpr,     eqExprRHS,      gteExpr,    TtCmp,    NodeCmp)
exprProd(binAndExpr, binAndExprRhs,  eqExpr,     TtBitAnd, NodeBitAnd)
exprProd(binXorExpr, binXorExprRhs,  binAndExpr, TtBitXor, NodeBitXor)
exprProd(binOrExpr,  binOrExprRhs,   binXorExpr, TtBitOr,  NodeBitOr)
exprProd(neExpr,     neExprRHS,      binOrExpr,  TtNeq,    NodeNe)
exprProd(andExpr,    andExprRHS,     neExpr,     TtAnd,    NodeAnd)
exprProd(orExpr,     orExprRhs,      andExpr,    TtOr,     NodeOr)

proc expression(ctx: Module): Con4mNode =
  let
    expr   = ctx.expressionStart()
    rhsOpt = ctx.orExpr()

  if rhsOpt.isSome():
    result = rhsOpt.get()
    if expr != nil:
      result.children = @[expr] & result.children
    while true:
      let optExpr = ctx.orExpr()
      if optExpr.isNone():
        return
      else:
        var r = optExpr.get()
        r.children = @[result] & r.children
        result = r
  else:
    return expr

production(identifier, NodeIdentifier):
  ctx.expect(TtIdentifier, consume = true)

proc memberExpr(ctx: Module, lhs: Con4mNode): Con4mNode =
  # For use from accessExpr, which peels off the first identifier.
  result = ctx.newNode(NodeMember)
  if lhs != Con4mNode(nil):
    result.addKid(lhs)

  while true:
    ctx.advance()
    if ctx.curKind() != TtIdentifier:
      ctx.errSkipStmt("MemberSpec")

    result.addKid(ctx.identifier())
    if ctx.curKind() != TtPeriod:
      return

proc memberExpr(ctx: Module): Con4mNode =
  # For use in contexts like parameters where we KNOW this can only be
  # an attribute.
  result = ctx.newNode(NodeMember)

  while true:
    result.addKid(ctx.identifier())
    if ctx.curKind() != TtPeriod:
      return
    ctx.advance()

proc indexExpr(ctx: Module, lhs: Con4mNode): Con4mNode =
  result = ctx.newNode(NodeIndex)

  result.addKid(lhs)

  adtLiteral:
    result.token = ctx.consume()
    result.addKid(ctx.expression())
    if ctx.curKind == TtColon:
      ctx.advance()
      result.addKid(ctx.expression())
    ctx.expect(TtRBracket, consume = true)

proc callActuals(ctx: Module, lhs: Con4mNode): Con4mNode =
  result = ctx.newNode(NodeCall)

  let
    actuals = ctx.newNode(NodeActuals)

  adtLiteral:
    ctx.advance()

    case lhs.kind
    of NodeIdentifier:
      result.addKid(lhs)
    of NodeMember:
      result.addKid(lhs.children[1])
      actuals.addKid(lhs.children[0])
    else:
      unreachable

    if ctx.curKind() == TtRParen:
      ctx.advance()
      result.addKid(actuals)
      return

    while true:
      actuals.children.add(ctx.expression())

      case ctx.curKind()
      of TtRParen:
        ctx.advance()
        result.addKid(actuals)
        return
      of TtComma:
        ctx.advance()
      else:
        ctx.errSkipStmt("ItemExpected", @[")"])

production(listLit, NodeListLit):
  ctx.advance()
  adtLiteral:
    if ctx.curKind() == TtRBracket:
      ctx.advance()
    elif ctx.curKind() == TtRBracketMod:
      let litmod = ctx.curTok().litType
      result.token.litType = litmod
      ctx.advance()
    else:
      while true:
        result.addKid(ctx.expression())
        case ctx.curKind()
        of TtComma:
          ctx.advance()
        of TtRBracket:
          let litmod = ctx.curTok().litType
          result.token.litType = litmod
          ctx.advance()
          return
        else:
          ctx.errSkipStmt("ItemExpected", @["]"])

production(kvPair, NodeKvPair):
  result.addKid(ctx.expression())
  ctx.expect(TtColon, consume = true)
  result.addKid(ctx.expression())

production(dictLit, NodeDictLit):
  ctx.advance()

  adtLiteral:
    if ctx.curKind() == TtRBrace:
      ctx.advance()
    elif ctx.curKind() == TtRBraceMod:
      let litmod = ctx.curTok().litType
      result.token.litType = litmod
      ctx.advance()
    else:
      while true:
        result.addKid(ctx.kvPair())
        case ctx.curKind()
        of TtComma:
          ctx.advance()
        of TtRBrace:
          ctx.advance()
          return
        of TtRBraceMod:
          let litmod = ctx.curTok().litType
          result.token.litType = litmod
          ctx.advance()
          return
        else:
          ctx.errSkipStmt("ItemExpected", @["}"])

production(tupleLit, NodeTupleLit):
  ctx.advance()

  adtLiteral:
    if ctx.curKind() in [ TtRParen, TtRParenMod ]:
      ctx.errSkipStmt("BadTuple")
    var gotSecondItem = false

    while true:
      result.addKid(ctx.expression())
      case ctx.curKind()
      of TtComma:
        gotSecondItem = true
        ctx.advance()
      of TtRParen:
        if not gotSecondItem:
          result.kind = NodeParenExpr
        ctx.advance()
        return
      of TtRParenMod:
        if not gotSecondItem:
          ctx.errSkipStmt("NotALit")
        else:
          let litmod = ctx.curTok().litType
          result.token.litType = litmod
          ctx.advance()
      else:
          ctx.errSkipStmt("ItemExpected", @[")"])

production(stringLit, NodeStringLit):
  ctx.expect(TtStringLit, consume = true)

production(intLit, NodeIntLit):
  ctx.expect(TtIntLit, consume = true)

production(hexLit, NodeHexLit):
  ctx.expect(TtHexLit, consume = true)

production(floatLit, NodeFloatLit):
  ctx.expect(TtFloatLit, consume = true)

production(charlit, NodeCharLit):
  ctx.expect(TtCharLit, consume = true)

production(boolLit, NodeBoolLit):
  ctx.advance()

production(nilLit, NodeNilLit):
  ctx.advance()

production(otherLit, NodeOtherLit):
  ctx.expect(TtOtherLit, consume = true)

production(parenExpr, NodeParenExpr):
  # Not technically an ADT literal, but OK.
  adtLiteral:
    ctx.advance()
    result = ctx.expression()
    ctx.expect(TtRParen, consume = true)

production(accessExpr, NodeIdentifier):
  case ctx.curKind()
  of TtLParen:
    result = ctx.parenExpr()
  of TtIdentifier:
    ctx.advance()
  else:
    ctx.errSkipStmt("AccessExpr")

  while true:
    case ctx.curKind()
    of TtPeriod:
      result = ctx.memberExpr(result)
    of TtLBracket:
      result = ctx.indexExpr(result)
    of TtLParen:
      result = ctx.callActuals(result)
    else:
      return

proc notExpr(ctx: Module): Option[Con4mNode] =
  case ctx.curKind()
  of TtNot:
    var res = ctx.newNode(NodeNot)
    ctx.advance()
    res.addKid(ctx.expression())
    return some(res)
  else:
    return

production(callback, NodeCallbackLit):
  ctx.advance()

  result.addKid(ctx.identifier())
  if ctx.curTok().kind == TtLParen:
    ctx.advance()
    result.addKid(ctx.typeSpec())

production(literal, NodeLiteral):
  case ctx.curKind()
  of TtBackTick, TtObject:
    result.addKid(ctx.typeSpec())
  of TtFunc:
    if ctx.lookAhead().kind == TTIdentifier:
      result.addKid(ctx.callback())
    else:
      ctx.advance()
      result.addKid(ctx.typeSpec())
  of TtIntLit:
    result.addKid(ctx.intLit())
  of TtHexLit:
    result.addKid(ctx.hexLit())
  of TtFloatLit:
    result.addKid(ctx.floatLit())
  of TtStringLit:
    result.addKid(ctx.stringLit())
  of TtCharLit:
    result.addKid(ctx.charLit())
  of TtTrue, TtFalse:
    result.addKid(ctx.boolLit())
  of TtNil:
    result.addKid(ctx.nilLit())
  of TtOtherLit:
    result.addKid(ctx.otherLit())
  of TtLBrace:
    result.addKid(ctx.dictLit())
  of TtLBracket:
    result.addKid(ctx.listLit())
  of TtLParen:
    result.addKid(ctx.tupleLit())
  of TtIdentifier:
    let txt = ctx.curTok().getText()

    if txt in getAllBuiltinTypeNames():
      result.addKid(ctx.typeSpec())
    else:
      ctx.errSkipStmt("LitExpected")
  else:
    ctx.errSkipStmt("LitExpected")

production(expressionStart, NodeExpression):
  case ctx.curKind()
  of TtIntLit, TtHexLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse,
     TtLBrace, TtLBracket, TtLParen, TtOtherLit, TtFunc, TtBacktick, TtObject,
     TtNil:
       result.addKid(ctx.literal())
  of TtIdentifier:
    let txt = ctx.curTok().getText()
    if txt in getAllBuiltinTypeNames():
      result.addKid(ctx.literal())
    else:
      result.addKid(ctx.accessExpr())
  of TtPlus, TtMinus, TtNot:
    # empty string for the LHS; For TtPlus and TtMinus, the proper
    # production below us will need to tease out whether it's a unary
    # or binary. And TtNot needs to reject when there's a lhs.
    return nil
  else:
    ctx.errSkipStmtNoBackup("ExprStart", @[$ctx.curTok()])

production(enumItem, NodeEnumItem):
  result.addKid(ctx.identifier())
  if ctx.curKind() in [TtAssign, TtColon]:
    ctx.advance()
    result.addKid(ctx.expression())

production(enumStmt, NodeEnumStmt):
  ctx.advance()

  while true:
    result.addKid(ctx.enumItem())
    if ctx.curKind() != TtComma:
      ctx.endOfStatement(false)
      return
    else:
      ctx.advance()

idStmtProd(assign, NodeAssign):
  ctx.advance()
  result.addKid(ctx.expression())
  ctx.endOfStatement()

production(lockAttr, NodeAttrSetLock):
  ctx.advance()
  result.addKid(ctx.assign(ctx.expression()))

production(elseStmt, NodeElseStmt):
  ctx.advance()
  result.addKid(ctx.body())

production(elifStmt, NodeElifStmt):
  ctx.advance()
  result.addKid(ctx.expression())
  result.addKid(ctx.body())

production(ifStmt, NodeIfStmt):
  ctx.advance()
  result.addKid(ctx.expression())
  result.addKid(ctx.body())

  while ctx.curKind() == TtElif:
    result.addKid(ctx.elifStmt())

  if ctx.curKind() == TtElse:
    result.addKid(ctx.elseStmt())

production(caseBody, NodeBody):
  ctx.advance()
  while true:
    try:
      let kind = ctx.curKind()
      case kind
      of TtCase, TtRBrace, TtElse:
        return
      of TtSemi, TtNewLine:
        ctx.advance()
        continue
      of TtEnum, TtFunc:
        ctx.errSkipStmtNoBackup("TopLevelOnly", @[$(ctx.curKind())])
      of TtLockAttr:
        result.addKid(ctx.lockAttr())
        continue
      of TtIf:
        result.addKid(ctx.ifStmt())
        continue
      of TtFor:
        result.addKid(ctx.forStmt())
        continue
      of TtWhile:
        result.addKid(ctx.whileStmt())
        continue
      of TtTypeOf:
        result.addKid(ctx.typeOfStmt())
        continue
      of TtValueOf:
        result.addKid(ctx.valueOfStmt())
        continue
      of TtContinue:
        if not ctx.inLoop():
          ctx.errSkipStmtNoBackup("InLoopsOnly", @["continue"])
        else:
          result.addKid(ctx.continueStmt())
        continue
      of TtBreak:
        if not ctx.inLoop():
          ctx.errSkipStmtNoBackup("InLoopsOnly", @["break"])
        else:
          result.addKid(ctx.breakStmt())
        continue
      of TtReturn:
        if not ctx.inFunction():
          ctx.errSkipStmtNoBackup("RetOutOfFunc")
        else:
          result.addKid(ctx.returnStmt())
        continue
      of TtVar:
        result.addKid(ctx.varStmt())
        continue
      of TtGlobal:
        result.addKid(ctx.globalStmt())
        continue
      of TtConst:
        result.addKid(ctx.constStmt())
        continue
      of TtIdentifier:
        if ctx.lookahead().kind == TtColon and
           ctx.lookahead(2).kind in [TtFor, TtWhile]:
          result.addKid(ctx.labelStmt())
          continue

        case ctx.curTok.getText()
        of "use":
          result.addKid(ctx.useStmt())
          continue
        of "parameter":
          ctx.errSkipStmtNoBackup("TopLevelPlural",
                                  @["<em>'parameter'</em> blocks"])
        of "extern":
          ctx.errSkipStmtNoBackup("TopLevelPlural",
                                  @["<em>'extern'</em> blocks"])
        else:
          discard
      else:
        discard

      let x = ctx.expression()
      case ctx.curKind()
      of TtColon, TtAssign:
        result.addKid(ctx.assign(x))
        continue
      of TtIdentifier, TtStringLit, TtLBrace:
        result.addKid(ctx.section(x))
        continue
      else:
        result.addKid(x)
        ctx.endOfStatement()
        continue
    except:
      if getCurrentException().msg == "BAIL":
        raise
      while true:
        if ctx.atEndOfLine() and ctx.curKind() notin [TtRBrace, TtRParen]:
          ctx.advance()
          break
        ctx.advance()

production(caseElseBlock, NodeElseStmt):
  ctx.advance()
  case ctx.curKind()
  of TtColon:
    result.addKid(ctx.caseBody())
  else:
    ctx.skipNextNewLine()
    if ctx.curKind() != TtLBrace:
      ctx.errBail("CaseBodyStart")
    else:
      result.addKid(ctx.body())

production(typeCaseCondition, NodeCaseCondition):
  while true:
    result.addKid(ctx.typeSpec())
    if ctx.curKind() == TtComma:
      ctx.advance()
    else:
      return

production(valueCaseCondition, NodeCaseCondition):
  while true:
    let n = ctx.expression()
    if ctx.curKind() == TtTo:
      let r = ctx.newNode(NodeRange)
      r.addKid(n)
      ctx.advance()
      r.addKid(ctx.expression())
      result.addKid(r)
    else:
      result.addKid(n)
    if ctx.curKind() == TtComma:
      ctx.advance()
    else:
      return

production(oneTypeCase, NodeCase):
  ctx.advance()
  let condition = ctx.typeCaseCondition()

  assert condition != nil
  result.addKid(condition)
  case ctx.curKind()
  of TtColon:
    result.addKid(ctx.caseBody())
  else:
    ctx.skipNextNewLine()
    if ctx.curKind() != TtLBrace:
      ctx.errBail("CaseBodyStart")
    else:
      result.addKid(ctx.body())

production(oneValueCase, NodeCase):
  ctx.advance()
  result.addKid(ctx.valueCaseCondition())
  case ctx.curKind()
  of TtColon:
    result.addKid(ctx.caseBody())
  else:
    ctx.skipNextNewLine()
    if ctx.curKind() != TtLBrace:
      ctx.errBail("CaseBodyStart")
    else:
      result.addKid(ctx.body())

production(typeofStmt, NodeTypeOfStmt):
  ctx.advance()
  result.addKid(ctx.memberExpr())
  ctx.skipNextNewLine()
  ctx.expect(TtLBrace, consume = true)
  ctx.expect(TtCase)

  while true:
    result.addKid(ctx.oneTypeCase())

    case ctx.curKind()
    of TtCase:
      continue
    of TtRBrace:
      ctx.advance()
      ctx.ignoreAllNewlines()
      return
    of TtElse:
      result.addKid(ctx.caseElseBlock())
      ctx.expect(TtRBrace, consume = true)
      return
    else:
      ctx.errBail("NextCase")

production(valueofStmt, NodeValueOfStmt):
  ctx.advance()
  result.addKid(ctx.expression())
  ctx.skipNextNewLine()
  ctx.expect(TtLBrace, consume = true)
  ctx.expect(TtCase)

  while true:
    result.addKid(ctx.oneValueCase())
    case ctx.curKind()
    of TtCase:
      continue
    of TtRBrace:
      ctx.advance()
      ctx.ignoreAllNewlines()
      return
    of TtElse:
      result.addKid(ctx.caseElseBlock())
      ctx.expect(TtRBrace, consume = true)
      return
    else:
      ctx.errBail("NextCase")

production(forVarList, NodeVarSymInfo):
  result.addKid(ctx.identifier())
  if ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.identifier())

production(forStmt, NodeForStmt):
  ctx.advance()
  result.addKid(ctx.forVarList()) # 1
  if ctx.curKind() == TtIn:
    ctx.advance()
    let n = ctx.expression() # 2
    if ctx.curKind() == TtTo:
      let r = ctx.newNode(NodeRange)
      r.addKid(n)
      ctx.advance()
      r.addKid(ctx.expression())
      result.addKid(r)
    else:
      result.addKid(n)
  else:
    ctx.expect(TtFrom, consume = true)
    let n = ctx.expression()
    ctx.expect(TtTo)
    let r = ctx.newNode(NodeRange)
    r.addKid(n)
    ctx.advance()
    r.addKid(ctx.expression())
    result.addKid(r)

  ctx.loopDepth += 1
  result.addKid(ctx.body()) # ^1
  ctx.loopDepth -= 1

production(whileStmt, NodeWhileStmt):
  ctx.advance()
  result.addKid(ctx.expression())
  ctx.loopDepth += 1
  result.addKid(ctx.body())
  ctx.loopDepth -= 1

production(builtinType, NodeTypeBuiltin):
  if ctx.curTok().getText() notin getAllBuiltinTypeNames():
    ctx.errSkipStmt("NameInTypeSpec", @[ctx.curTok().getText()])
  result.addKid(ctx.identifier())

production(refType, NodeTypeRef):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  ctx.expect(TtRBracket, consume = true)

production(tupleType, NodeTypeTuple):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.typeSpec())
  ctx.expect(TtRBracket, consume = true)
  if result.children.len() < 2:
    ctx.parseError("BadTuple")

production(typeOneOf, NodeTypeOneOf):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.typeSpec())
  ctx.expect(TtRBracket, consume = true)
  if result.children.len() < 2:
    ctx.parseError("BadOneOf")

production(typeMaybe, NodeTypeMaybe):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  ctx.expect(TtRBracket, consume = true)

production(dictType, NodeTypeDict):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  ctx.expect(TtComma, consume = true)
  result.addKid(ctx.typeSpec())
  ctx.expect(TtRBracket, consume = true)

production(listType, NodeTypeList):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  ctx.expect(TtRBracket, consume = true)

production(typeVariable, NodeTypeVar):
  ctx.advance()
  result.addKid(ctx.identifier())

production(typeTypeSpec, NodeTypeTypeSpec):
  ctx.advance()
  if ctx.curKind() == TtLBracket:
    ctx.advance()
    result.addKid(ctx.typeVariable())
    ctx.expect(TtRBracket, consume = true)

production(objectType, NodeTypeObj):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  case ctx.curKind()
  of TtBacktick:
    result.addKid(ctx.typeVariable())
  of TtIdentifier:
    result.addKid(ctx.identifier())
  else:
    ctx.errSkipStmt("BadObjType")
  ctx.expect(TtRBracket, consume = true)

production(typeVararg, NodeTypeVararg):
  ctx.advance()
  result.addKid(ctx.typeSpec())
  ctx.expect(TtRParen, consume = true)

production(returnType, NodeReturnType):
  if ctx.curKind() == TtArrow:
    ctx.advance()
    result.addKid(ctx.typeSpec())

production(funcType, NodeTypeFunc):
  ctx.advance()
  if ctx.curKind() == TtRParen:
    ctx.advance()
  else:
    while true:
      if ctx.curKind() == TtMul:
        result.addKid(ctx.typeVararg())
        break
      else:
        result.addKid(ctx.typeSpec())
        if ctx.curKind() == TtComma:
          ctx.advance()
        else:
          ctx.expect(TtRParen, consume = true)
          break
  result.addKid(ctx.returnType())

production(typeSpec, NodeType):
  case ctx.curKind()
  of TtBacktick:
    result.addKid(ctx.typeVariable())
  of TtLParen:
    result.addKid(ctx.funcType())
  of TtObject:
      result.addKid(ctx.objectType())
  of TtIdentifier:
    case ctx.curTok().getText()
    of "list":
      result.addKid(ctx.listType())
    of "dict":
      result.addKid(ctx.dictType())
    of "tuple":
      result.addKid(ctx.tupleType())
    of "ref":
      result.addKid(ctx.refType())
    of "typespec":
      result.addKid(ctx.typeTypeSpec())
    of "oneof":
      result.addKid(ctx.typeOneOf())
    of "maybe":
      result.addKid(ctx.typeMaybe())
    else:
      result.addKid(ctx.builtinType())
  else:
    ctx.errSkipStmt("BadTypeDecl")

production(formal, NodeFormal):
  result.addKid(ctx.identifier())
  if ctx.curKind() == TtColon:
    ctx.advance()
    result.addKid(ctx.typeSpec())

production(varargsFormal, NodeVarargsFormal):
  result.addKid(ctx.identifier())

production(formalList, NodeFormalList):
  ctx.advance()
  if ctx.curKind() == TtRParen:
    ctx.advance()
  else:
    while true:
      if ctx.curKind() == TtMul:
        result.addKid(ctx.varargsFormal())
        ctx.expect(TtRParen, consume = true)
        return
      result.addKid(ctx.formal())
      case ctx.curKind()
      of TtRParen:
        ctx.advance()
        return
      of TtComma:
        ctx.advance()
      else:
        ctx.errBail("BadFormalEnd")

production(funcDef, NodeFuncDef):
  ctx.advance()
  ctx.inFunc = true
  result.addKid(ctx.identifier())
  ctx.expect(TtLParen)
  result.addKid(ctx.formalList())
  if ctx.curKind() == TtArrow:
    result.addKid(ctx.returnType())
  result.addKid(ctx.body())
  ctx.inFunc = false

production(varSymInfo, NodeVarSymInfo):
   while true:
     result.addKid(ctx.identifier())
     if ctx.curKind() == TtComma:
       ctx.advance()
     else:
       break
   if ctx.curKind() == TtColon:
     ctx.advance()
     result.addKid(ctx.typeSpec())

production(varStmt, NodeVarStmt):
  var constNode: Con4mNode

  ctx.advance()
  if ctx.curKind() == TtConst:
    constNode = ctx.newNode(NodeConstStmt)
    ctx.advance()

  result.addKid(ctx.varSymInfo())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.varSymInfo())
  ctx.endOfStatement()

  if constNode != nil:
    result.addKid(constNode)

production(globalStmt, NodeGlobalStmt):
  var constNode: Con4mNode

  ctx.advance()
  if ctx.curKind() == TtConst:
    constNode = ctx.newNode(NodeConstStmt)
    ctx.advance()

  result.addKid(ctx.varSymInfo())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.varSymInfo())
  ctx.endOfStatement()

  if constNode != nil:
    result.addKid(constNode)

production(constStmt, NodeConstStmt):
  var globalNode: Con4mNode

  ctx.advance()
  if ctx.curKind() == TtGlobal:
    globalNode = ctx.newNode(NodeConstStmt)
    ctx.advance()
  elif ctx.curKind() == TtVar:
    ctx.advance()

  result.addKid(ctx.varSymInfo())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.varSymInfo())
  ctx.endOfStatement()

  if globalNode != nil:
    result.addKid(globalNode)

production(useStmt, NodeUseStmt):
  ctx.advance()
  result.addKid(ctx.identifier())

  if ctx.atEndOfLine():
    ctx.endOfStatement()
    return

  ctx.expect(TtFrom, consume = true)

  let kind = ctx.curKind()
  if kind == TtStringLit:
    result.addKid(ctx.stringLit())
    ctx.endOfStatement()
  else:
    ctx.errSkipStmt("BadUseSyntax")

production(labelStmt, NodeLabelStmt):
  result.addKid(ctx.identifier())
  ctx.advance()
  ctx.skipNextNewLine()

production(assertStmt, NodeAssert):
  ctx.advance()
  result.addKid(ctx.expression())
  ctx.endOfStatement()

production(oneVarDecl, NodeVarSymInfo):
  result.addKid(ctx.identifier())
  if ctx.curKind() == TtColon:
    ctx.advance()
    result.addKid(ctx.typeSpec())

production(paramVarDecl, NodeVarStmt):
  ctx.advance()
  result.addKid(ctx.oneVarDecl())

production(parameterBlock, NodeParamBlock):
  ctx.advance()
  case ctx.curKind()
  of TtIdentifier:
    result.addKid(ctx.memberExpr())
  of TtVar:
    result.addKid(ctx.paramVarDecl())
  else:
    ctx.errBail("BadParamName")
  result.addKid(ctx.optionalBody())

production(docString, NodeDocString):
  result.addKid(ctx.stringLit())
  ctx.skipNextNewline()
  if ctx.curKind() == TtStringLit:
    result.addKid(ctx.stringLit())
    ctx.skipNextNewline()

proc commonExternStart(ctx: Module) =
  ctx.advance()
  case ctx.curKind()
  of TtColon, TtAssign:
    ctx.advance()
  else:
    ctx.errSkipStmtNoBackup("MissingTok", @["a <em>:</em>"])

production(externLocalDef, NodeExternLocal):
  ctx.commonExternStart()
  result.addKid(ctx.identifier())
  if ctx.curKind() != TtLParen:
    ctx.errSkipStmtNoBackup("NeedSig")
  result.addKid(ctx.typeSpec())
  ctx.endOfStatement()

production(externDllName, NodeExternDll):
  ctx.commonExternStart()
  result.addKid(ctx.stringLit())
  ctx.endOfStatement()

production(externPure, NodeExternPure):
  ctx.commonExternStart()
  if ctx.curKind() notin [TtTrue, TtFalse]:
    ctx.errSkipStmtNoBackup("PureBool")
  result.addKid(ctx.boolLit())
  ctx.endOfStatement()

production(externRetLit, NodeExternReturn):
  ctx.advance()

production(externHolds, NodeExternHolds):
  ctx.commonExternStart()
  while true:
    if ctx.curKind() == TtIdentifier:
      result.addKid(ctx.identifier())
    else:
      ctx.errSkipStmtNoBackup("BadRCParam")
    if ctx.curKind() == TtComma:
      ctx.advance()
    else:
      ctx.endOfStatement()
      return

production(externAllocs, NodeExternAllocs):
  ctx.commonExternStart()
  while true:
    case ctx.curKind()
    of TtIdentifier:
      result.addKid(ctx.identifier())
    of TtReturn:
      result.addKid(ctx.externRetLit())
    else:
      ctx.errSkipStmtNoBackup("BadRCParam")
    if ctx.curKind() == TtComma:
      ctx.advance()
    else:
      ctx.endOfStatement()
      return

production(externParam, NodeExternParam):
  # If there's a colon, the first param is a variable name instead of
  # a CType spec. When we see the colon, we will swap out its node w/
  # a NodeMember so that we can easily capture the intent without more
  # superfluous node types, or checking against the FFI type names.

  var
    asId = ctx.identifier()

  if ctx.curKind() == TtColon:
    let fnameNode = ctx.newNode(NodeMember)
    fnameNode.addKid(asId)
    result.addKid(fNameNode)
    ctx.advance()
    result.addKid(ctx.identifier())
  else:
    result.addKid(asId)

  let ctype = result.children[^1].getText()
  if ctype notin cTypeNames:
    ctx.errSkipStmtNoBackup("BadCType", @[ctype])

  while result.children[^1].getText() in cTypeTakesParam:
    if ctx.curKind() != TtIdentifier:
      break
    result.addKid(ctx.identifier())
    if result.children[^1].getText() notin cTypeNames:
      ctx.errSkipStmtNoBackup("BadCType")

production(externSignature, NodeExternSig):
  ctx.expect(TtLParen, consume = true)
  if ctx.curKind() == TtRParen:
    ctx.advance()
  else:
    while true:
      result.addKid(ctx.externParam())
      if ctx.curKind() == TtComma:
        ctx.advance()
      else:
        ctx.expect(TtRParen, consume = true)
        break

  ctx.expect(TtArrow, consume = true)
  result.addKid(ctx.externParam())

production(externBlock, NodeExternBlock):
  ctx.advance()
  result.addKid(ctx.identifier())
  result.addKid(ctx.externSignature())
  ctx.expect(TtLBrace, consume = true)
  if ctx.curKind() == TtStringLit:
    result.addKid(ctx.docString())

  while true:
    if ctx.curKind() == TtRBrace:
      ctx.advance()
      return
    case ctx.curTok.getText()
    of "local":
      result.addKid(ctx.externLocalDef())
    of "dll":
      result.addKid(ctx.externDllName())
    of "pure":
      result.addKid(ctx.externPure())
    of "holds":
      result.addKid(ctx.externHolds())
    of "allocs":
      result.addKid(ctx.externAllocs())
    else:
      ctx.errSkipStmtNoBackup("BadExternField")

production(optionalBody, NodeBody):
  if ctx.curKind() == TtLBrace:
    result = ctx.body()
  else:
    ctx.endOfStatement()

idStmtProd(section, NodeSection):
  if ctx.curKind() == TtStringLit:
    result.addKid(ctx.stringLit())
  elif ctx.curKind() == TtIdentifier:
    result.addKid(ctx.identifier())

  result.addKid(ctx.optionalBody())

production(returnStmt, NodeReturnStmt):
  ctx.advance()
  if not ctx.atEndOfLine():
    result.addKid(ctx.expression())
  ctx.endOfStatement()

production(breakStmt, NodeBreakStmt):
  ctx.advance()
  if not ctx.atEndOfLine():
    result.addKid(ctx.identifier())
  ctx.endOfStatement()

production(continueStmt, NodeContinueStmt):
  ctx.advance()
  if not ctx.atEndOfLine():
    result.addKid(ctx.identifier())
  ctx.endOfStatement()

production(body, NodeBody):
  ctx.skipNextNewline()
  ctx.expect(TtLBrace, consume = true)
  if ctx.curKind() == TtStringLit:
    result.addKid(ctx.docString())

  ctx.nesting += 1
  let savedIx = ctx.curTokIx
  while true:
    try:
      let kind = ctx.curKind()
      case kind
      of TtEof:
        ctx.curTokIx = savedIx
        ctx.parseError("EofInBlock")
      of TtRBrace:
        ctx.nesting -= 1
        ctx.advance()
        ctx.ignoreAllNewlines()
        return
      of TtSemi, TtNewline:
        ctx.advance()
        continue
      of TtEnum, TtFunc:
        ctx.errSkipStmtNoBackup("TopLevelOnly", @[$(ctx.curKind())])
      of TtLockAttr:
        result.addKid(ctx.lockAttr())
        continue
      of TtIf:
        result.addKid(ctx.ifStmt())
        continue
      of TtFor:
        result.addKid(ctx.forStmt())
        continue
      of TtWhile:
        result.addKid(ctx.whileStmt())
        continue
      of TtTypeOf:
        result.addKid(ctx.typeOfStmt())
        continue
      of TtValueOf:
        result.addKid(ctx.valueOfStmt())
        continue
      of TtContinue:
        if not ctx.inLoop():
          ctx.errSkipStmtNoBackup("InLoopsOnly", @["continue"])
        else:
          result.addKid(ctx.continueStmt())
        continue
      of TtBreak:
        if not ctx.inLoop():
          ctx.errSkipStmtNoBackup("InLoopsOnly", @["break"])
        else:
          result.addKid(ctx.breakStmt())
        continue
      of TtReturn:
        if not ctx.inFunction():
          ctx.errSkipStmtNoBackup("RetOutOfFunc")
        else:
          result.addKid(ctx.returnStmt())
        continue
      of TtVar:
        result.addKid(ctx.varStmt())
        continue
      of TtGlobal:
        result.addKid(ctx.globalStmt())
        continue
      of TtConst:
        result.addKid(ctx.constStmt())
        continue
      of TtIdentifier:
        if ctx.lookahead().kind == TtColon and
           ctx.lookahead(2).kind in [TtFor, TtWhile]:
          result.addKid(ctx.labelStmt())
          continue

        case ctx.curTok.getText()
        of "use":
          result.addKid(ctx.useStmt())
          continue
        of "parameter":
          ctx.errSkipStmtNoBackup("TopLevelPlural",
                                  @["<em>'parameter'</em> blocks"])
        of "extern":
          ctx.errSkipStmtNoBackup("TopLevelPlural",
                                  @["<em>'extern'</em> blocks"])
        else:
          discard
      else:
        discard

      let x = ctx.expression()
      case ctx.curKind()
      of TtColon, TtAssign:
        result.addKid(ctx.assign(x))
        continue
      of TtIdentifier, TtStringLit, TtLBrace:
        result.addKid(ctx.section(x))
        continue
      else:
        result.addKid(x)
        ctx.endOfStatement()
        continue
    except:
      if getCurrentException().msg == "BAIL":
        raise
      while true:
        if ctx.atEndOfLine() and ctx.curKind() notin [TtRBrace, TtRParen]:
          ctx.advance()
          break
        ctx.advance()

production(topLevel, NodeModule):
  # Instead of being under token 1, we really want Module to be bound to
  # the start-of-stream token.
  result.token = ctx.tokens[0]

  while true:
    try:
      case ctx.curKind()
      of TtEof, ErrorTok:
        return
      of TtSemi, TtNewLine:
        ctx.advance()
        continue
      of TtEnum:
        result.addKid(ctx.enumStmt())
        continue
      of TtLockAttr:
        result.addKid(ctx.lockAttr())
        continue
      of TtIf:
        result.addKid(ctx.ifStmt())
        continue
      of TtFor:
        result.addKid(ctx.forStmt())
        continue
      of TtWhile:
        result.addKid(ctx.whileStmt())
        continue
      of TtTypeOf:
        result.addKid(ctx.typeOfStmt())
        continue
      of TtValueOf:
        result.addKid(ctx.valueOfStmt())
        continue
      of TtContinue:
        ctx.errSkipStmt("InLoopsOnly", @["continue"])
      of TtBreak:
        ctx.errSkipStmt("InLoopsOnly", @["break"])
      of TtReturn:
        ctx.errSkipStmt("RetOutOfFunc")
      of TtFunc:
        result.addKid(ctx.funcDef())
        continue
      of TtVar:
        result.addKid(ctx.varStmt())
        continue
      of TtGlobal:
        result.addKid(ctx.globalStmt())
        continue
      of TtConst:
        result.addKid(ctx.constStmt())
        continue
      of TtIdentifier:
        if ctx.lookahead().kind == TtColon and
           ctx.lookahead(2).kind in [TtFor, TtWhile]:
          result.addKid(ctx.labelStmt())
          continue
        case ctx.curTok().getText()
        of "assert":
          result.addKid(ctx.assertStmt())
          continue
        of "use":
          result.addKid(ctx.useStmt())
          continue
        of "parameter":
          result.addKid(ctx.parameterBlock())
          continue
        of "extern":
          result.addKid(ctx.externBlock())
          continue
        else:
          discard
      else:
        discard
      let x = ctx.expression()
      case ctx.curKind()
      of TtColon, TtAssign:
        result.addKid(ctx.assign(x))
        continue
      of TtIdentifier, TtStringLit, TtLBrace:
        result.addKid(ctx.section(x))
        continue
      else:
        result.addKid(x)
        ctx.endOfStatement()
        continue
    except:
      if getCurrentException().msg == "BAIL":
        return
      while true:
        if ctx.atEndOfLine() and ctx.curKind() notin [TtRBrace, TtRParen]:
          ctx.advance()
          break
        ctx.advance()

proc buildType*(n: Con4mNode, tvars: var Dict[string, TypeId]): TypeId =
  ## If error, you need to fill in the module.
  case n.kind
  of NodeTypeBuiltin:
    let biname = n.children[0].getText()
    return biname.idFromTypeName()
  of NodeType:
    return n.children[0].buildType(tvars)
  of NodeTypeList:
    return tList(n.children[0].buildType(tvars))
  of NodeTypeDict:
    return tDict(n.children[0].buildType(tvars),
                 n.children[1].buildType(tvars))
  of NodeTypeTuple:
    var items: seq[TypeId]
    for kid in n.children:
      items.add(kid.buildType(tvars))
    return tTuple(items)
  of NodeTypeObj:
    var
      props: Dict[string, TypeId]
      name:  string

    props.initDict()
    if n.children[0].kind == NodeIdentifier:
      name = n.children[0].getText()
    return tStruct(props, name)
  of NodeTypeVar:
    let varname = n.children[0].getText()
    let objOpt = tvars.lookup(varname)
    if objOpt.isSome():
      return objOpt.get()
    let tobj = newTypeVar()
    tobj.localName = some(varname)
    tvars[varname] = tobj.typeId
    result = tobj.typeId
  of NodeTypeRef:
    return tRef(n.children[0].buildType(tvars))
  of NodeTypeTypeSpec:
    if n.children.len() == 0:
      return tTypeSpec()
    else:
      return tTypeSpec(n.children[0].buildType(tvars))
  of NodeTypeFunc:
    var
      items: seq[TypeId]
      va:    bool = false

    for kid in n.children:
      if kid.kind == NodeTypeVararg:
        va = true
        items.add(kid.children[0].buildType(tvars))
      elif kid.kind == NodeReturnType:
        items.add(kid.children[0].buildType(tvars))
      else:
        items.add(kid.buildType(tvars))
    return newFuncType(items, va).typeId
  of NodeTypeOneOf:
    var items: seq[TypeId]
    for kid in n.children:
      items.add(kid.buildType(tvars))

    return tOneOf(items)
  of NodeTypeMaybe:
    return tMaybe(n.children[0].buildType(tvars))
  else:
    var errs: seq[Con4mError]

    errs.baseError("BadTypeDecl", n, "", ErrIrGen)
    print errs.formatErrors()
    raise newCon4mException(errs)

proc buildType*(n: Con4mNode): TypeId =
  ## Take a parse tree containing type information, and yield a type
  ## indicator (TypeId)
  var tvars: Dict[string, TypeId]

  tvars.initDict()
  return n.buildType(tvars)

proc parseType*(s: string): TypeId =
  var
    errs:  seq[Con4mError]
    tvars: Dict[string, TypeId]

  tvars.initDict()

  let tree = parseTypeSpec(s, errs)

  if len(errs) != 0:
    raise newCon4mException(errs)

  return buildType(tree, tvars)
