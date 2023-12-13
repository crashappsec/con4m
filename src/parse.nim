## This is a recursive simple descent parser.  Note that I've explicitly
## factored the grammar for right recursion, so in the expression grammar
## there is a bit of tree jockeying to get the tree to look natural.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import lex, basetypes
export lex, basetypes

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
  of NodeParamBlock:     toPrint = n.ropeNt("ParamBlock")
  of NodeAttrAssign:     toPrint = n.ropeNt("AttrAssign")
  of NodeAttrSetLock:    toPrint = n.ropeNt("AttrSetLock")
  of NodeVarAssign:      toPrint = n.ropeNt("VarAssign")
  of NodeUnpack:         toPrint = n.ropeNt("Unpack")
  of NodeSection:        toPrint = n.ropeNt("Section")
  of NodeIfStmt:         toPrint = n.ropeNt("If")
  of NodeWhileStmt:      toPrint = n.ropeNt("While")
  of NodeElifStmt:       toPrint = n.ropeNt("Elif")
  of NodeElseStmt:       toPrint = n.ropeNt("Else")
  of NodeForStmt:        toPrint = n.ropeNt("For")
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
  of NodeUseStmt:        toPrint = n.ropeNt("UseStmt")
  of NodeLabelStmt:      toPrint = n.ropeNt("LabelStmt")
  of NodeExpression:     toPrint = n.ropeNt("Expression")
  of NodeTypeOneOf:      toPrint = n.ropeNt("OneOf")
  of NodeTypeMaybe:      toPrint = n.ropeNt("Maybe")
  of NodeBreakStmt:      toPrint = n.ropeT("Break")
  of NodeContinueStmt:   toPrint = n.ropeT("Continue")
  of NodeVarargsFormal:  toPrint = n.ropeNt("Varargs")
  of NodeCallbackLit:    toPrint = n.ropeNtNamed("CallbackLit")
  of NodeStringLit:      toPrint = n.ropeNtNamed("String")
  of NodeIntLit:         toPrint = n.ropeNtNamed("Int")
  of NodeHexLit:         toPrint = n.ropeNtNamed("Hex")
  of NodeFloatLit:       toPrint = n.ropeNtNamed("Float")
  of NodeBoolLit:        toPrint = n.ropeNtNamed("Bool")
  of NodeFuncDef:        toPrint = n.ropeNtNamed("Def")

  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    toPrint = n.ropeNt($(n.token))
  of NodeIdentifier:   toPrint = n.ropeNtNamed("Identifier")

  result = (toPrint, n.children)

proc toRope*(n: Con4mNode): Rope =
  return n.quickTree(ropeWalker)

proc getText*(token: Con4mToken, adjust: static[bool] = false): string =
  when adjust:
    return $(token)
  else:
      if token.kind == TtStringLit: return token.unescaped
      else:                         return $(token)

proc getText*(node: Con4mNode, adjust: static[bool] = false): string =
  ## This returns the raw string associated with a token.  Internal.
  return node.token.getText(adjust)

proc `$`*(n: Con4mNode): string =
  case n.kind
  of NodeModule:         "a module"
  of NodeBody:           "a block of statements"
  of NodeParamBlock:     "a parameter block"
  of NodeAttrAssign:     "an assignment"
  of NodeAttrSetLock:    "a lock operation"
  of NodeVarAssign:      "a variable assignment"
  of NodeUnpack:         "an unpack operation"
  of NodeSection:        "a section declaration"
  of NodeIfStmt:         "an <em>if</em> block"
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
  of NodeOtherLit:       "a con4m special literal"
  of NodeCharLit:        "a character literal"
  of NodeLiteral:        "a literal"
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
  of NodeFuncDef:        "a <em>function</em> declaration"

  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    ("the <em>" & $(n.getText()) & "</em> operator")

  of NodeIdentifier:
    "an identifier <em>(" & $(n.getText()) & ")</em>"

proc addKid*(parent: Con4mNode, kid: Con4mNode) =
  parent.children.add(kid)
  kid.parent = parent

template tokAt*(ctx: var CompileCtx, i: int): Con4mToken =
  ctx.tokens[i]

# Prototypes for things where we need the forward reference.
proc body(ctx: var CompileCtx): Con4mNode
proc optionalBody(ctx: var CompileCtx): Con4mNode
proc typeSpec(ctx: var CompileCtx): Con4mNode
proc expressionStart(ctx: var CompileCtx): Con4mNode
proc notExpr(ctx: var CompileCtx): Option[Con4mNode]
proc accessExpr(ctx: var CompileCtx): Con4mNode

proc inFunction(ctx: var CompileCtx): bool {.inline.} =
  return ctx.inFunc

proc inLoop(ctx: var CompileCtx): bool {.inline.} =
  return ctx.loopDepth != 0

proc curTok(ctx: var CompileCtx): Con4mToken

const stmtStartList = [NodeAttrAssign, NodeAttrSetLock, NodeVarAssign,
                       NodeUnpack, NodeSection, NodeIfStmt, NodeElifStmt,
                       NodeElseStmt, NodeForStmt, NodeWhileStmt,
                       NodeBreakStmt]

proc newNode(ctx: var CompileCtx, kind: Con4mNodeKind): Con4mNode =
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
proc unconsume(ctx: var CompileCtx) =
  while true:
    ctx.curTokIx -= 1
    if not alwaysSkip.contains(ctx.curTok().kind):
      return

proc parseBaseError*(ctx: var CompileCtx, code: string, backup: bool,
                     warn: bool, subs: seq[string], st: string,
                    ii: Option[InstantiationInfo]) =
  if backup:
    ctx.unconsume()

  let tok = ctx.curTok()
  let sev = if warn: LlWarn else: LlErr

  ctx.errors.baseError(code, tok, ctx.module, ErrParse, sev, subs, st, ii)

template parseError*(ctx: var CompileCtx, msg: string,
                    extra: seq[string] = @[]) =
  when defined(debug):
    ctx.parseBaseError(msg, true, false, extra, getStackTrace(),
                       some(instantiationInfo()))
  else:
    ctx.parseBaseError(msg, true, false, extra, "", none(InstantiationInfo))

template parseErrorNoBackup*(ctx: var CompileCtx, msg: string,
                            extra: seq[string] = @[]) =
  when defined(debug):
    ctx.parseBaseError(msg, false, false, extra, getStackTrace(),
                       some(instantiationInfo()))
  else:
    ctx.parseBaseError(msg, false, false, extra, "", none(InstantiationInfo))

template errSkipStmt*(ctx: var CompileCtx, msg: string,
                     extra: seq[string] = @[]) =
  ctx.parseError(msg, extra)
  con4mLongJmp()

template errSkipStmtNoBackup*(ctx: var CompileCtx, msg: string,
                             extra: seq[string] = @[]) =
  ctx.parseErrorNoBackup(msg, extra)
  con4mLongJmp()

template production(prodName: untyped,
                    nodeType: Con4mNodeKind, prodImpl: untyped) {.dirty.} =
  proc `prodName`(ctx: var CompileCtx): Con4mNode =
    result = ctx.newNode(nodeType)
    prodImpl

  proc `parse prodName`*(ctx: var CompileCtx): bool =
    if ctx.tokens.len() == 0:
      if not ctx.lex():
        return false

    ctx.root = ctx.`prodName`()

    return ctx.errors.canProceed()

  proc `parse prodName`*(s: string, errs: var seq[Con4mError],
                         module = ""): Con4mNode =
    var ctx: CompileCtx
    ctx.s      = s.newStringCursor()
    ctx.module = module

    if ctx.`parse prodName`():
      result =  ctx.root

    errs = ctx.errors

template exprProd(prodName, rhsName, chainNext, tokenType, nodeType: untyped) =
  proc prodName(ctx: var CompileCtx): Option[Con4mNode]

  proc rhsName(ctx: var CompileCtx): Con4mNode =
    var n = ctx.expressionStart()
    while true:
      let optExpr = ctx.prodName()
      if optExpr.isSome():
        var r = optExpr.get()
        if len(r.children) == 0:
          ctx.errSkipStmt("ExprStart", @[$(r.token)])
        case len(n.children)
        of 0:
          if r.kind notin [NodePlus, NodeMinus, NodeNot]:
            ctx.errSkipStmt("ExprStart", @[$(r.token)])
          n = r
        else:
          if r.kind == NodeNot:
            ctx.errSkipStmt("BinaryNot")
          r.children = @[n, r.children[0]]
          n = r
      else:
        return n

  proc prodName(ctx: var CompileCtx): Option[Con4mNode] =
    var res: Con4mNode
    if ctx.curKind() == tokenType:
      res = ctx.newNode(nodeType)
      ctx.advance()
      res.addKid(ctx.rhsName())
      result = some(res)
    else:
      result = ctx.chainNext()

template errBail(ctx: var CompileCtx, msg: string) =
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

const ignore1Nl = [TtPlus, TtMinus, TtMul, TtDiv, TtMod, TtLte, TtLt, TtGte,
                   TtGt, TtNeq, TtNot, TtLocalAssign, TtColon, TtAttrAssign,
                   TtCmp, TtComma, TtPeriod, TtLBrace, TtLBracket,
                   TtLParen, TtAnd, TtOr, TTFrom, TtTo, TtArrow]

const commentToks = [TtLineComment, TtLongComment]


proc doNewlineSkipping(ctx: var CompileCtx) =
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

template skipNextNewline(ctx: var CompileCtx) =
  ctx.skipOneNewline = true

proc skipOneNewlineIfAppropriate(ctx: var CompileCtx) =

  if ctx.skipOneNewline:
    ctx.doNewlineSkipping()
    ctx.skipOneNewline = false
    return

  if ctx.curTokIx == 0:
    return
  if ctx.literalDepth == 0 and ctx.tokAt(ctx.curTokIx - 1).kind notin ignore1Nl:
    return

  ctx.doNewlineSkipping()

proc curTok(ctx: var CompileCtx): Con4mToken =
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

  return ctx.tokAt(ctx.curTokIx)

template curKind(ctx: var CompileCtx): Con4mTokenKind =
  ctx.curTok().kind

proc consume(ctx: var CompileCtx): Con4mToken {.inline.} =
  result = ctx.curTok()
  ctx.curTokIx += 1

proc advance(ctx: var CompileCtx) {.inline.} =
  discard ctx.curTok()
  ctx.curTokIx += 1

proc ignoreAllNewlines(ctx: var CompileCtx) =
  while ctx.curKind() == TtNewline:
    ctx.advance()

proc lookAhead(ctx: var CompileCtx, numToks: int = 1): Con4mToken =
  let cur = ctx.curTokIx
  var n = numToks

  while n != 0:
    ctx.advance()
    n = n - 1

  result = ctx.curTok()
  ctx.curTokIx = cur

proc atEndOfLine(ctx: var CompileCtx): bool =
  let kind = ctx.curKind()
  if kind in [TtSemi, TtNewLine, TtRBrace, TtRParen, TtEOF]:
    return true

proc describeLastNode(ctx: var CompileCtx): string =
  var n = ctx.prevNode
  if n.kind == NodeIdentifier and
     n.parent.kind in [NodeLiteral, NodeTypeBuiltin, NodeTypeVar,
                       NodeTypeMaybe, NodeTypeOneOf]:
    n = n.parent

  return $(n)

proc endOfStatement(ctx: var CompileCtx, errIfNotThere = true) =
  if errIfNotThere and not ctx.atEndOfLine():
    ctx.errSkipStmtNoBackup("StmtEnd", @[ctx.describeLastNode()])
  else:
    while not ctx.atEndOfLine():
      ctx.advance() # Skip errors.
  while ctx.curKind() in [TtSemi, TtNewline]:
    ctx.advance()

proc expectOrErrConsuming(ctx: var CompileCtx, kind: Con4mTokenKind) =
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

proc expectOrErr(ctx: var CompileCtx, kind: Con4mTokenKind) =
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

template expect(ctx: var CompileCtx, kind: Con4mTokenKind, consume = false) =
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

exprProd(divExpr,   divExprRHS,     notExpr,   TtDiv,   NodeDiv)
exprProd(mulExpr,   mulExprRHS,     divExpr,   TtMul,   NodeMul)
exprProd(modExpr,   modExprRHS,     mulExpr,   TtMod,   NodeMod)
exprProd(minusExpr, minusExprRHS,   modExpr,   TtMinus, NodeMinus)
exprProd(plusExpr,  plusExprRHS,    minusExpr, TtPlus,  NodePlus)
exprProd(ltExpr,    ltExprRHS,      plusExpr , TtLt,    NodeLt)
exprProd(gtExpr,    gtExprRHS,      ltExpr,    TtGt,    NodeGt)
exprProd(lteExpr,   lteExprRHS,     gtExpr,    TtLte,   NodeLte)
exprProd(gteExpr,   gteExprRHS,     lteExpr,   TtGte,   NodeGte)
exprProd(eqExpr,    eqExprRHS,      gteExpr,   TtCmp,   NodeCmp)
exprProd(neExpr,    neExprRHS,      eqExpr,    TtNeq,   NodeNe)
exprProd(andExpr,   andExprRHS,     neExpr,    TtAnd,   NodeAnd)
exprProd(orExpr,    expression,     andExpr,   TtOr,    NodeOr)


production(identifier, NodeIdentifier):
  ctx.expect(TtIdentifier, consume = true)

proc memberExpr(ctx: var CompileCtx, lhs: Con4mNode): Con4mNode =
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

proc memberExpr(ctx: var CompileCtx): Con4mNode =
  # For use in contexts like parameters where we KNOW this can only be
  # an attribute.
  result = ctx.newNode(NodeMember)

  while true:
    result.addKid(ctx.identifier())
    if ctx.curKind() != TtPeriod:
      return
    ctx.advance()

proc indexExpr(ctx: var CompileCtx, lhs: Con4mNode): Con4mNode =
  result = ctx.newNode(NodeIndex)

  result.addKid(lhs)

  adtLiteral:
    ctx.advance()
    result.addKid(ctx.expression())
    if ctx.curKind == TtColon:
      ctx.advance()
      result.addKid(ctx.expression())
    ctx.expect(TtRBracket, consume = true)

proc callActuals(ctx: var CompileCtx, lhs: Con4mNode): Con4mNode =
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

proc notExprRHS(ctx: var CompileCtx): Con4mNode =
    var n = ctx.expressionStart()
    while true:
      let optExpr = ctx.divExpr()
      if optExpr.isSome():
        var r = optExpr.get()
        if len(r.children) == 0:
          ctx.errSkipStmt("ExprStart", @[$(r.token)])
        case len(n.children)
        of 0:
          if r.kind notin [NodePlus, NodeMinus, NodeNot]:
            ctx.errSkipStmt("ExprStart", @[$(r.token)])
        else:
          if r.kind == NodeNot:
            ctx.errSkipStmt("BinaryNot")
          r.children = @[n, r.children[0]]
          n = r
      else:
        return n

proc notExpr(ctx: var CompileCtx): Option[Con4mNode] =
  case ctx.curKind()
  of TtNot:
    var res = ctx.newNode(NodeNot)
    ctx.advance()
    res.addKid(ctx.notExprRHS())
    return some(res)
  of TtIdentifier:
    print getStackTrace()
    result = some(ctx.accessExpr())
    print result.get().toRope()
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

    if txt in getAllTypeIdentifiers():
      result.addKid(ctx.typeSpec())
    else:
      ctx.errSkipStmt("LitExpected")
  else:
    ctx.errSkipStmt("LitExpected")

production(expressionStart, NodeExpression):
  case ctx.curKind()
  of TtIntLit, TtHexLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse,
     TtLBrace, TtLBracket, TtLParen, TtOtherLit, TtFunc, TtBacktick, TtObject:
       result.addKid(ctx.literal())
  of TtIdentifier:
    let txt = ctx.curTok().getText()
    if txt in getAllTypeIdentifiers():
      result.addKid(ctx.literal())
    else:
      result.addKid(ctx.accessExpr())
  of TtPlus, TtMinus, TtNot:
    # empty string for the LHS; For TtPlus and TtMinus, the proper
    # production below us will need to tease out whether it's a unary
    # or binary. And TtNot needs to reject when there's a lhs.
    discard
  else:
    ctx.errSkipStmtNoBackup("ExprStart", @[$ctx.curTok()])

production(enumItem, NodeEnumItem):
  result.addKid(ctx.identifier())
  if ctx.curKind() in [TtLocalAssign, TtAttrAssign, TtColon]:
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

production(attrAssign, NodeAttrAssign):
  var child = ctx.identifier()

  if ctx.curKind() == TtPeriod:
    child = ctx.memberExpr(child)

  let op = ctx.consume()
  case op.kind
  of TtAttrAssign, TtColon:
    result.token = op
  else:
    ctx.errSkipStmt("NotAttrAssign")

  result.addKid(child)
  result.addKid(ctx.expression())

  ctx.endOfStatement()

production(lockAttr, NodeAttrSetLock):
  ctx.advance()
  if ctx.curKind() != TtIdentifier:
    ctx.errSkipStmt("BadLock")
  result.addKid(ctx.attrAssign())

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
    result.addKid(ctx.expression()) # 2
  else:
    if result.children[0].children.len() != 1:
      ctx.errSkipStmt("ForFromIx")

    ctx.expect(TtFrom, consume = true)
    result.addKid(ctx.expression()) # 2
    ctx.expect(TtTo, consume = true)
    result.addKid(ctx.expression()) # 3
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
  ctx.advance()
  result.addKid(ctx.varSymInfo())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.varSymInfo())
  ctx.endOfStatement()

production(globalStmt, NodeGlobalStmt):
  ctx.advance()
  result.addKid(ctx.varSymInfo())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.varSymInfo())
  ctx.endOfStatement()

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
  ctx.advance()
  result.addKid(ctx.identifier())
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

production(varAssign, NodeVarAssign):
  result.addKid(ctx.identifier())
  # The := has already been validated by the caller checking lookahead.
  ctx.advance()
  result.addKid(ctx.expression())
  ctx.endOfStatement()

production(unpack, NodeUnpack):
  result.addKid(ctx.identifier())
  while ctx.curKind() == TtComma:
    ctx.advance()
    result.addKid(ctx.identifier)

  ctx.expect(TtLocalAssign, consume = true)
  result.addKid(ctx.expression())
  ctx.endOfStatement()

production(optionalBody, NodeBody):
  if ctx.curKind() == TtLBrace:
    result = ctx.body()
  else:
    ctx.endOfStatement()

production(section, NodeSection):
  result.addKid(ctx.identifier())

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
      of TtEnum, TtFunc:
        ctx.errSkipStmtNoBackup("TopLevelOnly", @[$(ctx.curKind())])
      of TtLockAttr:
        result.addKid(ctx.attrAssign())
      of TtIf:
        result.addKid(ctx.ifStmt())
      of TtFor:
        result.addKid(ctx.forStmt())
      of TtWhile:
        result.addKid(ctx.whileStmt())
      of TtContinue:
        if not ctx.inLoop():
          ctx.errSkipStmtNoBackup("InLoopsOnly", @["continue"])
        else:
          result.addKid(ctx.continueStmt())
      of TtBreak:
        if not ctx.inLoop():
          ctx.errSkipStmtNoBackup("InLoopsOnly", @["break"])
        else:
          result.addKid(ctx.breakStmt())
      of TtReturn:
        if not ctx.inFunction():
          ctx.errSkipStmtNoBackup("RetOutOfFunc")
        else:
          result.addKid(ctx.returnStmt())
      of TtVar:
        result.addKid(ctx.varStmt())
      of TtGlobal:
        result.addKid(ctx.globalStmt())
      of TtIdentifier:
        case ctx.curTok.getText()
        of "label":
          result.addKid(ctx.labelStmt())
        of "use":
          result.addKid(ctx.useStmt())
          continue
        of "parameter":
          result.addKid(ctx.parameterBlock())
          continue
        else:
          discard

        case ctx.lookAhead().kind
        of TtAttrAssign, TtColon, TtPeriod:
          result.addKid(ctx.attrAssign())
          continue
        of TtLocalAssign:
          result.addKid(ctx.varAssign())
          continue
        of TtComma:
          result.addKid(ctx.unpack())
        of TtIdentifier, TtStringLit, TtLBrace:
          result.addKid(ctx.section())
        else:
          result.addKid(ctx.expression())
          ctx.endOfStatement()
      else:
        result.addKid(ctx.expression())
        ctx.endOfStatement()
    except:
      if getCurrentException().msg == "BAIL":
        raise
      while true:
        if ctx.atEndOfLine() and ctx.curKind() notin [TtRBrace, TtRParen]:
          ctx.advance()
          break
        ctx.advance()

production(module, NodeModule):
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
      of TtEnum:
        result.addKid(ctx.enumStmt())
      of TtLockAttr:
        result.addKid(ctx.attrAssign())
      of TtIf:
        result.addKid(ctx.ifStmt())
      of TtFor:
        result.addKid(ctx.forStmt())
      of TtWhile:
        result.addKid(ctx.whileStmt())
      of TtContinue:
        ctx.errSkipStmt("InLoopsOnly", @["continue"])
      of TtBreak:
        ctx.errSkipStmt("InLoopsOnly", @["break"])
      of TtReturn:
        ctx.errSkipStmt("RetOutOfFunc")
      of TtFunc:
        result.addKid(ctx.funcDef())
      of TtVar:
        result.addKid(ctx.varStmt())
      of TtGlobal:
        result.addKid(ctx.globalStmt())
      of TtIdentifier:
        case ctx.curTok.getText()
        of "label":
          result.addKid(ctx.labelStmt())
        of "use":
          result.addKid(ctx.useStmt())
          continue
        of "parameter":
          result.addKid(ctx.parameterBlock())
          continue
        else:
          discard
        case ctx.lookAhead().kind
        of TtAttrAssign, TtColon, TtPeriod:
          result.addKid(ctx.attrAssign())
          continue
        of TtLocalAssign, TtComma:
          result.addKid(ctx.varAssign())
          continue
        of TtIdentifier, TtStringLit, TtLBrace:
          result.addKid(ctx.section())
        else:
          result.addKid(ctx.expression())
          ctx.endOfStatement()
      else:
          result.addKid(ctx.expression())
          ctx.endOfStatement()
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
