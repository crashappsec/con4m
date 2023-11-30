## This is a simple recursive descent parser.  Note that I've explicitly
## factored the grammar for right recursion, so in the expression grammar
## there is a bit of tree jockeying to get the tree to look natural.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import nimutils, lex, options, types, unicode, strcursor

proc getAllBuiltinTypeNames*(): auto = 
  return ["bool", "int", "char", "float", "string", "void", "Duration",
          "Ip", "Cidr", "Size", "Date", "Time", "DateTime", "Path"]

const allTypeIdentifiers = ["bool", "int", "char", "float", "string", "void", 
                            "Duration", "Ip", "Cidr", "Size", "Date", "Time",
                            "DateTime", "Path", "list", "dict", "tuple", 
                            "struct", "ref"]

proc ropeNt(n: Con4mNode, name: string): Rope =
  result = atom(name).italic().fgColor("atomiclime")

proc ropeT(n: Con4mNode, name: string): Rope =
  return atom(name).fgColor("fandango")

proc ropeNtNamed(n: Con4mNode, name: string): Rope =
  result = atom(name).italic().fgColor("atomiclime") + atom(" ") + 
           strong($n.token.get())

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
  of NodeBreakStmt:      toPrint = n.ropeT("Break")
  of NodeContinueStmt:   toPrint = n.ropeT("Continue")
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
  of NodeCallbackLit:    toPrint = n.ropeNtNamed("CallbackLit")
  of NodeNoCallbackName: toPrint = n.ropeNtNamed("NoCallbackName")
  of NodeStringLit:      toPrint = n.ropeNtNamed("String")
  of NodeIntLit:         toPrint = n.ropeNtNamed("Int")
  of NodeFloatLit:       toPrint = n.ropeNtNamed("Float")
  of NodeBoolLit:        toPrint = n.ropeNtNamed("Bool")
  of NodeOtherLit:       toPrint = n.ropeNt("Other")
  of NodeCharLit:        toPrint = n.ropeNt("Char")
  of NodeLiteral:        toPrint = n.ropeNt("Literal")
  of NodeEnumStmt:       toPrint = n.ropeNt("Enum")
  of NodeFuncDef:        toPrint = n.ropeNtNamed("Def")
  of NodeFormalList:     toPrint = n.ropeNt("Formals")
  of NodeType:           toPrint = n.ropeNt("Type")
  of NodeTypeSpec:       toPrint = n.ropeNt("TypeSpec")
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
  of NodeExportStmt:     toPrint = n.ropeNt("ExportStmt")
  of NodeVarSymNames:    toPrint = n.ropeNt("VarSymNames")
  of NodeUseStmt:        toPrint = n.ropeNt("UseStmt")
  of NodeExpression:     toPrint = n.ropeNt("Expression")
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    toPrint = n.ropeNt($(n.token.get()))
  of NodeIdentifier:   toPrint = n.ropeNtNamed("Identifier")

  result = (toPrint, n.children)

proc toRope*(n: Con4mNode): Rope =
  return n.quickTree(ropeWalker)

proc getTokenText*(token: Con4mToken, adjust: static[bool] = false): string =
  when adjust:
    return $(token)
  else:
      if token.kind == TtStringLit: return token.unescaped
      else:                         return $(token)

proc getTokenText*(node: Con4mNode, adjust: static[bool] = false): string =
  ## This returns the raw string associated with a token.  Internal.
  return node.token.get().getTokenText(adjust)

template addKid*(parent: Con4mNode, kid: Con4mNode) =
  parent.children.add(kid)

type 
  Con4mErrType* = enum ErrParse
  Con4mError* = object
    kind*:  Con4mErrType
    msg*:   Rope
    token*: Con4mToken
    node*:  Con4mNode

    when not defined(release):
      st:  string
      ii:  tuple[filename: string, line: int, column: int]

  ParseCtx* = object
    tokenBox*:       TokenBox
    curTokIx*:       int
    prevTokIx*:      int
    curNodeId*:      int
    nesting*:        int
    skipOneNewLine*: bool
    literalDepth*:   int
    errors*:         seq[Con4mError]
    inFunc*:         bool
    loopDepth*:      int

template tokAt*(ctx: var ParseCtx, i: int): Con4mToken =
  ctx.tokenBox.tokAt(i)

# Prototypes for things where we need the forward reference.
proc body(ctx: var ParseCtx): Con4mNode
proc optionalBody(ctx: var ParseCtx): Con4mNode
proc typeSpec(ctx: var ParseCtx): Con4mNode
proc expressionStart(ctx: var ParseCtx): Con4mNode
proc notExpr(ctx: var ParseCtx): Option[Con4mNode]

proc inFunction(ctx: var ParseCtx): bool {.inline.} =
  return ctx.inFunc

proc inLoop(ctx: var ParseCtx): bool {.inline.} =
  return ctx.loopDepth != 0

proc curTok(ctx: var ParseCtx): Con4mToken

proc newNode(ctx: var ParseCtx, kind: Con4mNodeKind): Con4mNode =
  ctx.curNodeId += 1

  result = Con4mNode(id: ctx.curNodeId, kind: kind, depth: ctx.nesting,
                     prevTokenIx: ctx.curTokIx, allTokens: ctx.tokenBox)
  result.token     = some(ctx.curTok())

const alwaysSkip = [TtSof, TtWhiteSpace, TtLineComment, TtLongComment]

# When outputting errors: return "" we might want to back up a token.
# need to skip ws when doing that.
proc unconsume(ctx: var ParseCtx) =
  while true:
    ctx.curTokIx -= 1
    if not alwaysSkip.contains(ctx.curTok().kind): 
      return

proc storeParseError(ctx: var ParseCtx, msg: Rope, backup: bool, 
                     ii: tuple[filename: string, line: int, column: int],
                     st: string) =
  var err = Con4mError(kind: ErrParse, msg: msg)
  
  when not defined(release):
    err.ii = ii
    err.st = st

  if backup:
    ctx.unconsume()
  
  err.token = ctx.curTok()
  ctx.errors.add(err)
  if backup:
    ctx.curTokIx += 1

template parseError(ctx: var ParseCtx, msg: Rope, backup: bool = true) =
  ctx.storeParseError(msg, backup, instantiationInfo(), getStackTrace())

template parseError(ctx: var ParseCtx, msg: string, backup: bool = true) =
  ctx.storeParseError(atom(msg), backup, instantiationInfo(), getStackTrace())

template errSkipStmt(ctx: var ParseCtx, msg: string | Rope, backup = true) =
  ctx.parseError(msg, backup)
  raise newException(ValueError, "STMT")

template production(prodName: untyped, 
                    nodeType: Con4mNodeKind, prodImpl: untyped) {.dirty.} =
  proc `prodName`(ctx: var ParseCtx): Con4mNode =
    result = ctx.newNode(nodeType)
    prodImpl

  proc `parse prodName`*(toks: var TokenBox,
                         errs: var seq[Con4mError]): Con4mNode =
      for tok in toks.tokens:
        if tok.kind in [ErrorTok, ErrorLongComment, ErrorStringLit, 
                        ErrorCharLit, ErrorOtherLit]:
          raise newException(ValueError, "Cannot parse; tokenization failed.")

      var ctx = ParseCtx(tokenBox: toks)

      try:
        result = ctx.`prodName`()
      finally:
        errs = ctx.errors

template exprProd(prodName, rhsName, chainNext, tokenType, nodeType: untyped) =
  proc prodName(ctx: var ParseCtx): Option[Con4mNode]

  proc rhsName(ctx: var ParseCtx): Con4mNode =
    var n = ctx.expressionStart()
    while true:
      let optExpr = ctx.prodName()
      if optExpr.isSome():
        var r = optExpr.get()
        if len(r.children) == 0:
          ctx.errSkipStmt("Invalid expression start")
        case len(n.children)
        of 0:
          if r.kind notin [NodePlus, NodeMinus, NodeNot]:
            ctx.errSkipStmt("Invalid expression")
          n = r
        else:
          if r.kind == NodeNot:
            ctx.errSkipStmt(em("not") + atom(" does not take a left-hand " &
              "operand."))
          r.children = @[n, r.children[0]]
          n = r
      else:
        return n

  proc prodName(ctx: var ParseCtx): Option[Con4mNode] =
    var res: Con4mNode
    if ctx.curKind() == tokenType:
      res = ctx.newNode(nodeType)
      ctx.advance()
      res.addKid(ctx.rhsName())
      result = some(res)
    else:
      result = ctx.chainNext() 

template errBail(ctx: var ParseCtx, msg: string | Rope, backup = false) =
  ## When re-syncing is too likely to be error-prone, we bail from
  ## the rest of the parse.
  ctx.parseError(msg, backup)
  raise newException(ValueError, "BAIL")

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


proc doNewlineSkipping(ctx: var ParseCtx) =
  var stopAtNextNewline = false
  while true:
    while ctx.tokenBox.tokens[ctx.curTokIx].kind in alwaysSkip:
      if ctx.tokenBox.tokens[ctx.curTokIx].kind in commentToks:
        stopAtNextNewLine = false
      ctx.curTokIx += 1
      
    if ctx.tokenBox.tokens[ctx.curTokIx].kind == TtNewline:
      ctx.curTokIx += 1
      if stopAtNextNewline:
        return
      else:
        stopAtNextNewline = true
    else:
      return
    

template skipNextNewline(ctx: var ParseCtx) =
  ctx.skipOneNewline = true

proc skipOneNewlineIfAppropriate(ctx: var ParseCtx) =

  if ctx.skipOneNewline:
    ctx.doNewlineSkipping()
    ctx.skipOneNewline = false
    return 

  if ctx.curTokIx == 0:
    return 
  if ctx.literalDepth == 0 and ctx.tokAt(ctx.curTokIx - 1).kind notin ignore1Nl:
    return 

  ctx.doNewlineSkipping()

proc curTok(ctx: var ParseCtx): Con4mToken =
  # when a token is consumed, we simply advance the index past it, and
  # then leave it to the next call to curTok() to advance.
  # First, we look at that last token; if it allows us to 
  # skip one new line, we do so.
  #
  # We're also allowed to skip _one_ newline if we're in a literal,
  # after ANY token whatsoever.

  ctx.skipOneNewlineIfAppropriate()
  while ctx.tokAt(ctx.curTokIx).kind in alwaysSkip:
    ctx.curTokIx += 1
  return ctx.tokAt(ctx.curTokIx)

template curKind(ctx: var ParseCtx): Con4mTokenKind =
  ctx.curTok().kind

proc consume(ctx: var ParseCtx): Con4mToken {.inline.} =
  result = ctx.curTok()
  ctx.curTokIx += 1

proc advance(ctx: var ParseCtx) {.inline.} =
  discard ctx.curTok()
  ctx.curTokIx += 1

proc ignoreAllNewlines(ctx: var ParseCtx) =
  while ctx.curKind() == TtNewline:
    ctx.advance()

proc lookAhead(ctx: var ParseCtx, numToks: int = 1): Con4mToken =
  let cur = ctx.curTokIx
  var n = numToks

  while n != 0:
    ctx.advance()
    n = n - 1

  result = ctx.curTok()
  ctx.curTokIx = cur

proc atEndOfLine(ctx: var ParseCtx): bool =
  let kind = ctx.curKind()
  if kind in [TtSemi, TtNewLine, TtRBrace, TtRParen, TtEOF]:
    return true

proc endOfStatement(ctx: var ParseCtx, errIfNotThere = true) =
  if errIfNotThere and not ctx.atEndOfLine():
    ctx.errSkipStmt("Expected end of statement here.", false)
  else:
    while not ctx.atEndOfLine():
      ctx.advance() # Skip errors.
  while ctx.curKind() in [TtSemi, TtNewline]:    
    ctx.advance()

proc expectOrErrConsuming(ctx: var ParseCtx, kind: Con4mTokenKind) =
  let tok = ctx.consume()
  if tok.kind != kind:
    echo getStackTrace()
    ctx.errSkipStmt("Expected " & $(kind) & " here") # Error backs us up one.

proc expectOrErr(ctx: var ParseCtx, kind: Con4mTokenKind) =
  if ctx.curKind() != kind:
    echo getStackTrace()
    ctx.errSkipStmt("Expected " & $(kind) & " here", false) 

template expect(ctx: var ParseCtx, kind: Con4mTokenKind, consume = false) =
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

exprProd(divExpr,   divExprRHS,   notExpr,   TtDiv,   NodeDiv)
exprProd(mulExpr,   mulExprRHS,   divExpr,   TtMul,   NodeMul)
exprProd(modExpr,   modExprRHS,   mulExpr,   TtMod,   NodeMod)
exprProd(minusExpr, minusExprRHS, modExpr,   TtMinus, NodeMinus)
exprProd(plusExpr,  plusExprRHS,  minusExpr, TtPlus,  NodePlus)
exprProd(ltExpr,    ltExprRHS,    plusExpr , TtLt,    NodeLt)
exprProd(gtExpr,    gtExprRHS,    ltExpr,    TtGt,    NodeGt)
exprProd(lteExpr,   lteExprRHS,   gtExpr,    TtLte,   NodeLte)
exprProd(gteExpr,   gteExprRHS,   lteExpr,   TtGte,   NodeGte)
exprProd(eqExpr,    eqExprRHS,    gteExpr,   TtCmp,   NodeCmp)
exprProd(neExpr,    neExprRHS,    eqExpr,    TtNeq,   NodeNe)
exprProd(andExpr,   andExprRHS,   neExpr,    TtAnd,   NodeAnd)
exprProd(orExpr,    expression,   andExpr,   TtOr,    NodeOr)


production(identifier, NodeIdentifier):
  ctx.advance()

proc memberExpr(ctx: var ParseCtx, lhs: Con4mNode): Con4mNode =
  # For use from accessExpr, which peels off the first identifier.
  result = ctx.newNode(NodeMember)
  if lhs != Con4mNode(nil):
    result.addKid(lhs)

  while true:
    ctx.advance()
    if ctx.curKind() != TtIdentifier:
      ctx.errSkipStmt(em("'.'") + 
                      atom(" operator must have an identifier on both sides"))
                    
    result.addKid(ctx.identifier())
    if ctx.curKind() != TtPeriod:
      return

proc memberExpr(ctx: var ParseCtx): Con4mNode =
  # For use in contexts like parameters where we KNOW this can only be
  # an attribute.
  result = ctx.newNode(NodeMember)

  while true:
    ctx.expect(TtIdentifier)
    result.addKid(ctx.identifier())
    if ctx.curKind() != TtPeriod:
      return
    ctx.advance()

proc indexExpr(ctx: var ParseCtx, lhs: Con4mNode): Con4mNode =
  result = ctx.newNode(NodeIndex)

  result.addKid(lhs)

  adtLiteral:
    ctx.advance()
    result.addKid(ctx.expression())
    ctx.expect(TtRBracket, consume = true)
    
proc callActuals(ctx: var ParseCtx, lhs: Con4mNode): Con4mNode =
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
        ctx.errSkipStmt(atom("After call argument, expect ") +
                        em("','") + atom(" or ") + em("')'"))

production(listLit, NodeListLit):
  ctx.advance()
  adtLiteral:
    if ctx.curKind() == TtRBracket:
      ctx.advance()
    else:
      while true:
        result.addKid(ctx.expression())
        case ctx.curKind()
        of TtComma:
          ctx.advance()
        of TtRBracket:
          ctx.advance()
          return
        else:
          ctx.errSkipStmt(atom("Expected either another item or ") + em("]"))

production(kvPair, NodeKvPair):
  result.addKid(ctx.expression())
  ctx.expect(TtColon, consume = true)
  result.addKid(ctx.expression())

production(dictLit, NodeDictLit):
  ctx.advance()

  adtLiteral:
    if ctx.curKind() == TtRBrace:
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
        else:
          ctx.errSkipStmt(atom("Expected either another item or ") + em("}"))

production(tupleLit, NodeTupleLit):
  ctx.advance()

  adtLiteral:
    if ctx.curKind() == TtRParen:
      ctx.errSkipStmt("Tuples cannot be empty.")
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
      else:
          ctx.errSkipStmt(atom("Expected either another item or ") + em(")"))

production(stringLit, NodeStringLit):
  ctx.expect(TtStringLit, consume = true)

production(intLit, NodeIntLit):
  ctx.expect(TtIntLit, consume = true)

production(floatLit, NodeFloatLit):
  ctx.expect(TtFloatLit, consume = true)

production(charlit, NodeCharLit):
  ctx.expect(TtCharLit, consume = true)

production(boolLit, NodeBoolLit):
  if ctx.curKind() in [TtTrue, TtFalse]:
    ctx.advance()
  else:
    ctx.errSkipStmt(atom("Expected either ") + em("True") + atom(" or ") + 
                    em("False"))

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
    ctx.errSkipStmt("Invalid start for member access expression")

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

proc notExprRHS(ctx: var ParseCtx): Con4mNode =
    var n = ctx.expressionStart()
    while true:
      let optExpr = ctx.divExpr()
      if optExpr.isSome():
        var r = optExpr.get()
        if len(r.children) == 0:
          ctx.errSkipStmt("Invalid expression start")
        case len(n.children)
        of 0:
          if r.kind notin [NodePlus, NodeMinus, NodeNot]:
            ctx.errSkipStmt("Invalid expression")
        else:
          if r.kind == NodeNot:
            ctx.errSkipStmt(em("not") + atom(" does not take a left-hand " &
              "operand."))
          r.children = @[n, r.children[0]]
          n = r
      else:
        return n

proc notExpr(ctx: var ParseCtx): Option[Con4mNode] =
  case ctx.curKind()
  of TtNot:
    var res = ctx.newNode(NodeNot)
    ctx.advance()
    res.addKid(ctx.notExprRHS())
    return some(res)
  of TtIdentifier, TtLParen:
    return some(ctx.accessExpr())
  else:
    return

production(callback, NodeCallbackLit):
  ctx.advance()

  case ctx.curKind()
  of TtLParen:
    result.addKid(ctx.newNode(NodeNoCallbackName))
    result.addKid(ctx.typeSpec())
  of TtIdentifier:
    result.addKid(ctx.identifier())
    if ctx.curTok().kind == TtLParen:
      result.addKid(ctx.typeSpec())
  else:
    ctx.errSkipStmt(atom("An identifier or parameters required after the ") +
                    em("func") + atom(" keyword"))

production(literal, NodeLiteral):
  case ctx.curKind()
  of TtBackTick, TtObject:
    result.addKid(ctx.typeSpec())
  of TtFunc:
    result.addKid(ctx.callback())
  of TtIntLit:
    result.addKid(ctx.intLit())
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
    let txt = ctx.curTok().getTokenText()

    if txt == "ref" or txt in allTypeIdentifiers:
      result.addKid(ctx.typeSpec())
    else:
      ctx.errSkipStmt("Expected a literal value")
  else:
    ctx.errSkipStmt("Expected a literal value")

production(expressionStart, NodeExpression):
  case ctx.curKind()
  of TtIntLit, TtFloatLit, TtStringLit, TtCharLit, TtTrue, TtFalse, TtLBrace,
     TtLBracket, TtLParen, TtOtherLit, TtFunc, TtBacktick, TtObject:
       result.addKid(ctx.literal())
  of TtIdentifier:
    let txt = ctx.curTok().getTokenText()
    if txt == "ref" or txt in allTypeIdentifiers:
      result.addKid(ctx.literal())
    else:
      result.addKid(ctx.accessExpr())
  of TtPlus, TtMinus, TtNot:
    # empty string for the LHS; For TtPlus and TtMinus, the proper
    # production below us will need to tease out whether it's a unary
    # or binary. And TtNot needs to reject when there's a lhs.
    discard 
  else:
    echo ctx.curKind()
    echo getStackTrace()
    ctx.errSkipStmt("Invalid expression start.")
        
production(enumStmt, NodeEnumStmt):
  ctx.advance()
  
  while true:
    if ctx.curKind() != TtIdentifier:
      ctx.errSkipStmt("Expected an identifier as part of enum")
    result.addKid(ctx.identifier())
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
    result.token = some(op)
  else:
    ctx.errSkipStmt("Expected a : or = for an attribute assignment")

  result.addKid(child)
  result.addKid(ctx.expression())
  
  ctx.endOfStatement()

production(lockAttr, NodeAttrSetLock):
  ctx.advance()
  if ctx.curKind() != TtIdentifier:
    ctx.errSkipStmt("Expected an attribute after '~' (attr lock indicator)")
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

production(forStmt, NodeForStmt):
  ctx.advance()
  ctx.expect(TtIdentifier)
  result.addKid(ctx.identifier())
  ctx.expect(TtFrom, consume = true)
  result.addKid(ctx.expression())
  ctx.expect(TtTo, consume = true)
  result.addKid(ctx.expression())
  ctx.loopDepth += 1
  result.addKid(ctx.body())
  ctx.loopDepth -= 1

production(whileStmt, NodeWhileStmt):
  ctx.advance()
  result.addKid(ctx.expression())
  ctx.loopDepth += 1
  result.addKid(ctx.body())
  ctx.loopDepth -= 1

production(builtinType, NodeTypeBuiltin):
  if ctx.curTok().getTokenText() notin getAllBuiltinTypeNames():
    ctx.errSkipStmt(em(ctx.curTok().getTokenText()) + 
                    atom(" is not a builtin type. Use ") +
                    em("struct[typename]") + atom(" for user types."))
  result.addKid(ctx.identifier())

production(refType, NodeTypeRef):
  ctx.advance()
  result.addKid(ctx.typeSpec())

production(tupleType, NodeTypeTuple):
  ctx.advance()
  ctx.expect(TtLBracket, consume = true)
  result.addKid(ctx.typeSpec())
  while ctx.curKind() == TtComma:
    ctx.advance()
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
  ctx.expect(TtIdentifier)
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
    ctx.errSkipStmt("Object types must provide either a name or a type " &
                    "variable if specifying an object where no fields " &
                    "will be referenced")
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

production(oneTypeSpec, NodeType):
  case ctx.curKind()
  of TtBacktick:
    result.addKid(ctx.typeVariable())
  of TtFunc:
    # Once we know we're parsing a type, we don't require the leading
    # "func".  It's only necessary for distinguishing generic
    # parenthesized expressions in an expression context.  We could
    # deal with that problem unambiguously, but requires more logic.
    ctx.advance()
    ctx.expect(TtLParen)
    result.addKid(ctx.funcType())
  of TtLParen:
    result.addKid(ctx.funcType())
  of TtObject:
      result.addKid(ctx.objectType())
  of TtIdentifier:
    case ctx.curTok().getTokenText()
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
    else:
      result.addKid(ctx.builtinType())
  else:
    ctx.errSkipStmt("Invalid syntax for a type declaration.")

production(typeSpec, NodeTypeSpec):
  result.addKid(ctx.oneTypeSpec())
  while ctx.curKind() == TtOr:
    ctx.advance()
    result.addKid(ctx.oneTypeSpec())
      

production(formal, NodeFormal):
  result.addKid(ctx.identifier())
  if ctx.curKind() == TtColon:
    ctx.advance()
    result.addKid(ctx.typeSpec())

production(formalList, NodeFormalList):
  ctx.advance()
  if ctx.curKind() == TtRParen:
    ctx.advance()
  else:
    while true:
      ctx.expect(TtIdentifier)
      result.addKid(ctx.formal())
      case ctx.curKind()
      of TtRParen:
        ctx.advance()
        return
      of TtComma:
        ctx.advance()
      else:
        ctx.errBail("Expected either a closing paren or an additional " &
                    "parameter")


production(funcDef, NodeFuncDef):
  ctx.advance()
  ctx.inFunc = true
  ctx.expect(TtIdentifier)
  result.addKid(ctx.identifier())
  ctx.expect(TtLParen)
  result.addKid(ctx.formalList())
  if ctx.curKind() == TtArrow:
    result.addKid(ctx.returnType())
  result.addKid(ctx.body())
  ctx.inFunc = false

production(varSymNames, NodeVarSymNames):
  while true:
    ctx.expect(TtIdentifier)
    result.addKid(ctx.identifier())
    if ctx.curKind() == TtComma:
      ctx.advance()
    else:
      return

production(varStmt, NodeVarStmt):
  ctx.advance()
  result.addKid(ctx.varSymNames)
  if ctx.curKind() == TtColon:
    ctx.advance()
    result.addKid(ctx.typeSpec())
  ctx.endOfStatement()

production(exportStmt, NodeExportStmt):
  ctx.advance()
  while true:
    ctx.expect(TtIdentifier)
    result.addKid(ctx.identifier())
    if ctx.curKind() == TtComma:
      ctx.advance()
    else:
      ctx.endOfStatement()
      return

production(useStmt, NodeUseStmt):
  ctx.advance()
  ctx.expect(TtIdentifier)
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
    ctx.errSkipStmt(em("use") +
                    atom(" statement requires a string literal after ") +
                    em("from"))

production(paramVarDecl, NodeVarStmt):
  ctx.advance()
  ctx.expect(TtIdentifier)
  result.addKid(ctx.identifier())
    
production(parameterBlock, NodeParamBlock):
  ctx.advance()
  case ctx.curKind() 
  of TtIdentifier:
    result.addKid(ctx.memberExpr())
  of TtVar:
    result.addKid(ctx.paramVarDecl())
  else:
    ctx.errBail(em("parameter") + 
                text(" keyword must be followed by an attribute " &
                  "(can be dotted) or `var` and a local variable name."))
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
    ctx.expect(TtIdentifier)
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
  if ctx.atEndOfLine():
    ctx.endOfStatement()
  else:
    result.addKid(ctx.expression())

production(breakStmt, NodeBreakStmt):
  ctx.advance()
  ctx.endOfStatement()

production(continueStmt, NodeBreakStmt):
  ctx.advance()
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
        ctx.parseError("Unclosed block when end-of-file found")
      of TtRBrace:
        ctx.nesting -= 1
        ctx.advance()
        ctx.ignoreAllNewlines()

        return
      of TtSemi, TtNewline:
        ctx.advance()
      of TtEnum, TtFunc, TtExportVar:
        ctx.errSkipStmt(em($(ctx.curKind())) + 
                 atom(" is only allowed at the top-level"), false)
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
          ctx.errSkipStmt(em("continue") + 
                   atom(" is only allowed inside loops"), false)
        else:
          result.addKid(ctx.continueStmt())
      of TtBreak:
        if not ctx.inLoop():
          ctx.errSkipStmt(em("break") + atom(" is only allowed inside loops"),
                          false)
        else:
          result.addKid(ctx.breakStmt())
      of TtReturn:
        if not ctx.inFunction():
          ctx.errSkipStmt(em("return") + 
                          atom(" is only allowed inside functions"), false)
        else:
          result.addKid(ctx.returnStmt())
      of TtVar:
        result.addKid(ctx.varStmt())
      of TtIdentifier:
        case ctx.curTok.getTokenText()
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
        echo "In body: ", ctx.curKind()
        result.addKid(ctx.expression())
        ctx.endOfStatement()
    except:
      while true:
        if ctx.atEndOfLine() and ctx.curKind() notin [TtRBrace, TtRParen]:
          ctx.advance()
          break
        ctx.advance()

production(module, NodeModule):
  # Instead of being under token 1, we really want Module to be bound to
  # the start-of-stream token.
  result.token = some(result.allTokens.tokens[0])

  while true:
    try:
      case ctx.curKind()
      of TtEof, ErrorTok, ErrorLongComment, ErrorStringLit, ErrorCharLit,
         ErrorOtherLit, ErrorLitMod:
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
        ctx.errSkipStmt("Found 'continue' outside a loop.")
      of TtBreak:
        ctx.errSkipStmt("Found 'break' outside a loop.")
      of TtReturn:
        ctx.errSkipStmt("Found 'return' outside a function definition.")
      of TtFunc:
        result.addKid(ctx.funcDef())
      of TtVar:
        result.addKid(ctx.varStmt())
      of TtExportVar:
        result.addKid(ctx.exportStmt())
      of TtIdentifier:
        case ctx.curTok.getTokenText()
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
      while true:
        if ctx.atEndOfLine() and ctx.curKind() notin [TtRBrace, TtRParen]:
          ctx.advance()
          break
        ctx.advance()
