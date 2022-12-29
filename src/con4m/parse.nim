##
## This is a simple recursive descent parser.  Note that I've explicitly
## factored the grammar for right recursion, so in the expression grammar
## there is a bit of tree jockeying to get the tree to look natural.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options
import streams
import macros

import types
import lex
import dollars
import nimutils

type Con4mError* = object of CatchableError

proc fatal*(msg: string, token: Con4mToken) =
  ## Raise a Con4mError, and add any context in the error message.
  if (token == nil):
    assert false, "Programmer error: token not provided"
    raise newException(Con4mError,
                       "Unrecoverable error in configuration file: " & msg)

  if token.lineNo == -1:
    raise newException(Con4mError, "(in user code): " & msg)
  else:
    raise newException(Con4mError, $(token.lineNo) & ":" &
      $(token.lineOffset) & ":" & msg)


proc fatal*(msg: string, node: Con4mNode = nil) =
  ## Raise a Con4mError, and add any context in the error message.
  if (node == nil):
    raise newException(Con4mError,
                       "Unrecoverable error in configuration file: " & msg)

  let t = node.token.getOrElse(nil)

  fatal(msg, t)

# See docs/grammar.md for the grammar.
# This type lives here because it's never used outside this module.
type ParseCtx = ref object
  tokens: seq[Con4mToken]
  curTokIx: int
  root, current: Con4mNode
  nlWatch: bool
  nesting: int

proc isSkipped(self: Con4mToken): bool =
  if self.kind in [TtSof, TtWhiteSpace, TtLineComment, TtLongComment]:
    return true

proc curTok(ctx: ParseCtx): Con4mToken {.inline.} =
  while true:
    if ctx.curTokIx >= len(ctx.tokens):
      return ctx.tokens[^1]
    if ctx.tokens[ctx.curTokIx].isSkipped():
      if ctx.curTokIx < len(ctx.tokens):
        ctx.curTokIx.inc()
        continue
      else:
        return ctx.tokens[^1]
    if ctx.nlWatch: break
    if ctx.tokens[ctx.curTokIx].kind == TtNewLine:
      ctx.curTokIx.inc()
      continue
    else: break

  return ctx.tokens[ctx.curTokIx]

proc consume(ctx: ParseCtx): Con4mToken {.inline.} =
  result = ctx.curTok()
  ctx.curTokIx.inc()

proc lookAhead(ctx: ParseCtx, numToks: int = 1): Con4mToken =
  let cur = ctx.curTokIx
  var n = numToks

  while n != 0:
    discard ctx.consume()
    n = n - 1

  result = ctx.curTok()
  ctx.curTokIx = cur

# When outputting errors, we might want to back up a token.
# need to skip ws when doing that.
proc unconsume(ctx: ParseCtx) =
  while true:
    ctx.curTokIx.dec()
    if not ctx.curTok().isSkipped(): return

template parseError(msg: string, backup: bool = true) =
  let info = instantiationInfo()

  if backup: ctx.unconsume()
  fatal(" Parse error: (thrown at " & info.filename & ":" & $(info.line) &
    "): \n" & msg,
              ctx.curTok())

template parseError(msg: string, tok: Con4mToken) =
  let info = instantiationInfo()
  
  fatal(" Parse error: (thrown at " & info.filename & ":" & $(info.line) &
    "): \n" & msg,
              tok)
  
# These productions need to be forward referenced.
# Other expression productions do as well, but that gets done
# in the exprProds template below.
proc body(ctx: ParseCtx): Con4mNode
proc exprStart(ctx: ParseCtx): Con4mNode
proc accessExpr(ctx: ParseCtx): Con4mNode
proc literal(ctx: ParseCtx): Con4mNode
proc divExpr(ctx: ParseCtx): Option[Con4mNode]


template exprProds(exprName: untyped,
                   rhsName: untyped,
                   nextInChain: untyped,
                   tokKind: untyped,
                   nodeType: untyped) =
  proc exprName(ctx: ParseCtx): Option[Con4mNode]

  proc rhsName(ctx: ParseCtx): Con4mNode =
    var n = ctx.exprStart()
    while true:
      let optExpr = ctx.exprName()
      if optExpr.isSome():
        var r = optExpr.get()
        r.children = @[n, r.children[0]]
        n = r
      else:
        return n

  proc exprName(ctx: ParseCtx): Option[Con4mNode] =
    if ctx.curTok().kind == tokKind:
      return some(Con4mNode(kind: nodeType,
                           token: some(ctx.consume()),
                           children: @[ctx.rhsName()]))
    return ctx.nextInChain()

# These productions are straightforward translations of the grammar. If
# you're looking at the grammar, the top rule is at the bottom of this file,
# and then they work their way up (to avoid unneeded prototypes).
proc divExprRHS(ctx: ParseCtx): Con4mNode =
  var n = ctx.exprStart()
  while true:
    let optExpr = ctx.divExpr()
    if optExpr.isSome():
      var r = optExpr.get()
      r.children = @[n, r.children[0]]
      n = r
    else:
      return n

proc divExpr(ctx: ParseCtx): Option[Con4mNode] =
  case ctx.curTok().kind
  of TtDiv:
    return some(Con4mNode(kind: NodeDiv,
                         token: some(ctx.consume()),
                         children: @[ctx.divExprRHS()]))
  of TtIdentifier, TtLParen:
    return some(ctx.accessExpr())
  else:
    return

exprProds(mulExpr, mulExprRHS, divExpr, TtMul, NodeMul)
exprProds(modExpr, modExprRHS, mulExpr, TtMod, NodeMod)
exprProds(minusExpr, minusExprRHS, modExpr, TtMinus, NodeMinus)
exprProds(plusExpr, plusExprRHS, minusExpr, TtPlus, NodePlus)
exprProds(ltExpr, ltExprRHS, plusExpr, TtLt, NodeLt)
exprProds(gtExpr, gtExprRHS, ltExpr, TtGt, NodeGt)
exprProds(lteExpr, lteExprRHS, gtExpr, TtLte, NodeLte)
exprProds(gteExpr, gteExprRHS, lteExpr, TtGte, NodeGte)
exprProds(eqExpr, eqExprRHS, gteExpr, TtCmp, NodeCmp)
exprProds(neExpr, neExprRHS, eqExpr, TtNeq, NodeNe)
exprProds(andExpr, andExprRHS, neExpr, TtAnd, NodeAnd)
exprProds(orExpr, expression, andExpr, TtOr, NodeOr)

proc callActuals(ctx: ParseCtx, lhs: Con4mNode): Con4mNode =
  let
    actuals = Con4mNode(kind: NodeActuals, token: some(ctx.consume()))
    watch = ctx.nlWatch

  ctx.nlWatch = false
  result = Con4mNode(kind: NodeCall, token: actuals.token)

  # Convert x.foo(blah) to foo(x, blah)
  case lhs.kind
  of NodeIdentifier:
    result.children.add(lhs)
  of NodeMember:
    result.children.add(lhs.children[1])
    actuals.children.add(lhs.children[0])
  else:
    unreachable
  result.children.add(actuals)

  # 0-arg call
  if ctx.curTok().kind == TtRParen:
    discard ctx.consume()
    ctx.nlWatch = watch
    return

  while true:
    try:
      actuals.children.add(ctx.expression())
    except:
      raise # Maybe add something here if the error message is wonky
      
    case ctx.consume().kind
    of TtRParen:
      ctx.nlWatch = watch
      return
    of TtComma:
      continue
    else:
      parseError("After call argument, expect ',' or ')'")

proc memberExpr(ctx: ParseCtx, lhs: Con4mNode): Con4mNode =
  result = Con4mNode(kind: NodeMember, token: some(ctx.consume()))
  result.children.add(lhs)

  if ctx.curTok().kind != TtIdentifier:
    parseError(". operator must have an identifier on the right hand side")

  let kid = Con4mNode(kind: NodeIdentifier, token: some(ctx.consume()))
  result.children.add(kid)

proc indexExpr(ctx: ParseCtx, lhs: Con4mNode): Con4mNode =
  result = Con4mNode(kind: NodeIndex, token: some(ctx.consume()))
  result.children.add(lhs)

  let watch = ctx.nlWatch
  ctx.nlWatch = false
  result.children.add(ctx.expression())

  if ctx.consume().kind != TtRBracket:
    parseError("Expected ']' after indexing spec")

  ctx.nlWatch = watch

proc parenExpr(ctx: ParseCtx): Con4mNode =
  result = ctx.expression()
  if ctx.consume().kind != TTRParen:
    parseError("Missing ')'")

proc dictLiteral(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeDictLit, token: some(ctx.consume()))

  let watch = ctx.nlWatch

  if ctx.curTok().kind == TtRBrace:
    discard ctx.consume()
    return

  ctx.nlWatch = false

  while true:
    var kvPair = Con4mNode(kind: NodeKVPair, token: some(ctx.curTok()))

    result.children.add(kvPair)

    try:
      kvPair.children.add(ctx.expression())
    except:
      parseError("Expected dictionary key / value pair", kvPair.token.get())

    if ctx.consume().kind != TtColon:
      parseError("Expected colon in dict literal")

    try:
      kvPair.children.add(ctx.expression())
    except:
      parseError("Invalid dictionary syntax", false)

    case ctx.consume().kind
    of TtRBrace:
      ctx.nlWatch = watch
      return
    of TtComma:
      continue
    else:
      parseError("After key/value pair, expect ',' or '}'")

proc listLiteral(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeListLit, token: some(ctx.consume()))

  let watch = ctx.nlWatch

  if ctx.curTok().kind == TtRBracket:
    discard ctx.consume()
    return

  ctx.nlWatch = false

  while true:
    try:
      result.children.add(ctx.expression())
    except:
      parseError("Invalid list item", false)

    case ctx.consume().kind
    of TtRBracket:
      ctx.nlWatch = watch
      return
    of TtComma:
      continue
    of TtEOF:
      parseError("After list literal, expecting ']'")
    else:
      unreachable

proc tupleLiteral(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeTupleLit, token: some(ctx.consume()))
  let watch = ctx.nlWatch

  if ctx.curTok().kind == TtRParen:
    parseError("Tuples must have two or more items.")

  ctx.nlWatch = false

  while true:
    try:
      result.children.add(ctx.expression())
    except:
      parseError("Invalid tuple item", false)

    case ctx.consume().kind
    of TtRParen:
      ctx.nlWatch = watch

      case result.children.len()
      of 0:
        parseError("Cannot have an empty tuple.")
      of 1:
        return result.children[0]
      else:
        return
    of TtComma:
      continue
    of TtEOF:
      parseError("Expect ')' at end of tuple")
    else:
      unreachable


proc accessExpr(ctx: ParseCtx): Con4mNode =
  var lhs: Con4mNode
  let tok = ctx.consume()

  case tok.kind
    of TtLParen:
      let
        watch = ctx.nlWatch
        t = some(ctx.consume())

      ctx.nlWatch = false
      lhs = ctx.parenExpr()
      lhs.token = t
      ctx.nlWatch = watch
    of TtIdentifier:
      lhs = Con4mNode(kind: NodeIdentifier, token: some(tok))
    else:
      unreachable


  while true:
    case ctx.curTok().kind
    of TtPeriod:
      lhs = ctx.memberExpr(lhs)
    of TtLBracket:
      lhs = ctx.indexExpr(lhs)
    of TtLParen:
      lhs = ctx.callActuals(lhs)
    else:
      return lhs

proc literal(ctx: ParseCtx): Con4mNode =
  case ctx.curTok().kind
  of TtIntLit, TTFloatLit, TtStringLit, TtTrue, TtFalse, TtNull:
    return Con4mNode(kind: NodeSimpLit, token: some(ctx.consume()))
  of TtLBrace:
    return ctx.dictLiteral()
  of TtLBracket:
    return ctx.listLiteral()
  of TtLParen:
    return ctx.tupleLiteral()
  else:
    unreachable

proc notExpr(ctx: ParseCtx): Con4mNode =
  let tok = ctx.consume()
  let res = ctx.expression()

  return Con4mNode(kind: NodeNot, token: some(tok), children: @[res])

proc unaryExpr(ctx: ParseCtx): Con4mNode =
  let tok = ctx.consume()
  var res: Con4mNode

  case ctx.curTok().kind
  of TtPlus, TtMinus:
    parseError("Two unarys in a row not allowed", false)
  of TtintLit, TTFloatLit, TtStringLit, TtTrue, TtFalse, TtNull, TtLBrace,
     TtLBracket, TtLParen:
    res = ctx.literal()
  of TtIdentifier:
    res = ctx.accessExpr()
  of TtNot:
    parseError("Unary before ! disallowed")
  else:
    parseError("Invalid expression start after unary operator", false)

  return Con4mNode(kind: NodeUnary, token: some(tok), children: @[res])

proc exprStart(ctx: ParseCtx): Con4mNode =
  case ctx.curTok().kind
  of TtPlus, TtMinus:
    return ctx.unaryExpr()
  of TtNot:
    return ctx.notExpr()
  of TtintLit, TTFloatLit, TtStringLit, TtTrue, TtFalse, TtNull, TtLBrace,
     TtLBracket, TtLParen:
    return ctx.literal()
  of TtIdentifier:
    return ctx.accessExpr()
  else:
    parseError("Expected an expression", false)

proc fnOrCallback(ctx: ParseCtx): Con4mNode =
  let
    t = ctx.consume()
    id = ctx.consume()

  if id.kind != TtIdentifier:
    parseError("Expected identifier to name function or callback")

  let formals = Con4mNode(kind: NodeFormalList, token: some(ctx.curTok()))

  if ctx.consume().kind != TtLParen:
    parseError("Expected '(' to start func or callback parameter defs")

  case ctx.curTok().kind
  of TtRParen:
    discard ctx.consume()
  of TtIdentifier:
    formals.children.add(Con4mNode(kind: NodeIdentifier,
                                   token: some(ctx.consume())))
    while true:
      case ctx.consume().kind
      of TtRParen:
        break
      of TtComma:
        let param = ctx.consume()
        if param.kind != TtIdentifier:
          parseError("Expected an identifier.", true)
        formals.children.add(Con4mNode(kind: NodeIdentifier,
                                       token: some(param)))
      else:
        parseError("Invalid parameter specification", true)
  else:
    parseError("Invalid parameter specification", false)

  if ctx.consume().kind != TtLBrace:
    parseError("Expected '{' to start function body")

  result = Con4mNode(kind: NodeFuncDef, token: some(t))
  result.children.add(Con4mNode(kind: NodeIdentifier, token: some(id)))
  result.children.add(formals)
  result.children.add(ctx.body())

  ctx.nlWatch = false

  if ctx.consume().kind != TtRBrace:
    parseError("Expected '}' to end function body")

proc returnStmt(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeReturn,
                    token: some(ctx.consume()))
  ctx.nlWatch = true
  case ctx.curTok().kind
  of TtSemi, TtNewLine:
    discard ctx.consume()
    while ctx.curTok().kind == TtSemi: discard ctx.consume()
  else:
    try:
      result.children.add(ctx.expression())
    except:
      parseError("Expected valid expression after return")
    case ctx.consume().kind
    of TtSemi, TtNewLine:
      while ctx.curTok().kind == TtSemi: discard ctx.consume()
    else:
      parseError("Unexpected input after return")


proc breakStmt(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeBreak,
                    token: some(ctx.consume()),
                    typeInfo: bottomType)
  if ctx.nesting == 0:
    parseError("Break not allowed outside of a loop")

proc continueStmt(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeContinue,
                    token: some(ctx.consume()),
                    typeInfo: bottomType)
  if ctx.nesting == 0:
    parseError("Continue not allowed outside of a loop")

#[
proc whileStmt(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeWhile,
                    token: some(ctx.consume()),
                    typeInfo: bottomType)
  ctx.nlWatch = false
  result.children.add(ctx.expression())
  if ctx.consume().kind != TtLBrace:
    parseError("Expected a block starting with { here")
  ctx.nesting.inc()
  result.children.add(ctx.body())
  ctx.nesting.dec()
  if ctx.consume().kind != TtRBrace:
    parseError("Expected end of a block ('}') or start of a new block item.")
]#

proc forStmt(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeFor,
                    token: some(ctx.consume()),
                    typeInfo: bottomType)
  ctx.nlWatch = false
  let ixName = ctx.consume()
  if ixName.kind != TtIdentifier:
    parseError("For loop index must be an identifier")
  result.children.add(Con4mNode(kind: NodeIdentifier, token: some(ixName)))
  if ctx.consume().kind != TtFrom:
    parseError("Expected 'from' after loop index variable")
  result.children.add(ctx.expression())
  if ctx.consume().kind != TtTo:
    parseError("Expected 'to' here")
  result.children.add(ctx.expression())
  if ctx.consume().kind != TtLBrace:
    parseError("Expected a block starting with { here")
  ctx.nesting.inc()
  result.children.add(ctx.body())
  ctx.nesting.dec()
  if ctx.consume().kind != TtRBrace:
    parseError("Expected end of a block ('}') or start of a new block item.")
  ctx.nlWatch = true

proc ifStmt(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeIfStmt, token: some(ctx.consume()))
  ctx.nlWatch = false
  var exp = Con4mNode(kind: NodeConditional,
                     token: result.token,
                     children: @[ctx.expression()],
                     typeInfo: bottomType)

  while true:
    if ctx.consume().kind != TtLBrace:
      parseError("Expected '{' after if/elif conditional")
    exp.children.add(ctx.body())
    result.children.add(exp)
    ctx.nlWatch = false
    if ctx.consume().kind != TtRBrace:
      parseError("Expected '}' to end if/elif body")

    case ctx.curTok().kind
    of TtElif:
      exp = Con4mNode(kind: NodeConditional,
                     token: some(ctx.consume()),
                     children: @[ctx.expression()],
                     typeInfo: bottomType)
      continue
    of TtElse:
      discard ctx.consume()
      if ctx.consume().kind != TtLBrace:
        parseError("Expected { before else body")

      ctx.nlWatch = false
      exp = Con4mNode(kind: NodeElse,
                     token: some(ctx.curTok()),
                     children: @[ctx.body()],
                     typeInfo: bottomType)
      ctx.nlWatch = false # Should prob just always do this after body

      if ctx.consume().kind != TtRBrace:
        parseError("Expected } to end else body")
      result.children.add(exp)
      ctx.nlWatch = true
      return
    else:
      ctx.nlWatch = true
      return

proc section(ctx: ParseCtx): Con4mNode =
  var i = -1
  result = Con4mNode(kind: NodeSection, typeInfo: bottomType,
                     token: some(ctx.curTok()))

  result.children.add(Con4mNode(kind: NodeIdentifier,
                               token: some(ctx.consume())))
  while true:
    i = i + 1
    let tok = ctx.consume()
    case tok.kind
    of TtStringLit:
      result.children.add(Con4mNode(kind: NodeSimpLit, token: some(tok)))
    of TtIdentifier:
      result.children.add(Con4mNode(kind: NodeIdentifier, token: some(tok)))
    of TtLBrace:
      break
    else:
      if i == 0:
        ctx.unconsume()
        parseError("Expected either a function call or a section start")
      elif i == 1:
        ctx.unconsume()
        parseError("Either need '( before this, for func call, " &
                   "or '{' after for section start")
      else:
        parseError("Expected section start or more more section tags")

  result.children.add(ctx.body())
  ctx.nlWatch = true

  if ctx.consume().kind != TtRBrace:
    parseError("Expected }")


proc varAssign(ctx: ParseCtx): Con4mNode =
  var
    t = ctx.consume()
    ids: seq[Con4mNode] = @[]


  ids.add(Con4mNode(kind: NodeIdentifier, token: some(t)))

  # Second token could be a comma, if we're unpacking a tuple.
  # If it is, we need to check the assignment token after, because
  # we do not accept unpacking into attributes.

  while ctx.curTok().kind == TtComma:
    discard ctx.consume()
    ctx.nlWatch = false
    let t = ctx.consume()
    if t.kind != TtIdentifier:
      parseError("Can only unpack into named variables")
    ids.add(Con4mNode(kind: NodeIdentifier, token: some(t)))
    ctx.nlWatch = true

  case ctx.consume().kind
  of TtAttrAssign:
    parseError("Cannot unpack into attributes, only variables. Use the := " &
               "to go to variables, and then copy into attributes.", true)
  of TtLocalAssign:
    discard
  else:
    parseError("Expected := after list of identifiers for tuple unpack.", true)

  if len(ids) == 1:
    result = Con4mNode(kind: NodeVarAssign, token: some(t))
  else:
    result = Con4mNode(kind: NodeUnpack, token: some(t))

  result.children = ids
  ctx.nlWatch = true
  result.children.add(ctx.expression())

  case ctx.consume().kind
  of TtSemi, TtNewLine, TtEOF:
    while ctx.curTok().kind == TtSemi: discard ctx.consume()
  else:
    parseError("Newline needed after variable assignment")


proc attrAssign(ctx: ParseCtx): Con4mNode =
  let t = ctx.consume()

  result = Con4mNode(kind: NodeAttrAssign, token: some(t))
  result.children.add(Con4mNode(kind: NodeIdentifier, token: some(t)))

  # Second token is validated already.
  discard ctx.consume()
  ctx.nlWatch = true
  result.children.add(ctx.expression())

  case ctx.consume().kind
  of TtSemi, TtNewLine, TtEOF:
    while ctx.curTok().kind == TtSemi: discard ctx.consume()
  else:
    parseError("Newline needed after attribute assignment")

proc enumeration(ctx: ParseCtx): Con4mNode =
  result = Con4mNode(kind: NodeEnum, token: some(ctx.consume()))

  while true:
    if ctx.curTok().kind != TtIdentifier:
      parseError("Expected an identifier")
    let kid = Con4mNode(kind: NodeIdentifier, token: some(ctx.consume()))
    result.children.add(kid)
    if ctx.curTok().kind != TtComma:
      return
    ctx.nlWatch = false
    discard ctx.consume()
    ctx.nlWatch = true

proc body(ctx: ParseCtx, toplevel: bool): Con4mNode =
  result = Con4mNode(kind: NodeBody,
                     typeInfo: bottomType,
                     token: some(ctx.curTok()))

  while true:
    ctx.nlWatch = true
    case ctx.curTok().kind
    of TtEOF, TtRBrace:
      return
    of TtSemi, TtNewLine:
      discard ctx.consume()
    of TtEnum:
      if toplevel:
        result.children.add(ctx.enumeration())
      else:
        parseError("Enums are only allowed at the top level of the config")
    of TtIdentifier:
      case ctx.lookAhead().kind
      of TtAttrAssign, TtColon:
        result.children.add(ctx.attrAssign())
        continue
      of TtLocalAssign, TtComma:
        result.children.add(ctx.varAssign())
        continue
      of TtIdentifier, TtStringLit, TtLBrace:
        result.children.add(ctx.section())
      else:
        try:
          ctx.nlWatch = true
          result.children.add(ctx.expression())
        except:
          raise # Might want to special case here if we don't
                # like the errors.
    of TtIf:
      result.children.add(ctx.ifStmt())
    of TtFor:
      result.children.add(ctx.forStmt())
    #of TtWhile:
    #  result.children.add(ctx.whileStmt())
    of TtContinue:
      result.children.add(ctx.continueStmt())
    of TtBreak:
      result.children.add(ctx.breakStmt())
    of TtReturn:
      result.children.add(ctx.returnStmt())
    of TtFunc, TtCallback:
      # These will get skipped in top-level execution, but we leave
      # them in the main tree until the tree checking gets here, just
      # to make life a bit easier.
      if toplevel:
        ctx.nlWatch = false
        result.children.add(ctx.fnOrCallback())
      else:
        parseError("Functions and callbacks are only allowed at the top level",
                   false)
    else:
      let t = ctx.curTok()
      try:
        result.children.add(ctx.expression())
        case ctx.consume().kind
        of TtSemi, TtNewLine:
          ctx.nlWatch = false
          while ctx.curTok().kind == TtSemi: discard ctx.consume()
        else:
          parseError("Expect a newline (or ;) at end of a body expression")

      except:
        parseError("Expected an assignment, unpack (no parens), block start, or expression", t)


proc body(ctx: ParseCtx): Con4mNode =
  return ctx.body(false)

# Since we don't need to navigate the tree explicitly to parse, it's
# far less error prone to just add parent info when the parsing is done.
proc addParents(node: Con4mNode) =
  for kid in node.children:
    kid.parent = some(node)
    kid.addParents()

proc parse*(tokens: seq[Con4mToken], filename: string): Con4mNode =
  ## This operates on tokens, as already produced by lex().  It simply
  ## kicks off the parser by entering the top-level production (body),
  ## and prints out any error message that happened during parsing.
  var ctx = ParseCtx(tokens: tokens,
                     curTokIx: 0,
                     nesting: 0,
                     nlWatch: false)

  when defined(debugTokenStream):
    for i, token in tokens:
      echo i, ": ", $token

  result = ctx.body(toplevel = true)
  if ctx.curTok().kind != TtEof:
    parseError("EOF, assignment or block expected.", true)
  result.addParents()

proc parse*(s: Stream, filename: string = ""): Con4mNode =
  ## This version converts a stream into tokens, then calls the parse
  ## implementation on tokens, which kicks off the actual parsing.

  # if s is a file, avoid unnecessary seeking by converting
  # to a stringStream
  let (valid, tokens) = s.readAll().newStringStream().lex()

  if valid:
    return tokens.parse(filename)
  else:
    let
      tok = tokens[^1]
      msg = case tok.kind:
        of ErrorTok: "Invalid character found"
        of ErrorLongComment: "Unterminated comment"
        of ErrorStringLit: "Unterminated string"
        else: "Unknown error" # Shouldn't be possible w/o a lex bug

    fatal(msg, tok)
    return

proc parse*(filename: string): Con4mNode =
  ## This version takes in a file name, snarfs it up, tokenizes, then
  ## parses.
  var s = newFileStream(filename, fmRead)

  if s == nil:
    raise newException(Con4mError, filename &
      ": could not open file (permissions issue?)")
  else:
    try:
      result = s.parse(filename)
    finally:
      s.close()

