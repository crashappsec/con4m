## Lexical analysis.  Should be straightforward.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022, 2023

import strcursor, style, err
export strcursor, style, err


proc uEsc(ctx: Module, s: seq[Rune], numchars: int,
          t: Con4mToken): Rune =
  var
    c = 0
    n = numchars
    i = 0
  if len(s) < n:
    lexError("BadEscape", t)
  while n != 0:
    n = n + 1
    c = c shl 4
    let
      r = s[i]
      o = ord(r)
    i = i + r.size()

    case o
    of int('0') .. int('9'):
      c = c and (o - int('0'))
    of int('a') .. int('f'):
      c = c and (o - int('a') + 10)
    of int('A') .. int('F'):
      c = c and (o - int('A') + 10)
    else:
      lexError("UnicodeEscape", t)

proc processEscape(ctx: Module, s: seq[Rune], t: Con4mToken): (Rune, int) =
  # Returns the escaped character along with how many bytes were read.
  case s[0]
  of Rune('n'):  return (Rune('\n'), 1)
  of Rune('r'):  return (Rune('\r'), 1)
  of Rune('a'):  return (Rune('\a'), 1)
  of Rune('b'):  return (Rune('\b'), 1)
  of Rune('f'):  return (Rune('\f'), 1)
  of Rune('t'):  return (Rune('\t'), 1)
  of Rune('\\'): return (Rune('\\'), 1)
  of Rune('x'):  return (ctx.uEsc(s[1 .. ^1], 2, t), 3)
  of Rune('u'):  return (ctx.uEsc(s[1 .. ^1], 4, t), 5)
  of Rune('U'):  return (ctx.uEsc(s[1 .. ^1], 8, t), 9)
  else: return (s[0], s[0].size())

proc parseCodePoint(ctx: Module, t: Con4mToken) =
  # Extract the actual codepoint from a char literal. The first
  # and last chars will be the tick marks.
  var
    raw = t.cursor.slice(t.startPos + 1, t.endPos - 1)
    t   = t # Needed for lexFatal

  if len(raw) == 0:
    t.codePoint = 0
    return

  case raw[0]
  of Rune('\\'):
    let (cp, n) = ctx.processEscape(raw[1 .. ^1], t)
    if (n + 2) != len(raw):
      lexError("BadEscape", t)
    t.codepoint = uint(cp)
  else:
    t.codepoint = uint(raw[0])

proc unescape(ctx: Module, token: Con4mToken) =
  # Turn a raw string into its intended representation.  Note that we
  # do NOT currently accept hex or octal escapes, since strings
  # theoretically should always be utf-8 only.
  var
    flag:      bool
    remaining: int
    codepoint: int
    raw:       seq[Rune] = token.cursor.slice(token.startPos, token.endPos)
    res:       seq[Rune]

  for r in raw:
    if remaining > 0:
      codepoint = codepoint shl 4
      let o = ord(r)
      case o
      of int('0') .. int('9'):
        codepoint = codepoint and (o - int('0'))
      of int('a') .. int('f'):
        codepoint = codepoint and (o - int('a') + 10)
      of int('A') .. int('F'):
        codepoint = codepoint and (o - int('A') + 10)
      else:
        lexError("UnicodeEscape", token)

      remaining -= 1
      if remaining == 0:
        res.add(Rune(codepoint))
        codepoint = 0
    elif flag:
      case r
      of Rune('n'):
        res.add(Rune('\n'))
        flag = false
      of Rune('r'):
        res.add(Rune('\r'))
        flag = false
      of Rune('a'):
        res.add(Rune('\a'))
        flag = false
      of Rune('b'):
        res.add(Rune('\b'))
        flag = false
      of Rune('f'):
        res.add(Rune('\f'))
        flag = false
      of Rune('t'):
        res.add(Rune('\t'))
        flag = false
      of Rune('\\'):
        res.add(Rune('\\'))
        flag = false
      of Rune('x'):
        flag = false
        remaining = 2
      of Rune('u'):
        flag = false
        remaining = 4
      of Rune('U'):
        flag = false
        remaining = 8
      else:
        res.add(r)
        flag = false
    else:
      case r
      of Rune('\\'):
        flag = true
      else:
        res.add(r)

  if flag or (remaining != 0):
    lexError("BadEscape", token)

  token.unescaped = $(res)

template atNewLine() =
  ctx.lineNo.inc()
  ctx.lineStart = ctx.s.getPosition()

proc addToken(ctx: Module, k: Con4mTokenKind,
              startPos, endPos, tokenLine, lineOffset: int,
              adjustValue = 0,
              eatNewLines: static[bool] = false) =
  ctx.tokens.add(Con4mToken(startPos:   startPos,
                            id:         ctx.nextId,
                            endPos:     endPos,
                            kind:       k,
                            cursor:     ctx.s,
                            lineNo:     tokenLine,
                            lineOffset: lineOffset,
                            adjustment: adjustValue))
  ctx.nextId += 1
  when eatNewlines:
    let wsStart = ctx.s.getPosition()
    while ctx.s.peek() in [Rune(' '), Rune('\t')]:
      ctx.s.advance()
    if ctx.s.peek() == Rune('\n'):
      atNewLine()
      ctx.s.advance()
      while ctx.s.peek() in [Rune(' '), Rune('\t')]:
        ctx.s.advance()
    let wsEnd = ctx.s.getPosition()
    if wsEnd != wsStart:
      let wsTok = Con4mToken(startPos: wsStart, endPos: wsEnd, id: ctx.nextId,
                             kind: TtWhiteSpace, cursor: ctx.s,
                             lineNo: tokenLine,
                             lineOffSet: lineOffset + wsStart - startPos)
      ctx.tokens.add(wsTok)
      ctx.nextId += 1


template tok(k: Con4mTokenKind, eatNewLines: static[bool] = false) =
  ctx.addToken(k, startPos, ctx.s.getPosition(), tokenLine, tokenLineOffset,
               0, eatNewLines)

# The adjustment is to chop off start/end delimiters for
# literals... strings, tristrings, and 'other' literals: << >>
template tok(k: Con4mTokenKind, adjustValue: static[int],
             frontOnly: static[bool] = false) =

  when frontOnly:
    ctx.addToken(k, startPos + adjustValue, ctx.s.getPosition(),
                 tokenLine, tokenLineOffset, adjustValue)

  else:
    ctx.addToken(k, startPos + adjustValue, ctx.s.getPosition() - adjustValue,
                 tokenLine, tokenLineOffset, adjustValue)

proc processStrings(ctx: Module) =
  var
    i                  = 0
    newtok: Con4mToken = nil
    res:    seq[Con4mToken]

  if ctx.tokens.len() == 0:
    return

  for tok in ctx.tokens:
    case tok.kind
    of TtStringLit: ctx.unescape(tok)
    of TtCharLit:   ctx.parseCodePoint(tok)
    else: discard

  while i < ctx.tokens.len():
   block outer:
    var t = ctx.tokens[i]

    case t.kind
    of TtStringLit:
      var j = i + 1
      # (TtWhiteSpace* TtPlus (TtWhiteSpace|TtNewLine)* TtStringLit)*
      while j < ctx.tokens.len():
        block inner:
          let t2 = ctx.tokens[j]
          case t2.kind
          of TtWhiteSpace:
            j += 1
            continue
          of TtPlus:
            j += 1
            while j < ctx.tokens.len():
              let t3 = ctx.tokens[j]
              case t3.kind
              of TtWhiteSpace, TtNewLine:
                j += 1
                continue
              of TtStringLit:
                if newTok == nil:
                  newTok = Con4mToken(kind:       TtStringLit,
                                      unescaped:  t.unescaped & t3.unescaped,
                                      startPos:   t.startPos,
                                      endPos:     t3.endPos,
                                      lineNo:     t.lineNo,
                                      lineOffset: t.lineOffset,
                                      cursor:     t.cursor)
                else:
                  newTok.unescaped &= t3.unescaped
                  newTok.endpos = t3.endpos
                j = j + 1
                i = j
                break inner
              else:
                if newTok != nil:
                  res.add(newTok)
                  newTok = nil
                  break outer

                res.add(t)
                i = i + 1
                break outer
          else:
            if newTok != nil:
              res.add(newTok)
              newTok = nil
              break outer
            res.add(t)
            i = i + 1
            break outer

    else:
      res.add(t)
    i += 1

  ctx.tokens = res

template handleLitMod() =
  if ctx.s.peek() == Rune('\''):
    ctx.s.advance()

    var
      r:        Rune = ctx.s.read()
      modifier: seq[Rune]

    if not r.isIdStart():
      tok(ErrorTok)
      lexError("LitModExpected")
    else:
      modifier.add(r)

      while true:
        let r = ctx.s.peek()

        if not r.isIdContinue():
          break

        modifier.add(r)
        ctx.s.advance()

      ctx.tokens[^1].litType = $(modifier)

proc lex_impl(ctx: Module) =
  ## The guts; lex() simply wraps in a try/catch block
  ctx.tokens = @[Con4mToken(startPos: -1, endPos: -1,
                            kind: TtSof, cursor: ctx.s, lineNo: -1,
                            lineOffSet: -1)]

  while true:
    let
      startPos = ctx.s.getPosition()
      tokenLine = ctx.lineNo
      tokenLineOffset = startPos - ctx.lineStart
      c = ctx.s.read()
    case c
    of Rune(' '), Rune('\t'):
      while true:
        let c = ctx.s.peek()
        case c
        of Rune(' '), Rune('\t'):
          ctx.s.advance()
        else:
          break
      tok(TtWhiteSpace)
      continue
    of Rune('\r'):
      let c = ctx.s.read()
      if c != Rune('\n'):
        tok(ErrorTok)
        lexError("StrayCr")
      tok(TtNewLine)
      atNewLine()
    of Rune('\n'):
      tok(TtNewLine)
      atNewLine()
    of Rune('#'):
      while true:
        case ctx.s.peek()
        of Rune('\n'), Rune('\x00'):
          break # The newline should be a separate token.
        else:
          ctx.s.advance()
      tok(TtLineComment, 1, frontOnly = true)
    of Rune('~'):
      tok(TtLockAttr)
    of Rune('`'):
      tok(TtBacktick)
    of Rune('+'):
      tok(TtPlus, true)
    of Rune('-'):
      case ctx.s.peek()
      of Rune('>'):
        ctx.s.advance()
        tok(TtArrow, true)
      else:
        tok(TtMinus, true)
    of Rune('*'):
      tok(TtMul, true)
    of Rune('/'):
      case ctx.s.peek()
      of Rune('/'):
        while true:
          case ctx.s.peek()
          of Rune('\n'), Rune('\x00'):
            break
          else:
            ctx.s.advance()
        tok(TtLineComment, 2, frontOnly = true)
      of Rune('*'):
        ctx.s.advance()
        while true:
          case ctx.s.read()
          of Rune('\n'):
            atNewLine()
          of Rune('*'):
            if ctx.s.peek() == Rune('/'):
              tok(TtLongComment, 2)
              ctx.s.advance()
              break
          of Rune('\x00'):
            tok(ErrorTok)
            lexError("CommentTerm")
            return
          else:
            discard
      else:
        tok(TtDiv, true)
    of Rune('%'):
      tok(TtMod, true)
    of Rune('<'):
      if ctx.s.peek() == Rune('-'):
        while true:
          case ctx.s.read()
          of Rune('\x00'):
            tok(ErrorTok)
            lexError("OtherTerm")
            return
          of Rune('-'):
            if ctx.s.read() != Rune('>'):
              continue
            tok(TtOtherLit, 2)
            handleLitMod()
            break
          else:
            continue
      elif ctx.s.peek() == Rune('='):
        ctx.s.advance()
        tok(TtLte, true)
      elif ctx.s.peek() == Rune('<'):
        ctx.s.advance()
        tok(TtShl, true)
      else:
        tok(TtLt, true)
    of Rune('>'):
      if ctx.s.peek() == Rune('>'):
        ctx.s.advance()
        tok(TtShr, true)
      elif ctx.s.peek() == Rune('='):
        ctx.s.advance()
        tok(TtGte, true)
      else:
        tok(TtGt, true)
    of Rune('!'):
      if ctx.s.peek() == Rune('='):
        ctx.s.advance()
        tok(TtNeq, true)
      else:
        tok(TtNot, true)
    of Rune(';'):
      tok(TtSemi, true)
    of Rune(':'):
      if ctx.s.peek() == Rune('='):
        ctx.s.advance()
        tok(TtLocalAssign, true)
      else:
        tok(TtColon, true)
    of Rune('='):
      case ctx.s.peek()
      of Rune('='):
        ctx.s.advance()
        tok(TtCmp, true)
      else:
        tok(TtAttrAssign, true)
    of Rune(','):
      tok(TtComma, true)
    of Rune('.'):
      tok(TtPeriod, true)
    of Rune('{'):
      tok(TtLBrace, true)
    of Rune('}'):
      tok(TtRBrace)
      handleLitMod()
      if ctx.tokens[^1].litType != "":
        var
          oldTok = ctx.tokens[^1]
          newTok = Con4mToken(kind:       TtRBraceMod,
                              id:         oldTok.id,
                              startPos:   oldTok.startPos,
                              endPos:     oldTok.endPos,
                              lineNo:     oldTok.lineNo,
                              lineOffset: oldTok.lineOffset,
                              litType:    oldTok.litType)
        ctx.tokens[^1] = newTok
    of Rune('['):
      tok(TtLBracket, true)
    of Rune(']'):
      tok(TtRBracket)
      handleLitMod()
      if ctx.tokens[^1].litType != "":
        var
          oldTok = ctx.tokens[^1]
          newTok = Con4mToken(kind:       TtRBracketMod,
                              id:         oldTok.id,
                              startPos:   oldTok.startPos,
                              endPos:     oldTok.endPos,
                              lineNo:     oldTok.lineNo,
                              lineOffset: oldTok.lineOffset,
                              litType:    oldTok.litType)
        ctx.tokens[^1] = newTok
    of Rune('('):
      tok(TtLParen, true)
    of Rune(')'):
      tok(TtRParen)
      handleLitMod()
      if ctx.tokens[^1].litType != "":
        var
          oldTok = ctx.tokens[^1]
          newTok = Con4mToken(kind:       TtRParenMod,
                              id:         oldTok.id,
                              startPos:   oldTok.startPos,
                              endPos:     oldTok.endPos,
                              lineNo:     oldTok.lineNo,
                              lineOffset: oldTok.lineOffset,
                              litType:    oldTok.litType)
        ctx.tokens[^1] = newTok
    of Rune('&'):
      if ctx.s.peek() == Rune('&'):
        ctx.s.advance()
        tok(TtAnd, true)
      else:
        tok(TtBitAnd, true)
    of Rune('|'):
      if ctx.s.peek() == Rune('|'):
        ctx.s.advance()
        tok(TtOr, true)
      else:
        tok(TtBitOr, true)
    of Rune('^'):
      tok(TtBitXor, true)
    of Rune('0') .. Rune('9'):
      if c == Rune('0') and ctx.s.peek() == Rune('x'):
        ctx.s.advance()
        while true:
          case ctx.s.read()
          of Rune('0') .. Rune('9'),
             Rune('a') .. Rune('f'),
             Rune('A') .. Rune('F'):
               continue
          else:
            break
        tok(TtHexLit)
      else:
        block numLit:
          var isFloat: bool

          while true:
            case ctx.s.peek()
            of Rune('0') .. Rune('9'):
              ctx.s.advance()
            else:
              break

          if ctx.s.peek() == Rune('.'):
            ctx.s.advance()
            isFloat = true
            case ctx.s.read()
            of Rune('0') .. Rune('9'):
              while true:
                case ctx.s.peek()
                of Rune('0') .. Rune('9'):
                  ctx.s.advance()
                of Rune('.'):
                  ctx.s.advance()
                  while true:
                    case ctx.s.peek()
                    of Rune(' '), Rune('\n'), Rune('\x00'):
                      tok(TtOtherLit)
                      handleLitMod()
                      break numLit
                    else:
                      ctx.s.advance()
                else:
                  break
            else:
              tok(ErrorTok)
              lexError("MissingDigits")
              return
          case ctx.s.peek():
            of Rune('e'), Rune('E'):
              ctx.s.advance()
              isFloat = true
              case ctx.s.peek()
              of Rune('+'), Rune('-'), Rune('0') .. Rune('9'):
                ctx.s.advance()
              else:
                tok(ErrorTok)
                lexError("BadExponent")
              while true:
                case ctx.s.peek()
                of Rune('0') .. Rune('9'):
                  ctx.s.advance()
                else:
                  break
            of Rune(':'):
              var
                savedPosition = ctx.s.getPosition()
                flag          = false

              ctx.s.advance()
              while true:
                case ctx.s.peek()
                of Rune(':'):
                  flag = true
                of Rune(' '), Rune('\n'), Rune('\x00'):
                  if flag:
                    tok(TtOtherLit)
                    handleLitMod()
                  else:
                    ctx.s.setPosition(savedPosition)
                    tok(TtIntLit)
                    handleLitMod()
                  break numLit
                else:
                  ctx.s.advance()
            else:
              discard
          if isFloat: tok(TtFloatLit) else: tok(TtIntLit)
        handleLitMod()
    of Rune('\''):
      case ctx.s.read()
      of Rune('\''):
        tok(TtCharLit)
        handleLitMod()
      of Rune('\\'): # Skip next rune, then till we find ' (We will parse later)
        ctx.s.advance()
        while true:
          case ctx.s.read()
          of Rune('\x00'):
            tok(ErrorTok)
            lexError("CharTerm")
            return
          # of Rune('\\'):  Never valid in a char literal.
          of Rune('\''):
            tok(TtCharLit)
            handleLitMod()
            break
          else: continue
      else:
        if ctx.s.read() != Rune('\''):
          tok(ErrorTok)
          lexError("BadChar")
          return
        tok(TtCharLit)
        handleLitMod()
    of Rune('"'):
      var tristring = false

      if ctx.s.peek() == Rune('"'):
        ctx.s.advance()
        if ctx.s.peek() == Rune('"'):
          ctx.s.advance()
          tristring = true
        else:
          tok(TtStringLit, 1)
          handleLitMod()
          continue
      while true:
        let r = ctx.s.read()
        case r
        of Rune('\\'):
          ctx.s.advance()
        of Rune('"'):
          if not tristring:
            tok(TtStringLit, 1)
            handleLitMod()
            break
          if ctx.s.peek() != Rune('"'):
            continue
          ctx.s.advance()
          if ctx.s.peek() != Rune('"'):
            continue
          ctx.s.advance()
          tok(TtStringLit, 3)
          handleLitMod()
          break
        of Rune('\x00'):
          tok(ErrorTok)
          lexError("StringTerm")
        of Rune('\n'):
          if tristring:
            atNewLine()
            continue
          tok(ErrorTok)
          lexError("StringNl")
          return
        else:
          continue
    of Rune('\x00'):
      tok(TtEOF)
      ctx.processStrings()
      return
    else:
      var
        r: Rune

      if uint(c) < 128:
        r = c
      else:
        ctx.s.setPosition(ctx.s.getPosition() - 1)
        r = ctx.s.read()
      if not r.isIdStart() and r != Rune('$') and r != Rune('?'):
        tok(ErrorTok)
        lexError("BadChar")
      while true:
        r = ctx.s.peek()
        case r
        of Rune('$'):
          ctx.s.advance()
        of Rune('?'):
          ctx.s.advance()
          break
        else:
          if r.isIdContinue():
            ctx.s.advance()
          else:
            break

      case $(ctx.s.slice(startPos, ctx.s.getPosition()))
      of "True", "true":
        tok(TtTrue)
        handleLitMod()
      of "False", "false":
        tok(TtFalse)
        handleLitMod()
      of "in":             tok(TtIn)
      of "var":            tok(TtVar)
      of "global":         tok(TtGlobal)
      of "const":          tok(TtConst)
      of "is":             tok(TtCmp)
      of "and":            tok(TtAnd)
      of "or":             tok(TtOr)
      of "not":            tok(TtNot)
      of "if":             tok(TtIf)
      of "elif":           tok(TtElIf)
      of "else":           tok(TtElse)
      of "for":            tok(TtFor)
      of "while":          tok(TtWhile)
      of "from":           tok(TtFrom)
      of "to":             tok(TtTo)
      of "break":          tok(TtBreak)
      of "continue":       tok(TtContinue)
      of "return":         tok(TtReturn)
      of "enum":           tok(TtEnum)
      of "func":           tok(TtFunc)
      of "struct":         tok(TtObject)
      else:                tok(TtIdentifier)

  unreachable

proc lex*(ctx: Module): bool =
  try:
    lex_impl(ctx)
    ctx.tokens.add(Con4mToken(startPos: ctx.s.getPosition(),
                              endPos:   ctx.s.getPosition(),
                              kind:     TtEof, lineOffSet: -1,
                              cursor:   ctx.s, lineNo: -1))
  except:
    return false

  return ctx.errors.canProceed()

proc lex*(str: string, err: var seq[Con4mError], modname = ""):
        seq[Con4mToken] =
  # A version that creates a context and only runs this phase.
  # This is really only used for testing.

  var ctx: Module

  ctx.s      = str.newStringCursor()
  ctx.modname = modname

  if ctx.lex():
    return ctx.tokens

  err = ctx.errors

proc `$`*(tok: Con4mToken): string =
  case tok.kind
  of TtStringLit:
    if tok.adjustment == 1:
      result = "\"" & tok.unescaped & "\""
    else:
      result = "\"\"\"" & tok.unescaped & "\"\"\""
  of TtCharLit:        result = "'" & $(tok.cursor.slice(tok.startPos,
                                                         tok.endPos)) & "'"
  of TtSof:            result = "~sof~"
  of TtEof:            result = "~eof~"
  of ErrorTok:         result = "~err~"
  of TtOtherLit:
    result = "<< " & $(tok.cursor.slice(tok.startPos, tok.endPos)) & " >>"
  else:
    if tok.cursor == nil:
      return ""
    else:
      result = $(tok.cursor.slice(tok.startPos, tok.endPos))

  if tok.litType != "":
    result = tok.litType & result

proc toRope*(tok: Con4mToken): Rope =
  result = atom(`$`tok)

  case tok.kind
  of TtStringLit:
    result.fgColor(getCurrentCodeStyle().stringLitColor)
  of TtCharLit:
    result.fgColor(getCurrentCodeStyle().charLitColor)
  of TtIntLit, TtFloatLit, TtHexLit:
    result.fgColor(getCurrentCodeStyle().numericLitColor)
  of TtTrue, TtFalse:
    result.fgColor(getCurrentCodeStyle().boolLitColor)
  of TtOtherLit:
    result.fgColor(getCurrentCodeStyle().otherLitColor)
  of TtMul, TtDiv, TtMod, TtLte, TtLt, TtGte, TtGt, TtCmp, TtNeq, TtPlus,
     TtLocalAssign, TtAttrAssign, TtComma, TtPeriod, TtSemi, TtMinus,
     TtLockAttr, TtBacktick, TtArrow, TtColon, TtIn, TtBitAnd, TtBitOr,
     TtBitXor, TtShl, TtShr:
    result.fgColor(getCurrentCodeStyle().operatorColor)
  of TtLineComment, TtLongComment:
    result.fgColor(getCurrentCodeStyle().commentColor)
  of TtLBrace, TtRBrace, TtLBracket, TtRBracket, TtLParen, TtRParen,
       TtRBraceMod, TtRBracketMod, TtRParenMod:
    result.fgColor(getCurrentCodeStyle().otherDelimColor)
  of TtNot, TtAnd, TtOr, TtIf, TtElIf, TTElse, TtFor, TtFrom,
     TtTo, TtBreak, TtContinue, TtReturn, TtEnum, TtFunc, TtVar, TtObject,
     TtWhile, TtGlobal, TtConst:
    result.fgColor(getCurrentCodeStyle().keywordColor)
  of TtIdentifier:
    result.fgColor(getCurrentCodeStyle().identColor)
  of TtWhiteSpace, TtNewLine, TtSof, TtEof, ErrorTok:
    discard

proc toRope*(tokens: seq[Con4mToken]): Rope =
  var cells: seq[seq[Rope]] = @[@[atom("Number"), atom("Type"), atom("Line"),
                                atom("Col"), atom("Value")]]

  for i, item in tokens:
    var row: seq[Rope] = @[]
    row.add(atom($(i + 1)))
    row.add(atom($(item.kind)))
    row.add(atom($(item.lineNo)))
    row.add(atom($(item.lineOffset)))
    row.add(item.toRope())
    cells.add(row)

  return quickTable(cells)

proc `$`*(kind: Con4mTokenKind): string =
  case kind
    of TtSemi:
      return ";"
    of TtNewLine:
       return "a newline"
    of TtLockAttr:
       return "~"
    of TtPlus:
      return "+"
    of TtMinus:
      return "-"
    of TtMul:
      return "*"
    of TtDiv:
      return "/"
    of TtMod:
      return "%"
    of TtLte:
      return "<="
    of TtLt:
      return "<"
    of TtGte:
      return ">="
    of TtGt:
      return ">"
    of TtNeq:
      return "!="
    of TtNot:
      return "not"
    of TtLocalAssign:
      return ":="
    of TtColon:
      return ":"
    of TtAttrAssign:
      return "= or :"
    of TtCmp:
      return "=="
    of TtComma:
      return ","
    of TtPeriod:
      return "."
    of TtLBrace:
      return "{"
    of TtRBrace:
      return "}"
    of TtLBracket:
      return "["
    of TtRBracket:
      return "]"
    of TtLParen:
      return "("
    of TtRParen:
      return ")"
    of TtAnd:
      return "and"
    of TtOr:
      return "or"
    of TtBitAnd:
      return "&"
    of TtBitOr:
      return "|"
    of TtBitXor:
      return "^"
    of TtShl:
      return "<<"
    of TtShr:
      return ">>"
    of TtIntLit:
      return "an integer"
    of TtFloatLit:
      return "a float"
    of TtStringLit:
      return "a string"
    of TtCharLit:
      return "a character"
    of TtTrue:
      return "true"
    of TtFalse:
      return "false"
    of TtIf:
      return "if"
    of TtElif:
      return "elif"
    of TtElse:
      return "else"
    of TtFor:
      return "for"
    of TtBreak:
      return "break"
    of TtContinue:
      return "continue"
    of TtReturn:
      return "return"
    of TtEnum:
      return "enum"
    of TtIdentifier:
      return "an identifier"
    of TtFunc:
      return "func"
    of TtVar:
      return "var"
    of TtGlobal:
      return "global"
    of TtConst:
      return "const"
    of TtOtherLit:
      return "a << literal >>"
    of TtBacktick:
      return "`"
    of TtArrow:
      return "->"
    of TtObject:
      return "object"
    of TtIn:
      return "in"
    else:
      return "other (id = " & $(int(kind)) & ")"
