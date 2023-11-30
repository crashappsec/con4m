## Lexical analysis.  Should be straightforward.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import unicode, nimutils, types, errmsg, strcursor, style

template lexFatal(msg: string, t: Con4mToken) =
  var st = when not defined(release): getStackTrace() else: ""
  fatal(msg, t, st, instantiationInfo())
    
proc uEsc(s: seq[Rune], numchars: int, t: Con4mToken): Rune =
  var
    c = 0
    n = numchars
    i = 0
  if len(s) < n:
    lexFatal("Unterminated escape sequence in literal", t)
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
      lexFatal("Invalid unicode escape in literal", t)

proc processEscape(s: seq[Rune], t: Con4mToken): (Rune, int) =
  # Returns the escaped character along with how many bytes were read.
  case s[0]
  of Rune('n'):  return (Rune('\n'), 1)
  of Rune('r'):  return (Rune('\r'), 1)
  of Rune('a'):  return (Rune('\a'), 1)
  of Rune('b'):  return (Rune('\b'), 1)
  of Rune('f'):  return (Rune('\f'), 1)
  of Rune('t'):  return (Rune('\t'), 1)
  of Rune('\\'): return (Rune('\\'), 1)
  of Rune('x'):  return (uEsc(s[1 .. ^1], 2, t), 3)
  of Rune('u'):  return (uEsc(s[1 .. ^1], 4, t), 5)
  of Rune('U'):  return (uEsc(s[1 .. ^1], 8, t), 9)
  else: return (s[0], s[0].size())

proc parseCodePoint(t: Con4mToken) =
  # Extract the actual codepoint from a char literal. The first
  # and last chars will be the tick marks.
  let
    raw = t.cursor.slice(t.startPos + 1, t.endPos - 1)

  if len(raw) == 0:
    t.codePoint = 0
    return

  case raw[0]
  of Rune('\\'):
    let (cp, n) = processEscape(raw[1 .. ^1], t)
    if (n + 2) != len(raw):
      lexFatal("Invalid escape in string literal", t)
    t.codepoint = int(cp)
  else:
    t.codepoint = int(raw[0])

proc unescape(token: Con4mToken) =
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
        lexFatal("Invalid unicode escape in string literal", token)

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
    lexFatal("Unterminated escape sequence in string literal", token)

  token.unescaped = $(res)

template atNewLine() =
  lineNo.inc()
  lineStart = s.getPosition()

template tok(k: Con4mTokenKind, eatNewlines: static[bool] = false) =
  toks.tokens.add(Con4mToken(startPos:   startPos,
                             id:         nextId,
                             endPos:     s.getPosition(),
                             kind:       k,
                             cursor:     s,
                             lineNo:     tokenLine,
                             lineOffset: tokenLineOffset))
  nextId += 1
  when eatNewlines:
    let wsStart = s.getPosition()
    while s.peek() in [Rune(' '), Rune('\t')]:
      s.advance()
    if s.peek() == Rune('\n'):
      atNewLine()
      s.advance()
      while s.peek() in [Rune(' '), Rune('\t')]:
        s.advance()
    let wsEnd = s.getPosition()
    if wsEnd != wsStart:
      let wsTok = Con4mToken(startPos: wsStart, endPos: wsEnd, id: nextId,
                             kind: TtWhiteSpace, cursor: s, lineNo: tokenLine,
                             lineOffSet: tokenLineOffset + wsStart - startPos)
      toks.tokens.add(wsTok)
      nextId += 1

# The adjustment is to chop off start/end delimiters for
# literals... strings, tristrings, and 'other' literals: << >>
template tok(k: Con4mTokenKind, adjustValue: static[int], 
             frontOnly: static[bool] = false) =

  when frontOnly:
    toks.tokens.add(Con4mToken(startPos:   startPos + adjustValue,
                               endPos:     s.getPosition(),
                               kind:       k,
                               id:         nextId,
                               cursor:     s,
                               lineNo:     tokenLine,
                               adjustment: adjustValue,
                               lineOffset: tokenLineOffset))
  else:
    toks.tokens.add(Con4mToken(startPos:   startPos + adjustValue,
                               endPos:     s.getPosition() - adjustValue,
                               kind:       k,
                               id:         nextId,
                               cursor:     s,
                               lineNo:     tokenLine,
                               adjustment: adjustValue,
                               lineOffset: tokenLineOffset))
  nextId += 1

proc processStrings(inToks: seq[Con4mToken]): seq[Con4mToken] =
  var
    i                  = 0
    newtok: Con4mToken = nil

  if len(inToks) == 0:
    return

  for tok in inToks:
    case tok.kind
    of TtStringLit: tok.unescape()
    of TtCharLit:   tok.parseCodePoint()
    else: discard

  while i < len(inToks):
   block outer:
    var t = inToks[i]

    case t.kind
    of TtStringLit:
      var j = i + 1
      # (TtWhiteSpace* TtPlus (TtWhiteSpace|TtNewLine)* TtStringLit)*
      while j < len(inToks):
        block inner:
          let t2 = inToks[j]
          case t2.kind
          of TtWhiteSpace:
            j += 1
            continue
          of TtPlus:
            j += 1
            while j < len(inToks):
              let t3 = inToks[j]
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
                  result.add(newTok)
                  newTok = nil
                  break outer

                result.add(t)
                i = i + 1
                break outer
          else:
            if newTok != nil:
              result.add(newTok)
              newTok = nil
              break outer
            result.add(t)
            i = i + 1
            break outer

    else:
      result.add(t)
    i += 1

template handleLitMod() =
  if s.peek() == Rune('\''):
    s.advance()
  
    var 
      r:        Rune = s.read()
      modifier: seq[Rune]

    if not r.isIdStart():
      tok(ErrorLitMod)
    else:
      modifier.add(r)

      while true:
        let r = s.peek()

        if not r.isIdContinue():
          break

        modifier.add(r)
        s.advance()

      toks.tokens[^1].litType = $(modifier)

proc lex*(s: StringCursor): (bool, TokenBox) =
  ## Lexical analysis. Doesn't need to be exported.

  var
    nextId: int = 1
    lineNo: int = 1
    lineStart: int = s.getPosition()
    toks = TokenBox(tokens: @[Con4mToken(startPos: -1, endPos: -1,
                              kind: TtSof, cursor: s, lineNo: -1,
                              lineOffSet: -1)])

  while true:
    let
      startPos = s.getPosition()
      tokenLine = lineNo
      tokenLineOffset = startPos - lineStart
      c = s.read()
    case c
    of Rune(' '), Rune('\t'):
      while true:
        let c = s.peek()
        case c
        of Rune(' '), Rune('\t'):
          s.advance()
        else:
          break
      tok(TtWhiteSpace)
      continue
    of Rune('\r'):
      let c = s.read()
      if c != Rune('\n'):
        tok(ErrorTok)
        return (false, toks)
      tok(TtNewLine)
      atNewLine()
    of Rune('\n'):
      tok(TtNewLine)
      atNewLine()
    of Rune('#'):
      while true:
        case s.peek()
        of Rune('\n'), Rune('\x00'):
          break # The newline should be a separate token.
        else:
          s.advance()
      tok(TtLineComment, 1, frontOnly = true)
    of Rune('~'):
      tok(TtLockAttr)
    of Rune('`'):
      tok(TtBacktick)
    of Rune('+'):
      tok(TtPlus, true)
    of Rune('-'):
      case s.peek()
      of Rune('>'):
        s.advance()
        tok(TtArrow, true)
      else:
        tok(TtMinus, true)
    of Rune('*'):
      tok(TtMul, true)
    of Rune('/'):
      case s.peek()
      of Rune('/'):
        while true:
          case s.peek()
          of Rune('\n'), Rune('\x00'):
            break
          else:
            s.advance()
        tok(TtLineComment, 2, frontOnly = true)
      of Rune('*'):
        s.advance()
        while true:
          case s.read()
          of Rune('\n'):
            atNewLine()
          of Rune('*'):
            if s.peek() == Rune('/'):
              tok(TtLongComment, 2)
              s.advance()
              break
          of Rune('\x00'):
            tok(ErrorLongComment)
            return (false, toks)
          else:
            discard
      else:
        tok(TtDiv, true)
    of Rune('%'):
      tok(TtMod, true)
    of Rune('<'):
      if s.peek() == Rune('<'):
        while true:
          case s.read()
          of Rune('\x00'):
            tok(ErrorOtherLit)
            return (false, toks)
          of Rune('>'):
            if s.read() != Rune('>'): 
              continue
            tok(TtOtherLit, 2)
            handleLitMod()
            break
          else:
            continue
      elif s.peek() == Rune('='):
        s.advance()
        tok(TtLte, true)
      else:
        tok(TtLt, true)
    of Rune('>'):
      if s.peek() == Rune('='):
        s.advance()
        tok(TtGte, true)
      else:
        tok(TtGt, true)
    of Rune('!'):
      if s.peek() == Rune('='):
        s.advance()
        tok(TtNeq, true)
      else:
        tok(TtNot, true)
    of Rune(';'):
      tok(TtSemi, true)
    of Rune(':'):
      if s.peek() == Rune('='):
        s.advance()
        tok(TtLocalAssign, true)
      else:
        tok(TtColon, true)
    of Rune('='):
      case s.peek()
      of Rune('='):
        s.advance()
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
    of Rune('['):
      tok(TtLBracket, true)
    of Rune(']'):
      tok(TtRBracket)
    of Rune('('):
      tok(TtLParen, true)
    of Rune(')'):
      tok(TtRParen)
    of Rune('&'):
      if s.read() != Rune('&'):
        tok(ErrorTok)
        return (false, toks)
      else:
        tok(TtAnd, true)
    of Rune('|'):
      if s.read() != Rune('|'):
        tok(ErrorTok)
        return (false, toks)
      else:
        tok(TtOr, true)
    of Rune('0') .. Rune('9'):
      block numLit:
        var isFloat: bool

        while true:
          case s.peek()
          of Rune('0') .. Rune('9'):
            s.advance()
          else:
            break

        if s.peek() == Rune('.'):
          s.advance()
          isFloat = true
          case s.read()
          of Rune('0') .. Rune('9'):
            while true:
              case s.peek()
              of Rune('0') .. Rune('9'):
                s.advance()
              of Rune('.'):
                s.advance()
                while true:
                  case s.peek()
                  of Rune(' '), Rune('\n'), Rune('\x00'):
                    tok(TtOtherLit)
                    handleLitMod()
                    break numLit
                  else:
                    s.advance()
              else:
                break
          else:
            tok(ErrorTok)
            return (false, toks) # Grammar doesn't allow no digits after dot.
        case s.peek():
          of Rune('e'), Rune('E'):
            s.advance()
            isFloat = true
            case s.peek()
            of Rune('+'), Rune('-'), Rune('0') .. Rune('9'):
              s.advance()
            else:
              tok(ErrorTok)
              return (false, toks) # e or E without numbers after it
            while true:
              case s.peek()
              of Rune('0') .. Rune('9'):
                s.advance()
              else:
                break
          of Rune(':'):
            var
              savedPosition = s.getPosition()
              flag          = false

            s.advance()
            while true:
              case s.peek()
              of Rune(':'):
                flag = true
              of Rune(' '), Rune('\n'), Rune('\x00'):
                if flag:
                  tok(TtOtherLit)
                  handleLitMod()
                else:
                  s.setPosition(savedPosition)
                  tok(TtIntLit)
                  handleLitMod()
                break numLit
              else:
                s.advance()
          else:
            discard
        if isFloat: tok(TtFloatLit) else: tok(TtIntLit)
        handleLitMod()
    of Rune('\''):
      case s.read()
      of Rune('\''):
        tok(TtCharLit)
        handleLitMod()
      of Rune('\\'): # Skip next rune, then till we find ' (We will parse later)
        discard s.read()
        while true:
          case s.read()
          of Rune('\x00'):
            tok(ErrorCharLit)
            s.setPosition(startPos)
            return (false, toks)
          # of Rune('\\'):  Never valid in a char literal.
          of Rune('\''):
            tok(TtCharLit)
            handleLitMod()
            break
          else: continue
      else:
        if s.read() != Rune('\''):
          tok(ErrorCharLit)
          s.setPosition(startPos)
          return (false, toks)
        tok(TtCharLit)
        handleLitMod()
    of Rune('"'):
      var tristring = false
      
      if s.peek() == Rune('"'):
        s.advance()
        if s.peek() == Rune('"'):
          s.advance()
          tristring = true
        else:
          tok(TtStringLit, 1)
          handleLitMod()
          continue
      while true:
        let r = s.read()
        case r
        of Rune('\\'):
          discard s.read()
        of Rune('"'):
          if not tristring:
            tok(TtStringLit, 1)
            handleLitMod()
            break
          if s.peek() != Rune('"'):
            continue
          s.advance()
          if s.peek() != Rune('"'):
            continue
          s.advance()
          tok(TtStringLit, 3)
          handleLitMod()
          break
        of Rune('\x00'):
          tok(ErrorStringLit)
          s.setPosition(startPos)
          return (false, toks)
        of Rune('\n'):
          if tristring:
            atNewLine()
            continue
          tok(ErrorStringLit)
          s.setPosition(startPos)
          return (false, toks)
        else:
          continue
    of Rune('\x00'):
      tok(TtEOF)
      toks.tokens = processStrings(toks.tokens)
      return (true, toks)
    else:
      var 
        r: Rune

      if uint(c) < 128:
        r = c
      else:
        s.setPosition(s.getPosition() - 1)
        r = s.read()
      if not r.isIdStart() and r != Rune('$') and r != Rune('?'):
        tok(ErrorTok)
        return (false, toks)
      while true:
        r = s.peek()
        case r
        of Rune('$'):
          discard s.read()
        of Rune('?'):
          discard s.read()
          break
        else:
          if r.isIdContinue():
            discard s.read()
          else:
            break
          
      case $(s.slice(startPos, s.getPosition()))
      of "var":            tok(TtVar)
      of "True", "true":   tok(TtTrue)
      of "False", "false": tok(TtFalse)
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
      of "export":         tok(TtExportVar)
      of "struct":         tok(TtObject)
      else:                tok(TtIdentifier)

  unreachable

proc lex*(str: string): (bool, TokenBox) =
  return str.newStringCursor().lex()

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
  of ErrorLongComment: result = "~unterm comment~"
  of ErrorStringLit:   result = "~unterm string~"
  of ErrorCharLit:     result = "~bad char lit~"
  of ErrorOtherLit:    result =  "~unterm other lit~"
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
  of TtIntLit, TtFloatLit:
    result.fgColor(getCurrentCodeStyle().numericLitColor)
  of TtTrue, TtFalse:
    result.fgColor(getCurrentCodeStyle().boolLitColor)
  of TtOtherLit:
    result.fgColor(getCurrentCodeStyle().otherLitColor)
  of TtMul, TtDiv, TtMod, TtLte, TtLt, TtGte, TtGt, TtCmp, TtNeq, TtPlus,
     TtLocalAssign, TtAttrAssign, TtComma, TtPeriod, TtSemi, TtMinus, 
     TtLockAttr, TtBacktick, TtArrow, TtColon:
    result.fgColor(getCurrentCodeStyle().operatorColor)
  of TtLineComment, TtLongComment:
    result.fgColor(getCurrentCodeStyle().commentColor)
  of TtLBrace, TtRBrace, TtLBracket, TtRBracket, TtLParen, TtRParen:
    result.fgColor(getCurrentCodeStyle().otherDelimColor)
  of TtExportVar, TtNot, TtAnd, TtOr, TtIf, TtElIf, TTElse, TtFor, TtFrom,
     TtTo, TtBreak, TtContinue, TtReturn, TtEnum, TtFunc, TtVar, TtObject,
     TtWhile:
    result.fgColor(getCurrentCodeStyle().keywordColor)
  of TtIdentifier:
    result.fgColor(getCurrentCodeStyle().identColor)
  of TtWhiteSpace, TtNewLine, TtSof, TtEof, ErrorTok, ErrorLongComment,
       ErrorStringLit, ErrorCharLit, ErrorOtherLit, ErrorLitMod:
    discard

proc toRope*(tokenBox: TokenBox): Rope =
  var cells: seq[seq[Rope]] = @[@[atom("Number"), atom("Type"), atom("Line"), 
                                atom("Col"), atom("Value")]]

  for i, item in tokenBox.tokens:
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
    of TtExportVar:
      return "export"
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
    of TtOtherLit:
      return "a << literal >>"
    of TtBacktick:
      return "`"
    of TtArrow:
      return "->"
    of TtObject:
      return "object"
    else:
      return "other (id = " & $(int(kind)) & ")"

template tokAt*(box: var TokenBox, i: int): Con4mToken =
  if i >= box.tokens.len():
    box.tokens[^1]
  else:
    box.tokens[i]

when isMainModule:
  useCrashTheme()
  import std/marshal
  let f = "x := x + 2"
  let (success, tokens) = f.lex()

  if not success:
    quit()

  print tokens.toRope()

  let marshalled = $$tokens

  print marshalled

  let tokens2 = to[seq[Con4mToken]](marshalled)

  print tokens2.toRope()
