## Lexical analysis.  Should be straightforward.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import streams, unicode, types, nimutils, errmsg

proc unescape(token: Con4mToken) =
  # Turn a raw string into its intended representation.  Note that we
  # do NOT currently accept hex or octal escapes, since strings
  # theoretically should always be utf-8 only.
  var
    flag:      bool
    remaining: int
    codepoint: int
    raw:       string = newStringOfCap(token.endPos - token.startPos)
    res:       string = newStringOfCap(token.endPos - token.startPos)

  token.stream.setPosition(token.startPos)
  raw = token.stream.readStr(token.endPos - token.startPos)

  for r in raw.runes():
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
        var   st: string
        const ii = instantiationInfo()
        echo "??!"
        when not defined(release):
          st = getStackTrace()
        else:
          st = ""
        fatal("Invalid unicode escape in string literal", token, st, ii)

      remaining -= 1
      if remaining == 0:
        res.add(Rune(codepoint))
        codepoint = 0
    elif flag:
      case r
      of Rune('n'):
        res.add('\n')
        flag = false
      of Rune('r'):
        res.add('\r')
        flag = false
      of Rune('a'):
        res.add('\a')
        flag = false
      of Rune('b'):
        res.add('\b')
        flag = false
      of Rune('f'):
        res.add('\f')
        flag = false
      of Rune('t'):
        res.add('\t')
        flag = false
      of Rune('\\'):
        res.add('\\')
        flag = false
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
    var   st: string
    const ii = instantiationInfo()

    when not defined(release):
      st = getStackTrace()
    else:
      st = ""
    fatal("Unterminated escape sequence in string literal", token, st, ii)

  token.unescaped = res


template tok(k: Con4mTokenKind) =
  toks.add(Con4mToken(startPos: startPos,
                    endPos: s.getPosition(),
                    kind: k,
                    stream: s,
                    lineNo: tokenLine,
                    lineOffset: tokenLineOffset))

# The adjustment is to chop off start/end delimiters for
# literals... strings, tristrings, and 'other' literals: << >>
template tok(k: Con4mTokenKind, adjustment: int) =
  toks.add(Con4mToken(startPos: startPos + adjustment,
                      endPos: s.getPosition() - adjustment,
                      kind: k,
                      stream: s,
                      lineNo: tokenLine,
                      lineOffset: tokenLineOffset))

template atNewLine() =
  lineNo.inc()
  lineStart = s.getPosition()

proc processStrings(inToks: seq[Con4mToken]): seq[Con4mToken] =
  var
    i      = 0
    newtok: Con4mToken = nil

  result = @[]

  if len(inToks) != 0:
    let savedPos = inToks[0].stream.getPosition()
    for tok in inToks:
      if tok.kind == TtStringLit:
        tok.unescape()
    inToks[0].stream.setPosition(savedPos)

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
                                      stream:     t.stream)
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


proc lex*(s: Stream): (bool, seq[Con4mToken]) =
  ## Lexical analysis. Doesn't need to be exported.
  var
    lineNo: int = 1
    lineStart: int = s.getPosition()
    toks = @[Con4mToken(startPos: -1, endPos: -1, kind: TtSof, stream: s,
                       lineNo: -1, lineOffSet: -1)]

  while true:
    let
      startPos = s.getPosition()
      tokenLine = lineNo
      tokenLineOffset = startPos - lineStart
      c = s.readChar()
    case c
    of ' ', '\t':
      while true:
        let c = s.peekChar()
        case c
        of ' ', '\t':
          discard s.readChar()
        else:
          break
      tok(TtWhiteSpace)
      continue
    of '\r':
      let c = s.readChar()
      if c != '\n':
        tok(ErrorTok)
        return (false, toks)
      tok(TtNewLine)
      atNewLine()
    of '\n':
      tok(TtNewLine)
      atNewLine()
    of '#':
      while true:
        case s.peekChar()
        of '\n', '\x00':
          break # The newline should be a separate token.
        else:
          discard s.readChar()
      tok(TtLineComment)
    of '~':
      tok(TtLockAttr)
    of '$':
      tok(TtExportVar)
    of '+':
      tok(TtPlus)
    of '-':
      tok(TtMinus)
    of '*':
      tok(TtMul)
    of '/':
      case s.peekChar()
      of '/':
        while true:
          case s.peekChar()
          of '\n', '\x00':
            break
          else:
            discard s.readChar()
        tok(TtLineComment)
      of '*':
        discard s.readChar()
        while true:
          case s.readChar()
          of '\n':
            atNewLine()
          of '*':
            if s.peekChar() == '/':
              tok(TtLongComment)
              discard s.readChar()
              break
          of '\x00':
            tok(ErrorLongComment)
            return (false, toks)
          else:
            discard
      else:
        tok(TtDiv)
    of '%':
      tok(TtMod)
    of '<':
      if s.peekChar() == '<':
        while true:
          case s.readChar()
          of '\x00':
            tok(ErrorOtherLit)
            return (false, toks)
          of '>':
            if s.readChar() != '>': continue
            tok(TtOtherLit, 2)
            break
          else:
            continue
      elif s.peekChar() == '=':
        discard s.readChar()
        tok(TtLte)
      else:
        tok(TtLt)
    of '>':
      if s.peekChar() == '=':
        discard s.readChar()
        tok(TtGte)
      else:
        tok(TtGt)
    of '!':
      if s.peekChar() == '=':
        discard s.readChar()
        tok(TtNeq)
      else:
        tok(TtNot)
    of ';':
      tok(TtSemi)
    of ':':
      if s.peekChar() == '=':
        discard s.readChar()
        tok(TtLocalAssign)
      else:
        tok(TtColon)
    of '=':
      case s.peekChar()
      of '=':
        discard s.readChar()
        tok(TtCmp)
      else:
        tok(TtAttrAssign)
    of ',':
      tok(TtComma)
    of '.':
      tok(TtPeriod)
    of '{':
      tok(TtLBrace)
    of '}':
      tok(TtRBrace)
    of '[':
      tok(TtLBracket)
    of ']':
      tok(TtRBracket)
    of '(':
      tok(TtLParen)
    of ')':
      tok(TtRParen)
    of '&':
      if s.readChar() != '&':
        tok(ErrorTok)
        return (false, toks)
      else:
        tok(TtAnd)
    of '|':
      if s.readChar() != '|':
        tok(ErrorTok)
        return (false, toks)
      else:
        tok(TtOr)
    of '0' .. '9':
      block numLit:
        var isFloat: bool

        while true:
          case s.peekChar()
          of '0' .. '9':
            discard s.readChar()
          else:
            break

        if s.peekChar() == '.':
          discard s.readChar()
          isFloat = true
          case s.readChar()
          of '0' .. '9':
            while true:
              case s.peekChar()
              of '0' .. '9':
                discard s.readChar()
              of '.':
                discard s.readChar()
                while true:
                  case s.peekChar()
                  of ' ', '\n', '\x00':
                    tok(TtOtherLit)
                    break numLit
                  else:
                    discard s.readChar()
              else:
                break
          else:
            tok(ErrorTok)
            return (false, toks) # Grammar doesn't allow no digits after dot.
        case s.peekChar():
          of 'e', 'E':
            discard s.readChar()
            isFloat = true
            case s.peekChar()
            of '+', '-', '0' .. '9':
              discard s.readChar()
            else:
              tok(ErrorTok)
              return (false, toks) # e or E without numbers after it
            while true:
              case s.peekChar()
              of '0' .. '9':
                discard s.readChar()
              else:
                break
          else:
            discard
        if isFloat: tok(TtFloatLit) else: tok(TtIntLit)
    of '"':
      var tristring = false

      if s.peekChar() == '"':
        discard s.readChar()
        if s.peekChar() == '"':
          discard s.readChar()
          tristring = true
        else:
          tok(TtStringLit, 1)
          continue # Back to the top
      while true:
        case char(s.readRune().ord())
        of '\\':
          discard s.readRune()
        of '"':
          if not tristring:
            tok(TtStringLit, 1)
            break
          if s.peekChar() != '"':
            continue
          discard s.readChar()
          if s.peekChar() != '"':
            continue
          discard s.readChar()
          tok(TtStringLit, 3)
          tristring = false
          break
        of '\x00', '\n':
          if tristring:
            continue
          tok(ErrorStringLit)
          s.setPosition(startPos)
          return (false, toks)
        else:
          continue
    of '\x00':
      tok(TtEOF)
      return (true, processStrings(toks))
    else:
      var r: Rune
      if uint(c) < 128:
        r = Rune(c)
      else:
        s.setPosition(s.getPosition() - 1)
        r = s.readRune()
      if not r.isIdStart():
        tok(ErrorTok)
        return (false, toks)
      while true:
        r = s.peekRune()
        if r.isIdContinue():
          discard s.readRune()
        else:
          break

      let pos = s.getPosition()
      s.setPosition(startPos)
      let txt = s.readStr(pos - startPos)
      s.setPosition(pos)

      case txt
      of "var": tok(TtVar)
      of "True", "true": tok(TtTrue)
      of "False", "false": tok(TtFalse)
      of "is": tok(TtCmp)
      of "and": tok(TtAnd)
      of "or": tok(TtOr)
      of "not": tok(TtNot)
      of "if": tok(TtIf)
      of "elif": tok(TtElIf)
      of "else": tok(TtElse)
      of "for": tok(TtFor)
      of "from": tok(TtFrom)
      of "to": tok(TtTo)
      of "break": tok(TtBreak)
      of "continue": tok(TtContinue)
      of "return": tok(TtReturn)
      of "enum": tok(TtEnum)
      of "func": tok(TtFunc)
      of "callback": tok(TtCallback)
      else: tok(TtIdentifier)

  unreachable

when isMainModule:
  let s = """
io_mode = "async"

service "http" "web_proxy" {
  listen_addr = "127.0.0.1:8080"

  process "main" {
    command = ["/usr/local/bin/awesome-app", "server"]
  }

  process "mgmt" {
    command = ["/usr/local/bin/awesome-app", "mgmt"]
  }
}
"""

  let (_, toks) = s.newStringStream().lex()

  for t in toks:
    stdout.write($t)
