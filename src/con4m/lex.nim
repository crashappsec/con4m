## Lexical analysis.  Should be straightforward.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import streams
import unicode

import ./types
import unicodeident

template tok(k: Con4mTokenKind) =
  toks.add(Con4mToken(startPos: startPos,
                    endPos: s.getPosition(),
                    kind: k,
                    stream: s,
                    lineNo: tokenLine,
                    lineOffset: tokenLineOffset))

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
      if s.peekChar() == '=':
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
      return (true, toks)
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
      of "True", "true": tok(TtTrue)
      of "False", "false": tok(TtFalse)
      of "Null", "null": tok(TtNull)
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

  unreachable()

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
