## This will evolve.

import pkg/nimutils

type
  CodeStyle* = object
    stringLitColor*:   string = "light sky blue"
    charLitColor*:     string = "jazzberry"
    boolLitColor*:     string = "gainsboro"
    numericLitColor*:  string = "jazzberry"
    otherLitColor*:    string = "jazzberry"
    litModColor*:      string = "atomic lime"
    typeNameColor*:    string = "jazzberry"
    keywordColor*:     string = "atomic lime"
    funcNameColor*:    string = "fandango"
    identColor*:       string = "gainsboro"
    operatorColor*:    string = "light slate gray"
    commentColor*:     string = "atomic lime"
    otherDelimColor*:  string = "" # Braces, bra
    blockIndent*:      int    = 4
    brB4Bracket*:      bool   = false
    prefWidth*:        int    = 80
    forcedAttrChar*:   string = ""
    padIndexOps*:      bool   = false
    padCallArgs*:      bool   = false
    breaksAfterSec*:   int    = 1 # EXTRA breaks, not total breaks.
    breaksAfterFunc*:  int    = 1
    breaksBe4Comment*: int    = 1 # Not for inline comments.
    breaksAfterBlock*: int    = 0
    spaceB4CallArgs*:  bool   = false
    comma*:            Rich

let defaultCodeStyle = CodeStyle()

var codeStyle = defaultCodeStyle

proc setCodeStyle*(cs: CodeStyle) =
  codeStyle = cs

proc getCurrentCodeStyle*(): CodeStyle =
  return codeStyle
