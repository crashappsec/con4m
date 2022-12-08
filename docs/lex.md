# Con4m Lexical tokens

This document enumerates all the lexical elements for SiCL.

```ebnf
; These rules are explicitly used in grammar.md
WS          ::= (" " | "\t")+  ; Whitespace
NL          ::= ("\r\n" | "\n") ; Newline
ID          ::= IdStart (IdContinue)* ; As defined by unicode standard
TRUE        ::= "True" | "true"
FALSE       ::= "False" | "false"
NULL        ::= "Null" | "null"
NUM         ::= ("0".."9")+
                    ("." ("0".."9")+)?
                    (("e"|"E") ("+"|"-")? ("0".."9")+)?
STR         ::= '"' ( ('\\' '"') | [^"\n] )* '"'

; Here are the other tokens, which are just string consts in the
; grammar file, if they appear at all.
LINECOMMENT ::= ("#" | "//") [^\n]*
LONGCOMMENT ::= "/*" .* "*/" ; Minimal munch match.
PLUS        ::= "+"
MINUS       ::= "-"
MUL         ::= "*"
DIV         ::= "/"
MOD         ::= "%"
LT          ::= "<"
GT          ::= ">"
LTE         ::= "<="
GTE         ::= ">="
NOT         ::= "!"
NEQ         ::= "!="
VARASSIGN   ::= ":="
ATTRASSIGN  ::= "=" 
COLON       ::= ":"
CMP         ::= ("==" | "is")
COMMA       ::= ","
LBRACE      ::= "{"
RBRACE      ::= "}"
LBRACKET    ::= "["
RBRACKET    ::= "]"
LPAREN      ::= "("
RPAREN      ::= ")"
AND         ::= ("&&" | "and")
OR          ::= ("||" | "or")
```
