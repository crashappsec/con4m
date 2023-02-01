<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Con4m Language syntax reference](#con4m-language-syntax-reference)
- [EBNF specification](#ebnf-specification)
- [Major Lexical elements](#major-lexical-elements)
- [Notes](#notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Con4m Language syntax reference

This document is a syntax reference for the Con4m language.   It’s a simple syntax that anyone with even minimal programming experience should be able to pick up.

The language is designed to be fast, and currently does not have any features that might lead to an infinite loop (or infinite recursion). If such features are added, they will be optional.

[Go back to the home page.](https://github.com/crashappsec/con4m)

# EBNF specification

Major lexical elements are  are all-uppercase here. Minor ones are inlined, in double quotes. Note that all comment and white space tokens, other than newlines, are always ignored.  Newlines
are ignored in most expression contexts (they’re only used to separate statements where there would otherwise be ambiguity).

```ebnf
top           ::= (sectBodyItems | enum | fnOrCallback) *
body          ::= sectBodyItems *
coreBodyItems ::= attrAssign | varAssign | ifStmt | forStmt | continueStmt |
                   breakStmt | returnStmt | expression (NL|";")+
sectBodyItems ::= coreBodyItems | section
enum          ::= "enum" ID ("," ID)*
attrAssign    ::= ("~")? ID("." ID)* ("="|":") expression (NL|";")+
varAssign     ::= ("$")? ID ("," ID)* ":=" expression (NL|";")+
section       ::= ID (STR | ID)? "{" body "}"
ifStmt        ::= "if" expression "{" body "}"
                  ("elif" expression "{" body "}")*
	 	              ("else" expression "{" body" "}")?
forStmt       ::= "for" ID "from" expression "to" expression "{" body "}"
continueStmt  ::= "continue" (";")?
breakStmt     ::= "break" (";")?
returnStmt    ::= "return" expression? (";")?
fnOrCallback  ::= ("func" | "callback") ID formalSpec fnBody
formalSpec    ::= "(" (ID? ("," ID)* ")"
fnBody        ::= "{" coreBodyItems* "}"
# Note that literal matches before accessExpr, so a lparen at an exprStart
# or in a unaryExpr will be treated as a tuple literal.
exprStart     ::= unaryExpr | notExpr | literal | accessExpr
unaryExpr     ::= ("+" | "-") (literal | accessExpr)
notExpr       ::= ("!" | "not") expression
literal       ::= NUM | STR | listLiteral | dictLiteral | TRUE | FALSE | NULL
accessExpr    ::= (ID | parenExpr) (memberExpr | indexExpr | callActuals)*
tupleLiteral  ::= "(" expression ("," expression)*)+ ")"
listLiteral   ::= "[" (expression ("," expression)* )? "]"
dictLiteral   ::= "{" (expression ":" expression
                       ("," expression ":" expression)*) "}"
parenExpr     ::= "(" expression ")"
memberExpr    ::= "." ID
indexExpr     ::= "[" expression "]"
callActuals   ::= "(" (expression ("," expression)* )? ")"
expression    ::= exprStart (orExpr*)
orExpr        ::= ("||" | "or")  expression | andExpr
andExpr       ::= ("&&" | "and") andExprRHS | neExpr
andExprRHS    ::= exprStart (andExpr)*
neExpr        ::= "!=" neExprRHS | eqExpr
neExprRHS     ::= exprStart (neExpr)*
eqExpr        ::= "==" eqExprRHS | gteExpr
eqExprRHS     ::= exprStart (eqExpr)*
gteExpr       ::= ">=" gteExprRHS | lteExpr
gteExprRHS    ::= exprStart (gteExpr)*
lteExpr       ::= "<=" lteExprRHS | gtExpr
lteExprRHS    ::= exprStart (lteExpr)*
gtExpr        ::= ">" gtExprRHS | ltExpr
gtExprRHS     ::= exprStart (gtExpr)*
ltExpr        ::= "<" ltExprRHS | plusExpr
ltExprRHS     ::= exprStart (ltExpr)*
plusExpr      ::= "+" plusExprRHS | minusExpr
plusExprRHS   ::= exprStart (plusExpr)*
minusExpr     ::= "-" minusExprRHS | modExpr
minusExprRHS  ::= exprStart (minusExpr)*
modExpr       ::= "%" modExprRHS | mulExpr
modExprRHS    ::= exprStart (modExpr)*
mulExpr       ::= "*" mulExprRHS | divExpr
mulExprRHS    ::= exprStart (mulExpr)*
divExpr       ::= "/" divExprRHS | accessExpr
divExprRHS    ::= exprStart (divExpr)*
# whileStmt     ::= "while" expression "{" body "}"
```

# Major Lexical elements

Most of the lexical elements are inlined above, exxcept for the following:

```ebnf
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
LINECOMMENT ::= ("#" | "//") [^\n]*
LONGCOMMENT ::= "/*" .* "*/" ; Minimal munch match.
```

# Notes

1. Member access (i.e., dot notation) parses, but is not allowed.  It will be added in, and at that point we will handle more complex assignments (i.e., arrays and dictionaries).
2. I removed the `while` operator, to ensure termination. I’ll probably add it back in as an option.
3. Though not yet in the grammar, I will be adding callbacks (basically non-recursive functions), so that the config file can process data that changes at runtime.
4. Similarly, I will be adding tuples to the language.
5. Strings actually are parsed for escape sequences, and handle both `\uxxxx` and `\Uxxxxxxxx` formats. However, we do not allow hex escapes, since input config files are always expected to be valid Unicode (generally UTF-8).
