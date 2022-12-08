# Con4m grammar

Major lexical elements are defined in lex.md, and are all-uppercase
here. Minor ones are inlined, in double quotes. Note that all comment
and white space tokens other than newlines are ignored, and newlines
are ignored in most expression contexts.

```ebnf
top           ::= (coreBodyItems | enum) *
body          ::= coreBodyItems *
coreBodyItems ::= attrAssign | varAssign | section | ifStmt | forStmt |
                   whileStmt | breakStmt | expression (NL|";")+
enum          ::= "enum" ID ("," ID)*		   
attrAssign    ::= ID ("="|":") expression (NL|";")+
varAssign     ::= ID ":=" expression (NL|";")+
section       ::= ID (STR | ID)* "{" body "}" 
ifStmt        ::= "if" expression "{" body "}" 
                  ("elif" expression "{" body "}")*
	 	  ("else" expression "{" body" "}")?
forStmt       ::= "for" ID "from" expression "to" expression "{" body "}"
# whileStmt     ::= "while" expression "{" body "}" removed b/c can't prove temrination
breakStmt     ::= "break" (";")?
exprStart     ::= unaryExpr | notExpr | literal | accessExpr
unaryExpr     ::= ("+" | "-") (literal | accessExpr)
notExpr       ::= ("!" | "not") expression
literal       ::= NUM | STR | listLiteral | dictLiteral | TRUE | FALSE | NULL
accessExpr    ::= (ID | parenExpr) (memberExpr | indexExpr | callActuals)* 
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
```
