import types, lex, parse, nimutils, style, unicode, strutils

type PrettyState = object
  lastIx:       int
  r:            Rope
  style:        CodeStyle
  pad:          string
  printingType: int

proc pretty(n: Con4mNode, state: var PrettyState)

template prettyDown(stmt = false) =
  for item in n.children:
    if stmt:
      state.r += text(state.pad)
      item.pretty(state)
      state.r += text("\n")
    else:
      item.pretty(state)

template indentDown() =
  var addedPad = ""
  for i in 0 ..< state.style.blockIndent:
    addedPad.add(' ')

  let 
    savedPad = state.pad
    newPad   = state.pad & addedPad

  state.pad = newPad

  prettyDown(true)

  state.pad = savedPad
  state.r += text(state.pad)

template prettyColor(rope: Rope, field: untyped): Rope =
  if state.style.field != "":
    rope.fgColor(state.style.field)
  else:
    rope

template prettySimpleLiteral(field: untyped) =
  var 
    tok      = n.token.get()
    modifier = tok.litType
    txt      = tok.getTokenText(adjust = true)

  if modifier != "":
    txt &= "'" & modifier

  if state.style.field != "":
    state.r += text(txt).fgColor(state.style.field)
  else:
    state.r += text(txt)


proc collectKids(n: Con4mNode, state: var PrettyState): seq[Rope] =
  let savedOut = state.r
  for item in n.children:
    state.r = nil
    item.pretty(state)
    result.add(state.r)

  state.r = savedOut

proc containerLiteral(n:              Con4mNode, 
                      lDelim, rDelim: string, 
                      state:          var PrettyState,
                      initialPad = 2) =
  let 
    kidValues    = n.collectKids(state)
    lFmt         = prettyColor(text(" " & lDelim), otherDelimColor)
    rFmt         = prettyColor(text(rDelim), otherDelimColor)
    oldPad       = state.pad
    spaceToBrace = state.style.prefWidth - state.pad.len()
    spaceOnLine  = spaceToBrace - initialPad - 2
  
  var 
    largestWidth: int
    numCols:      int

  for item in kidValues:
    let w = item.runeLength()
    if w > largestWidth:
      largestWidth = w

  numCols = spaceOnLine div (largestWidth + 2)

  if numCols < 1:
    numCols = 1

  state.r += lFmt

  if numCols < kidValues.len():
    state.pad &= $(Rune(' ').repeat(initialPad))
    
    for i, item in kidValues:
      if i mod numCols == 0:
        state.r += text("\n" & state.pad)
      state.r += item
      if i != kidValues.len() - 1:
        state.r += state.style.comma

      var colPad = largestWidth - item.runeLength()

      if colPad < 0:
        colPad = 0

      state.r += text($(Rune(' ').repeat(colPad)))

    state.pad = oldPad
    state.r += text("\n" & oldPad)+ rFmt

  else:
    for i, item in kidValues:
      state.r += item
      if i != kidValues.len() - 1:
        state.r += state.style.comma
    state.r += rFmt

template keyword(n: Con4mNode): Rope =
  prettyColor(text(n.getTokenText() & " "), keywordColor)

proc handleComments(n: Con4mNode, state: var PrettyState) =
  let curTok = n.token.get()

  var 
    commentSet: seq[seq[Con4mToken]] = @[@[]]
    asString:   seq[string]
    asLines:    seq[seq[string]]
    nlBefore    = false
    prevWasNl   = false

  if curTok.id <= state.lastIx:
    return

  for i in state.lastIx .. curTok.id:
    let tok = n.allTokens.tokAt(i)

    case tok.kind:
      of TtNewLine:
        if commentSet.len() == 1 and commentSet[0].len() == 0:
          nlBefore = true
        else:
          if prevWasNl and commentSet[^1].len() != 0:
            commentSet.add(@[])
        prevWasNl = true
      of TtLineComment:
        prevWasNl   = false
        commentSet[^1].add(tok)
      of TtLongComment:
        prevWasNl = false
        if commentSet[^1].len() == 0:
          commentSet[^1].add(tok)
        else:
          commentSet.add(@[tok])
        commentSet.add(@[])
      else:
        discard

  state.lastIx = curTok.id

  if commentSet.len() == 1 and commentSet[0].len() == 0:
    return

  for num, cset in commentSet:
    # The comment delimiters are gone. If it's a line comment and the
    # first character past the delimiter, there might be another #,
    # and we'd like to remove it.
    #
    # After we look for that, whether we find it or not, if there is
    # a leading space, we'll remove ONE space.
    var oneComment: seq[string]

    for i, item in cset:
      var edited = $item

      if edited.startswith("#"):
        edited = edited[1 .. ^1]
      if edited.startswith(" "):
        edited = edited[1 .. ^1]
      oneComment.add(edited)

    asString.add(oneComment.join("\n"))
  
  let width = state.style.prefWidth - state.pad.len() - 6

  if asString.len() == 0:
    return

  echo asString

  for i in 0 ..< asString.len():
    for line in asString[i].split("\n"):
      asLines.add(line.indentWrap(width, hangingIndent = 0).split("\n"))
  
  echo asLines

  if asString.len() == 1 and not nlBefore and not prevWasNl:
    var newlines: seq[string] = @[]

    if asLines[0].len() == 1:
      newlines.add("/* " & aslines[0]  & " */")
    else:
      for i, line in asLines[0]:
        if i == 0:
          newlines.add("/*")
          newlines.add(" * " & line)
        elif i == asLines[0].len() - 1:
          newlines.add(" * " & line)
          newlines.add(" */")
        else:
          newlines.add(" * ")
    asLines = @[newLines]
  else:
    for i, comment in asLines:
      var newLines: seq[string]
      for line in comment:
        newLines.add("# " & line)
      asLines[i] = newLines
    
  # I'm not combining these loops to keep the code more understandable.
  for i, comment in asLines:
    var newLines: seq[string]

    for j, line in comment:
      if i == 0 and j == 0 and not nlBefore and curTok.id > 1:
        newLines.add(" " & line)
      else:
        newLines.add(state.pad & line)

    asLines[i] = newLines
    
  for i, comment in asLines:
    if i == 0:
      if not prevWasNl:
        let atoms = state.r.allAtoms

        # If this comment was at the end of a line, we might have
        # added a newline we shouldn't have.
        if len(atoms) != 0:
          var i = len(atoms) - 1
          if atoms[i].text.len() != 0 and atoms[i].text[^1] == Rune('\n'):
              atoms[i].text = atoms[i].text[0 ..< ^1]
          
        state.r += text(comment.join("\n"))
      else:
        state.r += text(comment.join("\n"))
    else:
      state.r += text(comment.join("\n"))
    if i != asLines.len() - 1 or prevWasNl:
      state.r += text("\n")

proc pretty(n: Con4mNode, state: var PrettyState) =
  if n.token.isSome():
    n.handleComments(state)

  case n.kind
  of NodeModule:
    prettyDown(stmt = true)
  of NodeExpression, NodeType, NodeLiteral:
    prettyDown()
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeNot,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv, NodeTypeRef:
    let operands = n.collectKids(state)

    # Unary operands don't print anything on the LHS. NodePlus and
    # NodeMinus could be either unary or binary; There are some
    # unary-only ops in here, in this branch, we add a space after the 
    # op, since they require spaces around them; the ones in the next
    # branch are unary operators where the space is weird.
    
    if operands.len() == 2:
      state.r += operands[0] + text(" ")

    state.r += prettyColor(text(n.getTokenText()), operatorColor)
    state.r += text(" ") + operands[^1]

  of NodeAttrSetLock, NodeTypeVar, NodeTypeVararg:
    state.r += prettyColor(text(n.getTokenText()), operatorColor)
    prettyDown()
  of NodeIdentifier:
    if state.printingType > 0:
      state.r += prettyColor(text(n.getTokenText()), typeNameColor)
    else:
      state.r += prettyColor(text(n.getTokenText()), identColor)
  of NodeBody:
    if n.children.len() != 0:
      let toAdd = 
        if state.style.brB4Bracket:  text("\n" & state.pad & "{") 
        else:                        text(" {\n")

      state.r += prettyColor(toAdd, otherDelimColor)
      indentDown()
      state.r += prettyColor(text("}"), otherDelimColor)

  of NodeStringLit:
    prettySimpleLiteral(stringLitColor)
  of NodeCharLit:
    prettySimpleLiteral(charLitColor)
  of NodeIntLit, NodeFloatLit:
    prettySimpleLiteral(numericLitColor)
  of NodeBoolLit:
    prettySimpleLiteral(boolLitColor)
  of NodeOtherLit:
    prettySimpleLiteral(otherLitColor)
  of NodeDictLit:
    n.containerLiteral("{", "}", state)
  of NodeListLit:
    n.containerLiteral("[", "]", state)
  of NodeTupleLit:
    n.containerLiteral("(", ")", state)
  of NodeUnpack:
    n.containerLiteral("", "", state, 0)
  of NodeKVPair:
    let kidValues = n.collectKids(state)
    state.r += kidValues[0] + prettyColor(text(" : "), otherDelimColor)
    state.r += kidValues[1]
  of NodeIfStmt, NodeElifStmt, NodeWhileStmt:
    state.r += n.keyword()
    n.children[0].pretty(state)
    state.r += text(" ")
    n.children[1].pretty(state)
  of NodeElseStmt:
    state.r += n.keyword()
    prettyDown()
  of NodeForStmt:
    state.r += n.keyword()
    n.children[0].pretty(state)
    state.r += text(" from ")
    n.children[1].pretty(state)
    
    if n.children.len() == 4:
      state.r += text(" to ")
      n.children[2].pretty(state)

    state.r += text(" ")
    n.children[^1].pretty(state)    
  of NodeBreakStmt, NodeContinueStmt:
    state.r += n.keyword()
  of NodeReturnStmt:
    state.r += n.keyword()
    prettyDown()
  of NodeAttrAssign:
    n.children[0].pretty(state)
    if state.style.forcedAttrChar.len() != 0:
      let op = text(" " & state.style.forcedAttrChar & " ")
      state.r += prettyColor(op, operatorColor)
    else:
      let attrChr = n.getTokenText()
      if attrChr == ":":
        state.r += prettyColor(text(": "), operatorColor)
      else:
        state.r += prettyColor(text(" = "), operatorColor)
      n.children[1].pretty(state)
  of NodeVarAssign:
    n.children[0].pretty(state)
    state.r += prettyColor(text(" := "), operatorColor)
    n.children[1].pretty(state)
  of NodeMember:
    let operands = n.collectKids(state)

    state.r += operands[0] + prettyColor(text("."), operatorColor) + operands[1]
  of NodeIndex:
    let ixPad = if state.style.padIndexOps: " " else: ""
    n.children[0].pretty(state)
    state.r += prettyColor(text("[" & ixPad), otherDelimColor)
    n.children[1].pretty(state)
    state.r += prettyColor(text(ixPad & "]"), otherDelimColor)
  of NodeEnumStmt, NodeExportStmt:
    state.r += n.keyword()
    n.containerLiteral("", "", state, 5)
  of NodeTypeSpec:
    state.printingType += 1
    let kids = n.collectKids(state)

    for i, item in kids:
      state.r += item
      if i != kids.len() - 1:
        state.r += prettyColor(text(" or "), operatorColor)

    state.printingType -= 1
  of NodeTypeList, NodeTypeDict, NodeTypeObj, NodeTypeTuple:
    let 
      name    = n.getTokenText()
      kidVals = n.collectKids(state)

    state.r += prettyColor(text(name), typeNameColor)
    state.r += prettyColor(text("["), otherDelimColor)
    for i, item in kidVals:
      state.r += prettyColor(item, typeNameColor)
      if i != kidVals.len() - 1:
        state.r += state.style.comma
    state.r += prettyColor(text("]"), otherDelimColor)
    
  of NodeTypeBuiltin:
    state.r += prettyColor(text(n.getTokenText()), typeNameColor)
  of NodeTypeTypeSpec:
    state.r += prettyColor(text("typespec"), operatorColor)
    if n.children.len() != 0:
      n.containerLiteral("[", "]", state, "typespec".len())
  of NodeParenExpr:
    n.containerLiteral("(", ")", state, 0)
  of NodeTypeFunc:  
    state.r += prettyColor(text("("), otherDelimColor)
    let kidValues = n.collectKids(state)

    for i, item in kidValues[0 ..< ^1]:
      state.r += item
      if i != kidValues.len() - 2:
         state.r += state.style.comma
  
    state.r += prettyColor(text(")"), otherDelimColor)
    state.r += prettyColor(text(" -> "), operatorColor)
  of NodeReturnType:
    if n.children.len() == 0:
      state.r += prettyColor(text("void"), typeNameColor)
    else:
      prettyDown()
  of NodeFormal:
    n.children[0].pretty(state)
    if n.children.len() == 2:
      state.r += prettyColor(text(": "), otherDelimColor)
      n.children[1].pretty(state)
  of NodeFormalList:
    state.r += prettyColor(text("("), otherDelimColor)
    let  kidValues = n.collectKids(state)

    for i, item in kidValues:
      state.r += item
      if i != kidValues.len() - 1:
         state.r += state.style.comma
  
    state.r += prettyColor(text(")"), otherDelimColor)
  of NodeFuncDef:
    state.r += n.keyword()
    state.r += prettyColor(text(n.children[0].getTokenText()), funcNameColor)
    n.children[1].pretty(state)

    if n.children.len() == 4:
      state.r += prettyColor(text(" -> "), operatorColor)
      n.children[2].pretty(state)

    n.children[^1].pretty(state)
    state.r += text($(Rune('\n').repeat(state.style.breaksAfterFunc)))
  of NodeActuals:
    let 
      argPad = if state.style.padCallArgs: " " else: ""
      kids   = n.collectKids(state)

    if kids.len() == 0:
      state.r += prettyColor(text("()"), otherDelimColor)
    else:
      state.r += prettyColor(text("(" & argPad), otherDelimColor)
      for i, item in kids:
        state.r += item
        if i != kids.len() - 1:
          state.r += prettyColor(text(", "), otherDelimColor)
      state.r += prettyColor(text(argPad & ")"), otherDelimColor)
  of NodeCall:
    let kidStuff = n.collectKids(state)

    kidStuff[0].fgColor(state.style.funcNameColor)
    state.r += kidStuff[0]
    
    if state.style.spaceB4CallArgs:
      state.r += text(" ")

    state.r += kidStuff[1]
  of NodeSection:  
    n.children[0].pretty(state)
    state.r += text(" ")
    if n.children.len() == 3:
      n.children[1].pretty(state)
      state.r += text(" ")
    n.children[^1].pretty(state)
    state.r += text($(Rune('\n').repeat(state.style.breaksAfterSec)))
  of NodeCallbackLit:
    state.r += n.keyword()
    prettyDown()
  of NodeNoCallbackName:
    discard
  of NodeUseStmt:
    state.r += n.keyword()
    n.children[0].pretty(state)
    if n.children.len() == 2:
      state.r += prettyColor(text(" from "), keywordColor)
      n.children[1].pretty(state)
  of NodeVarStmt:
    state.r += n.keyword()
    n.children[0].pretty(state)
    if n.children.len() > 1:
      state.r += prettyColor(text(": "), operatorColor)
      n.children[1].pretty(state)
  of NodeVarSymNames: 
    let kidValues = n.collectKids(state)

    for i, item in kidValues:
      state.r += item
      if i != kidValues.len() - 1:
        state.r += state.style.comma
  of NodeParamBlock:
    state.r += n.keyword()
    prettyDown()

proc pretty*(n: Con4mNode, style = getCurrentCodeStyle()): Rope =
  var state = PrettyState(style: style)

  if state.style.comma == nil:
     state.style.comma = text(", ").fgColor(state.style.otherDelimColor)

  n.pretty(state)

  result = state.r

when isMainModule:
  useCrashTheme()
  let f = readFile("ptest.c4m")
  var (status, tokenBox) = lex(f)


  var errs: seq[Con4mError]

  let tree = parseModule(tokenBox, errs)
  
  for item in errs:
    echo fgColor("error:", "red") + fgColor($(item.token.lineNo), "jazzberry") +
         atom(":") + fgColor($(item.token.lineOffset), "jazzberry") + 
         atom(": ") + item.msg
  let prettyText = tree.pretty()

  print tree.toRope()
  echo prettyText.toUtf8()
  print tokenBox.toRope()
  print prettyText
