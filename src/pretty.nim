import "."/[parse, style]

type PrettyState = object
  r:                Rope
  bodyUseColon:     bool
  style:            CodeStyle
  pad:              string
  printingType:     int
  ## State only used for processing comments
  lastIx:           int
  curComments:      seq[seq[string]]
  lineComments:     seq[bool]
  noPreBreak:       bool
  noPostBreak:      bool
  longForm:         bool
  commentStartRope: Rope

template prettyColor(rope: Rope, field: untyped): Rope =
  if state.style.field != "":
    rope.fgColor(state.style.field)
  else:
    rope

## Our approach with comments is to mostly just preserve them right
## now.  If there are connected comments, we lump them into one
## comment.  If there are newlines between them, we limit it to one
## newline.
##
## We combine # or // comments; /* */ comments we leave alone.
##
## We could try to move inline comments above, but that's a bit more
## work; we'd have to do more tracking.
template noCommentsYet(state: var PrettyState): bool =
  if state.curComments.len() > 1:
    false
  elif state.curComments[0].len() != 0:
    false
  else:
    true

template currentSetIsEmpty(state: var PrettyState): bool =
  if state.curComments[^1].len() == 0:
    true
  else:
    false

template endCurComments(state: var PrettyState) =
  state.curComments.add(@[])
  state.lineComments.add(true) # Assume it's a line comment

template addToComment(state: var PrettyState, s: string, multiline = false) =
  if multiline:
    if not state.currentSetIsEmpty():
      state.endCurComments()

    state.lineComments[^1] = false
    state.curComments[^1].add(strutils.split(s, "\n"))
    state.endCurComments()
  else:
    state.curComments[^1].add(s)

proc buildCommentGroups(node: ParseNode, state: var PrettyState): bool =
  return false
  when false:
    # Returns true if there are comments.
    let curTok    = node.token
    var prevWasNl = false

    if curTok.id <= state.lastIx:
      return

    state.curComments  = @[@[]]
    state.lineComments = @[true]
    state.noPreBreak   = true

    for i in state.lastIx .. curTok.id:
      let tok = node.allTokens.tokAt(i)

      case tok.kind:
        of TtNewLine, TtSof:
          if state.noCommentsYet():
            state.noPreBreak = false
          elif prevWasNl and not state.currentSetIsEmpty():
            state.endCurComments()
          prevWasNl = true
        of TtLineComment:
          prevWasNl = false
          state.addToComment($tok)
        of TtLongComment:
          prevWasNl = false
          state.addToComment(`$`(tok), multiline = true)

        else:
          continue

    state.lastIx      = curTok.id
    state.noPostBreak = not prevWasNl

    if state.noCommentsYet():
      return false

    if state.curComments[^1].len() == 0:
      state.curComments = state.curComments[0 ..< ^1]

    return true

proc removeLeadingPadding(state: var PrettyState) =
  ## This treats a leading "#", "# " or " * " as pad and removes,
  ## in case we need to wrap.
  ##
  ## For now, when we put this back together, we do not remember what
  ## style was used, we just use "# " wherever appropriate, and /* */
  ## if not.

  for n, comment in state.curComments:
    var newComment: seq[string]
    let isLineComment = state.lineComments[n]

    for i, line in comment:
      var edited = line

      if isLineComment:
        if edited.startswith("#"):
          edited = edited[1 .. ^1]

        if edited.startswith(" "):
          edited = edited[1 .. ^1]
      else:
        if edited.startswith(" "):
          edited = edited[1 .. ^1]
          if edited.startswith("* "):
            edited = edited[2 .. ^1]

      newComment.add(edited)

    state.curComments[n] = newComment

proc wrapComments(state: var PrettyState, width: int) =
  ## For multi-line comments, all lines that have now been de-padded should
  ## be merged into one line and re-wrapped.
  ##
  ## For single line comments, we assume that each line is distinct; we'll
  ## wrap individual lines, but not merge lines.
  for n, comment in state.curComments:

    if state.lineComments[n]:
      var newLines: seq[string] = @[]
      for line in comment:
         newLines &= line.indentWrap(width, hangingIndent = 0)

      state.curComments[n] = newLines

    else:
      let
        oneLine = comment.join("\n")
        wrapped = oneLine.indentWrap(width, hangingIndent = 0)

      state.curComments[n] = wrapped.split("\n")

proc addBackPaddingAndHash(state: var PrettyState) =
  for n, comment in state.curComments:
    var newLines: seq[string] = @[]

    for line in comment:
      newLines.add("# " & line)

    state.curComments[n] = newLines

proc addBackPaddingLong(state: var PrettyState) =
  for n, comment in state.curComments:
    var newLines: seq[string] = @[]

    if len(comment) == 1:
      newlines = @[ "/* " & comment[0] & " */"]
    else:
      newlines = @["/*"]
      for line in comment:
        newlines.add(" * " & line)
      newlines.add(" */")

    state.curComments[n] = newLines

proc indentComments(state: var PrettyState) =
  for n, comment in state.curComments:
    var newLines: seq[string] = @[]
    for i, line in comment:
      if i == 0 and n == 0 and state.noPreBreak:
        newLines.add(" " & line)
      else:
        newLines.add(state.pad & line)

    state.curComments[n] = newLines

proc outputFormattedComments(state: var PrettyState) =
  if state.longForm:
    var finalLines: seq[string]
    for comment in state.curComments:
      finalLines.add(comment.join("\n"))

    let toOut = prettyColor(text(finalLines.join(" ")), commentColor)
    state.commentStartRope += toOut

  else:
    var finalLines: seq[string]
    for i, comment in state.curComments:
      finalLines &= comment
      if i != state.curComments.len() - 1:
        finalLines.add("")

    let toOut = prettyColor(text(finalLines.join("\n")), commentColor)
    state.commentStartRope += toOut

  if not state.noPostBreak:
    state.commentStartRope += newBreak()

proc undoLineBreakIfNeeded(state: var PrettyState) =
  state.commentStartRope = state.r

  if state.noPreBreak:
    var textAtoms = state.r.allAtoms()

    while textAtoms.len() != 0 and textAtoms[^1].text.len() == 0:
      textAtoms = textAtoms[0 ..< ^1]

    if textAtoms.len() == 0:
      return

    state.commentStartRope = textAtoms[^1]

proc handleComments(node: ParseNode, state: var PrettyState) =
  if not node.buildCommentGroups(state):
    return

  echo "after bcg: ", state.curComments
  # We loop through comments multiple times to make the code
  # more clear.
  state.removeLeadingPadding()
  echo "after pad rm: ", state.curComments
  let width = state.style.prefWidth - state.pad.len() - 6
  state.wrapComments(width)
  echo "after wrap: ", state.curComments

  state.undoLineBreakIfNeeded()

  if state.noPreBreak and state.noPostBreak:
    state.longForm = true
    state.addBackPaddingLong() # Add /* */ style padding.
  else:
    state.undoLineBreakIfNeeded()
    state.longForm = false
    state.addBackPaddingAndHash()

  state.indentComments()
  state.outputFormattedComments()

proc pretty(n: ParseNode, state: var PrettyState)

template prettyDown(stmt = false, start = 0, addColon = false, newline = true) =
  let l = n.children[start .. ^1]

  for i, item in l:
    if addColon:
      state.bodyUseColon = true # Will immediately be turned off.

    if stmt:
      state.r += text(state.pad)
      item.pretty(state)
      if newline or i != l.len() - 1:
        state.r += text("\n")
    else:
      item.pretty(state)

template applyIndent(code: untyped) =
  var addedPad = ""
  for i in 0 ..< state.style.blockIndent:
    addedPad.add(' ')

  let
    savedPad = state.pad
    newPad   = state.pad & addedPad

  state.pad = newPad

  code

  state.pad = savedPad
  state.r += text(state.pad)

template indentDown(s = 0, colon = false, nl = true) =
  applyIndent:
    n.docNodes.pretty(state)
    prettyDown(stmt = true, start = s, addColon = colon, newline = nl)

template prettySimpleLiteral(field: untyped) =
  var
    tok      = n.token
    txt      = tok.getText(adjust = true)

  if state.style.field != "":
    state.r += text(txt).fgColor(state.style.field)
  else:
    state.r += text(txt)


proc collectKids(n: ParseNode, state: var PrettyState): seq[Rope] =
  let savedOut = state.r
  for item in n.children:
    state.r = nil
    item.pretty(state)
    result.add(state.r)

  state.r = savedOut

proc containerLiteral(n:              ParseNode,
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
    state.r += text("\n" & oldPad) + rFmt

  else:
    for i, item in kidValues:
      state.r += item
      if i != kidValues.len() - 1:
        state.r += state.style.comma
    state.r += rFmt

proc keyword(n: ParseNode, state: var PrettyState, space = " "): Rope =
  var baseText: string

  if n.kind == NodeMember:
    baseText = n.children[0].getText()
  else:
    baseText = n.getText()

  prettyColor(text(baseText & space), keywordColor)

proc pretty(n: ParseNode, state: var PrettyState) =
  # We don't check before passing in docnodes where they might appear.
  if n == nil:
    return

  case n.kind
  of NodeModule:
    n.docNodes.pretty(state)
    prettyDown(stmt = true)
  of NodeExpression, NodeLiteral, NodeTypeBuiltin, NodeCase:
    prettyDown()
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeNot,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv, NodeTypeRef,
    NodeBitOr, NodeBitXor, NodeBitAnd, NodeShl, NodeShr:
    let operands = n.collectKids(state)

    # Unary operands don't print anything on the LHS. NodePlus and
    # NodeMinus could be either unary or binary; There are some
    # unary-only ops in here, in this branch, we add a space after the
    # op, since they require spaces around them; the ones in the next
    # branch are unary operators where the space is weird.

    if operands.len() == 2:
      state.r += operands[0] + text(" ")

    state.r += prettyColor(text(n.getText()), operatorColor)
    state.r += text(" ") + operands[^1]

  of NodeAttrSetLock, NodeTypeVararg:
    state.r += prettyColor(text(n.getText()), operatorColor)
    prettyDown()
  of NodeTypeVar:
    state.r += prettyColor(text(n.getText()), typeNameColor)
    prettyDown()
  of NodeIdentifier:
    if state.printingType > 0:
      state.r += prettyColor(text(n.getText()), typeNameColor)
    else:
      state.r += prettyColor(text(n.getText()), identColor)
  of NodeBody:
    let colon = state.bodyUseColon
    state.bodyUseColon = false

    if n.children.len() != 0:
      let toAdd =
        if colon:                     text(":\n")
        elif state.style.brB4Bracket: text("\n" & state.pad & "{")
        else:                         text(" {\n")

      state.r += prettyColor(toAdd, otherDelimColor)
      indentDown(nl = not colon)

      if not colon:
        state.r += prettyColor(text("}"), otherDelimColor)

  of NodeStringLit:
    prettySimpleLiteral(stringLitColor)
  of NodeCharLit:
    prettySimpleLiteral(charLitColor)
  of NodeIntLit, NodeFloatLit, NodeHexLit:
    prettySimpleLiteral(numericLitColor)
  of NodeBoolLit:
    prettySimpleLiteral(boolLitColor)
  of NodeOtherLit, NodeNilLit:
    prettySimpleLiteral(otherLitColor)
  of NodeDictLit:
    n.containerLiteral("{", "}", state)
  of NodeListLit:
    n.containerLiteral("[", "]", state)
  of NodeTupleLit:
    n.containerLiteral("(", ")", state)
  of NodeKVPair:
    let kidValues = n.collectKids(state)
    state.r += kidValues[0] + prettyColor(text(" : "), otherDelimColor)
    state.r += kidValues[1]
  of NodeIfStmt, NodeElifStmt, NodeWhileStmt:
    state.r += n.keyword(state)
    n.children[0].pretty(state)
    state.r += text(" ")
    n.children[1].pretty(state)
  of NodeElseStmt:
    state.r += n.keyword(state, space = if state.bodyUseColon: "" else: " ")
    prettyDown()
  of NodeForStmt:
    state.r += n.keyword(state)
    n.children[0].pretty(state)

    if n.children[1].kind == NodeRange:
      state.r += text(" from ")
    else:
      state.r += text(" in ")

    prettyDown(start = 1)
  of NodeBreakStmt, NodeContinueStmt:
    state.r += n.keyword(state)
  of NodeReturnStmt:
    state.r += n.keyword(state)
    prettyDown()
  of NodeAssign:
    n.children[0].pretty(state)
    if state.style.forcedAttrChar.len() != 0:
      let op = text(" " & state.style.forcedAttrChar & " ")
      state.r += prettyColor(op, operatorColor)
    else:
      let attrChr = n.getText()
      if attrChr == ":":
        state.r += prettyColor(text(": "), operatorColor)
      else:
        state.r += prettyColor(text(" = "), operatorColor)
      n.children[1].pretty(state)
  of NodeMember:
    let operands = n.collectKids(state)

    for i, item in operands:
      if i != 0:
        state.r += prettyColor(text("."), operatorColor)
      state.r += item

  of NodeIndex:
    let ixPad = if state.style.padIndexOps: " " else: ""
    n.children[0].pretty(state)
    state.r += prettyColor(text("[" & ixPad), otherDelimColor)
    n.children[1].pretty(state)
    state.r += prettyColor(text(ixPad & "]"), otherDelimColor)
  of NodeType:

    state.printingType += 1
    let kids = n.collectKids(state)

    for i, k in n.children:
      k.pretty(state)
      if i != kids.len() - 1:
        state.r += prettyColor(text(" or "), operatorColor)

    state.printingType -= 1
  of NodeTypeList, NodeTypeDict, NodeTypeObj, NodeTypeTuple, NodeTypeOneOf,
       NodeTypeMaybe:
    let
      name    = n.getText()
      kidVals = n.collectKids(state)

    state.r += prettyColor(text(name), typeNameColor)
    state.r += prettyColor(text("["), otherDelimColor)
    for i, item in kidVals:
      state.r += prettyColor(item, typeNameColor)
      if i != kidVals.len() - 1:
        state.r += state.style.comma
    state.r += prettyColor(text("]"), otherDelimColor)

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
    state.r += kidValues[^1]
  of NodeReturnType:
    if n.children.len() == 0:
      state.r += prettyColor(text("void"), typeNameColor)
    else:
      n.children[0].pretty(state)
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
    state.r += n.keyword(state)
    state.r += prettyColor(text(n.children[0].getText()), funcNameColor)
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
    state.r += n.keyword(state)
    prettyDown()
  of NodeUseStmt:
    state.r += n.keyword(state)
    n.children[0].pretty(state)
    if n.children.len() == 2:
      state.r += prettyColor(text(" from "), keywordColor)
      n.children[1].pretty(state)
  of NodeVarSymInfo:
    for i in 0 ..< n.children.len():
      if i == n.children.len() - 1:
        if n.children[i].kind == NodeType:
          state.r += prettyColor(text(": "), operatorColor)
        elif i != 0:
          state.r += prettyColor(text(", "), operatorColor)
      elif i != 0:
        state.r += prettyColor(text(", "), operatorColor)

      n.children[i].pretty(state)
  of NodeVarStmt, NodeGlobalStmt, NodeConstStmt:
    var start = 0
    state.r += n.keyword(state)

    if n.children[0].kind != NodeVarSymInfo:
      n.children[0].pretty(state)
      start  = 1

    for i in start ..< n.children.len():
      if i != 0:
        state.r += prettyColor(text(", "), operatorColor)

      n.children[i].pretty(state)

  of NodeParamBlock:
    state.r += n.keyword(state)
    prettyDown()

  of NodeCast:
    discard # Not generated yet.

  of NodeTypeOfStmt, NodeValueOfStmt:
    state.r += n.keyword(state)
    n.children[0].pretty(state)

    state.r += prettyColor(text(" {\n"), otherDelimColor)
    indentDown(1, colon = true)
    state.r += prettyColor(text("}\n"), otherDelimColor)
  of NodeEnumStmt:
    state.r += n.keyword(state)
    n.containerLiteral("", "", state, 5)

  of NodeEnumItem:
    n.children[0].pretty(state)
    if n.children.len() == 2:
      state.r += prettyColor(text(" = "), operatorColor)
      n.children[1].pretty(state)

  of NodeVarargsFormal:
    state.r += prettyColor(text("*"), operatorColor)
    n.children[0].pretty(state)
    if n.children.len() == 2:
      state.r += prettyColor(text(": "), otherDelimColor)
      n.children[1].pretty(state)

  of NodeExternBlock:
    state.r += n.keyword(state)
    n.children[0].pretty(state)
    n.children[1].pretty(state)
    let toAdd =
      if state.style.brB4Bracket:  text("\n" & state.pad & "{")
      else:                        text(" {\n")

    state.r += prettyColor(toAdd, otherDelimColor)

    applyIndent:
      n.docNodes.pretty(state)
      for item in n.children[2 .. ^1]:
        state.r += text(state.pad)
        item.pretty(state)
        state.r += text("\n")

    state.r += text(state.pad & "}\n")
  of NodeExternSig:
    if n.children.len() == 1:
      state.r += text("()")
    else:
      state.r += text("(")
      for i, item in n.children[0 ..< ^1]:
        if i != 0:
          state.r += text(", ")
        item.pretty(state)

      state.r += text(")")

    state.r += prettyColor(text(" -> "), operatorColor)
    n.children[^1].pretty(state)
  of NodeExternParam:
    state.r += n.children[0].keyword(state, space = "")

    if n.children.len() > 1:
      state.r += prettyColor(text(": "), operatorColor)
      for i, item in n.children[1 ..< ^1]:
        if i != 0:
          state.r += text(" ")
        item.pretty(state)

      n.children[^1].pretty(state)
  of NodeExternDll, NodeExternPure:
    state.r += n.keyword(state, space = "")
    state.r += prettyColor(text(": "), operatorColor)
    prettyDown()
  of NodeExternLocal:
    state.r += n.keyword(state, space = "")
    state.r += prettyColor(text(": "), operatorColor)
    n.children[0].pretty(state)
    n.children[1].pretty(state)
    state.r += prettyColor(text(" -> "), operatorColor)
    n.children[2].pretty(state)
  of NodeExternHolds, NodeExternAllocs:
    state.r += n.keyword(state)
    let parts = n.collectKids(state)
    state.r += parts[0] + text(": ")
    state.r += join(parts[1 .. ^1], text(", "))
  of NodeExternReturn:
    state.r += n.keyword(state)
  of NodeLabelStmt:
    prettyDown()
    state.r +=  prettyColor(text(":\n"), otherDelimColor)
  of NodeCaseCondition:
    state.r +=   prettyColor(text("case "), keywordColor)
    let numConditions = n.children.len() - 1

    for i in  0 ..< numConditions:
      if i != 0:
        state.r += text(", ")
      n.children[i].pretty(state)

    n.children[^1].pretty(state)
  of NodeRange:
    n.children[0].pretty(state)
    state.r += text(" to ")
    n.children[1].pretty(state)
  of NodeDocString:
    for item in n.children:
      item.pretty(state)
      state.r += text("\n")

    state.r += text("\n")
  of NodeAssert:
    state.r += n.keyword(state)
    prettyDown()
  of NodeConfSpec:
    state.r += n.keyword(state)
    let toAdd =
      if state.style.brB4Bracket:  text("\n" & state.pad & "{")
      else:                        text(" {\n")

    state.r += prettyColor(toAdd, otherDelimColor)
    indentDown(1)
    state.r += prettyColor(text("}\n"), otherDelimColor)
  of NodeSecSpec:
    state.r += n.keyword(state)
    var start = 1

    if n.children[0].getText() != "root":
      n.children[1].pretty(state)
      state.r += text(" ")
      start = 2

    let toAdd =
      if state.style.brB4Bracket:  text("\n" & state.pad & "{")
      else:                        text(" {\n")

    state.r += prettyColor(toAdd, otherDelimColor)

    indentDown(start)

    state.r += text("}\n")

  of NodeFieldSpec:
    state.r += n.keyword(state)

    n.children[0].pretty(state)

    let toAdd =
      if state.style.brB4Bracket:  text("\n" & state.pad & "{")
      else:                        text(" {\n")

    state.r += prettyColor(toAdd, otherDelimColor)

    indentDown(1)

    state.r += text("}")


  of NodeSecProp, NodeFieldProp:
    state.r += prettyColor(text(n.children[0].getText()), keywordColor)
    state.r += prettyColor(text(": "), otherDelimColor)

    for i, item in n.children[1 .. ^1]:
      if i != 0:
        state.r += text(", ")
      item.pretty(state)

proc pretty*(n: ParseNode, style = getCurrentCodeStyle()): Rope =
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
