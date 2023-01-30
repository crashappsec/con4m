## Functions to represent various data types as strings.  For the
## things mapping to internal data structures, these are pretty much
## all just used for debugging.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options, strformat, strutils, streams, tables, json, unicode
import types, nimutils

# If you want to be able to reconstruct the original file, swap this
# false to true.
when false:
  proc `$`*(tok: Con4mToken): string =
    let pos = tok.stream.getPosition()

    tok.stream.setPosition(tok.startPos)
    result = tok.stream.readStr(tok.endPos - tok.startPos)
    tok.stream.setPosition(pos)
else:
  proc `$`*(tok: Con4mToken): string =
    case tok.kind
    of TtStringLit: return "\"" & tok.unescaped & "\""
    of TtWhiteSpace: return "~ws~"
    of TtNewLine: return "~nl~"
    of TtSof: return "~sof~"
    of TtEof: return "~eof~"
    of ErrorTok: return "~err~"
    of ErrorLongComment: return "~unterm comment~"
    of ErrorStringLit: return "~unterm string~"
    else:
      let pos = tok.stream.getPosition()

      tok.stream.setPosition(tok.startPos)
      result = tok.stream.readStr(tok.endPos - tok.startPos)
      tok.stream.setPosition(pos)

      if result.contains('\n'):
        return "~multi-line value~"

proc `$`*(t: Con4mType): string =
  ## Prints a type object the way it should be written for input, with
  ## the exception of the bottom type, which prints as its
  ## mathematical symbol (`⊥`)
  case t.kind
  of TypeString: return "string"
  of TypeBool: return "bool"
  of TypeInt: return "int"
  of TypeFloat: return "float"
  of TypeList: return "[{t.itemType}]".fmt()
  of TypeDict: return "{{{t.keyType} : {t.valType}}}".fmt()
  of TypeTuple:
    var s: seq[string]
    for item in t.itemTypes:
      s.add($(item))
    return fmt"""({s.join(", ")})"""
  of TypeTVar:
    if t.link.isSome():
      return $(t.link.get())
    return fmt"@{t.varNum}"
  of TypeBottom: return "⊥"
  of TypeProc:
    if t.params.len() == 0: return "f() -> {$(t.retType)}".fmt()
    else:
      var paramTypes: seq[string]
      for item in t.params:
        paramTypes.add($(item))
      if t.va:
        paramTypes[^1] = "*" & paramTypes[^1]
      return "f({paramTypes.join(\", \")}) -> {$(t.retType)}".fmt()

proc formatNonTerm(self: Con4mNode, name: string, i: int): string

proc formatTerm(self: Con4mNode, name: string, i: int): string =
  if not self.token.isSome():
    return ' '.repeat(i) & name & " <???>"

  result = ' '.repeat(i) & name & " " & $(self.token.get())
  if self.typeInfo != nil:
    result = result & " -- type: " & $(self.typeInfo)

template fmtNt(name: string) =
  return self.formatNonTerm(name, i)

template fmtNtNamed(name: string) =
  return self.formatNonTerm(name & " " & $(self.token.get()), i)

template fmtT(name: string) =
  return self.formatTerm(name, i) & "\n"

proc `$`*(self: Con4mNode, i: int = 0): string =
  case self.kind
  of NodeBody: fmtNt("Body")
  of NodeAttrAssign: fmtNt("AttrAssign")
  of NodeVarAssign: fmtNt("VarAssign")
  of NodeUnpack: fmtNt("Unpack")
  of NodeSection: fmtNt("Section")
  of NodeIfStmt: fmtNt("If Stmt")
  of NodeConditional: fmtNt("Conditional")
  of NodeElse: fmtNt("Else")
  of NodeFor: fmtNt("For")
  of NodeBreak: fmtT("Break")
  of NodeContinue: fmtT("Continue")
  of NodeReturn: fmtNt("Return")
  of NodeSimpLit: fmtT("Literal")
  of NodeUnary: fmtNtNamed("Unary")
  of NodeNot: fmtNt("Not")
  of NodeMember: fmtNt("Member")
  of NodeIndex: fmtNt("Index")
  of NodeCall: fmtNt("Call")
  of NodeActuals: fmtNt("Actuals")
  of NodeDictLit: fmtNt("DictLit")
  of NodeKVPair: fmtNt("KVPair")
  of NodeListLit: fmtNt("ListLit")
  of NodeTupleLit: fmtNt("TupleLit")
  of NodeEnum: fmtNt("Enum")
  of NodeFuncDef: fmtNtNamed("Def")
  of NodeFormalList: fmtNt("Formals")
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    fmtNt($(self.token.get()))
  of NodeIdentifier: fmtT("Identifier")

proc reprOneLevel(self: AttrScope, path: var seq[string]): string =
  path.add(self.name)
  
  result = toAnsiCode([acBold]) & path.join(".") & toAnsiCode([acReset]) & "\n"
  var rows = @[@["Name", "Type", "Value"]]


  for k, v in self.contents:
    var row: seq[string] = @[]
    
    if v.isA(Attribute):
      var attr = v.get(Attribute)
      if attr.value.isSome():
        row.add(@[attr.name, $(attr.tInfo), $(attr.value.get())])
      else:
        row.add(@[attr.name, $(attr.tInfo), "<not set>"])
    else:
      var sec = v.get(AttrScope)
      row.add(@[sec.name, "section", "n/a"])
    rows.add(row)

  var tbl = newTextTable(3,
                         rows          = rows,
                         fillWidth     = true,
                         rowHeaderSep  = some(Rune('-')),
                         colHeaderSep  = none(Rune),
                         colSep        = some(Rune('|')),
                         addLeftBorder = true, addRightBorder = true,
                         rHdrFmt       = @[acBCyan], 
                         eColFmt       = @[acBGCyan, acBBlack],
                         oColFmt       = @[acBGWhite, acBBlack])
  result &= tbl.render()

  for k, v in self.contents:
    if v.isA(AttrScope):
      var scope = v.get(AttrScope)
      result &= scope.reprOneLevel(path)
  
proc `$`*(self: AttrScope): string =
  var parts: seq[string] = @[]
  return reprOneLevel(self, parts)
      
proc formatNonTerm(self: Con4mNode, name: string, i: int): string =
  const
    typeTemplate = " -- type: {typeRepr}"
    mainTemplate = "{spaces}{name}{typeVal}\n"
    indentTemplate = "{result}{subitem}"
  let
    spaces = ' '.repeat(i)
    ti = self.typeInfo
    typeRepr = if ti == nil: "" else: $(ti)
    typeVal = if ti == nil: "" else: typeTemplate.fmt()

  result = mainTemplate.fmt()

  for item in self.children:
    let subitem = item.`$`(i + 2)
    result = indentTemplate.fmt()
