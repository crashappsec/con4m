## Functions to represent various data types as strings.  For the
## things mapping to internal data structures, these are pretty much
## all just used for debugging.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options, strformat, strutils, streams, tables, json, unicode, algorithm
import nimutils, types

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
    of TtOtherLit:
      let pos = tok.stream.getPosition()
      tok.stream.setPosition(tok.startPos)
      result = "<<" & tok.stream.readStr(tok.endPos - tok.startPos) &  ">>"
      tok.stream.setPosition(pos)
    of TtWhiteSpace: return "~ws~"
    of TtNewLine: return "~nl~"
    of TtSof: return "~sof~"
    of TtEof: return "~eof~"
    of ErrorTok: return "~err~"
    of ErrorLongComment: return "~unterm comment~"
    of ErrorStringLit: return "~unterm string~"
    of ErrorOtherLit: return "~unterm other lit~"
    else:
      let pos = tok.stream.getPosition()

      tok.stream.setPosition(tok.startPos)
      result = tok.stream.readStr(tok.endPos - tok.startPos)
      tok.stream.setPosition(pos)

template colorType(s: string): string =
  toAnsiCode(acGreen) & s & toAnsiCode(acReset)

template colorLit(s: string): string =
  toAnsiCode(acRed) & s & toAnsiCode(acReset)

template colorNT(s: string): string =
  toAnsiCode(acBrown) & s & toAnsiCode(acReset)

template colorT(s: string): string =
  toAnsiCode(acYellow) & s & toAnsiCode(acReset)

proc `$`*(t: Con4mType): string =
  ## Prints a type object the way it should be written for input, with
  ## the exception of the bottom type, which prints as its
  ## mathematical symbol (`⊥`)
  case t.kind
  of TypeBottom:   return "void"
  of TypeString:   return "string"
  of TypeBool:     return "bool"
  of TypeInt:      return "int"
  of TypeFloat:    return "float"
  of TypeDuration: return "Duration"
  of TypeIPAddr:   return "IPAddr"
  of TypeCIDR:     return "CIDR"
  of TypeSize:     return "Size"
  of TypeDate:     return "Date"
  of TypeTime:     return "Time"
  of TypeDateTime: return "DateTime"
  of TypeList:     return fmt"list[{t.itemType}]"
  of TypeDict:     return fmt"dict[{t.keyType}, {t.valType}]"
  of TypeTuple:
    var s: seq[string] = @[]
    for item in t.itemTypes:
      s.add($(item))
    return fmt"""tuple[{s.join(", ")}]"""
  of TypeTypeSpec:
    result = "typespec"
    if t.binding.localName.isSome() or t.binding.link.isSome():
      result &= "[" & $(t.binding) & "]"
  of TypeTVar:
    if t.link.isSome():
      return $(t.link.get())
    else:
      var parts: seq[string] = @[]
      for (k, v) in [(TypeString,   "string"),
                     (TypeBool,     "bool"),
                     (TypeInt,      "int"),
                     (TypeFloat,    "float"),
                     (TypeDuration, "Duration"),
                     (TypeIPAddr,   "IPAddr"),
                     (TypeCIDR,     "CIDR"),
                     (TypeSize,     "Size"),
                     (TypeDate,     "Date"),
                     (TypeTime,     "Time"),
                     (TypeTypeSpec, "typespec"),
                     (TypeDateTime, "DateTime")]:
        if t.constraints.contains(k):
          parts.add(v)
      if len(parts) == 0:
        return "`" & t.localName.getOrElse($(t.varNum))
      else:
        return parts.join(" or ")
  of TypeFunc:
    if t.params.len() == 0:
      return fmt"func() -> {$(t.retType)}"
    else:
      var paramTypes: seq[string]
      for item in t.params:
        paramTypes.add($(item))
      if t.va:
        paramTypes[^1] = "*" & paramTypes[^1]
      return "f({paramTypes.join(\", \")}) -> {$(t.retType)}".fmt()
  of TypeUnion:
    var s: seq[string] = @[]
    for item in t.components:
      s.add($(item))
    return s.join(" or ")

proc formatNonTerm(self: Con4mNode, name: string, i: int): string

proc formatTerm(self: Con4mNode, name: string, i: int): string =
  if not self.token.isSome():
    return ' '.repeat(i) & name & " <???>"

  result = ' '.repeat(i) & name & " " & colorLit($(self.token.get()))
  if self.typeInfo != nil:
    result = result & " -- type: " & colorLit($(self.typeInfo))

template fmtNt(name: string) =
  return self.formatNonTerm(colorNT(name), i)

template fmtNtNamed(name: string) =
  return self.formatNonTerm(colorNT(name) & " " &
            colorT($(self.token.get())), i)

template fmtT(name: string) =
  return self.formatTerm(colorT(name), i) & "\n"

template fmtTy(name: string) =
  return self.formatNonTerm(colorType(name), i)

proc `$`*(self: Con4mNode, i: int = 0): string =
  case self.kind
  of NodeBody:         fmtNt("Body")
  of NodeAttrAssign:   fmtNt("AttrAssign")
  of NodeAttrSetLock:  fmtNt("AttrSetLock")
  of NodeVarAssign:    fmtNt("VarAssign")
  of NodeUnpack:       fmtNt("Unpack")
  of NodeSection:      fmtNt("Section")
  of NodeIfStmt:       fmtNt("If Stmt")
  of NodeConditional:  fmtNt("Conditional")
  of NodeElse:         fmtNt("Else")
  of NodeFor:          fmtNt("For")
  of NodeBreak:        fmtT("Break")
  of NodeContinue:     fmtT("Continue")
  of NodeReturn:       fmtNt("Return")
  of NodeSimpLit:      fmtT("Literal")
  of NodeUnary:        fmtNtNamed("Unary")
  of NodeNot:          fmtNt("Not")
  of NodeMember:       fmtNt("Member")
  of NodeIndex:        fmtNt("Index")
  of NodeCall:         fmtNt("Call")
  of NodeActuals:      fmtNt("Actuals")
  of NodeDictLit:      fmtNt("DictLit")
  of NodeKVPair:       fmtNt("KVPair")
  of NodeListLit:      fmtNt("ListLit")
  of NodeTupleLit:     fmtNt("TupleLit")
  of NodeCallbackLit:  fmtNt("CallbackLit")
  of NodeEnum:         fmtNt("Enum")
  of NodeFuncDef:      fmtNtNamed("Def")
  of NodeFormalList:   fmtNt("Formals")
  of TmpDictType:     fmtTy("DictType")
  of TmpListType:     fmtTy("ListType")
  of TmpTupleType:    fmtTy("TupleType")
  of TmpStringType:   fmtTy("StringType")
  of TmpIntType:      fmtTy("IntType")
  of TmpFloatType:    fmtTy("FloatType")
  of TmpBoolType:     fmtTy("BoolType")
  of TmpTSpecType:    fmtTy("TSpecType")
  of TmpFuncType:     fmtTy("FuncType")
  of TmpDurationType: fmtTy("DurationType")
  of TmpIPAddrType:   fmtTy("IpAddrType")
  of TmpCidrType:     fmtTy("CidrType")
  of TmpSizeType:     fmtTy("SizeType")
  of TmpDateType:     fmtTy("DateType")
  of TmpTimeType:     fmtTy("TimeType")
  of TmpDateTimeType: fmtTy("DateTimeType")
  of TmpUnionType:    fmtTy("UnionType")
  of TmpVoidType:     fmtTy("VoidType")
  of TmpVarargsType:  fmtNt("VarargsType")
  of TmpTVar:         fmtNt("TVar")
  of NodeType:        fmtNt("Type")
  of NodeVarDecl:      fmtNt("VarDecl")
  of NodeExportDecl:   fmtNt("ExportDecl")
  of NodeVarSymNames:  fmtNt("VarSymNames")
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    fmtNt($(self.token.get()))
  of NodeIdentifier:   fmtNtNamed("Identifier")

proc formatNonTerm(self: Con4mNode, name: string, i: int): string =
  const
    typeTemplate = " -- type: {typeRepr}"
    mainTemplate = "{spaces}{name}{typeVal}\n"
    indentTemplate = "{result}{subitem}"
  let
    spaces = ' '.repeat(i)
    ti = self.typeInfo
    typeRepr = if ti == nil: "" else: colorType($(ti))
    typeVal = if ti == nil: "" else: typeTemplate.fmt()

  result = mainTemplate.fmt()

  for item in self.children:
    let subitem = item.`$`(i + 2)
    result = indentTemplate.fmt()

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

proc `$`*(self: VarScope): string =
  result = ""

  if self.parent.isSome():
    result = $(self.parent.get())

  var rows = @[@["Name", "Type"]]
  for k, v in self.contents:
    rows.add(@[k, $(v.tInfo)])

  var tbl = newTextTable(2,
                         rows          = rows,
                         fillWidth     = true,
                         rowHeaderSep  = some(Rune('-')),
                         colHeaderSep  = none(Rune),
                         colSep        = some(Rune('|')),
                         addLeftBorder = true, addRightBorder  = true,
                         addTopBorder  = true, addBottomBorder = true,
                         rHdrFmt       = @[acBCyan],
                         eRowFmt       = @[acBGCyan, acBBlack],
                         oRowFmt       = @[acBGWhite, acBBlack])

  return result & tbl.render()

proc `<`(x, y: seq[string]): bool =
  if x[0] == y[0]:
    return x[1] < y[1]
  else:
    return x[0] < y[0]

proc `$`*(funcTable: Table[string, seq[FuncTableEntry]]): string =
  # Not technically a dollar, but hey.
  var rows = @[@["Name", "Type", "Kind"]]
  for key, entrySet in funcTable:
    for entry in entrySet:
      rows.add(@[key, $(entry.tinfo), $(entry.kind)])
  rows.sort()
  var tbl = newTextTable(3,
                         rows          = rows,
                         fillWidth     = true,
                         rowHeaderSep  = some(Rune('-')),
                         colHeaderSep  = none(Rune),
                         colSep        = some(Rune('|')),
                         addLeftBorder = true, addRightBorder  = true,
                         addTopBorder  = true, addBottomBorder = true,
                         rHdrFmt       = @[acBCyan],
                         eRowFmt       = @[acBGCyan, acBBlack],
                         oRowFmt       = @[acBGWhite, acBBlack])

  return result & tbl.render()
