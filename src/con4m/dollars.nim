## Functions to represent various data types as strings.  For the
## things mapping to internal data structures, these are pretty much
## all just used for debugging.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options, strformat, streams, tables, json, unicode, algorithm
import nimutils, types
from strutils import join, repeat


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
    of TtStringLit: result = "\"" & tok.unescaped & "\""
    of TtOtherLit:
      let pos = tok.stream.getPosition()
      tok.stream.setPosition(tok.startPos)
      result = "<<" & tok.stream.readStr(tok.endPos - tok.startPos) &  ">>"
      tok.stream.setPosition(pos)
    of TtWhiteSpace:     result = "~ws~"
    of TtNewLine:        result = "~nl~"
    of TtSof:            result = "~sof~"
    of TtEof:            result = "~eof~"
    of ErrorTok:         result = "~err~"
    of ErrorLongComment: result = "~unterm comment~"
    of ErrorStringLit:   result = "~unterm string~"
    of ErrorOtherLit:    result =  "~unterm other lit~"
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
  ## Prints a type object the way it should be written for input.
  ## Note that, in some contexts, 'func' might be required to
  ## distinguish a leading parenthesis from other expressions,
  ## as that is not printed out here.
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
    for item in t.itemTypes: s.add($(item))
    return fmt"""tuple[{join(s, ", ")}]"""
  of TypeTypeSpec:
    result = "typespec"
    if t.binding.kind == TypeTVar:
      if t.binding.localName.isSome() or t.binding.link.isSome():
        result &= "[" & $(t.binding) & "]"
  of TypeTVar:
    if t.link.isSome():
      return $(t.link.get())
    else:
      if len(t.components) != 0:
        var parts: seq[string] = @[]
        for item in t.components:
          parts.add($(item))
        return parts.join(" or ")
      else:
        return "`" & t.localName.getOrElse($(t.varNum))
  of TypeFunc:
    if t.noSpec: return "(...)"
    if t.params.len() == 0:
      return fmt"() -> {$(t.retType)}"
    else:
      var paramTypes: seq[string]
      for item in t.params:
        paramTypes.add($(item))
      if t.va:
        paramTypes[^1] = "*" & paramTypes[^1]
      return "({paramTypes.join(\", \")}) -> {$(t.retType)}".fmt()

proc `$`*(c: CallbackObj): string =
  result = "func " & c.name
  if not c.getType().noSpec: result &= $(c.tInfo)

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
  of NodeCallbackLit:  fmtNtNamed("CallbackLit")
  of NodeEnum:         fmtNt("Enum")
  of NodeFuncDef:      fmtNtNamed("Def")
  of NodeFormalList:   fmtNt("Formals")
  of NodeType:         fmtNt("Type")
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
    typeVal  = if ti == nil: "" else: typeTemplate.fmt()

  result = mainTemplate.fmt()

  for item in self.children:
    let subitem = item.`$`(i + 2)
    result = indentTemplate.fmt()

proc nativeSizeToStrBase2*(input: Con4mSize): string =
  var n, m: uint64

  if input == 0: return "0 bytes"
  else:          result = ""

  m = input div 1099511627776'u64
  if m != 0:
    result = $(m) & "TB "
    n = input mod 1099511627776'u64

  m = n div 1073741824
  if m != 0:
    result &= $(m) & "GB "
    n = n mod 1073741824

  m = n div 1048576
  if m != 0:
    result &= $(m) & "MB "
    n = n mod 1048576

  m = n div 1024
  if m != 0:
    result &= $(m) & "KB "
    n = n mod 1024

  if n != 0:
    result &= $(m) & "B"

  result = result.strip()

proc nativeDurationToStr*(d: Con4mDuration): string =
  var
    usec    = d mod 1000000
    numSec  = d div 1000000
    n: uint64

  if d == 0: return "0 sec"
  else:      result = ""

  n = numSec div (365 * 24 * 60 * 60)

  case n
  of 0:  discard
  of 1:  result = "1 year "
  else:  result = $(n) & " years "

  numSec = numSec mod (365 * 24 * 60 * 60)

  n = numSec div (24 * 60 * 60)  # number of days
  case n div 7
  of 0:    discard
  of 1:    result &= "1 week "
  else:    result &= $(n) & " weeks "

  case n mod 7
  of 0:    discard
  of 1:    result &= " 1 day "
  else:    result &= $(n mod 7) & " days "

  numSec = numSec mod (24 * 60 * 60)
  n      = numSec div (60 * 60)

  case n
  of 0:    discard
  of 1:    result &= " 1 hour "
  else:    result &= $(n) & " hours "

  numSec = numSec mod (60 * 60)
  n      = numSec div 60

  case n
  of 0:    discard
  of 1:    result &= " 1 min "
  else:    result &= $(n) & " mins "

  numSec = numSec mod 60

  case numSec
  of 0:    discard
  of 1:    result &= " 1 sec "
  else:    result &= $(numSec) & " secs "

  n = usec div 1000
  if n != 0: result &= $(n) & " msec "

  usec = usec mod 1000
  if usec != 0: result &= $(usec) & " usec"

  result = result.strip()

type ValToStrType* = enum vTDefault, vTNoLits
  
proc oneArgToString*(t: Con4mType,
                     b: Box,
                     outType = vTDefault,
                     lit     = false): string =
  case t.kind
  of TypeString:
    if outType != vtNoLits and lit:
      return "\"" & unpack[string](b) & "\""
    else:
      return unpack[string](b)
  of TypeIPAddr, TypeCIDR, TypeDate, TypeTime, TypeDateTime:
    if outType != vtNoLits and lit:
      return "<<" & unpack[string](b) & ">>"
    else:
      return unpack[string](b)
  of TypeTypeSpec:
    return $(unpack[Con4mType](b))
  of TypeFunc:
    let cb = unpack[CallbackObj](b)
    return "func " & cb.name & $(cb.tInfo)
  of TypeInt:
    return $(unpack[int](b))
  of TypeFloat:
    return $(unpack[float](b))
  of TypeBool:
    return $(unpack[bool](b))
  of TypeDuration:
    return nativeDurationToStr(Con4mDuration(unpack[int](b)))
  of TypeSize:
    return nativeSizeToStrBase2(Con4mSize(unpack[int](b)))
  of TypeList:
    var
      strs: seq[string] = @[]
      l:    seq[Box]    = unpack[seq[Box]](b)
    for item in l:
      let itemType = t.itemType.resolveTypeVars()
      strs.add(oneArgToString(itemType, item, outType, true))
    result = strs.join(", ")
    if outType == vTDefault:
      result = "[" & result & "]"
  of TypeTuple:
    var
      strs: seq[string] = @[]
      l:    seq[Box]    = unpack[seq[Box]](b)
    for i, item in l:
      let itemType = t.itemTypes[i].resolveTypeVars()
      strs.add(oneArgToString(itemType, item, outType, true))
    result = strs.join(", ")
    if outType == vTDefault:
      result = "(" & result & ")"
  of TypeDict:
    var
      strs: seq[string]               = @[]
      tbl:  OrderedTableRef[Box, Box] = unpack[OrderedTableRef[Box, Box]](b)

    for k, v in tbl:
      let
        t1 = t.keyType.resolveTypeVars()
        t2 = t.valType.resolveTypeVars()
        ks = oneArgToString(t1, k, outType, true)
        vs = oneArgToString(t2, v, outType, true)
      strs.add(ks & " : " & vs)

    result = strs.join(", ")
    return "{" & result  & "}"
  else:
    return "<??>"

proc reprOneLevel(self: AttrScope, inpath: seq[string]): string =
  var path = inpath & @[self.name]

  result = toAnsiCode([acBold]) & path.join(".") & toAnsiCode([acReset]) & "\n"
  var rows = @[@["Name", "Type", "Value"]]


  for k, v in self.contents:
    var row: seq[string] = @[]

    if v.isA(Attribute):
      var attr = v.get(Attribute)
      if attr.value.isSome():
        let val = oneArgToString(attr.tInfo, attr.value.get())
        row.add(@[attr.name, $(attr.tInfo), val])
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
