import options
import strformat
import strutils
import streams
import tables

import con4m_types

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
  case t.kind
  of TypeString: return "string"
  of TypeBool: return "bool"
  of TypeInt: return "int"
  of TypeFloat: return "float"
  of TypeList: return "[{t.itemType}]".fmt()
  of TypeDict: return "{{{t.keyType} : {t.valType}}}".fmt()
  of TypeTVar: return "`{t.varNum}".fmt()
  of TypeBottom: return "⊥"
  of TypeProc:
    if t.params.len() == 0: return "() -> {$(t.retType)}".fmt()
    else:
      var paramTypes: seq[string]
      for item in t.params:
        paramTypes.add($(item))
      if t.va:
        paramTypes[^1] = "*" & paramTypes[^1]
      return "({paramTypes.join(\", \")}) -> {$(t.retType)}".fmt()

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
  return self.formatNonTerm(name & " " & $(self.token), i)

template fmtT(name: string) =
  return self.formatTerm(name, i) & "\n"

proc `$`*(self: Con4mNode, i: int = 0): string =
  case self.kind
  of NodeBody: fmtNt("Body")
  of NodeAttrAssign: fmtNt("AttrAssign")
  of NodeVarAssign: fmtNt("VarAssign")
  of NodeSection: fmtNt("Section")
  of NodeIfStmt: fmtNt("If Stmt")
  of NodeConditional: fmtNt("Conditional")
  of NodeElse: fmtNt("Else")
  of NodeFor: fmtNt("For")
  of NodeBreak: fmtT("Break")
  of NodeContinue: fmtT("Continue")
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
  of NodeEnum: fmtNt("Enum")
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt,
     NodeLt, NodePlus, NodeMinus, NodeMod, NodeMul, NodeDiv:
    fmtNt($(self.token.get()))
  of NodeIdentifier: fmtT("Identifier")

proc `$`*(self: Box): string =
  case self.kind
  of TypeBool: return $(self.b)
  of TypeString: return self.s
  of TypeInt: return $(self.i)
  of TypeFloat: return $(self.f)
  of TypeList: return "some list"
  of TypeDict: return "some dict"
  of TypeProc: return "some proc"
  else: unreachable

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

proc `$`*(scope: Con4mScope, indent: int): string =
  let pad = " ".repeat(indent + 2)

  for k, v in scope.entries.mpairs():
    if v.subscope.isSome(): continue
    let s = $(v.tInfo)
    result = "{result}{pad}{k}: {s}".fmt()
    if v.value.isSome():
      result = "{result} {$(v.value.get())}\n".fmt()
    else:
      result = "{result}\n".fmt()

  for k, v in scope.entries.mpairs():
    if v.subscope.isNone(): continue
    let subscope = v.subscope.get()
    result = result & "{pad[0 .. ^2]}[subscope {k}]:\n".fmt()
    let s = `$`(subscope, indent + 2)
    
    result = result & s
    
proc `$`*(scope: Con4mScope, goDown=true): string =

  if not goDown:
    for k, v in scope.entries:
      let s = $(v.tInfo)
      result = "{result}{k}: {s}\n".fmt()

    if scope.parent.isSome():
      let parent = scope.parent.get()
      result = "{result}\n--------------\nPrevious Scope:\n".fmt()
      result = "{result}--------------\n".fmt()
      result = "{result}{$parent}".fmt()

  else:
    return `$`(scope, 0)

proc `$`*(spec: AttrSpec): string =
  result = "type: {spec.attrType}, required:".fmt()
  if spec.required:
    result = "{result} true, default:".fmt()
  else:
    result = "{result} false, default:".fmt()
  if spec.defaultVal.isSome():
    result = "{result} {`$`(spec.defaultVal.get())}".fmt()
  else:
    result = "{result} none".fmt()

proc `$`*(attrs: FieldAttrs): string =
  var s: seq[string]

  for k, v in attrs:
    s.add("  {k} : {`$`(v)}".fmt())
  
  return s.join("\n  ")
