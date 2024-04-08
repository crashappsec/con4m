## The IR is essentially the graph representation on which we do all checking.
## Ideally, we will keep refining the nodes until we can use them essentially
## as fat VM instructions that we can directly marshal.

import "std"/terminal
import "."/[parse, scope]
export parse, scope

# When we are indexing into a tuple, we go ahead and constant-fold the
# index immediately, instead of waiting till the folding pass.
proc foldDown(ctx: Module, newNode: IrNode) {.importc, cdecl.}
proc parseTreeToIr(ctx: Module): IrNode

proc rangeValidator(ctx: RuntimeState, path: string, t: TypeSpec, val: pointer,
                     param: FlexArray[pointer]): Rich {.importc, cdecl.}

proc choiceValidator(ctx: RuntimeState, path: string, t: TypeSpec, val: pointer,
                      param: FlexArray[pointer]): Rich {.importc, cdecl.}
proc userFieldValidator(ctx: RuntimeState, path: string, t: TypeSpec,
                        val: pointer, param: ptr ZCallback): Rich
                                                         {.importc, cdecl.}
proc userSectionValidator(ctx: RuntimeState, path: string,
                          fields: FlexArray[pointer], cb: ptr ZCallback): Rich
                             {.importc, cdecl.}

proc newSpec(): ValidationSpec {.importc, cdecl.}

proc getRootSection(spec: ValidationSpec): SectionSpec {.importc, cdecl.}

proc resultingNumType(m: Module, t1, t2: TypeSpec): TypeSpec =
  var warn: cint

  result = get_promotion_type(t1, t2, addr warn)

  if (warn != 0):
    m.irWarn("UtoSSameSz")

template getTid*(n: untyped): TypeSpec =
  n.tid.followForwards()

proc isConstant*(n: IrNode): bool =
  # Doesn't capture everything that can be constant, just things we
  # are currently folding.
  return n.getTid() != tspec_error() and n != nil

proc lockFn*(impl: FuncInfo) =
  impl.frozen = true
  let to      = impl.tid.followForwards()
  tspec_lock(to)

proc unlockFn*(impl: FuncInfo) =
  impl.frozen = false
  let to      = impl.tid.followForwards()
  tspec_unlock(to)

template withFnLock(fi: FuncInfo, code: untyped) =
  let
    tobj = fi.tid.followForwards()
    lock = tspec_is_locked(tobj)

  tspec_lock(tobj)

  try:
    code
  finally:
    if lock:
      tspec_lock(tobj)
    else:
      tspec_unlock(tobj)

proc evalConstExpr(ctx: Module, n: ParseNode, prop: string): IrNode =
  ctx.current = nil
  ctx.pt      = n
  result      = ctx.parseTreeToIr()
  ctx.foldDown(result)

  if not result.isConstant():
    ctx.irError("NotConst", @[prop])
    return nil

proc findPromotionType(ctx: Module, t1, t2: TypeSpec): TypeSpec =
  # Assumes we're trying to promote either type into either type.
  var
    t1  = t1.followForwards()
    t2  = t2.followForwards()
    to1 = t1.followForwards()
    to2 = t2.followForwards()
    k1  = to1.get_type_kind()
    k2  = to2.get_type_kind()

  if k1 != k2:
    return tspec_error()
  case k1
  of BT_LIST:
    return tspec_list(ctx.findPromotionType(to1.getParam(0),
                                            to2.getParam(0)))
  of BT_DICT:
    return tspec_dict(ctx.findPromotionType(to1.getParam(0), to2.getParam(0)),
                      ctx.findPromotionType(to1.getParam(1), to2.getParam(1)))
  of BT_TUPLE:
    var pi: seq[TypeSpec]
    let
      n1 = to1.num_params()
      n2 = to1.num_params()

    if n1 != n2:
      return tspec_error()

    for i in 0 ..< n1:
      let
        item1 = to1.getParam(i)
        item2 = to2.getParam(i)
      pi.add(ctx.findPromotionType(item1, item2))
    return tspec_tuple(toXList(pi))
  of BT_PRIMATIVE:
    if t1.isIntType():
      return ctx.resultingNumType(t1, t2)
  else:
    discard

  return t1.unify(t2)

proc addCast(node: var IrNode, promoteTo: TypeSpec) =
  var newNode = IrNode(contents:  IrContents(kind: IrCast, srcData: node),
                       parseNode: node.parseNode,
                       tid:       promoteTo,
                       parent:    node.parent,
                       scope:     node.scope)
  node.parent = newNode
  node        = newNode

# This version is meant for when there's a fixed type ID we're expecting,
# such as for things that take a boolean type.
proc unifyOrCast(ctx: Module, t: TypeSpec, irNode: var IrNode): TypeSpec =
  result = t.unify(irNode.tid)

  if result == tspec_error():
    if t.unify(tspec_bool()) != tspec_error():
      ctx.irWarn("BoolAutoCast", irNode, @[irNode.getTid().toString()])
      irNode.addCast(tspec_bool())
    else:
      # Checks to see if we can get them to the same type, but
      # generally we called this because the RHS has to promote,
      # so if the result isn't t, it's an error.
      result = ctx.findPromotionType(t, irNode.getTid())

      if result != t.followForwards():
        ctx.irError("CannotCast", irNode, @[irNode.getTid().toString(),
                                            t.toString()])
        # No need to force bottom here; we could end up with extraneous
        # errors.
      else:
        irNode.addCast(result)

# This version is for use when we have 2 or more nodes all expected to
# be the same type.
proc unifyOrCast(ctx:   Module,
                 nodes: var openarray[IrNode],
                 errIx: var int): TypeSpec =
  var
    curTid   = nodes[0].tid.followForwards()
    dontExit = true

  # We loop through all the items we need to cast at this level.  If
  # we find a new type in the middle that we need to promote previous
  # nodes into (via casting them), we go ahead and start the loop
  # over, which is why the `for` loop is wrapped in a `while`
  # loop. The `while` loop's first order of business is to set
  # `dontExit` to false, in hopes that its going to exit normally;
  # only if we have to restart does it get set to true.

  while dontExit:
    dontExit = false
    for i, item in nodes:
      let t = item.tid.followForwards()
      if t == curTid:
        continue
      if t.unify(curTid) != tspec_error():
        continue
      let newType = ctx.findPromotionType(curTid, t)
      if newType == curTid:
        nodes[i].addCast(curTid)
        continue
      elif newType == tspec_error():
        errIx = i
        return tspec_error()
      else:
        curTid = newType
        dontExit = true
        break

  return curTid

# For binary ops. The err param is extraneous for them.
proc unifyOrCast(ctx: Module, n1, n2: var IrNode): TypeSpec =
  var errIx: int
  var nodes = [n1, n2]

  result = ctx.unifyOrCast(nodes, errIx)
  n1     = nodes[0]
  n2     = nodes[1]

template doc_extract(t: string): Rich =
  r(unicode.strip(t))

proc extractShortDoc(n: ParseNode): Rich =
  if n == nil or n.docNodes == nil:
    return nil
  else:
    return doc_extract(n.docNodes.children[0].getText())

proc extractShortDocPlain(n: ParseNode): string =
  if n == nil or n.docNodes == nil:
    return ""
  else:
    return n.docNodes.children[0].getText()

proc extractLongDoc(n: ParseNode): Rich =
  if n == nil or n.docNodes == nil or len(n.docNodes.children) < 2:
    return nil
  else:
    return doc_extract(n.docNodes.children[1].getText())

proc extractLongDocPlain(n: ParseNode): string =
  if n == nil or n.docNodes == nil or len(n.docNodes.children) < 2:
    return ""
  else:
    return n.docNodes.children[1].getText()


proc fmt(s: string, x = "", y = "", t = tspec_error()): Rich =
  result = atom(s).fgColor("atomiclime")
  if x != "":
    result = result + atom(" " & x).em()

  if y != "":
      result = result + atom(" (" & y & ")").fgcolor("fandango")

  if t != tspec_error():
    result = result + atom(" ") + bold(t.toString())

template reprBasicLiteral(ctx: IrNode): string =
  if ctx.value == nil:
    ""
  elif ctx.tid.get_type_kind() == BT_FUNC:
    # Special cased because I am weary.
    let v = cast[ptr Callback](ctx.value)
    if v == nil:
      "buggy_callback()"
    else:
      ($(v.name) & v.tid.toString())
  else:
    con4m_repr(ctx.value, ctx.tid).toNimStr()

proc irWalker(ctx: IrNode): (Rich, seq[IrNode]) =
  var
    descriptor: string
    moreinfo:   string

  if ctx == nil or ctx.contents == nil:
    return (em("empty"), @[])

  case ctx.contents.kind
  of IrBlock:
    return (fmt("Block"), ctx.contents.stmts)
  of IrSection:
    var sec: string = ctx.contents.prefix

    if sec == "":
      sec = ctx.contents.sectName
    else:
      sec &= "." & ctx.contents.sectName

    return (fmt("Section", sec, ctx.contents.instance), @[ctx.contents.blk])
  of IrLoop:
    if ctx.contents.label != nil:
      moreinfo = "label: " & ctx.contents.label.getText()
    if ctx.contents.whileLoop:
      descriptor = "while"
    else:
      descriptor = "for / "
      if ctx.contents.condition.contents.kind == IrRange:
        descriptor &= " from"
      else:
        descriptor &= " in"
      var loopvars: seq[string]
      for item in ctx.contents.loopVars:
        loopvars.add(item.name)
      moreInfo &= " var(s): " & loopvars.join(", ")

    return (fmt("Loop", descriptor, moreinfo),
            @[ctx.contents.condition, ctx.contents.loopBody])
  of IrAssign:
    if ctx.lock:
      moreinfo = "+lock"
    return (fmt("Assign", "", moreinfo, ctx.getTid()),
            @[ctx.contents.assignLhs, ctx.contents.assignRhs])
  of IrConditional:
    return (fmt("Conditional"), @[ctx.contents.predicate,
                                  ctx.contents.trueBranch,
                                  ctx.contents.falseBranch])
  of IrJump:
    if ctx.contents.exitLoop:
      moreinfo = "break"
    else:
      moreinfo = "continue"
    if ctx.contents.targetNode.contents.label != nil:
      let labelStr = ctx.contents.targetNode.contents.label.getText()
      result = (fmt("Jump", moreinfo, "label: " & labelStr), @[])
    else:
      result = (fmt("Jump", moreinfo), @[])
  of IrRet:
    return (fmt("Return", "", "", ctx.getTid()), @[ctx.contents.retVal])
  of IrLit:
    if ctx.getTid().getTypeKind == BT_PRIMATIVE:
      moreinfo = ctx.reprBasicLiteral()
    return (fmt("Literal", "", moreinfo, ctx.getTid()), ctx.contents.items)
  of IrNil:
    return (fmt("Nil", "", "", ctx.getTid()), @[])
  of IrFold:
    return (fmt("Folded", "", ctx.reprBasicLiteral(), ctx.getTid()), @[])
  of IrNop:
    return (fmt("Nop"), @[])
  of IrMember, IrMemberLhs:
    if ctx.lock:
      moreinfo = "+lock"
    return (fmt("Member", ctx.contents.name, moreinfo, ctx.getTid()), @[])
  of IrIndex, IrIndexLhs:
    return (fmt("Index"), @[ctx.contents.toIx, ctx.contents.indexStart,
                            ctx.contents.indexEnd])
  of IrCall:
    descriptor = ctx.contents.fname
    if ctx.contents.module != "":
      descriptor = ctx.contents.module & "::" & descriptor
    if ctx.contents.replacement:
      moreinfo = "converted op to call"
    return (fmt("Call", descriptor, moreinfo, ctx.getTid()),
            ctx.contents.actuals)
  of IrUse:
    return (fmt("Use", ctx.contents.targetModule, ctx.contents.targetLoc), @[])
  of IrUminus:
    return (fmt("Uminus", "", "", ctx.getTid()), @[ctx.contents.uRhs])
  of IrNot:
    return (fmt("Not"), @[ctx.contents.uRhs])
  of IrBinary:
    return (fmt("BinaryOp", ctx.contents.bOp, "", ctx.getTid()),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrBool:
    return (fmt("BooleanOp", ctx.contents.bOp, "", ctx.getTid()),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrLogic:
    return (fmt("LogicOp", ctx.contents.bOp, "", ctx.getTid()),
            @[ctx.contents.bLhs, ctx.contents.bRhs])
  of IrLhsLoad:
    return (fmt("LhsLoad", ctx.contents.symbol.name, "", ctx.getTid()), @[])
  of IrLoad:
    if ctx.lock:
      moreinfo = "+lock"
    return (fmt("Load", ctx.contents.symbol.name, moreinfo, ctx.getTid()), @[])
  of IrSwitch:
    if ctx.contents.typecase:
      moreinfo = "type"
    else:
      moreinfo = "value"
    return (fmt("Switch", moreinfo, ""), @[ctx.contents.switchTarget] &
            ctx.contents.branches)
  of IrSwitchBranch:
    return (fmt("Branch"), ctx.contents.conditions & @[ctx.contents.action])
  of IrRange:
    return (fmt("Range"), @[ctx.contents.rangeStart, ctx.contents.rangeEnd])
  of IrAssert:
    return (fmt("Assert"), @[ctx.contents.assertion])
  of IrCast:
    return (fmt("Cast"), @[ctx.contents.srcData])

proc treeDown(t: Tree, kids: seq[IrNode]) =
  for kid in kids:
    let (toPrint, subkids) = irWalker(kid)
    let sub = t.add_node(toPrint)
    treeDown(sub, subkids)

proc toRich*(ctx: IrNode): Grid =
  var (toPrint, kids) = irWalker(ctx)
  var t: Tree = new_tree(toPrint)
  treeDown(t, kids)

  return grid_tree(t)

proc getLitMod(ctx: Module): string =
  return ctx.pt.token.litType

proc irNode(ctx: Module, kind: IrNodeType): IrNode =
  let payload = IrContents(kind: kind)
  result = IrNode(parseNode: ctx.pt, contents: payload, parent: ctx.current)
  ctx.current = result

proc downNode(ctx: Module, n: ParseNode): IrNode =
  var saved   = ctx.current
  var savedpt = ctx.pt
  ctx.pt      = n
  result      = ctx.parseTreeToIr()
  ctx.pt      = savedpt
  ctx.current = saved

proc downNode(ctx: Module, which: int): IrNode =
  var saved   = ctx.current
  var savedpt = ctx.pt
  ctx.pt      = ctx.pt.children[which]
  result      = ctx.parseTreeToIr()
  ctx.pt      = savedpt
  ctx.current = saved

proc downNode(ctx: Module, kid, grandkid: int): IrNode =
  var saved = ctx.current
  var savedpt = ctx.pt

  ctx.pt      = ctx.pt.children[kid].children[grandkid]
  result      = ctx.parseTreeToIr()
  ctx.pt      = savedpt
  ctx.current = saved

proc downNode(ctx: Module, kid, gk, ggk: int): IrNode =
  var saved = ctx.current
  var savedpt = ctx.pt

  ctx.pt      = ctx.pt.children[kid].children[gk].children[ggk]
  result      = ctx.parseTreeToIr()
  ctx.pt      = savedpt
  ctx.current = saved

template independentSubtree(code: untyped) =
  var saved = ctx.current

  ctx.current = nil
  code
  ctx.current = saved

template getText(ctx: Module): string =
  ctx.pt.getText()

template getText(ctx: Module, which: int): string =
   ctx.pt.children[which].getText()

template getText(ctx: Module, kidIx: int, grandIx: int): string =
  ctx.pt.children[kidIx].children[grandIx].getText()

template numKids(ctx: Module): int =
  ctx.pt.children.len()

template numKids(ctx: Module, i: int): int =
  ctx.pt.children[i].children.len()

template numKids(ctx: Module, i, j: int): int =
  ctx.pt.children[i].children[j].children.len()

template kidKind(ctx: Module, i: int): ParseNodeKind =
  ctx.pt.children[i].kind

template kidKind(ctx: Module, i, j: int): ParseNodeKind =
  ctx.pt.children[i].children[j].kind

template parseKid(ctx: Module, i: int): ParseNode =
  ctx.pt.children[i]

template parseKid(ctx: Module, i, j: int): ParseNode =
  ctx.pt.children[i].children[j]

template parseKid(ctx: Module, i, j, k: int): ParseNode =
  ctx.pt.children[i].children[j].children[k]

const
  ntLoops        = [ NodeForStmt, NodeWhileStmt ]
  ntConditionals = [ NodeIfStmt, NodeElifStmt ]

proc addFalseBranch(conditional: IrNode, falseBranch: IrNode) =
  var n = conditional
  while n.contents.falseBranch != nil:
    n = n.contents.falseBranch

  n.contents.falseBranch = falseBranch

proc checkLabelDupe(ctx: Module, label: string) =
  var
    n       = ctx.current.parent

  while n != nil:
    if n.contents.kind == IrLoop and n.contents.label != nil and
       n.contents.label.getText() == label:
      ctx.irWarn("LabelDupe", n, @[label])
      return
    n = n.parent

proc convertEnum*(ctx: Module) =
  var
    usedVals: seq[int]
    nextVal = 0
    value: int64

  independentSubtree:
    for i in 0 ..< ctx.numKids():
      let itemName = ctx.getText(i, 0)

      if ctx.numKids(i) == 2:
        let v = ctx.downNode(i, 1)
        if not v.isConstant():
          ctx.irError("EnumDeclConst", w = ctx.parseKid(i))
          continue
        if not v.getTid().isIntType():
          ctx.irError("EnumInt", w = ctx.parseKid(i))
          continue
        value = cast[int64](v.value)
        if value in usedVals:
          ctx.irWarn("EnumReuse", @[$value], w = ctx.parseKid(i))
        else:
          usedVals.add(value)
          if value > nextVal:
            nextVal = value + 1
      else:
        value = nextVal
        usedVals.add(value)
        nextVal = nextVal + 1

      if itemName[0] == '$':
        ctx.irError("$assign", w = ctx.parseKid(i, 0))

      var
        symOpt = ctx.scopeDeclare(ctx.moduleScope, itemName, false,
                                  tspec_i64(), declnode = ctx.parseKid(i, 0))

      if symOpt.isSome():
        let sym = symOpt.get()
        sym.constValue = cast[pointer](value)
        sym.haveConst = true
        sym.immutable = true

proc convertIdentifier(ctx: Module): IrNode =
  var name = ctx.pt.getText()
  var stOpt: Option[SymbolInfo]

  if ctx.lhsContext:
    if ctx.attrContext and ctx.curSecPrefix != "":
      name = ctx.curSecPrefix & "." & name

    result = ctx.irNode(IrLhsLoad)
    stOpt  = ctx.addDef(name, result, tspec_typevar())

  else:
    result = ctx.irNode(IrLoad)
    stOpt  = ctx.addUse(name, result, tspec_typevar())

  result.contents.symbol = stOpt.getOrElse(nil)

  if result.contents.symbol != nil:
    result.tid = result.contents.symbol.getTid()

proc convertAssignment(ctx: Module): IrNode =
  result                  = ctx.irNode(IrAssign)
  ctx.lhsContext          = true
  ctx.attrContext         = true
  if ctx.pt.getText() != ":":
    ctx.ambigAssign         = true

  result.contents.assignLhs = ctx.downNode(0)
  ctx.attrContext           = false
  ctx.lhsContext            = false
  ctx.ambigAssign           = false
  result.contents.assignRhs = ctx.downNode(1)

  result.tid = ctx.typeCheck(result.contents.assignRhs.getTid(),
                             result.contents.assignLhs.getTid())

proc convertMember(ctx: Module): IrNode =
  var subaccess: IrNode

  var parts: seq[string]
  for i, kid in ctx.pt.children:
    case kid.kind:
    of NodeIdentifier:
      parts.add(kid.getText())
    else:
      # For now, this whole branch should be unreachable actually.
      if i != ctx.pt.children.len() - 1:
        ctx.irError("MemberTop", w = ctx.pt)
        return
      subaccess = ctx.downnode(i)

  if ctx.lhsContext and subaccess == nil:
    result = ctx.irNode(IrMemberLhs)
  else:
    result = ctx.irNode(IrMember)

  result.contents.name      = parts.join(".")
  result.contents.subaccess = subaccess

  if ctx.attrContext:
    if ctx.curSecPrefix != "":
      result.contents.name = ctx.curSecPrefix & "." & result.contents.name

  let sym = ctx.addDef(result.contents.name, result, tspec_typevar())

  if sym.isSome():
    result.contents.attrSym = sym.get()
    result.tid              = result.contents.attrSym.getTid()

proc extractSymInfo(ctx: Module, scope: Scope, isConst = false,
                    checkdollar = false): SymbolInfo {.discardable.} =
  # Returns the last symbol; useful for convertParamBlock where
  # it only accepts one symbol.
  var
    toAdd: seq[(string, TypeSpec, ParseNode)]

  for varNode in ctx.pt.children:
    if varNode.kind in [NodeGlobalStmt, NodeVarStmt, NodeConstStmt]:
      continue
    var
      varNames:  seq[string]
      foundType: TypeSpec = tspec_error()
    for kid in varNode.children:
      if kid.kind == NodeIdentifier:
        let name = kid.getText()
        if name[0] == '$':
          ctx.irError("$assign", w = kid)
        varNames.add(name)
      else:
        foundType = kid.buildType()
    for i, oneVarName in varNames:
      if foundType == tspec_error():
        toAdd.add((oneVarName, tspec_typevar(), varnode.children[i]))
      else:
        toAdd.add((oneVarName, foundType.copyType(),
                   varnode.children[i]))

  for i, (name, tid, kid) in toAdd:
    let symOpt = ctx.scopeDeclare(scope, name, false, tid.getTid(), isConst,
                                  declnode = kid, inparam = ctx.inParamCtx)
    if i == toAdd.len() - 1 and symOpt.isSome():
      result = symOpt.get()

proc convertspec_typevarStmt(ctx: Module) =
  var
    symbolsAreConst = false
    sym: SymbolInfo

  if ctx.pt.children[^1].kind == NodeConstStmt:
    symbolsAreConst = true

  if ctx.funcScope != nil:
    sym = ctx.extractSymInfo(ctx.funcScope, symbolsAreConst,
                               checkdollar = true)
  else:
    sym = ctx.extractSymInfo(ctx.moduleScope, symbolsAreConst,
                             checkdollar = true)

proc convertGlobalStmt(ctx: Module) =
  var symbolsAreConst = false

  if ctx.pt.children.len() == 0:
    return

  if ctx.pt.children[^1].kind == NodeConstStmt:
    symbolsAreConst = true

  ctx.extractSymInfo(ctx.globalScope, symbolsAreConst, checkdollar = true)

proc convertConstStmt(ctx: Module) =
  var scope: Scope

  if ctx.pt.children.len() == 0:
    return

  if ctx.pt.children[^1].kind == NodeGlobalStmt:
    scope = ctx.globalScope
  elif ctx.funcScope == nil:
    scope = ctx.moduleScope
  else:
    scope = ctx.funcScope

  ctx.extractSymInfo(scope, true, checkdollar = true)

proc convertParamBody(ctx: Module, sym: var SymbolInfo) =
  var
    gotValid, gotDefault, gotInitialize, gotPrivate, isPrivate: bool
    paramInfo = ParamInfo(shortdoc: ctx.pt.extractShortDoc(),
                          longdoc:  ctx.pt.extractLongDoc())

  independentSubtree:
    for i in 0 ..< ctx.numKids():
      if ctx.kidKind(i) != NodeAssign:
        ctx.irError("No code is allowed inside parameter blocks")
        continue
      let propname = ctx.getText(i, 0)
      case propname
      of "initialize":
        if gotDefault:
          ctx.irError("DefaultMutex", @[], ctx.pt.children[i])
        if gotInitialize:
          ctx.irError("DupeParamProp", @["initialize"], ctx.pt.children[i])
        let
          irNode = ctx.downNode(i, 1)
          to     = irNode.tid.followForwards()
          kind   = to.get_type_kind()

        if to.get_type_kind() != BT_FUNC:
          ctx.irError("ParamType", @["initializor", "callback"],
                      ctx.pt.children[i])
        elif to.num_params() != 0:
          if to.num_params() != 1:
            ctx.irError("InitArg", @[], ctx.pt.children[i])
          else:
            if unify(tspec_varargs_fn(sym.tid, 0), irNode.tid) == tspec_error():
              ctx.irError("InitArg", @[], ctx.pt.children[i])

        paramInfo.initializeIr = some(irNode)
        gotInitialize          = true

      of "validator":
        let
          irNode = ctx.downNode(i, 1)
          to     = irNode.tid.followForwards()

        if to.get_type_kind() != BT_FUNC:
          ctx.irError("ParamType", @["validator", "callback"],
                      ctx.pt.children[i])
        elif to.num_params() != 0:
          if to.num_params() != 2:
            ctx.irError("ParamValNArgs", @[], ctx.pt.children[i])
          else:
            if unify(sym.tid, to.get_param(0)) == tspec_error():
              ctx.irError("ParamValParTy", @[sym.tid.toString(),
                                             irnode.tid.toString()],
                          w = irNode.parseNode)

            elif unify(irNode.tid,
                       tspec_fn(tspec_string(), toXList(@[sym.tid]), false)) ==
                       tspec_error():
                         ctx.irError("ParamValRetTy")

        paramInfo.validatorIr = some(irNode)
        gotValid              = true

      of "private":
        if gotPrivate:
          ctx.irError("DupeParamProp", @["private"], ctx.pt.children[i])
        else:
          gotPrivate = true
          let irNode = ctx.downNode(i, 1)
          ctx.typeCheck(irNode.tid, tspec_bool(), where = irNode.parseNode)
          if irNode.contents.kind != IrLit:
            ctx.irError("LitRequired",
                        @["<em>private</em> field for parameters", "bool"],
                        ctx.pt.children[i])
          isPrivate = cast[bool](irNode.value)
      of "default":
        if gotInitialize:
          ctx.irError("DefaultMutex", @[], ctx.pt.children[i])
        if gotDefault:
          ctx.irError("DupeParamProp", @["default"], ctx.pt.children[i])
          continue
        let irNode = ctx.downNode(i, 1)
        paramInfo.defaultParse = some(ctx.pt.children[i].children[1])
        gotDefault             = true
        ctx.typeCheck(irNode.tid, sym.tid, where = irNode.parseNode)
      else:
        ctx.irError("BadParamProp", @[propname], ctx.pt.children[i])
        continue

  paramInfo.private = isPrivate
  paramInfo.sym     = sym
  sym.pinfo         = paramInfo

  if isPrivate and not gotDefault and not gotInitialize:
    ctx.irError("CantInitialize")

  ctx.params.add(paramInfo)

proc convertParamBlock(ctx: Module) =
  var sym: SymbolInfo
  var ir:  IrNode

  ctx.inParamCtx = true


  if ctx.pt.children[0].kind == NodeMember:

    independentSubtree:
      ir = ctx.downNode(0)
      sym = ctx.scopeDeclare(ctx.usedAttrs, ir.contents.name, false,
                             tspec_typevar()).getOrElse(nil)
  else:
    let
      ir   = IrNode()
      n    = ctx.pt.children[0].children[0]
      name = n.getText()
      opt  = ctx.scopeDeclare(ctx.moduleScope, name, false, tid = tspec_typevar(),
                              declNode = n, inparam = true)

    if opt.isSome():
      sym = opt.get()
    else:
      unreachable # TODO, make sure this is true

  sym.defs.add(ir)

  var savedPt = ctx.pt
  ctx.pt = ctx.pt.children[1]
  ctx.convertParamBody(sym)
  ctx.pt = savedPt

  ctx.inParamCtx = false

proc extractCTypes(ctx: Module, n: ParseNode, fn: var FuncInfo) =
  var
    info = ExternFnInfo()

  for i, kid in n.children:
    var
      l: seq[string] # All type info for C parameter.
      s: string

    for j, item in kid.children:
      if j == 0:
        if item.kind == NodeMember:
          s = item.children[0].getText()
          if s in info.cParamNames:
            ctx.irError("DupeCTypeParam", @[s], w = item)
          else:
            info.cParamNames.add(s)
            continue
        else:
          info.cParamNames.add("")

      s = item.getText()
      l.add(s)

    info.ffiArgTypes[i] = l[0].lookupFfiArgType()
    info.cArgTypes.add(l)

  let sz = n.children.len()

  ffi_prep_cif(info.ffiInfo, ffiAbi, cuint(sz - 1), info.ffiArgTypes[sz - 1],
               addr info.ffiArgTypes[0])

  fn.externInfo = info

proc convertFormal(info: FuncInfo, n: ParseNode) =
  var formalInfo = FormalInfo()

  if n.children[0].kind == NodeIdentifier:
    formalInfo.name = n.children[0].getText()
    formalInfo.loc  = n.children[0]
  else:
    formalInfo.va   = true
    formalInfo.name = n.children[0].children[0].getText()
    formalInfo.loc  = n.children[0].children[0]

  if n.children.len() == 2:
    formalInfo.tid = n.children[1].buildType()
  else:
    formalInfo.tid = tspec_typevar()

  info.params.add(formalInfo)
  info.paramNames.add(formalInfo.name) # TODO: redundant w/ formalInfo.name

proc setupTypeSignature(info: FuncInfo, n: ParseNode) =
  if n != nil:
    info.retVal     = FormalInfo(name: "result")
    info.retval.tid = n.buildType()
  else:
    info.retVal = FormalInfo(name: "result", tid: tspec_typevar())

  var
    actTypes: seq[TypeSpec]
    va = false

  if info.params.len() != 0 and info.params[^1].va:
    va = true

  for actual in info.params:
    actTypes.add(actual.getTid())

  info.tid = tspec_fn(info.retVal.getTid(), toXList(actTypes), va)

proc extractLocalSym(ctx: Module, n: ParseNode, info: FuncInfo) =
  info.name = n.children[0].getText()
  for item in n.children[1].children:
     if item.children.len() == 1:
      ctx.irError("NeedExName", w = item)
      return
     info.convertFormal(item)

  var paramNames: seq[string]
  for item in info.params:
    if item.name in paramNames:
      ctx.irError("DupeExParam", @[item.name], w = item.loc)
    else:
      paramNames.add(item.name)
  info.setupTypeSignature(n.children[^1].children[0])
  info.lockFn()

proc checkHolds(ctx: Module, n: ParseNode, info: var ExternFnInfo) =
  var argNames: seq[string]

  for item in n.children:
    let s = item.getText()
    if s in argNames:
      ctx.irWarn("DupeVal", @[s, "holds"], w = item)
    argNames.add(s)

    let n = info.cParamNames.find(s)
    if n == -1:
      ctx.irError("ExtNotSpecd", @[s], w = item)
    elif n in info.allocedParams:
      ctx.irError("ExtAllocNHold", @[s], w = item)
    elif n < info.cArgTypes.len() and
         info.cArgTypes[n][0] notin ["ptr", "cstring", "carray"]:
      ctx.irError("NoMemMan", @[s], item)
    else:
      info.heldParams.add(n)

proc checkAllocs(ctx: Module, n: ParseNode, info: var ExternFnInfo) =
  var argNames: seq[string]

  for item in n.children:
    var s: string
    if item.kind == NodeExternReturn:
      s = "result"
    else:
      s = item.getText()
    if s in argNames:
      ctx.irWarn("DupeVal", @[s, "allocs"], w = item)
    argNames.add(s)

    if s == "result":
      info.allocedParams.add(info.cParamNames.len())
      continue

    let n = info.cParamNames.find(s)
    if n == -1:
      ctx.irError("ExtNotSpecd", @[s], w = item)
    elif n in info.heldParams:
      ctx.irError("ExtAllocNHold", @[s], w = item)
    else:
      info.allocedParams.add(n)

proc convertExternBlock(ctx: Module) =
  var
    sym: SymbolInfo
    info     = ExternFnInfo(externName: ctx.getText(0))
    fn       = FuncInfo()

    gotLocal = false
    gotPure  = false
    gotHold  = false
    gotAlloc = false
    gotDll   = false

  fn.externName = info.externName
  fn.externInfo = info

  ctx.extractCTypes(ctx.pt.children[1], fn)

  for i in 2 ..< ctx.numKids():
    let k = ctx.pt.children[i]
    case k.kind
    of NodeDocString:
      fn.shortdoc = ctx.pt.children[i].extractShortDoc()
      fn.longdoc  = ctx.pt.children[i].extractLongDoc()
    of NodeExternLocal:
      if gotLocal:
        ctx.irError("DupeExtField", @["local"])
        continue
      gotLocal = true
      ctx.extractLocalSym(k, fn)
    of NodeExternDll:
      gotDll = true

      let dllName = ctx.getText(i, 0)
      if dllName in fn.externInfo.dlls:
        ctx.irWarn("DupeDllName", @[dllName])
      else:
        fn.externInfo.dlls.add(dllName)
    of NodeExternPure:
      if gotPure:
        ctx.irError("DupeExtField", @["pure"])
        continue
      gotPure = true
      if ctx.getText(i, 0) == "true":
        fn.pure = true
    of NodeExternHolds:
      if gotHold:
        ctx.irWarn("DupeExtField", @["holds"])
      gotHold = true
      ctx.checkHolds(k, fn.externInfo)
    of NodeExternAllocs:
      if gotAlloc:
        ctx.irWarn("DupeExtField", @["allocs"])
      gotAlloc = true
      ctx.checkAllocs(k, fn.externInfo)
    else:
      unreachable

  if gotDll:
    fn.externInfo.binPtr = findSymbol(info.externName, fn.externInfo.dlls)
  else:
    fn.externInfo.binPtr = findSymbol(info.externName, [])

  if fn.externInfo.binPtr == nil:
    ctx.irWarn("WontLink", @[info.externName], w = ctx.pt.children[0])

  if not gotPure:
    once:
      ctx.irWarn("PurePlz", @[info.externName], w = ctx.pt.children[0])
  ## To do:
  ## 1. Check that the local signature is compat w/ the external one.
  ## 2. Error / warn on alloc / hold for non-object / ptr fields.

  var symOpt = ctx.scopeDeclare(ctx.moduleScope, fn.name, true)
  if symOpt.isSome():
    symOpt.get().fimpls.add(fn)

  symOpt = ctx.globalScope.table.lookup(fn.name)
  if symOpt.isSome():
    sym = symOpt.get()
    if not sym.isFunc:
      ctx.irWarn("CantLiftFunc", @[fn.name], w = ctx.pt)
      return
  else:
    sym = ctx.scopeDeclare(ctx.globalScope, fn.name, true).get()

  sym.fimpls.add(fn)


proc handleFuncdefSymbols(ctx:   Module,
                          info:  FuncInfo) =
  var
    symOpt: Option[SymbolInfo]
    allParams = info.params & @[info.retVal]


  info.fnScope = initScope(fn = true)

  for i, item in allParams:
    symOpt = ctx.scopeDeclare(info.fnScope, item.name, false, item.getTid(),
                              declNode = item.loc)
    allParams[i].sym = symOpt.getOrElse(nil)
    if i != allParams.len() - 1:
      # This will give these variables negative stack offsets; the space for
      # the return value doesn't get that treatment; it is the only
      # thing we currently pass in a register, and will live like any
      # other variable on the stack until it's time to return.
      allParams[i].sym.formal = true

  symOpt = ctx.scopeDeclare(ctx.moduleScope, info.name, true)


  if symOpt.isSome():
    symOpt.get().fimpls.add(info)

proc findDeclarations(ctx: Module, n: ParseNode)

proc convertFuncDefinition(ctx: Module) =
  ## This converts the declaration portion, NOT the body. For now, the
  ## body just goes into the FuncInfo `rawImpl` parameter. Once we have
  ## found all explicitly declared symbols, we then come back to
  ## convert the tree into IR.
  var
    funcName   = ctx.getText(0)
    info       = FuncInfo(defModule: ctx)
    returnType = ParseNode(nil)

  # Params are in the second node, and the last item there might
  # have the varargs marker, which changes what we insert into the symbol
  # table for that thing.
  for item in ctx.pt.children[1].children:
    info.convertFormal(item)

  if ctx.numKids() == 4:
    returnType = ctx.pt.children[2].children[0]

  info.setupTypeSignature(returnType)
  info.name = funcName
  ctx.handleFuncdefSymbols(info)

  info.rawImpl  = ctx.pt.children[^1]
  info.shortdoc = ctx.pt.children[^1].extractShortDoc()
  info.longdoc  = ctx.pt.children[^1].extractLongDoc()

  ctx.funcScope = info.fnScope
  ctx.findDeclarations(info.rawImpl)
  ctx.funcScope = nil
  info.lockFn()


  # Below, we also 'lift' this function into the global scope, unless
  # there is a global variable that currently conflicts.
  #
  # In the future we may add 'private' functions that stop them from
  # being lifted into the global symbol table.
  var sym: SymbolInfo
  let symOpt = ctx.globalScope.table.lookup(funcName)
  if symOpt.isSome():
    sym = symOpt.get()
    if not sym.isFunc:
      ctx.irWarn("CantLiftFunc", @[funcName], w = info.rawImpl)
      return
  else:
    sym = ctx.scopeDeclare(ctx.globalScope, funcName, true).get()

  sym.fimpls.add(info)

proc processUseStmt(ctx: Module) =
  ## Since currently use statements both import symbols and cause
  ## execution, we want to import symbols early, but we'll also leave
  ## this in the tre, and process it in the 2nd pass w/ convertUseStmt()
  var
    moduleName = ctx.getText(0)
    moduleLoc  = if ctx.numKids() == 2: ctx.getText(1) else: ""

  ctx.usedModules.add((moduleLoc, moduleName))

proc findAndLoadModule(ctx: CompileCtx, location, fname, ext: string):
                       Option[Module] {.importc, cdecl.}

proc convertUseStmt(ctx: Module): IrNode =
  var
    modName = ctx.getText(0)
    loc     = ""
  result                       = ctx.irNode(IrUse)
  result.contents.targetModule = modName
  if ctx.numKids() == 2:
    loc                       = ctx.getText(1)
    result.contents.targetLoc = loc

  let possibleModule = ctx.compileCtx.findAndLoadModule(loc, modName, "")
  if possibleModule.isSome():
    result.contents.moduleObj = possibleModule.get()
    if result.contents.moduleObj notin ctx.imports:
      ctx.imports.add(result.contents.moduleObj)
  else:
    ctx.irError("ModuleNotFound", @[modName])

template boolPropConvert(guard: untyped, name: string): bool =
  if guard:
    ctx.irError("DupeProp", @[name])
    false
  else:
    guard       = true
    let tree    = ctx.evalConstExpr(n.children[1], name)
    if tree == nil:
      false
    elif tree.tid.unify(tspec_bool()) == tspec_error():
        ctx.irError("NotBool", @[name, tree.tid.toString()])
        false
    else:
        cast[bool](tree.value)

proc convertOneField(ctx: Module, f: ParseNode): FieldSpec =
  var
    name         = f.children[0].getText()
    gotType      = false
    gotLock      = false
    gotDef       = false
    gotChoice    = false
    gotRange     = false
    gotHidden    = false
    gotReq       = false
    lock         = false
    hidden       = false
    required     = false

    exclude:    seq[string]
    def:        pointer
    tid:        TypeSpec
    defType:    TypeSpec
    choiceType: TypeSpec

  result = FieldSpec(name: name, fieldKind: FSField)
  for i in 1 ..< f.children.len():
    let n = f.children[i]
    case n.children[0].getText()
    of "type":
      if gotType:
        ctx.irError("DupeProp", @["type"])
        continue
      gotType = true

      if n.children[1].kind != NodeIdentifier:
        result.tid = n.children[1].buildType()
      else:
        result.deferredType = n.children[1].getText()
    of "default":
      if gotDef:
        ctx.irError("DupeProp", @["default"])
        continue
      gotDef   = true
      let tree = ctx.evalConstExpr(n.children[1], "default")

      if tree != nil:
        result.defaultVal  = tree.value
        result.haveDefault = true
        defType            = tree.tid
    of "lock", "locked":
      result.lockOnWrite = boolPropConvert(gotLock, "lock")
    of "hide", "hidden":
      result.hidden = boolPropConvert(gotHidden, "hide")
    of "require", "required":
      result.required = boolPropConvert(gotReq, "required")
    of "choice", "choices":
      if gotChoice:
        ctx.irError("DupeProp", @["choices"])
        continue
      if gotRange:
        ctx.irError("RangeAndChoice")
        continue

      gotChoice = true

      let listLit = n.children[1]
      var s: seq[pointer]

      for i, item in listLit.children:
        let tree = ctx.evalConstExpr(item, "choices")
        if tree == nil:
          s = @[]
          break
        elif i == 0:
          choiceType = tree.tid
        else:
          if choiceType.unify(tree.tid) == tspec_error():
            ctx.irError("TyDiffListItem", @[choiceType.toString(),
                                            tree.tid.toString()])
            s = @[]
            break
          s.add(tree.value)

      var
        raw = newArrayFromSeq[pointer](s)
        arr = cast[pointer](raw)

      GC_ref(raw)

      if s.len() != 0:
        let v = Validator(fn:        choiceValidator,
                          params:    arr,
                          paramType: tspec_list(choiceType))
        result.validators.add(v)
    of "range":
      if gotRange:
        ctx.irError("DupeProp", @["range"])
        continue
      if gotChoice:
        ctx.irError("RangeAndChoice")
        continue
      let
        startNode = ctx.evalConstExpr(n.children[1], "range")
        endNode   = ctx.evalConstExpr(n.children[2], "range")

      if startNode != nil and endNode != nil:
        # This isn't yet checking the actual type, which is a bit
        # problematic.
        if startNode.tid.unify(tspec_i64()) == tspec_error() or
           endNode.tid.unify(tspec_i64()) == tspec_error():
          ctx.irError("RangeInt")
        else:
          let
            startVal = cast[int](startNode.value)
            endVal   = cast[int](endNode.value)

          var f = new_list[int](@[startVal, endVal])

          if startVal >= endVal:
            ctx.irError("BadRangeSpec")

          var
            arr = cast[pointer](f)
            pt  = tspec_list(tspec_i64())
            v   = Validator(fn:        rangeValidator,
                            params:    arr,
                            paramType: pt)

          result.validators.add(v)

    of "exclude", "exclusions":
      for kid in n.children[1 .. ^1]:
        let exclusion = kid.getText()
        if exclusion in result.exclusions:
          ctx.irWarn("DupeExclusion", @[exclusion])
        else:
          result.exclusions.add(exclusion)
    of "validator":
      if n.children[1].kind != NodeCallbackLit:
        ctx.irError("ParamType", @["validator", "callback"], n.children[1])
      let
        irNode = ctx.downNode(n.children[1])
        paramt = tspec_fn(tspec_string(),
                          toXList(@[tspec_string(), tspec_typevar()]),
                          false)
      ctx.typeCheck(irNode.tid, paramt)

      let v = Validator(fn:        userFieldValidator,
                        node:      irNode,
                        paramType: paramt)
    else:
      unreachable

proc convertOneSectionSpec(ctx: Module, sec: ParseNode) =
  var
    rootSection = false
    maxAllowed  = 1
    startIx     = 2
    userDefOk   = false
    gotDefOk    = false
    gotHidden   = false
    hidden      = false
    specLocal   = ctx.declaredSpec
    sectionL:  SectionSpec
    fields:    Dict[Rich, FieldSpec]
    allows:    seq[Rich]
    requires:  seq[Rich]
    validator: Validator
    secName:   string

  if ctx.attrSpec.locked:
    ctx.irError("SpecWhenLocked")

  if sec.children[0].getText() == "root":
    rootSection = true
    startIx     = 1
  else:
    secName = sec.children[1].getText()
    if sec.children[0].getText() == "named":
      maxAllowed = high(int)

  fields.initDict()

  for i in startIx ..< sec.children.len():
    let n = sec.children[i]
    if n.kind == NodeFieldSpec:
      let fs = ctx.convertOneField(n)
      if not fields.add(r(fs.name), fs):
        ctx.irError("DupeSpecField", @[fs.name])
      else:
        fields[r(fs.name)] = fs
    else:
      case n.children[0].getText()
      of "user_def_ok":
        if gotDefOk:
          ctx.irWarn("DupeProp", @["use_def_ok"])
      of "hidden", "hide":
        if gotHidden:
          ctx.irWarn("DupeProp", @["hidden"])
      of "validator":
        if n.children[1].kind != NodeCallbackLit:
          ctx.irError("ParamType", @["validator", "callback"], n.children[1])
        let
          irNode = ctx.downNode(n.children[1])
          paramt = tspec_fn(tspec_string(),
                            toXList(@[tspec_string(),
                                      tspec_list(tspec_string())]),
                            false)

        ctx.typeCheck(irNode.tid, paramt)

        let v = Validator(fn:        userSectionValidator,
                          node:      ctx.parseTreeToIr(),
                          paramType: paramt)
      of "allow", "allowed":
        for i in 1 ..< n.children.len():
          let secName = r(n.children[i].getText())
          if secName in allows:
            ctx.irWarn("DupeAllow", @[secName.toNimStr()])
          elif secName in requires:
            ctx.irWarn("AllowAndReq", @[secName.toNimStr()])
          else:
            allows.add(secName)
      of "require", "required":
        for i in 1 ..< n.children.len():
          let secName = r(n.children[i].getText())
          if secName in requires:
            ctx.irWarn("DupeRequire", @[secName.toNimStr()])
          elif secName in allows:
            ctx.irWarn("AllowAndReq", @[secName.toNimStr()])
          requires.add(secName)
      else:
        unreachable

  if rootSection:
    if specLocal.rootSpec != nil:
      ctx.irWarn("DupeRootSpec")
    sectionL = specLocal.getRootSection()

    for (n, fs) in fields.items():
      sectionL.fields[n] = fs

  else:
    sectionL = SectionSpec(name: secName, fields: fields)
    specLocal.secSpecs[r(secName)] = sectionL

  # Fields we dealt w/ differently since we might add to the root section
  # a second time, and the error checking needed to be different.
  # For these things, we're ok.
  sectionL.maxAllowed = maxAllowed
  sectionL.userDefOk  = userDefOk
  if validator != nil:
    sectionL.validators.add(validator)
  let
    long  = sec.docNodes.extractLongDoc()
    short = sec.docNodes.extractShortDoc()

  sectionL.doc              = long
  sectionL.shortdoc         = short
  sectionL.hidden           = hidden
  sectionL.allowedSections  = allows
  sectionL.requiredSections = requires

proc convertConfSpec(ctx: Module) =
  if ctx.declaredSpec == nil:
    ctx.declaredSpec = newSpec()

  if ctx.attrSpec == nil:
    ctx.attrSpec = newSpec()

  ctx.attrSpec.used     = true
  ctx.declaredSpec.used = true
  var pt = ctx.pt
  ctx.pt = nil
  for item in pt.children:
    ctx.convertOneSectionSpec(item)

  ctx.pt = pt

proc findDeclarations(ctx: Module, n: ParseNode) =
  ## To make life easier for us when handling def's and uses, we will
  ## scan through either the module scope or individual function scopes
  ## in their entirety, looking just for 'var' and 'global' statements,
  ## so that when we get to def/use handling, we can know definitively
  ## what to do.
  ##
  ## If we're scanning in the module scope, we do NOT descend into
  ## FuncDef nodes; we'll call this function when creating our entry
  ## for a function.
  ##
  ## All executable bodies get properly processed only when all explicit
  ## declarations are found.
  ## This consists of:
  ##
  ## 1) Body content in the top level.
  ## 2) Body content in functions.
  ## 3) The default value in a parameter block.

  let saved = ctx.pt

  for kid in n.children:
    ctx.pt = kid
    case kid.kind
    of NodeFuncDef, NodeParamBlock:
      continue
    of NodeVarStmt:
      ctx.convertspec_typevarStmt()
    of NodeGlobalStmt:
      ctx.convertGlobalStmt()
    of NodeConstStmt:
      ctx.convertConstStmt()
    of NodeExternBlock:
      ctx.convertExternBlock()
    of NodeConfSpec:
      ctx.convertConfSpec()
    of NodeUseStmt:
      ctx.processUseStmt()
    else:
      ctx.findDeclarations(kid)

  ctx.pt = saved

proc findDeclarations*(ctx: Module) =
  ctx.pt = ctx.root

  # First, pull declarations from the toplevel scope.
  let root = ctx.pt

  ctx.findDeclarations(ctx.pt)

  for item in root.children:
    case item.kind
    of NodeEnumStmt:
      ctx.pt = item
      ctx.convertEnum() # Enums are currently 100% processed this pass
      continue
    of NodeFuncDef:
      # Function definitions are processed for declarations.
      ctx.pt = item
      ctx.convertFuncDefinition()
      continue
    of NodeParamBlock:
      ctx.pt = item
      ctx.convertParamBlock()
      continue
    else:
      ctx.findDeclarations(item)

proc statementsToIr(ctx: Module): IrNode =
  result = ctx.irNode(IrBlock)

  for i, item in ctx.pt.children:

    case item.kind
    of NodeBreakStmt, NodeContinueStmt, NodeReturnStmt:
      # For these, we're going to process them when we descend;
      # But we check for dead code here.
      if i != ctx.numKids() - 1:
        ctx.irWarn("DeadCode", @[$(item)], w = item)
        # Do process this one statement.
        result.contents.stmts.add(ctx.downNode(i))
        return # Don't process that dead code.
    of NodeLabelStmt:
      if i != ctx.numKids() - 1 and ctx.parseKid(i + 1).kind in ntLoops:
        ctx.labelNode = ctx.parseKid(i, 0)
        ctx.checkLabelDupe(ctx.labelNode.getText())
        continue
      else:
        ctx.irError("LabelLoc", w = item)
        continue
    of NodeElifStmt:
      if i != 0:
        let prev = ctx.parseKid(i - 1)
        if prev.kind in ntConditionals:
          result.contents.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("ElifLoc", w = item)
      continue
    of NodeElseStmt:
      if i != 0:
        let prev = ctx.parseKid(i = 1)
        if prev.kind in ntConditionals:
          result.contents.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("ElseLoc", w = item)
      continue
    # These items all get handled early so that we know what is
    # explicitly declared. Funcdefs and parameter blocks do need some
    # further processing, but that happens before the main body is
    # processed; this function gets called for those nodes seprately.
    of NodeEnumStmt, NodeFuncDef, NodeParamBlock, NodeVarStmt, NodeGlobalStmt,
         NodeConstStmt, NodeExternBlock, NodeConfSpec:
      continue
    else:
      discard  # An expression.

    result.contents.stmts.add(ctx.downNode(i))

proc convertSimpleLit(ctx: Module, st: SyntaxType): IrNode =
  var
    err:   LitError
    val:   pointer
    lmod = ctx.getLitMod()
    p    = cast[pointer](ctx.getText())

  result                 = ctx.irNode(IrLit)
  result.contents.litmod = lmod
  result.value           = con4m_simple_lit(p, cint(st),
                                            cstring(lmod), addr err)

  if result.value != nil:
    result.tid = get_my_type(cast[C4Obj](result.value))
  else:
    ctx.irError("BadLitTodo")

proc convertOtherLit(ctx: Module): IrNode =
  when false:
    let
      modifier = ctx.getLitMod()
      parent   = ctx.current
      symNode  = parent.contents.assignlhs.contents

    var
      sym:   SymbolInfo
      err:   string
      val:   pointer
      byVal: bool
      l:     int

    if modifier notin ["", "auto"]:
      result                 = ctx.convertSimpleLit(STStrQuotes)
      result.contents.litmod = modifier

    case symNode.kind
    of IrLhsLoad:
      sym = symNode.symbol
    of IrMember:
      sym = symNode.attrSym
    else:
      discard

    let
      text = ctx.getText()
      p    = cast[pointer](text)

    if sym != nil and sym.tid.isConcrete():
      val = instantiate_literal(sym.tid, p, StStrQuotes, "", byVal, err)

      result.contents.byVal = byVal

      if err == "":
        result                = ctx.irNode(IrLit)
        result.tid            = sym.tid
        result.value          = val
        result.haveVal        = true
        result.contents.sz    = l
        return
      else:
        ctx.irError(err)
        return

    if text.len() >= 2:
      if (text[0] == text[^1] and text[0] in ['"', '\'']):
        ctx.irWarn("OtherQuotes")
      elif (text[0] == '[' and text[^1] == ']') or
           (text[0] == '{' and text[^1] == '}') or
           (text[0] == '(' and text[^1] == ')'):
        ctx.irWarn("OtherBrak")

    for (_, dt) in syntaxInfo[int(STOther)].litmods:
      err = ""

      val = instantiate_literal(dt.dtid, p, STOther, "", byVal, err)
      if err == "":
        result                = ctx.irNode(IrLit)
        result.tid            = dt.dtid
        result.value          = val
        result.haveVal        = true
        result.contents.byVal = byVal
        result.contents.sz    = l
        ctx.irInfo("OtherLit", @[dt.name])
        return

  ctx.irError("InvalidOther")

proc convertCharLit(ctx: Module): IrNode =
  var
    err:   LitError
    byVal: bool
    l:     int
    lmod = ctx.getLitMod()
    cp   = cast[pointer](ctx.pt.token.codepoint)

  result                 = ctx.irNode(IrLit)
  result.contents.litmod = lmod
  result.value           = con4m_simple_lit(cp, cint(StChrQuotes),
                                            cstring(lmod), addr err)

  if result.value != nil:
    result.tid = get_my_type(cast[C4Obj](result.value))
  else:
    ctx.irError("BadLitTodo")

proc convertNilLit(ctx: Module): IrNode =
  result = ctx.irNode(IrNil)

  result.tid = tspec_error() #tMaybe(tspec_typevar())
  # We could explicitly add a value, but this node's mere presence implies
  # the value.

proc convertTypeLit(ctx: Module): IrNode =
  # TODO, this should move into a base type.
  var
    tvars: Dict[string, TypeSpec]
    tinfo: TypeSpec

  tvars.initDict()
  result         = ctx.irNode(IrLit)
  result.tid     = tspec_typespec()
  tinfo          = ctx.pt.buildType(tvars)
  result.value   = cast[pointer](tinfo)

proc convertListLit(ctx: Module): IrNode =
  var
    lmod = ctx.getLitMod()
    err:       bool
    errIx:     int
    itemType:  TypeSpec
    itemTypes: seq[TypeSpec]

  result = ctx.irNode(IrLit)

  for i in 0 ..< ctx.numKids():
    result.contents.items.add(ctx.downNode(i))

  if ctx.numKids() == 0:
    itemType = tspec_typevar()
  else:
    itemType = ctx.unifyOrCast(result.contents.items, errIx)

  if itemType == tspec_error():
    ctx.irError("TyDiffListItem",
                result.contents.items[errIx],
                @[result.contents.items[0].getTid().toString(),
                  result.contents.items[errIx].getTid().toString()])
    return


  # TODO: literal modifiers.
  if lmod != "":
    ctx.irError("BadLitMod", @[lmod, "list"])

  result.tid             = tspec_list(itemType)
  result.contents.litmod = lmod


proc convertDictLit(ctx: Module): IrNode =
  var
    err:      bool
    errIx:    int
    keyType:  TypeSpec
    itemType: TypeSpec
    kNodes:   seq[IrNode]
    iNodes:   seq[IrNode]
    lmod  = ctx.getLitMod()

  result                 = ctx.irNode(IrLit)
  result.contents.litmod = lmod

  for i in 0 ..< ctx.numKids():
    kNodes.add(ctx.downNode(i, 0))
    iNodes.add(ctx.downNode(i, 1))

  if ctx.numKids() != 0:
    keyType = ctx.unifyOrCast(kNodes, errIx)
  else:
    keyType = tspec_typevar()


  if keyType == tspec_error():
    ctx.irError("TyDiffKey", kNodes[errIx],
                @[kNodes[0].getTid().toString(),
                  kNodes[errIx].getTid().toString()])
    return

  if ctx.numKids() != 0:
    itemType = ctx.unifyOrCast(iNodes, errIx)
  else:
    itemType = tspec_typevar()

  if itemType == tspec_error():
    ctx.irError("TyDiffVal", iNodes[errIx],
                @[iNodes[0].getTid().toString(),
                  iNodes[errIx].getTid().toString()])
    return

  for i in 0 ..< kNodes.len():
    result.contents.items.add(kNodes[i])
    result.contents.items.add(iNodes[i])

  result.tid = tspec_dict(keyType, itemType)

  if lmod != "":
    ctx.irError("BadLitMod", @[lmod, "dict"])

proc convertTupleLit(ctx: Module): IrNode =
  var
    err:   bool
    types: seq[TypeSpec]
    lmod      = ctx.getLitMod()
    gotBottom = false

  result                 = ctx.irNode(IrLit)
  result.contents.litmod = lmod

  for i in 0 ..< ctx.numKids():
    let oneItem = ctx.downNode(i)
    types.add(oneItem.getTid())
    if oneItem.getTid() == tspec_error():
      gotBottom = true
    result.contents.items.add(oneItem)

  if not gotBottom:
    result.tid = tspec_tuple(toXList(types))

  if lmod != "":
    ctx.irError("BadLitMod", @[lmod, "tuple"])

proc newCallbackFromFuncInfo(s: FuncInfo): ptr Callback =
  result = Callback.create()
  result.name = s.name
  result.tid  = s.tid
  result.impl = s

  GC_ref(s)

proc convertCallbackLit(ctx: Module): IrNode =
  result = ctx.irNode(IrLit)

  if ctx.numKids() == 2:
    var tvars = newDict[string, TypeSpec]()

    result.tid = ctx.pt.children[1].buildType(tvars)
  else:
    ctx.irError("FuncSigMustBeProvidedNow")

  ctx.funcsToResolve.add((result, result.tid, ctx.getText(0)))


proc convertForStmt(ctx: Module): IrNode =
  result                    = ctx.irNode(IrLoop)
  result.contents.label     = ctx.labelNode
  ctx.labelNode             = nil
  var
    scope = initScope()
    v1, v2, c3: SymbolInfo
    rangeLoop:  bool

  if ctx.pt.children[1].kind == NodeRange:
    rangeLoop = true

  result.scope = scope

  if ctx.blockScopes.len() != 0:
    scope.addParent(ctx.blockScopes[^1])
  elif ctx.funcScope != nil:
    scope.addParent(ctx.funcScope)
  else:
    scope.addParent(ctx.moduleScope)

  ctx.blockScopes.add(scope)

  # For loops over containers get a phantom index variable named $i,
  # since the named variable gets the contents of the container. The
  # programmer can use this variable. They also get a phantom variable
  # called `$container`, which holds the container object after it's
  # evaluated during loop initialization.
  #
  # The index variable is always loopVars[0] (even for while loops, where
  # we also auto-add $i).
  #
  # Both types of for loops get a $len value that indicates the max
  # number of iterations. The programmer can also use this, as $len.
  #
  # Eventually we'll add aliases for the index and len variables for
  # labeled loops, but that isn't done yet.
  #
  # $max will always be in loopVars[^1], and does not exist for
  # `while` loops.
  #
  # For container loops, the $container variable is second-from-last.

  if rangeLoop:
    let
      kid = ctx.pt.children[0].children[0]
      sym = ctx.scopeDeclare(scope, kid.getText(), false,
                             tspec_i64(), true).get()
    result.contents.loopVars.add(sym)
  else:
    let ixVar = ctx.scopeDeclare(scope, "$i", false,
                                 tspec_i64(), true).get()

    result.contents.loopVars.add(ixVar)

    for item in ctx.pt.children[0].children:
      let sym = ctx.scopeDeclare(scope, item.getText(), false,
                                 tspec_typevar(), true)
      result.contents.loopVars.add(sym.get())
    let cVar = ctx.scopeDeclare(scope, "$container", false,
                                tspec_typevar(), true)
    result.contents.loopVars.add(cvar.get())

  let maxSym = ctx.scopeDeclare(scope, "$len", false,
                                tspec_i64(), true)
  result.contents.loopVars.add(maxSym.get())

  result.contents.condition = ctx.downNode(1)
  result.contents.loopBody  = ctx.downNode(2)

  if not rangeLoop:
    let lvTy = result.contents.loopVars[^2].tid
    discard lvTy.unify(result.contents.condition.tid)

    if result.contents.loopVars.len() == 5: # It's a dict container.
      ctx.typeCheck(result.contents.condition.tid,
                    tspec_dict(result.contents.loopVars[1].tid,
                          result.contents.loopVars[2].tid))
    else:
      ctx.typeCheck(result.contents.condition.tid,
                    tspec_list(result.contents.loopVars[1].tid))

  discard ctx.blockScopes.pop()

proc convertWhileStmt(ctx: Module): IrNode =
  result                    = ctx.irNode(IrLoop)
  result.contents.label     = ctx.labelNode
  ctx.labelNode             = nil

  var scope = initScope()

  result.scope = scope

  if ctx.blockScopes.len() != 0:
    scope.addParent(ctx.blockScopes[^1])
  elif ctx.funcScope != nil:
    scope.addParent(ctx.funcScope)
  else:
    scope.addParent(ctx.moduleScope)

  ctx.blockScopes.add(scope)

  let loopVar = ctx.scopeDeclare(scope, "$i", false,
                                 tspec_i64(), true).get()

  result.contents.loopVars.add(loopVar)

  result.contents.condition = ctx.downNode(0)

  if ctx.unifyOrCast(tspec_bool(), result.contents.condition) == tspec_error():
    ctx.irError("NoBoolCast",
                @[result.contents.condition.getTid().toString()],
                w = ctx.parseKid(0))

  result.contents.loopBody  = ctx.downNode(1)
  result.contents.whileLoop = true

  discard ctx.blockScopes.pop()

proc makeVariant(parent: SymbolInfo): SymbolInfo =
  # We really don't need to copy everything we copy here.
  result              = SymbolInfo()
  result.name         = parent.name
  result.isAttr       = parent.isAttr
  result.inFunc       = parent.inFunc
  result.defaultVal   = parent.defaultVal
  result.haveDefault  = parent.haveDefault
  result.declaredType = parent.declaredType
  result.tid          = tspec_typevar()
  result.constValue   = parent.constValue
  result.haveConst    = parent.haveConst
  result.module       = parent.module
  result.declNode     = parent.declNode
  result.actualSym    = parent
  result.variantId    = parent.variantId
  parent.variantId   += 1

proc convertTypeOfStmt(ctx: Module): IrNode =
  ## Todo... detect overlap between cases, and check for coverage.
  let target                   = ctx.downNode(0)
  result                       = ctx.irNode(IrSwitch)
  result.contents.typeCase     = true
  result.contents.switchTarget = target
  var sym: SymbolInfo

  case target.contents.kind
  of IrMember:
    sym = target.contents.attrSym
  of IrLoad, IrLhsload:
    sym = target.contents.symbol
  else:
    unreachable

  result.contents.targetSym = sym

  # All remaining kids will be Case nodes; at least one will have two
  # prongs; the last could be an `else`, which has only one prong.
  for i in 1 ..< ctx.numKids():
    let n = ctx.irNode(IrSwitchBranch)
    var
      scope    = initScope()
      symCopy  = sym.makeVariant()
      actionBranch: int

    if ctx.blockScopes.len() != 0:
      scope.addParent(ctx.blockScopes[^1])
    elif ctx.funcScope != nil:
      scope.addParent(ctx.funcScope)
    else:
      scope.addParent(ctx.moduleScope)

    if ctx.numKids(i) == 2:
      var branchType: TypeSpec
      actionBranch = 1

      # If multiple type options share the same branch, we use a OneOf
      if ctx.numKids(i, 0) > 1:
        var opts: seq[TypeSpec]
        for j in 0 ..< ctx.numKids(i, 0):
          opts.add(ctx.parseKid(i, 0, j).buildType())
        try:
          #branchType = tOneOf(opts)
          discard
        except:
          ctx.irError("TCaseOverlap", w = ctx.parseKid(i, 0, 0))
        ctx.irError("Not supported yet")
      else:
        branchType = ctx.parseKid(i, 0, 0).buildType()

      symCopy.tid = branchType
    else:
      symCopy.tid  = tspec_typevar()
      actionBranch = 0

    n.contents.branchSym  = symCopy
    scope.table[sym.name] = symCopy
    scope.numSyms        += 1
    ctx.blockScopes       = @[scope] & ctx.blockScopes
    n.tid                 = symCopy.tid

    if tspec_compare(symCopy.tid, sym.tid) == false:
      ctx.irWarn("DeadTypeCase", @[symCopy.tid.toString()])

    n.contents.action = ctx.downNode(i, actionBranch)
    result.contents.branches.add(n)

    discard ctx.blockScopes.pop()

proc convertValueOfStmt(ctx: Module): IrNode =
  # TODO, check overlap, completeness, etc.
  result                       = ctx.irNode(IrSwitch)
  result.contents.switchTarget = ctx.downNode(0)


  for i in 1 ..< ctx.numKids():
    # All except a potential else block will have left side be
    # conditions, right side be a block.
    let n = ctx.irNode(IrSwitchBranch)
    if ctx.numKids(i) == 2:
      for j in 0 ..< ctx.numKids(i, 0):
        let branch = ctx.downNode(i, 0, j)
        n.contents.conditions.add(branch)
        case branch.contents.kind
        of IrRange:
          let targetType = tspec_list(result.contents.switchTarget.tid)
          ctx.typeCheck(branch.tid, targetType)
        else:
          ctx.typeCheck(branch.tid, result.contents.switchTarget.tid)
      n.contents.action = ctx.downNode(i, 1)
    else:
      n.contents.action = ctx.downNode(i, 0)
    result.contents.branches.add(n)

proc convertRange(ctx: Module): IrNode =
  result                     = ctx.irNode(IrRange)
  result.contents.rangeStart = ctx.downNode(0)
  result.contents.rangeEnd   = ctx.downNode(1)

  if not result.contents.rangeStart.tid.isIntType():
    ctx.irError("BadCxRange", w = result.contents.rangeStart)
  elif not result.contents.rangeEnd.tid.isIntType():
    ctx.irError("BadCxRange", w = result.contents.rangeEnd)
  else:
     result.tid = ctx.unifyOrCast(result.contents.rangeStart,
                                  result.contents.rangeEnd)

proc loopExit(ctx: Module, loopExit: bool): IrNode =
  result                   = ctx.irNode(IrJump)
  result.contents.exitLoop = loopExit
  if ctx.numKids() != 0:
    let label = ctx.getText(0)
    var n = result.parent
    while n != nil:
      if n.contents.kind == IrLoop:

        if n.contents.label != nil and n.contents.label.getText() == label:
          result.contents.targetNode = n
          return
      n = n.parent
    ctx.irError("BadLoopExit", @[ctx.getText(), label])
  else:
    var n = result.parent
    while n != nil:
      if n.contents.kind == IrLoop:
        result.contents.targetNode = n
        return
      n = n.parent

proc convertConditional(ctx: Module): IrNode =
  result                      = ctx.irNode(IrConditional)
  result.contents.predicate   = ctx.downNode(0)
  result.contents.trueBranch  = ctx.downNode(1)

  if ctx.numKids() == 3:
    result.contents.falseBranch = ctx.downNode(2)

  if ctx.unifyOrCast(tspec_bool(), result.contents.predicate) == tspec_error():
    ctx.irError("NoBoolCast",
                @[result.contents.predicate.getTid().toString()],
                w = ctx.parseKid(0))

proc convertLock(ctx: Module): IrNode =
  result      = ctx.downNode(0)
  result.lock = true
  if result.contents.kind != IrAssign:
    result.tid = tspec_void()

proc convertReturn(ctx: Module): IrNode =
  result = ctx.irNode(IrRet)
  if ctx.numKids() == 1:
    let rv = ctx.downNode(0)
    result.contents.retVal = rv
    discard ctx.addVarDef("result", result, rv.getTid())

proc convertUnaryPlus(ctx: Module): IrNode =
  result = ctx.downNode(0)

proc convertUnaryMinus(ctx: Module): IrNode =
  result               = ctx.irNode(IrUMinus)
  result.contents.uRhs = ctx.downNode(0)
  result.tid           = result.contents.uRhs.getTid()

proc convertNotOp(ctx: Module): IrNode =
  result               = ctx.irNode(IrNot)
  result.contents.uRhs = ctx.downNode(0)
  result.tid           = tspec_bool()


proc convertBooleanOp(ctx: Module): IrNode =
  # Comparison operators.

  result                = ctx.irNode(IrBool)
  result.contents.bOp   = ctx.getText()
  var
    bLhs                = ctx.downNode(0)
    bRhs                = ctx.downNode(1)

  var operandType = ctx.unifyOrCast(bLhs, bRhs)

  if operandType == tspec_error():
    ctx.irError("BinaryOpCompat",
                @[bLhs.getTid().toString(), bRhs.getTid().toString()])

  result.tid            = tspec_bool()
  result.contents.bLhs  = bLhs
  result.contents.bRhs  = bRhs

  case ctx.pt.kind
  of NodeNe:
    result.contents.opId = OpNeq
  of NodeCmp:
    result.contents.opId = FEq
  of NodeGte:
    result.contents.opId = OpGte
  of NodeLte:
    result.contents.opId = OpLte
  of NodeGt:
    result.contents.opId = FGt
  of NodeLt:
    result.contents.opId = FLt
  else:
    unreachable

const notFloatOps = ["<<", ">>", "and", "or", "^", "&", "|", "div", "%"]

proc convertBinaryOp(ctx: Module): IrNode =
  result                = ctx.irNode(IrBinary)
  result.contents.bOp   = ctx.getText()

  # If we have +=, we already rewrote the parse tree; this node
  # should be a plus, the assignment will be above us.
  if result.contents.bOp[^1] == '=':
    result.contents.bOp = result.contents.bOp[0 ..< ^1]

  var
    bLhs = ctx.downNode(0)
    bRhs = ctx.downNode(1)

  result.tid = ctx.unifyOrCast(bLhs, bRhs)

  if result.tid == tspec_error() and bLhs.getTid().isNumericBuiltin() and
     bRhs.getTid().isNumericBuiltin():
    result.tid = ctx.resultingNumType(bLhs.getTid(), bRhs.getTid())

    if result.getTid() == tspec_f64() and result.contents.bOp in notFloatOps:
        result.tid = tspec_error()

  if result.getTid() == tspec_error() and bLhs.getTid() != tspec_error() and
    bRhs.getTid() != tspec_error():
    ctx.irError("BinaryOpCompat", @[bLhs.getTid().toString(),
                                    bRhs.getTid().toString()])
  elif result.contents.bOp == "/" and (bLhs.getTid() == tspec_u64() or
                                       bRhs.getTid() == tspec_u64()):
      ctx.irError("U64Div")
      result.tid = tspec_error()

  #if bLhs.getTid().intBits() == 128 or bRhs.getTid().intBits() == 128:
  #  ctx.irError("128BitLimit", @["Float division"])
  #  result.tid = tspec_error()

  result.contents.bLhs  = bLhs
  result.contents.bRhs  = bRhs

  case ctx.pt.kind
  of NodePlus:
    result.contents.opId = FAdd
  of NodeMinus:
    result.contents.opId = FSub
  of NodeMod:
    result.contents.opId = FMod
  of NodeMul:
    result.contents.opId = FMul
  of NodeDiv:
    result.contents.opId = FFDiv
    # TODO: Add integer division as well.
  of NodeBitOr:
    result.contents.opId = FBor
  of NodeBitXor:
    result.contents.opId = FBxor
  of NodeBitAnd:
    result.contents.opId = FBand
  of NodeShl:
    result.contents.opId = FShl
  of NodeShr:
    result.contents.opId = FShr
  else:
    unreachable

proc convertLogicOp(ctx: Module): IrNode =
  result                = ctx.irNode(IrLogic)
  result.contents.bOp   = ctx.getText()
  var
    bLhs                = ctx.downNode(0)
    bRhs                = ctx.downNode(1)

  if ctx.pt.kind == NodeOr:
    result.contents.opId = OpLogicOr
  else:
    result.contents.opId = OpLogicAnd

  if ctx.unifyOrCast(tspec_bool(), bLhs) == tspec_error():
    ctx.irError("NoBoolCast", @[bLhs.getTid().toString()],
                w = ctx.parseKid(0))
    result.tid = tspec_error()
    return

  if ctx.unifyOrCast(tspec_bool(), bRhs) == tspec_error():
    ctx.irError("NoBoolCast", @[bRhs.getTid().toString()],
                w = ctx.parseKid(1))
    result.tid = tspec_error()
    return
  else:
      result.tid = tspec_bool()

  result.contents.bLhs  = bLhs
  result.contents.bRhs  = bRhs

proc convertSection(ctx: Module): IrNode =
  # Section declarations not specs.
  var
    haveSpec     = ctx.attrSpec != nil and ctx.attrSpec.used
    savedSecSpec = ctx.curSecSpec
  result                   = ctx.irNode(IrSection)
  result.contents.prefix   = ctx.curSecPrefix
  result.contents.sectName = ctx.getText(0)

  var savedInSection = ctx.secDefContext
  ctx.secDefContext  = true

  if haveSpec:
    let specOpt = ctx.attrSpec.secSpecs.lookup(r(result.contents.sectName))
    if specOpt.isNone():
      ctx.irError("BadSectionType", @[result.contents.sectName])
    else:
     if savedSecSpec != nil and
        r(result.contents.sectName) notin savedSecSpec.allowedSections:
        ctx.irError("SecNotAllowed", @[result.contents.sectName])

     ctx.curSecSpec = specOpt.get()

     if ctx.curSecSpec.maxAllowed == 1:
       if ctx.pt.children.len() == 3:
         ctx.irError("NotASingleton", @[result.contents.sectName])
     elif ctx.pt.children.len() == 2:
       ctx.irError("IsASingleton", @[result.contents.sectName])

  if ctx.curSecPrefix != "":
    ctx.curSecPrefix &= "." & result.contents.sectName
  else:
    ctx.curSecPrefix = result.contents.sectName

  if ctx.pt.children.len() == 3:
    result.contents.instance = ctx.getText(1)
    ctx.curSecPrefix &= "." & result.contents.instance
    result.contents.blk = ctx.downNode(2)
  else:
    result.contents.blk = ctx.downNode(1)

  ctx.curSecPrefix  = result.contents.prefix
  ctx.curSecSpec    = savedSecSpec
  ctx.secDefContext = savedInSection

  if ctx.pt.children[1].docnodes != nil:
    result.shortdoc = ctx.pt.children[1].extractShortDoc()
    result.longdoc  = ctx.pt.children[1].extractLongDoc()
    ctx.secDocNodes.add(result)
  elif ctx.pt.children.len() > 2 and ctx.pt.children[2].docnodes != nil:
    result.shortdoc = ctx.pt.children[2].extractShortDoc()
    result.longdoc  = ctx.pt.children[2].extractLongDoc()
    ctx.secDocNodes.add(result)

proc convertIndex(ctx: Module): IrNode =
  # The logic for when to generate address loads instead of value
  # loads (the LHS flag) will need to be redone once we add object
  # types with fields.

  var
    onLhs = ctx.lhsContext
    brak  = ctx.pt
    toIx, ixStart, ixEnd: IrNode

  if onLhs and ctx.pt.children[0].kind == NodeIndex:
      ctx.lhsContext = false

  toIx = ctx.downNode(0)

  ctx.lhsContext = false
  ixStart = ctx.downNode(1)

  if onLhs:
    result = ctx.irNode(IrIndexLhs)
  else:
    result = ctx.irNode(IrIndex)

  if toIx.contents.kind in [IrMember, IrMemberLhs]:
    # This is tracked in case we can fold.
    result.contents.toIxSym = toIx.contents.attrSym
  elif toIx.contents.kind in [IrLoad, IrLhsLoad]:
    result.contents.toIxSym = toIx.contents.symbol

  result.tid = tspec_typevar()

  if ctx.numKids() == 2:
    let tobj = toIx.getTid().followForwards()
    case tobj.get_type_kind()
    of BT_LIST:
      if ctx.unifyOrCast(tspec_i64(), ixStart) == tspec_error():
        if not ixStart.getTid().isIntType():
          ctx.irError("ListIx")
      ctx.typeCheck(toIx.getTid(), tspec_list(result.tid))
    of BT_DICT:
      let expected = tspec_dict(ixStart.getTid(), result.tid)
      ctx.typeCheck(toIx.getTid(), expected)

    of BT_TUPLE:
      ctx.foldDown(ixStart)
      if not ixStart.isConstant():
        ctx.irError("TupleConstIx", w = ixStart)
        return
      let v = cast[int64](ixStart.value)
      if v < 0 or v >= tobj.num_params():
        ctx.irError("TupleIxBound", w = ixStart)
        return
      let to = toIx.getTid().followForwards().get_param(cint(v))
      ctx.typeCheck(result.tid, to)
    of BT_TVAR:
      # Currently, we do not try to infer the container type;
      # we instead just error that the type needs to be known
      # to index it.
      ctx.irError("ContainerType", w = toIx)
    else:
      if tobj == tspec_error():
        return
      ctx.irError("NotIndexible", @[tobj.toString()])
  else:  # SLICE operator. *must* be a list.
    ixEnd = ctx.downNode(2)
    result.tid = ctx.typeCheck(tspec_list(tspec_typevar()), toIx.getTid())

  result.contents.toIx       = toIx
  result.contents.indexStart = ixStart
  result.contents.indexEnd   = ixEnd

  ctx.lhsContext = onLhs  # Totally wasteful, this.

proc convertCall(ctx: Module): IrNode =
  result                = ctx.irNode(IrCall)
  result.contents.fname = ctx.getText(0)
  result.parseNode      = ctx.parseKid(0)
  result.tid            = tspec_typevar()

  var
    fArgs: seq[TypeSpec]

  for i in 0 ..< ctx.numKids(1):
    let oneActual = ctx.downNode(1, i)
    result.contents.actuals.add(oneActual)
    fArgs.add(oneActual.tid)

  #fArgs.add(result.tid)

  ctx.funcsToResolve.add((result,
                          tspec_fn(result.tid, toXList(fArgs), false),
                                   result.contents.fname))

proc convertAssert(ctx: Module): IrNode =
  result                    = ctx.irNode(IrAssert)
  result.contents.assertion = ctx.downNode(0)

proc fmtImplementationList(fname: string, fns: seq[FuncInfo], t: TypeSpec,
                           extra: seq[string] = @[]): seq[seq[Rich]] =
  var
    row:   seq[Rich]
    cur:   string

  result = @[@[text(" "), text("Signature"), text("Location")]]

  for num, item in fns:
    cur = fname & "("
    for i, param in item.params:
      cur &= item.paramNames[i]
      cur &= ": "
      if i + 1 == item.params.len() and param.va:
        cur &= "*"
      cur &= param.tid.toString()
      if i + 1 < item.params.len():
        cur &= ", "
    cur &= ") -> "
    if item.retval != nil:
      cur &= item.retval.tid.toString()
    else:
      cur &= tspec_typevar().toString()

    row.add(text($(num + 1)))
    row.add(bold(cur))

    if item.defModule != nil:
      cur = item.defModule.modname & ":"
    if item.rawImpl != nil:
      cur &= $(item.rawImpl.token.lineNo) & ":"
      cur &= $(item.rawImpl.token.lineOffset) & "("
      cur &= item.defModule.where & ") "
    if extra.len() != 0:
      cur &= extra[num]

    row.add(text(cur))
    result.add(row)
    row = @[]

proc showCallMistakes(fname: string, fns: seq[FuncInfo], t: TypeSpec): Grid =
  fmtImplementationList(fname, fns, t).table()

proc resolveDeferredSymbols*(ctx: CompileCtx, m: Module) =
  for (n, t, name) in m.funcsToResolve:
    let ty         = t.followForwards()
    let l          = ty.num_params()
    let retvalType = ty.get_param(l)

    var
      possibles:    seq[FuncInfo]
      matches:      seq[FuncInfo]
      fails:        seq[FuncInfo]
      symbolExists: bool
      sym:          SymbolInfo
      symOpt = m.moduleScope.table.lookup(name)

    if symOpt.isSome():
      symbolExists = true
      sym = symOpt.get()
      if sym.isFunc:
        possibles = sym.fimpls
      elif sym.tid.tspec_compare(t) == false:
        n.contents.cbSymbol = sym
        continue

    symOpt = ctx.globalScope.table.lookup(name)

    if symOpt.isSome():
      symbolExists = true
      sym = symOpt.get()

      if sym.isFunc:
        for item in sym.fimpls:
          if item notin possibles:
            possibles.add(item)
      elif sym.tid.tspec_compare(t) == false:
        n.contents.cbSymbol = sym
        continue

    if possibles.len() == 0:
      if symbolExists:
        m.irError("NotAFunc", n, @[name, t.toString()])
        continue
      else:
        m.irError("NoImpl", n, @[name, t.toString()])
        continue

    for item in possibles:
      withFnLock(item):
        if item.tid.tspec_compare(t) == false:
          fails.add(item)
        else:
          matches.add(item)

    if matches.len() == 1:
      withFnLock(matches[0]):
        discard matches[0].tid.unify(t)
      if n.contents.kind == IrLit:
        n.value   = cast[pointer](newCallbackFromFuncInfo(matches[0]))
      else:
        n.contents.toCall = matches[0]
      continue
    elif matches.len() == 0:
      if n.contents.kind == IrLit:
        m.irError("NoCbMatch", w = n, @[name, t.toString])
      else:
        let info = showCallMistakes(n.contents.fname, fails, t)
        m.irError("BadSig", n, @[n.contents.fname, t.toString(), "call"],
                  detail = info.grid_to_str(terminalWidth()))
    else:
      if n.contents.kind == IrLit:
        let info = fmtImplementationList(name, matches, t).table()
        m.irError("CallAmbig", w = n, @[name, t.toString(), "callback"],
                                 detail = info.grid_to_str(terminalWidth()))
      else:
        let info = fmtImplementationList(n.contents.fname,
                                         matches, t).table()
        m.irError("CallAmbig", n, @[n.contents.fname, t.toString(), "call"],
                  detail = info.grid_to_str(terminalWidth()))

  m.funcsToResolve = @[]

  # Also take a pass looking for overlapping signatures.
  for (name, sym) in m.moduleScope.table.items():
    if sym.isFunc and sym.fimpls.len() > 1:
      for i in 0 ..< (sym.fimpls.len() - 1):
        let oneFuncType = sym.fimpls[i].tid
        for j in i + 1 .. (sym.fimpls.len() - 1):
          if oneFuncType.unify(sym.fimpls[j].tid) != tspec_error() and
             sym.fimpls[i].implementation != nil:
            let
              t1 = sym.fimpls[i].tid.toString()
              t2 = sym.fimpls[j].tid.toString()
              l1 = $(sym.fimpls[i].implementation.parseNode.token.lineNo)
            m.irError("SigOverlap", w = sym.fimpls[j].implementation,
                      @[name, t1, t2, l1])

proc parseTreeToIr(ctx: Module): IrNode =
  if ctx.pt == nil:
    return nil

  case ctx.pt.kind:
    of NodeModule, NodeBody:
      result = ctx.statementsToIr()
      result.shortdoc = ctx.pt.extractShortDoc()
      result.longdoc  = ctx.pt.extractLongDoc()
    of NodeIdentifier:
      result = ctx.convertIdentifier()
    of NodeExpression, NodeLiteral, NodeElseStmt, NodeParenExpr:
      result = ctx.downNode(0)
    of NodeStringLit:
      result = ctx.convertSimpleLit(StStrQuotes)
    of NodeIntLit:
      result = ctx.convertSimpleLit(STBase10)
    of NodeHexLit:
      result = ctx.convertSimpleLit(StHex)
    of NodeFloatLit:
      result = ctx.convertSimpleLit(STFloat)
    of NodeBoolLit:
      result = ctx.convertSimpleLit(StBoolLit)
    of NodeOtherLit:
      result = ctx.convertOtherLit()
    of NodeNilLit:
      result = ctx.convertNilLit()
    of NodeCharLit:
      result = ctx.convertCharLit()
    of NodeType:
      result = ctx.convertTypeLit()
    of NodeRange:
      result = ctx.convertRange()
    of NodeDictLit:
      result = ctx.convertDictLit()
    of NodeListLit:
      result = ctx.convertListLit()
    of NodeTupleLit:
      result = ctx.convertTupleLit()
    of NodeCallbackLit:
      result = ctx.convertCallbackLit()
    of NodeUseStmt:
      result = ctx.convertUseStmt()
    of NodeForStmt:
      result = ctx.convertForStmt()
    of NodeWhileStmt:
      result = ctx.convertWhileStmt()
    of NodeTypeOfStmt:
      result = ctx.convertTypeOfStmt()
    of NodeValueOfStmt:
      result = ctx.convertValueOfStmt()
    of NodeBreakStmt:
      result = ctx.loopExit(true)
    of NodeContinueStmt:
      result = ctx.loopExit(false)
    of NodeIfStmt, NodeElifStmt:
      result = ctx.convertConditional()
    of NodeAttrSetLock:
      result = ctx.convertLock()
    of NodeReturnStmt:
      result = ctx.convertReturn()
    of NodeAssign:
      result = ctx.convertAssignment()
    of NodeSection:
      result = ctx.convertSection()
    of NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt:
      result = ctx.convertBooleanOp()
    of NodeOr, NodeAnd:
      result = ctx.convertLogicOp()
    of NodeMod, NodeMul, NodeDiv, NodeBitOr, NodeBitXor, NodeBitAnd,
       NodeShl, NodeShr:
      result = ctx.convertBinaryOp()
    of NodePlus:
      if ctx.numKids == 2:
        result = ctx.convertBinaryOp()
      else:
        result = ctx.convertUnaryPlus()
    of NodeMinus:
      if ctx.numKids == 2:
        result = ctx.convertBinaryOp()
      else:
        result = ctx.convertUnaryMinus()
    of NodeNot:
      result = ctx.convertNotOp()
    of NodeMember:
      result = ctx.convertMember()
    of NodeIndex:
      result = ctx.convertIndex()
    of NodeCall:
      result = ctx.convertCall()
    of NodeAssert:
      result = ctx.convertAssert()
    else:
      unreachable

proc finishFunctionProcessing(ctx: Module, impl: FuncInfo) =
  # If a result was never set inside the function,
  # then we need to change the type to void.
  #
  # If the return variable was never used, this should not pose a
  # problem. However, if there are any uses of the return value, the
  # error will get picked up when we do our def/use analysis after
  # the folding pass, so we ignore any type error for now.
  let resSym = impl.fnScope.table["result"]
  if resSym.defs.len() == 0:
    discard resSym.tid.unify(tspec_void())

  # Next, we go ahead and 're-lock' the function, to be safe. This
  # ensures we, from this point forward, won't accidentally modify the
  # type.
  impl.lockFn()
  ctx.funcScope  = nil
  ctx.definingFn = nil

proc toIr*(ctx: Module): bool {.discardable.} =
  ctx.pt        = ctx.root
  ctx.funcScope = nil
  ctx.ir        = ctx.parseTreeToIr()

  for (name, sym) in ctx.moduleScope.table.items():
    for impl in sym.fimpls:
      if impl.rawImpl != nil:
        impl.unlockFn()
        ctx.definingFn      = impl
        ctx.funcScope       = impl.fnScope
        ctx.pt              = impl.rawImpl
        let nodes           = ctx.parseTreeToIr() # This should be a body node.
        impl.implementation = nodes
        ctx.finishFunctionProcessing(impl)

    if sym.pInfo != nil and sym.pinfo.defaultParse.isSome():
      ctx.funcScope = nil
      ctx.pt        = sym.pinfo.defaultParse.get()
      ctx.current   = nil
      let paramIr   = ctx.parseTreeToIr()

      sym.pinfo.defaultIr = some(paramIr)
      ctx.typeCheck(sym, paramIr.getTid())

  result = ctx.errors.canProceed()
