## The IR is essentially the graph representation on which we do all checking.
## Ideally, we will keep refining the nodes until we can use them essentially
## as fat VM instructions that we can directly marshal.
#
# TODO:
# Expressions.
#
# Type propogation.
#
# After pass 1, check to see if declared variables are used, and
# warn if not.


import parse, basetypes, types, options, strutil

type
  IrNodeType* = enum
    IrBlock, IrLoop, IrAttrAssign, IrVarAssign, IrSectionScope, IrConditional,
    IrJump, IrRet, IrLit, IrMember, IrIndex, IrCall, IrUse, IrUnary, IrBinary,
    IrEnum, IrEnumItem, IrCast

  IrNode* = object
    parseNode*: Con4mNode
    tid*:       TypeId
    value*:     Option[Mixed]
    parent*:    IrNode
    contents*:  IrContents

  IrContents* = ref object
    case kind*: IrNodeType
    of IrBlock:
      stmts*: seq[IrNode]
    of IrLoop:
      label*:      Con4mNode # Any type of loop.
      keyVar*:     string    # Both types of for loops.
      valVar*:     string    # for k, v in dict
      startIx*:    IrNode    # for x from 0 to 10
      endIx*:      IrNode    # for x from 0 to 10
      condition*:  IrNode    # While loop condition or object to iterate over.
      loopBody*:   IrNode
      scope*:      Scope
    of IrAttrAssign:
      attrlhs*: string
      attrrhs*: IrNode
      lock*:    bool
    of IrVarAssign:
      varlhs*: seq[IrNode]
      varrhs*: IrNode
    of IrSectionScope:
      secType*: string
      secName*: string
      secBody*: IrNode
    of IrConditional:
      condition*:   IrNode
      trueBranch*:  IrNode
      falseBranch*: IrNode
    of IrJump:
      exitLoop*:   bool
      targetNode*: IrNode
    of IrRet:
      retVal*: IrNode
    of IrLit:
      syntax*: SyntaxType
      litmod*: string # Only for containers.
      items*:  seq[IrNode] # For dicts, [k1, v1, k2, v2]
    of IrMember:
      name*: string
    of IrIndex:
      indexStart*: IrNode
      indexEnd*:   IrNode
    of IrCall:
      binop*:   bool     # When we, e.g., replace + with __add__()
      module*:  string   # For explicit module specifier
      fname*:   string
      actuals*: seq[IrNode]
    of IrUse:
      targetModule*: string
      targetLoc*: string
    of IrUnary:
      uOp*: string
      uRhs*: IrNode
    of IrBinary:
      bOp*:  string
      bLhs*: IrNode
      bRhs*: IrNode
    of IrEnumItem:
      enumItemName*: string
      customValue*:  IrNode
    of IrEnum:
      enumItems*: seq[IrNode]

  FormalInfo* = ref object
    name*: string
    tid*:  TypeId
    va*:   bool

  FuncInfo* = ref object
    # One module can have multiple instantiations of a function, as
    # long as the type signatures of the parameters are not
    # overlapping.
    #
    # So for functions, we ignore SymbolInfo's tid field and
    # come here.
    name*:           string
    rawImpl*:        Con4mNode
    implementation*: IrNode
    tid*:            TypeId
    params*:         seq[FormalInfo]
    retval*:         FormalInfo
    fnScope*:        Scope

  ParamInfo*  = ref object
    shortdoc*:     Option[string]
    doc*:          Option[string]
    validator*:    Option[Callback]
    defaultIr*:    Option[IrNode]
    startValue*:   Option[Mixed]

  SymbolInfo* = ref object
    name*:         string
    isFunc*:       bool
    tid*:          TypeId
    uses*:         seq[IrNode]
    defs*:         seq[IrNode]
    declaredType*: bool
    immutable*:    bool
    fimpls*:       seq[FuncInfo]
    pInfo*:        ParamInfo

  IrGenCtx = object
    pt*:          Con4mNode
    parent*:      IrNode
    errors*:      seq[IrErrInfo]
    globalScope*: Scope # Stuff explicitly declared global w/in this module.
    moduleScope*: Scope
    funcScope*:   Scope
    usedAttrs*:   Scope
    blockScopes*: seq[Scope]
    enums*:       seq[IrNode]
    lhsContext*:  bool
    attrContext*: bool

  Scope = ref object
    table*:  Dict[string, SymbolInfo]
    attr*:   bool

template irError*(ctx: var IrGenCtx, msg: string, prevLoc: IrNode = nil) =
  ctx.errors.add(Con4mErr(msg: msg.text(), kind: ErrIr,
                          token: cast[pointer](ctx.pt.token.get())
                          node: cast[pointer](ctx.pt)))

template irWarn*(ctx: var IrGenCtx, msg: string, prevLoc: IrNode = nil) =
  ctx.errors.add(Con4mErr(msg: msg.text(), kind: Warn,
                          token: cast[pointer](ctx.pt.token.get())
                          node: cast[pointer](ctx.pt)))

template irError*(ctx: var IrGenCtx, msg: Rope, prevLoc: IrNode = nil) =
  ctx.errors.add(Con4mErr(msg: msg, kind: ErrIr,
                          token: cast[pointer](ctx.pt.token.get())
                          node: cast[pointer](ctx.pt)))

template irWarn*(ctx: var IrGenCtx, msg: Rope, prevLoc: IrNode = nil) =
  ctx.errors.add(Con4mErr(msg: msg, kind: Warn,
                          token: cast[pointer](ctx.pt.token.get())
                          node: cast[pointer](ctx.pt)))

template irError*(ctx: var IrGenCtx, err: var Con4mErr) =
  ctx.errors.add(err)

proc getLitMod(ctx: var IrGenCtx): string =
  return ctx.pt.token.get().litType

proc irNode(ctx: var IrGenCtx, kind: IrNodeType) =
  let payload = IrContents(kind: kind)
  return IrNode(parseNode: ctx.pt, contents: payload, parent: ctx.parent)

proc parseTreeToIr(ctx: var IrGenCtx): IrNode

proc downNode(ctx: var IrGenCtx, which: int): IrNode =
  var savedParent = ctx.parent
  ctx.parent      = ctx.pt
  ctx.pt          = ctx.pt.children[which]
  result          = ctx.parseTreeToIr()
  ctx.pt          = ctx.parent
  ctx.parent      = savedParent

proc downNode(ctx: var IrGenCtx, kid, grandkid: int): IrNode =
  var savedParent = ctx.parent

  ctx.parent = ctx.pt.children[kid]
  ctx.pt     = ctx.parent[grandkid]
  result     = ctx.parseTreeToIr()
  ctx.pt     = ctx.parent.parent
  ctx.parent = savedParent

template independentSubtree(newRoot: IrNode, code: untyped) =
  var savedParent = ctx.parent

  ctx.parent = newRoot
  code
  ctx.parent = savedParent

template getText(ctx: var IrGenCtx): string =
  ctx.pt.getTokenText()

template getText(ctx: var IrGenCtx, which: int): string =
   ctx.pt.children[which].getTokenText()

template getText(ctx: var IrGenCtx, kidIx: int, grandIx: int): string =
  ctx.pt.children[kidIx].children[grandIx].getTokenText()

template numKids(ctx: var IrGenCtx): int =
  ctx.pt.children.len()

template kidKind(ctx: var IrGenCtx, i: int): Con4mNodeType =
  ctx.pt.children[i].kind

template kidKind(ctx: var IrGenCtx, i, j: int): Con4mNodeType =
  ctx.pt.children[i].children[j].kind

template numGrandKids(ctx: var IrGenCtx, i: int): int =
  ctx.pt.children[i].children.len()

template parseKid(ctx: var IrGenCtx, i: int): Con4mNode =
  ctx.pt.children[i]

template parseGrandKid(ctx: var IrGenCtx, i, j: int): Con4mNode =
  ctx.pt.children[i].children[j]

proc initScope*(s: var Scope, parent = Scope(nil)) =
  s = Scope(parent: parent)
  s.table.initDict()

proc updateType(sym: var SymbolInfo, tid: TypeId):
               Option[Con4mError] =
  if sym.typeId.unify(tid) == TBottom:
    return some(Con4mError(kind: IrErr,
                           msg: "Current type is incomptable with " &
                             "previous type information"))

proc baseScopeLookup(scope: var Scope, n: string, sym: var SymbolInfo,
                     isFunc: bool, tid = TBottom): Option[Con4mError] =
  # This is a helper function to look up a symbol in the given scope,
  # or create the symbol in the scope, if it is not found.
  #
  # If it's found, we make sure that the current symbol matches us in
  # terms of being a variable or a function.  If there's no error,
  # then it returns the symbol. If it creates the symbol, it will use
  # the value of 'isFunc' passed.

  let symOpt = scope.table.lookup(n)

  if symOpt.isSome():
    sym = symOpt.get()

    if sym.isFunc != isFunc:
      if isFunc:
        return some(Con4mError(kind: IrErr,
                               msg: "Variable cannot have same name as a func"))
      else:
        return some(Con4mError(kind: IrErr,
                               msg: "Func cannot have same name as a variable"))
    if tid != TBottom:
      result = sym.updateType(tid)
  else:
    sym = SymbolInfo(name: n, tid: tid)
    scope.table[name] = sym

proc scopeDeclare*(scope: var Scope, n: string, sym: var SymbolInfo,
                   isfunc: bool, tid = TBottom, immutable = false):
                     Option[Con4mError] =
 # This is meant for things that should be declared in a specific scope,
 # like enums, functions and explicitly declared variables.
 # Generally, these are not assignments, so we do NOT treat this as a 'def'.
 # `immutable` currently is used for enum; we may also add a `const`.

  result = scope.baseScopeLookup(n, sym, id, isfunc, tid)
  if result.isSome():
    return

  # Funcs can have multiple declarations as long as signatures are disjoint.
  if sym.declaredType and not isFunc:
    return some(Con4mError(kind: ErrVar,
                           msg: "Variable declared multiple times."))
  if immutable and sym.defs.len() > 0:
      return some(Con4mError(kinds: ErrVar,
         msg: "Enums are immutable, but value was previously assigned."))

  sym.declaredType = true
  sym.immutable    = immutable

proc addDef*(ctx: var IrGenCtx, name: string, sym: var SymbolInfo,
             loc: IRNode, tid = TBottom): Option[Con4mError] =
  ## This is the interface for *variables* used on the LHS. It should
  ## *not* be called during an explicit definition via `global` or
  ## `var`.
  ##
  ##
  ## If TID isn't TBottom, we'll type-check against whatever we find
  ## already in the symbol table.
  ##
  ## Basically, we have the following scopes:

  ## 1. The global scope.
  ## 2. The module scope.
  ## 3. Function scope.
  ## 4. Block scope for loop index variables only.
  ##
  ## For #4, if you have multiple loops that all do:
  ## `for item in somelist`
  ##
  ## `item` might end up being a different type across loops.
  ## For the this use case, it's fine for `item` to disappear
  ## when exiting the loop.
  ##
  ## However, in cases where we do:
  ## `for i from 0 to somelist.len()`
  ## it might be useful to know the value of i after the loop ends.
  ##
  ## At the IR level we handle this by giving each loop its own scope,
  ## into which we only drop index variables.
  ##
  ## This results in unique SymbolInfo objects, giving us flexibility
  ## for code generation approaches.
  ##
  ## The global vs. module scope is a bit trickier given our attempts
  ## to do as much type inferencing as possible.
  ##
  ## When we first construct the IR, we do not consider the variables
  ## we might get from the global scope directly.  We need to be able
  ## to figure that out when we go to link things together.
  ##
  ## Here's our approach:
  ##
  ## 1. The `var` statement will declare a variable to be unique to
  ##    the scope it's in. If it's used in a function, it stays in the
  ##    function scope. If you use it outside a function, then it's
  ##    defined at a function level.
  ##
  ##    If you use this at the module level, when it's link time, any
  ##    variable of the same name in the global scope will be
  ##    considered different.
  ##
  ##    a. In the local scope, if something has been explicitly added
  ##       to the module scope (with a var statement), then we assume
  ##       variables inside the module's functions can use that variable,
  ##       unless they mask it with a `var` statement.
  ##
  ## 2. If you explicitly declare something to be in the `global`
  ##    scope, it's put in the global scope, no matter where you
  ##    declare it. When we go to link this module, all globals are
  ##    merged, so names must be of compatable type.
  ##
  ##    However, if you declare it global inside the function, we will
  ##    warn about this.
  ##
  ## 3. If you do NOT declare a variable explicitly, then it's hard to
  ##    do the sane thing, but here's our current attempt:
  ##
  ##    a. If a variable is used in the module scope, but not declared,
  ##       and does *not* exist in the global scope, then we leave it
  ##       in the module scope.
  ##
  ##    b. If it *is* in the global scope, we merge if the types are
  ##       compatable, and leave it shadowing the global scope if not,
  ##       giving a warning.  Explicitly declaring removes the warning,
  ##       of course.
  ##
  ##    c. If you use something in the local scope, *if* we see it in the
  ##       module scope, and we can merge it, we will. But we give a
  ##       warning when doing so, always. Then it follows the rules
  ##       for variables in the module scope.
  ##
  ##    d. At link time, we look through all function scopes in that
  ##       module. If they've got non-declared variables, we will merge
  ##       them with the global. This will *also* result in a warning;
  ##       you can get rid of it by re-declaring the variables `global`
  ##       in your function's scope.
  ##
  ##    e. Loop index variables can only be shaddowed by a deeper loop.
  ##       Trying to add a `var` or `global` statement inside the loop
  ##       to shadow the loop index will give an error.
  ##
  ## We want to be insensitive to code moving around, so we should
  ## give the same results whether or not the module scope declares a
  ## variable before or after a function. To that end, when we *do*
  ## explicitly add a `var` or `global` statement, we will go to every
  ## symbol in the module we've seen so far and merge them if
  ## neccessary.
  ##
  ## This also means that, if we *don't* find symbols that are explicitly
  ## declared yet, we can't assume they won't be taken out of our scope.
  ## The var/global statements check to see what's already declared and
  ## will conflict. This kind of stuff will give errors.
  ##
  ## Loops will push on new block scope frames if and only if there are
  ## index variables. In this function, we just check block scopes to
  ## make sure there is no masking of these vars.
  ##
  ## Additionally, if we get to the module or global scope and see a
  ## SymFunc, we will act as if we are looking to shadow that symbol;
  ## this only looks up SymVars.

  var lookup: Option[SymbolInfo]

  assert sym == nil

  for scope in ctx.blockScopes:
    if scope.table.lookup(name).isSome():
      ctx.irError("Cannot assign to (or re-declare) loop iteration variables.")
      return

  if ctx.funcScope != nil:
    lookup = ctx.funcScope.table.lookup(name)

    if lookup.isSome():
      sym = lookup.get()

  if sym == nil:
    lookup = ctx.moduleScope.table.lookup(name)

    if lookup.isSome():
      sym = lookup.get()

  if sym == nil:
    lookup = ctx.globalScope.table.lookup(name)

    if lookup.isSome():
      sym = lookup.get()

  if sym.isFunc and ctx.funcScope != nil:
    # We can shadow this symbol in the local scope.
    sym = nil
  elif sym.isFunc:
    return some(Con4mError(kinds: IrErr, err: "Variable names cannot conflict" &
    " with functions from the same module.")

  if sym == nil:
    let scope = if ctx.funcScope != nil: ctx.functionScope else: ctx.moduleScope

    result = scope.baseScopeLookup(name, sym, tid)
  else:
    if sym.immutable:
      return some(Con4mError(kinds: ErrVal,
                             msg: "Cannot change immutable value."))

    result = sym.updateType(tid)
    if result.isSome():
      return

  sym.defs.add(loc)

proc addUse*(ctx: var IrGenCtx, name: string, sym: var SymbolInfo,
             loc: IRNode, tid = TBottom): Option[Con4mError] =
  var lookup: Option[SymbolInfo]

  assert sym == nil

  for scope in ctx.blockScopes:
    lookup = scope.table.lookup(name)
    if lookup.isSome():
      sym = lookup.get()
      break

  if sym == nil and ctx.funcScope != nil:
    lookup = ctx.funcScope.table.lookup(name)
    if lookup.isSome():
      sym = lookup.get()

  if sym == nil:
    lookup = ctx.moduleScope.table.lookup(name)
    if lookup.isSome():
      sym = lookup.get()

  if sym == nil:
    lookup = ctx.globalScope.table.lookup(name)
    if lookup.isSome():
      sym = lookup.get()

  if sym.isFunc and ctx.funcScope != nil:
    # We can shadow this symbol in the local scope.
    sym = nil
  elif sym.isFunc:
    return some(Con4mError(kinds: IrErr, err: "Variable names cannot conflict" &
    " with functions from the same module.")

  if sym == nil:
    let scope = if ctx.funcScope != nil: ctx.functionScope else: ctx.moduleScope

    result = scope.baseScopeLookup(name, sym, tid)
  else:

    result = sym.mergeTypes(tid)
    if result.isSome():
      return

  sym.uses.add(loc)
const
  ntLoops        = [ NodeForStmt, NodeWhileStmt ]
  ntConditionals = [ NodeIfStmt, NodeElifStmt ]

proc addFalseBranch(conditional: IrNode, falseBranch: IrNode) =
  var n = conditional
  while n.falseBranch != nil:
    n = n.falseBranch

  n.falseBranch = falseBranch

proc checkLabelDupe(ctx: var IrGen, label: string) =
  var n = ctx.parent
  while n != nil:
    if n.kind == IrLoop and n.contents.label == label:
      ctx.irWarn("Nested loops have the same label (" & label & ")", n)
      return
    n = n.parent

proc convertEnum*(ctx: var IrGen) =
  # We skip adding the actual values to the symbol table here, because
  # we're not propogating type information in this pass, at least
  # not yet.
  var stmtNode = ctx.irNode(IrEnum)

  independentSubtree(stmtNode):
    for i in 0 ..< ctx.numKids():

      var itemNode = IrNode(kind: EnumItem, tid: tInt(),
                            parseNode: ctx.pt.children[i],
                            enumItemName: ctx.getText(i, 0),
                            parent: stmtNode)

      if ctx.numKids(i) == 2:
        itemNode.customValue = itemNode.ctx.downNode(i, 1)

      stmtNode.enumItems.add(itemNode)
      var
        sym: SymbolInfo
        err = ctx.moduleScope.scopeDeclare(itemNode.enumItemName, sym,
                                          false, tInt().typeId)
        if err.isSome():
          result.irError(err.get())
          return false

  ctx.enums.add(stmtNode)

proc extractSymInfo(ctx: var IrGen, scope: Scope): SymbolInfo {.discardable.} =
  # Returns the last processed symbol; useful for convertParamBlock where
  # it only accepts one symbol.
  var
    toAdd: seq[(string, TypeId)]

  for varNode in ctx.pt.children:
    var
      varNames:  seq[string]
      foundType: TypeId = TBottom
    for kid in varNode.children:
      if kid.kind == NodeIdentifier:
        varNames.add(kid.getText())
      else:
        foundType = kid.buildType()
    for oneVarName in varNames:
      if foundType == TBottom:
        toAdd.add((oneVarName, tVar()))
      else:
        toAdd.add((oneVarName, foundType.copyType())

  for (name, tid) in toAdd:
    let optErr = scope.scopeDeclare(name, result, false, tid)
    if optErr.isSome():
      ctx.irError(optErr.get())

proc convertVarStmt*(ctx: var IrGen) =
  if ctx.funcScope != nil:
    ctx.extractSymInfo(ctx.funcScope)
  else:
    ctx.extractSymInfo(ctx.moduleScope)

proc convertGlobalStmt*(ctx: var IrGen) =
  ctx.extractSymInfo(ctx.globalScope)

proc convertParamBody*(ctx: var IrGen, sym: var Sym) =
  var
    gotShort, gotLong, gotValid, gotDefault: bool
    paramInfo: ParamInfo()

  independentSubtree(nil):
    for i in 0 ..< ctx.numKids():
      if ctx.kidKind(i) != NodeAttrAssign:
        ctx.irError("No code is allowed inside parameter blocks")
        continue
      let propname = ctx.getText(i, 0)
      case propname
      of "shortdoc":
        if gotShortDoc:
          ctx.irError("Duplicate parameter property for 'shortdoc'")
          continue
        if ctx.kidKind(i, 1) != NodeStringLit:
          ctx.irError("'shortdoc' parameter field must be a string literal.")
          continue
        paramInfo.shortdoc = some(ctx.getText(i, 1))
        gotShortDoc = true
      of "doc":
        if gotLong:
          ctx.irError("Duplicate parameter property for 'doc'")
          continue
        if ctx.kidKind(i, 1) != NodeStringLit:
          ctx.irError("'doc' parameter field must be a string literal.")
          continue
        paramInfo.doc = some(ctx.getText(i, 1))
        gotLong = true
      of "validator":
        if gotValid:
          ctx.irError("Duplicate parameter property for 'validator'")
          continue
        if ctx.kindKind(i, 1) != NodeCallback:
          ctx.irError("'validator' parameter property must be a callback.")
        let irNode = ctx.downNode(i, 1)
        ## You are here.
        paramInfo.validator = some(toVal[Callback])
        gotValid = true
      of "default":
        if gotDefault:
          ctx.irError("Duplicate parameter property for 'default'")
          continue
        paramInfo.defaultIr = some(ctx.pt.children[i].children[1])
        gotDefault          = true
      else:
        ctx.irError("Invalid parameter property: " & propname)
        continue

  sym.pinfo = paramInfo()

proc convertParamBlock*(ctx: var IrGen) =
  var
    sym: SymbolInfo
    tid: TypeIndo

  if ctx.pt.children[0].kind == NodeMember:

    independentSubtree(nil):
      let memberIr = ctx.downNode(0)
      ctx.usedAttrs.scopeDeclare(memberIr.name, sym, false, tVar())
  else: # will be something we can call extractSymInfo on.
    var savedPt = ctx.pt
    ctx.pt = ctx.pt.children[0]
    sym = ctx.extractSymInfo(ctx.moduleScope)
    ctx.pt = savedPt

  var savedPt = ctx.pt
  ctx.pt = ctx.pt.children[1]
  ctx.convertParamBody(sym)
  ctx.pt = savedPt

proc findExplicitDeclarations(ctx: var IrGenCtx, n: Con4mNode) =
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

  for kid in n.children:
    case item.kind
    of NodeFuncDef:
      continue
    of NodeVarStmt:
      ctx.convertVarStmt()
    of NodeGlobalStmt:
      ctx.convertGlobalStmt()
    of NodeParamBlock:
      ctx.convertParamBlock()
    else:
      ctx.findExplicitDeclarations(kid)

proc findExplicitDeclarations(ctx: var IrGenCtx) =
  # First, pull declarations from the toplevel scope.
  ctx.findExplicitDeclarations(ctx.pt)

  for i in 0 ..< ctx.numKids():
    let item = ctx.parseKid(i)

    case item.kind
    of NodeEnumStmt:
      ctx.convertEnum() # Enums are fully processed in this first pass.
      continue
    of NodeFuncDef:
      # Function definitions are processed for declarations.
      ctx.convertFuncDefinition()
      continue
    else:
      discard

proc convertFormal(info: FuncInfo, n: Con4mNode) =
  var formalInfo = FormalInfo()

  if n.children[0].kind == NodeIdentifier:
    formalInfo.name = n.children[0].getText()
  else:
    formalInfo.va = true
    formalInfo.name = n.children[0].children[0].getText()

  if n.children.len() == 2:
    formalInfo.tid = n.children[1].buildType()
  else:
    formalInfo.tid = newTypeVar().typeid

  info.params.add(formalInfo)

proc setupTypeSignature(info: FuncInfo, n: Con4mNode) =
  if n != nil:
    info.retVal     = FormalInfo(name: "result")
    info.retval.tid = n.buildType()
  else:
    info.retVal = FormalInfo(name: "result", tid = newTypeVar().typeId)

  var
    actTypes: seq[typeid]
    va = false

  if info.params.len() != 0 and info.params[^1].va:
    va = true

  for actual in info.params:
    actTypes.add(actual.tid)

  actTypes.add(info.retVal.tid)

  info.tid = newFuncType(actTypes, va)

  if va == true:
    # From the body of the function, the named parameter collects
    # all arguments of the given type into a list. This sets the
    # ACTUAL type that we'll add to the function's symbol table.
    info.params[^1].tid = tList(info.params[^1].tid).typeId

proc handleFuncdefSymbols(ctx: var IrGen, fname: string, info: var FuncInfo) =
  var
    sym: SymbolInfo

  info.fnScope.initScope()

  for item in info.params & @[info.retVal]:
    ctx.irErrOpt(info.fnScope.scopeDeclare(info.name, sym, SymVar, item.tid))

  ctx.irErrOpt(ctx.moduleScope.scopeDeclare(info.name, sym, SymFunc, info.tid))
  sym.fimpls.add(info)

proc convertFuncDefinition(ctx: var IrGen) =
  ## This converts the declaration portion, NOT the body. For now, the
  ## body just goes into the FuncInfo `rawImpl` parameter. Once we have
  ## found all explicitly declared symbols, we then come back to
  ## convert the tree into IR.
  var
    funcName   = ctx.getText(0)
    info       = FuncInfo()
    returnType = Con4mNode(nil)

  # Params are in the second node, and the last item there might
  # have the varargs marker, which changes what we insert into the symbol
  # table for that thing.
  for item in ctx.st.children[1]:
    info.convertFormal(item)

  if ctx.numKids() == 4:
    returnType = ctx.ct.kids[2]

  info.setupTypeSignature(returnType)
  ctx.handleFuncdefSymbols(funcName, info)

  info.rawImpl = ctx.pt.children[^1]
  ctx.findExplicitDeclarations(info.rawImpl)

proc statementsToIr(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrBlock)
  var skipI = false

  for i in 0 ..< ctx.numKids():
    let item = ctx.parseKid(i)

    case item.kind
    of NodeBreak, NodeContinue, NodeReturn:
      if i != ctx.numKids() + 1:
        ctx.irWarn("Dead code after '" & ctx.getText() & "' statement.")
        result.stmts.add(ctx.downNode(i))
        return # Don't process that dead code.
    of NodeLabel:
      if i == ctx.numKids() - 1 and ctx.parseKid(i + 1).kind in ntLoops:
          let one   = ctx.downNode(i + 1)
          one.label = ctx.parseGrandKid(i, 0)
          skipI = true
          result.stmts.add(one)
          ctx.checkLabelDupe(one.label)
      else:
        ctx.irError("'label' statement must proceed a 'for' or 'while' loop")
        continue
    of NodeElifStmt:
      if i != 0:
        let prev = ctx.parseKid(i - 1)
        if prev.kind in ntConditionals:
          result.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("'elif' statement must follow an 'if' block or another " &
                  "'elif' block.")
      continue
    of NodeElseStmt:
      if i != 0:
        let prev = ctx.parseKid(i = 1)
        if prev.kind in ntConditionals:
          result.stmts[^1].addFalseBranch(ctx.downNode(i))
          continue
      ctx.irError("'else' statement must follow an 'if' block or another " &
                  "'elif' block.")
      continue
    # These items all get handled early so that we know what is
    # explicitly declared. Funcdefs and parameter blocks do need some
    # further processing, but that happens before the main body is
    # processed; this function gets called for those nodes seprately.
    of NodeEnumStmt, NodeFuncDef, NodeParamBlock, NodeVarStmt, NodeGlobalStmt:
      continue
    else:
      discard  # An expression.

    if skipI:
      skipI = false
      continue
    else:
      result.stmts.add(ctx.downNode(i))

proc convertAttrAssignment(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrAttrAssign)
  var
    parts: seq[string] = @[]
    kid = ctx.parseKid(0)
  while true:
    parts.add(kid.getTokenText())
    if kid.children.len() == 0:
      break
    kid = kid.children[0]
  ctx.lhsContext  = true
  ctx.attrContext = true
  result.attrlhs  = parts.join(".")
  ctx.attrContext = false
  ctx.lhsContext  = false
  result.attrrhs  = ctx.downNode(1)
  result.tid      = result.attrrhs.tid

proc convertVarAssignment(ctx: var IrGenCtx): IrNode =
  ctx.convertVarAssignment()
  result = ctx.irNode(IrVarAssign)
  ctx.lhsContext = true
  for i in 0 ..< ctx.children.len() - 1:
    result.varlhs.add(ctx.downNode(i))
  ctx.lhsContext = false
  result.varrhs  = ctx.downNode(ctx.children.len() - 1)

proc convertConditional(ctx: var IrGenCtx): IrNode =
  result            = ctx.irNode(IrConditional)
  result.condition  = ctx.downNode(0)
  result.trueBranch = ctx.downNode(1)

proc convertSection(ctx: var IrGenCtx): IrNode =
  result         = ctx.irNode(IrSectionScope)
  result.secType = ctx.getText(0)
  if ctx.numKids() == 3:
    result.secName = ctx.getText(1)
    result.body    = ctx.downNode(2)
  else:
    result.body    = ctx.downNode(1)

proc convertForStmt(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrLoop)
  if ctx.numKids() == 3:
    result.contents.keyVar = ctx.getText(0, 0)
    if ctx.numGrandKids(0) == 2:
      result.contents.valVar = ctx.getText(0, 1)

    result.contents.condition = ctx.downNode(1)
    result.contents.loopBody  = ctx.downNode(2)
  else:
    result.contents.keyVar   = ctx.getText(0, 0)
    result.contents.startIx  = ctx.downNode(1)
    result.contents.endIx    = ctx.downNode(2)
    result.contents.loopBody = ctx.downNode(3)

proc convertWhileStmt(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrLoop)
  result.contents.condition = ctx.downNode(0)
  result.contents.loopBody  = ctx.downNode(1)

proc loopExit(ctx: var IrGenCtx, loopExit: bool): IrNode =
  result = ctx.irNode(IrJump)
  result.contents.exitLoop = loopExit
  if ctx.numKids() != 0:
    let label = ctx.getText(0)
    var n = ctx.parent
    while n != nil:
      if n.contents.kind == IrLoop:
        if n.contents.label == label:
          result.contents.targetNode = n
          return
      n = n.parent
    ctx.irError(ctx.getText() & " to label '" & label &
      "' is invalid, because it is not contained inside a loop that was " &
      "given that label.")

  var n = ctx.parent
  while n != nil:
    if n.kind == IrLoop:
      result.contents.targetNode = n
      return
    n = n.parent

proc convertReturn(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrRet)
  if ctx.numKids() == 1:
    result.contents.retVal = ctx.downNode(0)

proc convertTypeLit(ctx: var IrGenCtx): IrNode =
  var
    tvars: Dict[string, TypeId]
    tinfo: TypeId

  result                 = ctx.irNode(IrLit)
  result.tid             = some(tTypeSpec().typeId)
  tinfo                  = ctx.pt.buildType(tvars)
  result.contents.litVal = tinfo.toMixed()

proc convertCharLit(ctx: var IrGenCtx): IrNode =
  let
    err: string
    tid: int
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)

  if litMod != "":
    tid = getTypeIdFromSyntax(tid, lmod, STChrQuotes)
    if err != "":
      ctx.irError(err)
    else:
      let
        codepoint = ctx.pt.token.get().codepoint
        val       = initializeCharLiteral(tid, codepoint, err)
      if err != "":
        ctx.irError(err)
      else:
        result.value = some(val)

proc convertLit(ctx: var IrGenCtx, st: SyntaxType): IrNode =
  var
    err: string
    tid: int
    lmod = ctx.getLitMod()

  result = ctx.irNode(IrLit)

  result.contents.syntax = st

  if lmod != "":
    tid = getTypeIdFromSyntax(st, lmod, err)
    if err != "":
      ctx.irError(err)
    else:
      let val = parseLiteral(tid, ctx.getText(), err, st)
      if err != "":
        ctx.irError(err)
      else:
        result.value = some(val)

proc convertListLit(ctx: var IrGenCtx): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STList
  result.contents.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids:
    result.items.add(ctx.downNode(i))

proc convertDictLit(ctx: var IrGenCtx): IrNode =
  result                 = ctx.irNode(IrLit)
  result.contents.syntax = STDict
  result.contents.litmod = ctx.getLitMod()
  for i in 0 ..< ctx.numKids():
    result.items.add(ctx.downNode(i, 0))
    result.items.add(ctx.downNode(i, 1))

proc convertTupleLit(ctx: var IrGenCtx): IrNode =
  result          = ctx.irNode(IrLit)
  result.contents = STTuple
  result.contents = ctx.getLitMod()
  for i in 0 ..< ctx.numKids():
    result.items.add(ctx.downNode(i))

proc convertCallbackLit(ctx: var IrGenCtx): IrNode =
  ## TODO:
  ##
  ##  1) Makes no sense for callback literals to not have a name,
  ##     unless we're actually in a *type* literal, where we might
  ##     want to accept the name as part of the type?? Nuke.
  ##
  ##  2) The callback needs to be run through a addFuncUse(); we need
  ##     to see if it matches anything in the module, and if it
  ##     doesn't, keep it in a list of things we need to resolve when
  ##     linking.
  var
    cb: Callback
  result = ctx.irNode(IrLit)
  if ctx.pt.children[0].kind != NodeNoCallbackName:
    cb.name = getText(0, 0)
  if ctx.numKids() == 2:
    var
      tvars: Dict[string, TypeId]
    cb.tid = ctx.pt.children[1].buildType(tvars)
    result.tid = tref(cb.tid)
  else:
    result.tid = tref(newFuncType(@[]))
  result.value = some(cb.toMixed())

proc convertMember(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrMember)
  var parts: seq[string]
  for i in 0 ..< ctx.numKids():
    parts.add(ctx.getText(i))
  result.name = parts.join(".")

proc convertIndex(ctx: var IrGenCtx): IrNode =
  result = ctx.irNode(IrIndex)
  result.indexStart = ctx.downNode(0)
  if ctx.numKids() == 2:
    result.indexEnd = ctx.downNode(1)

proc convertCall(ctx: var IrGenCtx): IrNode =
  result       = ctx.irNode(IrCall)
  result.fname = ctx.getText(0)

  for i in 0 ..< ctx.numKids(1):
    result.actuals.add(ctx.downNode(1, i))

proc convertUseStmt(ctx: var IrGenCtx): IrNode =
  result                       = ctx.irNode(IrUse)
  result.contents.targetModule = ctx.getText(0)
  if ctx.numKids() == 2:
    result.contents.targetLoc = ctx.getText(1)

proc convertUnaryOp(ctx: var IrGenCtx): IrNode =
  result               = ctx.irNode(IrUnary)
  result.contents.uOp  = ctx.getText()
  result.contents.uRhs = ctx.downNode(0)

const notFloatOps = ["shl", "shr", "and", "or", "xor", "div", "%"]

template binOpReplace(ctx: var IrGenCtx, o, f: string) =
  if n.binOp == o:
    cc.fname = f
    cc.actuals = @[n.contents.bLhs, n.contents.bRhs]
    n.contents = cc

template boolOpReplace(ctx: var IrGenCtx, o, f: string) =
  if n.binOp == o:
    cc.fname = f
    cc.actuals = @[n.contents.bLhs, n.contents.bRhs]
    n.contents = cc
    n.tid = TBool

proc replaceBinOpWithCall(ctx: var IrGenCtx, n: IrNode) =
  var cc = IrContents(kind: IrCall, binop: true)

  ctx.binOpReplace("/", "__slash__")
  ctx.binOpReplace("*", "__star__")
  ctx.binOpReplace("+", "__plus__")
  ctx.binOpReplace("-", "__minus__")
  ctx.binOpReplace("%", "__percent__")
  ctx.binOpReplace("shl", "__shl__")
  ctx.binOpReplace("shr", "__shr__")
  ctx.binOpReplace("div", "__div__")
  ctx.binOpReplace("&", "__bitand__")
  ctx.binOpReplace("|", "__bitor__")
  ctx.binOpReplace("^", "__bitxor__")
  ctx.binOpReplace("shl", "__shl__")
  ctx.binOpReplace("shr", "__shr__")
  ctx.boolOpReplace("<",   "__lt__")
  ctx.boolOpReplace("<=",  "__lte__")
  ctx.boolOpReplace(">",   "__gt__")
  ctx.boolOpReplace(">=",  "__gte__")
  ctx.boolOpReplace("!=",  "__ne__")
  ctx.boolOpReplace("==",  "__eq__")

proc convertBinaryOp(ctx: var IrGenCtx): IrNode =
  result          = ctx.irNode(IrBinary)
  result.contents.bOp   = ctx.getText()
  result.contents.bLhs  = ctx.downNode(0)
  result.contents.bRhs  = ctx.downNode(1)

  result.tid = result.bLhs.contents.tid.unify(result.bRhs.contents.tid)

  if result.tid == TBottom and result.bLhs.isNumericBuiltin() and
     result.bRhs.isNumericBuiltin():
    result.tid = resultingNumType(result.bLhs.tid, result.bRhs.tid, warn)

    if result.tid == TFloat and result.bOp in notFloatOps:
      result.tid = TBottom

  if result.tid == TBottom:
    result.irError(BinaryOperandsNotCompatable)
  else:
    case result.contents.bOp
    of "/":
      if result.tid.isNumericBuiltin():
        result.tid == TFloat
      else:
        ctx.replaceBinOpWithCall(result)
    of "+", "-", "*":
      if not result.tid.isNumericBuiltin():
        ctx.replaceBinOpWithCall(result)
    of "shl", "shr", "div", "&", "|", "^", "%":
      if result.tid == TFloat or not result.tid.isNumericBuiltin():
        ctx.replaceBinOpWithCall(result)
    of ">", "<", ">=", "<=", "!=", "==":
      result.tid = TBool
      if not result.tid.isNumericBuiltin():
        ctx.replaceBinOpWithCall(result)

proc parseTreeToIr(ctx: var IrGenCtx): IrNode =
  case ctx.pt.kind
  of NodeModule:
    ctx.moduleScope.initScope()
    ctx.findExplicitDeclarations()
    for (name, sym) in ctx.globalScope.table.items():
      for impl in sym.fimpls:
        ctx.funcScope = impl.fnScope
        ctx.pt        = impl.rawImpl
        ctx.parent    = nil
        ctx.parseTreeToIr() # This should be a body node.
      if sym.pInfo != nil and sym.pinfo.defaultIr.isSome():
        ctx.funcScope = nil
        ctx.pt        = ctx.pinfo.defaultIr.get()
        ctx.parent    = nil
        ctx.parseTreeToIr() # This should be some sort of expression node.
    result = ctx.statementsToIr()
  of NodeBody:
    result = ctx.statementsToIr()
  of NodeAttrAssign:
    result = ctx.convertAttrAssignment()
  of NodeAttrSetLock:
    result      = ctx.downNode(0)
    result.lock = true
  of NodeUnpack, NodeVarAssign:
    result = ctx.convertVarAssignment()
  of NodeSection:
    result = ctx.convertSection()
  of NodeIfStmt, NodeElifStmt:
    result = ctx.convertConditional()
  of NodeElseStmt:
    result = ctx.downNode(0)
  of NodeForStmt:
    result = ctx.convertForStmt()
  of NodeWhileStmt:
    result = ctx.convertWhileStmt()
  of NodeBreakStmt:
    result = ctx.loopExit(true)
  of NodeContinueStmt:
    result = ctx.loopExit(false)
  of NodeReturnStmt:
    result = ctx.convertReturn()
  of NodeType:
    result = ctx.convertTypeLit()
  of NodeStringLit:
    result = ctx.convertLit(StStrQuotes)
  of NodeIntLit:
    result = ctx.convertLit(STBase10)
  of NodeHexLit:
    result = ctx.convertLit(StHex)
  of NodeFloatLit:
    result = ctx.convertLit(STFloat)
  of NodeBoolLit:
    result = ctx.convertLit(StBoolLit)
  of NodeOtherLit:
    result = ctx.convertLit(StOther)
  of NodeCharLit:
    result = ctx.convertCharLit()
  of NodeDictLit:
    result = ctx.convertDictLit()
  of NodeListLit:
    result = ctx.convertListLit()
  of NodeTupleLit:
    result = ctx.convertTupleLit()
  of NodeCallbackLit:
    result = ctx.convertCallbackLit()
  of NodeOr, NodeAnd, NodeNe, NodeCmp, NodeGte, NodeLte, NodeGt, NodeLt,
     NodeMod, NodeMul, NodeDiv:
    result = ctx.convertBinaryOp()
  of NodePlus, NodeMinus:
    if ctx.numKids == 2:
      result = ctx.convertBinaryOp()
    else:
      result = ctx.convertUnaryOp()
  of NodeNot:
    result = ctx.convertUnaryOp()
  of NodeMember:
    result = ctx.convertMember()
  of NodeLiteral, NodeParenExpr, NodeExpression:
    result = ctx.downNode(0)
  of NodeIndex:
    result = ctx.convertIndex()
  of NodeCall:
    result = ctx.convertCall()
  of NodeUseStmt:
    result = ctx.convertUseStmt()
  of NodeIdentifier:
    result = ctx.downNode(0)
  else:
    unreachable
