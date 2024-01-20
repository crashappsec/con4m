import parse, strutils, specs

template getTid*(s: SymbolInfo): TypeId =
  s.tid.getTid()

proc initScope*(fn = false): Scope =
  result         = Scope()
  result.fnScope = fn
  result.table.initDict()

proc newModuleObj*(ctx: CompileCtx, contents: string, name: string, where = "",
                   ext, url, key: string): Module =

  result = Module(url: url, where: where, modname: name, ext: ext,
                  key: key, s: newStringCursor(contents))
  result.moduleScope = initScope()
  result.usedAttrs   = ctx.usedAttrs
  result.globalScope = ctx.globalScope
  result.attrSpec    = ctx.attrSpec
  ctx.modules[key]   = result

proc lookupOrAdd(ctx: Module, scope: var Scope, n: string,
                 isFunc: bool, tid = TBottom): Option[SymbolInfo] =
  # This is a helper function to look up a symbol in the given scope,
  # or create the symbol in the scope, if it is not found.
  #
  # If it's found, we make sure that the current symbol matches us in
  # terms of being a variable or a function.  If there's no error,
  # then it returns the symbol. If it creates the symbol, it will use
  # the value of 'isFunc' passed.

  var sym: SymbolInfo

  result = scope.table.lookup(n)

  if result.isSome():
    sym = result.get()

    if sym.isFunc != isFunc:
      if isFunc:
        ctx.irError("AlreadyAFunc")
        return none(SymbolInfo)
      else:
        ctx.irError("AlreadyAVar")
        return none(SymbolInfo)

    if tid != TBottom:
      ctx.typeCheck(sym, tid)
  else:
    sym = SymbolInfo(name: n, tid: tid, isFunc: isFunc, module: ctx)

    if scope.fnScope:
      sym.inFunc = true
    scope.table[n] = sym
    scope.numSyms += 1
    result = some(sym)
    if scope == ctx.moduleScope or
       (ctx.funcScope == nil and scope != ctx.usedAttrs):
      sym.heapAlloc = true

proc scopeDeclare*(ctx: Module, scope: var Scope, n: string,
                   isfunc: bool, tid = TBottom, immutable = false,
                  declnode = ctx.pt):
                     Option[SymbolInfo] =
 # This is meant for things that should be declared in a specific scope,
 # like enums, functions and explicitly declared variables.
 # Generally, these are not assignments, so we do NOT treat this as a 'def'.
 # `immutable` currently is used for enum; we may also add a `const`.

  result = ctx.lookupOrAdd(scope, n, isfunc, tid)
  if result.isNone():
    return

  var sym = result.get()

  # Funcs can have multiple declarations as long as signatures are disjoint.
  if sym.declaredType and not isFunc:
    ctx.irWarn("VarRedef")
  else:
    sym.declNode = declnode

  # Since local enums are always processed before defs, this check is
  # useless.
  #if immutable and sym.defs.len() > 0:
  #  ctx.irWarn("Immutable")

  sym.declaredType = true
  sym.immutable    = immutable

proc typeFromSpec*(sec: SectionSpec, name: string): TypeId =
  if sec == nil:
    return TBottom

  let fsOpt = sec.fields.lookup(name)
  if fsOpt.isNone():
    return TBottom

  result = fsOpt.get().tid.copyType().typeId

proc resolveSection*(ctx: Module, n: seq[string]): SectionSpec =
  var
    curSpec      = ctx.curSecSpec
    i            = 0

  while i < n.len():
    let
      subSecName = n[i]
      subSecOpt  = ctx.attrSpec.secSpecs.lookup(subSecName)


    # Allowed section in cur spec?
    if subSecName notin curSpec.allowedSections:
      if subSecOpt.isSome():
        ctx.irError("SecNotAllowed", @[subSecName, curSpec.name])
      else:
        ctx.irError("NotASection", @[subSecName])

    if subSecOpt.isNone():
      if curSpec != nil and not curSpec.singleton and i != 0:
        if n[i - 1] in curSpec.allowedSections:
          ctx.irError("4gotObjName?", @[subSecName, n[i - 1]])
          return nil
      ctx.irError("NotASection", @[subSecName])
      return nil

    i += 1

    curSpec = subSecOpt.get()
    if not curSpec.singleton:
      if i == n.len():
        ctx.irError("NotASingleton", @[subSecName])
        return nil
      i += 1

  return curSpec

proc searchForSymbol*(ctx: Module, name: string): Option[SymbolInfo] =
  ## Look for a symbol, first in lexical scopes, then in the attribute
  ## scope.

  var sym: SymbolInfo

  for scope in ctx.blockScopes:
    result = scope.table.lookup(name)
    if result.isSome():
      return

  if ctx.funcScope != nil:
    result = ctx.funcScope.table.lookup(name)
    if result.isSome():
      return

  result = ctx.moduleScope.table.lookup(name)
  if result.isSome():
    return

  result = ctx.globalScope.table.lookup(name)
  if result.isSome():
    return

  result = ctx.usedAttrs.table.lookup(name)
  if result.isSome():
    return

  let
    parts = name.split(".")
    fi    = ctx.attrSpec.getFieldInfo(parts)

  case fi.fieldKind
  of FsErrorNoSpec, FsUserDefField:
    if parts.len() == 1:
      return none(SymbolInfo)
  of FsField:
    discard
  else:
    return none(SymbolInfo)

  sym = SymbolInfo(name: name, tid: fi.tid.tCopy(), isAttr: true)

  if fi.tid == TBottom:
    fi.tid = tVar()

  sym.defaultVal = fi.defaultVal
  ctx.usedAttrs.table[name] = sym
  ctx.usedAttrs.numSyms    += 1

  return some(sym)

proc addVarDef*(ctx: Module, name: string, loc: IRNode,
             tid: TypeId): Option[SymbolInfo] =
  ## This is the interface for *variables* used on the LHS. It should
  ## *not* be called during an explicit definition via `global` or
  ## `var` (unless we later add assignment within these statements).
  ##
  ## If TID isn't TBottom, we'll type-check against whatever we find
  ## already in the symbol table.
  ##
  ## Basically, we have the following scopes:
  ##
  ## 1. The global scope.
  ## 2. The module scope.
  ## 3. Function scope.
  ## 4. Block scope for loop index variables only.
  ## 5. Attribute scopes.

  ## If this is called, either := was used, indicating to only try the
  ## attr scope (this operator might go away), or we tried the attr
  ## scope, and the variable wasn't allowed there.
  ##
  ## We only stick stuff in the global scope if it was pre-declared.
  ##
  ## So we need to make sure the name isn't conflicting w/ something
  ## in the block scope, and if not, then we look in the function,
  ## module and global scopes as appropriate to see if they're already
  ## there (taking the first hit); if they aren't, then we declare it
  ## in the function scope (if we're in a function), or the module
  ## scope if not.
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

  var sym: SymbolInfo

  if name[0] == '$':
    ctx.irError("$assign")

  result = ctx.searchForSymbol(name)

  if result.isSome():
    sym = result.get()

    for scope in ctx.blockScopes:
      let symOpt = scope.table.lookup(name)
      if symOpt.isSome():
        ctx.irError("LoopVarAssign")

    if sym.isFunc:
        ctx.irError("AlreadyAFunc")

    if sym.immutable and sym.defs.len() != 0:
      ctx.irError("Immutable")
      return

    if sym.tid != TBottom:
      ctx.typeCheck(sym.tid, tid)

  else:
    var scope = if ctx.funcScope != nil: ctx.funcScope else: ctx.moduleScope

    result = ctx.lookupOrAdd(scope, name, false, tid)
    if result.isNone():
      return
    else:
      sym = result.get()


  if sym.module == nil:
    sym.module = ctx

  sym.defs.add(loc)
  result = some(sym)

proc addAttrDef*(ctx: Module, name: string, loc: IRNode,
                 tid: TypeId, canConvertToVar: bool): Option[SymbolInfo] =
  ## `canConvertToVar` means that we used the `=` operator, which does
  ## attribute assignment, unless the attribute isn't allowed, in which
  ## case:
  ##
  ## - If we're defining a section, it warns, then sets a variable.
  ## - Otherwise, sets the variable without complaining.
  ##
  ## The `:` operator currently is attribute-assignment only.
  ##
  ## If there's no '.' in the name, this flag is true, and we
  ## get an error back (including the no-spec error), then we
  ## call addVarDef().

  result = ctx.usedAttrs.table.lookup(name)

  if result.isSome():
    let sym = result.get()
    ctx.typeCheck(sym.tid, tid, loc.parseNode)
    return

  ## We *never* allow user defined fields in the root scope.
  ## Turn it off here in case someone accidentally turns it on.
  if ctx.attrSpec != nil:
    ctx.attrSpec.rootSpec.userDefOk = false

  var sym: SymbolInfo

  let
    parts     = name.split(".")
    fieldInfo = ctx.attrSpec.getFieldInfo(parts)

  if len(parts) == 1:
    if canConvertToVar and fieldInfo.fieldKind != FsField:
      if ctx.secDefContext:
         ctx.irWarn("VarInSecDef", loc)
      return ctx.addVarDef(name, loc, tid)
    if not canConvertToVar and
       fieldInfo.fieldKind in [FsErrorFieldNotAllowed]:
      ctx.irError("TryVarAssign", loc)
      return ctx.addVarDef(name, loc, tid)
  case fieldInfo.fieldKind
  of FsUserDefField, FsErrorNoSpec:
    sym = SymbolInfo(name: name, tid: tVar(), isAttr: true)
    ctx.usedattrs.numSyms += 1
  of FsField:
    # Don't mess up other sections if there's a type variable..
    sym = SymbolInfo(name: name, tid: fieldInfo.tid.tCopy(), isAttr: true)
    ctx.usedattrs.numSyms += 1
    sym.defaultVal = fieldInfo.defaultVal
  of FsObjectType:
    ctx.irError("AssignToSec", loc, @[name])
  of FsSingleton:
    ctx.irError("AsgnSingleton", loc, @[name])
  of FsObjectInstance:
    ctx.irError("AsgnInstance", loc, @[name])
  of FsErrorSecUnderField:
    let subname = parts[0 .. fieldInfo.errIx].join(".")
    ctx.irError("SecUnderField", loc, @[subname])
  of FsErrorNoSuchSec:
    if fieldInfo.errIx == 0:
      ctx.irError("RootNoSec", loc, @[name])
    else:
      let subname = parts[0 ..< fieldInfo.errIx].join(".")
      ctx.irError("SectionNoSpec", loc, @[subname, parts[fieldInfo.errIx]])
  of FsErrorSecNotAllowed:
    if fieldInfo.errIx == 0:
      ctx.irError("RootSecDenied", loc, @[name])
    else:
      let subname = parts[0 ..< fieldInfo.errIx].join(".")
      ctx.irError("SectionDenied", loc, @[subname, parts[fieldInfo.errIx]])
  of FsErrorFieldNotAllowed:
    ctx.irError("AttrNotSpecd", loc, @[name])

  if sym == nil:
    sym = SymbolInfo(name: name, tid: tVar(), isAttr: true, err: true)
    ctx.usedattrs.numSyms += 1

  ctx.usedAttrs.table[name] = sym
  ctx.typeCheck(sym.tid, tid)

  if sym.module == nil:
    sym.module = ctx


  sym.defs.add(loc)
  result = some(sym)

proc addDef*(ctx: Module, name: string, loc: IRNode,
             tid = TBottom): Option[SymbolInfo] =
  if ctx.attrContext:
    result = ctx.addAttrDef(name, loc, tid, ctx.ambigAssign)
  else:
    result = ctx.addVarDef(name, loc, tid)

proc addUse*(ctx: Module, name: string, loc: IRNode,
             tid: TypeId): Option[SymbolInfo] =
  var sym: SymbolInfo

  result = ctx.searchForSymbol(name)
  if result.isSome():
    sym = result.get()
    ctx.typeCheck(sym.tid, tid)
    sym.uses.add(loc)
    return some(sym)
  elif "." in name:
    let
      parts     = name.split(".")
      fieldInfo = ctx.attrSpec.getFieldInfo(parts)

    # searchForSymbol will have already tried the call to getFieldInfo,
    # but doesn't give any error message.
    case fieldInfo.fieldKind
    of FsErrorNoSpec, FsUserDefField:
      sym = SymbolInfo(name: name, tid: tid.tCopy(), isAttr: true)
      sym.uses.add(loc)
      ctx.usedAttrs.table[name] = sym
      ctx.usedAttrs.numSyms    += 1
      return some(sym)
    of FsField:
      unreachable
    of FsObjectType, FsSingleton:
      ctx.irError("NoSecUse", loc)
    of FsObjectInstance:
      ctx.irError("NoInstUse", loc)
      # Else assume it's a variable.
    of FsErrorSecUnderField:
      let subname = parts[0 .. fieldInfo.errIx].join(".")
      ctx.irError("SecUnderField", loc, @[subname])
    of FsErrorNoSuchSec:
      let subname = parts[0 ..< fieldInfo.errIx].join(".")
      ctx.irError("SectionNoSpec", loc, @[subname, parts[fieldInfo.errIx]])
    of FsErrorSecNotAllowed:
      let subname = parts[0 ..< fieldInfo.errIx].join(".")
      ctx.irError("SectionDenied", loc, @[subname, parts[fieldInfo.errIx]])
    of FsErrorFieldNotAllowed:
      ctx.irError("AttrNotSpecd", loc, @[name])

    sym = SymbolInfo(name: name, tid: tVar(), isAttr: true, err: true)
    ctx.usedAttrs.table[name] = sym
    ctx.usedAttrs.numSyms    += 1
    sym.uses.add(loc)
    return some(sym)

  else:
    var scope: Scope

    if ctx.funcScope != nil:
      scope = ctx.funcScope
    else:
      scope = ctx.moduleScope

    result = ctx.lookupOrAdd(scope, name, false, tid.followForwards())

    if result.isNone():
      return
    else:
      sym = result.get()

  if sym.module == nil:
    sym.module = ctx

  sym.uses.add(loc)

template isDeclaredRepr(v: bool): Rope =
  if v:
    fgColor("✓", "atomiclime")
  else:
    fgColor("✗", "red")

proc toRope*(s: Scope, title = ""): Rope =
  if s == nil:
    return h4("Scope is not initialized.")
  var
    hdr   = @[atom("Symbol"), atom("Type"), atom("Offset"), atom("Decl?"),
              atom("Const"), atom("fn?"), atom("#Def"), atom("#Use")]
    cells = @[hdr]
    contents = s.table.items(sort = true)

  if contents.len() == 0:
    if title != "":
      return h4(title & ": Scope contains no contents.")
    else:
      return h4("Scope contains no contents.")

  for (name, sym) in contents:
    var
      cc     = fgColor("✗", "red")
      isFunc = fgColor("✗", "red")
    if sym.haveConst:
      let box = sym.constValue
      cc = fgColor($(call_repr(box, sym.tid)), "atomiclime")
    elif sym.immutable:
      cc = fgColor("✓ ", "atomiclime") + fgColor("(not set)", "yellow")
    if sym.fImpls.len() != 0:
      isFunc = fgColor("✓", "atomiclime")

    if sym.fImpls.len() == 0:
      cells.add(@[name.atom(), sym.tid.followForwards().toRope(),
                  atom($(sym.offset)),
                  sym.declaredType.isDeclaredRepr(), cc, isFunc,
                  atom($(sym.defs.len())),
                  atom($(sym.uses.len()))
            ])
    else:
      for item in sym.fImpls:
        cells.add(@[name.atom(), item.tid.toRope(), atom("n/a"),
                  sym.declaredType.isDeclaredRepr(),
                  fgColor("✓", "atomiclime"), fgColor("✓", "atomiclime"),
                  atom("n/a"), atom("n/a")])


  return quickTable(cells, title = title, caption = $(s.numSyms) & " symbols")

proc allFunctions*(s: Scope): seq[FuncInfo] =
  for (_, v) in s.table.items(sort = true):
    if v.isFunc:
      result &= v.fimpls

proc allFunctions*(m: Module): seq[FuncInfo] =
  return m.moduleScope.allFunctions()

proc allParams*(m: Module): seq[SymbolInfo] =
  for (_, v) in m.moduleScope.table.items(sort = true):
    if v.pInfo != nil:
      result.add(v)


proc allVars*(s: Scope): seq[SymbolInfo] =
  for (_, v) in s.table.items(sort = true):
    if not v.isFunc:
      result.add(v)

proc calculateOffsets*(s: Scope, startOffset = 0) =
  if s.sized:
    return
  # For now, we just allocate everything 16 bytes on the stack,
  # regardless of type. Formal parameters get negative offsets.
  # We do this because for expedience we're going to store type info
  # with items we push, even when we don't have to (really we should
  # only need to be doing this in anything data dependent on a generic
  # parameter).
  if s == nil:
    return

  let symInfo = s.table.items(sort = true)
  var
    sz           = startOffset
    formalOffset = -1

  for (n, sym) in symInfo:
    if sym.isFunc:
      continue
    if sym.formal:
      sym.offset = formalOffset
      formalOffset -= 1
      continue

    sym.offset = sz
    sym.size   = 1
    sz        += 1

  s.scopeSize = sz

  for item in s.childScopes:
    # For subscopes, they start with the size of their parent scope, but
    # we consider the 'max' size of a scope to be what any subscope might
    # need, because that's the amount we're going to alloc on function
    # entry.
    let p = cast[pointer](item)
    item.calculateOffsets(sz)
    if item.scopeSize > s.scopeSize:
      s.scopeSize = item.scopeSize

  s.sized = true

proc addParent*(child, parent: Scope) =
  child.parent = parent
  parent.childScopes.add(child)

  if parent.fnScope:
    child.fnScope = true
