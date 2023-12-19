import parse, algorithm, strutils

template getTid*(s: SymbolInfo): TypeId =
  s.tid.followForwards()

proc initScope*(s: var Scope) =
  s = Scope()
  s.table.initDict()

proc newCompileCtx*(s: string, m: string): CompileCtx =
  result.module = m
  result.s      = newStringCursor(s)


  result.globalScope.initScope()
  result.moduleScope.initScope()
  result.usedAttrs.initScope()

proc lookupOrAdd*(ctx: var CompileCtx, scope: var Scope, n: string,
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
    sym = SymbolInfo(name: n, tid: tid, isFunc: isFunc)
    scope.table[n] = sym
    result = some(sym)

proc scopeDeclare*(ctx: var CompileCtx, scope: var Scope, n: string,
                   isfunc: bool, tid = TBottom, immutable = false):
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

proc resolveSection*(ctx: var CompileCtx, n: seq[string]): SectionSpec =
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

proc addAttrDef*(ctx: var CompileCtx, name: string, loc: IRNode,
                 tid = TBottom): Option[SymbolInfo] =
  var
    sym: SymbolInfo
    sec: SectionSpec
    parts = name.split(".")
    tid = tid


  if ctx.curSecSpec != nil:
    if parts.len() > 1:
      sec = ctx.resolveSection(parts[0 ..< 1])
    else:
      sec = ctx.curSecSpec

    if sec != nil and tid != TBottom:
      let expectedType = sec.typeFromSpec(parts[^1])
      tid = tid.unify(expectedType)
      if tid == TBottom:
        ctx.irError("TypeVsSpec", @[tid.toString(), expectedType.toString()])

  result = ctx.lookupOrAdd(ctx.usedAttrs, name, false, tid)


proc addVarDef*(ctx: var CompileCtx, name: string, loc: IRNode,
             tid = TBottom): Option[SymbolInfo] =
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
  ##
  ## While we get told when we're in an attribute scope lhs (the
  ## assignment operator is : or =), we do want to be intelligent when
  ## people fat-finger this (which leads to this function getting
  ## called instead of addAttrDef).
  ##
  ## To that end, if there's an attr spec available, we do not allow
  ## assigning to variable names if they would conflict with an
  ## attribute name (though we may add qualifiers like local:: and
  ## root:: to override this).
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

  var sym: SymbolInfo


  for scope in ctx.blockScopes:
    if scope.table.lookup(name).isSome():
      ctx.irError("LoopVarAssign")
      return

  if ctx.funcScope != nil:
    result = ctx.funcScope.table.lookup(name)

    if result.isSome():
      sym = result.get()

  if sym == nil:
    result = ctx.moduleScope.table.lookup(name)

    if result.isSome():
      sym = result.get()

  if sym == nil:
    result = ctx.globalScope.table.lookup(name)

    if result.isSome():
      sym = result.get()

      if sym.isFunc and ctx.funcScope != nil:
        # We can shadow this symbol in the local scope.
        sym = nil
      elif sym.isFunc:
        ctx.irError("AlreadyAVar")
      return

  if sym == nil:
    var scope = if ctx.funcScope != nil: ctx.funcScope else: ctx.moduleScope

    result = ctx.lookupOrAdd(scope, name, false, tid)
    if result.isNone():
      return
    else:
      sym = result.get()
  else:
    if sym.immutable:
      ctx.irError("Immutable")
      return
    if tid != TBottom:
      sym.tid = sym.tid.unify(tid)

  sym.defs.add(loc)

proc addDef*(ctx: var CompileCtx, name: string, loc: IRNode,
             tid = TBottom): Option[SymbolInfo] =
  if ctx.attrContext:
    return ctx.addAttrDef(name, loc, tid)
  else:
    return ctx.addVarDef(name, loc, tid)

proc addUse*(ctx: var CompileCtx, name: string, loc: IRNode,
             tid = TBottom): Option[SymbolInfo] =
  ## Note that, when we're in a `def` context, we know for sure
  ## whether or not we should be using an attribute, or a variable.
  ##
  ## However, in a use context, we may or may not be. If there's a
  ## dot in the name, then it's definitely an attribute context.
  ##
  ## Otherwise:
  ##
  ## 1. If something is explicitly declared in the local function or
  ## in the module scope, then we go with that.
  ##
  ## 2. If a attribute validation context exists and the name is
  ##    explicitly named, then we assume it's that.
  ##
  ## 3. If we've seen it on the LHS of an attr assign in the local
  ##    module, then okay, we'll go with it.
  ##
  ## 4. Otherwise, we put it in the 'unresolved' scope, which
  ##    will possibly get merged in with the global state, or
  ##    else will always go in the module scope.

  var sym: SymbolInfo

  for scope in ctx.blockScopes:
    result = scope.table.lookup(name)

    if result.isSome():
      sym = result.get()
      break

  if sym == nil and ctx.funcScope != nil:
    result = ctx.funcScope.table.lookup(name)
    if result.isSome():
      sym = result.get()

  if sym == nil:
    result = ctx.moduleScope.table.lookup(name)
    if result.isSome():
      sym = result.get()

  if sym == nil:
    result = ctx.globalScope.table.lookup(name)
    if result.isSome():
      sym = result.get()

      if sym.isFunc and ctx.funcScope != nil:
        # We can shadow this symbol in the local scope.
        sym = nil

      elif sym.isFunc:
        ctx.irError("AlreadyAVar")
        return

  if sym == nil:
    var scope = if ctx.funcScope != nil: ctx.funcScope else: ctx.moduleScope

    result = ctx.lookupOrAdd(scope, name, false, tid.followForwards())
    if result.isNone():
      return
    else:
      sym = result.get()
  elif tid != TBottom:
    discard sym.tid.followForwards().unify(tid.followForwards())

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
    hdr   = @[atom("Symbol"), atom("Type"), atom("Decl?"),
              atom("Const"), atom("fn?")]
    cells = @[hdr]
    contents = s.table.items()

  if contents.len() == 0:
    if title != "":
      return h4(title & ": Scope contains no contents.")
    else:
      return h4("Scope contains no contents.")

  contents.sort()
  for (name, sym) in contents:
    var
      cc     = fgColor("✗", "red")
      isFunc = fgColor("✗", "red")
    if sym.constValue.isSome():
      let box = sym.constValue.get()
      # TODO: This only works for base types.  Need to build CBox to Rope
      cc = em(rawBuiltinRepr(box.t, box.v))
    elif sym.immutable:
      cc = fgColor("✓ ", "atomiclime") + fgColor("(not set)", "yellow")
    if sym.fImpls.len() != 0:
      isFunc = fgColor("✓", "atomiclime")

    cells.add(@[name.atom(), sym.tid.followForwards().toRope(),
                sym.declaredType.isDeclaredRepr(), cc, isFunc])


  return quickTable(cells, title = title)
