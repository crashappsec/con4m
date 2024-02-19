# Whole program / cross-module checks.

import "."/cfg
export cfg

proc programErrBase(ctx: CompileCtx, code: string, sym: SymbolInfo,
                  extra: seq[string], ll: Con4mSeverity) =
  var
    module = "(module)"
    tok = Con4mToken(cursor: StringCursor())

  if sym != nil and sym.module != nil:
    module = sym.module.modname
  if sym != nil:
    if sym.defs.len() != 0:
      tok = sym.defs[0].parseNode.token
    elif sym.uses.len() != 0:
      tok = sym.uses[0].parseNode.token
    else:
      if sym.declNode != nil:
        tok = sym.declNode.token

  ctx.errors.baseError(code, modname = module,
                       tok = tok,
                       phase = ErrIrGen, severity = ll,
                       extra = extra)

template programError(ctx: CompileCtx, code: string, sym: SymbolInfo,
                  extra: seq[string] = @[]) =
  ctx.programErrBase(code, sym, extra, LlFatal)

template programWarn(ctx: CompileCtx, code: string, sym: SymbolInfo,
                  extra: seq[string] = @[]) =
  ctx.programErrBase(code, sym, extra, LlWarn)

proc getAllUserScopes(ctx: CompileCtx, attrs: bool): seq[Scope] =
  result.add(ctx.globalScope)

  for (_, m) in ctx.modules.items():
    if m.system:
      continue

    if m.moduleScope notin result:
      result.add(m.moduleScope)
    for (_, sym) in m.moduleScope.table.items():
      if sym.isFunc:
        for fimpl in sym.fimpls:
          if fimpl.fnScope != nil and fimpl.fnScope notin result:
            result.add(fimpl.fnScope)

proc defWoUseCheck(ctx: CompileCtx) =
  for scope in ctx.getAllUserScopes(false):
    for (name, sym) in scope.table.items():
      if sym.isFunc:
        continue
      if sym.formal:
        if sym.uses.len() != 0 or sym.defs.len() > 0:
          continue
      if sym.uses.len() != 0:
        if sym.declNode != nil:
          continue
      if name == "result":
        continue
      if name.startswith("$"):
        continue
      if sym.uses.len() != 0:
        continue
      ctx.programWarn("DefWoUse", sym, @[name])

proc useWoDefCheck(ctx: CompileCtx) =
  for scope in ctx.getAllUserScopes(false):
    for (name, sym) in scope.table.items():
      if sym.isFunc or sym.formal:
        continue
      if sym.uses.len() != 0 and sym.defs.len() == 0:
        ctx.programError("UseWoDef", sym, @[name])

proc constAssignmentCheck(ctx: CompileCtx) =
  for scope in ctx.getAllUserScopes(false):
    for (name, sym) in scope.table.items():
      if sym.immutable and not sym.haveConst:
        ctx.programError("ConstNotSet", sym, @[name])

proc wholeProgramChecks*(ctx: CompileCtx) =
  ctx.defWoUseCheck()
  ctx.constAssignmentCheck()
  ctx.useWoDefCheck()
