import types, run, st

proc runConfig*(contents: cstring, fname: cstring): cstring {.exportc.} =
  try:
    let (ctx, res) = firstRun($(contents), $(fname))
    if not res:
      return "Unknown error"
    return ctx.attrs.scopeToJson()
  except:
    return getCurrentExceptionMsg()
