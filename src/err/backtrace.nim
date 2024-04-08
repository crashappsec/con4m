import ".."/common

template getModName*(ctx: RuntimeState): string =
  ctx.curModule.modName & ".c4m"

template getLineNo*(ctx: RuntimeState): int =
  ctx.curModule.instructions[ctx.ip].lineNo

proc storageAddr*(ctx: RuntimeState, x: ZInstruction,
                     p: int64): ptr pointer =
  if x.moduleId == -1:
    result = addr ctx.stack[ctx.fp - (p * 2)]
  elif x.moduleId == -2:
    # Static data, no type info assumed.
    result = cast[ptr pointer](addr ctx.obj.staticData[p])
  else:
    result = addr ctx.moduleAllocations[x.moduleId][p * 2]

proc getSourceLoc*(ctx: RuntimeState): string =
  ## Decode the source location of the current runtime state from
  ## the current instruction.
  let line = ctx.getLineNo()
  if line != -1:
    return ctx.getModName() & " (line #" & $(ctx.getLineNo()) & ")"
  else:
    return ""

proc setColWidths(g: Grid) =
  var
    col0 = new_render_style()
    col1 = new_render_style()
    col2 = new_render_style()

  col0.set_absolute_size(17)
  col1.set_flex_size(2)
  col2.set_flex_size(5)

proc get_stack_trace*(ctx: RuntimeState): Grid {.exportc, cdecl.} =
  var cells: seq[seq[string]] = @[@["Caller module", "Line #",
                                   "Call target"]]

  for i in 1 ..< ctx.numFrames:
    var
      frame = ctx.frameInfo[i]
      row: seq[string]

    row.add(frame.callModule.modname)
    if i == ctx.numFrames - 1:
      row.add($(ctx.getLineNo()))
    else:
      row.add($(frame.calllineno))

    if frame.targetfunc == nil:
      row.add(frame.targetmodule.modname & ".__mod_run__")
    else:
      row.add(frame.targetmodule.modname & "." & frame.targetfunc.funcname)

    cells.add(row)

  let loc = ctx.getSourceLoc()
  if loc != "":
    result = cells.table(title =  "Stack trace",
                         caption = "Source location: " &
                              ctx.getSourceLoc())
  else:
    result = cells.table(title =  "Stack trace")

  result.setColWidths()
