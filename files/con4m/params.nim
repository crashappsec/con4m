import types, treecheck, options, tables, nimutils, eval, dollars


proc basicConfigureOneParam(state:     ConfigState,
                            component: ComponentInfo,
                            param:     ParameterInfo) =
  var
    boxOpt:  Option[Box]
    default = "<<no default>>"

  if param.value.isSome():
    boxOpt = param.value
  elif param.default.isSome():
    boxOpt = param.default
  elif param.defaultCb.isSome():
    boxOpt = state.sCall(param.defaultCb.get(), @[])

  if boxOpt.isSome():
    default = param.defaultType.oneArgToString(boxOpt.get())
  let
    short   = param.shortDoc.getOrElse("No description provided")
    long    = param.doc.getOrElse("")

  print("### Configuring: " & param.name & " -- " & short & "\n" & long)

  if boxOpt.isSome():
    print("Press [enter] to accept default, or enter a value: ")

  while true:
    let line = stdin.readLine()

    if line != "":
      try:
        boxOpt = some(line.parseConstLiteral(param.defaultType))
      except:
        print("<red>error:</red> " & getCurrentExceptionMsg())

    if boxOpt.isNone():
      print("<red>error:</red> Must enter a valid value.")
      continue
    elif line == "":
      print("<atomiclime>success:</atomiclime> Accepting the default value.")
    else:
      print("<atomiclime>success:</atomiclime> Value accepted.")

    param.value = boxOpt
    break

proc basicConfigureParameters*(state:         ConfigState,
                               component:     ComponentInfo,
                               componentList: seq[ComponentInfo]) =
  print("# Congiguring Component: " & component.url)
  for subcomp in componentList:
    for _, param in subcomp.varParams:
      state.basicConfigureOneParam(subcomp, param)
