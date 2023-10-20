import types, treecheck, options, tables, nimutils, eval, dollars


proc validateParameter*(state: ConfigState, param: ParameterInfo):
                      Option[string] =
  if param.value.isNone():
    return some("<red>error:</red> Must provide a valid value.")

  if param.validator.isSome():
    let
      boxOpt = state.sCall(param.validator.get(), @[param.value.get()])
      err    = unpack[string](boxOpt.get())

    if err != "":
      return some(err)

  return none(string)


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
    default = stylize("Default is: " & default, ensureNl = false)
  let
    short   = param.shortDoc.getOrElse("No description provided")
    long    = param.doc.getOrElse("")
    intro   = "Configuring: <jazzberry>" & param.name & "</jazzberry> -- " &
              "<i>" & short & "</i>\n" & long

  echo intro.stylizeMd()

  while true:
    if boxOpt.isSome():
      echo default
      print("Press [enter] to accept default, or enter a value: ")
    else:
      print("Please enter a value: ", ensureNl = false)

    let line = stdin.readLine()

    if line != "":
      try:
        boxOpt = some(line.parseConstLiteral(param.defaultType))
      except:
        echo(stylize(withColor("error: ", "red") & getCurrentExceptionMsg()))
        continue

    param.value = boxOpt

    let err = state.validateParameter(param)

    if err.isNone():
      break

    echo(stylize(err.get()))

proc basicConfigureParameters*(state:         ConfigState,
                               component:     ComponentInfo,
                               componentList: seq[ComponentInfo],
                               nextPrompt = "Press [enter] to continue."
                              ) =
  var shouldPause = false
  print("# Configuring Component: " & component.url)
  for subcomp in componentList:
    for name, param in subcomp.varParams:
      state.basicConfigureOneParam(subcomp, param)
      shouldPause = true

    for name, param in subcomp.attrParams:
      state.basicConfigureOneParam(subcomp, param)
      shouldPause = true
  print("# Finished configuration for " & component.url)
  if shouldPause:
    print(nextPrompt)
    discard stdin.readLine()
