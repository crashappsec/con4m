import types, treecheck, typecheck, options, tables, nimutils, eval, dollars


proc validateParameter*(state: ConfigState, param: ParameterInfo):
                      Option[Rope] =
  if param.value.isNone():
    return some(color("error: ", "red") + text("Must provide a valid value."))

  if param.validator.isSome():
    let
      boxOpt = state.sCall(param.validator.get(), @[param.value.get()])
      err    = unpack[string](boxOpt.get())

    if err != "":
      return some(text(err))

  return none(Rope)


proc basicConfigureOneParam(state:     ConfigState,
                            component: ComponentInfo,
                            param:     ParameterInfo) =
  var
    boxOpt:  Option[Box]
    default = text("<<no default>>")

  if param.value.isSome():
    boxOpt = param.value
  elif param.default.isSome():
    boxOpt = param.default
  elif param.defaultCb.isSome():
    boxOpt = state.sCall(param.defaultCb.get(), @[])

  if boxOpt.isSome():
    default = h2(atom("Default is: ") +
                  fgColor(param.defaultType.oneArgToString(boxOpt.get()),
                           c0Text))
  let
    short   = param.shortDoc.getOrElse("No description provided")
    long    = param.doc.getOrElse("")

  print(h2(atom("Configuring variable: ") + em(param.name) +
           text(" -- ") + italic(short)) + markdown(long))

  while true:
    if boxOpt.isSome():
      print(default)
      print("Press [enter] to accept default, or enter a value: ",
            ensureNl = false)
    else:
      print("Please enter a value: ", ensureNl = false)

    let line = stdin.readLine()

    if line != "":
      try:
        boxOpt = some(line.parseConstLiteral(param.defaultType))
      except:
        print (color("error: ", "red") + text(getCurrentExceptionMsg()))
        continue

    let box = boxOpt.get()

    if not param.defaultType.unify(floatType).isBottom() and box.kind == MkInt:
      let f = float(unpack[int](box))
      boxOpt = some(pack(f))

    param.value = boxOpt

    let err = state.validateParameter(param)

    if err.isNone():
      break

    print(err.get())

proc basicConfigureParameters*(state:         ConfigState,
                               component:     ComponentInfo,
                               componentList: seq[ComponentInfo],
                               nextPrompt = "Press [enter] to continue.") =
  var shouldPause = false
  print(h1("Configuring Component: " & component.url))
  for subcomp in componentList:
    for name, param in subcomp.varParams:
      state.basicConfigureOneParam(subcomp, param)
      shouldPause = true

    for name, param in subcomp.attrParams:
      state.basicConfigureOneParam(subcomp, param)
      shouldPause = true
  print(h2("Finished configuration for " & component.url))
  if shouldPause:
    print(nextPrompt)
    discard stdin.readLine()
