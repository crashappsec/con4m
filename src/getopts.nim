## This is a partial redo of nimutils argParse capability, but with
## more functionality and the specification of commands, flags and
## options checked via a c42 spec.
##
## If you call the API directly, and didn't do the input checking,
## results are undefined :)

import std/[os, sequtils]
import "."/[common, attrstore, compile, codegen, vm, vm_func]

const errNoArg = "Expected a command but didn't find one"

proc getValue*(f: FlagSpec): pointer =
  case f.kind
  of afPair:
    return cast[pointer](f.boolValue[f.finalFlagIx])
  of afChoice:
    let x = cast[XList[Rich]](f.selected[f.finalFlagIx])
    return cast[pointer](x[0])
  of afMultiChoice:
    return cast[pointer](f.selected[f.finalFlagIx])
  of afStrArg:
    return cast[pointer](f.strVal[f.finalFlagIx])
  of afMultiArg:
    return cast[pointer](f.strArrVal[f.finalFlagIx])

proc flagSpecEq(f1, f2: FlagSpec): bool =
  if f1 == f2:
    return true   # They're literally the same ref
  elif f1.kind != f2.kind:
    return false
  elif f1.reportingName == f2.reportingName:
    return true
  else:
    return false

proc newSpecObj*(reportingName: Rich         = rich"",
                 allNames: Xlist[Rich]       = nil,
                 minArgs                     = 0,
                 maxArgs                     = 0,
                 subOptional                 = false,
                 unknownFlagsOk              = false,
                 dockerSingleArg             = true,
                 noFlags                     = false,
                 sdoc                        = rich"",
                 ldoc                        = rich"",
                 argName                     = rich"",
                 callback                    = none(ptr ZCallBack),
                 parent                      = CommandSpec(nil),
                 noColon                     = false,
                 noSpace                     = false): CommandSpec =
  if noFlags and unknownFlagsOk:
    raise newException(ValueError, "Can't have noFlags and unknownFlagsOk")
  result = CommandSpec(reportingName:     reportingName,
                       allNames:          allNames,
                       minArgs:           minArgs,
                       maxArgs:           maxArgs,
                       subOptional:       subOptional,
                       unknownFlagsOk:    unknownFlagsOk,
                       dockerSingleArg:   dockerSingleArg,
                       noFlags:           noFlags,
                       sdoc:              sdoc,
                       ldoc:              ldoc,
                       argName:           argName,
                       callback:          callback,
                       parent:            parent,
                       noColon:           noColon,
                       noSpace:           noSpace,
                       autoHelp:          false,
                       finishedComputing: false)
  initDict(result.commands)
  initDict(result.flags)
  initDict(result.allPossibleFlags)

proc addCommand*(spec:            CommandSpec,
                 name:            Rich,
                 aliases:         Xlist[Rich]           = nil,
                 subOptional:     bool                  = false,
                 unknownFlagsOk:  bool                  = false,
                 noFlags:         bool                  = false,
                 dockerSingleArg: bool                  = false,
                 sdoc:            Rich                  = nil,
                 ldoc:            Rich                  = nil,
                 argName:         Rich                  = rich"",
                 callback:        Option[ptr ZCallback] = none(ptr ZCallback),
                 noColon:         bool                  = false,
                 noSpace:         bool                  = false):
                   CommandSpec {.discardable.} =
  ## Creates a command under the top-level argument parsing spec,
  ## or a sub-command under some other command.
  ## The `name` field is the 'official' name of the command, which
  ## will be used in referencing the command programatically, and
  ## when producing error messages.
  ##
  ## The values in `aliases` can be used at the command line in
  ##
  ## place of the official name.
  ##
  ## If there are sub-commands, then the `subOptional` flag indicates
  ## whether it's okay for the sub-command to not be provided.
  ##
  ## If `unknownFlagsOk` is provided, then you can still add flags
  ## for that section, but if the user does provide flags that wouldn't
  ## be valid in any section, then they will still be accepted.  In
  ## this mode, unknown flags are put into the command arguments.
  ##
  ## If `noFlags` is provided, then the rest of the input will be
  ## treated as arguments, even if they start with dashes.  If this
  ## flag is set, unknownFlagsOk cannot be set, and there may not
  ## be further sub-commands.
  ##
  ## Note that, if you have sub-commands that are semantically the
  ## same, you still should NOT re-use objects. The algorithm for
  ## validating flags assumes that each command object to be unique,
  ## and you could definitely end up accepting invalid flags.
  result = newSpecObj(reportingName   = name,
                      allNames        = aliases,
                      subOptional     = subOptional,
                      unknownFlagsOk  = unknownFlagsOk,
                      dockerSingleArg = dockerSingleArg,
                      noFlags         = noFlags,
                      noColon         = noColon,
                      noSpace         = noSpace,
                      sdoc            = sdoc,
                      ldoc            = ldoc,
                      argName         = argName,
                      callback        = callback,
                      parent          = spec)

  result.attrtop = spec.attrtop

  if name notin result.allNames: result.allNames.add(name)
  for oneName in result.allNames:
    if oneName in spec.commands:
      raise newException(ValueError, "Duplicate command: " & name.toNimStr())
    spec.commands[oneName] = result

proc addArgs*(cmd:      CommandSpec,
              min:      int = 0,
              max:      int = high(int)): CommandSpec {.discardable.} =
  ## Adds an argument specification to a CommandSpec.  Without adding
  ## it, arguments won't be allowed, only flags.
  ##
  ## This returns the command spec object passed in, so that you can
  ## chain multiple calls to addArgs / flag add calls.

  result = cmd
  if min < 0 or max < 0 or min > max:
    raise newException(ValueError, "Invalid arguments")

  cmd.minArgs     = min
  cmd.maxArgs     = max

proc newFlag(cmd:             CommandSpec,
             kind:            ArgFlagKind,
             reportingName:   Rich,
             clOk:            bool,
             recognizedNames: XList[Rich],
             sdoc:            Rich = nil,
             ldoc:            Rich = nil,
             callback:        Option[ptr ZCallback] = none(ptr ZCallback),
             toSet:           Rich = rich"",
             optArg:          bool = false,
            ): FlagSpec =
  if cmd.noFlags:
    raise newException(ValueError,
                       "Cannot add a flag for a spec where noFlags is true")

  result = FlagSpec(reportingName: reportingName, kind: kind, clobberOk: clOk,
                    recognizedNames: recognizedNames, sdoc: sdoc,
                    ldoc: ldoc, callback: callback, fieldToSet: toSet,
                    noColon: cmd.noColon, noSpace: cmd.noSpace,
                    argIsOptional: optArg)
  cmd.flags[reportingName] = result

proc addChoiceFlag*(cmd:             CommandSpec,
                    reportingName:   Rich,
                    recognizedNames: XList[Rich],
                    choices:         XList[Rich],
                    flagPerChoice:   bool                = false,
                    multi:           bool                = false,
                    clobberOk:       bool                = false,
                    sdoc:            Rich                = nil,
                    ldoc:            Rich                = nil,
                    callback:      Option[ptr ZCallback] = none(ptr ZCallback),
                    toSet:           Rich                = rich""):
                      FlagSpec {.discardable.} =
  ## This creates a flag for `cmd` that requires a string argument if
  ## provided, but the string argument must be from a fixed set of
  ## choices, as specified in the `choices` field.
  ##
  ## If `flagPerChoice` is provided, then we add a yes/no flag for
  ## each choice, which, on the command-line, acts as a 'boolean'.
  ## But, the value will be reflected in this field, instead.
  ##
  ## For instance, if you add a '--log-level' choice flag with values
  ## of ['info', 'warn', 'error'], then these two things would be
  ## equal:
  ##
  ## --log-level= warn
  ##
  ## --warn
  ##
  ## And you would still check the value after parsing via the name
  ## 'log-level'.
  ##
  ## The `name`, `aliases` and `clobberOk` fields work as with other
  ## flag types.

  let kind            = if multi: afMultiChoice else: afChoice
  var flag            = newFlag(cmd, kind, reportingName, clobberOk,
                                recognizedNames, sdoc, ldoc, callback, toSet)
  flag.choices        = choices

  if flagPerChoice:
    for item in choices:
      let itemName = rich"->" + item # -> for forwards...
      var oneFlag = newFlag(cmd, afPair, itemName, clobberOk, toXList(@[item]),
                            sdoc, ldoc, callback)
      oneFlag.positiveNames = toXList(@[item])
      oneFlag.linkedChoice  = some(flag)

  result          = flag
  initDict[int, XList[Rich]](result.selected)

proc addYesNoFlag*(cmd:           CommandSpec,
                   reportingName: Rich,
                   yesValues:     XList[Rich],
                   noValues:      XList[Rich]           = nil,
                   clobberOk:     bool                  = false,
                   sdoc:          Rich                  = nil,
                   ldoc:          Rich                  = nil,
                   callback:      Option[ptr ZCallback] = none(ptr ZCallback),
                   toSet:         Rich                  = rich""):
                     FlagSpec {.discardable.} =

  var both   = yesValues & noValues
  var ynFlag = newFlag(cmd, afPair, reportingName, clobberOk, both,
                       sdoc, ldoc, callback, toSet)

  ynFlag.positiveNames = yesValues
  ynFlag.negativeNames = noValues
  initDict(ynFlag.boolValue)

  if reportingName notin yesValues and reportingName notin noValues:
    let c      = cmd.addChoiceFlag(rich"->" + reportingName,
                             recognizedNames = toXList(@[reportingName]),
                             choices = both, clobberOk = clobberOk)
    c.linkedYN = some(ynFlag)

  result = ynFlag

proc addFlagWithArg*(cmd:             CommandSpec,
                     reportingName:   Rich,
                     recognizedNames: Xlist[Rich]       = nil,
                     multi:           bool              = false,
                     clobberOk:       bool              = false,
                     sdoc:            Rich              = nil,
                     ldoc:            Rich              = nil,
                     callback:    Option[ptr ZCallback] = none(ptr ZCallback),
                     toSet:           Rich            = rich"",
                     optArg:          bool              = false):
                       FlagSpec {.discardable.} =
  ## This simply adds a flag that takes a required string argument, or,
  ## in the case of multi-args, an array of string arguments.  The arguments
  ## are identical in semantics as for other flag types.

  let kind  = if multi: afMultiArg else: afStrArg
  result = newFlag(cmd, kind, reportingName, clobberOk, recognizedNames,
                   sdoc, ldoc, callback, toSet, optArg)
  if kind == afMultiArg:
    initDict(result.strArrVal)
  else:
    initDict(result.strVal)

template argpError(msg: Rich) =
  var fullError = msg.toNimStr()

  if ctx.res.command.len() != 0:
    fullError = "When parsing command '" & ctx.res.command &
      "': " & msg.toNimStr()

  raise newException(ValueError, fullError)

template argpError(flagName: Rich, msg: string) =
  argpError(r("[red]--" & flagName.toNimStr() & ": [/]" & msg))

proc validateOneFlag(ctx:     var ParseCtx,
                     name:    Rich,
                     inspec:  FlagSpec,
                     foundArg = none(Rich)) =
  var
    argCrap = foundArg
    spec    = inspec
    flagSep = if not spec.noColon: [':', '='] else: ['=', char(0)]

  if ctx.i < len(ctx.args) and ctx.args[ctx.i][0] in flagSep:
    if argCrap.isNone() and not spec.noSpace:
      argCrap = some(r(unicode.strip(ctx.args[ctx.i][1 .. ^1])))
      ctx.i = ctx.i + 1
      if argCrap.get() == rich"":
        if ctx.i < len(ctx.args):
          argCrap = some(r(ctx.args[ctx.i]))
          ctx.i = ctx.i + 1
        else:
          argpError(name, "requires an argument.")

  if spec.kind == afPair:
    if argCrap.isSome():
      argpError(name, "takes no argument.")
    if spec.linkedChoice.isSome():
      spec    = spec.linkedChoice.get()
      argCrap = some(name)

  elif argCrap.isNone():
    if not spec.argIsOptional:
      # Here we require an argument, and we didn't find a ':' or '=',
      # so we just assume it's the next word, unless we see a dash
      # followed by anything (otherwise, we'll assume the dash itself
      # is the argument, since this often would mean 'stdin')
      if ctx.i == len(ctx.args) or (ctx.args[ctx.i][0] == '-' and
                                    len(ctx.args[ctx.i]) > 1):
        argpError(name, "requires an argument.")
      if spec.noSpace:
        argpError(name, "requires an argument.")
      argCrap = some(r(unicode.strip(ctx.args[ctx.i])))
      ctx.i  = ctx.i + 1
    else:
      # When arguments are optional and we don't see them (or at least a
      # = or :), we set them to ""
      argCrap = some(rich"")

  if spec.kind notin [afMultiChoice, afMultiArg] and
     not spec.clobberOk and spec.reportingName.toNimStr() in ctx.res.flags:
    argpError(name, "redundant flag not allowed")

  case spec.kind
  of afPair:
    if   name in spec.positiveNames: spec.boolValue[ctx.parseId] = true
    elif name in spec.negativeNames: spec.boolValue[ctx.parseId] = false
    else: raise newException(ValueError, "Reached unreachable code")
  of afChoice:
    let arg = argCrap.get()
    if arg notin spec.choices:
      argpError(name, "Invalid choice: '" & arg.toNimStr() & "'")
    if spec.linkedYN.isSome():
      spec = spec.linkedYN.get()
      if not spec.clobberOk and spec.reportingName.toNimStr() in ctx.res.flags:
        argpError(name, "redundant flag not allowed")
      if arg in spec.positiveNames:   spec.boolValue[ctx.parseId] = true
      elif arg in spec.negativeNames: spec.boolValue[ctx.parseId] = false
      else: raise newException(ValueError, "Reached unreachable code")
    else:
      spec.selected[ctx.parseId] = toXList(@[arg])
  of afMultiChoice:
      let arg = argCrap.get()
      if arg notin spec.choices:
        argpError(name, "Invalid choice: '" & arg.toNimStr() & "'")
      if ctx.parseId notin spec.selected:
        spec.selected[ctx.parseId] = toXList(@[arg])
      elif arg notin spec.selected[ctx.parseId]:
        var l = spec.selected[ctx.parseId]
        spec.selected[ctx.parseId] = l
  of afStrArg:
    spec.strVal[ctx.parseId] = argCrap.get()
  of afMultiArg:
    var parts = argCrap.get().toNimStr()
    if len(parts) != 0 and parts[^1] == ',':
      while ctx.i != len(ctx.args) and ctx.args[ctx.i][0] != '-':
        parts = parts & unicode.strip(ctx.args[ctx.i])
        ctx.i = ctx.i + 1
    if len(parts) != 0 and parts[^1] == ',': parts = parts[0 ..< ^1]
    if ctx.parseId notin spec.strArrVal:
      spec.strArrVal[ctx.parseId] = toRichXList(parts.split(","))
    else:
      var l = spec.strArrVal[ctx.parseId]
      l &= toRichXList(parts.split(","))
      spec.strArrVal[ctx.parseId] = l

  ctx.res.flags[spec.reportingName.toNimStr()] = spec

proc parseOneFlag(ctx: var ParseCtx, spec: CommandSpec, validFlags: auto) =
  var
    orig        = ctx.args[ctx.i]
    cur         = orig[1 .. ^1]
    singleDash  = true
    definiteArg = none(Rich)

  ctx.i = ctx.i + 1

  # I really want to change this to a while, just because I've
  # accidentally done three dashes once or twice.  But I'm going to
  # assume I'm in the minority and there's some common use case
  # where --- should be treated as an argument not a flag?
  if cur[0] == '-':
    cur        = cur[1 .. ^1]
    singleDash = false

  var
    colonix = if spec.noColon: -1 else: cur.find(':')
    eqix    = cur.find('=')
    theIx   = colonix

  if theIx == -1:
    theIx = eqIx
  else:
    if eqIx != -1 and eqIx < theIx:
      theIx = eqIx
  if theIx != -1:
    let rest    = unicode.strip(cur[theIx+1 .. ^1])
    cur         = unicode.strip(cur[0 ..< theIx])

    if len(rest) != 0:
      definiteArg = some(r(rest))
    elif ctx.i != len(ctx.args):
      definiteArg = some(r(ctx.args[ctx.i]))
      ctx.i = ctx.i + 1

  if r(cur) in validFlags:
    ctx.validateOneFlag(r(cur), validFlags[r(cur)], definiteArg)
  elif not singleDash:
    if spec.unknownFlagsOk:
      ctx.curArgs.add(orig)
    else:
      argpError(r(cur), "Invalid flag")
  else:
    if spec.dockerSingleArg and len(cur) > 1 and r($(cur[0])) in validFlags:
      let flag = r($cur[0])
      let n = validFlags[flag]
      ctx.validateOneFlag(flag, n, some(r(cur[1 .. ^1])))
    else:
      # Single-dash flags bunched together cannot have arguments, unless
      # unknownFlagsOk is on.
      if definiteArg.isSome() and not spec.unknownFlagsOk:
        argpError(r(cur), "Invalid flag")
      if spec.unknownFlagsOk:
        ctx.curArgs.add(orig)
      else:
        for i, c in cur:
          let oneCharFlag = r($(c))
          if oneCharFlag in validFlags:
            ctx.validateOneFlag(oneCharFlag, validFlags[oneCharFlag])
          elif spec.unknownFlagsOk: continue
          elif i == 0: argpError(r(cur), "Invalid flag")
          else:
            argpError(r(cur), "Couldn't process all characters as flags")


proc buildValidFlags(inSpec: Dict[Rich, FlagSpec]):
                    Dict[Rich, FlagSpec] =
  initDict(result)

  for (reportingName, f) in inSpec.items(sort = true):
    for name in f.recognizedNames:
      result[name] = f

proc parseCmd(ctx: var ParseCtx, spec: CommandSpec) =
  # If we are here, we know we're parsing for the spec passed in; it matched.
  # We accept that arguments and flags might be intertwined. We basically
  # will scan till we hit the end or hit a valid command that isn't
  # part of a flag argument.
  #
  # Then, we validate the number of arguments against the spec, handle
  # recursing if there's a sub-command, and decide if we're allowed to
  # finish if we have no more arguments to parse.
  var lookingForFlags = if spec.noFlags: false
                        else:            true

  ctx.curArgs = @[]

  let validFlags = spec.allPossibleFlags.buildValidFlags()

  # Check that any flags we happened to accept in a parent context
  # (because we were not sure what the exact sub-command would be),
  # are still valid now that we have more info about our subcommand.
  for (k, _) in ctx.res.flags.items(sort = true):
    if r(k) notin spec.allPossibleFlags:
      argpError(r(k), "Not a valid flag for this command.")

  while ctx.i != len(ctx.args):
    let cur = ctx.args[ctx.i]
    # If len is 1, we pass it through, usually means 'use stdout'
    if lookingForFlags and len(cur) > 1 and cur[0] == '-':
      if cur == "--":
        lookingForFlags = false
        ctx.i           = ctx.i + 1
        continue
      try:
        ctx.parseOneFlag(spec, validFlags)
      except:
        # If we get an error when parsing a flag, but we don't have a
        # top-level command yet, we're going to scan the whole string
        # looking for any word that matches. If we find one, we'll
        # assume the intent was to use that command, but that the
        # error was with a flag.
        if ctx.foundCmd == false:
          while ctx.i != len(ctx.args):
            let cur = ctx.args[ctx.i]
            if r(cur) in spec.commands:
              ctx.foundCmd = true
              break
            else:
              ctx.i = ctx.i + 1
        raise # Reraise.
      continue

    if r(cur) in spec.commands:
      ctx.foundCmd = true
      ctx.i = ctx.i + 1
      if len(ctx.curArgs) < spec.minArgs:
        argpError(r("Too few arguments for command " & cur &
                  "(expected " & $(spec.minArgs) & ")"))
      if len(ctx.curArgs) > spec.maxArgs:
        argpError(r("Too many arguments provided for command " & cur &
          " (max = " & ($(spec.maxArgs) & ")")))
      ctx.res.args[ctx.res.command] = toRichXList(ctx.curArgs)
      let nextSpec = spec.commands[r(cur)]
      if ctx.res.command != "":
        ctx.res.command &= "." & nextSpec.reportingName.toNimStr()
      else:
        ctx.res.command             = nextSpec.reportingName.toNimStr()
      ctx.res.args[ctx.res.command] = toRichXList(ctx.curArgs)
      ctx.parseCmd(nextSpec)
      return

    ctx.curArgs.add(ctx.args[ctx.i])
    ctx.i = ctx.i + 1

  # If we exited the loop, we need to make sure the parse ended up in
  # a valid final state.
  if len(spec.commands.keys()) != 0 and not spec.subOptional:
    argpError(r(errNoArg))
  if len(ctx.curArgs) < spec.minArgs:
    argpError(r("Too few arguments (expected " & $(spec.minArgs) & ")"))
  if len(ctx.curArgs) > spec.maxArgs:
    argpError(r("Too many arguments provided (max = " & $(spec.maxArgs) & ")"))
  ctx.res.args[ctx.res.command] = toRichXList(ctx.curArgs)
  ctx.finalCmd = spec

proc computePossibleFlags(spec: CommandSpec) =
  # Because we want to allow for flags for commands being passed to us
  # before we know whether they're valid (e.g., in a subcommand), we are
  # going to keep multiple flag states, one for each possible combo of
  # subcommands. To do this, we will flatten the tree of possible
  # subcommands, and then for each tree, we will compute all flags we
  # might see.
  #
  # The top of the tree will have all possible flags, but as we descend
  # we need to keep re-checking to see if we accepted flags that we
  # actually shouldn't have accepted.
  #
  # Note that we do not allow flag conflicts where the flag specs are
  # not FULLY compatible.  And, we do not allow subcommands to
  # re-define a flag that is defined already by a higher-level command.
  #
  # Note that, as we parse, we will accept flags we MIGHT smack down
  # later, depending on the command. We will validate what we've accepted
  # so far every time we enter a new subcommand.
  if spec.finishedComputing:
    return
  if spec.parent != nil:
    let parentFlags = spec.parent.allPossibleFlags
    for (k, v) in parentFlags.items(sort = true):
      spec.allPossibleFlags[k] = v
  for (k, v) in spec.flags.items(sort = true):
    if k in spec.allPossibleFlags:
      raise newException(ValueError, "When checking flag '" & k.toNimStr() &
        "', In section '" & spec.reportingName.toNimStr() &
        "' -- command flag names cannot " &
        "conflict with parent flag names or top-level flag names." &
        "This is because we want to make sure users don't have to worry " &
        "about getting flag position right whenever possible."
      )
    spec.allPossibleFlags[k] = v

  var flagsToAdd: Dict[string, FlagSpec]

  initDict(flagsToAdd)

  for (_, kid) in spec.commands.items(sort = true):
    kid.computePossibleFlags()
    for (k, v) in kid.allPossibleFlags.items(sort = true):
      if k in spec.allPossibleFlags:
        continue
      if k.toNimStr() notin flagsToAdd:
        flagsToAdd[k.toNimStr()] = v
        continue
      if not flagSpecEq(flagsToAdd[k.toNimStr()], v):
        raise newException(ValueError, "Sub-commands with flags of the " &
          "same name must have identical specifications (flag name: " &
          k.toNimStr() & ")")
  for (k, v) in flagsToAdd.items(sort = true):
    spec.allPossibleFlags[r(k)] = v
  spec.finishedComputing = true

var parseId = 0
proc parseOne(ctx: var ParseCtx, spec: CommandSpec) =
  ctx.i       = 0
  ctx.res     = ArgResult(parseCtx: ctx)
  ctx.parseId = parseId
  parseId     = parseId + 1

  initDict(ctx.res.flags)
  initDict(ctx.res.args)

  ctx.parseCmd(spec)

proc ambiguousParse*(spec:          CommandSpec,
                     inargs:        openarray[string] = [],
                     defaultCmd:    Option[string]    = some("")):
                       seq[ArgResult] =
  ## This parse function accepts multiple parses, if a parse is
  ## ambiguous.
  ##
  ## First, it attempts to parse `inargs` as-is, based on the
  ## specification passed in `spec`.  If that fails because there was
  ## no command provided, what happens is based on the value of the
  ## `defaultCmd` field-- if it's none(string), then no further action
  ## is taken.  If there's a default command provided, it's re-parsed
  ## with that default command.
  ##
  ## However, you provide "" as the default command (i.e., some("")),
  ## then this will try all possible commands and return any that
  ## successfully parse.
  ##
  ## If `inargs` is not provided, it is taken from the system-provided
  ## arguments.  In nim, this is commandLineParams(), but would be
  ## argv[1 .. ] elsewhere.

  if defaultCmd.isSome() and spec.subOptional:
    raise newException(ValueError,
             "Can't have a default command when commands aren't required")
  var
    validParses   = seq[ParseCtx](@[])
    firstError    = ""
    args          = if len(inargs) != 0: inargs.toSeq()
                    else:                commandLineParams()

  # First, try to see if no inferencing results in a full parse
  spec.computePossibleFlags()

  var ctx = ParseCtx(args: args)

  try:
    ctx.parseOne(spec)
    return @[ctx.res]
  except:
    firstError = getCurrentExceptionMsg()
    if ctx.foundCmd or defaultCmd.isNone():
      raise
    # else, ignore.

  let default = defaultCmd.get()
  if default != "":
    try:    return spec.ambiguousParse(@[default] & args, none(string))
    except: firstError = getCurrentExceptionMsg()

  for (cmd, ss) in spec.commands.items(sort = true):
    if ss.reportingName != cmd: continue
    var ctx = ParseCtx(args: @[cmd.toNimStr()] & args)
    try:
      ctx.parseOne(spec)
      validParses.add(ctx)
    except:
      discard

  result = @[]
  for item in validParses: result.add(item.res)

  if len(result) == 0: raise newException(ValueError, firstError)

proc parse*(spec:       CommandSpec,
            inargs:     openarray[string] = [],
            defaultCmd: Option[string]    = none(string)): ArgResult =
  ## This parses the command line specified via `inargs` as-is using
  ## the `spec` for validation, and if that parse fails because no
  ## command was provided, then tries a single default command, if it
  ## is provided.
  ##
  ## If `inargs` is not provided, it is taken from the system-provided
  ## arguments.  In nim, this is commandLineParams(), but would be
  ## argv[1 .. ] elsewhere.
  ##
  ## The return value of type ArgResult can have its fields queried
  ## directly, or you can use getBoolValue(), getValue(), getCommand()
  ## and getArgs() to access the results.

  let allParses = spec.ambiguousParse(inargs, defaultCmd)
  if len(allParses) != 1:
    raise newException(ValueError, "Ambiguous arguments: please provide an " &
                                   "explicit command name")
  result = allParses[0]

type LoadInfo = ref object
  runtime:        RuntimeState
  base_path:      string
  defaultCmd:     Option[string]
  defaultYesPref: seq[string]
  defaultNoPref:  seq[string]
  showDocOnErr:   bool
  addHelpCmds:    bool

template u2d(s: Rich): Rich = r(s.toNimStr().replace("_", "-"))

proc loadYn(cmdObj: CommandSpec, info: LoadInfo) =
  let
    rt = info.runtime

  for flagname in rt.get_subsections(info.base_path):
    let
      p          = info.base_path & flagname.toNimStr() & "."
      realName   = u2d(flagname).toNimStr()
      yesAliases = lookup[XList[Rich]](rt, p & "yes_aliases").get()
      noAliases  = lookup[XList[Rich]](rt, p & "no_aliases").get()
      yesPrefOpt = lookup[XList[Rich]](rt, p & "yes_prefixes")
      noPrefOpt  = lookup[XList[Rich]](rt, p & "no_prefixes")
      cb         = lookup[ptr ZCallback](rt, p & "callback")
      fieldToSet = lookup[Rich](rt, (p & "field_to_set")).get(nil).toNimStr()
      sdoc       = rt.get_short_doc(p)
      ldoc       = rt.get_long_doc(p)


    var
      yesPref: seq[string]
      noPref:  seq[string]

    if yesPrefOpt.isSome():
      yesPref = yesPrefOpt.get().toSeqStr()
    else:
      yesPref = info.defaultYesPref

    if noPrefOpt.isSome():
      noPref = noPrefOpt.get().toSeqStr()
    else:
      noPref = info.defaultNoPref

    var
      yesNames = yesAliases.toSeqStr()
      noNames  = noAliases.toSeqStr()

    if len(yesPref) == 0:
      yesNames.add(realName)
    else:
      for prefix in yesPref:
        if prefix.endswith("-"): yesNames.add(prefix & realName)
        else:                    yesNames.add(prefix & "-" & realName)
    for prefix in noPref:
      if prefix.endswith("-"): noNames.add(prefix & realName)
      else:                    noNames.add(prefix & "-" & realName)

    cmdObj.addYesNoFlag(r(realName), toRichXlist(yesNames),
                        toRichXlist(noNames), false, sdoc,
                        ldoc, cb, r(fieldToSet))

proc loadHelps(cmdObj: CommandSpec, info: LoadInfo) =
  let
    rt    = info.runtime
    names = lookup[XList[Rich]](rt, info.base_path & "names").get()
    sdoc  = rt.get_short_doc(info.base_path)
    ldoc  = rt.get_long_doc(info.base_path)

  cmdObj.addYesNoFlag(r("help"), names, nil, false, sdoc, ldoc)

proc loadChoices(cmdObj: CommandSpec, info: LoadInfo) =
  var
    rt = info.runtime

  for flagname in rt.get_subsections(info.base_path):
    let
      p          = info.base_path & flagname.toNimStr() & "."
      realName   = u2d(flagname)
      aliases    = lookup[XList[Rich]](rt, p & "aliases").get()
      choices    = lookup[XList[Rich]](rt, p & "choices").get()
      addFlags   = lookup[bool](rt, p & "add_choice_flags").get()
      sdoc       = rt.get_short_doc(p)
      ldoc       = rt.get_long_doc(p)
      cb         = lookup[ptr ZCallback](rt, p & "callback")
      fieldToSet = lookup[Rich](rt, p & "field_to_set").get(nil)

    var allNames = aliases & toXlist(@[realName])

    cmdObj.addChoiceFlag(realName, allNames, choices, addFlags, false,
                         false, sdoc, ldoc, cb, fieldToSet)

proc loadMChoices(cmdObj: CommandSpec, info: LoadInfo) =
  var
    rt = info.runtime

  for flagname in rt.get_subsections(info.base_path):
    let
      p          = info.base_path & flagname.toNimStr() & "."
      realName   = u2d(flagname)
      aliases    = lookup[XList[Rich]](rt, p & "aliases").get()
      choices    = lookup[Xlist[Rich]](rt, p & "choices").get()
      addFlags   = lookup[bool](rt, p & "add_choice_flags").get()
      sdoc       = rt.get_short_doc(p)
      ldoc       = rt.get_long_doc(p)
      cb         = lookup[ptr ZCallback](rt, p & "callback")
      fieldToSet = lookup[Rich](rt, p & "field_to_set").get(nil).toNimStr()
      min        = lookup[int](rt, p & "min").get()
      max        = lookup[int](rt, p & "max").get()

    var
      allNames = aliases & toXList(@[realName])
      f        = cmdObj.addChoiceFlag(realName, allNames, choices, addFlags,
                                      true, false, sdoc, ldoc, cb,
                                      r(fieldToSet))
    f.min = min
    f.max = max

proc loadFlagArgs(cmdObj: CommandSpec, info: LoadInfo, multi = false) =
  var
    rt = info.runtime

  for flagname in rt.get_subsections(info.base_path):
    let
      p          = info.base_path & flagname.toNimStr() & "."
      realName   = u2d(flagname)
      aliases    = lookup[XList[Rich]](rt, p & "aliases").get()
      cb         = lookup[ptr ZCallback](rt, p & "callback")
      fieldToSet = lookup[Rich](rt, p & "field_to_set").get(nil)
      optArg     = lookup[bool](rt, p & "optional_arg").get()
      sdoc       = rt.get_short_doc(p)
      ldoc       = rt.get_long_doc(p)

    var allNames = aliases & toXList(@[realName])

    discard cmdObj.addFlagWithArg(realName, allNames, multi, false, sdoc,
                                         ldoc, cb, fieldToSet, optArg)

template subsection(subsect: string, code: untyped) =
  info.base_path &= subsect & "."
  code
  info.base_path = sec

proc loadSection(cmdObj: CommandSpec, info: LoadInfo) =
  # The command object was created by the caller.  We need to:
  # 1) Add any flags spec'd.
  # 2) Create any subcommands spec'd.
  let
    sec      = info.base_path
    rt       = info.runtime
    contents = rt.get_subsections(sec)


  if rich"flag_yn" in contents:
    subsection("flag_yn", cmdObj.loadYn(info))
  if rich"flag_help" in contents:
    subsection("flag_help", cmdObj.loadHelps(info))
  if rich"flag_choice" in contents:
    subsection("flag_choice", cmdObj.loadChoices(info))
  if rich"flag_multi_choice" in contents:
    subsection("flag_multi_choice", cmdObj.loadMChoices(info))
  if rich"flag_arg" in contents:
    subsection("flag_arg", cmdObj.loadFlagArgs(info))
  if rich"flag_multi_arg" in contents:
    subsection("flag_multi_arg", cmdObj.loadFlagArgs(info, multi = true))

  let have_cmd = rich"command" in contents

  let cmdsec = sec & "command" & "."

  if info.addHelpCmds and (not have_cmd or
                           rich"help" notin rt.get_section_contents(cmdsec)):
    #if not have_cmd:
    #  cmdObj.subOptional = true
    let help = cmdObj.addCommand(rich"help", unknownFlagsOk = true)
    help.addArgs()
    help.autoHelp = true

  if not have_cmd:
    return

  for command in rt.get_subsections(cmdsec):
    let base       = cmdsec & command.toNimStr() & "."
    info.base_path = base

    let
      args     = lookup[XList[Rich]](rt, base & "args").get()
      minarg   = cast[int](args[0])
      maxarg   = cast[int](args[1])
      aliases  = lookup[Xlist[Rich]](rt, base & "aliases").get()
      cbOpt    = lookup[ptr ZCallback](rt, base & "callback")
      #asubmut  = lookup[bool](rt, base & "arg_sub_mutex").get()
      subsOk   = lookup[bool](rt, base & "optional_subcommands").get()
      ignoreF  = lookup[bool](rt, base & "ignore_all_flags").get()
      argname  = lookup[Rich](rt, base & "arg_name").get().toNimStr()
      ibdef    = cmdObj.unknownFlagsOk
      ignoreB  = lookup[bool](rt, base & "ignore_bad_flags").get(ibdef)
      dashFArg = lookup[bool](rt, base & "dash_arg_space_optional").get()
      colOk    = lookup[bool](rt, base & "colon_ok").get(not cmdObj.noColon)
      spcOk    = lookup[bool](rt, base & "space_ok").get(not cmdObj.noSpace)
      sdoc     = rt.get_short_doc(base)
      ldoc     = rt.get_long_doc(base)
      sub      = cmdObj.addCommand(command, aliases, subsOk,
                                   ignoreB, ignoreF, dashFArg, sdoc, ldoc,
                                   r(argname), cbOpt, not colOk, not spcOk)

    sub.addArgs(minarg, maxarg).loadSection(info)
    info.base_path = cmdsec

proc stringizeFlags(inflags: Dict[string, FlagSpec], id: int):
                   Dict[Rich, Rich] =
  result.initDict()

  for (f, spec) in inflags.items(sort = true):
    let k = c4str(f)
    case spec.kind
    of afPair:
      result[k] = c4str($(spec.boolValue[id]))
    of afChoice:
      result[k] = spec.selected[id][0]
    of afMultiChoice:
      result[k] = spec.selected[id].string_join(rich",")
    of afStrArg:
      result[k] = spec.strVal[id]
    of afMultiArg:
      result[k] = spec.strArrVal[id].string_join(rich",")

proc stringizeFlags*(winner: ArgResult): Dict[Rich, Rich] =
  return winner.flags.stringizeFlags(winner.parseCtx.parseId)

proc addDash(s: string): string =
  if len(s) == 1: return "-" & s
  else:            return "--" & s

proc getUsage(cmd: CommandSpec): Rich =
  var cmdName, flags, argName, subs: string

  let fname = getAppFilename().splitPath().tail

  if cmd.reportingName.len() == 0:
    cmdName = fname
  else:
    cmdname = fname & " " & cmd.reportingName.toNimStr().replace(".", " ")

  if cmd.maxArgs == 0:
    argName = ""
  else:
    for i in 0 ..< cmd.minArgs:
      argName &= cmd.argName.toNimStr() & " "
    if cmd.minArgs != cmd.maxArgs:
      argName &= "[" & cmd.argName.toNimStr() & "] "
      if cmd.maxArgs == high(int):
        argName &= "..."
      else:
        argName &= "(0, " & $(cmd.maxArgs - cmd.minArgs) & ") "

  if len(cmd.flags.keys()) != 0:
    flags = "[FLAGS]"

  if len(cmd.commands.keys()) != 0:
    if cmd.subOptional: subs = "[COMMANDS]"
    else:               subs = "COMMAND"

  return cell(bold("Usage:") + atom(" " & cmdname & " " & flags & " " &
    argName & subs), "h1").grid_to_str(80)

proc getCommandList(cmd: CommandSpec): Rich =
  var
    title = "Available commands"
    cmds: seq[seq[Rich]] = @[@[text("Command"), text("Description")]]
    found: seq[Rich]

  for (k, sub) in cmd.commands.items(sort = true):
    if sub.reportingName notin found and sub.reportingName.len() != 0:
      found.add(sub.reportingName)

      var doc = sub.sdoc
      if doc == nil:
        doc = rich" "
      cmds.add(@[sub.reportingName, doc] )

  result = cmds.table(title = title).grid_to_str(80)

proc getFlagHelp(cmd: CommandSpec): Rich =
  var
    flagList: seq[Rich]
    rows:     seq[seq[Rich]] = @[@[text("Flag"), text("Description")]]
    aliases:  seq[Rich]
    numFlags: int
    fstr:     string

  for (k, spec) in cmd.flags.items():
    if not k.toNimStr().startswith("->"):
      flagList.add(k)

  if len(flaglist) == 0: return

  for k in flagList:
    let spec = cmd.allpossibleflags[k]
    numFlags = len(spec.recognizedNames)
    if spec.reportingName in spec.recognizedNames:
      fstr = spec.reportingName.toNimStr().addDash()
      aliases = @[]
      for item in spec.recognizedNames:
        if item != spec.reportingName:
          aliases.add(r(item.toNimStr().addDash()))

    else:
      fstr = spec.recognizedNames[0].toNimStr().addDash()
      aliases = @[]
      for i in 1 ..< spec.recognizedNames.len():
        let item = spec.recognizedNames[i]
        aliases.add(item.toNimStr().addDash().r())


    case spec.kind
    of afPair:
      if spec.reportingName notin spec.positiveNames:
        # TODO... implement this branch.  Don't need for chalk tho.
        discard
      else:
        fstr    = spec.reportingName.toNimStr().addDash()
        aliases = @[]
        for item in spec.positiveNames:
          if item != spec.reportingName:
            aliases.add(r(item.toNimStr().addDash()))
        if len(aliases) != 0:
          fstr &= "\nor: " & toNimStr(aliases.toXList().string_join(rich", "))
        rows.add(@[text(fstr), spec.sdoc])
        if len(spec.negativeNames) != 0:
          aliases = @[]
          fstr = spec.negativeNames[spec.negativeNames.len()-1].toNimStr().
                                                                 addDash()
          for item in spec.negativeNames.toSeq()[0 ..< ^1]:
            aliases.add(item.toNimStr().addDash().r())
          if len(aliases) != 0:
            fstr &= "\nor: " & aliases.toSeqStr().join(", ")
          rows.add(@[text(fstr), text("Does the opposite of the row above.")])
    of afChoice:
      fstr &= "= " & spec.choices.toSeqStr().join(" | ")
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.toSeqStr().join(", ")
      rows.add(@[text(fstr), spec.sdoc])
    of afMultiChoice:
      fstr &= "= " & spec.choices.toSeqStr().join(",")
      if spec.min == spec.max:
        fstr &= "(select " & $(spec.min) & ") "
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.toSeqStr().join(", ")
      rows.add(@[text(fstr), spec.sdoc])
    of afStrArg:
      fstr &= "= ARG"
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.toSeqStr().join(", ")
      rows.add(@[text(fstr), spec.sdoc])
    of afMultiArg:
      fstr &= "= ARG,ARG,..."
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.toSeqStr().join(", ")
      rows.add(@[c4str(fstr), spec.sdoc])

    if rows[^1][1] == nil:
      rows[^1][1] = c4str(" ")

  if len(rows) != 0:
    result = table(rows, title = "Command Flags").grid_to_str()

proc getCmdAndFlagHelp*(cmd: CommandSpec): Rich =
  result = getUsage(cmd)

  if cmd.commands.items().len() != 0:
     result += cmd.getCommandList()
  result += cmd.ldoc
  result += cmd.getFlagHelp()

proc find_sub_commands(c: CommandSpec, d: var Dict[Rich, CommandSpec]) =
  for (_, sub) in c.commands.items(sort = true):
    sub.find_sub_commands(d)

  for item in c.allNames:
    d[item] = c

proc get_all_sub_commands(c: CommandSpec): Dict[Rich, CommandSpec] =
  initDict(result)
  c.find_sub_commands(result)

proc getCmdHelp*(rt: RuntimeState, cmd: CommandSpec, args: seq[string]): Rich =
  ## This version allows you to pass in a command spec object, and
  ## the arguments if provided are expected to be commands under it.
  ## Compare w/ `searchCmdHelp()`

  if len(args) == 0:
    result = getCmdAndFlagHelp(cmd)

  else:
    var legitCmds: seq[(bool, Rich, Rich)] = @[]

    for arg in args:
      let item = r(arg)
      if item in cmd.commands and cmd.commands[item].reportingName == item:
        legitCmds.add((true, item, item))
      else:
        var found = false
        for (sub, spec) in cmd.commands.items(sort = true):
          if item in spec.allNames:
            legitCmds.add((true, item, spec.reportingName))
            found = true
            break
        if not found and not item.toNimStr().startswith("-"):
            stderr.writeLine("No such command: " & item.toNimStr())

    if len(legitCmds) == 0:
      result += getCmdAndFlagHelp(cmd)
    else:
      for (c, given, reporting) in legitCmds:
        if not c:
          result += h1("Help for " & given.toNimStr())
          result += reporting
          continue
        if given != reporting:
          result += rich"Note: '" + given + rich"' is an alias for '" +
            reporting + rich"'"

        result += getCmdAndFlagHelp(cmd.commands[reporting])

proc searchCmdHelp*(rt: RuntimeState, res: ArgResult, args: seq[Rich]): Rich =
  ## Gets command help, assuming that the args are valid command names, but
  ## might be subcommands.

  if args.len() == 0:
    result = rt.getCmdHelp(res.topSpec, @[])
    return

  let exename = r(getAppFileName().splitPath().tail)
  if args.len() == 1 and args[0] == exename:
    return res.topSpec.getCmdAndFlagHelp()

  let
    sc_info = res.topSpec.get_all_sub_commands()
    cmdkeys = sc_info.keys()

  var
    last:  CommandSpec = res.topSpec

  for item in args:
    if item in cmdkeys:
      last = sc_info[item]
      result += last.getCmdAndFlagHelp()
      continue
    else:
      result += text("No help found for: ") + em(item)

proc managedCommit(winner: ArgResult, attrtop: string,
                   runtime: RuntimeState): Rich =
  let
    parseId = winner.parseCtx.parseId
    endCmd  = winner.parseCtx.finalCmd

  for (flag, spec) in winner.flags.items(sort = true):
    spec.finalFlagIx = parseId

    var
      val:       pointer
      fieldType: TypeSpec

    case spec.kind
    of afPair:
      val       = cast[pointer](spec.boolValue[parseId])
      fieldType = tspec_bool()

    of afChoice:
      val        = cast[pointer](spec.selected[parseId][0])
      fieldType  = tspec_string()

    of afStrArg:
      val        = cast[pointer](spec.strVal[parseId])
      fieldType  = tspec_string()

    of afMultiArg:
      val        = cast[pointer](spec.strArrVal[parseId])
      fieldType  = tspec_list(tspec_string())

    of afMultiChoice:
      val        = cast[pointer](spec.selected[parseId])
      fieldType  = tspec_list(tspec_string())

    if spec.callback.isSome():
        let
          cb     = spec.callback.get()
          retBox = runtime.run_callback_internal(cb, [(val, fieldType)])
          ret    = cast[Rich](retbox).toNimStr()

        if ret != "":
          raise newException(ValueError, ret)

    if spec.fieldToSet.con4m_len() != 0:
      runtime.override(spec.fieldToSet.toNimStr(), val, fieldType)

  var
    cmdObj  = endCmd
    cmdName = winner.command

  while true:
    if cmdObj.callback.isSome():
      let c4arr = winner.args[cmdName]
      let arg = [(cast[pointer](c4arr), tspec_list(tspec_string()))]

      discard runtime.run_callback_internal(cmdObj.callback.get(), arg)

    if cmdObj.autoHelp and cmdObj.parent != nil:
      result = runtime.getCmdHelp(cmdObj.parent,
                                  winner.args[cmdName].toseqstr())

    let parts = cmdName.split(".")
    cmdName = parts[0 ..< ^1].join(".")
    if cmdObj.parent == nil:
      break
    cmdObj = cmdObj.parent

  let
    cmd_field   = attrtop & "command_attribute"
    flag_field  = attrtop & "flag_attribute"
    arg_field   = attrtop & "arg_attribute"
    cmdAttrBox  = lookup[Rich](runtime, cmd_field)
    flagAttrBox = lookup[Rich](runtime, flag_field)
    argAttrBox  = lookup[Rich](runtime, arg_field)

  if cmdAttrBox.isSome():
    let attrVal = cast[pointer](c4str(winner.command))
    runtime.override(cmdAttrBox.get().toNimStr(), attrVal, tspec_string())

  if flagAttrBox.isSome():
    let
      flags = cast[pointer](winner.flags.stringizeFlags(parseId))
      ty    = tspec_dict(tspec_string(), tspec_string())

    runtime.override(flagAttrBox.get().toNimStr(), flags, ty)

    let arr = winner.args[winner.command]

    runtime.override(argAttrBox.get().toNimStr(),
                     cast[pointer](arr), tspec_list(tspec_string()))

proc finalizeManagedGetopt*(runtime: RuntimeState,
                            options: seq[ArgResult],
                            getopts_path = "",
                            outputHelp   = true): ArgResult =
  var base: string

  if getopts_path != "":
    base &= ".getopts."
  else:
    base = "getopts."

  var top = options[0].parseCtx.finalCmd
  while top.parent != nil:
    top = top.parent

  for item in options:
    item.topSpec = top

  if options.len() == 1:
    return options[0]

  var matchingCmds: seq[string] = @[]
  let
    cmd_field  = base & "command_attribute"
    cmdAttrBox = lookup[Rich](runtime, cmd_field)

  if cmdAttrBox.isSome():
    let
      cmdLoc = cmdAttrBox.get().toNimStr()
      cmdBox = lookup[Rich](runtime, cmdLoc)

    if cmdBox.isSome():
      let cmd = cmdBox.get().toNimStr()

      for item in options:
        let thisCmd = item.command.split(".")[0]
        if cmd == thisCmd:
          item.helpToPrint = item.managedCommit(base, runtime)
          if outputHelp and item.helpToPrint != nil:
            print item.helpToPrint
          return item
        elif cmd == "":
          matchingCmds.add(thisCmd)

      if cmd == "":
        raise newException(ValueError, "Couldn't guess the command because " &
          "multiple commands match: " & matchingCmds.join(", "))
      # If we get here, there are one of two situations:
      # 1) The default command isn't an actual valid command, in
      #    which case whoever is using this API made a mistake
      # 2) We assumed a valid command, but when we added it to the front
      #    when we were trying all completions, we got a error.
      #    But, currently, we're throwing away bad error messages.
      #    So let's just give a pretty lame but clear message.

      raise newException(ValueError, "Bad command line: no explicit command " &
          "provided, and if we add the default command ('" & cmd & "') then " &
          "the result doesn't properly parse (add the explicit command " &
          " to see the error)")
  else:
    if options.len() == 1:
        raise newException(ValueError, "Couldn't guess the command because " &
          "multiple commands match: " & matchingCmds.join(", "))

  raise newException(ValueError,
                     "No command found in input, and no default command " &
                     "was provided by configuration.")

proc runManagedGetopt*(runtime:      RuntimeState,
                       args:         seq[string],
                       getopts_path = "",
                       output_help  = true): seq[ArgResult] =
  # By this point, the spec should be validated, making the
  # checks for getopts() correctness unneeded.
  var base: string

  if getopts_path != "":
    base &= ".getopts."
  else:
    base = "getopts."

  let
    args_opt     = lookup[Array](runtime, base & "args")
    cmd_sec      = base & "command"
    cmd_contents = runtime.get_section_contents(cmd_sec)

  var
    min_arg = 0
    max_arg = 0
    li      = LoadInfo(runtime: runtime, base_path: base)


  if len(cmd_contents) == 0:
      max_arg = high(int) # No commands provided, so default is to allow any #

  if args_opt.isSome():
    let arr = args_opt.get()

    minArg = cast[int](arr[0])
    maxArg = cast[int](arr[1])

  let
    def_opt   = lookup[Rich](runtime, base & "default_command").get(nil)
    yesBox    = lookup[XList[Rich]](runtime, base & "default_yes_prefixes")
    noBox     = lookup[XList[Rich]](runtime, base & "default_no_prefixes")
    docOnErr  = lookup[bool](runtime, base & "show_doc_on_err").get(true)
    colonOk   = lookup[bool](runtime, base & "colon_ok").get(true)
    spaceOk   = lookup[bool](runtime, base & "space_ok").get(true)
    ignoreBad = lookup[bool](runtime, base & "ignore_bad_flags").get(false)
    addHelp   = lookup[bool](runtime, base & "add_help_commands").get(true)
    sdoc      = runtime.get_short_doc(base)
    ldoc      = runtime.get_long_doc(base)
    argName   = lookup[Rich](runtime, base & "arg_name").get(c4str("ARG"))
    dashFArg  = lookup[bool](runtime,
                             base & "dash_arg_space_optional").get(true)

  li.defaultCmd = some(def_opt.toNimStr())

  if yesBox.isSome():
    li.defaultYesPref = yesBox.get().toSeqStr()

  if noBox.isSome():
    li.defaultNoPref = noBox.get().toSeqStr()

  li.showDocOnErr = docOnErr
  li.addHelpCmds  = addHelp

  let topLevelCmd = newSpecObj(rich"", minArgs = minArg, maxArgs = maxArg,
                              subOptional = false, unknownFlagsOk = ignoreBad,
                              dockerSingleArg = dashFArg, noFlags = false,
                              sdoc = sdoc, ldoc = ldoc, argname = argname,
                              noColon = not colonOk, noSpace = not spaceOk)

  topLevelCmd.attrTop = r(base)
  topLevelCmd.loadSection(li)

  result = topLevelCmd.ambiguousParse(args, defaultCmd = li.defaultCmd)
  for item in result:
    item.attrtop = base

  if len(result) == 1:
    let helpToPrint = result[0].managedCommit(base, runtime)
    if outputHelp:
      if helpToPrint != nil:
        print(helpToPrint)
    else:
      result[0].helpToPrint = helpToPrint

proc parse_command_line*(code: string, refname = "c4m_getopt"): RuntimeState =
  ## Parse a command line spe, and returns the con4m runtime state
  ## for the config file execution.

  let
    ctx  = newCompileContext(nil)
    spec = ctx.loadInternalModule(refname, code)

  ctx.buildProgram(spec)
  if not ctx.canproceed():
    discard ctx.printErrors(file = stderr)
    quit(-4)

  var
    rt     = ctx.generateInitialCodeObject()
    exit   = rt.executeObject()
    params = commandLineParams()

  try:
    let
      pre   = rt.runManagedGetopt(params)

    rt.cmdline_info = rt.finalizeManagedGetopt(pre)

    if rt.cmdline_info != nil:
      return rt
    else:
      print fgColor("error: ", "red") + text("command not understood.")
      quit(-1)

  except:
    print(fgColor("error: ", "red") + em(getCurrentException().msg))
    if "--debug" in params:
      rt.print_attributes()
      echo getCurrentException().getStackTrace()
    quit(-1)


proc con4m_getopt*(code: cstring, refname: cstring):
                 RuntimeState {.exportc, cdecl.} =
  return parse_command_line($(code), $(refname))
