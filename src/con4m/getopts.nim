## Partial redo of nimutils argParse capability, but with more
## functionality and the specification of commands, flags and options
## checked via a c42 spec.
##
## If you call the API directly, and didn't do the input checking,
## results are undefined :)

import unicode, options, tables, os, sequtils
import strutils except strip

const errNoArg = "Expected a command but didn't find one"

type
  ArgFlagKind*    = enum
    afPair, afChoice, afStrArg, afMultiChoice, afMultiArg
    
  DeferredFlags*  = Table[string, Option[string]]
  FlagSpec* = ref object
    reportingName:    string
    clobberOk:        bool
    recognizedNames:  seq[string]
    case kind:        ArgFlagKind
    of afPair:
      boolValue:      Table[int, bool]
      positiveNames:  seq[string]
      negativeNames:  seq[string]
      linkedChoice:   Option[FlagSpec]  
    of afChoice, afMultiChoice:
      choices:        seq[string]
      selected:       Table[int, seq[string]]
      linkedYN:       Option[FlagSpec]
    of afStrArg:
      strVal:         Table[int, string]
    of afMultiArg:
      strArrVal:      Table[int, seq[string]]
  CommandSpec* = ref object
    commands:          Table[string, CommandSpec]
    reportingName:     string
    allNames:          seq[string]
    flags:             Table[string, FlagSpec]
    minArgs:           int
    maxArgs:           int
    subOptional:       bool
    unknownFlagsOk:    bool
    noFlags:           bool
    parent:            Option[CommandSpec]
    allPossibleFlags:  Table[string, FlagSpec]
    finishedComputing: bool
  ArgResult* = ref object
    command*:    string
    args*:       Table[string, seq[string]]
    flags*:      Table[string, FlagSpec]
    parseCtx*:   ParseCtx
  ParseCtx = ref object
    args:     seq[string]
    curArgs:  seq[string]
    res:      ArgResult
    i:        int
    parseId*: int # globally unique parse ID.
    finalCmd: CommandSpec

proc flagSpecEq(f1, f2: FlagSpec): bool =
  if f1 == f2:           return true   # They're literally the same ref
  if f1.kind != f2.kind: return false
  if f1.reportingName == f2.reportingName: return true
  return false

proc newSpecObj*(reportingName: string       = "",
                 allNames: openarray[string] = [],
                 minArgs                     = 0,
                 maxArgs                     = 0,
                 subOptional                 = false,
                 unknownFlagsOk              = false,
                 noFlags                     = false,
                 parent                      = none(CommandSpec)): CommandSpec =
  if noFlags and unknownFlagsOk:
    raise newException(ValueError, "Can't have noFlags and unknownFlagsOk")
  return CommandSpec(reportingName:  reportingName,
                     allNames:       allNames.toSeq(),
                     minArgs:        minArgs,
                     maxArgs:        maxArgs,
                     subOptional:    subOptional,
                     unknownFlagsOk: unknownFlagsOk,
                     noFlags:        noFlags,
                     parent:         parent)

proc addCommand*(spec:           CommandSpec,
                 name:           string,
                 aliases:        openarray[string] = [],
                 subOptional:    bool              = false,
                 unknownFlagsOk: bool              = false,
                 noFlags:        bool              = false):
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
  result = newSpecObj(reportingName  = name,
                      allNames       = aliases,
                      subOptional    = subOptional,
                      unknownFlagsOk = unknownFlagsOk,
                      noFlags        = noFlags,
                      parent         = some(spec))

  if name notin result.allNames: result.allNames.add(name)
  for oneName in result.allNames:
    if oneName in spec.commands:
      raise newException(ValueError, "Duplicate command: " & name)
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
             reportingName:   string,
             clOk:            bool,
             recognizedNames: openarray[string]): FlagSpec =
  if cmd.noFlags:
    raise newException(ValueError,
                       "Cannot add a flag for a spec where noFlags is true")
  result = FlagSpec(reportingName: reportingName, kind: kind, clobberOk: clOk,
                    recognizedNames: recognizedNames.toSeq())
  cmd.flags[reportingName] = result

proc addChoiceFlag*(cmd:             CommandSpec,
                    reportingName:   string,
                    recognizedNames: openarray[string],
                    choices:         openarray[string],
                    flagPerChoice:   bool              = false,
                    multi:           bool              = false,
                    clobberOk:       bool              = false):
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
                                recognizedNames)
  flag.choices        = choices.toSeq()
  if flagPerChoice:
    for item in choices:
      let itemName = "->" & item # -> for forwards...
      var oneFlag = newFlag(cmd, afPair, itemName, clobberOk, @[item])
      oneFlag.positiveNames = @[item]
      oneFlag.linkedChoice  = some(flag)

  result = flag

proc addYesNoFlag*(cmd:           CommandSpec,
                   reportingName: string,  
                   yesValues:     openarray[string],
                   noValues:      openarray[string] = [],
                   clobberOk:     bool = false): FlagSpec {.discardable.} =
    
  var both   = yesValues.toSeq()
  both       = both & noValues.toSeq()
  var ynFlag = newFlag(cmd, afPair, reportingName, clobberOk, both)
    
  ynFlag.positiveNames = yesValues.toSeq()
  ynFlag.negativeNames = noValues.toSeq()

  if reportingName notin yesValues and reportingName notin noValues:
    let c      = cmd.addChoiceFlag("->" & reportingName,
                                          recognizedNames = @[reportingName],
                                          choices = both,
                                   clobberOk = clobberOk)
    c.linkedYN = some(ynFlag)
    
  result = ynFlag

proc addFlagWithArg*(cmd:             CommandSpec,
                     reportingName:   string,
                     recognizedNames: openarray[string] = [],
                     multi:           bool              = false, 
                     clobberOk:       bool              = false):
                       FlagSpec {.discardable.} =
  ## This simply adds a flag that takes a required string argument, or,
  ## in the case of multi-args, an array of string arguments.  The arguments
  ## are identical in semantics as for other flag types.

  let kind  = if multi: afMultiArg else: afStrArg
  result = newFlag(cmd, kind, reportingName, clobberOk, recognizedNames)

template argpError(msg: string) =
  var fullError = msg

  if ctx.res.command != "":
    fullError = "When parsing command '" & ctx.res.command & "': " & msg

  raise newException(ValueError, fullError)

template argpError(flagName: string, msg: string) =
  argpError("--" & flagName & ": " & msg)

proc validateOneFlag(ctx:     var ParseCtx,
                     name:    string,
                     inspec:  FlagSpec,
                     foundArg = none(string)) =
  var
    argCrap = foundArg
    spec    = inspec

  if ctx.i < len(ctx.args) and ctx.args[ctx.i][0] in [':', '=']:
    if argCrap.isNone():
      argCrap = some(ctx.args[ctx.i][1 .. ^1].strip())
      ctx.i = ctx.i + 1
      if argCrap.get() == "":
        if ctx.i < len(ctx.args):
          argCrap = some(ctx.args[ctx.i])
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
    # Here we require an argument, and we didn't find a ':' or '=',
    # so we just assume it's the next word, unless we see a dash.
    if ctx.i == len(ctx.args) or ctx.args[ctx.i][0] == '-':
      argpError(name, "requires an argument.")
    argCrap = some(ctx.args[ctx.i].strip())
    ctx.i  = ctx.i + 1

  if spec.kind notin [afMultiChoice, afMultiArg] and
     not spec.clobberOk and spec.reportingName in ctx.res.flags:
    argpError(name, "redundant flag not allowed")

  case spec.kind
  of afPair:
    if   name in spec.positiveNames: spec.boolValue[ctx.parseId] = true
    elif name in spec.negativeNames: spec.boolValue[ctx.parseId] = false
    else: raise newException(ValueError, "Reached unreachable code")
  of afChoice:
    let arg = argCrap.get()
    if arg notin spec.choices:
      argpError(name, "Invalid choice: '" & arg & "'")
    if spec.linkedYN.isSome():
      spec = spec.linkedYN.get()
      if not spec.clobberOk and spec.reportingName in ctx.res.flags:
        argpError(name, "redundant flag not allowed")
      if arg in spec.positiveNames:   spec.boolValue[ctx.parseId] = true
      elif arg in spec.negativeNames: spec.boolValue[ctx.parseId] = false
      else: raise newException(ValueError, "Reached unreachable code")
    else:
      spec.selected[ctx.parseId] = @[arg]
  of afMultiChoice:
      let arg = argCrap.get()
      if arg notin spec.choices:
        argpError(name, "Invalid choice: '" & arg & "'")
      if ctx.parseId notin spec.selected:
        spec.selected[ctx.parseId] = @[arg]
      elif arg notin spec.selected[ctx.parseId]:
        spec.selected[ctx.parseId].add(arg)
  of afStrArg:
    spec.strVal[ctx.parseId] = argCrap.get()
  of afMultiArg:
    var parts = argCrap.get()
    if parts[^1] == ',':
      while ctx.i != len(ctx.args) and ctx.args[ctx.i][0] != '-':
        parts = parts & ctx.args[ctx.i].strip()
        ctx.i = ctx.i + 1
    if parts[^1] == ',': parts = parts[0 ..< ^1]
    if ctx.parseId notin spec.strArrVal:
      spec.strArrVal[ctx.parseId] = parts.split(",")
    else:
      spec.strArrVal[ctx.parseId] &= parts.split(",")
    
  ctx.res.flags[spec.reportingName] = spec

proc parseOneFlag(ctx: var ParseCtx, spec: CommandSpec, validFlags: auto) =
  var
    orig        = ctx.args[ctx.i]
    cur         = orig[1 .. ^1]
    singleDash  = true
    definiteArg = none(string)

  ctx.i = ctx.i + 1

  if cur[0] == '-':
    cur        = cur[1 .. ^1]
    singleDash = false

  var
    colonix = cur.find(':')
    eqix    = cur.find('=')
    theIx   = colonix

  if theIx == -1:
    theIx = eqIx
  else:
    if eqIx != -1 and eqIx < theIx:
      theIx = eqIx
  if theIx != -1:
    let rest    = cur[theIx+1 .. ^1].strip()
    cur         = cur[0 ..< theIx].strip()

    if len(rest) != 0:
      definiteArg = some(rest)
    elif ctx.i != len(ctx.args):
      definiteArg = some(ctx.args[ctx.i])
      ctx.i = ctx.i + 1

  if cur in validFlags:
    ctx.validateOneFlag(cur, validFlags[cur], definiteArg)
  elif not singleDash:
    if spec.unknownFlagsOk:
      ctx.curArgs.add(orig)
      if ':' in orig or '=' in orig:
        ctx.i = ctx.i - 1
    else:
      argpError(cur, "Invalid flag")
  else:
    # Single-dash flags bunched together cannot have arguments.
    if definiteArg.isSome():
      argpError(cur, "Invalid flag")
    for i, c in cur:
      let oneCharFlag = $(c)
      if oneCharFlag in validFlags:
        ctx.validateOneFlag(oneCharFlag, validFlags[cur])
      elif  spec.unknownFlagsOk: continue
      elif i == 0: argpError(cur, "Invalid flag")
      else:
        argpError(cur, "Couldn't process all characters as flags")
    if spec.unknownFlagsOk: ctx.curArgs.add(orig)

proc buildValidFlags(inSpec: Table[string, FlagSpec]): Table[string, FlagSpec] =
  for reportingName, f in inSpec:
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
  for k, _ in ctx.res.flags:
    if k notin spec.allPossibleFlags:
      argpError(k, "Not a valid flag for this command.")

  while ctx.i != len(ctx.args):
    let cur = ctx.args[ctx.i]
    # If len is 1, we pass it through, usually means 'use stdout'
    if lookingForFlags and cur[0] == '-' and len(cur) != 1:
      if cur == "--":
        lookingForFlags = false
        ctx.i           = ctx.i + 1
        continue
      ctx.parseOneFlag(spec, validFlags)
      continue
    if cur in spec.commands:
      ctx.i = ctx.i + 1
      if len(ctx.curArgs) < spec.minArgs:
        argpError("Too few arguments (expected " & $(spec.minArgs) & ")")
      if len(ctx.curArgs) > spec.maxArgs:
        argpError("Too many arguments provided (max = " & $(spec.maxArgs) & ")")
      ctx.res.args[ctx.res.command] = ctx.curArgs
      let nextSpec = spec.commands[cur]
      if ctx.res.command != "":
        ctx.res.command &= "." & nextSpec.reportingName
      else:
        ctx.res.command = nextSpec.reportingName
      ctx.parseCmd(nextSpec)
      return

    ctx.curArgs.add(ctx.args[ctx.i])
    ctx.i = ctx.i + 1

  # If we exited the loop, we need to make sure the parse ended up in
  # a valid final state.
  if len(spec.commands) != 0 and not spec.subOptional:
    argpError(errNoArg)
  if len(ctx.curArgs) < spec.minArgs:
    argpError("Too few arguments (expected " & $(spec.minArgs) & ")")
  if len(ctx.curArgs) > spec.maxArgs:
    argpError("Too many arguments provided (max = " & $(spec.maxArgs) & ")")
  ctx.res.args[ctx.res.command] = ctx.curArgs
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
  if spec.parent.isSome():
    let parentFlags = spec.parent.get().allPossibleFlags
    for k, v in parentFlags: spec.allPossibleFlags[k] = v
  for k, v in spec.flags:
    if k in spec.allPossibleFlags:
      raise newException(ValueError, "command flag names cannot " &
        "conflict with parent flag names or top-level flag names." &
        "This is because we want to make sure users don't have to worry " &
        "about getting flag position right whenever possible."
      )
    spec.allPossibleFlags[k] = v

  var flagsToAdd: Table[string, FlagSpec]
  for _, kid in spec.commands:
    kid.computePossibleFlags()
    for k, v in kid.allPossibleFlags:
      if k in spec.allPossibleFlags: continue
      if k notin flagsToAdd:
        flagsToAdd[k] = v
        continue
      if not flagSpecEq(flagsToAdd[k], v):
        raise newException(ValueError, "Sub-commands with flags of the " &
          "same name must have identical specifications (flag name: " & k & ")")
  for k, v in flagsToAdd:
    spec.allPossibleFlags[k] = v
  spec.finishedComputing = true

var parseId = 0
proc parseOne(ctx: var ParseCtx, spec: CommandSpec) =
  ctx.i       = 0
  ctx.res     = ArgResult(parseCtx: ctx)
  ctx.parseId = parseId
  parseId     = parseId + 1
  ctx.parseCmd(spec)

proc ambiguousParse*(spec:          CommandSpec,
                     inargs:        openarray[string] = [],
                     defaultCmd:    Option[string]    = none(string)):
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

  try:
    var ctx = ParseCtx(args: args)
    ctx.parseOne(spec)
    return @[ctx.res]
  except ValueError:
    if getCurrentExceptionMsg() != errNoArg: raise
    if defaultCmd.isNone():                  raise
    # else, ignore.

  let default = defaultCmd.get()
  if default != "":
    try:    return spec.ambiguousParse(@[default] & args)
    except: firstError = getCurrentExceptionMsg()

  for cmd, ss in spec.commands:
    if ss.reportingName != cmd: continue
    var ctx = ParseCtx(args: @[cmd] & args)
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

when isMainModule:
  var top = newSpecObj()
  top.addYesNoFlag("color", ["color", "c"], ["no-color", "C"])
  top.addYesNoFlag("dry-run", ["dry-run", "d"], ["no-dry-run", "D"])
  top.addYesNoFlag("publish-defaults", ["publish-defaults", "p"],
                   ["no-publish-defaults", "P"])
  top.addYesNoFlag("help", ["help", "h"])
  top.addChoiceFlag("log-level", ["log-level", "l"],
                    ["verbose", "trace", "info", "warn", "error", "none"],
                    true)
  top.addFlagWithArg("config-file", ["config-file", "f"], multi = true)
  
  var ins = top.addCommand("insert", ["inject", "ins", "in", "i"]).addArgs()
  ins.addYesNoFlag("recursive", ["recursive", "r"], ["no-recursive", "R"])
  
  var ext = top.addCommand("extract", ["ex", "e"]).addArgs()
  ext.addYesNoFlag("recursive", ["recursive", "r"], ["no-recursive", "R"])

  var dcmd = top.addCommand("delete", ["del"], unknownFlagsOk = true).addArgs()
  dcmd.addYesNoFlag("recursive", ["recursive", "r"], ["no-recursive", "R"])
  top.addCommand("defaults", ["def"])
  top.addCommand("confdump", ["dump"]).addArgs(min = 1)
  top.addCommand("confload", ["load"]).addArgs(min = 1, max = 1)
  top.addCommand("version", ["vers", "v"])
  top.addCommand("docker", noFlags = true).addArgs()
  top.addCommand("help", ["h"]).addArgs(min = 0, max = 1)

  let x = when true:
    top.parse(@["ex", "--no-color", "--log-level", "=", "info",
                        "defaults", "--recursive", "--config-file=foo",
                        "--config-file=bar,boz,", "zork",
                        "--no-publish-defaults", "testy",
                        "these", "are", "my", "test", "args"],
                      defaultCmd = some("extract"))
  elif false:
    top.parse(@["--no-color", "docker", "--log-level", "=", "info",
                        "defaults", "--recursive", "--config-file=foo",
                        "--no-publish-defaults", "testy",
                        "these", "are", "my", "test", "args"],
                      defaultCmd = some("extract"))
  else:
    top.parse(@["--no-color", "delete", "--log-level", "=", "info",
                       "defaults", "--recursive", "--unknown-flag=", "foo",
                        "--no-publish-defaults", "testy",
                        "these", "are", "my", "test", "args"],
                      defaultCmd = some("extract"))
  echo x.command
  echo x.args
  let id = x.parseCtx.parseId
  for k, v in x.flags:
    var value: string = ""
    case v.kind
    of afPair:
      value = $(v.boolValue[id])
    of afChoice:
      value = $(v.selected[id][0])
    of afMultiChoice:
      value = $(v.selected[id])
    of afStrArg:
      value = $(v.strVal[id])
    of afMultiArg:
      value = $(v.strArrVal[id])
      
    echo "Got flag: ", k, "; value = ", value
  
    
  
