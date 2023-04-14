## This is a partial redo of nimutils argParse capability, but with
## more functionality and the specification of commands, flags and
## options checked via a c42 spec.
##
## If you call the API directly, and didn't do the input checking,
## results are undefined :)

import unicode, options, tables, os, sequtils, types, nimutils, st, eval,
       std/terminal, algorithm, spec
import strutils except strip

const errNoArg = "Expected a command but didn't find one"

type
  ArgFlagKind*    = enum
    afPair, afChoice, afStrArg, afMultiChoice, afMultiArg
  FlagSpec* = ref object
    reportingName:    string
    clobberOk:        bool
    recognizedNames:  seq[string]
    doc:              string
    callback:         Option[CallbackObj]
    fieldToSet:       string
    case kind:        ArgFlagKind
    of afPair:
      helpFlag:       bool
      boolValue:      Table[int, bool]
      positiveNames:  seq[string]
      negativeNames:  seq[string]
      linkedChoice:   Option[FlagSpec]  
    of afChoice, afMultiChoice:
      choices:        seq[string]
      selected:       Table[int, seq[string]]
      linkedYN:       Option[FlagSpec]
      min, max:       int
    of afStrArg:
      strVal:         Table[int, string]
    of afMultiArg:
      strArrVal:      Table[int, seq[string]]
  CommandSpec* = ref object
    commands:          Table[string, CommandSpec]
    reportingName:     string
    allNames:          seq[string]
    flags:             Table[string, FlagSpec]
    callback:          Option[CallbackObj]
    doc:               string
    argName:           string
    minArgs:           int
    maxArgs:           int
    subOptional:       bool
    unknownFlagsOk:    bool
    noFlags:           bool
    autoHelp:          bool
    finishedComputing: bool
    parent:            Option[CommandSpec]
    allPossibleFlags:  Table[string, FlagSpec]
    
  ArgResult* = ref object
    stashedTop*: AttrScope
    command*:    string
    args*:       Table[string, seq[string]]
    flags*:      Table[string, FlagSpec]
    parseCtx*:   ParseCtx
  ParseCtx = ref object
    curArgs:  seq[string]
    args:     seq[string]
    res:      ArgResult
    i:        int
    parseId*: int # globally unique parse ID.
    finalCmd*: CommandSpec

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
                 doc                         = "",
                 argName                     = "",
                 callback                    = none(CallbackObj),
                 parent                      = none(CommandSpec)): CommandSpec =
  if noFlags and unknownFlagsOk:
    raise newException(ValueError, "Can't have noFlags and unknownFlagsOk")
  return CommandSpec(reportingName:     reportingName,
                     allNames:          allNames.toSeq(),
                     minArgs:           minArgs,
                     maxArgs:           maxArgs,
                     subOptional:       subOptional,
                     unknownFlagsOk:    unknownFlagsOk,
                     noFlags:           noFlags,
                     doc:               doc,
                     argName:           argName,
                     callback:          callback,
                     parent:            parent,
                     autoHelp:          false,
                     finishedComputing: false)

proc addCommand*(spec:           CommandSpec,
                 name:           string,
                 aliases:        openarray[string]   = [],
                 subOptional:    bool                = false,
                 unknownFlagsOk: bool                = false,
                 noFlags:        bool                = false,
                 doc:            string              = "",
                 argName:        string              = "",
                 callback:       Option[CallbackObj] = none(CallbackObj)):
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
                      noFlags         = noFlags,
                      doc             = doc,
                      argName         = argName,
                      callback        = callback,
                      parent          = some(spec))

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
             recognizedNames: openarray[string],
             doc:             string = "",
             callback:        Option[CallbackObj] = none(CallbackObj),
             toSet:           string = ""): FlagSpec =
  if cmd.noFlags:
    raise newException(ValueError,
                       "Cannot add a flag for a spec where noFlags is true")
  result = FlagSpec(reportingName: reportingName, kind: kind, clobberOk: clOk,
                    recognizedNames: recognizedNames.toSeq(), doc: doc,
                    callback: callback, fieldToSet: toSet)
  cmd.flags[reportingName] = result

proc addChoiceFlag*(cmd:             CommandSpec,
                    reportingName:   string,
                    recognizedNames: openarray[string],
                    choices:         openarray[string],
                    flagPerChoice:   bool                = false,
                    multi:           bool                = false,
                    clobberOk:       bool                = false,
                    doc:             string              = "",
                    callback:        Option[CallbackObj] = none(CallbackObj),
                    toSet:           string              = ""):
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
                                recognizedNames, doc, callback)
  flag.choices        = choices.toSeq()
  if flagPerChoice:
    for item in choices:
      let itemName = "->" & item # -> for forwards...
      var oneFlag = newFlag(cmd, afPair, itemName, clobberOk, @[item],
                            doc, callback, toSet)
      oneFlag.positiveNames = @[item]
      oneFlag.linkedChoice  = some(flag)

  result = flag

proc addYesNoFlag*(cmd:           CommandSpec,
                   reportingName: string,  
                   yesValues:     openarray[string],
                   noValues:      openarray[string] = [],
                   clobberOk:     bool = false,
                   doc:           string = "", 
                   callback:      Option[CallbackObj] = none(CallbackObj),
                   toSet:         string              = ""):                   
                     FlagSpec {.discardable.} =
    
  var both   = yesValues.toSeq()
  both       = both & noValues.toSeq()
  var ynFlag = newFlag(cmd, afPair, reportingName, clobberOk, both,
                       doc, callback, toSet)
    
  ynFlag.positiveNames = yesValues.toSeq()
  ynFlag.negativeNames = noValues.toSeq()

  if reportingName notin yesValues and reportingName notin noValues:
    let c      = cmd.addChoiceFlag("->" & reportingName,
                                          recognizedNames = @[reportingName],
                                          choices = both, clobberOk = clobberOk)
    c.linkedYN = some(ynFlag)
    
  result = ynFlag

proc addFlagWithArg*(cmd:             CommandSpec,
                     reportingName:   string,
                     recognizedNames: openarray[string]   = [],
                     multi:           bool                = false, 
                     clobberOk:       bool                = false,
                     doc:             string              = "",
                     callback:        Option[CallbackObj] = none(CallbackObj),
                     toSet:           string              = ""):
                       FlagSpec {.discardable.} =
  ## This simply adds a flag that takes a required string argument, or,
  ## in the case of multi-args, an array of string arguments.  The arguments
  ## are identical in semantics as for other flag types.

  let kind  = if multi: afMultiArg else: afStrArg
  result = newFlag(cmd, kind, reportingName, clobberOk, recognizedNames,
                   doc, callback, toSet)

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
        ctx.res.command             = nextSpec.reportingName
      ctx.res.args[ctx.res.command] = ctx.curArgs
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

type LoadInfo = ref object
  defaultCmd:     Option[string]
  defaultYesPref: seq[string]
  defaultNoPref:  seq[string]
  showDocOnErr:   bool
  errCmd:         string
  addHelpCmds:    bool

proc getSec(aOrE: AttrOrErr): Option[AttrScope] =
  if aOrE.isA(AttrErr) : return none(AttrScope)
  let aOrS = aOrE.get(AttrOrSub)
  if aOrS.isA(Attribute): return none(AttrScope)
  return some(aOrS.get(AttrScope))

template u2d(s: string): string = s.replace("_", "-")

proc loadYn(cmdObj: CommandSpec, all: AttrScope, info: LoadInfo) =
  for k, v in all.contents:
    let
      realName   = u2d(k)
      one        = v.get(AttrScope)
      yesAliases = unpack[seq[string]](one.attrLookup("yes_aliases").get())
      noAliases  = unpack[seq[string]](one.attrLookup("no_aliases").get())
      yesPrefOpt = one.attrLookup("yes_prefixes")
      noPrefOpt  = one.attrLookup("no_prefixes")
      doc        = unpack[string](one.attrLookup("doc").get())
      cbOpt      = one.attrLookup("callback")
      yesPref    = if yesPrefOpt.isSome(): unpack[seq[string]](yesPrefOpt.get())
                   else: info.defaultYesPref
      noPref     = if noPrefOpt.isSome(): unpack[seq[string]](noPrefOpt.get())
                   else: info.defaultNoPref
      cb         = if   cbOpt.isSome(): some(unpack[CallbackObj](cbOpt.get()))
                   else:                none(CallbackObj)
      ftsOpt     = one.attrLookup("field_to_set")
      fieldToSet = if ftsOpt.isSome(): unpack[string](ftsOpt.get())
                   else:               ""
    var
      yesNames = yesAliases
      noNames  = noAliases

    if len(yesPref) == 0:
      yesNames.add(realName)
    else:
      for prefix in yesPref:
        if prefix.endswith("-"): yesNames.add(prefix & realName)
        else:                    yesNames.add(prefix & "-" & realName)
    for prefix in noPref:
      if prefix.endswith("-"): noNames.add(prefix & realName)
      else:                    noNames.add(prefix & "-" & realName)

    cmdObj.addYesNoFlag(realName, yesNames, noNames, false, doc, cb, fieldToSet)
      
proc loadHelps(cmdObj: CommandSpec, one: AttrScope, info: LoadInfo) =
  let
    names = unpack[seq[string]](one.attrLookup("names").get())
    doc   = unpack[string](one.attrLookup("doc").get())
  
  cmdObj.addYesNoFlag("--help", names, [], false, doc)
  
proc loadChoices(cmdObj: CommandSpec, all: AttrScope, info: LoadInfo) =
  for k, v in all.contents:
    let
      realName   = u2d(k)
      one        = v.get(AttrScope)
      aliases    = unpack[seq[string]](one.attrLookup("aliases").get())
      choices    = unpack[seq[string]](one.attrLookup("choices").get())
      addFlags   = unpack[bool](one.attrLookup("add_choice_flags").get())
      doc        = unpack[string](one.attrLookup("doc").get())
      cbOpt      = one.attrLookup("callback")
      cb         = if cbOpt.isSome(): some(unpack[CallbackObj](cbOpt.get()))
                   else:              none(CallbackObj)
      ftsOpt     = one.attrLookup("field_to_set")      
      fieldToSet = if ftsOpt.isSome(): unpack[string](ftsOpt.get())
                   else:               ""

    var allNames = aliases & @[realName]

    cmdObj.addChoiceFlag(realName, allNames, choices, addFlags, false,
                         false, doc, cb, fieldToSet)
                         
proc loadMChoices(cmdObj: CommandSpec, all: AttrScope, info: LoadInfo) =
  for k, v in all.contents:
    let
      realName   = u2d(k)
      one        = v.get(AttrScope)
      aliases    = unpack[seq[string]](one.attrLookup("aliases").get())
      choices    = unpack[seq[string]](one.attrLookup("choices").get())
      addFlags   = unpack[bool](one.attrLookup("add_choice_flags").get())
      doc        = unpack[string](one.attrLookup("doc").get())
      cbOpt      = one.attrLookup("callback")
      cb         = if cbOpt.isSome(): some(unpack[CallbackObj](cbOpt.get()))
                   else:              none(CallbackObj)
      min        = unpack[int](one.attrLookup("min").get())
      max        = unpack[int](one.attrLookup("min").get())      
      ftsOpt     = one.attrLookup("field_to_set")      
      fieldToSet = if ftsOpt.isSome(): unpack[string](ftsOpt.get())
                   else:               ""
    var
      allNames = aliases & @[realName]
      f        = cmdObj.addChoiceFlag(realName, allNames, choices, addFlags,
                                      true, false, doc, cb, fieldToSet)
    f.min = min
    f.max = max

proc loadFlagArgs(cmdObj: CommandSpec, all: AttrScope, info: LoadInfo) =
  for k, v in all.contents:
    let
      realName   = u2d(k)
      one        = v.get(AttrScope)
      aliases    = unpack[seq[string]](one.attrLookup("aliases").get())
      doc        = unpack[string](one.attrLookup("doc").get())
      cbOpt      = one.attrLookup("callback")
      cb         = if cbOpt.isSome(): some(unpack[CallbackObj](cbOpt.get()))
                   else:              none(CallbackObj)
      ftsOpt     = one.attrLookup("field_to_set")      
      fieldToSet = if ftsOpt.isSome(): unpack[string](ftsOpt.get())
                   else:               ""

    var allNames = aliases & @[realName]
      
    cmdObj.addFlagWithArg(realName, allNames, false, false, doc,
                          cb, fieldToSet)
   
proc loadFlagMArgs(cmdObj: CommandSpec, all: AttrScope, info: LoadInfo) =
  for k, v in all.contents:
    let
      realName   = u2d(k)
      one        = v.get(AttrScope)
      aliases    = unpack[seq[string]](one.attrLookup("aliases").get())
      doc        = unpack[string](one.attrLookup("doc").get())
      cbOpt      = one.attrLookup("callback")
      cb         = if cbOpt.isSome(): some(unpack[CallbackObj](cbOpt.get()))
                   else:              none(CallbackObj)
      ftsOpt     = one.attrLookup("field_to_set")      
      fieldToSet = if ftsOpt.isSome(): unpack[string](ftsOpt.get())
                   else:               ""
      

    var allNames = aliases & @[realName]
      
    cmdObj.addFlagWithArg(realName, allNames, true, false, doc,
                          cb, fieldToSet)

proc loadSection(cmdObj: CommandSpec, sec: AttrScope, info: LoadInfo) =
  # The command object was created by the caller.  We need to:
  # 1) Add any flags spec'd.
  # 2) Create any subcommands spec'd.
  let
    commandOpt   = sec.attrLookup(["command"], 0, vlExists).getSec()
    flagYns      = sec.attrLookup(["flag_yn"], 0, vlExists).getSec()
    flagHelps    = sec.attrLookup(["flag_help"], 0, vlExists).getSec()
    flagChoices  = sec.attrLookup(["flag_choice"], 0, vlExists).getSec()
    flagMChoices = sec.attrLookup(["flag_multi_choice"], 0, vlExists).getSec()
    flagArgs     = sec.attrLookup(["flag_arg"], 0, vlExists).getSec()
    flagMArgs    = sec.attrLookup(["flag_multi_arg"], 0, vlExists).getSec()

  if flagYns.isSome():
    cmdObj.loadYn(flagYns.get(), info)
  if flagHelps.isSome():
    cmdObj.loadHelps(flagHelps.get(), info)
  if flagChoices.isSome():
    cmdObj.loadChoices(flagChoices.get(), info)
  if flagMChoices.isSome():
    cmdObj.loadMChoices(flagMChoices.get(), info)
  if flagArgs.isSome():
    cmdObj.loadFlagArgs(flagArgs.get(), info)
  if flagMArgs.isSome():
    cmdObj.loadFlagMArgs(flagMArgs.get(), info)
  if info.addHelpCmds and (commandOpt.isNone() or
                           "help" notin commandOpt.get().contents):
    if commandOpt.isNone():
      cmdObj.subOptional = true
    let help = cmdObj.addCommand("help", unknownFlagsOk = true)
    help.addArgs()
    help.autoHelp = true

  if commandOpt.isNone(): return
  
  let commands = commandOpt.get()
  
  for k, v in commands.contents:
    let
      one      = v.get(AttrScope)
      aliases  = unpack[seq[string]](one.attrLookup("aliases").get())
      argBox   = unpack[seq[Box]](one.attrLookup("args").get())
      minArg   = unpack[int](argBox[0])
      maxArg   = unpack[int](argBox[1])
      doc      = unpack[string](one.attrLookup("doc").get())
      argName  = unpack[string](one.attrLookup("arg_name").get())      
      cbOpt    = one.attrLookup("callback")
      cb       = if cbOpt.isSome(): some(unpack[CallbackObj](cbOpt.get()))
                 else:              none(CallbackObj)
      asubmut  = unpack[bool](one.attrLookup("arg_sub_mutex").get())
      ignoreF  = unpack[bool](one.attrLookup("ignore_all_flags").get())
      ignoreB  = unpack[bool](one.attrLookup("ignore_bad_flags").get())
      sub      = cmdObj.addCommand(k, aliases, not asubmut, ignoreB, ignoreF,
                                   doc, argName, cb)
    sub.addArgs(minArg, maxArg).loadSection(one, info)

proc stringizeFlags(inflags: Table[string, FlagSpec], id: int):
                     Table[string, string] =
  for f, spec in inflags:
    case spec.kind
    of afPair:        result[f] = $(spec.boolValue[id])
    of afChoice:      result[f] = $(spec.selected[id][0])
    of afMultiChoice: result[f] = spec.selected[id].join(",")
    of afStrArg:      result[f] = spec.strVal[id]
    of afMultiArg:    result[f] = spec.strArrVal[id].join(",")

proc stringizeFlags*(winner: ArgResult): Table[string, string] =
  return winner.flags.stringizeFlags(winner.parseCtx.parseId)

template heading(s: string, color = acMagenta): string =
  toAnsiCode(color) & s & toAnsiCode(acReset)

proc showUsage(cmd: CommandSpec) =
  var cmdName, flags, argName, subs: string

  if cmd.reportingName == "":
    cmdName = getAppFilename().splitPath().tail
  else:
    cmdname = cmd.reportingName.replace(".", " ")

  if cmd.maxArgs == 0:
    argName = ""
  else:
    for i in 0 ..< cmd.minArgs:
      argName &= cmd.argName & " "
    if cmd.minArgs != cmd.maxArgs:
      argName &= "[" & cmd.argName & "] "
      if cmd.maxArgs == high(int):
        argName &= "..."
      else:
        argName &= "(0, " & $(cmd.maxArgs - cmd.minArgs) & ") "

  if len(cmd.flags) != 0: flags = "[FLAGS]"
    
  if len(cmd.commands) != 0:
    if cmd.subOptional: subs = "[COMMANDS]"
    else:               subs = "COMMAND"
    
  let use = "Usage: " & cmdname & " " & flags & " " & argName & subs
  echo heading(use, color = acBRed)
  
  
proc instantTable*(cells: seq[string]): string =
  #  TODO: push this into texttable.
  var
    remainingWidth         = terminalWidth()
    numcol                 = 0
    rows: seq[seq[string]]
    row:  seq[string]      = @[]
  
  # Find the ideal with. 
  for item in cells:
    remainingWidth = remainingWidth - len(item) - 2
    if remainingWidth < 0: break
    numcol = numcol + 1
  if numcol == 0: numcol = 1
      
  for i, item in cells:
    if i != 0 and i mod numcol == 0:
      rows.add(row)
      row = @[]
    row.add(item)

  var n = len(cells)
  while n mod numcol != 0:
    row.add("")
    n = n + 1
      
  rows.add(row)
    
  var outTbl = tableC4mStyle(numcol, rows)
  outTbl.setNoHeaders()
  outTbl.setNoFormatting()
  
  return outTbl.render()
  
proc showCommandList(cmd: CommandSpec) =
  var cmds: seq[string]
  
  for k, sub in cmd.commands:
    if sub.reportingName notin cmds:
      cmds.add(sub.reportingName)

  cmds.sort()
  
  echo heading("Available Commands: ")
  stdout.write(instantTable(cmds))
  echo heading("See ... [COMMAND] help for info on each command.",
                           color = acBold)
  echo()
proc addDash(s: string): string =
  if len(s) == 1: return "-" & s
  else:            return "--" & s
    
proc showFlagHelp(cmd: CommandSpec) =
  var
    flagList: seq[string]
    rows:     seq[seq[string]] = @[@["Flag", "Description"]]
    row:      seq[string]      = @[]
    aliases:  seq[string]
    numFlags: int
    fstr:     string
                                     
  
  for k, spec in cmd.flags:
    if not k.startswith("->"):
      flagList.add(k)

  if len(flaglist) == 0: return

  flagList.sort()
  
  for k in flagList:
    let spec = cmd.flags[k]
    numFlags = len(spec.recognizedNames)
    if spec.reportingName in spec.recognizedNames:
      fstr = spec.reportingName.addDash()
      aliases = @[]
      for item in spec.recognizedNames:
        if item != spec.reportingName:
          aliases.add(item.addDash())
          
    else:
      fstr = spec.recognizedNames[0].addDash()
      aliases = @[]
      for item in spec.recognizedNames[1 .. ^1]:
        aliases.add(item.addDash())

    case spec.kind
    of afPair:
      if spec.reportingName notin spec.positiveNames:
        # TODO... implement this branch.  Don't need for chalk tho.
        discard
      else:
        fstr    = spec.reportingName.addDash()
        aliases = @[]
        for item in spec.positiveNames:
          if item != spec.reportingName:
            aliases.add(item.addDash())
        if len(aliases) != 0:
          fstr &= "\nor: " & aliases.join(", ")
        rows.add(@[fstr, spec.doc])
        if len(spec.negativeNames) != 0:
          aliases = @[]
          fstr = spec.negativeNames[^1].addDash()
          for item in spec.negativeNames[0 ..< ^1]:
            aliases.add(item.addDash())
          if len(aliases) != 0:
            fstr &= "\nor: " & aliases.join(", ")
          rows.add(@[fstr, "Does the opposite of the row above."])
    of afChoice:
      fstr &= "= " & spec.choices.join(" | ")
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.join(", ")
      rows.add(@[fstr, spec.doc])
    of afMultiChoice:
      fstr &= "= " & spec.choices.join(",")
      if spec.min == spec.max:
        fstr &= "(select " & $(spec.min) & ") "
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.join(", ")
      rows.add(@[fstr, spec.doc])
    of afStrArg:
      fstr &= "= ARG"
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.join(", ")
      rows.add(@[fstr, spec.doc])
    of afMultiArg:
      fstr &= "= ARG,ARG,..."
      if len(aliases) != 0:
        fstr &= "\nor: " & aliases.join(", ") 
      rows.add(@[fstr, spec.doc])

  var outTbl = tableC4mStyle(2, rows, wrapStyle = WrapLinesHang)
  
  echo heading("Flags: ")
  echo outTbl.render()

proc showOneCmdHelp(cmd: CommandSpec) =
  showUsage(cmd)
  echo cmd.doc.perLineWrap()
  
  if len(cmd.commands) != 0:
     cmd.showCommandList()
    
  cmd.showFlagHelp()
 
proc showCmdHelp*(cmd: CommandSpec, args: seq[string]) =

  if len(args) == 0:
    showOneCmdHelp(cmd)
  else:
    var legitCmds: seq[(string, string)] = @[]
    
    for item in args:
      if item in cmd.commands and cmd.commands[item].reportingName == item:
        legitCmds.add((item, item))
      else:
        var found = false
        for sub, spec in cmd.commands:
          if item in spec.allNames:
            legitCmds.add((item, spec.reportingName))
            found = true
            break
        if not found:
          echo("No such command: " & item)

    if len(legitCmds) == 0:
      showOneCmdHelp(cmd)
    else:
      for (given, reporting) in legitCmds:
        if given != reporting:
          echo heading("Note: '" & given & "' is an alias for '" &
            reporting & "'", color = acBCyan)
          
        showOneCmdHelp(cmd.commands[reporting])
    
  quit(0)

proc managedCommit(winner: ArgResult, runtime: ConfigState) =
  let
    parseId = winner.parseCtx.parseId
    endCmd  = winner.parseCtx.finalCmd
  
  for flag, spec in winner.flags:
    let
      val = case spec.kind
            of afPair:        pack(spec.boolValue[parseId])
            of afChoice:      pack(spec.selected[parseId][0])
            of afMultiChoice: pack(spec.selected[parseId])
            of afStrArg:      pack(spec.strVal[parseId])
            of afMultiArg:    pack(spec.strArrVal[parseId])
    if spec.callback.isSome():
        let
          retBox = runtime.sCall(spec.callback.get(), @[val]).get()
          ret    = unpack[string](retbox)
        if ret != "":
          raise newException(ValueError, ret)
          
    if spec.fieldToSet != "":
      if not runtime.setOverride(spec.fieldToSet, some(val)):
        raise newException(ValueError, "Couldn't apply override to field " &
                           spec.fieldToSet)

  var
    cmdObj  = endCmd
    cmdName = winner.command
  while true:
    if cmdObj.callback.isSome():
      let args = @[pack(winner.args[cmdName])]
      discard runtime.sCall(cmdObj.callback.get(), args)
    if cmdObj.autoHelp:
      showCmdHelp(cmdObj.parent.get(), winner.args[cmdName])
    let parts = cmdName.split(".")
    cmdName = parts[0 ..< ^1].join(".")
    if cmdObj.parent.isNone(): break
    cmdObj = cmdObj.parent.get()

  let
    specTop     = winner.stashedTop
    cmdAttrBox  = specTop.attrLookup("command_attribute")
    flagAttrBox = specTop.attrLookup("flag_attribute")
    argAttrBox  = specTop.attrLookup("arg_attribute")

  if cmdAttrBox.isSome():
    discard runtime.attrSet(unpack[string](cmdAttrBox.get()),
                            pack(winner.command))
  if argAttrBox.isSome():
    discard runtime.attrSet(unpack[string](argAttrBox.get()), pack(winner.args))
  if flagAttrBox.isSome():
    let flags = winner.flags.stringizeFlags(parseId)
    discard runtime.attrSet(unpack[string](flagAttrBox.get()), pack(flags))

proc finalizeManagedGetopt*(runtime: ConfigState, options: seq[ArgResult]):
                          ArgResult =
  var matchingCmds: seq[string] = @[]
  let
    spectop    = options[0].stashedTop
    cmdAttrBox = spectop.attrLookup("command_attribute")

  if cmdAttrBox.isSome():
    let cmd = unpack[string](cmdAttrBox.get())
    for item in options:
      let thisCmd = item.command.split(".")[0]
      if cmd == thisCmd:
        item.managedCommit(runtime)
        return item
      else:
        matchingCmds.add(thisCmd)

  raise newException(ValueError,
                     "No explicit command provided in arguments, " &
                       "and multiple commands match: " &
                       matchingCmds.join(", "))

proc runManagedGetopt*(runtime:      ConfigState,
                       args:         seq[string],
                       getoptsPath = "getopts"): seq[ArgResult] =
  # By this point, the spec should be validated, making the
  # checks for getopts() correctness unneeded.
  let aOrE = runtime.attrs.attrLookup(getoptsPath.split("."), 0, vlExists)
  if aOrE.isA(AttrErr):
    raise newException(ValueError, "Specified getopts section not found: " &
                       getOptsPath)
  let aOrS = aOrE.get(AttrOrSub)
  if aOrS.isA(Attribute):
    raise newException(ValueError, "The getopts path is a field not a section")
  let
    sec     = aOrS.get(AttrScope)
    argsOpt = sec.attrLookup("args")
    cmdSec  = sec.attrLookup(["command"], 0, vlExists)

  var
    minArg = 0
    maxArg = 0
    commandScope: AttrScope = nil
    li     = LoadInfo()

  if cmdSec.isA(AttrOrSub):
    let aOrS = cmdSec.get(AttrOrSub)
    commandScope = aOrS.get(AttrScope)
    if len(commandScope.contents) == 0:
      maxArg = high(int) # No commands provided, so default is to allow any args

  if argsOpt.isSome():
    let boxSeq = unpack[seq[Box]](argsOpt.get())
    minArg = unpack[int](boxSeq[0])
    maxArg = unpack[int](boxSeq[1])

  let
    defaultOpt  = sec.attrLookup("default_command")
    yesBox      = sec.attrLookup("default_yes_prefixes").get()
    noBox       = sec.attrLookup("default_no_prefixes").get()
    docOnErrBox = sec.attrLookup("show_doc_on_err").get()
    errorCmdBox = sec.attrLookup("error_command").get()
    addHelpBox  = sec.attrLookup("add_help_commands").get()
    doc         = unpack[string](sec.attrLookup("doc").get())
    argName     = unpack[string](sec.attrLookup("arg_name").get())
    
    
  if defaultOpt.isNone(): li.defaultCmd = none(string)
  else:                   li.defaultCmd = some(unpack[string](defaultOpt.get()))

  li.defaultYesPref = unpack[seq[string]](yesBox)
  li.defaultNoPref  = unpack[seq[string]](noBox)
  li.showDocOnErr   = unpack[bool](docOnErrBox)
  li.errCmd         = unpack[string](errorCmdBox)
  li.addHelpCmds    = unpack[bool](addHelpBox)
  
  let topLevelCmd = newSpecObj(minArgs = minArg, maxArgs = maxArg, doc = doc,
                                                           argName = argName)
  topLevelCmd.loadSection(sec, li)

  result = topLevelCmd.ambiguousParse(args, defaultCmd = li.defaultCmd)
  for item in result:
    # We need to look up some items in this scope in managedCommit;
    # it's the top-level getopts() scope in the specification context.
    item.stashedTop = sec  

  if len(result) == 1:
    result[0].managedCommit(runtime)

