import options, tables, strutils, strformat, nimutils, macros, unicode, sugar
import algorithm, builtins, types, typecheck, eval, st, dollars, spec, os

type
  CDocKind* = enum
    CDocHtml, CDocRaw, CDocConsole

  CObjDocs* = object
    sectionDocs: Table[string, string]
    fieldInfo:   OrderedTable[string, TableRef[string, string]]

  # Note, we're currently not supporting all these options.
  CmdLineDocOpts* = object
    userPropsFunc*:    (string) -> seq[(string, string)]
    showShort*:         bool        = true
    showDefaultProps*:  bool        = true
    showLong*:          bool        = true
    showSubs*:          bool        = true
    noteEmptySubs*:     bool        = false
    addDescHeader*:     bool        = true
    commandHeadingStr*: string      = "# "
    descrHeadingStr*:   string      = "### "
    subsecHeadingStr*:  string      = "## "
    recursiveSubPaths*: seq[string] = @["help"]
    showFlags*:         bool        = true
    showNumFlags*:      bool        = true
    subCmdLinks*:       bool        = true
    inlineSubCmds*:     bool        = true
    docKind*:           CDocKind    = CDocConsole
    attrStart*:         string      = "getopts"


const
  defaultCmdLineDocOpts = CmdLineDocOpts()

template ensureNewline() =
  if result[^1] != '\n':
    result.add('\n')

template noSpec() =
  raise newException(ValueError, "No spec found for field: " & path)

proc getSectionSpecOfFqn*(state: ConfigState, pathString: string):
                        Option[Con4mSectionType] =
  if state.spec.isNone():
    raise newException(ValueError, "No specs provided.")

  if pathString == "":
    return some(state.spec.get().rootSpec)

  var offset = 1 # len(parts) - ... to get to the name.
  let
    parts    = pathString.split(".")
    fieldOpt = getOpt[Box](state.attrs, pathString)

  if fieldOpt.isSome():
    offset += 1

  if len(parts) < offset:
    return none(Con4mSectionType)

  let singletonOrInstanceName = parts[len(parts) - offset]

  if singletonOrInstanceName in state.spec.get().secSpecs:
    return some(state.spec.get().secSpecs[singletonOrInstanceName])

  offset += 1

  if len(parts) < offset:
    return none(Con4mSectionType)

  let objName = parts[len(parts) - offset]

  if objName in state.spec.get().secSpecs:
    return some(state.spec.get().secSpecs[objName])
  else:
    return none(Con4mSectionType)

proc getFieldSpec*(state: ConfigState, path: string): FieldSpec =
  let specOpt = state.getSectionSpecOfFqn(path)

  if specOpt.isNone():
    noSpec()

  let
    name = path.split(".")[^1]
    spec = specOpt.get()

  if name notin spec.fields:
    noSpec()

  result = spec.fields[name]

proc docFormat*(s: string, kind = CDocConsole): string =
  case kind
  of CDocHtml:
    return s.markdownToHtml()
    discard
  of CDocRaw:
    return s
  of CDocConsole:
    return stylize(s)
    discard

proc getOneFieldDocs*(state: ConfigState, path: string,
                   docKind = CDocConsole): (string, string) =
  ## For a given field referenced by its path, this looks up the
  ## associated spec object and returns "doc" and "shortdoc" from
  ## those specs, if they exist.
  ##
  ## If the field exists, but the documentation doesn't, we return
  ## empty strings. Errors are only thrown for bad fields.

  let
    spec  = state.getFieldSpec(path)
    long  = spec.doc.getOrElse("").docFormat(docKind)
    short = spec.shortdoc.getOrElse("").docFormat(docKind)
  return

proc extractFieldInfo*(finfo: FieldSpec, docKind: CDocKind):
                     TableRef[string, string] =
  result = newTable[string, string]()

  let eType = finfo.extType
  case eType.kind
  of TypePrimitive, TypeC4TypeSpec:
    result["allowed_type"]  = $(eType.tInfo)
    if eType.range.low != eType.range.high or eType.range.low > 0:
      # This is used only for integers and is inclusive.
      result["min"] = $(eType.range.low)
      result["max"] = $(eType.range.high)
    elif eType.itemCount.low != eType.itemCount.high or eType.itemCount.low > 0:
      # This is used for lists.
      result["min"] = $(eType.itemCount.low)
      result["max"] = $(eType.itemCount.high)
    if len(eType.intChoices) != 0:
      result["choices"] = `$`(eType.intChoices)[1 .. ^1]
    elif len(eType.strChoices) != 0:
      result["choices"] = `$`(eType.strChoices)[1 .. ^1]
  of TypeC4TypePtr:
    result["provides_type_for"] = eType.fieldRef
  of TypeSection:
    discard

  if eType.validator != CallbackObj(nil):
    result["validator_name"] = eType.validator.name
    result["validator_type"] = $(eType.validator.tInfo)

  # Don't use this version... the spec sets the defult value for
  # lockOnWrite but it can be changed per-object.
  # result["write_lock"] = $(finfo.lock)
  result["default"]    = $(finfo.default.getOrElse(pack("<none>")))
  result["exclusions"] = finfo.exclusions.join(", ")
  result["longdoc"]    = finfo.doc.getOrElse("").docFormat(docKind)
  result["shortdoc"]   = finfo.shortdoc.getOrElse("").
                              docFormat(docKind)
  result["hidden"]     = $(finfo.hidden)

  if finfo.minRequired == 0:
    result["required"] = "false"
  else:
    result["required"] = "true"

proc fillFromObj(obj: AttrScope, name: string, info: TableRef[string, string]) =
  let opt = obj.attrLookup([name], 0, vlExists)

  if opt.isA(AttrErr):
    info["is_set"] = "false"
    return

  let
    aOrS = opt.get(AttrOrSub)
    attr = aOrS.get(Attribute)

  if attr.override.isSome():
    info["override_on"] = "true"
    info["value"]       = $(get[Box](attr.override))
    info["is_set"]      = "true"
  elif attr.value.isSome():
    info["override_on"] = "false"
    info["value"]       = $(get[Box](attr.value))
    info["is_set"]      = "true"
  else:
    info["override_on"] = "false"
    info["is_set"]      = "false"

  info["type"]               = $(attr.tInfo)
  info["locked"]             = $(attr.locked)
  info["lock_on_next_write"] = $(attr.lockOnWrite)

proc getAllFieldInfoForObj*(state: ConfigState, path: string,
                            docKind: CDocKind = CDocConsole):
                        OrderedTable[string,
                                     TableRef[string, string]] =
  ## Returns all the field documentation for a specific
  ## 'object'.

  let
    secOpt = state.getSectionSpecOfFqn(path)
    objOpt = state.attrs.getObjectOpt(path)

  if objOpt.isNone():
    raise newException(ValueError, "No object found: " & path)

  if secOpt.isNone():
    raise newException(ValueError, "No spec found for: " & path)

  let
    obj     = objOpt.get()
    secSpec = secOpt.get()

  for k, v in secSpec.fields:
    if v.extType.kind == TypeSection:
      continue

    var fieldInfo = v.extractFieldInfo(docKind)

    obj.fillFromObj(k, fieldInfo)

    result[k] = fieldInfo

template dbug(a, b) = print("<jazzberry>" & a & ": </jazzberry>" & b)

proc getObjectLevelDocs*(state: ConfigState, path: string,
                           docKind = CDocConsole):
                   Table[string, string] =
  ## This returns the sections docs for a particular fully dotted
  ## section (meaning, a fully dotted object).

  ## The "longdoc" and "shortdoc" keys are taken from the section's
  ## fields, and "metalong" and "metashort" are taken from any "doc"
  ## or "shortdoc" fields in the section's spec.

  var secSpec: Con4mSectionType

  let
    obj   = state.attrs.getObject(path)
    doc   = getOpt[string](obj, "doc").getOrElse("")
    short = getOpt[string](obj, "shortdoc").getOrElse("")
    parts = path.split(".")
    specs = state.spec.get()

  result["longdoc"]  = doc.docFormat(docKind)
  result["shortdoc"] = short.docFormat(docKind)

  if path == "":
    return

  if (doc == "" and short == "") or len(parts) == 1:
    if parts[^1] in specs.secSpecs:
      secSpec = specs.secSpecs[parts[^1]]
    if not secSpec.singleton:
      secSpec = Con4mSectionType(nil)

  if secSpec == nil and len(parts) > 1:
    if parts[^2] in specs.secSpecs:
      secSpec = specs.secSpecs[parts[^2]]
      if secSpec.singleton:
        secSpec = Con4mSectionType(nil)

  if secSpec == Con4mSectionType(nil):
    result["metalong"]  = ""
    result["metashort"] = ""
  else:
    let
      mlong  = secSpec.doc.getOrElse("").docFormat(docKind)
      mshort = secSpec.shortdoc.getOrElse("").docFormat(docKind)

    result["metashort"] = mshort
    result["metalong"]  = mlong

proc getAllSubScopes*(scope: AttrScope): OrderedTable[string, AttrScope] =
  for k, v in scope.contents:
    if v.isA(Attribute):
      continue
    result[k] = v.get(AttrScope)

proc getAllObjectDocs*(state: ConfigState, path: string,
                       docKind = CDocConsole): CObjDocs =
  result.sectionDocs = state.getObjectLevelDocs(path, docKind)
  result.fieldInfo   = state.getAllFieldInfoForObj(path, docKind)

proc formatCommandTable(obj:  AttrScope,
                        cmd:  string,
                        opts: CmdLineDocOpts): string =
  result = """
<table>
<tr>
  <th>Command Name</th>
  <th>Description</th>
</tr>
"""
  for k, v in obj.contents:
    let scope = v.get(AttrScope)

    result &= "<tr>\n"
    # TODO: wrap this in an <a href=> element ...
    result &= "  <td>" & k & "</td>\n"
    if "shortdoc" in scope.contents:
      let shortDoc = get[string](scope, "shortdoc")
      # TODO: should get a markdownToHtml applied, but needs to
      # have an 'inline' version.
      result &= "  <td>" & unicode.strip(shortdoc) & "</td>\n"
    else:
      result &= "  <td>None</td>\n"
    result &= "</tr>\n"

  result &= "</table>\n"

proc formatFlag(flagname: string): string =
  if len(flagname) == 1:
    result = "<code>-" & flagname & "</code>"
  else:
    result = "<code>--" & flagname & "</code>"

proc formatAliases(scope: AttrScope, flagname: string,
                   defYes, defNo: seq[string]): string =
  var
    aliases:   seq[string]
    negators:  seq[string]
    formatted: seq[string]

  if "aliases" in scope.contents:
    aliases = get[seq[string]](scope, "aliases")
  elif "yes_aliases" in scope.contents:
    aliases = get[seq[string]](scope, "yes_aliases")

  if "no_aliases" in scope.contents:
    negators = get[seq[string]](scope, "no_aliases")

  if len(defYes) != 0:
    for item in defYes:
      aliases.add(item & "-" & flagname)

  if len(defNo) != 0:
    for item in defNo:
      negators.add(item & "-" & flagname)

  if len(aliases) != 0:
    for item in aliases:
      formatted.add(item.formatFlag())
    result &= "<em>Aliases: </em> " & formatted.join(", ") & "\n<br>\n"
    formatted = @[]

  if len(negators) != 0:
    for item in negators:
      formatted.add(item.formatFlag())
    result &= "<em>Negated by: </em> " & formatted.join(", ") & "\n<br>\n"

proc baseFlag(flagname: string, scope: AttrScope, opts: CmdLineDocOpts,
              extraCol1, extraCol2: string, defYes: seq[string] = @[],
              defNo: seq[string] = @[]): string =
    result &= "<tr>\n  <td>\n"
    result &= "    " & flagName.formatFlag() & "\n    <br>\n    <br>\n"
    result &= scope.formatAliases(flagname, defYes, defNo)
    if extraCol1 != "":
      result &= "    <br>\n" & extraCol1
      ensureNewLine()
    result &= "  </td>\n  <td>\n"

    if "doc" in scope.contents:
      result &= markdownToHtml(get[string](scope, "doc"))
    else:
      result &= "No description available."
    ensureNewLine()
    result &= "<br>"
    if "field_to_set" in scope.contents:
      result &= "    <em>Sets config field: <code>"
      result &= get[string](scope, "field_to_set") & "</code></em>\n"
    if extraCol2 != "":
      result &= "<br>\n" & extraCol2
      ensureNewLine()
    result &= "  </td>\n</tr>\n"

proc formatYnFlags(scope: AttrScope, opts: CmdLineDocOpts,
                   defaultYes, defaultNo: seq[string]): string =
  for k, v in scope.contents:
    let subscope = v.get(AttrScope)
    result &= k.baseFlag(subscope, opts, "", "", defaultYes, defaultNo)

const
  reqArg      = "Flag requires an argument"
  reqArgMulti = "Flag can be a comma-separated list, or provided multiple times"

proc formatArgFlags(scope: AttrScope, opts: CmdLineDocOpts): string =
  for k, v in scope.contents:
    let subscope = v.get(AttrScope)
    result &= k.baseFlag(subscope, opts, "", reqArg)

proc formatMultiArgFlags(scope: AttrScope, opts: CmdLineDocOpts): string =
  for k, v in scope.contents:
    let subscope = v.get(AttrScope)
    result &= k.baseFlag(subscope, opts, "", reqArgMulti)

proc formatChoiceFlags(scope: AttrScope, opts: CmdLineDocOpts,
                            multi = false): string =
  var
    choiceText: string
    formatted:  seq[string]
    choices:    seq[string]
    flag:       bool # flag for adding per-choice flags.

  for k, v in scope.contents:
    let subscope = v.get(AttrScope)
    choices    = get[seq[string]](subscope, "choices")
    flag       = getOpt[bool](subscope, "add_choice_flags").getOrElse(false)
    choicetext = ""

    formatted = @[]
    if flag:
      for item in choices:
        formatted.add(item.formatFlag())
      choiceText &= "Per-choice alias flags: <ul><li>" & formatted.join("</li><li>") & "</li></ul>\n"
      formatted = @[]

    for item in choices:
      formatted.add("<em>" & item & "</em>")
    choiceText &= "Value choices: <ul><li>" & formatted.join("</li><li>") & "</li></ul>\n"

    if multi:
      choiceText &= "<em> Multiple arguments may be provided. </em><br>\n"

    if flag:
      choiceText &= "<em>Flag requires an argument (does not apply to " &
                    "per-choice aliases)</em><br>\n"
    elif not multi:
      choiceText &= "<em>Flag requires an argument.</em><br>\n"

    result &= k.baseFlag(subscope, opts, "", choiceText)

proc formatMultiChoiceFlags(scope: AttrScope, opts: CmdLineDocOpts): string =
   return scope.formatChoiceFlags(opts, true)

proc formatAutoHelpFlag(opts: CmdLineDocOpts): string =
  return """
<tr>
  <td><code>--help</code>
    <br>
    <br>
    <em>Aliases:</em> <code>-h</code>
  </td>
  <td>
    Shows help for this command.
  </td>
</tr>
"""

proc formatFlags(obj: AttrScope, subsects: OrderedTable[string, AttrScope],
                 opts: CmdLineDocOpts, defaultYes: seq[string],
                 defaultNo: seq[string]): string =
  result &= """
<table>
<tr>
  <th>Flag Name</th>
  <th>Description</th>
</tr>
"""
  if "flag_yn" in subsects:
    result &= subsects["flag_yn"].formatYnFlags(opts, defaultYes, defaultNo)
  if "flag_arg" in subsects:
    result &= subsects["flag_arg"].formatArgFlags(opts)
  if "flag_multi_arg" in subsects:
    result &= subsects["flag_multi_arg"].formatMultiArgFlags(opts)
  if "flag_choice" in subsects:
    result &= subsects["flag_choice"].formatChoiceFlags(opts)
  if "flag_multi_choice" in subsects:
    result &= subsects["flag_multi_choice"].formatMultiChoiceFlags(opts)
  if "flag_help" in subsects:
    result &= formatAutoHelpFlag(opts)

proc formatProps(obj: AttrScope, cmd: string, opts: CmdLineDocOpts): string =
  var props: seq[(string, string)]

  if opts.showDefaultProps:
    let
      aliasOpts = getOpt[seq[string]](obj, "aliases")
      argOpts   = getOpt[seq[Box]](obj, "args")

    if aliasOpts.isSome():
      var fmtAliases: seq[string]

      for item in aliasOpts.get():
        fmtAliases.add("<code>" & item & "</code>")

      props.add(("aliases", fmtAliases.join(", ")))
    else:
      props.add(("aliases", "none"))

    if argOpts.isNone():
      props.add(("arguments", "none"))
    else:
      let
        vals = argOpts.get() # Will contain 2 items.
        vmin = unpack[int](vals[0])
        vmax = unpack[int](vals[1])

      if vmin == vmax:
        if vmin == 0:
          props.add(("arguments", "none"))
        else:
          props.add(("arguments", $(vmin) & " (exactly)"))
      elif vmax > (1 shl 32):
        case vmin
        of 0:
          props.add(("arguments", "not required; any number okay"))
        else:
          props.add(("arguments", $(vmin) & " required; more allowed"))
      else:
        props.add(("arguments", $(vmin) & " to " & $(vmax)))

  if opts.userPropsFunc != nil:
    props &= opts.userPropsFunc(cmd)

  if len(props) != 0:
    result &= "<table>"
    for (k, v) in props:
      result &= "<tr>\n  <td>" & k & "</td>\n  <td>" & v & "</td>\n</tr>"
    result &= "</table>"

proc getCommandDocs*(state: ConfigState, cmd = "",
                     opts = defaultCmdLineDocOpts): string =

  var
    attrPath: string = opts.attrStart

  if cmd != "":
    for item in cmd.split("."):
      attrPath &= ".command." & item
  let
    obj      = state.attrs.getObject(attrPath)
    objDocs  = state.getAllObjectDocs(attrPath, CDocRaw)
    subsects = obj.getAllSubScopes()
    short    = objDocs.sectionDocs["shortdoc"]
    long     = objDocs.sectionDocs["longdoc"]
    subCmd   = if '.' in cmd: true else: false

  result = opts.commandHeadingStr

  if cmd != "":
    result &= cmd
    if subCmd:
      result &= " subcommand"
    else:
      result &= " command"
  else:
    result &= getMyAppPath().splitPath().tail

  result &= "\n\n"

  if opts.showShort:
    result &= opts.descrHeadingStr
    if short != "":
       result &= short
    else:
      result &= "Top-level command documentation"

  result &= "\n\n"

  result &= obj.formatProps(cmd, opts)

  result &= "\n\n"

  if opts.showSubs:
    if "command" notin subsects:
      if opts.noteEmptySubs:
        result &= "\n\n" & opts.subsecHeadingStr & "\n\n Subcommands\nNone\n"
    else:
      result &= subsects["command"].formatCommandTable(cmd, opts)

  result &= "\n\n"

  result &= "\n\n"

  if opts.showLong:
    if opts.addDescHeader:
      result &= opts.subsecHeadingStr & "Description" & "\n"
    if long != "":
      result &= strutils.strip(long)
    else:
      result &= """
This is the default documentation for your command. If you'd like to
change it, set the 'shortdoc' field to set the title, and the 'doc'
field to edit this description.  You can use Markdown with embedded
HTML, and it will get rendered appropriately, whether at the command
line, or in generated HTML docs.
"""

  result &= "\n\n"

  if opts.showflags:
    let
      yesAttr    = opts.attrStart & ".default_yes_prefixes"
      noAttr     = opts.attrStart & ".default_no_prefixes"
      defaultYes = getOpt[seq[string]](state.attrs, yesAttr).getOrElse(@[])
      defaultNo  = getOpt[seq[string]](state.attrs, noAttr).getOrElse(@[])

    result &= opts.subsecHeadingStr & "Flags" & "\n"
    result &= obj.formatFlags(subsects, opts, defaultYes, defaultNo)



  return result.docFormat(opts.docKind)
