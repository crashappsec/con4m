## Routines for specifying a config file schema, and for checking an
## executed config against that schema.
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options
import tables
import strutils
import unicode
import strformat
import streams

import types
import parse
import st
import treecheck
import typecheck
import eval
import dollars
import nimutils/box
import nimutils/unicodeid


proc newConfigSpec*(customTopLevelOk: bool = false): ConfigSpec =
  ## Returns a new, empty ConfigSpec object, which allows us to define
  ## a schema for our config files that we will validate after loading
  ## the file.
  ##
  ## By default, the top-level section will not accept new
  ## user-defined attributes added to it.  Users get variables, so
  ## generally they shouldn't need it in the top-level space.
  return ConfigSpec(customTopLevelOk: customTopLevelOk)

proc addGlobalAttr*(spec: ConfigSpec,
                    name: string,
                    con4mType: string,
                    default: Option[Box] = none(Box),
                    required: bool = true,
                    lockOnWrite: bool = false,
                    v: FieldValidator = nil,
                    doc: string = "") =
  ## This call specifies properties of specific attributes set in the
  ## global namespace. By default (unless you set `customTopLevelOK`
  ## to false when calling `newConfigSpec`), user-defined attributes
  ## will NOT be allowed in the global namespace.  They do get
  ## user-defined variables that don't bubble up to your app, though!
  ##
  ## Right now, this is the biggest wart in con4m. I was going to have
  ## there only be an `addAttr()` API, but the section would have to
  ## have a back-reference to the top, and I thought that would be
  ## confusing.  But I think eventually that's where I'm going to go,
  ## as I don't like the irregularity.
  ##
  ## The `doc` parameter is for doc strings, which is currently not
  ## used, but will be used down the road when we merge in supporting
  ## command-line argument handling, and start providing help
  ## messages.

  if spec.globalAttrs.contains(name):
    raise newException(ValueError, "Global attribute already has a spec")

  if not name.isValidId():
    raise newException(ValueError, "Name is not a valid identifier")

  let validator: Option[FieldValidator] = if v != nil: some(v)
                                          else: none(FieldValidator)
  let attr = AttrSpec(doc: doc,
                      attrType: con4mType,
    defaultVal: default,
    lockOnWrite: lockOnWrite,
    required: required,
    validator: validator)

  spec.globalAttrs[name] = attr

proc addSection*(spec: ConfigSpec,
                 name: string,
                 doc: string = "",
                 requiredSubSecs: seq[string] = @[],
                 validSubSecs: seq[string] = @[],
                 allowCustomAttrs: bool = false): SectionSpec =
  ## Adds information about the section to our specification.  Note
  ## that currently, we limit section schema specs to the top-level
  ## space.  That limitation will eventually change, but right now,
  ## this con4m code:
  ##
  ## key "test" "foo" "bar" {
  ##  test: 10
  ## }
  ##
  ## is invalid, because the namespace for the descriptors at the
  ## start of the block is the same as the namespace for attributes
  ## inside the block.  I did it this way because HCL does, then
  ## realized it's confusing AF, so instead of kludging a way to have
  ## both approaches interoperate, I decided to not implement nested
  ## sections quite yet; I'm going to come back and fix it properly by
  ## giving both concepts their own namespace, essentially.
  ##
  ## But it's a lot of work, so until I prioritize it, you cannot do:
  ##
  ## host "test" {
  ##    subsec {
  ##       foo : 10
  ##    }
  ## }
  ##
  ## Because that currently is the same as:
  ##
  ## host "test" subsec {
  ##   foo: 10
  ## }
  ##
  ## Actually, you *can* do it, but it must have the same schema at
  ## the parent.
  ##
  ## Currently, `requiredSubSecs` and `validSubSecs` are a list of
  ## allowed values for the sub-block descritors (the `"test" "foo"
  ## "bar"` above).  An asterisk in one position allows anything in
  ## that position.  You cannot match arbitrary lengths.  Use dot
  ## notation here.
  ##
  ## `allowCustomAttrs` can be turned on, but it's off by default,
  ## because, hey, Con4m has a separate set of variables.
  ##
  ## The `doc` parameter is for doc strings, which is currently not
  ## used, but will be used down the road when we merge in supporting
  ## command-line argument handling, and start providing help
  ## messages.

  if spec.secSpecs.contains(name):
    raise newException(ValueError, "Cannot redefine section {name}.".fmt())

  if not name.isValidId():
    raise newException(ValueError, "Name is not a valid identifier")

  result = SectionSpec(doc: doc,
                       requiredSubsections: requiredSubSecs,
                       allowedSubsections: validSubSecs,
                       customAttrs: allowCustomAttrs,
                       associatedSpec: spec)

  spec.secSpecs[name] = result

proc addSection*(parent: SectionSpec,
                 name: string,
                 doc: string = "",
                 requiredSubSecs: seq[string] = @[],
                 validSubSecs: seq[string] = @[],
                 allowCustomAttrs: bool = false): SectionSpec =
  ## Same as above, but just delegates to the associatedSpec field to
  ## allow these things to be chained.
  return parent.associatedSpec.addSection(name,
                                          doc,
                                          requiredSubSecs,
                                          validSubSecs,
                                          allowCustomAttrs)

proc addAttr*(section: SectionSpec,
              name: string,
              con4mType: string,
              default: Option[Box] = none(Box),
              required: bool = true,
              lockOnWrite: bool = false,
              v: FieldValidator = nil,
              doc: string = "") =
  ## Same as `globalAddAttr`, except for the first parameter being the
  ## section to put the attr into, not the global state.  We're going
  ## to fix this :(
  if section.predefinedAttrs.contains(name):
    raise newException(ValueError, "Attribute already has a spec")

  if not name.isValidId():
    raise newException(ValueError, "Name is not a valid identifier")

  let validator: Option[FieldValidator] = if v != nil: some(v)
                                          else: none(FieldValidator)
  let attr = AttrSpec(doc: doc,
                      attrType: con4mType,
    defaultVal: default,
    lockOnWrite: lockOnWrite,
    required: required,
    validator: validator)

  section.predefinedAttrs[name] = attr


proc containsFields(scope: Con4mScope): bool =
  for n, e in scope.entries:
    if e.subscope.isNone():
      return true

  return false

proc containsSubscopes(scope: Con4mScope): bool =
  for n, e in scope.entries:
    if e.subscope.isSome():
      return true
  return false

type ValidState = enum
  Invalid, ValidData, PathMatch

proc checkOneSectionSpec(spec: string, stack: seq[string]): ValidState =
  let parts = spec.split('.')

  if parts.len() < stack.len(): return Invalid

  for i in 0 ..< stack.len():
    if parts[i] == "*": continue # Wait this makes no sense.
    if cmpRunesIgnoreCase(parts[i], stack[i]) != 0:
      return Invalid

  if stack.len() == parts.len():
    return ValidData
  else:
    return PathMatch

proc okayToBeHere(specs, stack: seq[string], scope: Con4mScope): bool =
  # Validate that this section existing makes sense, based on the
  # spec.  Note that the spec identifies what can contain DATA, and
  # this scope may not contain data.  Still, even if it doesn't, we
  # need to make sure there COULD be a legitimate data-containing
  # scope under us.

  let thisScopeContainsData = scope.containsFields()

  for spec in specs:
    case spec.checkOneSectionSpec(stack)
    of Invalid: continue
    of ValidData:
      return true
    of PathMatch:
      if not thisScopeContainsData:
        return true

proc validateAttr(ctx: ConfigState,
                  stack: seq[string],
                  entry: STEntry,
                  fields: FieldAttrs,
                  customOk: bool) =


  # 1. If not customOk, does the field name exist in the list of okay fields?
  # 2. Is there actually a value set?  If not, no actual problem here.
  # 3. Does the type in the symbol table match what was spec'd?
  # 4. If there's a validator callback, call it!
  # 5. Are we supposed to lock this field now that it's set? If so, do it.

  let
    symbol = stack[^1]

  if not customOk and not fields.contains(symbol):
    ctx.errors.add("{stack.join(\".\")} is a custom key in a section ".fmt() &
                   "that does not accept custom keys.")
    return

  if entry.value.isNone():
    # This can happen when code isn't evaluated, for instance due to a
    # conditional. In such a case, the attribute never got set. It's not
    # an error condition, but we also don't want to end up marking the
    # symbol as having been seen.
    return

  let
    box = entry.value.get()
    spec = fields[symbol]
    t1 = entry.tInfo
    t2 = spec.attrType.toCon4mType()
    unified = unify(t1, t2)

  if unified.isBottom():
    ctx.errors.add("The value of {stack.join(\".\")} is not of the ".fmt() &
                   "right type ({$(t1)} vs {$(t2)})".fmt())
    return

  if spec.validator.isSome():
    let f = spec.validator.get()
    if not f(stack, box):
      ctx.errors.add(
        """{stack.join(".")} didn't pass its validation check""".fmt())
      return

  if spec.lockOnWrite:
    entry.locked = true

proc requiredFieldCheck(ctx: ConFigState,
                        scope: Con4mScope,
                        attrs: FieldAttrs,
                        scopeName: string) =
  # Fill in fields tha were not provided, when there are defaults we
  # can fill in.  Otherwise, if fields are required, error.
  if scope.containsSubscopes() and not scope.containsFields():
    if scopeName != "<global>":
      return

  for key, specEntry in attrs:
    if scope.entries.contains(key):
      let entry = scope.entries[key]
      if entry.value.isSome():
        continue
      else:
        if specEntry.defaultVal.isSome():
          entry.value = specEntry.defaultVal
          scope.entries[key] = entry
        else:
          if specEntry.required:
            ctx.errors.add("In {scopeName}: Required symbol {key}".fmt() &
              "not found")
          continue
    else:
      if specEntry.defaultVal.isNone():
        if specEntry.required:
          ctx.errors.add("In {scopeName}: ".fmt() &
                         "Required symbol {key} not found".fmt())
        else:
          # Add in an empty value.
          discard scope.addEntry(key, tinfo = specEntry.attrType.toCon4mType())
      else:
        let
          opt = scope.addEntry(key, tinfo = specEntry.attrType.toCon4mType())
          entry = opt.get()
        entry.value = specEntry.defaultVal
        scope.entries[key] = entry

proc validateScope(ctx: ConfigState,
                   scope: Con4mScope,
                   stack: seq[string],
                   myState: SectionState) =
  let sname = stack[0]

  myState.beenSeen = true

  # Top-level sections MUST be pre-specified when validation is
  # invoked.  So when we're looking at a top-level section (ie, when
  # there's only one name on the stack), make sure there's a section
  # spec.
  if stack.len() == 1:
    if not ctx.spec.get().secSpecs.contains(sname):
      ctx.errors.add("Invalid top-level section in config: {sname}".fmt())
      return

  let
    spec = ctx.spec.get().secSpecs[sname]
    customOk = spec.customAttrs

  if len(stack) > 1 and
     not okayToBeHere(spec.requiredSubsections, stack[1 .. ^1], scope) and
     not okayToBeHere(spec.allowedSubsections, stack[1 .. ^1], scope):
    ctx.errors.add("Invalid section: {stack.join(\".\")}".fmt())
    return

  for key, entry in scope.entries:
    var pushed = stack
    pushed.add(key)

    if entry.subscope.isSome():
      let secState = SectionState()
      myState.subStateObjs[key] = secState
      ctx.validateScope(entry.subscope.get(), pushed, secState)
    else:
      ctx.validateAttr(pushed, entry, spec.predefinedAttrs, customOk)

  # Next check required fields.
  # Add a default value in, if need be.
  requiredFieldCheck(ctx, scope, spec.predefinedAttrs, stack.join("."))


proc validateConfig*(config: ConfigState): bool =
  ## This function validates the executed configuration file against
  ## the specification set in the `config` variable (which you should
  ## have already set with `addSpec()`).
  ##
  ## Note that, unlike static errors and the few possible runtime
  ## errors (currently just index out of bounds errors) this does
  ## *NOT* throw an exception on error.  Instead, the config context
  ## will contain a list of error strings.
  ##
  ## This makes it easier you to decide whether you want to error
  ## outright, or overcome the mistakes somehow, as we don't bail on
  ## our work in the middle of it.
  let
    scope = config.st
    optSpec = config.spec

  if optSpec.isNone():
    raise newException(ValueError,
                       "Attempting to validate a configuration against " &
                       "a specification, but no specification has been set " &
                       "for this config.")
  let spec = optSpec.get()

  #Check required fields for the global scope, adding in defaults if
  #needed.
  requiredFieldCheck(config, scope, spec.globalAttrs, "<global>")

  # We walk through scopes that have actually appeared, and
  # compare what we see in those scopes vs. what we expected.

  for key, entry in scope.entries:
    if entry.subscope.isSome():
      let secState = SectionState()
      config.stateObjs[key] = secState
      config.validateScope(entry.subscope.get(), @[key], secState)
    else:
      config.validateAttr(@[key],
                          entry,
                          spec.globalAttrs,
                          spec.customTopLevelOk)

  # Then check for missing required sections.
  for cmd, spec in spec.secSpecs:
    for targetSection in spec.requiredSubsections:
      if targetSection.contains("*"):
        config.errors.add("Required section spec cannot contain " &
                          "wildcards (spec {targetSection})".fmt())
        continue
      let parts = targetSection.split(".")
      if dottedLookup(scope, parts).isNone():
        config.errors.add("Required section not provided: {targetSection}".fmt())

  if config.errors.len() == 0:
    return true

proc stackBase(s: ConfigState, tree: Con4mNode): Option[Con4mScope] =
  if tree == nil: return none(Con4mScope)
  s.errors = @[]
  tree.checkTree(s)

  s.pushRuntimeFrame()
  try:
    tree.evalNode(s)
  finally:
    discard s.popRuntimeFrame()

  if s.spec.isSome():
    if not s.validateConfig():
      return none(Con4mScope)

  return some(tree.scopes.get().attrs)
  
proc stackConfig*(s: ConfigState,
                  stream: Stream,
                  filename: string): Option[Con4mScope] =
  stackBase(s, parse(stream, filename))


proc stackConfig*(s: ConfigState, filename: string): Option[Con4mScope] =
  stackBase(s, parse(filename))
    

proc getConfigVar*(state: ConfigState, field: string): Option[Box] =
  ## This interface allows you to look up individual fields to get
  ## their value as a Box (since the config schema doesn't need to be
  ## static).
  ##
  ## The contents of `field` use standard object dot notation.
  let
    parts = field.split('.')
    optEntry = state.st.dottedLookup(parts)

  if optEntry.isNone():
    return

  let entry = optEntry.get()

  if entry.override.isSome():
    return entry.override

  return entry.value

type Con4mSectInfo = seq[(string, string, Con4mScope)]

proc lockConfigVar*(state: ConfigState, field: string): bool =
  ## This prevents the variable from being written.  Returns false if
  ## the variable doesn't exist in the scope, which will generally
  ## imply a programmer error.
  let
    parts = field.split('.')
    optEntry = state.st.dottedLookup(parts)

  if optEntry.isNone():
    return false

  let entry = optEntry.get()
  entry.locked = true
  return true

proc setOverride*(state: ConfigState, field: string, value: Box): bool =
  ## This indicates that, while the executing config file might be
  ## able to write to a field, some higher power (generally a
  ## command-line flag) is going to clobber with this value, as soon
  ## as execution completes.
  ##
  ## Will return false if the specified variable doesn't exist in the
  ## scope, which will generally imply a programmer error.
  let
    parts = field.split('.')
    optEntry = state.st.dottedLookup(parts)

  if optEntry.isNone():
    return false

  let entry = optEntry.get()
  entry.override = some(value)
  return true

proc walkSTForSects(toplevel: string,
                    path: string,
                    scope: Con4mScope,
                    s: var Con4mSectInfo) =

  if scope.containsFields():
    s.add((toplevel, path, scope))

  for n, e in scope.entries:
    if e.subscope.isNone():
      continue

    let newpath = if path != "":
                    path & "." & n
                  else:
                    n

    walkSTForSects(toplevel, newpath, e.subscope.get(), s)

proc getAllSectionSTs*(ctx: ConfigState): Con4mSectInfo =
  ## Returns a sequence of tuples, one per section provided in
  ## a config file that's been read in.
  ##
  ## The tuples are of the format (`topsection`, `dotted path`, `scope`)
  ##
  ## Where scope is an object of type `Con4mScope`


  result = @[]

  for k, entry in ctx.st.entries:
    if entry.subscope.isSome():
      walkStForSects(k, "", entry.subscope.get(), result)

proc addSpec*(s: ConfigState, spec: ConfigSpec) =
  ## Associate a ConfigSpec object with an existing state object.
  s.spec = some(spec)

