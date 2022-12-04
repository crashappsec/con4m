import options
import tables
import strutils
import unicode
import strformat

import con4m_types
import unicodeident
import st
import typecheck
import typerepr
import builtins


proc newConfigSpec*(customTopLevelOk: bool = false): ConfigSpec =
  return ConfigSpec(customTopLevelOk: customTopLevelOk)

proc addGlobalAttr*(spec: ConfigSpec,
                    name: string,
                    con4mType: string,
                    default: Option[Box] = none(Box),
                    required: bool = true,
                    lockOnWrite: bool = false,
                    v: FieldValidator = nil,
                    doc: string = "") =
  
  if spec.globalAttrs.contains(name):
    raise newException(ValueError, "Global attribute already has a spec")

  if not name.isValidId():
    raise newException(ValueError, "Name is not a valid identifier")

  let validator: Option[FieldValidator] = if v != nil: some(v)
                                          else: none(FieldValidator)
  let attr = AttrSpec(doc: doc,
                      attrType: con4mType, # TODO, make this an actual type
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
  if spec.secSpecs.contains(name):
    raise newException(ValueError, "Cannot redefine.")

  if not name.isValidId():
    raise newException(ValueError, "Name is not a valid identifier")
    
  result = SectionSpec(doc: doc,
                       requiredSubsections: requiredSubSecs,
                       allowedSubsections: validSubSecs,
                       customAttrs: allowCustomAttrs)
  
  spec.secSpecs[name] = result
                                    
proc addAttr*(section: SectionSpec,
              name: string,
              con4mType: string,
              default: Option[Box] = none(Box),
              required: bool = true,
              lockOnWrite: bool = false,
              v: FieldValidator = nil,
              doc: string = "") =
  
  if section.predefinedAttrs.contains(name):
    raise newException(ValueError, "Attribute already has a spec")

  if not name.isValidId():
    raise newException(ValueError, "Name is not a valid identifier")

  let validator: Option[FieldValidator] = if v != nil: some(v)
                                          else: none(FieldValidator)
  let attr = AttrSpec(doc: doc,
                      attrType: con4mType, # TODO, make this an actual type
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
    if parts[i] == "*": continue  # Wait this makes no sense.
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
      ctx.errors.add("{stack.join(\".\")} didn't pass its validation check")
      return

  if spec.lockOnWrite:
    entry.locked = true

proc requiredFieldCheck(ctx: ConFigState,
                        scope: Con4mScope,
                        attrs: FieldAttrs,
                        scopeName: string) =

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
        else:
          if specEntry.required:
            ctx.errors.add("In {scopeName}: Required symbol {key}".fmt() &
              "not found")
          continue
    else:
      if specEntry.defaultVal.isNone():
        if specEntry.required:
          ctx.errors.add("In {scopeName}: Required symbol {key} not found".fmt())
        continue
      else:
        let
          opt = scope.addEntry(key, -1, specEntry.attrType.toCon4mType())
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
      ctx.errors.add("Invalid top-level section in config: {sname}")
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
                          "wildcards (spec {targetSection})")
        continue
      let parts = targetSection.split(".")
      if dottedLookup(scope, parts).isNone():
         config.errors.add("Required section not provided: {targetSection}")

  if config.errors.len() == 0:
    return true

proc newConfigState*(scope: Con4mScope,
                     spec: ConfigSpec = nil,
                     addBuiltins: bool = true
                    ): ConfigState =

  if spec != nil:
    result = ConfigState(st: scope, spec: some(spec))
  else:
    result = ConfigState(st: scope)

  if addBuiltins:
    result.addDefaultBuiltins()    
           
# TODO: stack-configs
proc getConfigVar*(state: ConfigState, field: string): Option[Box] =
  let
    parts = field.split('.')
    optEntry = state.st.dottedLookup(parts)

  if optEntry.isNone():
    return

  return optEntry.get().value

proc getSections*(state: ConfigState, field: string = "") : seq[string] =
  var scope: Con4mScope
  
  if field == "":
    scope = state.st
  else:
    let
      parts = field.split(".")
      optEntry = state.st.dottedLookup(partS)

    if optEntry.isNone():
      return
    let entry = optEntry.get()
    if entry.subscope.isNone():
      return
    scope = entry.subscope.get()

  for k, v in scope.entries:
    if v.subscope.isSome():
      result.add(k)

type Con4mSectInfo = seq[(string, Con4mScope)]

proc walkSTForSects(path: string, scope: Con4mScope, s: var Con4mSectInfo) =
  if scope.containsFields():
    s.add((path, scope))
    
  for n, e in scope.entries:
    if e.subscope.isNone(): continue
    walkSTForSects(path & "." & n, e.subscope.get(), s)
                       
proc getAllSectSTs*(ctx: ConfigState): OrderedTableRef[string, Con4mSectInfo] =
  result = newOrderedTable[string, Con4mSectInfo]()
  
  for k, entry in ctx.st.entries:
    if entry.subscope.isNone():
      continue
    var s: Con4mSectInfo

    s = newSeq[(string, Con4mScope)]()
    walkSTForSects(k, entry.subscope.get(), s)
    result[k] = s 

proc addSpec*(s: ConfigState, spec: ConfigSpec) =
  s.spec = some(spec)
  

#[
var spec = Con4mSpec()

var keySection = spec.addSection("key", allowedSubKeys = @["string", "binary"])
keySection.addAttr("type",
                   string,
                   
                   "The type associated with the key. Must be one of: " &
                   "string, int, bool, binary, list(x), dict(x, y) where " &
                   "x and y represent other valid types"
                   writeOnce: true)
keySection.addAttr("required",
                   "bool",
                   "Whether the key MUST be present")
keySection.addDefault("required", false)
keySection.addAttr("missing_ll",
                   "string",
                   "Log level for when key is not present in input SAMI")
keySection.addDefault("missing_ll", "warn")
keySection.addAttr("system",
                   "bool",
                   "If true, this implementation will only allow itself " &
                   "to set the value of this key.")
keySection.addDefault("system", false)
keySection.addAttr("squash",
                   "bool",
                   "If an existing SAMI is present, a new SAMI will remove " &
                   "the old value.")
keySection.addDefault("squash", true)
keySection.addAttr("standard",
                   "bool",
                   "Is the key part of the specification?")
keySection.addDefault("standard", false)
keySection.addAttr("must_force",
                   "bool",
                   "Even if a plugin can discover the data, the user must " &
                   "explicitly ask to get this key in output SAMIs.")
keySection.addDefault("must_force", false)
keySection.addAttr("priority",
                   "int",
                   "The order in which keys are written, from low to high." &
                   " The minimum value is 0, max is 1000 and values may " &
                   "not repeat. This may be unspecified, in which case " &
                   "the system will assign values near the middle when needed",
                   required: false)
keySection.addAttr("since",
                   "string",
                   "The version of SAMI where the key was added.",
                   required: false)


]#
