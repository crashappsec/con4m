import strutils, parse, vm

proc getRootSection*(spec: ValidationSpec): SectionSpec {.exportc, cdecl.} =
  if spec.rootSpec != nil:
    result = spec.rootSpec
  else:
    result        = SectionSpec()
    spec.rootSpec = result
    initDict(result.fields)

proc newSpec*(): ValidationSpec {.exportc, cdecl.} =
  result = ValidationSpec()
  initDict(result.secSpecs)

proc addField*(sec: SectionSpec, name: string, typeInfo: string,
               doc: Rope = nil, shortDoc: Rope = nil,
              lockOnWrite = false, validators = seq[Validator](@[]),
              hidden = false) =

  if sec.fields.lookup(name).isSome():
    raise newException(ValueError, "Field '" & name & "' is already " &
                                   "defined in this section.")

  let f = FieldSpec(name: name, tid: typeInfo.parseType(),
                    lockOnWrite: lockOnWrite,
                    validators: validators, doc: doc, shortDoc: shortdoc,
                    hidden: hidden, fieldKind: FsField)

  sec.fields[name] = f

proc getFieldInfo*(spec: ValidationSpec, parts: seq[string]): FieldSpec =
  ## Returns none() if the path is invalid. If the path is valid, the
  ## returned field spec will contain one of the following in the
  ## `fieldKind` field:
  ##
  ## - `FsField`: This is the actual field object instance set for a
  ##              section. We do not copy it; consider the state
  ##              immutable.
  ##
  ## - `FsUserDefField`: The encompasing section allows user-defined
  ##                     fields. We do not keep this object around.
  ##
  ## - `FsObjectType`: The path leads us to a section that can have
  ##                   objects underneath it. Any name right beyond
  ##                   this name will be ok.
  ##
  ## - `FsSingleton`: The name is valid, and represents a singleton.
  ##
  ## - `FsObjectInstance`: The name is valid, and points to an object
  ##                       name (not the type, or the fields).

  if spec == nil:
    return FieldSpec(fieldKind: FsErrorNoSpec)

  var
    curSec = spec.rootSpec
    i      = 0

  while true:
    let
      curName  = parts[i]
      fieldOpt = curSec.fields.lookup(curName)

    if fieldOpt.isSome():
      if i != parts.len() - 1: # Can't be a field and a section.
        return FieldSpec(fieldKind: FsErrorSecUnderField,
                           errIx: i)
      else:
        # We don't stick field spec objects inside specs unless they're
        # for actual fields, so no need to check on the contents.
        return fieldOpt.get()

    let secOpt = spec.secSpecs.lookup(curName)
    if secOpt.isNone():
      # Name isn't a field or a section, but check to see if
      # user-defined fields are okay.
      if i == parts.len() - 1:
        if curSec.userDefOk:
          return FieldSpec(fieldKind: FsUserDefField)
        else:
          return FieldSpec(fieldKind: FsErrorFieldNotAllowed)
      else:
        return FieldSpec(fieldKind: FsErrorNoSuchSec, errIx: i)

    if curName notin curSec.allowedSections:
      return FieldSpec(fieldKind: FsErrorSecNotAllowed, errIx: i)

    curSec = secOpt.get()

    if i == parts.len() - 1:
      if curSec.maxAllowed == 1:
        return FieldSpec(fieldKind: FsSingleton)
      else:
        return FieldSpec(fieldKind: FsObjectType)

    if curSec.maxAllowed != 1:
      i += 1
      if i == parts.len() - 1:
        return FieldSpec(fieldKind: FsObjectInstance)
    i = i + 1

proc allow*(spec: ValidationSpec, sec: SectionSpec,
               allowed: varargs[string]) =
  for item in allowed:
    let other = spec.secSpecs.lookup(item)
    if other.isNone():
      raise newException(ValueError, "No section defined yet named '" & item &
                                     "'")
    sec.allowedSections.add(item)

proc baseNewSection(spec: ValidationSpec, n: string,
                    allowed = seq[string](@[]),
                    validators = seq[Validator](@[]), hidden = false,
                    doc = Rope(nil), shortdoc = Rope(nil),
                    userDefOk = false): SectionSpec =

  result = SectionSpec(name: n, maxAllowed: high(int), shortdoc: shortdoc,
                       userDefOk: userDefOk, hidden: hidden, doc: doc)
  initDict(result.fields)

  # TODO: add seems not to be working. Is this picking up some nim thing?
  #if not spec.secSpecs.add(n, result):
  #  raise newException(ValueError, "Section named '" & n & "' already exists.")
  # Until then, you might blow away the old result!
  spec.secSpecs[n] = result

  for item in allowed:
    spec.allow(result, item)

proc newSingleton*(spec: ValidationSpec, n: string,
                   allowed = seq[string](@[]),validators = seq[Validator](@[]),
                   hidden = false, doc = Rope(nil), shortdoc = Rope(nil),
                   userDefOk = false): SectionSpec =
  result = baseNewSection(spec, n, allowed, validators, hidden, doc,
                          shortdoc, userDefOk)
  result.maxAllowed = 1


proc newInstanceSection*(spec: ValidationSpec, n: string,
                         allowed = seq[string](@[]),
                         validators = seq[Validator](@[]), hidden = false,
                         doc = Rope(nil), shortdoc = Rope(nil),
                         userDefOk = false): SectionSpec {.cdecl, exportc.} =
  result = baseNewSection(spec, n, allowed, validators, hidden, doc,
                          shortdoc, userDefOk)

proc oneChoiceValidator*(attrs: AttrDict, path: string, t: TypeId,
                         val: Option[pointer], args: seq[pointer]):
                           Rope {.exportc, cdecl.} =
  # This validator doesn't care about whether fields are required.
  if val.isNone():
    return nil

  var box = val.get()

  if not t.isBasicType():
    raise newException(ValueError,
                       "Choice validator only works with primitive types.")

  for item in args:
    if item.call_eq(box, t):
      return nil

  result = atom("Invalid choice for field: ") + em(path) +
           atom(". Got value: ") + em($(box.call_repr(t))) +
           atom(", but valid choices are: ")

  for i, item in args:
    result += em($(item.call_repr(t)))
    if i != args.len() - 1:
      result += atom(", ")

proc sectionValidator*(attrs: var AttrDict, path: string, t: TypeId,
                        val: Option[pointer],
                        args: seq[pointer]): Rope {.exportc, cdecl.} =
  # Todo-- need to implement this; call the user-defined function.
  discard

proc rangeValidator*(attrs: var AttrDict, path: string, t: TypeId,
                        val: Option[pointer],
                        args: seq[pointer]): Rope {.exportc, cdecl.} =
  # Todo-- need to implement this; call the user-defined function.
  discard

proc choiceValidator*(attrs: var AttrDict, path: string, t: TypeId,
                        val: Option[pointer],
                        args: seq[pointer]): Rope {.exportc, cdecl.} =
  # Todo-- need to implement this; call the user-defined function.
  discard


proc mutexValidator*(attrs: var AttrDict, path: string, t: TypeId,
                     val: Option[pointer], args: seq[pointer]): Rope
                     {.exportc, cdecl.} =
  let
    parts = path.split(".")
    base  = if len(parts) == 1: "" else: parts[0 ..< 1].join(".") & "."
    this  = parts[^1]

  # No conflict if the value isn't set.
  if val.isNone():
    return

  for item in args:
    let
      toCmp      = extractRef[string](item)
      toCmpFull  = base & toCmp
      valOpt     = attrs.lookup(toCmpFull)

    if valOpt.isSome():
      result = atom("Fields ") + em(this) + atom(" and ") + em(toCmp) +
               atom(" may not appear together in this section.")

template specTypeErr(fullPath: string, foundType: TypeId,
                     expectedType: TypeId) =
  if foundType != TBottom:
    lateError("SpecFieldType", @[fullPath, foundType.toString(),
                                 expectedType.toString()])

# TODO: warn on extraneous fields in the spec.
proc add_field(ctx: RuntimeState, path: string, sec: SectionSpec) =
  var
    err:       bool
    foundType: TypeId
    fieldType: TypeId
    fieldName = path.split(".")[^1]
    field     = FieldSpec(name: fieldName)
    tspec     = tTypeSpec()

  field.tid = cast[TypeId](ctx.get(path & ".type", err, addr fieldType,
                                          expectedType = tspec))
  if err:
    specTypeErr(path & ".type", fieldType, expectedType = tspec)
    lateError("RequiredProp", @[path, "type"])

  field.lockOnWrite = cast[bool](
    ctx.get(path & ".write_lock", err, addr foundType,
                                     expectedType = TBool))
  if err:
    specTypeErr(path & ".write_lock", foundType, expectedType = TBool)

  field.defaultVal = ctx.get(path & ".default", err, addr foundType,
                                    expectedType = fieldType)
  if err:
    specTypeErr(path & ".default", foundType, expectedType = fieldType)
  else:
    field.haveDefault = true

  let require = ctx.get(path & ".require", err, addr foundType,
                               expectedType = TBool)
  if err:
    specTypeErr(path & ".require", foundType, expectedType = TBool)
  elif require != nil:
    field.required = true

  field.hidden = cast[bool](ctx.get(path & ".hidden", err, addr foundType,
                                expectedType = TBool))
  if err:
    specTypeErr(path & ".hidden", foundType, TBool)

  let range = ctx.get(path & ".range", err, addr foundType,
                             expectedType = tTuple(@[TInt, TInt]))
  if err:
    specTypeErr(path & ".range", foundType, tTuple(@[TInt, TInt]))
  else:
    field.validators.add(Validator(fn: cast[pointer](rangeValidator),
                                   params: @[range]))

  let
    fValidatorT = tFunc(@[TString, fieldType, TRich])
    validator   = ctx.get(path & ".validator", err, addr foundType,
                                   expectedType = fValidatorT)
  if err:
    specTypeErr(path & ".validator", foundType, fValidatorT)
  else:
    field.validators.add(Validator(fn: cast[pointer](sectionValidator),
                                   params: @[validator]))

  let choices = ctx.get(path & ".choice", err, expectedType = tList(fieldType))

  if err:
    specTypeErr(path & ".choice", foundType, tList(fieldType))
  else:
    field.validators.add(Validator(fn: cast[pointer](choiceValidator),
                                   params: @[choices]))

  sec.fields[path] = field
  echo "Added field, ", path

  # TODO: required ...
proc load_one_section_spec(ctx: RuntimeState, path: string, sec: SectionSpec) =
  var
    err:        bool
    foundType: TypeId


  if sec.name != "" and ctx.obj.spec.secSpecs.lookup(sec.name).isSome():
    lateError("DupeSection", @["sec.name"])

  ctx.obj.spec.secSpecs[sec.name] = sec

  # TODO: validate these.
  sec.allowedSections = ctx.get_section_contents(path & ".allow")
  sec.userDefOk = cast[bool](ctx.get(path & ".user_def_ok", err,
                            addr foundType, expectedType = TBool))
  if err:
    specTypeErr(path & ".user_def_ok", foundType, TBool)

  sec.hidden = cast[bool](ctx.get(path & ".hidden", err,
                                  addr foundType, expectedType = TBool))

  if err:
    specTypeErr(path & ".hidden", foundType, TBool)

  let
    secValidatorT = tFunc(@[TString, TRich])
    validator     = ctx.get(path & ".validator", err, addr foundType,
                                   expectedType = secValidatorT)
  if err:
    specTypeErr(path & ".validator", foundType, secValidatorT)
  else:
    sec.validators.add(Validator(fn: sectionValidator,
                                  params: @[validator]))
  # TODO: extract doc and shortdoc from the section.
  # TODO: mutual exclusion.

  for field in ctx.get_section_contents(path & ".field"):
    ctx.add_field(path, sec)


proc load_con4m_spec*(ctx: RuntimeState, lock_after = false)
    {.cdecl, exportc.} =

  if ctx.obj.spec != nil and ctx.obj.spec.locked:
    lateError("SpecLock")

  ctx.obj.spec        = newSpec()
  ctx.obj.spec.locked = lock_after

  const
    objectPath    = "_spec.object"
    lObjPath      = objectPath.len()
    singletonPath = "_spec.singleton"
    lSingPath     = singletonPath.len()
    rootPath      = "_spec.root"

  ctx.load_one_section_spec(rootPath, ctx.obj.spec.getRootSection())

  for item in ctx.get_section_contents(singletonPath):
    let secName = item[lSingPath .. ^1]
    ctx.load_one_section_spec(item, newSingleton(ctx.obj.spec, secName))

  for item in ctx.get_section_contents(objectPath):
    let secName = item[lObjPath .. ^1]
    ctx.load_one_section_spec(item, newInstanceSection(ctx.obj.spec, secName))

proc lock_con4m_spec*(ctx: RuntimeState) {.cdecl, exportc.} =
  if ctx.obj.spec != nil:
    ctx.obj.spec.locked = true
  else:
    ctx.obj.spec        = newSpec()
    ctx.obj.spec.locked = true

type
  AttrTreeNode = ref object
    name:     string
    children: Dict[string, AttrTreeNode]

proc newAttrNode(): AttrTreeNode =
  result = AttrTreeNode()
  result.children.initDict()

proc validateField(ctx: RuntimeState, spec: FieldSpec, path: string) =
  let
    opt  = ctx.attrs.lookup(path)
    info = opt.getOrElse(nil)

  if opt.isNone() or not info.isSet:
    if spec.required:
      lateError("MissingField", @[path])
    return

  for validator in spec.validators:
    let
      fn  = cast[FieldValidator](validator.fn)
      err = fn(ctx, path, info.contents, info.tid, validator.params)

    exitOnValidationError(err)

proc validateSection(ctx: RuntimeState, n: AttrTreeNode, spec: SectionSpec,
                     path: string) =
  var sep = if n.name == "": "" else: "."

  # First, check through the fields in the spec.
  for (name, fieldSpec) in spec.fields.items():
    ctx.validateField(fieldSpec, path & sep & name)

  for subSecName in spec.allowedSections:
    let
      subSpecObj = ctx.obj.spec.secSpecs[subSecName]
      subpath    = path & sep & subSecName

    if subSpecObj.maxAllowed == 1: # Singleton.
      let opt = ctx.attrs.lookup(subpath)

      if opt.isSome():
        ctx.validateSection(n.children[subSecName], subSpecObj, subpath)
      else:
        if subSpecObj.minAllowed == 1:
          lateError("MissingSingle", @[subpath])
          # TODO: buffer up errors; currently we exit on first error.
          # return
    else:
      for (item, node) in n.children[subSecName].children.items():
        # 1. Try the attribute lookup. If it's there, freak if it's not
        #    a sub-section itself.
        # 2. Call validateObject on the *object* not the level above it.
        let
          objPath  = subpath & sep & item
          optEntry = ctx.attrs.lookup(objPath)

        if optEntry.isSome():
          let entry = optEntry.get()
          if entry.isSet or entry.tid != TBottom:
            lateError("NonInstantiation", @[subpath, item])

        ctx.validateSection(node, subSpecObj, objPath)
        # The validator for the section gets called for each object
        # processed, not once for all objects
        return

    # Cool, now we look at every symbol. We want to complain for any
    # definite sections that aren't spec'd (and not descend into those
    # obvs).
    #
    # Also, if userDefOk isn't true, we want to error if there are
    # unknown fields.

  var allFoundFields: seq[string]

  for (item, node) in n.children.items():
    var
      subPath  = path & sep & item
      optEntry = ctx.attrs.lookup(subPath)
      entry: AttrContents

    if subPath.startsWith("_"):
      return

    if optEntry.isSome():
      entry = optEntry.get()

    if optEntry.isNone() or not entry.isSet:
      runtimeWarn("NoSecSpec", @[path, item])
      continue

    allFoundFields.add(item)

    if not spec.userDefOk and spec.fields.lookup(item).isNone():
      lateError("InvalidField", @[path, item])

  # Finally, if we have section validators, we want to call them, as
  # the very last thing we do.
  #
  # We pass in the full path to the item, as well as a complete list
  # of all of the fields found (NOT sections).
  #
  # If the validator supplied parameters, those get passed too.

  for validator in spec.validators:
    let
      fn  = cast[SecValidator](validator.fn)
      err = ctx.fn(path, allFoundFields, validator.params)

    exitOnValidationError(err)

proc validateSpec*(ctx: RuntimeState) =
  # Using nested dictionaries in the first version of con4m was easier
  # to manage. What we're going to do is:
  #
  # 1. Build out a tree containing all the attributes we have a
  #    record for.
  #
  # 2. Walk the tree, depth first, validating in-order...

  # Don't run if there's no spec.
  if ctx.obj.spec == nil or ctx.obj.spec.rootSpec == nil:
    return

  var
    keys = ctx.get_all_keys()
    root = newAttrNode()

  for key in keys:
    var
      cur   = root
      parts = key.split(".")

    for item in parts:
      let kidOpt = cur.children.lookup(item)

      if kidOpt.isNone():
        let newNode  = newAttrNode()
        newNode.name = item

        cur.children[item] = newNode
        cur = newNode
      else:
        cur = kidOpt.get()

  ctx.validateSection(root, ctx.obj.spec.rootSpec, "")

proc validate_con4m_spec*() {.cdecl, exportc.} =
  get_con4m_runtime().validateSpec()

proc lock_spec*() {.cdecl, exportc.} =
  get_con4m_runtime().lock_con4m_spec()

proc load_spec*() {.cdecl, exportc.} =
  get_con4m_runtime().load_con4m_spec()

proc apply_spec_defaults*() {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  if ctx.obj.spec != nil:
    ctx.applyOneSectionSpecDefaults("", ctx.obj.spec.rootSpec)

addStaticFunction("validate_spec", validate_con4m_spec)
addStaticFunction("load_spec", load_spec)
addStaticFunction("lock_spec", lock_spec)
addStaticFunction("apply_defaults", apply_spec_defaults)
