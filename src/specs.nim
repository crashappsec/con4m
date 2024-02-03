import "."/[common, attrstore]
import ztypes/api

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

proc getFieldInfo*(spec: ValidationSpec, parts: seq[string]): FieldSpec {.
  exportc, cdecl.} =
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

  if spec == nil or not spec.used:
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

proc lock_con4m_spec*(ctx: RuntimeState) {.cdecl, exportc.} =
  if ctx.obj.spec != nil:
    ctx.obj.spec.locked = true
  else:
    ctx.obj.spec        = newSpec()
    ctx.obj.spec.locked = true

proc lock_spec*() {.cdecl, exportc.} =
  get_con4m_runtime().lock_con4m_spec()

proc apply_spec_defaults*() {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  if ctx.obj.spec != nil:
    ctx.applyOneSectionSpecDefaults("", ctx.obj.spec.rootSpec)

addStaticFunction("lock_spec", lock_spec)
addStaticFunction("apply_defaults", apply_spec_defaults)


proc declareTopLevelSpecItems(m: Module) =
  # Whenever we have a spec, we will go through the top-level
  # items and pre-delcare them as attributes, otherwise they will
  # get picked up as variables.
  #
  # We don't currently have to worry about anything beyond
  # the top-level, since those things automatically have to
  # be attributes; the only ambiguity is at the root.

  discard
  #for (fname, fspec) in m.attrSpec.getRootSection().fields.items():
  #  m.usedAttrs.table[

proc mergeStaticSpec*(m: Module) {.cdecl, exportc.} =
  if m.declaredSpec != nil:
    # TODO: add where to the error reporting here.
    if m.attrSpec.locked:
      m.irError("SpecLock")

    for (name, obj) in m.declaredSpec.secSpecs.items():
      if not m.attrSpec.secSpecs.add(name, obj):
        let val = m.attrSpec.secSpecs[name]

        if val != obj:
          m.irError("DupeSection", @[name])

        continue
    let
      allSectionNames = m.attrSpec.secSpecs.keys()
      globalRootSec   = m.attrSpec.getRootSection()
      localRootSec    = m.declaredSpec.getRootSection()

    if not localRootSec.userDefOk:
      globalRootSec.userDefOk = false

    for validator in localRootSec.validators:
      globalRootSec.validators.add(validator)

    if localRootSec.doc != nil:
      if globalRootSec.doc != nil:
        m.irWarn("RootOverwrite", @["doc field"])
      globalRootSec.doc = localRootSec.doc

    if localRootSec.shortdoc != nil:
      if globalRootSec.shortdoc != nil:
        m.irWarn("RootOverwrite", @["shortdoc field"])
      globalRootSec.shortdoc = localRootSec.shortdoc

    for item in localRootSec.requiredSections:
      if item notin allSectionNames:
        m.irError("MissingSec", @[item, "require"])

      if item notin globalRootSec.requiredSections:
        globalRootSec.requiredSections.add(item)

    for item in localRootSec.allowedSections:
      if item notin allSectionNames:
        m.irError("MissingSec", @[item, "allow"])

      if item notin globalRootSec.allowedSections:
        globalRootSec.allowedSections.add(item)

    for (fname, fspec) in localRootSec.fields.items():
      let rOpt = globalRootSec.fields.lookup(fname)
      if rOpt.isSome():
        m.irError("RootOverwrite", @[fname])
      globalRootSec.fields[fname] = fspec


  # The below should now not be necessary; it's all moved.
  # Leaving for now just in case I missed something.
  #
  #for (name, item) in m.usedAttrs.table.items():
  #  let info = m.attrSpec.getFieldInfo(name.split('.'))
  #  if info.fieldKind == FsField:
  #    m.typeCheck(info.tid.tCopy(), item.tid)
  # for item in allows:
  #   if item in sectionG.allowedSections:
  #     ctx.irWarn("DupeAllow", @[item])
  #   elif item in sectionG.requiredSections:
  #     ctx.irWarn("AllowInReq", @[item])
  #   else:
  #     sectionG.allowedSections.add(secName)

  # for item in requires:
  #   if item in sectionG.requiredSections:
  #     ctx.irWarn("DupeRequire", @[item])
  #   else:
  #     sectionG.requiredSections.add(item)
  #     if item in sectionG.allowedSections:
  #       ctx.irWarn("ReqAfterAllow", @[item])
