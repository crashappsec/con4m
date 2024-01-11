import strutils, parse

proc newSpec*(): ValidationSpec =
  result = ValidationSpec()
  initDict(result.secSpecs)

proc getRootSection*(spec: ValidationSpec): SectionSpec =
  if spec.rootSpec != nil:
    result = spec.rootSpec
  else:
    result        = SectionSpec()
    spec.rootSpec = result
    initDict(result.fields)


proc addField*(sec: SectionSpec, name: string, typeInfo: string,
               doc: Rope = nil, shortDoc: Rope = nil,
              lockOnWrite = false, default = none(pointer),
              addDefaultsBeforeRun = true, validators = seq[Validator](@[]),
              hidden = false) =

  if sec.fields.lookup(name).isSome():
    raise newException(ValueError, "Field '" & name & "' is already " &
                                   "defined in this section.")

  let f = FieldSpec(name: name, tid: typeInfo.parseType(),
                    lockOnWrite: lockOnWrite, defaultVal: default,
                    addDefaultsBeforeRun: addDefaultsBeforeRun,
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
      if curSec.singleton:
        return FieldSpec(fieldKind: FsSingleton)
      else:
        return FieldSpec(fieldKind: FsObjectType)

    if not curSec.singleton:
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

  result = SectionSpec(name: n, singleton: false, shortdoc: shortdoc,
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
  result.singleton = true


proc newInstanceSection*(spec: ValidationSpec, n: string,
                         allowed = seq[string](@[]),
                         validators = seq[Validator](@[]), hidden = false,
                         doc = Rope(nil), shortdoc = Rope(nil),
                         userDefOk = false): SectionSpec =
  result = baseNewSection(spec, n, allowed, validators, hidden, doc,
                          shortdoc, userDefOk)

proc oneChoiceValidator*(attrs: AttrDict, path: string, t: TypeId,
                         val: Option[pointer], args: seq[pointer]):
                           Rope {.cdecl.} =
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


proc requiredValidator*(attrs: var AttrDict, path: string, t: TypeId,
                        val: Option[pointer],
                        args: seq[pointer]): Rope {.cdecl.} =
  if val.isNone():
    return atom("Field ") + em(path) +
           atom(" is a required value, but was not provided.")

proc mutexValidator*(attrs: var AttrDict, path: string, t: TypeId,
                     val: Option[pointer], args: seq[pointer]): Rope {.cdecl.} =
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
