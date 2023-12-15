import basetypes, cbox, strutils

proc newSpec*(): ValidationSpec =
  result = ValidationSpec()
  initDict(result.secSpecs)

proc getRootSection*(spec: ValidationSpec): SectionSpec =
  if spec.rootSpec:
    result = spec.rootSpec
  else:
    result = SectionSpec()
    initDict(result.fields)

proc newField*(sec: SectionSpec, name: string, typeInfo: string,
               doc: Rope, longDoc: Rope,
              lockOnWrite = false, default = none(CBox),
              addDefaultsBeforeRun = true, validators = seq[Validator](@[]),
              hidden = false) =
  result = FieldSpec

proc newAllow*(spec: ValidationSpec, sec: SectionSpec,
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
                    doc = "", shortdoc = "", userDefOk = false): SectionSpec =
  let existingOpt = s.secSpecs.get(n)

  if existingOpt.isSome():
    raise newException("Section named '" & n & "' already exists.")

  result = SectionSpec(name: n, singleton: false, shortdoc: shortdoc,
                       userDefOk: userDefOk, hidden: hidden, doc: doc) =
  initDict(result.fields)

  for item in allowed:
    spec.newAllow(result, item)

  s.secSpecs[n] = result

  return s

proc newSingleton*(spec: ValidationSpec, n: string,
                       allowed = seq[string](@[]),
                       validators = seq[Validator](@[]), hidden = false,
                       doc = "", shortdoc = "", userDefOk = false):
                         SectionSpec =
  result = baseNewSection(spec, n, allowed, validators, hidden, doc,
                          shortdoc, userDefOk)
  result.singleton = true

proc newObjectSection*(spec: ValidationSpec, n: string,
                       allowed = seq[string](@[]), hidden = false,
                       doc = "", shortdoc = "", userDefOk = false):
                         SectionSpec =
  result = baseNewSection(spec, n, allowed, validators, hidden, doc,
                          shortdoc, userDefOk)

proc oneChoiceValidator*(attrs: AttrDict, path: string, val: Option[CBox],
                         args: seq[CBox]): Rope {.cdecl.} =
  # This validator doesn't care about whether fields are required.
  if val.isNone():
    return nil

  var
    box = val.get()
    t   = box.getType()

  if not t.isBasicType():
    raise newException("Choice validator only works with primitive types.")

  # Otherwise, types should be validated already.
  for item in args:
    if item.baseValueEq(box):
      return nil

  result = atom("Invalid choice for field: ") + em(path) +
           atom(". Got value: ") + em(box.builtinRepr()) +
           atom(", but valid choices are: ")

  for i, item in args:
    result + em(item.builtinRepr())
    if i != args - 1:
      result += atom(", ")


proc requiredValidator*(attrs: AttrDict, path: string, val: Option[CBox],
                        args: seq[CBox]): Rope {.cdecl.} =
  if val.isNone():
    return atom("Field ") + em(path) +
           atom(" is a required value, but was not provided.")

proc mutexValidator*(attrs: AttrDict, path: string, val: Option[CBox],
                     args: seq[CBox]): Rope {.cdecl.} =
  let
    parts = path.split(".")
    base  = if len(parts) == 1: "" else: parts[0 ..< 1].join(".") & "."
    this  = parts[^1]

  # No conflict if the value isn't set.
  if val.isNone():
    return

  for item in args:
    let
      toCmp      = unbox[string](item)
      toCmpFull  = base & toCmp
      valOpt     = attrs.lookup(toCmpFull)

    if valOpt.isSome():
      result = atom("Fields ") + em(this) + atom(" and ") + em(toCmp) +
               atom(" may not appear together in this section.")
