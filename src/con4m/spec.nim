## Routines for specifying a config file schema, and for checking an
## executed config against that schema.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options, tables, strutils, strformat, nimutils
import errmsg, types, parse, typecheck, st, dollars

proc sectionType*(spec:       ConfigSpec,
                  name:       string,
                  singleton:  bool = false): Con4mSectionType {.discardable.} =
  if name in spec.secSpecs:
    raise newException(ValueError, fmt"Duplicate section type name: {name}")
  result = Con4mSectionType(typeName:      name,
                            singleton:     singleton,
                            backref:       spec)
  if name != "":
    spec.secSpecs[name] = result

proc addAttr*(sect:     Con4mSectionType,
              name:     string,
              tinfo:    Con4mType,
              required: bool,
              lock:     bool = false,
              default:  Option[Box] = none(Box)):
                Con4mSectionType {.discardable.} =
  if name in sect.fields:
    raise newException(ValueError, fmt"Duplicate field name: {name}")
  if "*" in name:
    if name != "*":
      raise newException(ValueError, "Attribute wilcard must be '*' only")
    elif required == true:
      raise newException(ValueError, "Wildcard attr spec can't have value " &
                                     "'required'")

  let
    tobj = ExtendedType(kind: TypePrimitive, tinfo: tinfo)
    info = FieldSpec(extType:     tobj,
                     minRequired: if required: 1 else: 0,
                     maxRequired: 1,
                     default:     default,
                     lock:        lock)

  sect.fields[name] = info
  return sect

proc addC4TypeField*(sect:     Con4mSectionType,
                     name:     string,
                     required: bool = true,
                     lock:     bool = false,
                     default: Option[Box] = none(Box)):
                       Con4mSectionType {.discardable.} =
  if name in sect.fields:
    raise newException(ValueError, fmt"Duplicate field name: {name}")
  if "*" in name:
    raise newException(ValueError, "User-defined fields can't be type fields")
  let
    info = FieldSpec(extType:     ExtendedType(kind: TypeC4TypeSpec),
                     minRequired: if required: 1 else: 0,
                     maxRequired: 1,
                     default:     default,
                     lock:        lock)
  sect.fields[name] = info
  return sect

proc addC4TypePtr*(sect:        Con4mSectionType,
                   name:        string,
                   pointsTo:    string,
                   required:    bool = true,
                   lock:        bool = false):
                     Con4mSectionType {.discardable.} =
  if name in sect.fields:
    raise newException(ValueError, fmt"Duplicate field name: {name}")
  if "*" in name:
    raise newException(ValueError, "User-defined fields can't be type fields")
  let
    tinfo = ExtendedType(kind:     TypeC4TypePtr,
                         fieldRef: pointsTo)
    info  = FieldSpec(extType:     tinfo,
                      minRequired: if required: 1 else: 0,
                      maxRequired: 1,
                      default:     none(Box),
                      lock:        lock)
  sect.fields[name] = info
  return sect

# Use this to add simple mutual exclusions... if we see X, then we're
# not allowed. For instance, in the c42 spec, default: and required:
# are mutually exclusive, but at least one of them is required.
#
# So we mark them as both required and mutually exclusive.
#
# The 'min required' is only enforced when there are no exclusions.
proc addExclusion*(sect: Con4mSectionType, fieldName1, fieldName2: string) =
  # The c42 object / singleton / root types should allow something
  # like:
  #
  # exclusions { field1: field2, field2: field3 }
  if fieldName1 notin sect.fields:
    raise newException(ValueError,
                       fmt"{fieldName1} must exist in section {sect.typeName}" &
                          "before it can be used in an exclusion.")
  if fieldName2 notin sect.fields:
    raise newException(ValueError,
                       fmt"{fieldName2} must exist in section {sect.typeName}" &
                          "before it can be used in an exclusion.")
  var
    field1 = sect.fields[fieldName1]
    field2 = sect.fields[fieldName2]

  # Note: not checking for a double add here.
  field1.exclusions.add(fieldName2)
  field2.exclusions.add(fieldName1)


proc setValidationContext*(spec: ConfigSpec,  ctx: ConfigState) =
  spec.validationCtx = ctx

proc addSectionCheckCallback*(spec: ConfigSpec,
                              sect: string,
                              c4mCallbackName: string) =
  if sect notin spec.secSpecs:
    raise newException(ValueError, fmt"Section {sect} must be added before a " &
                                      "validator can be added.")
  spec.secSpecs[sect].c4Check = some(c4mCallbackName)

proc addSection*(sect:     Con4mSectionType,
                 typeName: string,
                 min:      int  = 0,
                 max:      int  = 0,
                 lock:     bool = false): Con4mSectionType {.discardable.} =

  let knownTypes = sect.backref.secSpecs

  if typeName notin knownTypes:
    raise newException(ValueError, fmt"Undeclared section type: {typeName}")
  if typeName in sect.fields:
    raise newException(ValueError, fmt"Duplicate spec: {typeName}")
  if min < 0 or max < 0:
    raise newException(ValueError, fmt"Values for min and max must be positive")
  if max != 0 and min > max:
    raise newException(ValueError,
                       fmt"Minimum number can't be greater than the maximum")

  let t = knownTypes[typeName]

  if t.singleton and min > 1 or max > 1:
    raise newException(ValueError, fmt"Section {typeName} is a singleton; " &
                                      "min/max fields must be 1 or less")

  var fs = FieldSpec(extType:     ExtendedType(t),
                     minRequired: min,
                     maxRequired: max,
                     default:     none(Box),
                     lock:        lock)
  sect.fields[typeName] = fs

proc newSpec*(): ConfigSpec =
  result = ConfigSpec()

  result.rootSpec = sectionType(result, "", true)

proc getRootSpec*(spec: ConfigSpec): Con4mSectionType =
  return spec.rootSpec

proc validateOneSection(attrs: AttrScope, spec: Con4mSectionType)

proc exclusionPresent(attrs, name, spec: auto): string =
  # Returns any one exclusion from the spec that has a value
  # associated with it in attrs, whether it's an instantiated
  # section (even if empty) or another attribute.
  for item in spec.exclusions:
    if item == name:
      continue # Ignore if we excluded ourselves.
    if item notin attrs.contents:
      continue
    let aOrS = attrs.contents[item]
    if aOrS.isA(AttrScope):
      return item # Once present, attr objects never go away.
    let attr = aOrS.get(Attribute)
    if attr.value.isSome() or attr.override.isSome():
      return item
  return ""

proc validateOneSectField(attrs: AttrScope, name: string, spec: FieldSpec) =
  let exclusion = exclusionPresent(attrs, name, spec)

  if name notin attrs.contents:
    if spec.minRequired > 0 and exclusion == "":
      fatal(fmt"Inside section {attrs.name}: Required section {name} is " &
            "missing, and there are no other fields present that would " &
            "remove this constraint.")
    else:
      return
  let aOrS = attrs.contents[name]
  if aOrS.isA(Attribute):
    fatal(fmt"Expected a section named {name}, but got an attribute.")
  let
    sectAttr = aOrS.get(AttrScope)
    secSpec = spec.extType.sinfo
  if secSpec.singleton:
    validateOneSection(sectAttr, secSpec)
    return
  for k, v in sectAttr.contents:
    if v.isA(Attribute):
      fatal(fmt"Cannot have a singleton for section type: {name}")
    else:
      validateOneSection(v.get(AttrScope), secSpec)

  if exclusion != "":
    if len(sectAttr.contents) > 0:
      fatal(fmt"{name} cannot appear alongside {exclusion}")
  else:
    if len(sectAttr.contents) < spec.minRequired:
      fatal(fmt"Expected {spec.minRequired} sections of {name}, " &
            fmt"but only have {len(sectAttr.contents)}.")
    if spec.maxRequired != 0 and len(sectAttr.contents) > spec.maxRequired:
      fatal(fmt"Expected no more than {spec.minRequired} sections of {name}, " &
            fmt"but got {len(sectAttr.contents)}.")

proc validateOneAttrField(attrs: AttrScope, name: string, spec: FieldSpec) =
  let exclusion = exclusionPresent(attrs, name, spec)

  if name notin attrs.contents:
    if spec.minRequired == 1 and exclusion == "":
      if spec.default.isSome():
        let t = spec.extType.tinfo
        # While we set the default here, it does have to drop down
        # below to properly type check.
        attrs.contents[name] = Attribute(name: name,
                                         scope: attrs,
                                         tInfo: t,
                                         value: spec.default,
                                         override: none(Box))
      else:
        fatal(fmt"Inside field {attrs.name}: Required attribute {name} " &
              "is missing, and there are no other fields present that would " &
              "remove this constraint.")
    else:
      return
  # The spec says the name is def a con4m type. Make sure it's not a section.
  let aOrS = attrs.contents[name]
  if aOrS.isA(AttrScope):
    fatal(fmt"Expected a field named {name} but got a section.")
  let
    attr = aOrS.get(Attribute)
  if not attr.value.isSome() and not attr.override.isSome() and
     spec.minRequired == 1:
    if exclusionPresent(attrs, name, spec) == "":
      if spec.default.isSome():
        attr.value = spec.default
      else:
        fatal(fmt"Required attribute {name} is missing.")
  else:
    if exclusion != "":
      fatal(fmt"In sect {attrs.name}: {name} can't appear with {exclusion}")
  case spec.extType.kind
  of TypePrimitive:
    if attr.tInfo.unify(spec.extType.tinfo.copyType()).isBottom():
      fatal(fmt"Wrong type for {name}")
  of TypeSection:
    unreachable
  of TypeC4TypeSpec:
    discard # Only the referrer needs to validate.
  of TypeC4TypePtr:
    let fieldRef = spec.extType.fieldRef

    if fieldRef notin attrs.contents:
      fatal(fmt"Type for field {name} (section {attrs.name}) is supposed to " &
            fmt"be taken from the '{fieldRef}' field, which was not provided.")

    let refAOrS = attrs.contents[fieldRef]
    if refAOrS.isA(AttrScope):
      fatal(fmt"Expected a field named '{fieldRef}' containing the type " &
            fmt" for the field '{name}'")

    let refAttr = refAOrS.get(Attribute)
    if not refAttr.value.isSome() and not refAttr.value.isSome():
      fatal(fmt"Field '{fieldRef}' is supposed to contain a con4m type for " &
            fmt"field '{name}', but that type is missing.")
    if refAttr.tInfo.unify(stringType).isBottom():
      fatal(fmt"Field '{fieldRef}' is supposed to contain a con4m type for " &
            fmt"field '{name}', but the field is not a valid con4m string.")
    let typeString = unpack[string](refAttr.value.get())
    try:
      let fieldType = typeString.toCon4mType()
      if attr.tInfo.unify(fieldType).isBottom():
        fatal(fmt"Wrong type for {name} (expected {typeString} per " &
              fmt"the type read from field '{fieldRef}'), but got: " &
              fmt"{`$`(attr.tInfo)}")
    except:
      fatal(fmt"When reading a type from field '{fieldRef}' (to type check " &
            fmt"the field '{name}'), got a parse error parsing the type: " &
            getCurrentExceptionMsg())

proc validateOneSection(attrs: AttrScope, spec: Con4mSectionType) =
  # Here we are 'in' a section and need to validate each field.
  for name, fieldspec in spec.fields:
    if fieldspec.extType.kind == TypeSection:
      validateOneSectField(attrs, name, fieldspec)
    else:
      validateOneAttrField(attrs, name, fieldspec)
  if "*" notin spec.fields:
    for name, _ in attrs.contents:
      if name notin spec.fields:
        fatal(fmt"Unknown field for a {spec.typeName} section: {name}")

proc validateState*(state: ConfigState) =
  validateOneSection(state.attrs, state.spec.get().rootSpec)
