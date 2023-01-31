## Routines for specifying a config file schema, and for checking an
## executed config against that schema.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import options, tables, strutils, strformat, nimutils
import errmsg, types, parse, typecheck

proc sectionType*(spec:       ConfigSpec,
                  name:       string,
                  singleton:  bool = false): Con4mSectionType {.discardable.} =
  # Will add this back in at some point, but trying to simplify
  # since I'm not getting as much time on this as I wanted.
  #validNames: seq[string] = []): Con4mSectionType =
  if name in spec.secSpecs:
    raise newException(ValueError, fmt"Duplicate section type name: {name}")
  result = Con4mSectionType(typeName:      name,
                            singleton:     singleton,
                            #validObjNames: validNames,
                            backref:       spec)
  if name != "":
    spec.secSpecs[name] = result

proc addAttr*(sect:     Con4mSectionType,
              name:     string,
              tinfo:    Con4mType,
              required: bool,
              lock:     bool = false): Con4mSectionType {.discardable.} =
  if name in sect.fields:
    raise newException(ValueError, fmt"Duplicate spec: {name}")
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
                     lock:        lock)

  sect.fields[name] = info
  return sect

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
                     lock:        lock)
  sect.fields[typeName] = fs

proc newSpec*(): ConfigSpec =
  result = ConfigSpec()

  result.rootSpec = sectionType(result, "", true)

proc getRootSpec*(spec: ConfigSpec): Con4mSectionType =
  return spec.rootSpec

proc validateOneSection(attrs: AttrScope, spec: Con4mSectionType)

proc validateOneSectField(attrs: AttrScope, name: string, spec: FieldSpec) =
  if name notin attrs.contents:
    if spec.minRequired > 0:
      fatal(fmt"Required section {name} is missing")
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
  if len(sectAttr.contents) < spec.minRequired:
    fatal(fmt"Expected {spec.minRequired} sections of {name}, " &
      fmt"but only have {len(sectAttr.contents)}.")
  if spec.maxRequired != 0 and len(sectAttr.contents) > spec.maxRequired:
    fatal(fmt"Expected no more than {spec.minRequired} sections of {name}, " &
      fmt"but got {len(sectAttr.contents)}.")


proc validateOneAttrField(attrs: AttrScope, name: string, spec: FieldSpec) =
  if name notin attrs.contents:
    if spec.minRequired == 1:
      fatal(fmt"Required attribute {name} is missing.")
    else:
      return
  # The spec says the name is def a con4m type. Make sure it's not a section.
  let aOrS = attrs.contents[name]
  if aOrS.isA(AttrScope):
    fatal(fmt"Expected a field named {name} but got a section.")
  let
    attr = aOrS.get(Attribute)
  if not attr.value.isSome() and not attr.override.isSome():
    if spec.minRequired == 1:
      fatal(fmt"Required attribute {name} is missing.")
  if attr.tInfo.copyType().unify(spec.extType.tinfo.copyType()).isBottom():
    fatal(fmt"Wrong type for {name}")

proc validateOneSection(attrs: AttrScope, spec: Con4mSectionType) =
  # Here we are 'in' a section and need to validate each field.
  for name, fieldspec in spec.fields:
    if fieldspec.extType.kind == TypePrimitive:
      validateOneAttrField(attrs, name, fieldspec)
    else:
      validateOneSectField(attrs, name, fieldspec)
  if "*" notin spec.fields:
    for name, _ in attrs.contents:
      if name notin spec.fields:
        fatal(fmt"Unknown field for a {spec.typeName} section: {name}")

proc validateState*(state: ConfigState) =
  validateOneSection(state.attrs, state.spec.get().rootSpec)
