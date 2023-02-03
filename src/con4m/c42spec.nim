## Reading from a con4m file, generate a ConfigSpec object to use for
## checking some *other* con4m file.  (⊙ꇴ⊙)
##
## The most mind-bending thing I've done in a while was in building a
## test case for this, where I wrote a partial implementation of the
## c42 spec. It hurts my brain even thinking about it.
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import tables, strformat, options, streams, nimutils, strutils
import types, run, spec, st,  errmsg, typecheck, dollars

proc buildC42Spec*(): ConfigSpec =
  # We're going to read a con4m file in from users with their specification
  # for what is allowed when parsing THEIR config files.  We're going to
  # read that in and compare *their* structure to what *we* expect, so
  # this is the spec object validating THEIR spec.
  result = newSpec()
  let
    rootScope  = result.getRootSpec()
    rootSec    = result.sectionType("root", singleton = true)
    field      = result.sectionType("field")
    singleton  = result.sectionType("singleton")
    obj        = result.sectionType("object")
    require    = result.sectionType("require")
    allow      = result.sectionType("allow")
    exclusions = result.sectionType("exclusions", singleton = true)

  rootSec.addSection("field")
  rootSec.addSection("require")
  rootSec.addSection("allow")
  rootSec.addSection("exclusions")
  rootSec.addAttr("user_def_ok", boolType, true)
  singleton.addSection("field")
  singleton.addSection("require")
  singleton.addSection("allow")
  singleton.addSection("exclusions")
  singleton.addAttr("user_def_ok", boolType, true)
  obj.addSection("field")
  obj.addSection("require")
  obj.addSection("allow")
  obj.addSection("exclusions")
  obj.addAttr("user_def_ok", boolType, true)

  rootScope.addSection("root", min = 1, max = 1)
  rootScope.addSection("singleton")
  rootScope.addSection("object")

  field.addAttr("type",         stringType, true)
  field.addAttr("default",      newTypeVar(), true)
  field.addAttr("require",      boolType, true)
  field.addAttr("write_lock",   boolType, false)
  field.addAttr("range",        toCon4mType("(int, int)"), false)
  field.addAttr("choice",       toCon4mType("[@T]"), false)
  field.addAttr("validator",    stringType, false)
  field.addAttr("stack_limit",  intType, false)
  field.addAttr("min_items",    intType, false)
  field.addAttr("max_items",    intType, false)
  field.addExclusion("default", "require")
  require.addAttr("write_lock", boolType, false)
  allow.addAttr("write_lock",   boolType, false)
  exclusions.addAttr("*",       stringType, false)

# TODO: make sure everything used as a section type is a valid ID
proc populateSec(spec:    ConfigSpec,
                 tinfo:   Con4mSectionType,
                 scope:   AttrScope,
                 require: bool) =
  let minSz = if require: 1 else: 0

  for k, v in scope.contents:
    if k notin spec.secSpecs:
      specErr(scope, fmt"No section type named '{k}' defined in spec")
    let
      fields = v.get(AttrScope).contents
      lock   = if "write_lock" in fields:
                 unpack[bool](fields["write_lock"].get(Attribute).value.get())
               else:
                 false
    tinfo.addSection(k, min = minSz, lock = lock)

template getField(fields: Table[string, AttrOrSub], name: string): untyped =
  if name notin fields:
    specErr(scope, "Expected a field '" & name & "'")
  let aOrS = fields[name]

  if not aOrS.kind:
    specErr(scope, "Expected '{name}' to be a field, but it is a section")

  var res = aOrS.attr
  res

proc unpackValue[T](scope: AttrScope, attr: Attribute, typeStr: string): T =
  let
    c4Type = toCon4mType(typeStr)
    valOpt = attr.value

  if attr.tInfo.unify(c4Type).isBottom():
    specErr(attr, "Field '" & attr.name & "' should be " & typeStr &
                   ", but got: " & $(attr.tInfo))
  if valOpt.isNone():
    specErr(attr,
            "Expected '" & attr.name & "' to have a value; none was provided")
  let box = valOpt.get()

  try:
    when T is (int, int):
      var box = unpack[seq[Box]](box)
      result = (unpack[int](box[0]), unpack[int](box[1]))
    else:
      result = unpack[T](box)
  except:
    specErr(attr,
            "Wrong type for '" & attr.name & "', expected a '" & typeStr &
              "', but got a '" & $(attr.tInfo) & "'")

template getValOfType(fields:  Table[string, AttrOrSub],
                      name:    string,
                      typeStr: string,
                      nimType: typedesc): untyped =
  unpackValue[nimType](scope, getField(fields, name), typeStr)

template valIfPresent(fields:  Table[string, AttrOrSub],
                      name:    string,
                      c4mType: string,
                      nimType: typedesc,
                      default: untyped): untyped =
  if name in fields:
    getValOfType(fields, name, c4mType, nimType)
  else:
    default

template optValIfPresent(fields:  Table[string, AttrOrSub],
                         name:    string,
                         c4mType: string,
                         nimType: typedesc): untyped =
  if name in fields:
    some(getValOfType(fields, name, c4mType, nimType))
  else:
    none(nimType)

proc populateFields(spec:       ConfigSpec,
                    tInfo:      Con4mSectionType,
                    scope:      AttrScope,
                    exclusions: seq[(string, string)]) =
  for k, v in scope.contents:
    var
      default:    Option[Box] = none(Box)
    let
      # valIfPresent sets defaults that we use even if not passed. The
      # fields using optValIfPresent don't get used if not provided.
      # choiceOpt is a snowflake b/c we have to make a type decision before
      # we pull the value out.
      fields     = v.get(AttrScope).contents
      c4mTypeStr = getValOfType(fields, "type", "string", string) #Not opt.
      lock       = valIfPresent(fields, "write_lock", "bool", bool, false)
      validator  = valIfPresent(fields, "validator", "string", string, "")
      stackLimit = valIfPresent(fields, "stack_limit", "int", int, -1)
      require    = valIfPresent(fields, "require", "bool", bool, false)
      `range?`   = optValIfPresent(fields, "range", "(int, int)", (int, int))
      `min?`     = optValIfPresent(fields, "min_items", "int", int)
      `max?`     = optValIfPresent(fields, "max_items", "int", int)
      choiceOpt  = if "choice" in fields:
                     some(getField(fields, "choice"))
                   else:
                     none(Attribute)

    if "default" in fields:
      if "require" in fields:
        specErr(scope, "Cannot have 'require' and 'default' together")
      let
        attr         = getField(fields, "default")
        attrType     = attr.tInfo
        attrTypeStr  = $(attrType)

      if len(c4mTypeStr) != 0 and c4mTypeStr[0] == '=':
        specErr(scope, "Fields that get their type from other fields may " &
                       "not have a default value")
      elif c4mTypeStr == "typespec":
        var ok = true
        if attrType != stringType or attr.value.isNone():
          ok = false
        else:
          try:
            discard toCon4mType(unpack[string](attr.value.get()))
          except:
            ok = false
        if not ok:
          specErr(scope, "Default values for 'typespec' fields must be " &
                         "valid con4m type strings.")
      else:
        let fieldType = toCon4mType(c4mTypeStr)
        if fieldType.unify(attr.tInfo).isBottom():
          specErr(scope, fmt"for {k}: default value actual type " &
                         fmt"({attrTypeStr}) does not match the provided " &
                         fmt"'type' field, which had type: {c4mTypeStr}")

      default = attr.value # We leave it as a boxed option.
    elif "require" notin fields:
      specErr(scope, "Fields must specify either a 'require' or " &
                     "'defaults' field")

    var count = 0
    if choiceOpt.isSome():                 count = count + 1
    if `range?`.isSome():                  count = count + 1
    if `min?`.isSome() or `max?`.isSome(): count = count + 1

    if count > 1:
      specErr(scope, "Can't specify multiple constraint types on one field.")
    if count != 0 and c4mTypeStr.startsWith("="):
      specErr(scope, "Fields typed from another field can't have constraints")

    # Time to add.
    if choiceOpt.isSome():
      case c4mTypeStr
      of "string":
        let v = unpackValue[seq[string]](scope, choiceOpt.get(), "[string]")
        addChoiceField(tinfo, k, v, require, lock, stackLimit, default,
                       validator)
      of "int":
        let v = unpackValue[seq[int]](scope, choiceOpt.get(), "[int]")
        addChoiceField(tinfo, k, v, require, lock, stackLimit, default,
                       validator)
      else:
        specErr(scope, "Choice field must have type 'int' or 'string'")
    elif `range?`.isSome():
      let (l, h) = `range?`.get()
      tInfo.addRangeField(k, l, h, require, lock, stackLimit, default,
                          validator)
    elif `min?`.isSome() or `max?`.isSome():
      var
        min_val = `min?`.getOrElse(-1)
        max_val = `max?`.getOrElse(-1)
        c4mType: Con4mType
      try:
        c4mType = toCon4mType(c4mTypeStr)
      except:
        specErr(scope, fmt"Invalid con4m type in spec: {c4mTypeStr}")
      tInfo.addBoundedContainer(k, min_val, max_val, c4mType, require,
                                lock, stackLimit, default, validator)
    elif c4mTypeStr == "typespec":
      tInfo.addC4TypeField(k, require, lock, stackLimit, default, validator)
    elif len(c4mTypeStr) != 0 and c4mTypeStr[0] == '=':
      let refField   = c4mTypeStr[1..^1]
      tInfo.addC4TypePtr(k, refField, require, lock, stackLimit, validator)
    else:
      var c4mType: Con4mType
      try:
        c4mType = toCon4mType(c4mTypeStr)
      except:
        specErr(scope, fmt"Invalid con4m type in spec: {c4mTypeStr}")
      tInfo.addAttr(k, c4mType, require, lock, stackLimit, none(Box),
                    validator)

  # Once we've processed all fields, check exclusion constraints.
  for (k, v) in exclusions:
    if k notin tInfo.fields:
      specErr(scope, fmt"Cannot exclude undefined field {k}")
    if v notin tInfo.fields:
      specErr(scope, fmt"Cannot exclude undefined field {v}")
    let
      kAttr = tInfo.fields[k]
      vAttr = tInfo.fields[v]
    if k notin vAttr.exclusions:
      vAttr.exclusions.add(k)
    if v notin kAttr.exclusions:
      kAttr.exclusions.add(v)

proc getExclusions(s: AttrScope): seq[(string, string)] =
  result = @[]

  for k, aOrS in s.contents:
    result.add((k, unpack[string](aOrS.get(Attribute).value.get())))

proc populateType(spec: ConfigSpec, tInfo: Con4mSectionType, scope: AttrScope) =
  let pairs = if "exclusions" in scope.contents:
                getExclusions(scope.contents["exclusions"].get(AttrScope))
              else: seq[(string, string)](@[])
  if "field" in scope.contents:
    spec.populateFields(tInfo, scope.contents["field"].get(AttrScope), pairs)
  elif len(pairs) != 0:
    specErr(scope, "Can't have exclusions without fields!")
  if "require" in scope.contents:
    spec.populateSec(tinfo, scope.contents["require"].get(AttrScope), true)
  if "allow" in scope.contents:
    spec.populateSec(tinfo, scope.contents["allow"].get(AttrScope), false)
  let attr = scope.contents["user_def_ok"].get(Attribute)

  if unpack[bool](attr.value.get()):
    addAttr(tInfo, "*", newTypeVar(), false)


proc registerSingletonType(spec: ConfigSpec, item: AttrOrSub) =
  let objInfo  = item.get(AttrScope)
  spec.sectionType(objInfo.name, singleton = true)

proc registerObjectType(spec: ConfigSpec, item: AttrOrSub) =
  let objInfo  = item.get(AttrScope)
  spec.sectionType(objInfo.name, singleton = false)

proc c42Spec*(s: Stream, fileName: string): Option[(ConfigSpec, ConfigState)] =
  ## Create a ConfigSpec object from a con4m file. The schema is
  ## validated against our c4-2-spec format.
  let (cfgContents, success) = firstRun(s, fileName, buildC42Spec())

  if not success:
    return none((ConfigSpec, ConfigState))

  let
    res      = newSpec()
    contents = cfgContents.attrs.contents
  result     = some((res, cfgContents))

  # Register all types before we populate them, so that we can safely
  # forward-reference; all type names will be registered before we
  # populate.

  if "singleton" in contents:
    for _, singletonSpec in contents["singleton"].get(AttrScope).contents:
      res.registerSingletonType(singletonSpec)

  if "object" in contents:
    for _, objectSpec in contents["object"].get(AttrScope).contents:
      res.registerObjectType(objectSpec)

  if "singleton" in contents:
    for name, singletonSpec in contents["singleton"].get(AttrScope).contents:
      res.populateType(res.secSpecs[name], singletonSpec.get(AttrScope))

  if "object" in contents:
    for name, objectSpec in contents["object"].get(AttrScope).contents:
      res.populateType(res.secSpecs[name], objectSpec.get(AttrScope))

  res.populateType(res.rootSpec, contents["root"].get(AttrScope))

proc c42Spec*(filename: string): Option[(ConfigSpec, ConfigState)] =
  var s = newFileStream(filename)

  if s == nil:
    fatal(fmt"Unable to open specification file '{filename}' for reading")

  return c42Spec(s.readAll().newStringStream(), filename)
