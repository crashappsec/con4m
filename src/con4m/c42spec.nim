## Reading from a con4m file, generate a ConfigSpec object to use for
## checking some *other* con4m file.  (⊙ꇴ⊙)
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import tables, strformat, options, streams, nimutils
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

  field.addAttr("type", stringType, true)
  field.addAttr("default", newTypeVar(), true)
  field.addAttr("require", boolType, true)
  field.addExclusion("default", "require")
  field.addAttr("write_lock", boolType, false)

  require.addAttr("write_lock", boolType, false)
  allow.addAttr("write_lock", boolType, false)
  exclusions.addAttr("*", stringType, false)

# TODO: make sure everything used as a section type is a valid ID
proc populateSec(spec:    ConfigSpec,
                 tinfo:   Con4mSectionType,
                 scope:   AttrScope,
                 require: bool) =
  let minSz = if require: 1 else: 0

  for k, v in scope.contents:
    if k notin spec.secSpecs:
      raise newException(ValueError, fmt"No section type named '{k}' defined")
    let
      fields = v.get(AttrScope).contents
      lock   = if "write_lock" in fields:
                 unpack[bool](fields["write_lock"].get(Attribute).value.get())
               else:
                 false
    tinfo.addSection(k, min = minSz, lock = lock)

proc populateFields(spec:       ConfigSpec,
                    tInfo:      Con4mSectionType,
                    scope:      AttrScope,
                    exclusions: seq[(string, string)]) =
  for k, v in scope.contents:
    let
      fields     = v.get(AttrScope).contents
      c4mTypeStr = unpack[string](fields["type"].get(Attribute).value.get())
      c4mType    = toCon4mType(c4mTypeStr)
      lock       = if "write_lock" in fields:
                     unpack[bool](fields["write_lock"].get(
                       Attribute).value.get())
                   else:
                     false
    if "require" in fields and "default" in fields:
      raise newException(ValueError,
                         "Cannot have 'require' and 'default' together")
    if "require" in fields:
      let require = unpack[bool](fields["require"].get(Attribute).value.get())
      tInfo.addAttr(k, c4mType, require, lock)
    elif "default" in fields:
      let
        attr        = fields["default"].get(Attribute)
        attrTypeStr = $(attr.tInfo)
      if unify(c4mType, attr.tInfo).isBottom():
        raise newException(ValueError,
                           fmt"for {k}: default value actual type " &
                           fmt"({attrTypeStr}) does not match the provided" &
                           fmt"'type' field, which had type: {c4mTypeStr}")
      tInfo.addAttr(k, c4mType, true, lock, attr.value)
    else:
      raise newException(ValueError, "'require' is required (unless " &
                                     "a default value is provided instead")
  for (k, v) in exclusions:
    if k notin tInfo.fields:
      raise newException(ValueError, fmt"In {scope.name}: cannot exclude " &
                                     fmt"undefined field {k}")
    if v notin tInfo.fields:
      raise newException(ValueError, fmt"In {scope.name}: cannot exclude " &
                                     fmt"undefined field {v}")
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
    raise newException(ValueError, "Can't have exclusions without fields!")
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

proc c42Spec*(s: Stream, fileName: string): Option[ConfigSpec] =
  let (cfgContents, success) = firstRun(s, fileName, buildC42Spec())

  if not success:
    return none(ConfigSpec)

  let
    res      = newSpec()
    contents = cfgContents.attrs.contents
  result     = some(res)

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

proc c42Spec*(filename: string): Option[ConfigSpec] =
  var s = newFileStream(filename)

  if s == nil:
    fatal(fmt"Unable to open file '{filename}' for reading")

  return c42Spec(s.readAll().newStringStream(), filename)

