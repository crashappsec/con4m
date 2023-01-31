## Reading from a con4m file, generate a ConfigSpec object to use for
## checking some *other* con4m file.  (⊙ꇴ⊙)
##
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2023

import tables, strformat, options, streams, nimutils
import types, run, spec, st,  errmsg

proc buildC42Spec*(): ConfigSpec =
  # We're going to read a con4m file in from users with their specification
  # for what is allowed when parsing THEIR config files.  We're going to
  # read that in and compare *their* structure to what *we* expect, so
  # this is the spec object validating THEIR spec.
  result = newSpec()
  let
    rootScope = result.getRootSpec()
    rootSec   = result.sectionType("root", singleton = true)
    field     = result.sectionType("field")
    singleton = result.sectionType("singleton")
    obj       = result.sectionType("object")
    require   = result.sectionType("require")
    allow     = result.sectionType("allow")

  rootSec.addSection("field")
  rootSec.addSection("require")
  rootSec.addSection("allow")
  rootSec.addAttr("user_def_ok", boolType, true)
  singleton.addSection("field")
  singleton.addSection("require")
  singleton.addSection("allow")
  singleton.addAttr("user_def_ok", boolType, true)
  obj.addSection("field")
  obj.addSection("require")
  obj.addSection("allow")
  obj.addAttr("user_def_ok", boolType, true)

  rootScope.addSection("root", min = 1, max = 1)
  rootScope.addSection("singleton")
  rootScope.addSection("object")

  field.addAttr("type", stringType, true)
  # TODO: implement this.
  # field.addAttr("default", newTypeVar(), false)
  field.addAttr("require", boolType, true)
  field.addAttr("write_lock", boolType, false)

  require.addAttr("write_lock", boolType, false)
  allow.addAttr("write_lock", boolType, false)

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

proc populateFields(spec:  ConfigSpec,
                    tInfo: Con4mSectionType,
                    scope: AttrScope) =
  for k, v in scope.contents:
    let
      fields     = v.get(AttrScope).contents
      c4mTypeStr = unpack[string](fields["type"].get(Attribute).value.get())
      c4mType    = toCon4mType(c4mTypeStr)
      require    = unpack[bool](fields["require"].get(Attribute).value.get())
      lock       = if "write_lock" in fields:
                     unpack[bool](fields["write_lock"].get(
                       Attribute).value.get())
                   else:
                     false

    tInfo.addAttr(k, c4mType, require, lock)

proc populateType(spec: ConfigSpec, tInfo: Con4mSectionType, scope: AttrScope) =
  if "field" in scope.contents:
    spec.populateFields(tInfo, scope.contents["field"].get(AttrScope))
  if "require" in scope.contents:
    spec.populateSec(tinfo, scope.contents["require"].get(AttrScope), true)
  if "allow" in scope.contents:
    spec.populateSec(tinfo, scope.contents["allowed"].get(AttrScope), false)
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

