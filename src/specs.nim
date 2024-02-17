import "."/[common, attrstore]
import ztypes/api


proc print_one_section(spec: SectionSpec, n: string) =
  var cells: seq[seq[Rope]]

  cells.add(@[text("Name"), em(n)])
  cells.add(@[text("Instantiable?"), if spec.maxAllowed > 1:
                                       fgColor("✓", "atomiclime")
                                     else:
                                       fgColor("✗", "red")])
  cells.add(@[text("User-defined fields?"), if spec.userDefOk:
                                              fgColor("✓", "atomiclime")
                                            else:
                                              fgColor("✗", "red")])
  cells.add(@[text("Allowed sections"),
              if spec.allowedSections.len() == 0:
                em("None")
              else:
                text((spec.allowedSections &
                  spec.requiredSections).join(", "))])

  if spec.requiredSections.len() != 0:
    cells.add(@[text("Required sections"),
                text(spec.requiredSections.join(", "))])

  print quickTable(cells,
                   verticalHeaders = true).colWidths([(40, true),
                           (20, true)]).tpad(0).bpad(0)

  let view = spec.fields.items()

  if view.len() != 0:
    cells = @[@[text("Field"), text("Type"), text("Required"), text("Lock")]]

    for (k, fs) in view:
      cells.add(@[text(k), em(fs.tid.toString()),
                if fs.required:
                  fgColor("✓", "atomiclime")
                else:
                  fgColor("✗", "red"),
                if fs.lockOnWrite:
                  fgColor("✓", "atomiclime")
                else:
                  fgColor("✗", "red")])

    print quickTable(cells)
  else:
    print(h5("Section has no defined fields."))

proc print_spec*(s: ValidationSpec) =
  if not s.used:
    print(h5("No attribute validation specification."))
    return

  print_one_section(s.rootSpec, "Top-level section")

  for (name, sec) in s.secSpecs.items(sort=true):
    print_one_section(sec, name)

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

proc rangeValidator*(ctx: RuntimeState, path: C4Str, t: TypeId, val: pointer,
                     param: FlexArray[pointer]): Rope {.exportc, cdecl.} =
  let
    lowval = cast[int](param[0])

  let
    hival  = cast[int](param[1])
    v      = cast[int](val)

  if v >= lowval and v <= hival:
    return nil # Success!

  else:
    let args = @[$(cast[cstring](path)),$(v), $(lowval), $(hival)]
    return ctx.format_validation_error(path, "BadRange", args)

proc choiceValidator*(ctx: RuntimeState, path: C4Str, t: TypeId, val: pointer,
                      param: FlexArray[pointer]): Rope {.exportc, cdecl.} =
  let
    choices = param.items()

  for item in choices:
    if call_eq(item, val, t) == true:
      return nil # Success

  var choice_list: seq[string]

  for item in choices:
    choice_list.add($(call_repr(item, t)))

  return ctx.format_validation_error(path, "BadChoice",
                                     @[ $(cast[cstring](path)),
                                        $(call_repr(val, t)),
                                        choice_list.join(", ")])

proc userFieldValidator*(ctx: RuntimeState, path: C4Str, t: TypeId,
                         val: pointer, cb: ptr ZCallback): Rope
                                                         {.exportc, cdecl.} =
  # 1. Push the value.
  # 2. Push the path.
  # 3. call run_callback()
  ctx.push_call_param(val, t)
  ctx.push_call_param(cast[pointer](path), TString)

  let cb_result = cast[C4Str](ctx.run_callback(cb))
  if cb_result != nil and cb_result.len() != 0:
    result = ctx.custom_validation_error(path, cb_result, cb)

proc userSectionValidator*(ctx: RuntimeState, path: C4Str, f: pointer,
                           cb: ptr ZCallback): Rope
                             {.exportc, cdecl.} =
  # 1. Push the list of fields.
  # 2. Push the path.
  # 3. call run_callback()
  ctx.push_call_param(f, tList(TString))
  ctx.push_call_param(cast[pointer](path), TString)

  let cb_result = cast[C4Str](ctx.run_callback(cb))
  if cb_result != nil and cb_result.len() != 0:
    result = ctx.custom_validation_error(path, cb_result, cb)

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
      if item notin globalRootSec.requiredSections:
        globalRootSec.requiredSections.add(item)

    for item in localRootSec.allowedSections:
      if item notin globalRootSec.allowedSections:
        globalRootSec.allowedSections.add(item)

    for (fname, fspec) in localRootSec.fields.items():
      let rOpt = globalRootSec.fields.lookup(fname)
      if rOpt.isSome():
        m.irError("RootOverwrite", @[fname])
      globalRootSec.fields[fname] = fspec

proc validate_section(ctx: RuntimeState, tree: AttrTree, spec: SectionSpec,
                      errs: var seq[Rope]) =
  ## To validate a section we must:
  ## 1. Look for sections that are present, but are not allowed.
  ## 2. Look for required subsections that do not exist.
  ## 3. Recurse into subsections to validate.
  ## 3. Look for fields that exist, but shouldn't.
  ## 4. Look for fields that don't exist, but should. (noting exclusions)
  ## 5. Check types of any fields with deferredType set in its spec.
  ## 6. Call any validation routines for the fields that do exist.
  ## 7. Call any validation routines that cover the entire section.

  ## For instantiable sections, we do #3 once for each object (which will
  ## cause #7 to run once for each object too).

  if spec == nil:
    return

  var
    found_fields:     seq[(string, AttrTree)]
    found_sections:   seq[(int, string)]
    found_f_names:    seq[string]
    found_s_names:    seq[string]
    field_specs:      seq[string]
    excluded_fields:  seq[string]
    ready_to_append:  string

  if tree.path == "":
    tree.path = "the top-level configuration"
  else:
    ready_to_append = tree.path & "."

  for i, item in tree.kids:
    let last = item.path.split(".")[^1]
    if item.kids.len() == 0:
      found_fields.add((last, item))
      found_f_names.add(last)
    else:
      found_sections.add((i, last))
      found_s_names.add(last)

  for (i, secname) in found_sections:
    if secname notin spec.allowedSections and
       secname notin spec.requiredSections:

      errs.add(ctx.format_validation_error(tree.path,
                                           "BadSection",
                                           @[secname]))
    else:
      let newSpec = ctx.obj.spec.secSpecs[secname]
      if newSpec.maxAllowed == 1:
        ctx.validate_section(tree.kids[i], newSpec, errs)
      else:
        for item in tree.kids[i].kids:
          ctx.validate_section(item, newSpec, errs)

  for requirement in spec.requiredSections:
    if requirement notin found_s_names:
      errs.add(ctx.format_validation_error(tree.path, "MissingSection",
                                           @[requirement]))

  if not spec.userDefOk:
    for name in found_f_names:
      if spec.fields.lookup(name).isNone():
        errs.add(ctx.format_validation_error(tree.path, "BadField",
                                             @[name, spec.name]))

  for name in found_f_names:
    let specOpt = spec.fields.lookup(name)
    if specOpt.isSome():
      let fieldSpec = specOpt.get()
      for item in fieldSpec.exclusions:
        if item in found_f_names:
          errs.add(ctx.format_validation_error(tree.path, "FieldMutex",
                                               @[name, item]))
        else:
          excluded_fields.add(item)

  for (name, field_spec) in spec.fields.items():
    if name notin (found_f_names & excluded_fields) and field_spec.required:
      errs.add(ctx.format_validation_error(tree.path, "MissingField",
                                           @[name]))
    else:
      var
        expected_type = TBottom
        found_type: TypeId
        full_path:  string
        err:        bool
        value:      pointer
        found_val:  bool = false

      if field_spec.deferredType != "":
        full_path = ready_to_append & field_spec.deferredType

        expectedType = cast[TypeId](ctx.get(full_path, err, addr foundType))
        if err:
          continue

        if unify(tTypeSpec(), foundType) == TBottom:

          errs.add(ctx.format_validation_error(full_path, "NotTSpec",
                                               @[ready_to_append, name]))
          continue

      full_path = ready_to_append & name

      for (n, tree) in found_fields:

        if n  == tree.path.split(".")[^1]:
          value     = ctx.get(full_path, err, addr found_type, by_addr = false,
                              expected_type = expected_type)
          found_val = true
          break

      if not found_val:
        if field_spec.required:
          # This is in some sense a dupe check to above, but it's possible
          # for us to att an entry w/o setting a value.
          errs.add(ctx.format_validation_error(tree.path, "MissingField",
                                               @[name]))
        continue

      for valid_obj in field_spec.validators:
        let
          fn = cast[FieldValidator](valid_obj.fn)
          r  = fn(ctx, newC4Str(full_path), found_type,
                    value, valid_obj.params)

        if r != nil:
          errs.add(r)

  if spec.validators.len() == 0:
    return

  var fields_to_pass = newArray[pointer](found_f_names.len())

  for i, item in found_f_names:
    fields_to_pass[i] = newC4Str(item)

  for validator in spec.validators:
      let
        fn   = cast[SecValidator](validator.fn)
        flds = cast[pointer](fields_to_pass)
        r    = fn(ctx, newC4str(tree.path), flds, validator.params)

      if r != nil:
        errs.add(r)

proc using_spec*(ctx: RuntimeState): bool {.exportc, cdecl.} =
  return ctx.obj.spec != nil and ctx.obj.spec.used

proc run_validator*(ctx: RuntimeState, startwith = ""):
                    FlexArray[pointer] {.cdecl, exportc.} =
  var
    errs: seq[Rope]

  if not ctx.usingSpec():
    return newArrayFromSeq[pointer](cast[seq[pointer]](errs))

  # We're going to first create the attribute tree as is, without any
  # validation whatsoever, and then walk it. That'll make our life
  # easier.

  var tree = ctx.build_attr_tree()

  if startwith  == "":
    ctx.validate_section(tree, ctx.obj.spec.rootSpec, errs)
  else:
    var
      parts = startwith.split(".")
      sofar = ""
      spec  = ctx.obj.spec.rootSpec
      i     = 0

    while i < parts.len():
      let specOpt = ctx.obj.spec.secSpecs.lookup(parts[i])

      if specOpt.isNone():
        errs.add(ctx.format_validation_error(startwith, "NoSpecForSec",
                                             @[parts[i]]))
        break
      spec  = specOpt.get()
      sofar = if i == 0: parts[0] else: sofar & "." & parts[i]
      i    += 1

      for item in tree.kids:
        if item.path == sofar:
          tree = item
          break

      if tree.path != sofar:
        errs.add(ctx.format_validation_error(startwith, "InvalidStart",
                                             @[sofar]))
        break

      if spec.maxAllowed == 1:
        continue
      if i == parts.len():
        errs.add(ctx.format_validation_error(startwith, "NoInstance", @[]))
        break

      sofar &= "." & parts[i]
      i     += 1

      for item in tree.kids:
        if item.path == sofar:
          tree = item
          break

      if tree.path != sofar:
        errs.add(ctx.format_validation_error(startwith, "InvalidStart",
                                             @[sofar]))
        break

    if errs.len() == 0:
      ctx.validate_section(tree, spec, errs)

  result          = newArrayFromSeq[pointer](cast[seq[pointer]](errs))
  result.metadata = cast[pointer](tList(TRich))
  GC_ref(result)


proc validate_state*(startswith = ""): FlexArray[pointer] {.cdecl, exportc.} =
  return get_con4m_runtime().run_validator(startswith)
