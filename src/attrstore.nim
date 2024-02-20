import std/algorithm
import ztypes/api
export api

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId,
          lock = false, override = false, internal = false, wlock = false):
            bool {.cdecl, exportc.}

proc applyOneSectionSpecDefaults*(ctx: RuntimeState, prefix: string,
                                 sec: SectionSpec) =
  # When we're first populating a program, if there are defaults we can
  # set without instantiating objects, we do so. Similarly, when a `set`
  # ends up being the first write to some new object section, we call
  # this to populate any defaults in the object.
  var
    prefix    = if (prefix != "" and prefix[^1] != '.'): prefix & "." else: ""
    attrToSet: string

  for (fname, fspec) in sec.fields.items():
    if fspec.haveDefault:
      attrToSet = prefix & fname
      discard ctx.set(attrToSet, fspec.defaultVal, fspec.tid,
                      fspec.lockOnWrite, internal = true)

  for secName in sec.allowedSections:
    let
      newSecOpt = ctx.obj.spec.secSpecs.lookup(secName)

    if newSecOpt.isNone():
      print(fgColor("error: ", "red") +
            text("No specification provided for section ") +
            em(secName))
      continue

    let newSec = newSecOpt.get()
    if newSec.maxAllowed <= 1:
      ctx.applyOneSectionSpecDefaults(prefix & secName, newSec)

proc populateDefaults(ctx: RuntimeState, key: string) =
  # Chop off the field.
  var
    parts = key.split(".")[0 ..< ^1]
    l     = len(parts)
    cur   = ""
    base  = ""
    start = -1

  for i in 0 .. l:
    if ctx.allSections.lookup(cur).isNone():
      start = i
      base  = cur
      ctx.allSections[cur] = true
      for n in i ..< l:
        if i == 0:
          cur = parts[i]
        else:
          cur &= "." & parts[i]
        ctx.allSections[cur] = true
      break
    if i == 0 and i != l:
      cur = parts[0]
    elif i != l:
      cur &= "." & parts[i]

  if start == -1 or ctx.obj.spec == nil:
    return

  var
    i   = 0
    sec = ctx.obj.spec.rootSpec
    attr: string

  if start == 0:
    ctx.applyOneSectionSpecDefaults("", sec)

  while i < start:
    cur = parts[i]

    if i == 0:
      attr = cur
    else:
      attr &= "." & cur

    #if ctx.obj.spec.secSpecs == nil:
    #  return

    let secOpt = ctx.obj.spec.secSpecs.lookup(cur)
    if secOpt.isNone():
      return

    sec = secOpt.get()
    i   = i + 1

    if sec.maxAllowed <= 1:
      continue
    else:
      if i + 1 == l:
        start = i
        break

      attr &= "." & parts[i]
      i = i + 1

  i = start

  while i < l:
    # assert sec.maxAllowed != 0
    #if sec == nil:
    #  return

    if sec.maxAllowed <= 1:
      ctx.applyOneSectionSpecDefaults(attr, sec)
    else:
      if i == 0:
        attr = parts[i]
      else:
        attr = attr & "." & parts[i]

      ctx.applyOneSectionSpecDefaults(attr, sec)
      i += 1
      if i == l:
        break

    let
      cur    = parts[i]
      secOpt = ctx.obj.spec.secSpecs.lookup(cur)
    if secOpt.isNone():
      return

    sec = secOpt.get()

    if i == 0:
      attr = cur
    else:
      attr &= "." & cur

    i = i + 1

proc print_attributes*(ctx: RuntimeState)

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId,
          lock = false, override = false, internal = false, wlock = false):
            bool {.cdecl, exportc.} =
  # We will create a new entry on every write, just to avoid any race
  # conditions with multiple threads updating via reference.

  # Here, make sure we have section info captured.

  ctx.usingAttrs = true

  if not internal:
    ctx.populateDefaults(key)

  var
    newInfo = AttrContents(contents: value, tid: tid, isSet: true)
    curOpt  = ctx.attrs.lookup(key)
    curInfo: AttrContents = nil

  GC_ref(newInfo)

  if curOpt.isSome():
    curInfo = curOpt.get()
    newInfo.moduleLock = curInfo.moduleLock

    if newInfo.override and not override:
      return true # Pretend it was successful.

    if curInfo.locked or
       (curInfo.moduleLock != 0 and
        curInfo.moduleLock != ctx.curModule.moduleId):
      if not override:
        if not call_eq(value, curInfo.contents, curInfo.tid):
          return false
        else:
          # Set to same value; ignore it basically.
          return true
    if curInfo.lockOnWrite:
      newInfo.locked = true

  if override:
    newInfo.override = true

  if ctx.running:
    newInfo.lastset = addr ctx.curModule.instructions[ctx.ip]
    if ctx.module_lock_stack.len() != 0:
      newInfo.moduleLock = ctx.module_lock_stack[^1]

  # Don't trigger write lock if we're setting a default (i.e., internal is set).
  if (lock or wlock) and not internal:
    newInfo.locked = true
  elif wlock:
    newInfo.lockOnWrite = true

  ctx.attrs[key] = newInfo

  if curInfo != nil:
    GC_unref(curInfo)

  return true

proc override*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId):
    bool {.discardable, cdecl, exportc.} =
  return ctx.set(key, value, tid, override = true)

proc get*(ctx: RuntimeState, key: string, err: var bool, tid: ptr TypeId = nil,
          byAddr = false, expectedType = TBottom): pointer {.cdecl, exportc.} =
  ctx.populate_defaults(key)

  let curOpt = ctx.attrs.lookup(key)

  if curOpt.isNone():
    err = true
    if tid != nil:
      tid[] = TBottom
    return nil

  let record = curOpt.get()

  if not record.isSet:
    err = true
    return
  elif tid != nil:
    tid[] = record.tid

  if expectedType != TBottom and expectedType.unify(record.tid) == TBottom:
    err = true
    # Can distinguish which error by checking for the found type.
    return

  if byAddr:
    result = addr record.contents
  else:
    result = record.contents

  err = false

proc lookup*[T](ctx: RuntimeState, key: string): Option[T] =
  ctx.populateDefaults(key)

  let curOpt = ctx.attrs.lookup(key)

  if curOpt.isNone():
    return none(T)

  let record = curOpt.get()
  if not record.isSet:
    return none(T)

  return some(cast[T](record.contents))

proc get_section_contents*(ctx: RuntimeState, key: string, oneLevel = true):
                         seq[string] =
  ## This only pulls fields, not sections.
  ## See `get_subsections()`.
  var key = if key == "" or key.endswith("."):
              key
            else:
              key & "."

  for item in ctx.attrs.keys(sort = true):
    if item.startswith(key):
      if not oneLevel or item.find('.', key.len()) == -1:
        result.add(item)

proc get_subsections*(ctx: RuntimeState, path: string, oneLevel = true):
                    seq[string] =
  var path = path

  if path != "" and not path.endswith("."):
    path = path & "."

  let l = path.len()

  for item in ctx.allSections.keys(sort = true):
    if item.startswith(path):
      let remainder = item[l .. ^1]
      if oneLevel:
        if "." notin remainder:
          result.add(remainder)
      else:
        result.add(remainder)

proc get_all_keys*(ctx: RuntimeState): seq[string] =
  result = ctx.attrs.keys()

proc attr_walker(attr: AttrTree): (Rope, seq[AttrTree]) =

  let last = text(attr.path.split(".")[^1])
  if attr.kids.len() != 0:
    result = (last, attr.kids)

  else:
    let opt = attr.cache.lookup(attr.path)
    if opt.isNone():
      return (text(attr.path) + text(" ") + em("Not Found"), @[])

    let match = opt.get()
    var rep   = $(call_repr(match.contents, match.tid))
    var extra: seq[string]

    if not match.isSet:
      extra.add("set: f")

    if match.locked:
      extra.add("locked: y")
    elif match.lockOnWrite:
      extra.add("locked: on write")
    else:
      extra.add("locked: n")

    if match.override:
      extra.add("override: y")

    result = (text(attr.path) + text(" ") + em(rep) + text(" ") +
              strong(match.tid.toString()) + text(" (") +
              text(extra.join(", ")) + text(")"), @[])

proc print_attributes*(tree: AttrTree) =
  print tree.quickTree(attr_walker)

proc build_attr_tree*(ctx: RuntimeState, view = false): AttrTree =
  result = AttrTree()
  if view:
    result.cache = ctx.attrs

  for item in ctx.attrs.keys():
    var
      cur   = result
      parts = item.split(".")
      i     = 0
      l     = parts.len()

    while i < l:
      let
        p = parts[0 .. i].join(".")

      var found = false

      for kid in cur.kids:
        if kid.path == p:
          found = true
          cur   = kid
          break

      if not found:
        let node = AttrTree(path: p)
        if view:
          node.cache = ctx.attrs
        cur.kids.add(node)
        cur = node

      i = i + 1

    cur.kids.sort()

proc print_attributes*(ctx: RuntimeState) =
  var tree = ctx.build_attr_tree(true)
  tree.print_attributes()
  GC_unref(tree.cache)

proc get_section_docs_internal*(ctx: RuntimeState,
                                path: string): Option[DocsContainer]
    {.exportc, cdecl.} =

  var path = path
  if path != "" and path[^1] == '.':
    path = path[0 ..< ^1]

  result = ctx.sectionDocs.lookup(path)


proc get_short_doc*(ctx: RuntimeState, path: string): Rope {.exportc, cdecl.} =
  let docOpt = ctx.get_section_docs_internal(path)

  if docOpt.isNone():
    return nil

  let docs = docOpt.get()
  return docs.shortdoc

proc get_long_doc*(ctx: RuntimeState, path: string): Rope {.exportc, cdecl.} =
  let docOpt = ctx.get_section_docs_internal(path)

  if docOpt.isNone():
    return nil

  let docs = docOpt.get()
  return docs.longdoc

proc print_section_docs*(ctx: RuntimeState) =
  for (k, v) in ctx.sectionDocs.items():
    print(h2("Section docs for: " & k))
    var cells = @[@[text("Short"), v.shortdoc]]

    if v.longdoc != nil:
      cells.add(@[text("Long"), v.longdoc])

    print(cells.quickTable(verticalHeaders = true))


proc con4m_sections(s: cstring): pointer {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  return toCon4m(ctx.get_subsections($s), tList(TString))

addStaticFunction("con4m_sections", con4m_sections)

proc con4m_fields(s: cstring): pointer {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  return toCon4m(ctx.get_section_contents($s), tList(TString))

addStaticFunction("con4m_fields", con4m_fields)

proc con4m_field_exists(s: cstring): bool {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  return ctx.attrs.lookup($s).isSome()

addStaticFunction("con4m_field_exists", con4m_field_exists)

proc con4m_section_exists(s: cstring): bool {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  return ctx.allSections.lookup($s).isSome()

addStaticFunction("con4m_section_exists", con4m_section_exists)

proc con4m_add_override(s: cstring, v: pointer): bool {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()
  if ctx.obj.spec.locked:
    return false

  var n: Mixed = cast[Mixed](v)

  return ctx.set($s, n.value, n.t, override = true)

addStaticFunction("con4m_add_override", con4m_add_override)

proc con4m_attr_type(s: cstring): TypeId {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  let infoOpt = ctx.attrs.lookup($s)

  if infoOpt.isNone():
    return TBottom

  return infoOpt.get().tid

addStaticFunction("con4m_attr_type", con4m_attr_type)

proc con4m_attr_split(s: C4Str): Con4mTuple {.cdecl, exportc.} =
  result = newTuple(tTuple(@[TString, TString]))
  let
    str = `$`(cast[cstring](s))
    ix  = rfind(str, '.')

  if ix == -1:
    result[0] = newC4Str("")
    result[1] = s
  else:
    result[0] = newC4Str(str[0 ..< ix])
    result[1] = newC4Str(str[ix + 1 .. ^1])

addStaticFunction("con4m_attr_split", con4m_attr_split)

proc con4m_typecmp(t1: TypeId, t2: TypeId): bool {.cdecl, exportc.} =
  return t1.copyType().unify(t2.copyType()) != TBottom

addStaticFunction("con4m_typecmp", con4m_typecmp)

proc con4m_attr_get(s: cstring, t: TypeId): pointer {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()
  var err: bool

  result = ctx.get($s, err, expectedType = t)

  if err:
    return nil

addStaticFunction("con4m_attr_get", con4m_attr_get)
