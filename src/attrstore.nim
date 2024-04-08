import std/algorithm, common

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeSpec,
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
      attrToSet = prefix & fname.toNimStr()
      discard ctx.set(attrToSet, fspec.defaultVal, fspec.tid,
                      fspec.lockOnWrite, internal = true)

  for secName in sec.allowedSections:
    let
      newSecOpt = ctx.obj.spec.secSpecs.lookup(secName)

    if newSecOpt.isNone():
      print(r("[red]error: [/]No specification provided for section [i]" &
        secName.toNimStr()))
      continue

    let newSec = newSecOpt.get()
    if newSec.maxAllowed <= 1:
      ctx.applyOneSectionSpecDefaults(prefix & secName.toNimStr(), newSec)

proc populateDefaults(ctx: RuntimeState, key: string) =
  # Chop off the field.
  var
    parts = key.split(".")[0 ..< ^1]
    l     = len(parts)
    cur   = ""
    base  = ""
    start = -1

  for i in 0 .. l:
    if ctx.allSections.lookup(r(cur)).isNone():
      start = i
      base  = cur
      ctx.allSections[r(cur)] = true
      for n in i ..< l:
        if i == 0:
          cur = parts[i]
        else:
          cur &= "." & parts[i]
        ctx.allSections[r(cur)] = true
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

    let secOpt = ctx.obj.spec.secSpecs.lookup(r(cur))
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
      secOpt = ctx.obj.spec.secSpecs.lookup(r(cur))
    if secOpt.isNone():
      return

    sec = secOpt.get()

    if i == 0:
      attr = cur
    else:
      attr &= "." & cur

    i = i + 1

proc print_attributes*(ctx: RuntimeState)

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeSpec,
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
    curOpt  = ctx.attrs.lookup(r(key))
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
        if not con4m_eq(curInfo.tid, value, curInfo.contents):
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

  ctx.attrs[r(key)] = newInfo

  if curInfo != nil:
    GC_unref(curInfo)

  return true

proc override*(ctx: RuntimeState, key: string, value: pointer, tid: TypeSpec):
    bool {.discardable, cdecl, exportc.} =
  return ctx.set(key, value, tid, override = true)

proc get*(ctx: RuntimeState, key: string, err: var bool, tid: ptr TypeSpec = nil,
          byAddr = false, expectedType: TypeSpec = nil):
            pointer {.cdecl, exportc.} =

  ctx.populate_defaults(key)

  let curOpt = ctx.attrs.lookup(r(key))

  if curOpt.isNone():
    err = true
    if tid != nil:
      tid[] = tspec_error()
    return nil

  let record = curOpt.get()

  if not record.isSet:
    err = true
    return
  elif tid != nil:
    tid[] = record.tid

  if expectedType != tspec_error() and
     tspec_compare(expectedType, record.tid) == false:
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

  let curOpt = ctx.attrs.lookup(r(key))

  if curOpt.isNone():
    return none(T)

  let record = curOpt.get()
  if not record.isSet:
    return none(T)

  return some(cast[T](record.contents))

proc get_section_contents*(ctx: RuntimeState, key: string, oneLevel = true):
                         seq[Rich] =
  ## This only pulls fields, not sections.
  ## See `get_subsections()`.
  var key = if key != "" and key.endswith("."):
              key
            else:
              key & "."

  for item in ctx.attrs.keys(sort = true):
    let s = item.toNimStr()
    if s.startswith(key):
      if not oneLevel or s.find('.', key.len()) == -1:
        result.add(item)

proc get_subsections*(ctx: RuntimeState, path: string, oneLevel = true,
                      fullpath = false):
                    seq[Rich] =
  var path = path

  if path != "" and not path.endswith("."):
    path = path & "."

  let l = path.len()

  for item in ctx.allSections.keys(sort = true):
    let s = item.toNimStr()
    if s.startswith(path):
      let remainder = s[l .. ^1]
      if oneLevel:
        if "." notin remainder:
          if fullpath:
            result.add(item)
          else:
            result.add(r(remainder))
      else:
        if fullpath:
          result.add(item)
        else:
          result.add(r(remainder))

proc get_all_keys*(ctx: RuntimeState): seq[Rich] =
  result = ctx.attrs.keys()

proc attrOne(attr: AttrTree): Rich =

  let last = r(attr.path.split(".")[^1])
  if attr.kids.len() != 0:
    result = last

  else:
    let opt = attr.cache.lookup(r(attr.path))
    if opt.isNone():
      return r(attr.path) + rich" [i]Not Found"

    let match = opt.get()
    var rep   = con4m_repr(match.contents, match.tid)
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

    result = c4str(attr.path) + em(rep) +
    bold(con4m_repr(match.tid)) + c4str(" (" & extra.join(", ") & ")")

proc treeDown(t: Tree, n: AttrTree) =
  for kid in n.kids:
    let sub = t.add_node(attrOne(n))
    treeDown(sub, kid)

proc toRich*(tree: AttrTree): Grid =
  var t: Tree = new_tree(attrOne(tree))
  treeDown(t, tree)

proc print_attributes*(tree: AttrTree) =
  print toRich(tree)

proc build_attr_tree*(ctx: RuntimeState, view = false): AttrTree =
  result = AttrTree()
  if view:
    result.cache = ctx.attrs

  for item in ctx.attrs.keys():
    var
      s     = item.toNimStr()
      cur   = result
      parts = s.split(".")
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

proc get_section_docs_internal*(ctx: RuntimeState,
                                path: string): Option[DocsContainer]
    {.exportc, cdecl.} =

  var path = path
  if path != "" and path[^1] == '.':
    path = path[0 ..< ^1]

  result = ctx.sectionDocs.lookup(r(path))


proc get_short_doc*(ctx: RuntimeState, path: string): Rich {.exportc, cdecl.} =
  let docOpt = ctx.get_section_docs_internal(path)

  if docOpt.isNone():
    return nil

  let docs = docOpt.get()
  return docs.shortdoc

proc get_long_doc*(ctx: RuntimeState, path: string): Rich {.exportc, cdecl.} =
  let docOpt = ctx.get_section_docs_internal(path)

  if docOpt.isNone():
    return nil

  let docs = docOpt.get()
  return docs.longdoc

proc print_section_docs*(ctx: RuntimeState) =
  for (k, v) in ctx.sectionDocs.items():
    print(cell(r("Section docs for: ") + k, "h2"))
    var cells = @[@[rich"Short", v.shortdoc]]

    if v.longdoc != nil:
      cells.add(@[rich"Long", v.longdoc])

    print(cells.table(header_rows = 0, header_cols = 1))


proc con4m_sections(s: Rich): pointer {.cdecl, exportc.} =
  let ctx      = get_con4m_runtime()
  let subsects = ctx.get_subsections(s.toNimStr())

  return newList[Rich](subsects)

addStaticFunction("con4m_sections", con4m_sections)

proc con4m_fields(s: Rich): pointer {.cdecl, exportc.} =
  let ctx    = get_con4m_runtime()
  let fields = ctx.get_section_contents(s.toNimStr())

  return newList[Rich](fields)

addStaticFunction("con4m_fields", con4m_fields)

proc con4m_field_exists(s: Rich): bool {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  return ctx.attrs.lookup(s).isSome()

addStaticFunction("con4m_field_exists", con4m_field_exists)

proc con4m_section_exists(s: Rich): bool {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  return ctx.allSections.lookup(s).isSome()

addStaticFunction("con4m_section_exists", con4m_section_exists)

proc con4m_add_override(s: Rich, v: pointer): bool {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()
  if ctx.obj.spec.locked:
    return false

  return ctx.set(s.toNimStr(), v, cast[C4Obj](v).getMyType(), override = true)

addStaticFunction("con4m_add_override", con4m_add_override)

proc con4m_attr_type(s: Rich): TypeSpec {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()

  let infoOpt = ctx.attrs.lookup(s)

  if infoOpt.isNone():
    return tspec_error()

  return infoOpt.get().tid

addStaticFunction("con4m_attr_type", con4m_attr_type)

proc con4m_attr_split(s: Rich): CTuple {.cdecl, exportc.} =
  result = con4m_tuple(tspec_tuple(toXList(@[tspec_string(), tspec_string()])))

  let
    str = s.toNimStr()
    ix  = rfind(str, '.')

  if ix == -1:
    result[0] = rich""
    result[1] = s
  else:
    result[0] = c4Str(str[0 ..< ix])
    result[1] = c4Str(str[ix + 1 .. ^1])

addStaticFunction("con4m_attr_split", con4m_attr_split)
addStaticFunction("con4m_typecmp", tspec_compare)

proc con4m_attr_get(s: Rich, t: TypeSpec): pointer {.cdecl, exportc.} =
  let ctx = get_con4m_runtime()
  var err: bool

  result = ctx.get(s.toNimStr(), err, expectedType = t)

  if err:
    return nil

addStaticFunction("con4m_attr_get", con4m_attr_get)
