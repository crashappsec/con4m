import std/algorithm
import ztypes/api
export api

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId,
          lock = false, override = false, internal = false):
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

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId,
          lock = false, override = false, internal = false):
            bool {.cdecl, exportc.} =
  # We will create a new entry on every write, just to avoid any race
  # conditions with multiple threads updating via reference.

  # Here, make sure we have section info captured.
  if not internal:
    ctx.populateDefaults(key)

  var
    newInfo = AttrContents(contents: value, tid: tid, isSet: true, locked: lock)
    curOpt  = ctx.attrs.lookup(key)
    curInfo: AttrContents = nil

  if curOpt.isSome():
    curInfo = curOpt.get()

    if newInfo.override and not override:
      return true # Pretend it was successful.

    if curInfo.locked:
      if not override:
        if not call_eq(value, curInfo.contents, curInfo.tid):
          return false
        else:
          # Set to same value.
          return true
      if curInfo.lockOnWrite:
        newInfo.locked = true

  if override:
    newInfo.override = true

  ctx.attrs[key] = newInfo

  if curInfo != nil:
    GC_unref(curInfo)

  return true

proc override*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId)
                                                        {.cdecl, exportc.} =
  discard ctx.set(key, value, tid, override = true)

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
    var
      tid: TypeId
      err: bool
      val = get_con4m_runtime().get(attr.path, err, addr tid)

    var rep = $(call_repr(val, tid))

    result = (text(attr.path) + text(" ") + em(rep), @[])

proc print_attributes*(tree: AttrTree) =
  print tree.quickTree(attr_walker)

proc build_attr_tree*(ctx: RuntimeState): AttrTree =
  result = AttrTree()

  for item in ctx.get_all_keys():
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
        cur.kids.add(node)
        cur = node

      i = i + 1

    cur.kids.sort()

proc print_attributes*(ctx: RuntimeState) =
  ctx.build_attr_tree.print_attributes()