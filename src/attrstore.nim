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

  for (fname, fspec) in sec.fields.items():
    if fspec.haveDefault:
      discard ctx.set(prefix & fname, fspec.defaultVal, fspec.tid,
                               fspec.lockOnWrite, internal = true)

  for secName in sec.allowedSections:
    let newSec = ctx.obj.spec.secSpecs[secName]
    if newSec.maxAllowed == 1:
      ctx.applyOneSectionSpecDefaults(prefix & secName & ".", newSec)

proc populateDefaults(ctx: RuntimeState, key: string) =
  let
    parts = key.split(".")

  var
    cur   = parts[0]
    path  = cur
    i     = 0
    l     = parts.len()

  while true:
    let secOpt = ctx.obj.spec.secSpecs.lookup(cur)
    if secOpt.isNone():
      return

    let
      sec         = secOpt.get()
      existingOpt = ctx.attrs.lookup(path)

    if sec.maxAllowed == 1:
      if existingOpt.isNone():
        ctx.applyOneSectionSpecDefaults(path, sec)
    else:
      i += 1
      if i == l:
        break
      path &= "." & cur[i]

      if existingOpt.isNone():
        ctx.applyOneSectionSpecDefaults(path, sec)

    i += 1
    if i == l:
      break
    path &= "." & cur[i]

proc set*(ctx: RuntimeState, key: string, value: pointer, tid: TypeId,
          lock = false, override = false, internal = false):
            bool {.cdecl, exportc.} =
  # Create a new entry on every write, just to avoid any race conditions
  # with multiple threads updating via reference.
  if ctx.obj.spec != nil and not internal:
    ctx.populateDefaults(key)

  var
    newInfo = AttrContents(contents: value, tid: tid, isSet: true, locked: lock)
    curOpt  = ctx.attrs.lookup(key)
    curInfo: AttrContents = nil

  if curOpt.isSome():
    curInfo = curOpt.get()

    if curInfo.locked:
      if not override:
        if not call_eq(value, curInfo.contents, curInfo.tid):
          return false
        else:
          # Set to same value.
          return true
      if curInfo.lockOnWrite:
        newInfo.locked = true

  ctx.attrs[key] = newInfo

  if curInfo != nil:
    GC_unref(curInfo)

  return true

proc get*(ctx: RuntimeState, key: string, err: var bool, tid: ptr TypeId = nil,
          byAddr = false, expectedType = TBottom): pointer {.cdecl, exportc.} =
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

proc get_section_contents*(ctx: RuntimeState, key: string, oneLevel = true):
                         seq[string] =
  var key = if key == "": key else: key & "."

  for item in ctx.attrs.keys(sort = true):
    if item.startswith(key):
      if not oneLevel or item.find('.', key.len()) == -1:
        result.add(item)

proc get_all_keys*(ctx: RuntimeState): seq[string] =
  result = ctx.attrs.keys()
