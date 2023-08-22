switch("define", "testCases")
switch("debugger", "native")
switch("d", "nimPreviewHashRef")
switch("d", "ssl")
when (NimMajor, NimMinor) < (1, 7):
  # Locklevels never worked and are gone but warnings persist.
  switch("warning", "LockLevel:off")
when (NimMajor, NimMinor, NimPatch) >= (1, 6, 12):
  # Someone made a move to deprecate, but they're undoing it.
  switch("warning", "BareExcept:off")
if defined(macosx):
  var cpu = ""
  var target = ""
  if defined(arm):
    cpu = "arm64"
    target = "arm64"
  elif defined(amd64):
    cpu = "amd64"
    target = "x86_64"
  switch("cpu", $cpu)
  switch("passc", "-flto -target " & $target & "-apple-macos11")
  switch("passl", "-flto -target " & $target & "-apple-macos11 -Wl,-object_path_lto,lto.o ")
