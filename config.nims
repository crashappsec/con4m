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
  var host, target: string
  when defined(doAmd64Build):
    host   = "amd64"
    target = "x86_64-apple-macos11"
  else:
    host   = "arm64"
    target = "arm64-apple-macos11"

  switch("cpu", host)
  switch("passc", "-flto -target " & target)
  switch("passl", "-flto -target " & target & "-Wl,-object_path_lto,lto.o")
else:
  switch("passl", "-static")
