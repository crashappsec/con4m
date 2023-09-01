import strutils

switch("define", "testCases")
switch("debugger", "native")
switch("d", "nimPreviewHashRef")
switch("d", "ssl")
switch("d", "useOpenSSL3")
switch("gc", "refc")

when (NimMajor, NimMinor) < (1, 7):
  # Locklevels never worked and are gone but warnings persist.
  switch("warning", "LockLevel:off")
when (NimMajor, NimMinor, NimPatch) >= (1, 6, 12):
  # Someone made a move to deprecate, but they're undoing it.
  switch("warning", "BareExcept:off")

when not defined(debug):
    switch("d", "release")
    switch("opt", "speed")

var targetArch = hostCPU


when defined(macosx):
  # -d:arch=amd64 will allow you to specifically cross-compile to intel.
  # The .strdefine. pragma sets the variable from the -d: flag w/ the same
  # name, overriding the value of the const.
  const arch          {.strdefine.} = "detect"

  var
    targetStr  = ""

  if arch == "detect":
    # On an x86 mac, the proc_translated OID doesn't exist. So if this
    # returns either 0 or 1, we know we're running on an arm. Right now,
    # nim will always use rosetta, so should always give us a '1', but
    # that might change in the future.
    let sysctlOut = staticExec("sysctl -n sysctl.proc_translated")

    if sysctlOut in ["0", "1"]:
      targetArch = "arm64"
    else:
      targetArch = "amd64"
  else:
    echo "Override: arch = " & arch

  if targetArch == "arm64":
    echo "Building for arm64"
    targetStr = "arm64-apple-macos11"
  elif targetArch == "amd64":
    targetStr = "x86_64-apple-macos11"
    echo "Building for amd64"
  else:
    echo "Invalid target architecture for MacOs: " & arch
    quit(1)

  switch("cpu", targetArch)
  switch("passc", "-flto -target " & targetStr)
  switch("passl", "-flto -w -target " & targetStr &
        "-Wl,-object_path_lto,lto.o")
elif defined(linux):
  switch("passc", "-static")
  switch("passl", "-static")
else:
  echo "Platform not supported."
  quit(1)

echo staticexec("ls " & thisDir())
exec thisDir() & "/bin/buildlibs.sh"

let
 deploc = thisDir() & "/deps/lib/" & hostOs & "-" & targetArch & "/"
 libs   = ["ssl", "crypto"]

for item in libs:
  let libFile = "lib" & item & ".a"

  switch("passL", deploc & libFile)
  switch("dynlibOverride", item)
