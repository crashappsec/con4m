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

when defined(macosx):
  # -d:arch=amd64 will allow you to specifically cross-compile to intel.
  # The .strdefine. pragma sets the variable from the -d: flag w/ the same
  # name, overriding the value of the const.
  const arch          {.strdefine.} = "detect"

  # This one is used if we can't find where con4m ends up after nimble
  # grabs the dep.
  const nimblePkgRoot {.strdefine.} = "~/.nimble/pkgs2/"

  var
    targetArch = arch
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
    targetStr = "arm64-apple-macos11"
  elif targetArch == "amd64":
    targetStr = "x86_64-apple-macos11"
  else:
    echo "Invalid target architecture for MacOs: " & arch

  switch("cpu", targetArch)
  switch("passc", "-flto -target " & targetStr)
  switch("passl", "-flto -w -target " & targetStr &
        "-Wl,-object_path_lto,lto.o")

  var deploc: string

  # If we are developing nim, everything has to be under files,
  # but if it's building the exe from nimble, there will be no files dir.
  if "con4m" notin getCurrentDir():

    if nimblePkgRoot.startsWith("~"):
      deploc = getenv("HOME")
      if not deploc.endswith("/"):
        deploc &= "/"

      let rest = nimblePkgRoot[1 .. ^1]
      if not rest.startswith("/"):
        deploc &= rest
      else:
        deploc &= rest[1 .. ^1]

    else:
      deploc = nimblePkgRoot

    if not dirExists(deploc):
      echo "Cannot find nimble path. Please set -d:nimblePkgRoot to the ",
           "location where con4m lives (usually ~/.nimble/pkgs2/)"
      quit(1)

    let
      latest = staticExec("ls " & deploc & " | egrep \"^con4m\" | " &
                          "sort -V | tail -1")
    if latest.strip() == "":
      echo "Cannot find con4m install. Please set an appropriate nimble ",
        "root where con4m is underneath via -d:nimblePkgRoot; we need it ",
        "to find libraries we need to link."
      echo "Detected directory: " & deploc
      quit(1)
    deploc = deploc & "/" & latest & "/deps/macos/"
  else:
    deploc =  getCurrentDir() & "/files/deps/macos/"

  let
    libs   = ["ssl", "crypto"]
    libDir = deploc & targetArch & "/"

  for item in libs:
    let libFile = "lib" & item & ".a"
    switch("passL", libDir & libFile)
    switch("dynlibOverride", item)
