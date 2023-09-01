# Package
version       = "0.0.2"
author        = "John Viega"
description   = "A generic configuration file format that allows for flexible, lightweight scripting."
license       = "Apache-2.0"
bin           = @["con4m"]
srcDir        = "files"
installExt    = @["nim", "c4m", "c42spec", "c", "h", "a", "sh", "specs"]

# Dependencies
requires "nim >= 1.6.12"
requires "https://github.com/crashappsec/nimutils == 0.1.0"

task ctest, "Build libcon4m":
 when hostOs == "linux" or hostOS == "macosx":
  exec "if [ ! -e lib ] ; then mkdir lib; fi"
  exec "if [ ! -e bin ] ; then mkdir bin; fi"
  exec "nim c --define:CAPI --app:staticlib --noMain:on src/con4m.nim"
  exec "mv src/libcon4m.a lib"
  exec "cc -Wall -o bin/test src/c/test.c lib/libcon4m.a -I ~/.choosenim/toolchains/nim-1.6.10/lib/ -lc -lm -ldl"
 else:
  echo "Platform ", hostOs, " Not supported."


proc fixMuslFile() =
  # nimble installs musl in a tmp dir, then moves the script to
  # ~/.nimble.  So the musl-gcc file will be pointing to the wrong
  # location for the musl-gcc.specs file.  Let's just always re-write
  # it to point to the right location.
  #
  # The location varies whether we've been installed by Nimble or
  # build locally.  Locally, "/files" will be a subdirectory, but
  # under Nimble it will not be.
  #
  # Since we're in the javascript world w/ nimscript, and since I
  # don't see any write functions, we'll write via the shell.

  var
    subdir = ""

  for item in listDirs(thisDir()):
    if item.endswith("/files"):
      subdir = "/files"
      break

  let
    muslLoc = thisDir() & subdir & "/deps/usr/musl/bin/musl-gcc.sh"
    specLoc = thisDir() & subdir & "/deps/usr/musl/lib/musl-gcc.specs"
    toRun   = "cat > " & muslLoc & """ <<'EOF'
#!/bin/sh
exec "${REALGCC:-gcc}" "$@" -specs "X"
EOF
""".replace("X", specLoc)

  discard staticExec(toRun)

when defined(linux):
  after build:
    fixMuslFile()
