# Package
version       = "0.0.2"
author        = "John Viega"
description   = "A generic configuration file format that allows for flexible, lightweight scripting."
license       = "Apache-2.0"
bin           = @["con4m"]
srcDir        = "files"
installExt    = @["nim", "c4m", "c42spec", "c", "h", "a", "sh"]

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
