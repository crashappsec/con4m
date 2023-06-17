# Package
version = "0.7.19"
author = "John Viega"
description = "A generic configuration file format that allows for flexible, lightweight scripting."
license = "Apache-2.0"
srcDir = "src"
bin = @["con4m"]
installExt = @["nim", "c4m", "c42spec"]

# Dependencies
requires "nim >= 1.6.10"
requires "https://github.com/crashappsec/nimutils == 0.4.4"
requires "nimSHA2 == 0.1.1"

let s = "nimble doc --project --git.url:https://github.com/crashappsec/con4m.git --git.commit:v" &
  version & " --outdir:docs src/con4m.nim"

task docs, "Build our docs":
 exec s

task ctest, "Build libcon4m":
 when hostOs == "linux" or hostOS == "macosx":
  exec "if [ ! -e lib ] ; then mkdir lib; fi"
  exec "if [ ! -e bin ] ; then mkdir bin; fi"
  exec "nim c --define:CAPI --app:staticlib --noMain:on src/con4m.nim"
  exec "mv src/libcon4m.a lib"
  exec "cc -Wall -o bin/test src/c/test.c lib/libcon4m.a -I ~/.choosenim/toolchains/nim-1.6.10/lib/ -lc -lm -ldl"
 else:
  echo "Platform ", hostOs, " Not supported."
