# Package

version       = "0.5.0"
author        = "John Viega"
description   = "A generic configuration file format that allows for flexible, lightweight scripting."
license       = "Apache-2.0"
srcDir        = "src"
bin           = @["con4m"]
installExt    = @["nim"]

# Dependencies
requires "nim >= 1.6.8"
requires "https://github.com/crashappsec/nimutils ~= 0.2.0"

let s = "nimble doc --project --git.url:https://github.com/crashappsec/con4m.git --git.commit:v" &
  version & " --outdir:docs src/con4m.nim"

task docs, "Build our docs":
 exec s
