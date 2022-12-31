# Package

version       = "0.3.4"
author        = "John Viega"
description   = "A generic configuration file format that allows for lightweight scripting. Inspired by HCL, but far more straightforward.  Yet far more functional than UCL."
license       = "Apache-2.0"
srcDir        = "src"
bin           = @["con4m"]
installExt    = @["nim"]

# Dependencies
requires "nim >= 1.6.8"
requires "https://github.com/crashappsec/nimutils >= 0.1.0"

let s = "nimble doc --project --git.url:https://github.com/crashappsec/con4m.git --git.commit:v" &
  version & " --outdir:docs src/con4m.nim"

task docs, "Build our docs":
 exec s
