import unittest, logging, streams

import con4m

test "manual inspection":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))
  addDefaultBuiltins()
  let s = """

defaults {
    phase: "build"
    if env()["TERM"].strip().contains("color") {
      color: false
    } elif false {
      echo("hi!")
    } elif 1 == 2 {
      echo("ha")
    } else {
      color: true
    }

    dryRun: false
    loglevel: "debug"
    artifactPath: ["."]
    logfile: "/dev/null"
  }

  for i from 1 to 10 {
    echo("Sup!"); echo("Yo!!")

    # I took out while loops to ensure programs halt
    #while true {
    #  break
    #}
  }
  # Each pluggable catagory has a schema associated with it.  Plugins
  # must produce data of that schema, and any command run
  # must output data in that schema, dumping a JSON blob to stdout,
  # or a string if the associated type is a simple type.

  REPO_ENVVAR := "REPO_URI"
  REPO_COMMAND := "REPO_COMMAND" # E.g., "ask-user What is the name of this repository?"
  repo_vars := split(env()[REPO_ENVVAR], ":")   # or system()

  repo {
    enabled: true
    defaults {
      origin: repo_vars[0]
      commitId: repo_vars[1]
      branch: repo_vars[2]
    }

    source "github" {
      enabled: true
      priority: 1
    }

    source "authors" {
      enabled: true
      priority: 10
    }

    source "defaults" {
      priority: 100 # Only if others fail
    }

    source "flags" {
      enabled: false
    }

    source "cmd" {
      enabled: true
      priority: 100 # Only if the others fail
      command: env()[REPO_COMMAND]
    }
  }

  sbom {
    enabled: true
    required: true

    plugin "cmd" {
      command: "/usr/bin/createsbom ${artifact}"
      force: true # cannot do --disable-sbom
      error: "abort"
      msg: "Couldn't create SBOM for ${artifact}"
    }

    plugin "*" {
      enabled: false
    }
  }

  other = { 
   "XLOCAL_USER": run("uname -a"),
   "XFRAUD" : "false"
  }
    
  custom {
    key "XSIGNATURE" {
      kind: "binary"
      force: true  # can't do --no-custom-x-signature
      command: "/usr/bin/sign ${artifact}"
      default: "jo mama"   # Default value to add if 
    }
  }
"""

  let node = s.newStringStream().parse("testfile")
  if node != nil:
    echo "Parse was successful!"

  checkTree(node)
  echo $(node)

  check node != nil # Why does this SEGV when node is nil??
