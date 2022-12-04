import unittest, logging
import con4m
import options
import streams


const conffile = """

sami_version := "0.2.0"
ascii_magic  := "dadfedabbadabbed"

key "_MAGIC" json {
    required: true
    missing_action: "abort"
    system: true
    type: "string"
    value: ascii_magic
    standard: true
    since: "0.1.0"
    priority: 0
}

key "SAMI_ID" {
    required: true
    missing_action: "error"
    system: true
    squash: false
    type: "integer"
    value: "'get_sami_id"
    since: "0.1.0"
    priority: 1
}

key "SAMI_VERSION" {
    required: true
    missing_action: "error"
    system: true
    type: "string"
    value: sami_version
    since: "0.1.0"
    priority: 2
}

key "SAMI_TIMESTAMP" {
    required: true
    missing_action: "error"
    system: true
    type: "integer"
    value: "now"
    since: "0.1.0"
    priority: 3
}

key "EARLIEST_VERSION" {
    type: "string"
    since: "0.1.0"
    system: true
    value: sami_version
    priority: 4
}

key "ERR_INFO" {
    type: "[string]"
    since: "0.1.0"
    system: true
    priority: 999
}

key "SIGNATURE" {
    type: "{string : string}"
    since: "0.1.0"
    system: true
    priority: 1000
}

key "ORIGIN_URI" {
    type: "string"
    missing_action: "warn"
    since: "0.1.0"
}

key "ARTIFACT_VERSION" {
    type: "string"
    since: "0.1.0"
}

key "ARTIFACT_FILES" {
    type: "[string]"
    since: "0.1.0"
}

key "IAM_USERNAME" {
    must_force: true
    type: "string"
    since: "0.1.0"
}

key "IAM_UID" {
    must_force: true
    type: "integer"
    since: "0.1.0"
}

key "BUILD_URI" {
    type: "string"
    since: "0.1.0"
}

key "STORE_URI" {
    type: "string"
    since: "0.1.0"
}

key "BRANCH" {
    type: "string"
    since: "0.1.0"
}

key "SRC_URI" {
    type: "string"
    since: "0.1.0"
}

key "REPO_ORIGIN" {
    type: "string"
    value: "'calc_origin"
    since: "0.1.0"
}

key "HASH" {
    type: "string"
    since: "0.1.0"
    codec: true
}

key "HASH_FILES" {
    type: "[string]"
    since: "0.1.0"
    codec: true
}

key "COMMIT_ID" {
    type: "string"
    since: "0.1.0"
}

key "JOB_ID" {
    type: "string"
    since: "0.1.0"
}

key "SRC_PATH" {
    type: "string"
    since: "0.1.0"
    codec: true
}

key "FILE_NAME" {
    type: "string"
    since: "0.1.0"
    codec: true
}

key "CODE_OWNERS" {
    type: "string"
    since: "0.1.0"
}

key "BUILD_OWNERS" {
    type: "string"
    since: "0.1.0"
}

key "OLD_SAMI" {
    type: "sami"
    since: "0.1.0"
}

key "EMBEDS" {
    type: "[(string, sami)]"
    since: "0.1.0"
}

key "SBOMS" {
    type: "{string, string}"
    since: "0.1.0"
}
"""

test "samiconf":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))
  addDefaultBuiltins()

  var spec = newConfigSpec()
  var
    defaultCfgPathSpec = @[".", "~"]
    defaultArtPathSpec = @["."]

  discard spec.addGlobalAttr("config_path",
                             "[string]",
                             some(box(defaultCfgPathSpec)))
  discard spec.addGlobalAttr("config_filename", "string",
                             some(box("sami.conf")))
  discard spec.addGlobalAttr("color", "bool", some(box(false)))
  discard spec.addGlobalAttr("log_level", "string", some(box("warn")))
  discard spec.addGlobalAttr("dry_run", "string", some(box(false)))
  discard spec.addGlobalAttr("artifact_search_path", "[string]",
                             some(box(defaultArtPathSpec)))
  discard spec.addGlobalAttr("recursive", "bool", some(box(true)))
  discard spec.addGlobalAttr("output_dir", "string", some(box(".")))
  discard spec.addGlobalAttr("output_file",
                             "string",
                             some(box("sami-extractions.json")))

  var sectKey = spec.addSection("key", validSubSecs =
    @["*", "*.json", "*.binary"])

  discard sectKey.addAttr("required", "bool", some(box(false)))
  discard sectKey.addAttr("missing_action", "string", some(box("warn")))
  discard sectKey.addAttr("system", "bool", some(box(false)))
  discard sectKey.addAttr("squash", "bool", some(box(true)))
  discard sectKey.addAttr("standard", "bool", some(box(false)))
  discard sectKey.addAttr("must_force", "bool", some(box(false)))
  discard sectKey.addAttr("plugin_ok", "bool", some(box(true)))
  discard sectKey.addAttr("skip", "bool", some(box(false)))
  discard sectKey.addAttr("priority", "int", required = false)
  discard sectKey.addAttr("since", "string", required = false)
  discard sectKey.addAttr("type", "string", required = true)
  discard sectKey.addAttr("value", "`x", required = false)
  discard sectKey.addAttr("codec", "bool", required = false)
  discard sectKey.addAttr("docstring", "string", required = false)

  let tree = parse(newStringStream(conffile))

  check tree != nil

  tree.checkTree()
  tree.evalTree()

  let scopes = tree.scopes.get()
  
  let st = scopes.attrs

  let ctx = newConfigState(st, spec)

  check ctx.validateConfig()

  echo $ctx.st
