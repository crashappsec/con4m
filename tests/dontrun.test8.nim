import unittest, logging
import con4m
import options
import streams
import tables


const conffile = """

sami_version := "0.2.0"
ascii_magic  := "dadfedabbadabbed"

#color: true

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

  var conf = con4m(Sami, conffile):
    attr(config_path, [string], @[".", "~"])
    attr(config_filename, string, "sami.conf")
    attr(color, bool, false)
    attr(log_level, string, "warn")
    attr(dry_run, string, false)
    attr(artifact_search_path, [string], @["."])
    attr(recursive, bool, true)
    attr(output_dir, string, ".")
    attr(output_file, string, "sami-extractions.json")
    section("key", allowedSubSections = @["*", "*.json", "*.binary"]):
      attr("required", bool, false)
      attr("missing_action", string, "warn")
      attr("system", bool, false)
      attr("squash",
           bool,
           defaultVal = true,
           lockOnWrite = true,
           required = true)
      attr("standard", bool, false)
      attr("must_force", bool, false)
      attr("plugin_ok", bool, true)
      attr("skip", bool, false)
      attr("priority", int, required = false)
      attr("since", string, required = false)
      attr("type", string, required = true)
      attr("value", @x, required = false)
      attr("codec", bool, false)
      attr("docstring", string, required = false)


  echo "log level: ", conf.log_level
  echo "artifact path: ", conf.artifact_search_path
  echo "config file path: ", conf.config_path
  for key, item in conf.key:
    echo "Section: ", key
    echo "  Required: ", item.required
    echo "  Missing action: ", item.missing_action
    echo "  System: ", item.system
    echo "  Squash: ", item.squash
    echo "  Standard: ", item.standard
    echo "  Must force: ", item.must_force
    echo "  Plugin OK: ", item.plugin_ok
    echo "  Skip: ", item.skip
    if item.priority.isSome():
      echo "  Priority: ", item.priority.get()
    else:
      echo "  Priority: <none>"
    if item.since.isSome():
      echo "  Since: ", item.since.get()
    else:
      echo "  Since: <none>"
    echo "  Type: ", item.`type`
    echo "  Codec: ", item.codec
    if item.docstring.isSome():
      echo "  Docstring: ", item.docstring.get()
    else:
      echo "  Docstring: <none>"

  check ctxSamiConf != nil
