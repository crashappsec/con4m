import unittest, logging
import con4m
import options
import streams
import tables
import strutils


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
        
type
  SamiKeySection = ref object
    required: bool
    missing_action: string
    system: bool
    squash: bool
    standard: bool
    must_force: bool
    plugin_ok: bool
    skip: bool
    priority: Option[int]
    since: Option[string]
    `type`: string
    value: Option[Box]
    codec: bool
    docstring: Option[string]
      
  SamiConf = ref object
    config_path: seq[string]
    config_filename: string
    color: bool
    log_level: string
    dry_run: bool
    artifact_search_path: seq[string]
    recursive: bool
    output_dir: string
    output_file: string
    # If key can't have subsections, there would be no table
    key: OrderedTable[string, SamiKeySection]

proc loadSamiConfig(ctx: ConfigState): SamiConf =
  result = SamiConf()

  var tmpBox: Box

  tmpBox = ctx.getConfigVar("config_path").get()
  
  let config_path_box_seq_str = unbox[seq[Box]](tmpBox)
  var config_path_seq_str: seq[string] = @[]

  for item in config_path_box_seq_str:
    config_path_seq_str.add(unbox[string](item))

  result.config_path = config_path_seq_str

  tmpBox = ctx.getConfigVar("config_filename").get()
  result.config_filename = unbox[string](tmpBox)
  
  tmpBox = ctx.getConfigVar("color").get()
  result.color = unbox[bool](tmpBox)
  
  tmpBox = ctx.getConfigVar("log_level").get()
  result.log_level = unbox[string](tmpBox)
  
  tmpBox = ctx.getConfigVar("dry_run").get()
  result.dry_run  = unbox[bool](tmpBox)

  tmpBox = ctx.getConfigVar("artifact_search_path").get()
  
  let artifact_search_path_box_seq_str = unbox[seq[Box]](tmpBox)
  var artifact_search_path_seq_str: seq[string] = @[]

  for item in artifact_search_path_box_seq_str:
    artifact_search_path_seq_str.add(unbox[string](item))
    
  result.artifact_search_path = artifact_search_path_seq_str

  tmpBox = ctx.getConfigVar("recursive").get()
  result.recursive = unbox[bool](tmpBox)

  tmpBox = ctx.getConfigVar("output_dir").get()
  result.output_dir = unbox[string](tmpBox)

  tmpBox = ctx.getConfigVar("output_file").get()
  result.output_file = unbox[string](tmpBox)

  let sectionInfo = ctx.getAllSectionSTs()

  for (toplevel, k, v) in sectionInfo:
    var stEntry: STEntry
    var entryOpt: Option[STEntry]
    
    let sectionKey = k.split(".")[1 .. ^1].join(".")
    var sectionData = SamiKeySection()
    result.key[sectionKey] = sectionData

    stEntry = v.lookupAttr("required").get()
    tmpBox = stEntry.value.get()
    sectionData.required = unbox[bool](tmpBox)

    stEntry = v.lookupAttr("missing_action").get()
    tmpBox = stEntry.value.get()
    sectionData.missing_action = unbox[string](tmpBox)

    stEntry = v.lookupAttr("system").get()
    tmpBox = stEntry.value.get()
    sectionData.system = unbox[bool](tmpBox)

    stEntry = v.lookupAttr("squash").get()
    tmpBox = stEntry.value.get()
    sectionData.squash = unbox[bool](tmpBox)
    
    stEntry = v.lookupAttr("standard").get()
    tmpBox = stEntry.value.get()
    sectionData.standard = unbox[bool](tmpBox)

    stEntry = v.lookupAttr("must_force").get()
    tmpBox = stEntry.value.get()
    sectionData.must_force = unbox[bool](tmpBox)

    stEntry = v.lookupAttr("plugin_ok").get()
    tmpBox = stEntry.value.get()
    sectionData.plugin_ok = unbox[bool](tmpBox)

    stEntry = v.lookupAttr("skip").get()
    tmpBox = stEntry.value.get()
    sectionData.skip = unbox[bool](tmpBox)

    entryOpt = v.lookupAttr("priority")
    if entryOpt.isSome():
      stEntry = entryOpt.get()
      tmpBox = stEntry.value.get()
      sectionData.priority = some(unbox[int](tmpBox))
    
    entryOpt = v.lookupAttr("since")
    if entryOpt.isSome():
      stEntry = entryOpt.get()
      tmpBox = stEntry.value.get()
      sectionData.since = some(unbox[string](tmpBox))

    stEntry = v.lookupAttr("type").get()
    tmpBox = stEntry.value.get()
    sectionData.`type` = unbox[string](tmpBox)

    entryOpt = v.lookupAttr("value")
    if entryOpt.isSome():
      stEntry = entryOpt.get()
      tmpBox = stEntry.value.get()
      sectionData.value = some(unbox[Box](tmpBox))

    stEntry = v.lookupAttr("codec").get()
    tmpBox = stEntry.value.get()
    sectionData.codec = unbox[bool](tmpBox)
    
    entryOpt = v.lookupAttr("docstring")
    if entryOpt.isSome():
      stEntry = entryOpt.get()
      tmpBox = stEntry.value.get()
      sectionData.docstring = some(unbox[string](tmpBox))

test "samiconf":
  addHandler(newConsoleLogger(fmtStr = "$appname: $levelname: "))

  var spec = newConfigSpec()
  var
    defaultCfgPathSpec = @[box("."), box("~")]
    defaultArtPathSpec = @[box(".")]

  spec.addGlobalAttr("config_path",
                     "[string]",
                     some(box(defaultCfgPathSpec)))
  spec.addGlobalAttr("config_filename", "string",
                     some(box("sami.conf")))
  spec.addGlobalAttr("color", "bool", some(box(false)))
  spec.addGlobalAttr("log_level", "string", some(box("warn")))
  spec.addGlobalAttr("dry_run", "string", some(box(false)))
  spec.addGlobalAttr("artifact_search_path", "[string]",
                     some(box(defaultArtPathSpec)))
  spec.addGlobalAttr("recursive", "bool", some(box(true)))
  spec.addGlobalAttr("output_dir", "string", some(box(".")))
  spec.addGlobalAttr("output_file",
                     "string",
                     some(box("sami-extractions.json")))
  
  var sectKey = spec.addSection("key", validSubSecs =
    @["*", "*.json", "*.binary"])
  
  sectKey.addAttr("required", "bool", some(box(false)))
  sectKey.addAttr("missing_action", "string", some(box("warn")))
  sectKey.addAttr("system", "bool", some(box(false)))
  sectKey.addAttr("squash", "bool", some(box(true)))
  sectKey.addAttr("standard", "bool", some(box(false)))
  sectKey.addAttr("must_force", "bool", some(box(false)))
  sectKey.addAttr("plugin_ok", "bool", some(box(true)))
  sectKey.addAttr("skip", "bool", some(box(false)))
  sectKey.addAttr("priority", "int", required = false)
  sectKey.addAttr("since", "string", required = false)
  sectKey.addAttr("type", "string", required = true)
  sectKey.addAttr("value", "@x", required = false)
  sectKey.addAttr("codec", "bool", some(box(false)))
  sectKey.addAttr("docstring", "string", required = false)

  let
    tree = parse(newStringStream(conffile))

  check tree != nil

  let ctx = tree.evalTree().getOrElse(nil)
  check ctx != nil

  ctx.addSpec(spec)  
  check ctx.validateConfig()

  var conf = ctx.loadSamiConfig()

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


#    value: Option[Box]

