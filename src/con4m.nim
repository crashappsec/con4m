# === High priority -- before Chalk integration ===

# - Apply component logic in runtime:
# a) Handle defaults, setting them on component entry if needed.
# b) Auto-lock attrs that *can* be set in a component, when the component
#   exits.
# - Test user-defined validaton functions

## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022 - 2024

import "std"/os
import "."/[getopts, docsapi]
import "commands"/[run, objdump, cmd_base]
export docsapi

const
  spec_file = joinPath(splitPath(currentSourcePath()).head,
                       "commands/spec.c4m")
  flag_spec = static_read(spec_file)


proc cmd_help(helprt: RuntimeState) =
  var args: seq[Rich]

  for item in config_args:
    args.add(r(item))

  print helprt.searchCmdHelp(helprt.cmdline_info, args)

when isMainModule:
  install_default_styles()
  setupSignalHandlers()

  let
    rt    = parse_command_line(flag_spec)
    ep    = lookup[Rich](rt, "entry_point").get(nil)
    ll    = lookup[Rich](rt, "log_level").get(nil)

  config_cmd           = lookup[Rich](rt, "command").get().toNimStr()
  config_args          = lookup[List[Rich]](rt, "args").get().items().toSeqStr()
  config_debug         = lookup[bool](rt, "debug").get(false)
  config_format        = lookup[bool](rt, "pretty").get(false)
  config_save_object   = lookup[bool](rt, "save_object").get(false)
  config_reentry_point = if ep == nil: "" else: ep.toNimStr()

  if ll != nil:
    let ch = (cast[cstring](ll))[0]

    if ch == 'w':
      config_log_level = LlWarn
    elif ch == 'e':
      config_log_level = LlErr

  var
    altPath = $(getEnv("CON4M_PATH"))
    session = newCompileContext(nil)

  if altPath != "":
    session.modulePath = altPath.split(Rune(':'))
  if config_cmd in ["lex", "parse", "check"]:
    session.cmd_stop_early()
  elif config_cmd == "pretty":
    session.cmd_pretty()
  elif config_cmd == "resume":
    cmd_resume()
  elif config_cmd == "add":
    cmd_add()
  elif config_cmd == "compile":
    session.cmd_compile()
  elif config_cmd == "help":
    rt.cmd_help()
  elif config_cmd == "run":
    if config_args[0].endswith(obj_file_extension):
      cmd_resume()
    else:
      session.cmd_run()
  elif config_cmd == "objdump":
    cmd_objdump()
  elif config_cmd == "disassemble":
    cmd_disassemble()
  else:
    unreachable
