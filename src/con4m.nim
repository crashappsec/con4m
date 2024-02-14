# TODO:
#
# === High priority -- before Chalk integration ===
#
# Travel for Gerhard
# Scott

# - Apply component logic in runtime:
# a) Handle defaults, setting them on component entry if needed.
# b) Auto-lock attrs that *can* be set in a component, when the component
#   exits.
# - Hook up validators for the checkpointing.
# - Allow selecting the module to fire up on restore.
# - Test 'Other' data types
# - Documentation.
# - Add code gen for tuple unpacking.
# - Array rcs
# - Space before doc string
# - Marshal secsion docs

# === Semi-high priority -- could ship internally w/ known issues ===
# - Embed f() docs as an option.
# - ZFuncInfo should have the module number, instead of relying on the
#   instruction to know.
# - container type bug noted in list
# - Save info about when last execution was, and how many executions.
# - Fill in missing error messages.
# - Proper marshaling of callbacks.
# - Lit mods should be available for runtime literal instantiation.
# - Capture location info for runtime attr def locations, and show
#   all def locations for things like spec errors (anything runtime).
# - Don't bother to issue a copy when we just instantiated a lit.
# - Remainder of needed stdlib stuff
# - Restrict the leading '$' properly.
# - Redo doc API / autogeneration
# - Typecheck c vs con4m api for ffi
# - Enums should be global by default.  Add a 'private' for enums,
#   funcs and, when they show up,
# - Some basic memory management in the runtime (dynamic alllocs are
#   currently just leaked).
# - Sort errors by file / line (they come out by phase in IR portion).
# - More Documentation.
# - When doing second pass for calls, add cast nodes where we could auto-cast.
# - Remove any remaining newRefVal / extractRef calls
# - Get control flow stuff working properly.
# - More flexibility on storing src in object
# - Automatic console err fd

# == Medium -- before public release ==
# - Integrate test command into con4m command.
# - Expect functionality in test command.
# - Flag what objdump dumps.
# - Comments in pretty(), and better spacing control based on original src.
# - Redo code gen for assignment to get rid of the extra ref/deref for index
#   ops
# - Redo the rich data type again; replace w/ a "container" type
# - Check attr to lock and attrs to access against spec statically.
# - Buffers should be mutable (like strings, they currently are not).
# - explicit casts -- to(obj, type) OR type(obj) (decide which)
# - In showCallMistakes(), show which functions have the wrong # of args,
# - Possibly allow generating a C API based on the spec.
#   and which parameters are right / wrong.
# - finish hasExitToOuterBlock in CFG.
# - Allow assignment inside var / global / const statements.
# - Use No-side-effect prop for funcs to allow calling functions at compile
#   time (and mark native f() no-side-effect if they do not use external
#   state).
#   Lots more folding work should be done.
# - Fold container literals wherever possible.
# - Access controls around extern and extensibility features.
# - ~ operator possibly should be renamed to 'lock' and not require an
#   assignment, but if there's no assignment it should error / warn if
#   one might be locking something that isn't assigned.
# - Be able to lock an entire section.
# - Warning when your declared type is more generic than the inferred type.
# - Warning when (in non-REPL-land) module vars / global vars are generic.
# - Implement _ as a 'discard' variable.
# - C api and bindings to other languages.
# - dlclose stuff.
# - Redo the CFG for SSA (cleaning up the existing bug)
# - Add some sort of mixed type
# - // for integer division.
# - REPL
# - Error if any variables not within a function do not have a
#   concrete type (after REPL is done)
# - Give names to enums / turn them into int subtypes.
# - Fold for list indexes when the length is fixed size and the list is static.
# - Add 'error' to functions.
# - string enums
# - some sort of module scope operator; perhaps root::, module::,
#   local:: I think? Or, rethink the whole import system :)
# - Keyword arguments for functions.
# - Some sort of debugger?
# - Add maybe / null checking
# - Should add variable aliases for $i and $label
# - Implement FFI Varargs.
# - Add objects, with typevars that can bind to all fields...
# - Add oneof
# - Add ref
# - Other hatrack data types
# - Code playground
# - LSP server
# - Only generate loop variables if they're used.
# - Ideally, target LLVM

# == Lower priority ==
# - For errors, make it easy to see "previous instance", and remove
#   table for 2nd line
# - Error msg squelching and colating
# - GUI for repl; show trees, etc.
# - let all the IO stuff be themeable
# - for x in <container>: generate a call to items() if the object is
#   not one of the built-in types.
# - Index for non-base types should generate a rewrite func
# - Validation routines need routines to validate their inputs.
# - TODO: should there be an option to leave functions in the module scope?
#   If so, what's the syntax?
# - Macro-style compile time rewriting and an aspect system
# - Allow arbitrary blocks within statements?
# - Check for object field is never used (or set and not read)
# - Config limit for how many errors we print.
# - Use tracking for function calls.
# - Package system.
# - Varargs for external calls.

## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022 - 2024

import std/os
import "."/getopts
import "commands"/[run, objdump, cmd_base]

const
  spec_file = joinPath(splitPath(currentSourcePath()).head,
                       "commands/spec.c4m")
  flag_spec = static_read(spec_file)


proc cmd_help(helprt: RuntimeState) =
  print helprt.searchCmdHelp(helprt.cmdline_info, config_args)

when isMainModule:
  useCrashTheme()
  setupSignalHandlers()

  let
    rt    = parse_command_line(flag_spec)
    ep    = lookup[C4Str](rt, "entry_point").get(nil)

  config_cmd           = lookup[C4Str](rt, "command").get().toNimStr()
  config_args          = lookup[Array](rt, "args").get().strlist()
  config_debug         = lookup[bool](rt, "debug").get(false)
  config_format        = lookup[bool](rt, "pretty").get(false)
  config_save_object   = lookup[bool](rt, "save_object").get(false)
  config_reentry_point = if ep == nil: "" else: ep.toNimStr()

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
  elif config_cmd == "help":
    rt.cmd_help()
  elif config_cmd == "compile":
    if config_args[0].endswith(obj_file_extension):
        print(fgcolor("error: ", "red") +
              text("Cannot compile object file ") + em(config_args[0]) +
              text(". Please specify a module entry point."))
        quit(-4)

    config_save_object = true

    session.cmd_compile()
  elif config_cmd == "run":
    if config_args[0].endswith(obj_file_extension):
      cmd_resume()
    else:
      session.cmd_compile()
  elif config_cmd == "objdump":
    cmd_objdump()
  elif config_cmd == "disassemble":
    cmd_disassemble()
  else:
    unreachable
