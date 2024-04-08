import std/posix
import ".."/common
import "."/backtrace

let sigNameMap = { 1: "SIGHUP", 2: "SIGINT", 3: "SIGQUIT", 4: "SIGILL",
                   6: "SIGABRT",7: "SIGBUS", 9: "SIGKILL", 11: "SIGSEGV",
                   15: "SIGTERM" }.toDict()
var
  LC_ALL {.importc, header: "<locale.h>".}: cint
  savedTermState: Termcap

proc restoreTerminal() {.noconv.} =
  tcSetAttr(cint(1), TcsaConst.TCSAFLUSH, savedTermState)

proc regularTerminationSignal(signal: cint) {.noconv.} =
  let pid = getpid()

  print_err(h5("pid: " & $(pid) & " - Aborting due to signal: " &
          sigNameMap[signal] & "(" & $(signal) & ")"))

  let rt = getCon4mRuntime()

  if rt != nil and rt.running:
    print_err(getCon4mRuntime().get_stack_trace())
  else:
    print_err(rich"[h2]Program was [i]NOT[/i] executing when we crashed.")

  when defined(debug):
    print(rich"[h4]Nim stack trace:")
    echo getStackTrace()
  else:
    print(rich"[h4]Nim stack trace is unavailable (must compile " +
          rich "w/ [b]-d:debug[/] for traces)")
  var sigset:  SigSet

  discard sigemptyset(sigset)

  for signal in [SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGKILL,
                 SIGSEGV, SIGTERM]:
    discard sigaddset(sigset, signal)
  discard sigprocmask(SIG_SETMASK, sigset, sigset)


  exitnow(signal + 128)

proc setlocale(category: cint, locale: cstring): cstring {.importc, cdecl,
                                nodecl, header: "<locale.h>", discardable .}

proc setupTerminal*() =
  setlocale(LC_ALL, cstring(""))
  tcGetAttr(cint(1), savedTermState)
  addQuitProc(restoreTerminal)

proc setupSignalHandlers*() =
  var handler: SigAction

  handler.sa_handler = regularTerminationSignal
  handler.sa_flags   = 0

  for signal in [SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGKILL,
                 SIGSEGV, SIGTERM]:
    discard sigaction(signal, handler, nil)
