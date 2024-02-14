import std/[re, algorithm]
import "."/compile

template error(msg: Rope) =
  print(fgcolor("error: ", "red") + msg, file = stderr)
  if not thisOneFailed:
    fails += 1
    thisOneFailed = true

template error(msg: string) =
  error(text(msg))

template printIfVerbose(r: Rope) =
  if verbose:
    print(r)

proc extractErrorCodes(s: string): seq[string] =
  if s == "":
    return
  else:
    return s.findAll(re"\([A-Z0-9][a-zA-Z0-9$_]+\)")

proc runInitialTests(verbose = true) =
  var
    path    = getCurrentDir()
    testDir = path.joinPath("tests")
    files   = testDir.getAllFileNames()
    total   = 0
    fails   = 0

  files.sort()

  putEnv("CON4M_PATH", getCurrentDir().joinPath("tests"))

  for item in files:
    var thisOneFailed = false
    let (dir, name, ext) = item.splitFile()

    if ext != ".c4m":
      continue

    total += 1

    printIfVerbose(h2("Test: " & name))

    let
      output = runCmdGetEverything("./con4m", @["run", item])
      kat    = unicode.strip(tryToLoadFile(dir.joinPath(name & ".kat")))
      errout = unicode.strip(output.getStderr().replace("\e[0m", ""))
      excode = output.getExit()
    if kat == "":
      if errout != "":
        error("Unexpected errors: ")
        echo errout
        continue
      elif excode != 0:
        error("Non-zero exit code (but with no error output?")
        continue
    else:
      var
        expectSuccess = true
        errcodes      = errout.extractErrorCodes()
        katcodes: seq[string]
        parts = kat.split("\n")

      for i, item in parts:
        if i == 0:
          if item == "ok":
            expectSuccess = true
            continue
          elif item == "fail":
            expectSuccess = false
            continue
        # Put the text to match in parens to keep tests looking for
        # error codes instead of matching specific tests.
        katCodes.add("(" & item & ")")

      if katCodes.join("\n") != errcodes.join("\n"):
        error("Expected error codes did not match found error codes.")
        print h3("Expected codes:")
        print(ol(katcodes))
        print h3("Acutal codes:")
        print(ol(errcodes))
        print h3("Full error output:")
        echo errout
      if expectSuccess and excode != 0:
        error("Compile did not pass as expected.")
      elif excode == 0 and not expectSuccess:
        error("Compilation passed, but should have failed.")
    #if not thisOneFailed:
    #  printIfVerbose(fgcolor("PASS", "atomiclime"))

  if fails == 0:
    print h4("All tests passed.")
  else:
    print h4("Failed " & $(fails) & " tests (out of " & $(total) & ")")
  quit(fails)

when isMainModule:
  useCrashTheme()
  var initialTests: bool = true

  for item in commandLineParams():
    case item
    of "basic", "all":
      initialTests = true
    else:
      print h1("Unknown argument: " & item)

  if initialTests:
    runInitialTests()
