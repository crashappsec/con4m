import unittest, nimutils, macros, os, strutils, osproc

let passString = toAnsiCode(acBGreen) & "[PASSED]" & toAnsiCode(acReset)
let failString = toAnsiCode(acBRed)   & "[FAILED]" & toAnsiCode(acReset)
var fails = 0

macro runTests(dir: static[string], cmd: untyped): untyped =
  result = newStmtList()

  let dirpath = staticExec("pwd") & "/" & dir & "/"
  for filepath in staticListFiles(dirpath & "*.c4m"):
    let
      (dname, fname, ext) = filepath.splitFile()

    result.add quote do:
      try:
        let
          cmdline = `cmd` & `filepath`
        echo cmdline
        let
          output  = execCmdEx(cmdline).output.strip()
          kat     = open(`dname` & "/" & `fname` & ".kat").readAll().strip()

        if output == kat:
          echo passString & " Test " & `fname`
        else:
          fails = fails + 1
          echo failString & " test " & `fname`
          echo "GOT:"
          echo output
          echo "EXPECTED: "
          echo kat
      except:
        fails = fails + 1
        echo "Exception raised: "
        echo getStackTrace()
        echo getCurrentExceptionMsg()


runTests("basics"):
  "./con4m --no-color --none "

runTests("spec"):
  "./con4m --no-color --none --c42 "
check fails == 0
