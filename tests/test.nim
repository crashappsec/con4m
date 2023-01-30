import unittest, logging, streams, nimutils, macros, os, strutils, osproc

macro runTests(dir: static[string]): untyped =
  result = newStmtList()
  
  let dirpath = staticExec("pwd") & "/" & dir & "/"
  for filepath in staticListFiles(dirpath & "*.c4m"):
    let
      (dname, fname, ext) = filepath.splitFile()
      kat   = staticRead(dname & "/kats/" & fname & ".kat").strip()
      cmd   = "./con4m " & filepath
    var val = FAILED
    result.add quote do:
      let output = execCmdEx(`cmd`).output.strip()
      if output == `kat`:
        echo "Test " & `fname` & " passed"
      else:
        echo "GOT:"
        echo output
        echo "EXPECTED: "
        echo `kat`

runTests("basics")
