## Makes it easy to build Apache-style configuration files with
## well-defined schemas, where you don't have to do significant work.
##
## And the people who write configuration files, can do extensive
## customization using the con4m language, which is built in a way
## that guarantees termination (e.g., no while loops, for loop index
## variables are immutible to the programmer).
## 
## :Author: John Viega (john@crashoverride.com)
## :Copyright: 2022

import con4m/parse
export getOrElse, fatal, parse.parse, Con4mError

## This operates on tokens, as already produced by lex().  It simply
## kicks off the parser by entering the top-level production (body),
## and prints out any error message that happened during parsing.

import con4m/treecheck
export checkTree

# Runtime bits
import con4m/eval
export evalTree, evalConfig

import con4m/box
export box

import con4m/builtins
export builtinIToS, builtinBToS, builtinFToS, builtInItoB, builtinFtoB,
       builtinStoB, builtinLToB, builtinDToB, builtinIToF, builtinFtoI,
       builtinSplit, builtinEcho, builtinEnv, builtinEnvExists,
       builtinEnvAll, builtinStrip, builtinContainsStrStr,
       builtinFindFromStart, builtinSlice, builtinSliceToEnd, builtInAbort,
       builtinCmd, newBuiltIn, addDefaultBuiltins


# Post-runtime (spec checking)
import con4m/spec
export newConfigSpec, addGlobalAttr, addSection, addAttr, validateConfig,
       newConfigState, getConfigVar, getAllSectionSTs, addSpec, lockConfigVar,
       setOverride

# Simplification via complexity
import con4m/codegen
export codegen.con4m, configDef

when defined(testCases):
  # There are really just exposed for our tests.  Should change that.
   import con4m/st
   export lookupAttr
   import con4m/dollars
   export dollars

