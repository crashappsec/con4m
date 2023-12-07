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

import err, lex, basetypes, parse

export err, lex, basetypes, parse

when isMainModule:
  import strutils
  useCrashTheme()

  proc utest(t1, t2: TypeRef) =
    assert t1 != nil
    assert t2 != nil
    let
      r1  = t1.toRope()
      r2  = t2.toRope()
      res = t1.unify(t2).toRope().italic().fgColor("atomiclime")

    print(r1 + text(" U ") + r2 + text(" = ") + res)

  proc utest(t1, t2: TypeId) =
    utest(t1.idToObj(), t2.idToObj())

  proc htest(t: TypeRef) =
    print fgColor(t.typeId.toHex(), "jazzberry") + text(" ") + t.toRope() +
             text(" -> ") + fgColor(t.typeHash().toHex(), "jazzberry")

  utest(tInt(), tInt())
  utest(tInt(), tBool())

  let t1 = tList(tInt())
  let t2 = tList(tString())
  let t3 = tList(newTypeVar())
  let t4 = newTypeVar()
  let t5 = tDict(tString(), newTypeVar())
  let t6 = tDict(newTypeVar(), newTypeVar())

  print h2("Type hash tests")
  htest tInt()
  htest tString()
  htest t1
  htest t2
  htest t3
  htest t4
  htest t1.copyType()
  htest t2.copyType()
  htest t3.copyType()
  htest t4.copyType()

  print h2("Container unification")
  utest(t1, t4) # expect list[int]
  utest(t2, t3) # expect list[string]
  utest(t3, t4) # expect bottom
  utest(t3, t5) # expect bottom
  utest(t5, t6) # expect dict[`t]

  print h2("Parsing types")

  print parseType("dict[int, string]").toRope()
  print parseType("dict[int, `x]").toRope()
  print parseType("tuple[`x, `y, `x]").toRope()
  print parseType("ref[tuple[`x, `y, `x]]").toRope()
  print parseType("oneof[int, string, bool]").toRope()
  print parseType("maybe[int]").toRope()

  print h2("Additional unification")
  utest(parseType("(int)->`t"), parseType("(`t)->`t"))   # (int) -> int
  utest(parseType("(*`t)->`v"), parseType("(*int)->`t")) # expect (*int) -> `t
  utest(parseType("(*`t)->`v"), parseType("(int, *int)->`t")) # expect bottom
  utest(parseType("(*`t)->`v"), parseType("(int, int, `t)->`t")) # (*int)->int
  utest(parseType("oneof[int, string, bool]"), parseType("int"))
