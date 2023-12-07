import nimutils

type
  Con4mErrType* = enum ErrLex, ErrParse
  Con4mError* = object
    kind*:  Con4mErrType
    msg*:   Rope
    token*: pointer
    node*:  pointer

    when not defined(release):
      st*:  string
      ii*:  tuple[filename: string, line: int, column: int]

  Con4mException = ref object of ValueError
    errList*: seq[Con4mError]

proc newCon4mException*(err: Con4mError): Con4mException =
  result = new(Con4mException)
  result.errList = @[err]

proc newCon4mException*(errs: seq[Con4mError]): Con4mException =
  result = new(Con4mException)
  result.errList = errs
