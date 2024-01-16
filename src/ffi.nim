import nimutils

const
  cTypeNames* = ["cvoid", "cu8", "ci8", "cu16", "ci16", "cu32", "ci32", "cu64",
                 "ci64", "cfloat", "cdouble", "cuchar", "cchar", "cshort",
                 "cushort", "cint", "cuint", "clong", "culong", "cbool",
                 "csize", "cssize", "ptr", "cstring", "carray"]
  cTypeTakesParam* = ["ptr", "carray"]

type
  FfiObj* = object
    size*:      csize_t
    alignment*: cshort
    ffitype*:   cshort
    elements*:  ptr UncheckedArray[ptr FfiObj]

  FfiType* = ptr FfiObj

  CallerInfo* = object
    abi*:       cuint
    nargs*:     cuint
    arg_types*: ptr FfiType
    rtype*:     FfiType
    bytes*:     cuint
    flags*:     cuint

  FfiStatus* {.size: sizeof(cint).} = enum
    FFI_OK, FFI_BAD_TYPEDEF, FFI_BAD_ABI

var
  ffiVoidO*   {.importc: "ffi_type_void".}: FfiObj
  ffiU8O*     {.importc: "ffi_type_uint8".}: FfiObj
  ffiI8O*     {.importc: "ffi_type_sint8".}: FfiObj
  ffiU16O*    {.importc: "ffi_type_uint16".}: FfiObj
  ffiI16O*    {.importc: "ffi_type_sint16".}: FfiObj
  ffiU32O*    {.importc: "ffi_type_uint32".}: FfiObj
  ffiI32O*    {.importc: "ffi_type_sint32".}: FfiObj
  ffiU64O*    {.importc: "ffi_type_uint64".}: FfiObj
  ffiI64O*    {.importc: "ffi_type_sint64".}: FfiObj
  ffiFloatO*  {.importc: "ffi_type_float".}: FfiObj
  ffiDoubleO* {.importc: "ffi_type_double".}: FfiObj
  ffiCharO*   {.importc: "ffi_type_sint8".}: FfiObj
  ffiuCharO*  {.importc: "ffi_type_uint8".}: FfiObj
  ffiPtrO*    {.importc: "ffi_type_pointer".}: FfiObj
  ffiVoid*:   FfiType = addr ffiVoidO
  ffiU8*:     FfiType = addr ffiU8O
  ffiI8*:     FfiType = addr ffiI8O
  ffiU16*:    FfiType = addr ffiU16O
  ffiI16*:    FfiType = addr ffiI16O
  ffiU32*:    FfiType = addr ffiU32O
  ffiI32*:    FfiType = addr ffiI32O
  ffiU64*:    FfiType = addr ffiU64O
  ffiI64*:    FfiType = addr ffiI64O
  ffiFloat*:  FfiType = addr ffiFloatO
  ffiDouble*: FfiType = addr ffiDoubleO
  ffiChar*:   FfiType = addr ffiCharO
  ffiuChar*:  FfiType = addr ffiuCharO
  ffiPtr*:    FfiType = addr ffiPtrO
  ffiAbi*: cuint = 2 # Not sure what ABI options are yet, but this works.

when sizeof(cshort) == 1:
  var
    ffiShort*  = ffiI8
    ffiUShort* = ffiU8
elif sizeof(cshort) == 2:
  var
    ffiShort*  = ffiI16
    ffiUShort* = ffiU16
elif sizeof(cshort) == 4:
  var
    ffiShort*  = ffiI32
    ffiUShort* = ffiU32
else:
  var
    ffiShort*  = ffiI64
    ffiUShort* = ffiU64

when sizeof(cint) == 1:
  var
    ffiInt*  = ffiI8
    ffiUInt* = ffiU8
elif sizeof(cint) == 2:
  var
    ffiInt*  = ffiI16
    ffiUInt* = ffiU16
elif sizeof(int) == 4:
  var
    ffiInt*  = ffiI32
    ffiUInt* = ffiU32
else:
  var
    ffiInt*  = ffiI64
    ffiUInt* = ffiU64

when sizeof(clong) == 1:
  var
    ffiLong*  = ffiI8
    ffiULong* = ffiU8
elif sizeof(clong) == 2:
  var
    ffiLong*  = ffiI16
    ffiULong* = ffiU16
elif sizeof(clong) == 4:
  var
    ffiLong*  = ffiI32
    ffiULong* = ffiU32
else:
  var
    ffiLong*  = ffiI64
    ffiULong* = ffiU64

when sizeof(csize_t) == 1:
  var
    ffiSSizeT* = ffiI8
    ffiSizeT*  = ffiU8
elif sizeof(csize_t) == 2:
  var
    ffiSSizeT* = ffiI16
    ffiSizeT*  = ffiU16
elif sizeof(csize_t) == 4:
  var
    ffiSSizeT* = ffiI32
    ffiSizeT*  = ffiU32
else:
  var
    ffiSSizeT* = ffiI64
    ffiSizeT*  = ffiU64


let
  ffiTypeNameMapping* = [ ffiVoid, ffiU8, ffiI8, ffiU16, ffiI16, ffiU32,
                          ffiI32, ffiU64, ffiI64, ffiFloat, ffiDouble,
                          ffiuChar, ffiChar, ffiShort, ffiUShort, ffiInt,
                          ffiUint, ffiLong, ffiULong, ffiU8, ffiSizeT,
                          ffiSSizet, addr ffiPtrO, addr ffiPtrO,
                          addr ffiPtrO ]

proc ffi_prep_cif*(cif: var CallerInfo, abi: cuint, nargs: cuint,
                   rtype: ptr FfiObj, argTypes:
                     pointer): FfiStatus {.discardable,
                       importc, cdecl.}

proc ffi_prep_cif_var*(cif: var CallerInfo, abi: cuint, nfixedargs: cuint,
                       ntotalargs: cuint, rtype: ptr FfiObj, argTypes:
                    pointer): FfiStatus {.
                       importc, cdecl.}

proc ffi_call*(cif: var CallerInfo, fn: pointer, ret: pointer,
               args: pointer) {.importc, cdecl.}

proc ffi_test*(s: cstring): int {.cdecl.} =
  print h2($(s))
  return 0

{.emit: """
#include <dlfcn.h>

static void *global_lookup(char *sym) {
  void *ptr = dlsym(RTLD_DEFAULT, sym);
  return ptr;
}

static void *attempt_lib_load(char *name) {
  return dlopen(name, RTLD_NOW|RTLD_GLOBAL);
}

""".}

var staticSymbols: Dict[string, pointer]
staticSymbols.initDict()

proc addStaticFunction*[T](s: string, f: T) =
  staticSymbols[s] = cast[pointer](f)

proc global_lookup(sym: cstring): pointer {.importc, cdecl, nodecl.}
proc attempt_lib_load(sym: cstring): pointer {.importc, cdecl, nodecl.}

proc findSymbol*(name: string, libs: openarray[string]): pointer =
  result = global_lookup(cstring(name))

  if result != nil:
    return

  let opt = staticSymbols.lookup(name)
  if opt.isSome():
    return opt.get()

  for item in libs:
    let r = attempt_lib_load(cstring(item))
    if r != nil:
      result = global_lookup(cstring(name))
      if result != nil:
        return

when isMainModule:
  var
    ci: CallerInfo
    v: int
    args = [ffiPtr]
    s = cstring("hello, world")
    params = [addr s]

  echo ffi_prep_cif(ci, ffiAbi, 1, ffiI64, addr args)
  ffi_call(ci, ffi_test, addr v, addr params[0])
  echo v
