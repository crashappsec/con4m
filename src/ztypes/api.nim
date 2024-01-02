import typecheck
export typecheck


proc get_container_info*(t: TypeId): DataType =
  let to = t.idToTypeRef()

  case to.kind
  of C4List:
    return tinfo(TList)
  of C4Dict:
    return tinfo(TDict)
  of C4Tuple:
    return tinfo(TTuple)
  else:
    unreachable

proc hasFunc*(t: TypeId, fn: int): bool =
  let info = t.tinfo()

  if fn >= info.ops.len():
    return false

  if info.ops[fn] == nil:
    return false

  return true

proc get_cast_fn*(tcur, tdst: DataType, err: var string): pointer =
  let op = cast[GetCastFn](tcur.ops[FCastFn])

  if op == nil:
    err = "CannotCast"
  else:
    return op(tdst, err)

proc can_cast_to_bool*(tid: TypeId): bool =
  var err: string

  if tid.isBasicType():
    return true

  let
    ct = tid.get_container_info()
    fn = get_cast_fn(ct, tinfo(TBool), err)

  return fn != nil

template int_bits*(tid: TypeId): int =
  if tid.is_basic_type():
    tid.tinfo().intw
  else:
    0

proc call_cast*(value: pointer, tcur, tdst: TypeId, err: var string): pointer =
  let
    dtcur = tinfo(tcur)
    dtdst = tinfo(tdst)
    fn    = cast[Castfn](dtcur.get_cast_fn(dtdst, err))

  if fn != nil:
    return fn(value)

template decl_bool_call_fn(fnname: untyped, opid: untyped) =
  proc fnname*(v1, v2: pointer, t: TypeId): bool =
    let
      info = dataTypeInfo[int(t)]
      op   = cast[BoolRetfn](info.ops[opid])

    return op(v1, v2)

template decl_bin_call_fn(fnname: untyped, opid: untyped) =
  proc fnname*(v1, v2: pointer, t: TypeId): pointer =
    let
      info = dataTypeInfo[int(t)]
      op   = cast[BinOpFn](info.ops[opid])

    return op(v1, v2)

decl_bool_call_fn(call_eq, FEq)
decl_bool_call_fn(call_lt, FLt)
decl_bool_call_fn(call_gt, FGt)
decl_bin_call_fn(call_add, FAdd)
decl_bin_call_fn(call_sub, FSub)
decl_bin_call_fn(call_mul, FMul)
decl_bin_call_fn(call_fdiv, FFDiv)
decl_bin_call_fn(call_idiv, FIDiv)
decl_bin_call_fn(call_mod, FMod)
decl_bin_call_fn(call_shl, FShl)
decl_bin_call_fn(call_shr, FShr)
decl_bin_call_fn(call_band, FBand)
decl_bin_call_fn(call_bor, FBor)
decl_bin_call_fn(call_bxor, FBxor)

proc call_index*(c: pointer, i: int, t: TypeId, err: var bool): pointer =
  let
    info = t.tinfo()
    op   = cast[IndexFn](info.ops[FIndex])

  return op(c, i, err)

proc call_dict_index*(c: pointer, i: pointer, t: TypeId,
                      err: var bool): pointer =
  let
    info = t.tinfo()
    op   = cast[DIndexFn](info.ops[FDictIndex])

  return op(c, i, err)

proc call_slice*(c: pointer, i, j: int, t: TypeId, err: var bool): pointer =
  let
    info = t.tinfo()
    op   = cast[SliceFn](info.ops[FSlice])

  return op(c, i, j)

proc call_assign_ix*(c, v: pointer, i: int, t: TypeId, err: var bool) =
  let
    info = t.tinfo()
    op   = cast[AssignIxFn](info.ops[FAssignIx])

  op(c, v, i, err)

proc call_assign_dict_ix*(c, v, i: pointer, t: TypeId, err: var bool) =
  let
    info = t.tinfo()
    op   = cast[SetDixFn](info.ops[FAssignDIx])

  op(c, v, i, err)

proc call_assign_slice*(c, v: pointer, i, j: int, t: TypeId,
                     err: var bool) =
  let
    info = t.tinfo()
    op   = cast[ASliceFn](info.ops[FAssignSlice])

  op(c, v, i, j, err)

proc get_tinfo_from_lit*(st: SyntaxType, litmod: string): DataType =
  # TODO: handle other lits
  if litmod == "":
      return syntaxInfo[int(st)].primary
  else:
    for (lm, dt) in syntaxInfo[int(st)].litmods:
      if litmod == lm:
        return dt

proc get_tid_for_simple_lit*(st: SyntaxType, litmod: string): TypeId =
  let dt = get_tinfo_from_lit(st, litmod)
  return dt.dtid

proc get_tid_for_container_lit*(st: SyntaxType, litmod: string,
                            ids: seq[TypeId] = @[], err: var bool): TypeId =
  let dt = get_tinfo_from_lit(st, litmod)

  if dt != nil:
    if dt.concrete:
      return dt.dtid

    return newContainerType(dt.ckind, ids).typeId
  else:
    err = true

proc instantiate_basic_lit*(id: TypeId, s: pointer, st: SyntaxType,
                            litmod: string, err: var string): pointer =
  let
    info = dataTypeInfo[id]
    op   = cast[NewLitFn](info.ops[FNewLit])

  return op(s, st, litmod, err)

proc instantiate_container*(t: TypeId, st: SyntaxType, litmod: string,
                            contents: seq[pointer], err: var string): pointer =
  let info = t.getContainerInfo()

  if info == nil:
    err = "BadLitMod"
    return nil

  let op = cast[CLitFn](info.ops[FContainerLit])

  return op(st, litmod, t, contents, err)

proc call_copy*(p: pointer, t: TypeId): pointer =
  let
    info = t.tinfo()
    op   = cast[CopyFn](info.ops[FCopy])

  return op(p)

proc call_len*(p: pointer, t: TypeId): int =
  let
    info = t.tinfo()
    op   = cast[LenFn](info.ops[FLen])

  return op(p)

proc call_plus_eq_ref*(v1, v2: pointer, t: TypeId) =
  let
    info = t.tinfo()
    op   = cast[PlusEqFn](info.ops[FPlusEqRef])

  op(v1, v2)
