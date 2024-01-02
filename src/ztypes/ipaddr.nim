import posix, base

proc new_ipv4_lit(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer {.cdecl.}
proc new_ipv6_lit(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer {.cdecl.}

type
  IPv4*     = Sockaddr_in
  IPv6*     = Sockaddr_in6

var
  ip4Ops = newVTable()
  ip6Ops = newVTable()

proc repr4(pre: pointer): string {.cdecl.} =
  # TODO: port as well.
  var
    inaddr = extractRef[Ipv4](pre)
    mem:     cstring = cast[cstring](alloc(128))
    s      = inet_ntop(AF_INET, cast[pointer](addr inaddr), mem, 128)

  return $(s)

proc repr6(pre: pointer): string {.cdecl.} =
  # TODO: port as well.
  var
    inaddr = extractRef[Ipv6](pre)
    mem:     cstring = cast[cstring](alloc(128))
    s      = inet_ntop(AF_INET6, cast[pointer](addr inaddr), mem, 128)

  return $(s)

proc ipv4_eq(a, b: pointer): bool {.cdecl.} =
  var
    d1 = extractRef[Ipv4](a)
    d2 = extractRef[Ipv4](b)

  let r = memcmp(cast[pointer](d1), cast[pointer](d2), csize_t(sizeof(Ipv4)))

  return r == 0

proc ipv6_eq(a, b: pointer): bool {.cdecl.} =
  var
    d1 = extractRef[Ipv6](a)
    d2 = extractRef[Ipv6](b)

  let r = memcmp(cast[pointer](d1), cast[pointer](d2), csize_t(sizeof(Ipv6)))

  return r == 0

ip4Ops[FRepr]   = cast[pointer](repr4)
ip4Ops[FEq]     = cast[pointer](ipv4_eq)
ip4Ops[FnewLit] = cast[pointer](new_ipv4_lit)
ip6Ops[FRepr]   = cast[pointer](repr6)
ip6Ops[FEq]     = cast[pointer](ipv6_eq)
ip6Ops[FnewLit] = cast[pointer](new_ipv6_lit)

let
  TIPv4* = addDataType(name = "ipv4", concrete = true, ops = ip4Ops)
  TIPv6* = addDataType(name = "ipv6", concrete = true, ops = ip4Ops)


registerSyntax(TIPv4, STOther, @["ip"])
registerSyntax(TIPv4, STStrQuotes, @["ip"])
registerSyntax(TIPv6, STOther, @[])
registerSyntax(TIPv6, STStrQuotes, @[])

proc new_ipv4_lit(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer =
  var
    s = s
    pstr:    string
    address: IPv4

  if ':' in s:
    let ix = s.find(':')
    pstr = s[ix + 1 .. ^1]
    s    = s[0 ..< ix]

  let res = inet_pton(AF_INET, cstring(s), cast[pointer](addr address))
  if res != 1:
    err = "BadIPv4"
    return

  if pstr.len() == 0:
    var
      val:  uint128
      sign: bool

    let r = parseInt128(pstr, val, sign)
    if  r <= 0 or sign or val > high(uint16):
      err = "BadPort"
      return

    address.sin_port = uint16(val)

  return newRefValue[IpV4](address, TIPv4)

proc new_ipv6_lit(s: string, st: SyntaxType, lmod: string,
                  err: var string): pointer =
  var
    address: IPv6

  var res = inet_pton(AF_INET6, cstring(s), cast[pointer](addr address))

  if res != 1:
    err = "BadIPv6"
    return

  return newRefValue[IpV6](address, TIPv6)
