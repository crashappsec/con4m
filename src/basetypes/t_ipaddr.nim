import posix, common, t_ints

type
  IPv4*     = Sockaddr_in
  IPv6*     = Sockaddr_in6

proc constructIPv4(s: string, outObj: var Mixed, st: SyntaxType):
                   string {.cdecl.} =
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
    return "Invalid IPv4 address."

  if pstr.len() == 0:
    var
      val:  uint128
      sign: bool

    let r = parseInt128(pstr, val, sign)
    if  r <= 0 or sign or val > high(uint16):
      return "Invalid port number."

    address.sin_port = uint16(val)

  outObj = address.toMixed()

proc constructIPV6(s: string, outObj: var Mixed, st: SyntaxType):
                  string {.cdecl.} =
  var address: IPv6

  let res = inet_pton(AF_INET6, cstring(s), cast[pointer](addr address))

  if res != 1:
    return "Invalid IPv6 address."

  outObj = address.toMixed()

let
  TIPv4* = addBasicType(name        = "ipv4",
                        kind        = stdOtherKind,
                        litMods     = @["ipv4", "ip"],
                        fromRawLit  = constructIPV4)
  TIPv6* = addBasicType(name        = "ipv6",
                        kind        = stdOtherKind,
                        litMods     = @["ipv6"],
                        fromRawLit  = constructIPV6)
