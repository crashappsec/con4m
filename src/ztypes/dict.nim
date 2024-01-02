import base

var dictOps = newVTable()

let TDict* =  addDataType(name = "dict", concrete = false, ops = dictOps,
                          ckind = C4Dict)

registerSyntax(TDict, STDict, @["d"], primary = true)
