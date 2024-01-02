import base

var tupOps = newVTable()

let TTuple* =  addDataType(name = "tuple", concrete = false, ops = tupOps,
                           ckind = C4Tuple)
