import base

var tsOps = newVTable()

let TTSpec* = addDataType(name = "typespec", concrete = true, ops = tsOps,
                          ckind = C4TypeSpec)
