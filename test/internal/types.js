import $ from "sanctuary-def"

// adapted from Sanctuary's source code

// TypeRep :: Type -> Type
const TypeRep = $.UnaryType
  ("sanctuary/TypeRep")
  ("https://github.com/fantasyland/fantasy-land#type-representatives")
  (function(x) {
     return $.AnyFunction._test (x) ||
            x != null && $.String._test (x["@@type"])
   })
  (_ => [])

// Radix :: Type
const Radix = $.NullaryType
  ("sanctuary/Radix")
  ("")
  (function(x) { return $.Integer._test (x) && x >= 2 && x <= 36 })


export {TypeRep, Radix}