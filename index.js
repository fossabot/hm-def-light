import HMLang        from "hm-lang-light"
import _runP         from "hm-lang-light/run-parser"

import TypeCarton    from "./type-carton"

import _SupraHMLang  from "./language"

export default ({create, 
                 env, 
                 $ : SDef, 
                 Z, 
                 checkTypes, 
                 typeClasses,
                 __checkTypes }) => {

  // checkTypes :: Boolean
  checkTypes = !!checkTypes

  // typeClasses :: [TypeClass]
  typeClasses = 
    typeof typeClasses === "undefined" ? [] : typeClasses

  // __checkTypes :: Boolean
  __checkTypes =
    typeof __checkTypes === "undefined" ? false : !!__checkTypes

  // runP :: String -> String -> Rose TypeCarton
  const runP       = _runP (HMLang)

  // parseEarly :: String -> Rose TypeCarton
  const parseEarly = runP ("declaration")

  const _create = checkTypes => env => create =>
    create ({
      checkTypes,
      env
    })

  const create_ = _create (checkTypes)
                          (env.concat ([TypeCarton]))

  const S    = create_ (create)
  const _def = create_ (SDef.create)

  // B combinator; alias
  const B = S.compose

  // SupraHMLang :: StrMap Parser
  const SupraHMLang      = _SupraHMLang ({S, $ : SDef, Z, typeClasses, __checkTypes})

  // parseType :: Parser
  const parseType        = B (S.map (fn => [fn.types.$1.type, fn.types.$2.type]))
                             (SupraHMLang.function)

  // groupConstraints :: [Pair Type TypeClass] -> [Pair Type [TypeClass]]
  const groupConstraints = 
    B (S.map (S.lift2 (S.Pair) 
                      (xs => S.fst (xs[0]))
                      (S.map (S.snd))))
      (S.groupBy (a => b => S.equals (S.fst (a)) (S.fst (b))))

  // constraintsFromPairs :: [Pair Type [TypeClass]] -> StrMap [TypeClass]
  const constraintsFromPairs = B (S.fromPairs) 
                                 (S.map (S.mapLeft (S.prop ("name"))))

  // parseConstraints :: [Rose TypeCarton] -> Either String (StrMap [TypeClass])
  const parseConstraints =
    B (S.map (B (constraintsFromPairs) (groupConstraints)))
      (S.traverse (S.Either) 
                  (SupraHMLang.constraint))

  /* Exports */
  // :: String -> AnyFunction # -> AnyFunction
  return _def ("def")
              ({})
              ([SDef.String, SDef.AnyFunction])
              (typeDeclaration => {

                // :: {
                //   name :: String,
                //   constraints :: [Rose TypeCarton],
                //   type :: Rose TypeCarton
                // }
                const {name, constraints, type} = parseEarly (typeDeclaration)

                // g :: Either String AnyFunction
                const g = S.lift3 (_def)
                                  (S.Right (name))
                                  (parseConstraints (constraints))
                                  (parseType        (type))

                // :: AnyFunction
                return S.either (err => {throw err})
                                (S.I)
                                (g)
              })
}