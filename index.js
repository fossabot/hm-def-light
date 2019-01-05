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
                 typeConstructors }) => {

  // checkTypes :: Boolean
  checkTypes = !!checkTypes

  // typeClasses :: [TypeClass]
  typeClasses = 
    typeof typeClasses === "undefined" ? [] : typeClasses

  // # typeConstructors :: StrMap ((Type ->)^{1,2} Type)
  typeConstructors =
    typeof typeConstructors === "undefined" ? {} : typeConstructors

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

  // internally, only S.unchecked is used
  const {unchecked : U} = S

  // B combinator; alias
  const B = U.compose

  // maybeToList :: Maybe a -> [a]
  const maybeToList = U.lift2 (U.maybe) (U.empty) (U.of) (Array)

  // extractTypeLL :: Type -> Maybe Type
  const extractTypeLL = k => U.gets (_ => true) (["types", k, "type"])

  // adaptFn :: Type -> [Type]
  const adaptFn = Fn => U.on (U.on (U.concat) (maybeToList))
                             (U.flip (extractTypeLL) (Fn))
                             ("$1") ("$2")

  // SupraHMLang :: StrMap Parser
  const SupraHMLang      = _SupraHMLang ({S, $ : SDef, Z, typeClasses, typeConstructors})

  // parseType :: Parser
  const parseType        = B (U.map (adaptFn))
                             (SupraHMLang.function)

  // groupConstraints :: [Pair Type TypeClass] -> [Pair Type [TypeClass]]
  const groupConstraints = 
    B (U.map (U.lift2 (U.Pair)
                      (xs => U.fst (xs[0]))
                      (U.map (U.snd))))
      (U.groupBy (U.on (U.equals) (B (U.prop ("name")) (U.fst))))

  // constraintsFromPairs :: [Pair Type [TypeClass]] -> StrMap [TypeClass]
  const constraintsFromPairs = B (U.fromPairs)
                                 (U.map (U.mapLeft (U.prop ("name"))))

  // parseConstraints :: [Rose TypeCarton] -> Either String (StrMap [TypeClass])
  const parseConstraints =
    B (U.map (B (constraintsFromPairs) (groupConstraints)))
      (U.traverse (U.Either)
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
                const g = U.lift3 (_def)
                                  (U.Right (name))
                                  (parseConstraints (constraints))
                                  (parseType        (type))

                // :: AnyFunction
                return U.either (err => {throw err})
                                (U.I)
                                (g)
              })
}