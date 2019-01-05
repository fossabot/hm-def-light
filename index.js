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

  // B combinator; alias
  const B = S.compose

  // maybeToList :: Maybe a -> [a]
  const maybeToList = S.lift2 (S.maybe) (S.empty) (S.of) (Array)

  // extractTypeLL :: Type -> Maybe Type
  const extractTypeLL = k => S.gets (_ => true) (["types", k, "type"])

  // adaptFn :: Type -> [Type]
  const adaptFn = Fn => S.on (S.on (S.concat) (maybeToList)) 
                             (S.flip (extractTypeLL) (Fn))
                             ("$1") ("$2")

  // SupraHMLang :: StrMap Parser
  const SupraHMLang      = _SupraHMLang ({S, $ : SDef, Z, typeClasses, typeConstructors})

  // parseType :: Parser
  const parseType        = B (S.map (adaptFn))
                             (SupraHMLang.function)

  // groupConstraints :: [Pair Type TypeClass] -> [Pair Type [TypeClass]]
  const groupConstraints = 
    B (S.map (S.lift2 (S.Pair) 
                      (xs => S.fst (xs[0]))
                      (S.map (S.snd))))
      (S.groupBy (S.on (S.equals) (B (S.prop ("name")) (S.fst))))

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