import {parse as parseTypeId} from "sanctuary-type-identifiers"

import TypeCarton from "./type-carton"

export default ({S, $ : SDef, Z, typeClasses, typeConstructors}) => {

  /* Dependencies */

  // internally, only S.unchecked is used
  const {unchecked : U} = S

  const {
    // Birds
    I,
    K,
    T,
    flip: C,
    compose: B,
    // A = I, W = join, S = ap

    // 'idiomatic'
    pipe,
    map,
    ap,
    lift2,
    zip,
    zipWith,
    chain,
    join,
    reduce,
    traverse,
    sequence,
    alt,
    filter,
    ifElse,
    show,
    lte,
    equals,
    gte,
    and,
    elem,
    size,
    prepend,
    append,
    concat,
    Pair,
    Either,
    Left,
    Right,
    either,
    isJust,
    isLeft,
    tagBy,
    mapLeft,
    maybeToEither

  } = U

  const {
      Type,
      TypeClass,
      TypeVariable,
      UnaryTypeVariable,
      BinaryTypeVariable,
      RecordType,
      Array: ArrayType,
      Function: FunctionType

  } = SDef

  /* Prelude */

  // type Fix f = f (Fix f)

  // type RoseF a b = [a, [b]]

  // type Rose a = Fix (RoseF a)

  // type TypeCarton = {type :: String, text :: String}

  // type Parser = Rose TypeCarton -> Either String Any # =
  // # ...              ::                          Type
  // # thunk            ::                          Symbol
  // # constraint       ::                          Pair Type TypeClass

  // type QuasiParser = StrMap Parser -> Parser

  // # type Type' = (Type ->)^{0,1,2} Type

  // typeClassesBaseline :: [String]
  const typeClassesBaseline = [
      "Setoid", "Ord", "Semigroupoid", "Category", "Semigroup", "Monoid", "Group",
      "Foldable", "Functor", "Contravariant", "Filterable", "Traversable", "Profunctor",
      "Bifunctor", "Alt", "Apply", "Extend", "Plus", "Applicative", "Chain", "Comonad",
      "Alternative", "Monad", "ChainRec"
  ]

  // polyVariadicMagic :: ((a ->)^n b) -> [a] -> b
  const polyVariadicMagic = U.reduce (I)

  // isRoseF :: Type -> Type -> Boolean
  const isRoseF = a => b => U.is (SDef.Array2 (a) (b))

  // unsafeHead :: [a] -> a
  const unsafeHead = xs => xs[0]

  // ErrorMsg :: {
  //  simulacrum :: String -> String -> String,
  //  unrecognizedType      :: String -> String,
  //  unrecognizedTypeClass :: String -> String
  // }
  const ErrorMsg = (() => {
    const simulacrum = expected => actual =>
      `Expected ${expected};\nActual: ${actual}`

    const unrecognizedType      = x =>
      `Unrecognized \`Type\` ${x}`

    const unrecognizedTypeClass = x =>
      `Unrecognized \`TypeClass\` ${x}`

    return {simulacrum, unrecognizedType, unrecognizedTypeClass}
  }) ()

  // indexBy :: (a -> String) -> b -> StrMap a
  const indexBy = f =>
    reduce (xs => x => U.insert (f (x)) (x) (xs))
           ({})

  // text :: a -> b
  const text   = U.prop ("text")

  // name :: a -> b
  const name   = U.prop ("name")

  // length :: a -> b
  const length = U.prop ("length")

  // lengthCmp :: a -> Boolean
  const lengthCmp = C (B) (length)

  // sizeCmp :: Foldable a => a -> Boolean
  const sizeCmp   = C (B) (size)

  // nonEmptyText :: a -> Either String String
  const nonEmptyText =
    ifElse (B (lengthCmp (gte (1))) (text))
           (B (Right) (text))
           (x => Left (`Empty text in ${show (x)}`))

  // UnitSym :: Symbol
  const UnitSym = Symbol ("Unit") // ()

  // Rose :: {
  //  root   :: Rose a -> a
  //  forest :: Rose a -> [a] 
  // }
  const Rose = (() => {
    const root   = rose => rose[0]
    const forest = rose => rose[1]

    return {root, forest}
  }) ()

  // Combinators :: {
  //  exactly, atLeast, atMost :: Foldable a => Number -> a -> Either String a
  // }
  const Combinators = (() => {

    // f :: Foldable a => (String, Number -> Boolean) -> Number -> a -> Either String a
    const f = ([desc, cmpFn]) => n => {
      const dual = n === 1 ? "" : "s"

      const err  = `a \`Foldable\` with ${desc} ${n} element${dual}`

      const g = B (mapLeft (B (ErrorMsg.simulacrum (err)) (show)))
                  (tagBy (sizeCmp (cmpFn (n))))

      return g
    }

    return map (f) ({
      exactly : ["exactly",  equals],
      atLeast : ["at least", gte],
      atMost  : ["at most",  lte]
    })
  }) ()

  // empty    :: [a] -> Either String (b -> b)
  const empty    = B (map (C (K)))
                     (Combinators.exactly (0))

  // nonEmpty :: [a] -> Either String [a]
  const nonEmpty = Combinators.atLeast (1)

  // single   :: [a] -> Either String a 
  const single   = B (map (unsafeHead))
                     (Combinators.exactly (1))

  // many     :: [a] -> Either String [a]
  const many     = Combinators.atLeast (2)

  // P :: {
  //  match :: [String] -> StrMap Parser -> Either String Parser,
  //  ward :: Pair String QuasiParser -> Pair String QuasiParser,
  //  createLanguage :: StrMap QuasiParser -> StrMap Parser
  // }
  const P = (() => {
    const match = expected => $ => s => {

      return U.ifElse (C (elem) (expected))
                      (k => $[k] (s))
                      (B (Left) (ErrorMsg.simulacrum (`any of ${show (expected)}`)))
                      (U.prop ("type") (Rose.root (s)))
    }

    const ward = p => {
      // parserId :: String
      const parserId = U.fst (p)

      return Pair (parserId) ($ => s => {

        if(!isRoseF (TypeCarton) (SDef.Any) (s)) {
          return Left (ErrorMsg.simulacrum ("a Rose TypeCarton") (show (s)))
        }

        // reqParser :: String
        const reqParser = U.prop ("type") (Rose.root (s))

        return reqParser === parserId ?
          U.snd (p) ($) (s) : Left (ErrorMsg.simulacrum (`a ${parserId}`) (`a ${reqParser}`))
      })
    }

    // scope binder; utility
    const createLanguage = parsers => {
      const language = {}

      for (let x of U.pairs (parsers)) {
        const parserId = U.fst (x)

        language[parserId] = s => mapLeft (err => `<${parserId}> ${err}`) 
                                          (U.snd (x) (language) (s))
      }

      return language
    }

    return {match, ward, createLanguage}
  }) ()

  // typeName :: Type -> String
  const typeName = B (name) (B (parseTypeId) (name))

  // isType :: a -> Boolean
  const isType =
    U.anyPass ([
      // Type ?
      U.is (Type),
      // Type -> Type ?
      U.is (FunctionType ([Type, Type])),
      // Type -> Type -> Type ?
      U.is (FunctionType ([Type, FunctionType ([Type, Type])]))
    ])

  // isTypeClass :: a -> Boolean
  const isTypeClass = U.is (TypeClass)

  // _typeMap :: Either String (StrMap Type')
  const _typeMap  = (() => {

    // tm0 :: StrMap Type'
    const tm0 = concat (indexBy (typeName) (S.env))
                       ({"Maybe"    : S.MaybeType,
                         "Either"   : S.EitherType,
                         "Pair"     : S.PairType,
                         "StrMap"   : SDef.StrMap,
                         "Array2"   : SDef.Array2,
                         "Array"    : ArrayType,
                         "Nullable" : SDef.Nullable,
                         "NonEmpty" : SDef.NonEmpty,
                         "Any"      : SDef.Any       })

    // tm1 :: Either String (StrMap Type')
    const tm1 = B (mapLeft (B (ErrorMsg.unrecognizedType) (U.fst)))
                  (map (U.fromPairs))
                       (traverse (Either)
                                 (tagBy (lift2 (and)
                                               (B (isType) (U.snd))
                                               (B (isJust)
                                                  (B (C (U.get (_ => true)) (tm0))
                                                     (U.fst)))))
                                 (U.pairs (typeConstructors)))

    // :: Either String (StrMap Type')
    return lift2 (concat) (Right (tm0))
                          (tm1)
  }) ()

  // typeMap :: StrMap Type'
  const typeMap = either (err => {throw err}) (I) (_typeMap)

  // _typeClassMap :: StrMap TypeClass
  const _typeClassMap = (() => {

    // tcm0 :: StrMap TypeClass
    const tcm0 = 
      U.justs (map (C (U.get (isTypeClass)) (Z))
                   (U.fromPairs (join (zip) (typeClassesBaseline))))
    
    // tcm1 :: Either a (StrMap TypeClass)
    const tcm1 = 
      B (mapLeft (B (ErrorMsg.unrecognizedTypeClass) (show)))
        (map (U.fromPairs))
             (map (xs => zip (map (typeName) (typeClasses)) (xs))
                  (traverse (Either)
                            (tagBy (isTypeClass))
                            (typeClasses)))

    // :: Either String (StrMap TypeClass)
    return lift2 (concat)
                 (Right (tcm0))
                 (tcm1)
  }) ()

  // typeClassMap :: StrMap TypeClass
  const typeClassMap = either (err => {throw err}) (I) (_typeClassMap)

  // fetchType :: String -> Either String Type
  const fetchType = name => 
    maybeToEither (ErrorMsg.unrecognizedType (name))
                  (U.get (isType) (name) (typeMap))

  // fetchTypeClass :: String -> Either String TypeClass
  const fetchTypeClass = name =>
    maybeToEither (ErrorMsg.unrecognizedTypeClass (name))
                  (U.get (isTypeClass) (name) (typeClassMap))

  // wardParsers :: StrMap QuasiParser -> StrMap QuasiParser
  const wardParsers = pipe ([U.pairs, map (P.ward), U.fromPairs])

  /* Main */

  // type :: QuasiParser
  const type =
    P.match ([
      "uncurriedFunction",
      "function",
      "list",
      "record",
      "constrainedType",
      "typeConstructor",
      "typeVariable"
    ])

  // _parsers :: StrMap QuasiParser
  const _parsers = {
    typeVariable: $ =>
      lift2 (C (ap))
            (pipe ([Rose.root, nonEmptyText, map (TypeVariable)]))
            (B (empty) (Rose.forest)),

    list: $ =>
      pipe ([Rose.forest, single, chain ($.type), map (ArrayType)]),

    record: $ =>
      pipe ([
        Rose.forest,
        nonEmpty,
        chain (traverse (Either) ($.recordField)), 
        map (B (RecordType) (U.fromPairs))
      ]),

    recordField: $ =>
      lift2 (ap)
            (pipe ([Rose.root, nonEmptyText, map (Pair)]))
            (pipe ([Rose.forest, single, chain ($.type)])),

    uncurriedFunctionParams: $ =>
      pipe ([Rose.forest, many, chain (traverse (Either) ($.type))]),

    uncurriedFunction: $ =>
      pipe ([
        Rose.forest,
        Combinators.exactly (2),
        chain (lift2 (C (ap))
                     (B ($.uncurriedFunctionParams) (Rose.root))
                     (pipe ([Rose.forest, $.type, map (append)]))),
        map (FunctionType)
      ]),

    thunk: $ =>
      lift2 (C (ap))
            (_ => Right (UnitSym))
            (B (empty) (Rose.forest)),

    function: $ => {

      // functionArg :: Parser
      const functionArg = lift2 (alt) ($.thunk) ($.type)

      // f :: Rose Type -> Type
      const f = ([a, ...b]) => {
        // _f :: [Rose Type] -> Type
        const _f  = 
          ifElse (sizeCmp (gte (2))) (f) (unsafeHead)

        const _g  = 
          a === UnitSym ? C (K) : prepend

        return FunctionType (_g (a) ([_f (b)]))
      }

      return pipe ([
        Rose.forest,
        many,
        chain (traverse (Either) (functionArg)),
        map (f)
      ])
    },

    typeConstructor: $ => {

      // typeConstructorArg :: Parser
      const typeConstructorArg =
        P.match ([
          "typeConstructor",
          "typeVariable",
          "function",
          "record",
          "list"
        ]) ($)

      return lift2 (ap) 
                   (pipe ([Rose.root, nonEmptyText, chain (fetchType), map (polyVariadicMagic)]))
                   (pipe ([Rose.forest, 
                           Combinators.atMost (2), 
                           chain (traverse (Either) (typeConstructorArg))]))
    },

    constrainedType: $ => {

      // constrainedTypeArg :: Parser
      const constrainedTypeArg = 
        P.match ([
          "list",
          "record",
          "typeConstructor",
          "typeVariable",
          "function",
          "constrainedType"
        ]) ($)

      // f :: String -> [Type] -> Type
      const f = name => xs => {
        const g = size (xs) === 1 ? UnaryTypeVariable : BinaryTypeVariable

        return polyVariadicMagic (g (name)) (xs)
      }

      return lift2 (ap)
                   (pipe ([Rose.root, nonEmptyText, map (f)]))
                   (pipe ([Rose.forest, 
                           Combinators.atMost (2), 
                           chain (traverse (Either) (constrainedTypeArg))]))
    },

    constraint: $ => {

      // constraintArg :: Parser
      const constraintArg =
        P.match ([
          "typeVariable",
          "constrainedType"
        ]) ($)

      return lift2 (ap)
                   (pipe ([Rose.root, nonEmptyText, chain (fetchTypeClass), map (C (Pair))]))
                   (pipe ([Rose.forest, single, chain (constraintArg)]))      
    }
  }

  /*
   * Exports
   */
  // :: StrMap Parser
  return P.createLanguage ({
    type,
    ...wardParsers (_parsers)
  })
}