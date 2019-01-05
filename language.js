import {parse as parseTypeId} from "sanctuary-type-identifiers"

export default ({S, $ : SDef, Z, typeClasses, typeConstructors}) => {

  /* Dependencies */

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
    isJust,
    isLeft,
    tagBy,
    mapLeft,
    maybeToEither

  } = S.unchecked

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
  const polyVariadicMagic = (S.unchecked).reduce (I)

  // unsafeHead :: [a] -> a
  const unsafeHead = xs => xs[0]

  // _errorMsg :: a -> b -> String
  const _errorMsg = expected => actual =>
    `Expected ${show (expected)};\nActual: ${show (actual)}`

  // indexBy :: (a -> String) -> b -> StrMap a
  const indexBy = f =>
    reduce (xs => x => S.insert (f (x)) (x) (xs))
           ({})

  // text :: a -> b
  const text   = S.prop ("text")

  // name :: a -> b
  const name   = S.prop ("name")

  // length :: a -> b
  const length = S.prop ("length") 

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

      const g = B (mapLeft (_errorMsg (err)))
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

  // notEmpty :: [a] -> Either String [a]
  const notEmpty = Combinators.atLeast (1)

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
      // root     :: TypeCarton
      const root     = Rose.root (s)
      // rootType :: String
      const rootType = S.prop ("type") (root) 

      return elem (rootType) (expected) ?
        $[rootType] (s) : Left (_errorMsg (expected) (rootType))
    }

    const ward = p => {
      // parserId :: String
      const parserId = S.fst (p)

      return Pair (parserId) ($ => s => {
        // reqParser :: String
        const reqParser = S.prop ("type") (Rose.root (s))

        return reqParser === parserId ?
          S.snd (p) ($) (s) : Left (_errorMsg (parserId) (reqParser))        
      })
    }

    // scope binder; utility
    const createLanguage = parsers => {
      const language = {}

      for (let x of S.pairs (parsers)) {
        const parserId = S.fst (x)

        language[parserId] = s => mapLeft (err => `<${parserId}> ${err}`) 
                                          (S.snd (x) (language) (s))
      }

      return language
    }

    return {match, ward, createLanguage}
  }) ()

  // typeName :: Type -> String
  const typeName = B (name) (B (parseTypeId) (name))

  // isType :: a -> Boolean
  const isType =
    S.anyPass ([
      // Type ?
      S.is (Type),
      // Type -> Type ?
      S.is (FunctionType ([Type, Type])),
      // Type -> Type -> Type ?
      S.is (FunctionType ([Type, FunctionType ([Type, Type])]))
    ])

  // isTypeClass :: a -> Boolean
  const isTypeClass = S.is (TypeClass)

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
                         "Any"      : SDef.Any       })

    // tm1 :: Either String (StrMap Type')
    const tm1 = B (mapLeft (B (x => `Unrecognized \`Type\` ${x}`) (S.fst)))
                  (map (S.fromPairs))
                       (traverse (Either)
                                 (tagBy (lift2 (and)
                                               (B (isType) (S.snd))
                                               (B (isJust)
                                                  (B (C (S.get (_ => true)) (tm0))
                                                     (S.fst)))))
                                 (S.pairs (typeConstructors)))

    // :: Either String (StrMap Type')
    return lift2 (concat) (Right (tm0))
                          (tm1)
  }) ()

  // typeMap :: StrMap Type'
  const typeMap = S.either (err => {throw err}) (I) (_typeMap)

  // _typeClassMap :: StrMap TypeClass
  const _typeClassMap = (() => {

    // tcm0 :: StrMap TypeClass
    const tcm0 = 
      S.justs (map (C (S.get (isTypeClass)) (Z))
                   (S.fromPairs (join (zip) (typeClassesBaseline))))
    
    // tcm1 :: Either a (StrMap TypeClass)
    const tcm1 = 
      B (mapLeft (x => `Unrecognized \`TypeClass\` ${show (x)}`))
        (map (S.fromPairs))
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
  const typeClassMap = S.either (err => {throw err}) (I) (_typeClassMap)

  // fetchType :: String -> Either String Type
  const fetchType = name => 
    maybeToEither (`Unrecognized \`Type\` ${name}`) 
                  (S.get (isType) (name) (typeMap))

  // fetchTypeClass :: String -> Either String TypeClass
  const fetchTypeClass = name =>
    maybeToEither (`Unrecognized \`TypeClass\` ${name}`)
                  (S.get (isTypeClass) (name) (typeClassMap))

  // wardParsers :: StrMap QuasiParser -> StrMap QuasiParser
  const wardParsers = pipe ([S.pairs, map (P.ward), S.fromPairs])

  /* Main */

  // type :: QuasiParser
  const type =
    ap (C (P.match)) (B (map (S.fst)) (S.pairs))

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
        notEmpty,
        chain (traverse (Either) ($.recordField)), 
        map (B (RecordType) (S.fromPairs))
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
        chain (traverse (Either) ($.type)),
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
          "record"
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