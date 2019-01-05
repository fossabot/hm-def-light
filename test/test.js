import Tape           from "tape"

import {create, env}  from "sanctuary"
import SDef           from "sanctuary-def"
import Z              from "sanctuary-type-classes"

import typeId         from "sanctuary-type-identifiers"

import $create        from "../index"

import {TypeRep, Radix} from "./internal/types"


// checkTypes :: Boolean
const checkTypes = true

const S = create ({
  checkTypes,
  env : env.concat ([TypeRep (SDef.Unknown), Radix])
})

const mock = (SDef/* is mutated! */) => {

  // mockSym :: Symbol
  const mockSym = Symbol ("mock!")

  const mockSDef$create = SDef$create => ({checkTypes, env}) => {

    const def = SDef$create ({checkTypes, env})

    return name => constraints => types => impl =>
      impl === mockSym ?
        ({name, constraints, types}) :
        def (name)
            (constraints)
            (types)
            (impl)
  }

  // mock; mutate
  SDef.create = mockSDef$create (SDef.create)

  const defProxy = $create ({
    create       : S.create,
    env          : S.env,
    $            : SDef,
    Z,
    checkTypes,
    typeConstructors : {TypeRep}
  })

  const defMocked = typeDeclaration => 
    defProxy (typeDeclaration) 
             (mockSym)

  return {SDef, defProxy, defMocked}

}

const test = defMocked       => 
             t               => 
             typeDeclaration => 
             expected        => {

  t.comment (typeDeclaration)

  // some pieces are adapted from ../language

  // adaptFnRepr :: [Type] -> Type
  const adaptFnRepr = ([a, ...b]) => {   
    // _f :: [Type] -> Type
    const _f  = x =>
      S.size (x) >= 2 ? adaptFnRepr (x) : x[0]

    return SDef.Function (S.prepend (a) ([_f (b)]))
  }

  // nameProp :: a -> b
  const nameProp = S.prop ("name")

  // typeName :: Type -> String
  const typeName = x => nameProp (typeId.parse (nameProp (x)))

  // adaptConstraints :: StrMap [TypeClass] -> StrMap [String]
  const adaptConstraints = S.map (S.map (typeName))

  // :: {
  //   name :: String,
  //   constraints :: StrMap TypeClass,
  //   types :: [Type]
  // }
  const actual = defMocked (typeDeclaration)

  // actualFnType   :: Type
  const actualFnType   = SDef.Function (actual.types)
  // expectedFnType :: Type
  const expectedFnType = adaptFnRepr   (expected.types)

  /* assertions */

  const ok = msg => b => t.ok (b, msg)

  ok ("name")
     (actual.name === expected.name)

  ok ("constraint")
     (S.on (S.equals) (adaptConstraints) 
           (actual.constraints) (expected.constraints))

  ok ("function types")
     (S.equals (actualFnType) (expectedFnType))
}

Tape.test ("sanctuary", _t => {

  /* Bootstrap */

  const end    = _t.end
  const fail   = _t.fail

  const {defMocked} = S.either (end)
                               (S.I) 
                               (S.encaseEither (S.I) (mock) (SDef))

  const test_  = x => y =>
    S.either (fail)
             (S.I)
             (S.encaseEither2 (S.I) 
                              (test (defMocked) (_t))
                              (x) (y))

  const $ = SDef

  //  :: Type
  const a = $.TypeVariable ("a")
  const b = $.TypeVariable ("b")
  const c = $.TypeVariable ("c")
  const d = $.TypeVariable ("d")
  const e = $.TypeVariable ("e")
  const g = $.TypeVariable ("g")
  const l = $.TypeVariable ("l")
  const r = $.TypeVariable ("r")

  //  :: Type -> Type
  const f = $.UnaryTypeVariable ("f")
  const m = $.UnaryTypeVariable ("m")
  const t = $.UnaryTypeVariable ("t")
  const w = $.UnaryTypeVariable ("w")

  //  :: Type -> Type -> Type
  const p = $.BinaryTypeVariable ("p")
  const s = $.BinaryTypeVariable ("s")

  // $Either :: Type -> Type -> Type
  const $Either = S.EitherType

  // $Maybe :: Type -> Type
  const $Maybe  = S.MaybeType

  // $Pair :: Type -> Type -> Type
  const $Pair   = S.PairType

  // Fn :: Type -> Type -> Type
  const Fn     = a => b => $.Function ([a, b])

  //  Match :: Type
  const Match = $.RecordType ({
    match: $.String,
    groups: $.Array ($Maybe ($.String))
  })

  /* Tests */

  test_ ("create :: { checkTypes :: Boolean, env :: Array Type } -> Object")
        ({name: "create", 
          constraints: {},
          types: [$.RecordType ({checkTypes: $.Boolean, env: $.Array ($.Type)}), $.Object]})

  test_ ("type :: Any -> { namespace :: Maybe String, name :: String, version :: NonNegativeInteger }")
        ({name: "type", 
          constraints: {},
          types: [$.Any,
            $.RecordType ({namespace: $Maybe ($.String),
                           name: $.String,
                           version: $.NonNegativeInteger})]})

  test_ ("show :: Any -> String")
        ({name: "show", 
          constraints: {},
          types: [$.Any, $.String]})

  test_ ("equals :: Setoid a => a -> a -> Boolean")
        ({name: "equals", 
          constraints: {a: [Z.Setoid]},
          types: [a, a, $.Boolean]})

  test_ ("lt :: Ord a => a -> a -> Boolean")
        ({name: "lt", 
          constraints: {a: [Z.Ord]},
          types: [a, a, $.Boolean]})

  test_ ("lte :: Ord a => a -> a -> Boolean")
        ({name: "lte", 
          constraints: {a: [Z.Ord]},
          types: [a, a, $.Boolean]})

  test_ ("gt :: Ord a => a -> a -> Boolean")
        ({name: "gt", 
          constraints: {a: [Z.Ord]},
          types: [a, a, $.Boolean]})

  test_ ("gte :: Ord a => a -> a -> Boolean")
        ({name: "gte", 
          constraints: {a: [Z.Ord]},
          types: [a, a, $.Boolean]})

  test_ ("min :: Ord a => a -> a -> a")
        ({name: "min", 
          constraints: {a: [Z.Ord]},
          types: [a, a, a]})

  test_ ("max :: Ord a => a -> a -> a")
        ({name: "max", 
          constraints: {a: [Z.Ord]},
          types: [a, a, a]})

  test_ ("id :: Category c => TypeRep c -> c")
        ({name: "id", 
          constraints: {c: [Z.Category]},
          types: [TypeRep (c), c]})

  test_ ("concat :: Semigroup a => a -> a -> a")
        ({name: "concat", 
          constraints: {a: [Z.Semigroup]},
          types: [a, a, a]})

  test_ ("empty :: Monoid a => TypeRep a -> a")
        ({name: "empty", 
          constraints: {a: [Z.Monoid]},
          types: [TypeRep (a), a]})

  test_ ("invert :: Group g => g -> g")
        ({name: "invert", 
          constraints: {g: [Z.Group]},
          types: [g, g]})

  test_ ("filter :: Filterable f => (a -> Boolean) -> f a -> f a")
        ({name: "filter", 
          constraints: {f: [Z.Filterable]},
          types: [$.Predicate (a), f (a), f (a)]})

  test_ ("reject :: Filterable f => (a -> Boolean) -> f a -> f a")
        ({name: "reject", 
          constraints: {f: [Z.Filterable]},
          types: [$.Predicate (a), f (a), f (a)]})

  test_ ("takeWhile :: Filterable f => (a -> Boolean) -> f a -> f a")
        ({name: "takeWhile", 
          constraints: {f: [Z.Filterable]},
          types: [$.Predicate (a), f (a), f (a)]})

  test_ ("dropWhile :: Filterable f => (a -> Boolean) -> f a -> f a")
        ({name: "dropWhile", 
          constraints: {f: [Z.Filterable]},
          types: [$.Predicate (a), f (a), f (a)]})

  test_ ("map :: Functor f => (a -> b) -> f a -> f b")
        ({name: "map", 
          constraints: {f: [Z.Functor]},
          types: [Fn (a) (b), f (a), f (b)]})

  test_ ("flip :: Functor f => f (a -> b) -> a -> f b")
        ({name: "flip", 
          constraints: {f: [Z.Functor]},
          types: [f (Fn (a) (b)), a, f (b)]})

  test_ ("bimap :: Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d")
        ({name: "bimap", 
          constraints: {p: [Z.Bifunctor]},
          types: [Fn (a) (b), Fn (c) (d), p (a) (c), p (b) (d)]})

  test_ ("mapLeft :: Bifunctor p => (a -> b) -> p a c -> p b c")
        ({name: "mapLeft", 
          constraints: {p: [Z.Bifunctor]},
          types: [Fn (a) (b), p (a) (c), p (b) (c)]})

  test_ ("promap :: Profunctor p => (a -> b) -> (c -> d) -> p b c -> p a d")
        ({name: "promap", 
          constraints: {p: [Z.Profunctor]},
          types: [Fn (a) (b), Fn (c) (d), p (b) (c), p (a) (d)]})

  test_ ("alt :: Alt f => f a -> f a -> f a")
        ({name: "alt", 
          constraints: {f: [Z.Alt]},
          types: [f (a), f (a), f (a)]})

  test_ ("zero :: Plus f => TypeRep f -> f a")
        ({name: "zero", 
          constraints: {f: [Z.Plus]},
          types: [TypeRep ($.TypeVariable ("f")), f (a)]})

  test_ ("reduce :: Foldable f => (b -> a -> b) -> b -> f a -> b")
        ({name: "reduce", 
          constraints: {f: [Z.Foldable]},
          types: [Fn (b) (Fn (a) (b)), b, f (a), b]})

  test_ ("traverse :: (Applicative f, Traversable t) => TypeRep f -> (a -> f b) -> t a -> f (t b)")
        ({name: "traverse", 
          constraints: {f: [Z.Applicative], t: [Z.Traversable]},
          types: [TypeRep ($.TypeVariable ("f")), Fn (a) (f (b)), t (a), f (t (b))]})

  test_ ("sequence :: (Applicative f, Traversable t) => TypeRep f -> t (f a) -> f (t a)")
        ({name: "sequence", 
          constraints: {f: [Z.Applicative], t: [Z.Traversable]},
          types: [TypeRep ($.TypeVariable ("f")), t (f (a)), f (t (a))]})

  test_ ("ap :: Apply f => f (a -> b) -> f a -> f b")
        ({name: "ap", 
          constraints: {f: [Z.Apply]},
          types: [f (Fn (a) (b)), f (a), f (b)]})

  test_ ("lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c")
        ({name: "lift2", 
          constraints: {f: [Z.Apply]},
          types: [Fn (a) (Fn (b) (c)), f (a), f (b), f (c)]})

  test_ ("lift3 :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d")
        ({name: "lift3", 
          constraints: {f: [Z.Apply]},
          types: [Fn (a) (Fn (b) (Fn (c) (d))), f (a), f (b), f (c), f (d)]})

  test_ ("apFirst :: Apply f => f a -> f b -> f a")
        ({name: "apFirst", 
          constraints: {f: [Z.Apply]},
          types: [f (a), f (b), f (a)]})

  test_ ("apSecond :: Apply f => f a -> f b -> f b")
        ({name: "apSecond", 
          constraints: {f: [Z.Apply]},
          types: [f (a), f (b), f (b)]})

  test_ ("of :: Applicative f => TypeRep f -> a -> f a")
        ({name: "of", 
          constraints: {f: [Z.Applicative]},
          types: [TypeRep ($.TypeVariable ("f")), a, f (a)]})

  test_ ("chain :: Chain m => (a -> m b) -> m a -> m b")
        ({name: "chain", 
          constraints: {m: [Z.Chain]},
          types: [Fn (a) (m (b)), m (a), m (b)]})

  test_ ("join :: Chain m => m (m a) -> m a")
        ({name: "join", 
          constraints: {m: [Z.Chain]},
          types: [m (m (a)), m (a)]})

  test_ ("chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b")
        ({name: "chainRec", 
          constraints: {m: [Z.ChainRec]},
          types: [TypeRep ($.TypeVariable ("m")),
            Fn (a) (m ($Either (a) (b))),
            a,
            m (b)]})

  test_ ("extend :: Extend w => (w a -> b) -> w a -> w b")
        ({name: "extend", 
          constraints: {w: [Z.Extend]},
          types: [Fn (w (a)) (b), w (a), w (b)]})

  test_ ("duplicate :: Extend w => w a -> w (w a)")
        ({name: "duplicate", 
          constraints: {w: [Z.Extend]},
          types: [w (a), w (w (a))]})

  test_ ("extract :: Comonad w => w a -> a")
        ({name: "extract", 
          constraints: {w: [Z.Comonad]},
          types: [w (a), a]})

  test_ ("contramap :: Contravariant f => (b -> a) -> f a -> f b")
        ({name: "contramap", 
          constraints: {f: [Z.Contravariant]},
          types: [Fn (b) (a), f (a), f (b)]})

  test_ ("I :: a -> a")
        ({name: "I", 
          constraints: {},
          types: [a, a]})

  test_ ("K :: a -> b -> a")
        ({name: "K", 
          constraints: {},
          types: [a, b, a]})

  test_ ("T :: a -> (a -> b) -> b")
        ({name: "T", 
          constraints: {},
          types: [a, Fn (a) (b), b]})

  test_ ("curry2 :: ((a, b) -> c) -> a -> b -> c")
        ({name: "curry2", 
          constraints: {},
          types: [$.Function ([a, b, c]), a, b, c]})

  test_ ("curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d")
        ({name: "curry3", 
          constraints: {},
          types: [$.Function ([a, b, c, d]), a, b, c, d]})

  test_ ("curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e")
        ({name: "curry4", 
          constraints: {},
          types: [$.Function ([a, b, c, d, e]), a, b, c, d, e]})

  test_ ("curry5 :: ((a, b, c, d, e) -> r) -> a -> b -> c -> d -> e -> r")
        ({name: "curry5", 
          constraints: {},
          types: [$.Function ([a, b, c, d, e, r]), a, b, c, d, e, r]})

  test_ ("compose :: Semigroupoid s => s b c -> s a b -> s a c")
        ({name: "compose", 
          constraints: {s: [Z.Semigroupoid]},
          types: [s (b) (c), s (a) (b), s (a) (c)]})

  test_ ("pipe :: Foldable f => f (Any -> Any) -> a -> b")
        ({name: "pipe", 
          constraints: {f: [Z.Foldable]},
          types: [f (Fn ($.Any) ($.Any)), a, b]})

  test_ ("pipeK :: (Foldable f, Chain m) => f (Any -> m Any) -> m a -> m b")
        ({name: "pipeK", 
          constraints: {f: [Z.Foldable], m: [Z.Chain]},
          types: [f (Fn ($.Any) (m ($.Any))), m (a), m (b)]})

  test_ ("on :: (b -> b -> c) -> (a -> b) -> a -> a -> c")
        ({name: "on", 
          constraints: {},
          types: [Fn (b) (Fn (b) (c)), Fn (a) (b), a, a, c]})

  test_ ("Pair :: a -> b -> Pair a b")
        ({name: "Pair", 
          constraints: {},
          types: [a, b, $Pair (a) (b)]})

  test_ ("fst :: Pair a b -> a")
        ({name: "fst", 
          constraints: {},
          types: [$Pair (a) (b), a]})

  test_ ("snd :: Pair a b -> b")
        ({name: "snd", 
          constraints: {},
          types: [$Pair (a) (b), b]})

  test_ ("swap :: Pair a b -> Pair b a")
        ({name: "swap", 
          constraints: {},
          types: [$Pair (a) (b), $Pair (b) (a)]})

  test_ ("Just :: a -> Maybe a")
        ({name: "Just", 
          constraints: {},
          types: [a, $Maybe (a)]})

  test_ ("isNothing :: Maybe a -> Boolean")
        ({name: "isNothing", 
          constraints: {},
          types: [$Maybe (a), $.Boolean]})

  test_ ("isJust :: Maybe a -> Boolean")
        ({name: "isJust", 
          constraints: {},
          types: [$Maybe (a), $.Boolean]})

  test_ ("fromMaybe :: a -> Maybe a -> a")
        ({name: "fromMaybe", 
          constraints: {},
          types: [a, $Maybe (a), a]})

  test_ ("fromMaybe_ :: (() -> a) -> Maybe a -> a")
        ({name: "fromMaybe_", 
          constraints: {},
          types: [$.Thunk (a), $Maybe (a), a]})

  test_ ("maybeToNullable :: Maybe a -> Nullable a")
        ({name: "maybeToNullable", 
          constraints: {},
          types: [$Maybe (a), $.Nullable (a)]})

  test_ ("toMaybe :: a -> Maybe a")
        ({name: "toMaybe", 
          constraints: {},
          types: [a, $Maybe (a)]})

  test_ ("maybe :: b -> (a -> b) -> Maybe a -> b")
        ({name: "maybe", 
          constraints: {},
          types: [b, Fn (a) (b), $Maybe (a), b]})

  test_ ("maybe_ :: (() -> b) -> (a -> b) -> Maybe a -> b")
        ({name: "maybe_", 
          constraints: {},
          types: [$.Thunk (b), Fn (a) (b), $Maybe (a), b]})

  test_ ("justs :: (Filterable f, Functor f) => f (Maybe a) -> f a")
        ({name: "justs", 
          constraints: {f: [Z.Filterable, Z.Functor]},
          types: [f ($Maybe (a)), f (a)]})

  test_ ("mapMaybe :: (Filterable f, Functor f) => (a -> Maybe b) -> f a -> f b")
        ({name: "mapMaybe", 
          constraints: {f: [Z.Filterable, Z.Functor]},
          types: [Fn (a) ($Maybe (b)), f (a), f (b)]})

  test_ ("encase :: (a -> b) -> a -> Maybe b")
        ({name: "encase", 
          constraints: {},
          types: [Fn (a) (b), a, $Maybe (b)]})

  test_ ("encase2 :: (a -> b -> c) -> a -> b -> Maybe c")
        ({name: "encase2", 
          constraints: {},
          types: [Fn (a) (Fn (b) (c)), a, b, $Maybe (c)]})

  test_ ("encase3 :: (a -> b -> c -> d) -> a -> b -> c -> Maybe d")
        ({name: "encase3", 
          constraints: {},
          types: [Fn (a) (Fn (b) (Fn (c) (d))), a, b, c, $Maybe (d)]})

  test_ ("maybeToEither :: a -> Maybe b -> Either a b")
        ({name: "maybeToEither", 
          constraints: {},
          types: [a, $Maybe (b), $Either (a) (b)]})

  test_ ("Left :: a -> Either a b")
        ({name: "Left", 
          constraints: {},
          types: [a, $Either (a) (b)]})

  test_ ("Right :: b -> Either a b")
        ({name: "Right", 
          constraints: {},
          types: [b, $Either (a) (b)]})

  test_ ("isLeft :: Either a b -> Boolean")
        ({name: "isLeft", 
          constraints: {},
          types: [$Either (a) (b), $.Boolean]})

  test_ ("isRight :: Either a b -> Boolean")
        ({name: "isRight", 
          constraints: {},
          types: [$Either (a) (b), $.Boolean]})

  test_ ("fromEither :: b -> Either a b -> b")
        ({name: "fromEither", 
          constraints: {},
          types: [b, $Either (a) (b), b]})

  test_ ("toEither :: a -> b -> Either a b")
        ({name: "toEither", 
          constraints: {},
          types: [a, b, $Either (a) (b)]})

  test_ ("either :: (a -> c) -> (b -> c) -> Either a b -> c")
        ({name: "either", 
          constraints: {},
          types: [Fn (a) (c), Fn (b) (c), $Either (a) (b), c]})

  test_ ("lefts :: (Filterable f, Functor f) => f (Either a b) -> f a")
        ({name: "lefts", 
          constraints: {f: [Z.Filterable, Z.Functor]},
          types: [f ($Either (a) (b)), f (a)]})

  test_ ("rights :: (Filterable f, Functor f) => f (Either a b) -> f b")
        ({name: "rights", 
          constraints: {f: [Z.Filterable, Z.Functor]},
          types: [f ($Either (a) (b)), f (b)]})

  test_ ("tagBy :: (a -> Boolean) -> a -> Either a a")
        ({name: "tagBy", 
          constraints: {},
          types: [$.Predicate (a), a, $Either (a) (a)]})

  test_ ("encaseEither :: (Error -> l) -> (a -> r) -> a -> Either l r")
        ({name: "encaseEither", 
          constraints: {},
          types: [Fn ($.Error) (l), Fn (a) (r), a, $Either (l) (r)]})

  test_ ("encaseEither2 :: (Error -> l) -> (a -> b -> r) -> a -> b -> Either l r")
        ({name: "encaseEither2", 
          constraints: {},
          types: [Fn ($.Error) (l), Fn (a) (Fn (b) (r)), a, b, $Either (l) (r)]})

  test_ ("encaseEither3 :: (Error -> l) -> (a -> b -> c -> r) -> a -> b -> c -> Either l r")
        ({name: "encaseEither3", 
          constraints: {},
          types: [Fn ($.Error) (l),
            Fn (a) (Fn (b) (Fn (c) (r))),
            a,
            b,
            c,
            $Either (l) (r)]})

  test_ ("eitherToMaybe :: Either a b -> Maybe b")
        ({name: "eitherToMaybe", 
          constraints: {},
          types: [$Either (a) (b), $Maybe (b)]})

  test_ ("and :: Boolean -> Boolean -> Boolean")
        ({name: "and", 
          constraints: {},
          types: [$.Boolean, $.Boolean, $.Boolean]})

  test_ ("or :: Boolean -> Boolean -> Boolean")
        ({name: "or", 
          constraints: {},
          types: [$.Boolean, $.Boolean, $.Boolean]})

  test_ ("not :: Boolean -> Boolean")
        ({name: "not", 
          constraints: {},
          types: [$.Boolean, $.Boolean]})

  test_ ("complement :: (a -> Boolean) -> a -> Boolean")
        ({name: "complement", 
          constraints: {},
          types: [$.Predicate (a), a, $.Boolean]})

  test_ ("ifElse :: (a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b")
        ({name: "ifElse", 
          constraints: {},
          types: [$.Predicate (a), Fn (a) (b), Fn (a) (b), a, b]})

  test_ ("when :: (a -> Boolean) -> (a -> a) -> a -> a")
        ({name: "when", 
          constraints: {},
          types: [$.Predicate (a), Fn (a) (a), a, a]})

  test_ ("unless :: (a -> Boolean) -> (a -> a) -> a -> a")
        ({name: "unless", 
          constraints: {},
          types: [$.Predicate (a), Fn (a) (a), a, a]})

  test_ ("allPass :: Foldable f => f (a -> Boolean) -> a -> Boolean")
        ({name: "allPass", 
          constraints: {f: [Z.Foldable]},
          types: [f ($.Predicate (a)), a, $.Boolean]})

  test_ ("anyPass :: Foldable f => f (a -> Boolean) -> a -> Boolean")
        ({name: "anyPass", 
          constraints: {f: [Z.Foldable]},
          types: [f ($.Predicate (a)), a, $.Boolean]})

  test_ ("slice :: Integer -> Integer -> Array a -> Maybe (Array a)")
        ({name: "slice", 
          constraints: {},
          types: [$.Integer, $.Integer, $.Array (a), $Maybe ($.Array (a))]})

  test_ ("at :: Integer -> Array a -> Maybe a")
        ({name: "at", 
          constraints: {},
          types: [$.Integer, $.Array (a), $Maybe (a)]})

  test_ ("head :: Array a -> Maybe a")
        ({name: "head", 
          constraints: {},
          types: [$.Array (a), $Maybe (a)]})

  test_ ("last :: Array a -> Maybe a")
        ({name: "last", 
          constraints: {},
          types: [$.Array (a), $Maybe (a)]})

  test_ ("tail :: Array a -> Maybe (Array a)")
        ({name: "tail", 
          constraints: {},
          types: [$.Array (a), $Maybe ($.Array (a))]})

  test_ ("init :: Array a -> Maybe (Array a)")
        ({name: "init", 
          constraints: {},
          types: [$.Array (a), $Maybe ($.Array (a))]})

  test_ ("take :: Integer -> Array a -> Maybe (Array a)")
        ({name: "take", 
          constraints: {},
          types: [$.Integer, $.Array (a), $Maybe ($.Array (a))]})

  test_ ("takeLast :: Integer -> Array a -> Maybe (Array a)")
        ({name: "takeLast", 
          constraints: {},
          types: [$.Integer, $.Array (a), $Maybe ($.Array (a))]})

  test_ ("drop :: Integer -> Array a -> Maybe (Array a)")
        ({name: "drop", 
          constraints: {},
          types: [$.Integer, $.Array (a), $Maybe ($.Array (a))]})

  test_ ("dropLast :: Integer -> Array a -> Maybe (Array a)")
        ({name: "dropLast", 
          constraints: {},
          types: [$.Integer, $.Array (a), $Maybe ($.Array (a))]})

  test_ ("size :: Foldable f => f a -> Integer")
        ({name: "size", 
          constraints: {f: [Z.Foldable]},
          types: [f (a), $.Integer]})

  test_ ("append :: (Applicative f, Semigroup (f a)) => a -> f a -> f a")
        ({name: "append", 
          constraints: {f: [Z.Applicative, Z.Semigroup]},
          types: [a, f (a), f (a)]})

  test_ ("prepend :: (Applicative f, Semigroup (f a)) => a -> f a -> f a")
        ({name: "prepend", 
          constraints: {f: [Z.Applicative, Z.Semigroup]},
          types: [a, f (a), f (a)]})

  test_ ("joinWith :: String -> Array String -> String")
        ({name: "joinWith", 
          constraints: {},
          types: [$.String, $.Array ($.String), $.String]})

  test_ ("elem :: (Setoid a, Foldable f) => a -> f a -> Boolean")
        ({name: "elem", 
          constraints: {a: [Z.Setoid], f: [Z.Foldable]},
          types: [a, f (a), $.Boolean]})

  test_ ("find :: Foldable f => (a -> Boolean) -> f a -> Maybe a")
        ({name: "find", 
          constraints: {f: [Z.Foldable]},
          types: [$.Predicate (a), f (a), $Maybe (a)]})

  test_ ("foldMap :: (Monoid b, Foldable f) => TypeRep b -> (a -> b) -> f a -> b")
        ({name: "foldMap", 
          constraints: {b: [Z.Monoid], f: [Z.Foldable]},
          types: [TypeRep (b), Fn (a) (b), f (a), b]})

  test_ ("unfoldr :: (b -> Maybe (Pair a b)) -> b -> Array a")
        ({name: "unfoldr", 
          constraints: {},
          types: [Fn (b) ($Maybe ($Pair (a) (b))), b, $.Array (a)]})

  test_ ("range :: Integer -> Integer -> Array Integer")
        ({name: "range", 
          constraints: {},
          types: [$.Integer, $.Integer, $.Array ($.Integer)]})

  test_ ("groupBy :: (a -> a -> Boolean) -> Array a -> Array (Array a)")
        ({name: "groupBy", 
          constraints: {},
          types: [Fn (a) ($.Predicate (a)), $.Array (a), $.Array ($.Array (a))]})

  test_ ("reverse :: (Applicative f, Foldable f, Monoid (f a)) => f a -> f a")
        ({name: "reverse", 
          constraints: {f: [Z.Applicative, Z.Foldable, Z.Monoid]},
          types: [f (a), f (a)]})

  test_ ("sort :: (Ord a, Applicative m, Foldable m, Monoid (m a)) => m a -> m a")
        ({name: "sort", 
          constraints: {a: [Z.Ord], m: [Z.Applicative, Z.Foldable, Z.Monoid]},
          types: [m (a), m (a)]})

  test_ ("sortBy :: (Ord b, Applicative m, Foldable m, Monoid (m a)) => (a -> b) -> m a -> m a")
        ({name: "sortBy", 
          constraints: {b: [Z.Ord], m: [Z.Applicative, Z.Foldable, Z.Monoid]},
          types: [Fn (a) (b), m (a), m (a)]})

  test_ ("zip :: Array a -> Array b -> Array (Pair a b)")
        ({name: "zip", 
          constraints: {},
          types: [$.Array (a), $.Array (b), $.Array ($Pair (a) (b))]})

  test_ ("zipWith :: (a -> b -> c) -> Array a -> Array b -> Array c")
        ({name: "zipWith", 
          constraints: {},
          types: [Fn (a) (Fn (b) (c)), $.Array (a), $.Array (b), $.Array (c)]})

  test_ ("prop :: String -> a -> b")
        ({name: "prop", 
          constraints: {},
          types: [$.String, a, b]})

  test_ ("props :: Array String -> a -> b")
        ({name: "props", 
          constraints: {},
          types: [$.Array ($.String), a, b]})

  test_ ("get :: (Any -> Boolean) -> String -> a -> Maybe b")
        ({name: "get", 
          constraints: {},
          types: [$.Predicate ($.Any), $.String, a, $Maybe (b)]})

  test_ ("gets :: (Any -> Boolean) -> Array String -> a -> Maybe b")
        ({name: "gets", 
          constraints: {},
          types: [$.Predicate ($.Any), $.Array ($.String), a, $Maybe (b)]})

  test_ ("singleton :: String -> a -> StrMap a")
        ({name: "singleton", 
          constraints: {},
          types: [$.String, a, $.StrMap (a)]})

  test_ ("insert :: String -> a -> StrMap a -> StrMap a")
        ({name: "insert", 
          constraints: {},
          types: [$.String, a, $.StrMap (a), $.StrMap (a)]})

  test_ ("remove :: String -> StrMap a -> StrMap a")
        ({name: "remove", 
          constraints: {},
          types: [$.String, $.StrMap (a), $.StrMap (a)]})

  test_ ("keys :: StrMap a -> Array String")
        ({name: "keys", 
          constraints: {},
          types: [$.StrMap (a), $.Array ($.String)]})

  test_ ("values :: StrMap a -> Array a")
        ({name: "values", 
          constraints: {},
          types: [$.StrMap (a), $.Array (a)]})

  test_ ("pairs :: StrMap a -> Array (Pair String a)")
        ({name: "pairs", 
          constraints: {},
          types: [$.StrMap (a), $.Array ($Pair ($.String) (a))]})

  test_ ("fromPairs :: Foldable f => f (Pair String a) -> StrMap a")
        ({name: "fromPairs", 
          constraints: {f: [Z.Foldable]},
          types: [f ($Pair ($.String) (a)), $.StrMap (a)]})

  test_ ("negate :: ValidNumber -> ValidNumber")
        ({name: "negate", 
          constraints: {},
          types: [$.ValidNumber, $.ValidNumber]})

  test_ ("add :: FiniteNumber -> FiniteNumber -> FiniteNumber")
        ({name: "add", 
          constraints: {},
          types: [$.FiniteNumber, $.FiniteNumber, $.FiniteNumber]})

  test_ ("sum :: Foldable f => f FiniteNumber -> FiniteNumber")
        ({name: "sum", 
          constraints: {f: [Z.Foldable]},
          types: [f ($.FiniteNumber), $.FiniteNumber]})

  test_ ("sub :: FiniteNumber -> FiniteNumber -> FiniteNumber")
        ({name: "sub", 
          constraints: {},
          types: [$.FiniteNumber, $.FiniteNumber, $.FiniteNumber]})

  test_ ("mult :: FiniteNumber -> FiniteNumber -> FiniteNumber")
        ({name: "mult", 
          constraints: {},
          types: [$.FiniteNumber, $.FiniteNumber, $.FiniteNumber]})

  test_ ("product :: Foldable f => f FiniteNumber -> FiniteNumber")
        ({name: "product", 
          constraints: {f: [Z.Foldable]},
          types: [f ($.FiniteNumber), $.FiniteNumber]})

  test_ ("div :: NonZeroFiniteNumber -> FiniteNumber -> FiniteNumber")
        ({name: "div", 
          constraints: {},
          types: [$.NonZeroFiniteNumber, $.FiniteNumber, $.FiniteNumber]})

  test_ ("pow :: FiniteNumber -> FiniteNumber -> FiniteNumber")
        ({name: "pow", 
          constraints: {},
          types: [$.FiniteNumber, $.FiniteNumber, $.FiniteNumber]})

  test_ ("mean :: Foldable f => f FiniteNumber -> Maybe FiniteNumber")
        ({name: "mean", 
          constraints: {f: [Z.Foldable]},
          types: [f ($.FiniteNumber), $Maybe ($.FiniteNumber)]})

  test_ ("even :: Integer -> Boolean")
        ({name: "even", 
          constraints: {},
          types: [$.Integer, $.Boolean]})

  test_ ("odd :: Integer -> Boolean")
        ({name: "odd", 
          constraints: {},
          types: [$.Integer, $.Boolean]})

  test_ ("parseDate :: String -> Maybe ValidDate")
        ({name: "parseDate", 
          constraints: {},
          types: [$.String, $Maybe ($.ValidDate)]})

  test_ ("parseFloat :: String -> Maybe Number")
        ({name: "parseFloat", 
          constraints: {},
          types: [$.String, $Maybe ($.Number)]})

  test_ ("parseInt :: Radix -> String -> Maybe Integer")
        ({name: "parseInt", 
          constraints: {},
          types: [Radix, $.String, $Maybe ($.Integer)]})

  test_ ("parseJson :: (Any -> Boolean) -> String -> Maybe a")
        ({name: "parseJson", 
          constraints: {},
          types: [$.Predicate ($.Any), $.String, $Maybe (a)]})

  test_ ("regex :: RegexFlags -> String -> RegExp")
        ({name: "regex", 
          constraints: {},
          types: [$.RegexFlags, $.String, $.RegExp]})

  test_ ("regexEscape :: String -> String")
        ({name: "regexEscape", 
          constraints: {},
          types: [$.String, $.String]})

  test_ ("test :: RegExp -> String -> Boolean")
        ({name: "test", 
          constraints: {},
          types: [$.RegExp, $.String, $.Boolean]})

  test_ ("match :: NonGlobalRegExp -> String -> Maybe { match :: String, groups :: Array (Maybe String) }")
        ({name: "match", 
          constraints: {},
          types: [$.NonGlobalRegExp, $.String, $Maybe (Match)]})

  test_ ("matchAll :: GlobalRegExp -> String -> Array { match :: String, groups :: Array (Maybe String) }")
        ({name: "matchAll", 
          constraints: {},
          types: [$.GlobalRegExp, $.String, $.Array (Match)]})

  test_ ("toUpper :: String -> String")
        ({name: "toUpper", 
          constraints: {},
          types: [$.String, $.String]})

  test_ ("toLower :: String -> String")
        ({name: "toLower", 
          constraints: {},
          types: [$.String, $.String]})

  test_ ("trim :: String -> String")
        ({name: "trim", 
          constraints: {},
          types: [$.String, $.String]})

  test_ ("stripPrefix :: String -> String -> Maybe String")
        ({name: "stripPrefix", 
          constraints: {},
          types: [$.String, $.String, $Maybe ($.String)]})

  test_ ("stripSuffix :: String -> String -> Maybe String")
        ({name: "stripSuffix", 
          constraints: {},
          types: [$.String, $.String, $Maybe ($.String)]})

  test_ ("words :: String -> Array String")
        ({name: "words", 
          constraints: {},
          types: [$.String, $.Array ($.String)]})

  test_ ("unwords :: Array String -> String")
        ({name: "unwords", 
          constraints: {},
          types: [$.Array ($.String), $.String]})

  test_ ("lines :: String -> Array String")
        ({name: "lines", 
          constraints: {},
          types: [$.String, $.Array ($.String)]})

  test_ ("unlines :: Array String -> String")
        ({name: "unlines", 
          constraints: {},
          types: [$.Array ($.String), $.String]})

  test_ ("splitOn :: String -> String -> Array String")
        ({name: "splitOn", 
          constraints: {},
          types: [$.String, $.String, $.Array ($.String)]})

  test_ ("splitOnRegex :: GlobalRegExp -> String -> Array String")
        ({name: "splitOnRegex", 
          constraints: {},
          types: [$.GlobalRegExp, $.String, $.Array ($.String)]})

  end ()
})