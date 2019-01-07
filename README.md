Javascript runtime [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) ad hoc type checking with [Sanctuary](https://github.com/sanctuary-js/sanctuary).

## Overview

It's basically a syntax sugar over [sanctuary-def](https://github.com/sanctuary-js/sanctuary-def), inspired on [hm-def](https://github.com/xodio/hm-def).

_"It facilitates the definition of curried JavaScript functions which are explicit about the number of arguments to which they may be applied and the types of those arguments." - sanctuary-def_

## Features

 - All Sanctuary's [type constructors](https://github.com/sanctuary-js/sanctuary-def#type-constructors) and [type classes](https://github.com/sanctuary-js/sanctuary-type-classes) available out of the box.
 - Supports custom type constructors and type classes.
 - Only three dependencies: [Sanctuary](https://github.com/sanctuary-js/sanctuary), [Parsimmon](https://github.com/jneen/parsimmon) and [hm-lang-light](https://github.com/leosbotelho/hm-lang-light).
 - Comprehensively tested - works with any Sanctuary type declaration.
 - Input is fairly validated.
 - Written in a -quite- functional Javascript idiom.

## Getting Started
 
 1. Install `hm-def-light` with `npm` - or by any preferred means.
    - For testing, it's sufficient to install the dev dependencies and run the `test` script.
    - It's dependent on `import`s. So, adopt it - with e.g: [esm](https://github.com/standard-things/esm), [babel](https://github.com/babel/babel).
 2. Then you're ready to go! Please, follow the examples for further apprehension.

## API

hm-def-light is reliant on:
```yaml
create           : S create
env              : S env
$                : sanctuary-def
Z                : sanctuary-type-classes
checkTypes       : Boolean
typeClasses      : optional [TypeClass]
typeConstructors : optional StrMap Type'
```
<sup>Params 1-5 are not checked for correctness: they're supposed to be correct.</sup><br>
<sup>Type' is a -fancy, uncanon- alias for $ `Nullary`, `Unary` and `Binary` types.</sup>

One by one brief:
```yaml
create           : for internal S retrieval
env              : basis for `Type'`s (e.g: `Number`, `HtmlElement`, `Error` etc)
$                : for `Type`s construction
Z                : basis for default `TypeClasses` (e.g: `Functor`, `Alt`, `Traversable` etc)
checkTypes       : "The checkTypes option determines whether type checking is enabled. 
                    This allows one to only pay the performance cost of run-time type checking  
                    during development."
typeClasses      : optional custom `TypeClass`es
typeConstructors : optional custom `Type'`s; * must also be defined in the environment (i.e env)
```

Hands on:

First define a `def` function for the setting:
```javascript
import {create, env}  from "sanctuary"
import SDef           from "sanctuary-def"
import Z              from "sanctuary-type-classes"

import $create        from "hm-def-light"

// checkTypes :: Boolean
const checkTypes = true

// minimal
const def = $create ({create, env, $ : SDef, Z, checkTypes})
```

Now you can use `def` in function definitions:
```javascript
const add =
def ("add :: Number -> Number -> Number")
    (x => y => x+y)

add (1) (2) // ok
add (3) (4) // ok

add ("a") ("b") // error - in case of checkTypes = true
add ("c") ("d") // error ...
```

As a design choice, `def` is not memoized.  
So you should be attentious to repeated `def` calls:
```javascript
while (true) {
  // this repeats the entire, sanctuary-def `def`, 'mirroring' for every iteration
  def ("K :: a -> b -> a")
      (x => y => x)
}
```

In this case, the function definition could be taken out of the loop.  
Alternatively, it's very easy to memoize `def` yourself:
```javascript
// e.g
const memoDef = def => memo =>
  memo (typeDeclaration => def (typeDeclaration))
```

minimal `def` also works for more complex type signatures:
```javascript
const ex1 =
def ("ex1 :: (Foldable a, Alternative a) => a -> a")
    (x => x)

ex1 (1)   // error
ex1 ([])  // ok

const ex2 =
def ("ex2 :: Maybe a -> Either String a")
    (x => maybeToEither ("dummy") (x))

ex2 (Just (1))    // Right (1)
ex2 (Nothing)     // Left  ("dummy")
ex2 ("arbitrary") // error

const ex3 =
def ("ex3 :: NonEmpty [Number] -> Number")
    (arr => arr[0])

ex3 ([1,2]) // ok
ex3 ([])    // error

// ...
```

And if you need custom `Type'`s or `TypeClass`es, there's no secret, e.g:
```javascript
const customEnv = env.concat ([MyCustomType (SDef.Unknown)]) // `Type` in env is mandatory

const S = create ({
  checkTypes, 
  env : customEnv
})

const def = $create ({
  create, 
  env              : customEnv, 
  $                : SDef, 
  Z, 
  checkTypes,
  typeClasses      : [MyCustomTypeClass],
  typeConstructors : {MyCustomType}
})

// ...
```

Running tests:  
<img src="https://raw.githubusercontent.com/leosbotelho/hm-def-light/master/img/tests-ilustration.png" alt="bash test results ilustration" width="50%" height="50%">

For more information, I recommend a stride around the referenced resources.

## License

This project is licensed under the MIT License - see the [LICENSE](./LICENSE.md) file for details
