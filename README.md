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
 2. Then you're ready to go!

## Examples

Running tests:  
<img src="https://raw.githubusercontent.com/leosbotelho/hm-def-light/master/img/tests-ilustration.png" alt="bash test results ilustration" width="50%" height="50%">

## License

This project is licensed under the MIT License - see the [LICENSE](./LICENSE.md) file for details
