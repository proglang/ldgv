# ldgv - Label dependent session types

This repository contains an implementation of a frontend (parser and
type checker) and a backend for LDGV.

## C backend

* [ ] syntax elements
  * [x] variable bindings
  * [x] integer literals & math operations
  * [x] label literals & case expressions
  * [x] lamdas
  * [x] call expressions, partial application
  * [x] pairs construction & destructuring
  * [ ] recursive bindings
  * [ ] `natrec`
  * [ ] `fork`
  * [ ] channel operations
    * [ ] `new`
    * [ ] `recv`
    * [ ] `send`
* [ ] entrypoint generation
    * [ ] `main()`
    * [ ] user requested
* [ ] garbage collector

## Requirements

The [nix packet manager](https://nixos.org/nix/), preferably with a binary cache set up
like described [here](https://github.com/obsidiansystems/obelisk/blob/master/README.md).


## Build

In the toplevel directory:

`nix-build -A ghcjs.ldgv`

## Test the Parser

Run

`nix-shells -A shells.ghc`

to go into a nix shell and execute

`cabal test`

to let [hspec](https://hspec.github.io/) discover and run all tests.

## Usage

After building, you will find an `index.html` in `results/bin/ldgv.exe`.

The input syntax is explained in file `syntax.txt`.
There are examples in the `examples` directory. Source files end in
`.ldgv`. 

### Typechecker

`stack run -- examples/node.ldgv`

### Interpreter
Run

`stack run -- -i examples/simple.ldgv`

to run the typical session types hello world.

There are further test cases in `example-inputs`, but this file contains
single lines which test subtyping and other features in isolation.
