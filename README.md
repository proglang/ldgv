# ldgv - Label dependent session types

This repository contains an implementation of a frontend (parser and
type checker) and a backend for LDGV.

## Requirements

Installation of the Haskell stack, ~~tested with Version 1.9.3~~

## Build

In the toplevel directory:

`stack build`

## Test the Parser

run

`stack test`

to let [hspec](https://hspec.github.io/) discover and run all tests.

## Usage

In the toplevel directory:

`stack exec ld-session-code-exe`

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
