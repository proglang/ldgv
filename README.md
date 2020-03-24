# ldgv - Label dependent session types

This reponsitory contains an implementation of a frontend (parser and
type checker) for LDGV.

## requirements

Installation of the Haskell stack, tested with Version 1.9.3

## build

In the toplevel directory:

`stack build`

## test

run

`stack test`

to let [hspec](https://hspec.github.io/) discover and run all tests.

## usage

In the toplevel directory:

`stack exec ld-session-code-exe`

The input syntax is explained in file `syntax.txt`.
There are examples in the `examples` directory. Source files end in
`.ldgv`. They should be redirected into the executable as in

`stack exec ld-session-code-exe < examples/node.ldgv`

There are further test cases in `example-inputs`, but this file contains
single lines which test subtyping and other features in isolation.
