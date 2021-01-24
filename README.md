# ldgv - Label dependent session types

This repository contains an implementation of a frontend (parser and
type checker) and a backend for LDGV.

The syntax is described in `syntax.txt`. The directory `examples/`
contains some examples, source files end in `.ldgv`.

## Interpreter

There are two ways to run the interpreter, via the [command
line](#command-line) and via a local [web page](#web-page).


## C backend

The C backend is not yet feature complete:

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
    * [x] `main(void)`
    * [ ] user requested
* [ ] garbage collector

It is available only via the [command line](#command-line). See there for
building and running.


## Building

### Command Line

To build the command line program the Haskell build tool `cabal` is required with GHC ≥8.6.1.

```
cabal run :exe:ldgv-exe -- --help
```

builds and runs the executable.

### Web Page

To build the webpage the [nix package manager](https://nixos.org/nix/) is
required, preferably with a binary cache set up as described
[here](https://github.com/obsidiansystems/obelisk/blob/master/README.md).

```
nix-build -A ghcjs.ldgv
```

builds the project. Afterwards there is a `index.html` in `results/bin/ldgv.exe`.


## Testing

```
cabal test
```
