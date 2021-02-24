# ldgv - Label dependent session types

This repository contains an implementation of a frontend (parser and
type checker) and a backend for LDGV.

The syntax is described in `syntax.txt`. The directory `examples/`
contains some examples, source files end in `.ldgv`.

## Interpreter

There are two ways to run the interpreter, via the [command
line](#command-line) and via a local [web page](#web-page).


## C backend

The C backend is nearly feature complete:

* [x] syntax elements
  * [x] variable bindings
  * [x] integer literals & math operations
  * [x] label literals & case expressions
  * [x] lamdas
  * [x] call expressions, partial application
  * [x] pairs construction & destructuring
  * [x] recursion
    * [x] recursive bindings
    * [x] `natrec`
  * [x] `fork`
  * [x] channel operations
    * [x] `new`
    * [x] `recv`
    * [x] `send`
* [x] entrypoint generation
    * [x] `main(void)`
    * [x] user requested (via `ldst__run`)
* [ ] garbage collector
* [ ] tests

It is available only via the [command line](#command-line). See there for
building and running.

### Compiling the generated C code

The generated C code is dependent on the files in `c-support/runtime/`:

* `LDST.h` defines the data structures and interfaces to the scheduler and
  channel handler.
* `LDST.c` contains implementations of helper functions for which there is no
  need to generate them every time.
* `LDST_serial.c` contains a serial implementation of a scheduler and channel
  handler.
* `thpool.h`, `thpool.c` come from [Pithikos/C-Thread-Pool][] and are used in
  the concurrent scheduler and channel handler implementation.

[Pithikos/C-Thread-Pool]: https://github.com/Pithikos/C-Thread-Pool

When compiling the generated code an optimization level of at least `-O2`
should be considered, with this the common compilers (clang, gcc) will
transform the abundant number of tail calls into jumps which will keep the
stack size from exploding.

### Integrating and using the generated C code

Every top level definition in the source file will correspond to a symbol of
type `LDST_fp0_t` (see `LDST.h`) with `ldst__` prepended. If the name contains
underscores or primes it will undergo additional transformations.

Such a function should *not* be called directly but only through `ldst_fork`
due to the interaction with the scheduler when using channel operations.
There exist `ldst_run` and `ldst_main` to help with the execution of top level
functions which don't exist as closures. See `LDST.h` for documentation of
these.

Using the option `-m IDENT / --main=IDENT` with the command line C backend emit
a `main(void)` function at the end of the resulting C code which executes
`IDENT` and prints the result. See the output of `$LDGV compile --help` for
more information.

Multiple LDST files can be compiled separatly and then linked together. It is
also possible to write functions in C and use them in LDST. The function has to
properly curried, the top level symbol must have a type signature matching
`LDST_fp0_t` and the name must match the name transformations pointed out
above. (NB: this allows to subvert the type system, in all good and bad ways.)


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

Incremental builds are not supported by `nix-build`, instead building via
`nix-shell` and `cabal` is possible:

1. Enter the `nix-shell`, this provides you with `cabal` and `ghcjs`:

      ```
      nix-shell -A shells.ghcjs
      ```

2. Use `cabal` inside the new shell, with a custom `--project-file`:

      ```
      cabal --project-file=cabal-ghcjs.project v2-build
      ```


## Testing

```
cabal test
```
