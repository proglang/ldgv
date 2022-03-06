# ldgv - Label dependent session types

This repository contains an implementation of a frontend (parser and
type checker), an interpreter and a C backend for LDGV.

LDGV stands for _Label Dependency_ an _Gay & Vasconcelos_, the authors of multiple papers about _Session Types_. It mainly implements [Label-Dependent Session Types](https://doi.org/10.1145/3371135). It additionally includes an implementation of the _Cast Calculus_ from [Label-Dependent Lambda Calculus and Gradual Typing](https://doi.org/10.1145/3485485).

The syntax is described in `syntax.txt`. The directory `examples/`
contains some examples, source files end in `.ldgv`.

Compilation is done via [Stack](https://docs.haskellstack.org/), which you need to have installed. You may either compile and run LDGV by `stack run ldgv` (**recommended**). Or you install LDGV by `stack install`, essentially copying the executable `ldgv` to your `$PATH`.

## Interpreter

Running the interpreter is possible via its command line interface

```stack run ldgv -- interpret```

resp. `ldgv interpret`.

## C backend

The C backend is nearly feature complete, but missing a garbage collector and
tests.

It is available only via the [command line](#command-line). See there for
building and running.

### Compiling the generated C code

The generated C code is dependent on the files in `c-support/runtime/`:

* `LDST.h` defines the data structures and interfaces to the scheduler and
  channel handler.
* `LDST.c` contains implementations of helper functions for which there is no
  need to generate them every time.
* `LDST_serial.c` contains a serial implementation of a scheduler and channel
  handler. Contexts in this implementation are not thread-safe.
* `LDST_concurrent.c` contains a concurrent implementation using a threadpool.
  Its size can be determined at compile time by defining
  `LDST_THREADPOOL_SIZE`, the default is four.
* `thpool.h`, `thpool.c` come from [Pithikos/C-Thread-Pool][] and are used in
  the concurrent scheduler and channel handler implementation.

[Pithikos/C-Thread-Pool]: https://github.com/Pithikos/C-Thread-Pool

The generated code requires C11 support and the concurrent runtime requires
support for the C11 atomics library and pthreads.

When compiling the generated code an optimization level of at least `-O2`
should be considered, with this the common compilers (clang, gcc) will
transform the abundant number of tail calls into jumps which will keep the
stack size from exploding.

The command line executable has support for invoking the C compiler on the
generated source code by passing either `-O` or `-L` on the command line, see
the output `compile --help` for more information.

### Integrating and using the generated C code

Every top level definition in the source file will correspond to a symbol of
type `LDST_fp0_t` (see `LDST.h`) with `ldst_` prepended. Additional
transformations are applied to support primes in function names: a `q` is
replaced by `qq` and a prime is replaced by `qQ`.

The top-level functions should *not* be called directly but only through
`LDST_fork` due to the interaction with the scheduler when using channel
operations.  There exist `LDST_run` and `LDST_main` to help with the execution
of top level functions which don't exist as closures. See `LDST.h` for
documentation of these.

Using the option `-m IDENT / --main=IDENT` with the command line C backend emit
a `int main(void)` function at the end of the resulting C code which executes
`IDENT` and prints the result. See the output of `$LDGV compile --help` for
more information.

Multiple LDST files can be compiled separatly and then linked together. It is
also possible to write functions in C and use them in LDST. The function has to
properly curried, the top level symbol must have a type signature matching
`LDST_fp0_t` and the name must match the name transformations pointed out
above. (NB: this allows to subvert the type system, in all good and bad ways.)

## Testing

You can run the full test suite by:

```
stack test
```

## Web Page (Deprecated)

There was a web version availabe, using [ghcjs](https://github.com/ghcjs/ghcjs) and the [reflex-platform](https://github.com/reflex-frp/reflex-platform). Because of some regression, presumably in some dependency of _reflex_, it is currently not available. Code and artifacts were factored out and moved to branch [browser-support](https://github.com/leyhline/ldgv/tree/browser-support).