# Gradual LDLC

An implementation of a frontend (parser and type checker) for LDGV, a
type system for label-dependent session types.
LDLC is a proper subset of LDGV.

The typechecker has been extended to GLDLC.

## requirements

Installation of the Haskell stack Version 2.7.1 or a recent version of
docker (see last section). Installation of the Haskell stack is
described on https://docs.haskellstack.org/en/stable/README/

## build

In the toplevel directory:

* `stack build` (obtains and builds compiler and libraries according to
  the resolver lts-13.19 and) compiles the type checker

## usage

In the toplevel directory:

`stack test` runs tests for scanner and parser 

To run the toplevel CLI:

`stack run`

This program is interactive and reads from standard input!
The input syntax is explained in file `syntax.txt`.
There are examples in the `examples` directory. Source files that
contain LDLC or LDGV end in `.ldgv`. They should be redirected into the executable as in

`stack run < examples/node.ldgv`

There are further test cases in `example-inputs`. This file contains
single lines which test subtyping and other features in isolation. It
can be processed as a whole.

Examples specific for GLDLC are marked by the file extension `.gldlc`.

* `examples/examples-subtyping-dynamic.txt` exercises subtyping. This file can be processed as a whole.
* `examples/section1.gldlc` contains all LDLC and GLDLC code from Section 1 of the paper.
* `examples/section2.gldlc` LDLC and GLDLC examples from Section 2, except:
* `examples/person.gldlc` an example built on the Person datatype in section 2.4.
* `examples/optionalint.gldlc` an examples built on the OptionalInt dataype in section 2.4.

The expected outputs may be found in the corresponding `.out` files as
in `examples/section1.out`.

## using the docker image

Instead of installing stack on your local machine, you can run the
whole process with docker (Docker version 20.10.7, build
f0df350). This is a quick-and-dirty image, but it seems to work ok.

* Build the image using `docker build -t pthie/gldlc .` in the top-level directory.
* Replace `stack run` by `docker run -i pthie/gldlc`. For example

`docker run -i pthie/gldlc < examples/node.ldgv`

