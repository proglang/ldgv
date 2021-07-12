# Artifact for Label Dependent Lambda Calculus and Gradual Typing

This artifact contains two parts corresponding to our claim at the end of section 1.

1. An implementation of the type checker for LDLC corresponding to section 3.2 from the paper and its gradual extension for GLDLC described in section 5.1. It does /not/ contain the translation described in section 5.2.
2. A formalization of the cast calculus CCLDLC (a proper extension of LDLC) described in section 4 and an implementation of the progress proof 4.6 in Agda. The remaining proofs are manual at this point.

The Getting Started Guide should contain setup instructions (including, for example, a pointer to the VM player software, its version, passwords if needed, etc.) and basic testing of your artifact that you expect a reviewer to be able to complete in 30 minutes. Reviewers will follow all the steps in the guide during an initial kick-the-tires phase. The Getting Started Guide should be as simple as possible, and yet it should stress the key elements of your artifact. Anyone who has followed the Getting Started Guide should have no technical difficulties with the rest of your artifact.

## Getting Started

1. The `README.md` file in the `ld-session-code` directory contains instructions to build the type checker. The easiest way is to use docker as described in the last paragraph. Producing the docker image takes on the order of 10 minutes on my machine.
2. If you happen to have Agda 2.6.1.3 and version 1.6 of the standard library installed, then it's a matter of loading the file ... in the `formalization` directory into an emacs buffer and running the Agda type checker using C-C C-C. Otherwise, use `docker build -t formalization .` to create the image (this will take quite a while). 

## Step by Step Instructions

### Implementation of Type checker

We suggest reading sections 1 and 2 of the paper. The `README.md` file gives an overview of the examples provided. They comprise all example code for LDLC and GLDLC shown in these two sections and some extra examples in (an extension of) LDLC. Be warned that the paper also contains code fragments from CCLDLC (essentially, any piece of code that contains a cast). Casts are /not/ implemented in the type checker and there is no syntax to express them. This is intentional as programmers are not expected to use CCLDLC directly.

Further experiments: after studying the subtyping in Figure 4, try modifying the example subtypings in `examples-subtyping-dynamic.txt`. Try extending `examples/person.gldlc` or try to encode an algebraic datatype like a binary tree using the approach demonstrated in section 2.4.

### Agda Formalization

There is not much to do here apart noticing that the files provided type check. Here is an overview of the most important definitions that you might want to study.


