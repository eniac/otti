# Circom Compiler

## Overview

Circom is a language for describing hierarchical circuits which encode rank-1
constraints over signals.

A signal is an input to the circuit, an output from it, or perhaps an
intermediate result.

A rank-1 constraint is an equality constraint of the form: A * B = C where
each of A, B, C are affine combinations of signals.

The language of circom is somewhat like verilog: you have modules
(`template`s) which contain wires (`signal`s) and submodules (`compnent`s).
Each module also contains expression and statements (with an ambient
binding environment) that is uses to construct rank-1 constraints.

Circom has an extra feature though. In addition to encoding constraints over
signals, it encodes the computation needed to determine all
intermediate/output signals from the inputs signals.

Thus, every circom program actually has two interpretations: as a constraint
system and as a function from inputs to intermediates and outputs.

When a circom program is "generated", its modules are expanded into one of the
above objects.

## Design

The principle behind this compiler is to share as much machinery as possible
between witness computation and constraint generation.

To facilitate this, the kinds of terms, and part of the generation
environment, are abstracted out of the compiler. Generation terms can be
(gen-time) constants, arrays, components, or abstract base terms (which encode
either something about constraints or functions). The generation environment
tracks local variables and components, but delegates part of getting/setting
signals to an abstract sub-environment which has something to do with
constraints or functions.

## (The Implementation's) Module Structure

* Compilation.hs
  * Contains the abstract compiler
  * **Does not** inline modules. Produces one output for each (parameterized)
     module.
* CompTypes.hs
  * Defines BaseCtx, BaseTerm (classes for abstract compilation) and
     implements them for terms that contain other base terms.
  * Defines the compilation context monad
  * LowDeg.hs
    * Defines a low-degree term (implements BaseTerm) and a context with
       constraints (implements BaseCtx)
  * WitComp.hs
    * Defines a smt-based term (implements BaseTerm) and a context which binds
       terms to signals (implements BaseCtx)
* Linking.hs
  * Contains functions for gluing constraints together between modules
  * Contains functions for computing signal values from a witness-computation
     compilation output (and the inputs!)
* Signal.hs
  * A few types for signals
* Typing.hs
  * A type for the interface to a module
* Utils.hs
  * Misc. For example, spanned errors.


