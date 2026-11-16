# Fiord

Fiord is a toy proof checker for FIrst ORDer logic using natural deduction.

It allows to write theorems and proofs in a language similar to Coq vernacular.

## Build

With [opam](https://opam.ocaml.org/) installed and initialized (`opam init`),
you can run `make switch` to build a development environment under `./_opam`

Using `make`, one can build two executables :
  - fiordtop is a very simple toplevel for the language.
  - fiordcheck FILE checks the proofs contained in FILE.
## Examples

One example Fiord program is contained in examples/arith, and proves the
commutativity of addition from Peano axioms.

## Project Structure

The lib/ directory contains the logic of the program:
  - lib/ast.ml defines most of the types and the AST of the language.
  - lib/context.ml defines the proof context.
  - lib/formula.ml contains the functions to manipulates formulas.
  - lib/interpreter.ml interprets the language in which theorems are written and
    proven.
  - lib/language.ml defines types to represent the first order language
    (signature) which is currently used by the program.
  - lib/tactics.ml contains the tactics, i.e. the instructions in which the
    proofs are written. This includes basic natural deduction rules but also
    more complex instructions.

The bin/ directory contains the material for input/output (lexer.mll,
parser.mly, print.ml), and the executables.
