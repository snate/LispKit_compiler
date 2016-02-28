# LispKit_Compiler

University project for Programming Languages' course.

## The Language ##

LispKit is a language that is very similar to LISP, with a few restrictions
that makes it easier to create a compiler for.

What LispKit offers:

* `let` and `letrec` operators (`letrec` allows the use of recursive
  references in bindings);
* `if then else` as the only control flow operator;
* the basic algebraic operations: sum, difference, multiplication and division;
* the possibility to define a function by means of `lambda`;
* operators to work on lists: `cons` (list *cons*truction), `car` (returns the
  head of a list), `cdr` (returns the tail of a list), `eq` (equality test),
  `leq` (less or equal test) and `atom` (check if the argument is an atom)

Notes:
* the bindings are separated by `and`s;
* the actual parameters are separated by commas.

## The Lexer ##
This module (`lexer.hs`) produces a list of tokens that translate the input
program into a sequence of lexems (i.e. in computer language) that are
understandable for the parser.

## The Parser ##
This module (`complete_parser.hs`) is responsible of processing the sequence
of tokens in order to return a hierarchical view of the program (we can see
every LispKit program as if it were a tree).

Monads are used here in place of exceptions, keeping the program flow neat.

## The Compiler ##
This module (`compiler.hs`) has to generate a sequence of instructions for
SECD machine starting from the tree that the parser produces.

## The Interpreter ##
This module (`interpreter.hs`) simulates a virtual machine called *SECD*
(Store, Environment, Control, Dump).

## Notes ##
This repository contains also several files (`Grammatica`, `GrammCorretta`)
that represents another step of the project.

Indeed, we had to modify a non-LL(1) grammar (`Grammatica`) by adding a
non-terminal and a production to obtain a LL(1) grammar (`GrammCorretta`).
