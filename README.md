# YAFOLPS (Yet Another First-Order Logic Problem Solver)

## EBNF (Extended Backus–Naur form):

```
<Forall> ::= "!"
<Exists> ::= "?"
<LogicAnd> ::= "&"
<LogicOr> ::= "|"
<LogicNegation> ::= "-"
<LogicImplication> ::= ">"
<Top> ::= "^"
<Bottom> ::= "_"

<Char> ::= [a-z] | [A-Z] | [0-9] | "_"
<Var> ::= [A-Z] (<Char>)*
<Term> ::= <Var> | [a-z] (<Char>)* "(" ((<Term> ",")* <Term>) ")"

<Quantifier> ::= <Forall> | <Exists>
<LogicOperation> ::= <LogicAnd> | <LogicOr> | <LogicImplication>

<PredicateSymbol> ::= [a-z] (<Char>)* "(" ((<Term> ",")* <Term>) ")"

<Singleton> ::= <Top> | <Bottom> | <PredicateSymbol>
<Formula> ::= <Singleton> | "(" <Formula> ")"
  | <Formula> <LogicOperation> <Formula> 
  | <Quantifier> <Var> "(" <Formula> ")" 
```

## Usage: stack build && stack exec -- YAFOLPS-exe <args>

## Args:

* ``--input-file filename / --i filename -- Reads a formula from a file. Without specifying this option reads a formula from standard input.``
* ``--print-formula / -f -- Prints the formula that have been read.``
* ``--print-ast / -a -- Prints the AST of the formula.``
* ``--print-ssf / -s -- Prints the SSF of the formula.``
* ``--print-solution / -r -- Prints is the formula solvable (SAT) or not (UNSAT).``
  
## Examples:

* ``stack build && stack exec -- YAFOLPS-exe --i test/example1 -fas`` -- ``Prints formula, AST and SSF``
* ``stack build && stack exec -- YAFOLPS-exe --i test/example1 -r`` ``-- Prints "SAT"``
* ``stack build && stack exec -- YAFOLPS-exe --i test/example2 -r`` ``-- Prints "UNSAT"``
* ``stack build && stack exec -- YAFOLPS-exe --i test/example3 -r`` ``-- Prints "UNSAT"``
* ``stack build && stack exec -- YAFOLPS-exe --i test/example4 -r`` ``-- Prints "SAT"``
