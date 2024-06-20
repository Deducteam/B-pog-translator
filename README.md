# pog2why
Prove a proof obligation in POG format using Why3.

# Requirements
- ocaml version>4.14
- Why3 library
- Markup library

To install the why3 and markup library, do
```
$ opam install why3 markup
```

# How to compile
```
$ ocamlbuild -pkg why3 -pkg markup main.native
```

# How to use
```
$ ./main.native [[-P] [-p prover]] -a M N -i input.pog -o output
```

It considers the goal N in the proof obligation M in input.pog. If prover is not set, it outputs a .mlw file (which is not intended to typecheck).
If prover is set, it outputs a TPTP/SMT-LIB/... file appropriate for that prover.
Moreover, if -P is set, it outputs an empty file (TOFIX) and the prover is called on the goal.

# Example
For instance, you can prove Cantor's theorem:
```
$ ./main.native -p "Zenon Modulo" -a 0 0 -i example/cantor.pog -o output.p
$ zenon_modulo -itptp ~/pog-parser/cantor.p -olp
```

To get the lambdapi logics, you need to have lambdapi installed, and to add the lambdapi-logics repository, before installing the package lambdapi-logics:

```
$ opam install lambdapi
$ opam repository -a add lambdapi https://github.com/deducteam/opam-lambdapi-repository.git
$ opam install lambdapi-logics
```

You can then try to check the example:
```
$ lambdapi check output.lp
```

This file contains variables 0.0 and 1.0, which are ill-named and should be removed by hand.

# Not implemented yet
- dealing with multiple goals
- structures
- fix Zenon Modulo use of floating-points (0.0 and 1.0)
- cleaning the code
- ...
