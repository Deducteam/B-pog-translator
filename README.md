# pog2why
Prove a proof obligation in POG format using Why3.

# Requirements
- ocaml version>4.14
- Why3 library
- Markup library

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

# Not implemented yet
- dealing with multiple goals
- structures
- Zenon Modulo
- cleaning the code
- ...
