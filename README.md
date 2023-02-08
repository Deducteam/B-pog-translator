# B-pog-translator
Translate a POG file into a lambdapi file

# How to compile
```
$ ocamlbuild -pkg markup main.native
```

And in the `b` directory, do
```
lambdapi install syntax.lp
```

# How to use
```
$ ./main.native -i input.pog -o output
```

This will create a directory `output` in which there will be some lambdapi files corresponding to the translation of `input.pog`. These files depends on the B.syntax module (the `syntax.lp` file in the `b` directory).
