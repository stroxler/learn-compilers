# Getting started


## Bootstrapping

Assuming you have a flake-enabled `nix` available, you can bootstrap
a dev environment using
```
nix develop -c $SHELL
```

This should get you rolling against LLVM 18 (the latest version as of when
the `flake.lock` was created).

## How to develop

Run `dune build` to compile, and view the output of compiling the AST
(which is directly written into OCaml - this is a tiny demo with no parser)
run `_build/default/main.exe`. You can redirect stdout to a file in order to
then use `llc` against it.

To actually compile code (either `demo.ll`, which is a hand-written
example, or the output of the compiler) into assembly, you can run
`llc <filename>.ll`
