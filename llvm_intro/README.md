# Getting started

Kick off by creating a version 3.0 dune-project and a trivial dune file

```lisp
(executable
  (name main)
  (libraries llvm))
```
and run `dune exec ./main.exe` to see whether the opam switch is up
and running with the `llvm` package installed.

After that follow along with the really nice tutorial video at
https://www.youtube.com/watch?v=Brcs3GW5-hM
where Colin James walks through building an arithmetic calculator
language with llvm and ocaml bindings.

Note that in that tutorial, he is using a pre-16 version of llvm because
the `build_call` signature looks the same as what I have (in the most
recent version, the return type is part of `build_call`).

I extended his example a bit to include the `Call` form from the AST.

You can view the output with `dune exec ./main.exe`.


To actually compile code (either `demo.ll`, which is a hand-written
example, or the output of the compiler) into assembly, you can run
`llc <filename>.ll`
