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


## Sharp edges

I discovered the hard way that you can wire everything up to be type-safe in OCaml
and yet get segfaults in llvm. This isn't really all that surprising since the bindings
are pretty raw, but I don't yet know how to get debug symbols for llvm in nix (and even
if I did, they might be so big you wouldn't want them) so it is hard to handle.

What I learned from my segfault was that in `build_call` the type is the type of the function,
not the return type... when I used the return type it crashed, I'd guess because it was trying
to follow a null pointer in the struct for a function type but I don't really know.

In the process, I realized that the test code at
https://github.com/llvm/llvm-project/tree/49865107d49b2be86d9ff5474d081c49d1556213/llvm/test/Bindings/OCaml
is the best place to examine correct usage of the llvm api. I fixed the segfault when I found
a `build_call` example there.
