# learn-compilers

Repo for going through beginner compiler tutorials (probably mostly in OCaml)

## Set up LLVM with brew

I got segfaults when I tried to use llvm 16 and OCaml 5 as of December 2023;
I may not have had the right config but it's possibly releated to known issues
with Llvm bindings and no-naked-pointers, or possibly related to the recent
change in `build_call` type (like maybe the ocaml and brew sources are out
of sync?).

As a result I'm using llvm 14 and OCaml 4.14.0 for these tutorials. I'm confident
that either I just messed up or if this is a real issue it will get solved over
the next year or so.

```
brew install llvm@14 cmake python@3.9
```

Note that the command-line tools are not at their normal names, they are postfixed,
for example `llc-mp-14` is the `llc` command.



## Set up opam switch to use for these projects

Install opam switch:
```bash
opam switch create learn-compilers 4.14.0
```

In any shell, before working on this project or installing ocaml
packages and tools:
```
eval "$(opam env --switch learn-compilers)"
```

Install the basic tools. Pin the dune version just in case:
```
opam install -y dune.3.12.1 utop merlin ocaml-lsp-server
```

Install llvm. First try to do it out of the box:
```
opam install -y llvm.14.0.6
```
which on my machine failed.

Opam doesn't give very good errors, but you can `cd` to the project
root at `~/.opam/learn-compilers/.opam-switch/build/llvm.16.0.6+nn`
and run `dune build --profile release` to figure out the problem;
in my case it was issues finding `zstd`.

The solution I eventually found was to explicitly set
`LIBRARY_PATH` using `brew --prefix`:

``` bash
LIBRARY_PATH=$LIBRARY_PATH:$(brew --prefix zstd)/lib/ opam install -y llvm.14.0.6
```


Install other ocaml dependencies, with pinned versions:
```
opam install -y \
  core.v0.16.2 \
```

## Note to self: figuring this out was a real pain. Try nix next time!

Two blog links to get started with nix for ocaml:
- https://dimitrije.website/posts/2023-03-04-nix-ocaml.html
- https://blog.jethro.dev/posts/ocaml_emacs_nixos/


Integrating stock VSCode with nix-shell:
- https://matthewrhone.dev/nixos-vscode-environment


A couple of more specific tools that might also be handy:
- https://www.tweag.io/blog/2023-02-16-opam-nix/ (Tweag usually has good stuff)
- https://github.com/rizo/onix
