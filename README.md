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

You may also need to run `brew link llvm@14`; I'm unsure whether this is
necesary for later steps to work.



## Set up opam switch to use for these projects

Install opam switch; you need to use OCaml 4 for this version of `llvm`
because it relies on naked pointers:
```bash
opam switch create learn-compilers 4.14.0
```

In any shell, before working on this project or installing ocaml
packages and tools:
```
export OPAMSWITCH=learn-compilers
```

Install the basic tools. Pin the dune version just in case:
```
opam install -y dune.3.12.1 utop merlin ocaml-lsp-server
```

Install the llvm package, pinned to version 14:
```
opam install -y llvm.14.0.6
```

This worked out of the box for me, but with version 16 I got errors finding
`zstd` which required me to manually hack my library path using `brew --prefix`,
and I want to record the fix here for future reference even though on version
14 there was no problem:
``` bash
LIBRARY_PATH=$LIBRARY_PATH:$(brew --prefix zstd)/lib/ opam install -y llvm.14.0.6
```


Install other ocaml dependencies, with pinned versions:
```
opam install -y \
  core.v0.16.2 \
```

## Note to self: try nix next time!

Getting this working was a huge pain, and:
- now I'm stuck on an older llvm version for now, system-wide
- I suspect the setup is brittle and wouldn't reliably work for someone else

I hate both of these things, and `nix` is the best solution I know of; I think
if I'm going to hack llvm in particular it's really worth seeing if I can figure
out a nix-driven development flow so that I can reproduce my development environment
much more reliably.


Two blog links to get started with nix for ocaml:
- https://dimitrije.website/posts/2023-03-04-nix-ocaml.html
- https://blog.jethro.dev/posts/ocaml_emacs_nixos/


Integrating stock VSCode with nix-shell:
- https://matthewrhone.dev/nixos-vscode-environment


A couple of more specific tools that might also be handy:
- https://www.tweag.io/blog/2023-02-16-opam-nix/ (Tweag usually has good stuff)
- https://github.com/rizo/onix


# Some resources to look at for ocaml + compilers

Colin James has several projects, including an IMP compiler
to llvm and some lambda calculus stuff.

Tiger compilers:
- fang is a really well written project
- the llvm compiler looks really cool

Norah Sandler's book is coming out soon, it has a C compiler.

The Bolt compiler looks fantastic.

In terms of practically useful skills, ramping up to basic
llvm is almost certainly the #1 valuable skill. But I recently
saw a job posting asking for forklift experience, and I actually
think from a skim of the docs that forklift could be a much better
framework to dive deep on from a learning perspective, because it
is production-grade but *vastly* simpler than llvm so it should be
possible to learn it pretty well in a year or two, whereas llvm is
beyond the scale where anyone could understand it ground-up in
a lifetime.


