FANG_VERSION ?= 0.0.0

all: dist

dist:
	git archive --output=fang_$(FANG_VERSION).tar.gz HEAD

.PHONY: clean

doc-human: overview.html development.html tiger.html

%.html: %.org
	emacs --batch --no-init --load publish.el --eval "(publish \"$<\")"

doc-api: doc-api-odoc doc-api-prettify
	@find _build/default/_doc/_html/fang -name "index.html" -exec bash -c '_build/default/doc/prettify.exe < {} > tmp.html; mv -f tmp.html {}' \;

doc-api-odoc:
	dune build @doc

doc-api-prettify:
	dune build doc/prettify.exe

format:
	dune build @fmt --auto-promote

test-flake:
	nix build && rm result

test-dist:
	dune build --no-buffer @run-dist-test

test-license:
	find . -name _build -prune -o \( -name '*.mli' -o -name '*.ml' -o -name '*.el' \) -print0 | xargs -0 -n1 grep -L 'SPDX-License-Identifier'

clean:
	rm -f overview.html development.html tiger.html
	dune clean
