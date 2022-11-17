.PHONY: all build clean doc repl fmt deps

all: build doc

build:
	dune build

doc:
	dune build @doc

clean:
	dune clean

repl:
	dune utop

fmt:
	dune build @fmt --auto-promote

deps:
	opam install --deps-only .
