.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

default: build

build:
	dune build

install:
	dune install

uninstall:
	dune uninstall

clean:
	mv roms roms_noclean
	dune clean
	git clean -dfXq
	mv roms_noclean roms

fmt:
	dune build @fmt
	dune promote

test: fmt
	dune runtest

run: build fmt
	dune exec gbcamel

debug: build fmt
	ocamldebug _build/default/gbcamel/main.bc
