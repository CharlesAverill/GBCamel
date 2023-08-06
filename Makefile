.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

default: build

fmt:
	opam exec -- dune build @fmt
	opam exec -- dune promote

build: fmt
	opam exec -- dune build

install:
	opam exec -- dune install

uninstall:
	opam exec -- dune uninstall

clean:
	mv roms roms_noclean
	opam exec -- dune clean
	git clean -dfXq
	mv roms_noclean roms
	rm -rf docs

test: fmt
	opam exec -- dune runtest

testf: fmt
	opam exec -- dune runtest -f

LOG_LEVEL ?= 2
ROM_FILE ?= roms/tetris.gb

run: build
	opam exec -- dune exec -- gbcamel -log-level $(LOG_LEVEL) $(ROM_FILE)

raw_run: build
	clear
	_build/default/bin/main.exe -log-level $(LOG_LEVEL) $(ROM_FILE)

debug: build
	opam exec -- ocamldebug _build/default/gbcamel/main.bc

DOCS_PATH=docs/

docs: build
	if [ ! -d $(DOCS_PATH) ]; then \
		mkdir $(DOCS_PATH); \
	fi
	opam exec -- dune build @doc
	mv _build/default/_doc/_html/* $(DOCS_PATH)
