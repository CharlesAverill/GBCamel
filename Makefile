.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

default: build

fmt:
	dune build @fmt
	dune promote

build: fmt
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
	rm -rf docs

test: fmt
	dune runtest

testf: fmt
	dune runtest -f

LOG_LEVEL ?= 2
ROM_FILE ?= roms/tetris.gb

run: build
	dune exec -- gbcamel -log-level $(LOG_LEVEL) $(ROM_FILE)

raw_run: build
	clear
	_build/default/bin/main.exe -log-level $(LOG_LEVEL) $(ROM_FILE)

debug: build
	ocamldebug _build/default/gbcamel/main.bc

DOCS_PATH=docs/

docs: build
	if [ ! -d $(DOCS_PATH) ]; then \
		mkdir $(DOCS_PATH); \
	fi
	dune build @doc
	mv _build/default/_doc/_html/* $(DOCS_PATH)
