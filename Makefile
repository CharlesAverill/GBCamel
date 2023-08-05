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

testf: fmt
	dune runtest -f

LOG_LEVEL ?= 2
ROM_FILE ?= roms/tetris.gb

run: build fmt
	dune exec -- gbcamel -log-level $(LOG_LEVEL) $(ROM_FILE)

raw_run: build fmt
	clear
	_build/default/bin/main.exe -log-level $(LOG_LEVEL) $(ROM_FILE)

debug: build fmt
	ocamldebug _build/default/gbcamel/main.bc
