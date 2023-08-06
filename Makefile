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
DOCS_NAME=GBCamel
DOCS_DESCR=A Game Boy Color Emulator written in OCaml
DOCS_INDEX_TITLE=$(DOCS_NAME) - $(DOCS_DESCR)
define DOCS_EMBED
<meta content="$(DOCS_NAME)" property="og:title" />\
<meta content="$(DOCS_DESCR)" property="og:description" />\
<meta content="https://charlesaverill.github.io/GBCamel" property="og:url" />\
<meta content="https://charlesaverill.github.io/GBCamel/media/logo_thin.png" property="og:image" />\
<meta content="#2d3192" data-react-helmet="true" name="theme-color" />
endef

cleandocs:
	if [ ! -d $(DOCS_PATH) ]; then \
		mkdir $(DOCS_PATH); \
	fi
	rm -rf $(DOCS_PATH)module $(DOCS_PATH)docs $(DOCS_PATH)odoc.support $(DOCS_PATH)index.html

docs: cleandocs build
	opam exec -- dune build @doc
	mv -f _build/default/_doc/_html/* $(DOCS_PATH)
	rm -f $(DOCS_PATH)index.html
	mv $(DOCS_PATH)gbcamel/gbcamel.html $(DOCS_PATH)index.html
	mv $(DOCS_PATH)gbcamel $(DOCS_PATH)module
	
	@echo "Preparing Index\n--------------"
	# Header
	sed -i 's/<title>.*<\/title>/<title>$(DOCS_INDEX_TITLE)<\/title>/g' $(DOCS_PATH)index.html
	sed -i 's@</head>@<link rel="icon" href="media/favicon.ico" type="image/x-icon">\n</head>@g' $(DOCS_PATH)index.html
	sed -i 's@</head>@$(DOCS_EMBED)\n</head>@g' $(DOCS_PATH)index.html
	sed -i 's/..\/odoc.support/odoc.support/g' $(DOCS_PATH)index.html
	# Body
	sed -i 's@<nav class="odoc-nav">.*gbcamel</nav>@@g' $(DOCS_PATH)index.html

push: cleandocs build
	@read -p "Commit message: " input; \
	if [ -z "$input" ]; then \
		echo "Error: Please provide a valid commit message."; \
		exit 1; \
	fi; \
	git add . && git commit -m "$input" && git push origin main
