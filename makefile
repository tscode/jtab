ENV=$(shell opam env)

all: env js/jtab.js bin/jtab

run: env js/jtab.js bin/jtab
	./bin/jtab

js/jtab.js: env bin/client.ml lib/*.ml
	mkdir -p js
	dune build --release bin/client.bc.js
	cp _build/default/bin/client.bc.js js/jtab.js

bin/jtab: env bin/server.ml bin/web.ml bin/debug.ml lib/*.ml
	dune build bin/server.exe
	cp _build/default/bin/server.exe bin/jtab

env:
	$(ENV)
