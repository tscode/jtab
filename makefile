ENV="opam config exec"

all: js/jtab.js bin/jtab

run: js/jtab.js bin/jtab
	./bin/jtab

js/jtab.js: client/*.ml lib/*.ml
	mkdir -p js
	dune build --release client/main.bc.js
	cp _build/default/client/main.bc.js js/jtab.js

bin/jtab: server/*.ml lib/*.ml
	mkdir -p bin
	dune build server/main.exe
	cp _build/default/server/main.exe bin/jtab
