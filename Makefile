# state accountManager

MODULES=account orderBook matchingEngine accountManager accountManager dao
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
SERVER=exchangeServer.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,oUnit,str,qcheck,cohttp,cohttp-lwt-unix,thread

default: build
	utop

rebuild: clean build

build:
	$(OCAMLBUILD) $(OBJECTS)

main:
	$(OCAMLBUILD) $(MAIN)

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

server:
	$(OCAMLBUILD) $(SERVER)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

check:
	bash checkenv.sh

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out
	
docs: docs-public docs-private

zip:
	zip nasdaq.zip *.ml* _tags .gitignore .ocamlinit .merlin Makefile INSTALL.md
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

# TODO update clean
clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report bisect*.out
