MODULES=main coursedata makeschedule
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,yojson
RAW-JSON=raw_data.json
TIDY-JSON=data.json
CONFIG=config.json

SEMESTER=FA19
SUBJECTS=CS MATH
ALL-SUBJECTS=$(shell python3 get_json.py subjects -s FA19 | sed -nE 's/[ ]+"value": "([A-Z]+)",/\1 /pg')

default: build
	utop

schedule:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

raw-json:
	python3 get_json.py --output $(RAW-JSON) courses $(SUBJECTS) --semester $(SEMESTER)

raw-json-all:
	python3 get_json.py --output $(RAW-JSON) courses $(ALL-SUBJECTS) --semester $(SEMESTER)

tidy:
	python3 tidy_json.py --input $(RAW-JSON) --config $(CONFIG) --output $(TIDY-JSON)

json: raw-json tidy

json-all: raw-json-all tidy

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

bisect-test:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

bisect: clean bisect-test
	bisect-report -I _build -html report bisect0001.out
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report bisect*.out
