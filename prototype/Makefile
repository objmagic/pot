all: clean build

OCAMLBUILD = ocamlbuild
OCAMLC = metaocamlc
TARGET = codemap.byte nparser.byte sparser.byte fparser.byte test_parser_generator.byte
TESTTARGET = gltest.byte
TAGS = annot,bin_annot
PKGS = bigarray,genlet

build:
	$(OCAMLBUILD) -tag $(TAGS) -pkgs $(PKGS) -ocamlc $(OCAMLC) $(TARGET)

test:
	$(OCAMLBUILD) -tag $(TAGS) -pkgs $(PKGS) -ocamlc $(OCAMLC) $(TESTTARGET)

clean:
	ocamlbuild -ocamlc $(METAOCAMLC) -clean
	rm -rf *.cmi *.cmo *.out *.byte _build

.PHONY: byte clean test
