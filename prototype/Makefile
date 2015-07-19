all: clean build

OCAMLBUILD = ocamlbuild
OCAMLC = metaocamlc
TARGET = codemap.byte nparser.byte sparser.byte
TAGS = annot,bin_annot
PKGS = bigarray,genlet

build:
	$(OCAMLBUILD) -tag $(TAGS) -pkgs $(PKGS) -ocamlc $(OCAMLC) $(TARGET)

clean:
	ocamlbuild -ocamlc $(METAOCAMLC) -clean
	rm -rf *.cmi *.cmo *.out *.byte

.PHONY: byte clean
