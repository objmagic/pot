all: clean build

OCAMLBUILD = ocamlbuild
OCAMLC = metaocamlc
TARGET = nparser.byte sparser.byte tmap.byte fparser.byte
TAGS = annot,bin_annot
PKGS = bigarray,genlet

build:
	$(OCAMLBUILD) -tag $(TAGS) -pkgs $(PKGS) -ocamlc $(OCAMLC) $(TARGET)

clean:
	ocamlbuild -ocamlc $(METAOCAMLC) -clean
	rm -rf *.cmi *.cmo *.out *.byte

.PHONY: byte clean
