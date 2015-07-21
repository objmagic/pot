all: clean build

OCAMLBUILD = ocamlbuild
OCAMLC = metaocamlc
TARGET = tmap.byte nparser.byte sparser.byte fparser.byte
TAGS = annot,bin_annot
PKGS = bigarray,genlet

build:
	$(OCAMLBUILD) -tag $(TAGS) -pkgs $(PKGS) -ocamlc $(OCAMLC) $(TARGET)

clean:
	ocamlbuild -ocamlc $(METAOCAMLC) -clean
	rm -rf *.cmi *.cmo *.out *.byte _build

.PHONY: byte clean
