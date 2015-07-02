#
# Build system for ocs library and interpreter
#
all: build

OCAMLBUILD = ocamlbuild
METAOCAMLC = metaocamlc
TARGET = nparser.byte sparser.byte

build:
	$(OCAMLBUILD) -tag annot -tag bin_annot -pkgs genlet,bigarray -ocamlc $(METAOCAMLC) $(TARGET)

clean:
	ocamlbuild -ocamlc $(METAOCAMLC) -clean
	rm -rf *.cmi *.cmo *.out *.byte

.PHONY: byte clean
