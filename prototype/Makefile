#
# Build system for ocs library and interpreter
#
all: build

OCAMLBUILD = ocamlbuild
METAOCAMLC = metaocamlc
TARGET = normal_parser.byte staging_parser.byte

build:
	$(OCAMLBUILD) -tag annot -tag bin_annot -pkgs genlet,bigarray -ocamlc $(METAOCAMLC) $(TARGET)

clean:
	ocamlbuild -clean
	rm -rf *.cmi *.cmo *.out

.PHONY: byte clean
