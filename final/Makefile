OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = ocamli2util tinyocaml ocamli2type ocamli2print ocamli2read ocamli2eval

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) ocamli2.ml

PACKS = compiler-libs.common

RESULT = ocamli2

OCAMLNCFLAGS = -safe-string -g -w -3 -w -40
OCAMLBCFLAGS = -safe-string -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code

clean ::
	rm -rf doc foo foo2 a.out foo.cmi foo.cmo

-include OCamlMakefile

