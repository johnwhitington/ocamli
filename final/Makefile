OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = ocamliutil tinyocaml ocamli2print ocamli2type ocamli2read ocamli2write ocamli2eval

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

