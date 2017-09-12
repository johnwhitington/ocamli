OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = nanocaml nanocamlrw ocamli2

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli)

PACKS = bigarray ocamli compiler-libs.common

RESULT = ocamli2

OCAMLNCFLAGS = -safe-string -g -w -3 -w -40
OCAMLBCFLAGS = -safe-string -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code byte-code-library

clean ::
	rm -rf doc foo foo2 a.out

-include OCamlMakefile

