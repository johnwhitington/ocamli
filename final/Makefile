OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = ocamliutil tinyocaml pptinyocaml finaltype

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) final.ml

PACKS = compiler-libs.common

RESULT = final

OCAMLNCFLAGS = -safe-string -g -w -3 -w -40
OCAMLBCFLAGS = -safe-string -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code

clean ::
	rm -rf doc foo foo2 a.out foo.cmi foo.cmo

-include OCamlMakefile

