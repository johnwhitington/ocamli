OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

SOURCES = ppx_auto.ml

PACKS = ppx_tools

RESULT = ppx_auto

clean ::
	rm -rf doc foo foo2 a.out eval.top

OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

all : native-code

-include OCamlMakefile

