OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

SOURCES = ppx_auto_util.ml ppx_auto.ml

PACKS = compiler-libs.common

RESULT = ppx_auto

OCAMLNCFLAGS = -g -w -3 -w -40
OCAMLBCFLAGS = -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code

clean ::
	rm -rf doc foo foo2 a.out eval.top

-include OCamlMakefile

