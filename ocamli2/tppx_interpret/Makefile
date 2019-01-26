OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

SOURCES = tppx_interpret.ml

PACKS = ocamli2 compiler-libs.common

RESULT = tppx_interpret

OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

all : native-code

clean ::
	rm -rf doc a.out *.o *.cmx *.cmi *.cmo

-include OCamlMakefile

