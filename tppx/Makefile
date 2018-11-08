OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

SOURCES = tppx_interpret.ml

PACKS = bigarray ppx_tools ocamli

RESULT = tppx_interpret

OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

all : native-code

install :
	ocamlfind install tppx_interpret META tppx_interpret

-include OCamlMakefile

