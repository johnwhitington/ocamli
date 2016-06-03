OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

SOURCES = ppx_eval.ml

PACKS = ppx_tools ocamli

RESULT = ppx_eval

OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

all : native-code

install :
	ocamlfind install ppx_blob META ppx_blob

-include OCamlMakefile

