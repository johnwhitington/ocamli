OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

SOURCES = ppx_eval.ml

PACKS = ppx_tools ocamli bigarray

RESULT = ppx_eval

OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

all : native-code

install :
	ocamlfind install ppx_eval META ppx_eval

-include OCamlMakefile

