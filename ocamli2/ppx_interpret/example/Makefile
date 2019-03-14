OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

#This is a normal makefile, but we specify ppx_interpret to
#make debugging happen. We then annotate the file b.ml with [@interpret].
#These two changes are all that is required to debug a module
SOURCES = ccode.c a.mli a.ml b.mli b.ml c.ml

#This will include the interpreter and C support code for it.
PACKS = bigarray str ocamli

RESULT = example

OCAMLNCFLAGS = -ppx ../ppx_interpret -ppx ../../ppx_auto/ppx_auto
OCAMLBCFLAGS = -ppx ../ppx_interpret -ppx ../../ppx_auto/ppx_auto

CFLAGS = -fPIC

all : native-code

clean ::
	rm -rf doc foo foo2

-include OCamlMakefile

