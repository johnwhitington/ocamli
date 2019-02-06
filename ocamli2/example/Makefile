#This makefile not in use. We need to call ocaml directly to get tppx to work since ocamlfind doesn't know about it.
#BUT THIS MAKEFILE CAN BE USED FOR CLEANING WITH "make clean"
OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

#This is a normal makefile, but we specify ppx_interpret to
#make debugging happen. We then annotate the file b.ml with [@interpret].
#These two changes are all that is required to debug a module
SOURCES = ccode.c a.mli a.ml b.mli b.ml c.ml

#This will include the interpreter.
PACKS = ocamli2

RESULT = example

#OCAMLNCFLAGS = -tppx ../tppx_interpret/tppx_interpret
#OCAMLBCFLAGS = -tppx ../tppx_interpret/tppx_interpret

CFLAGS = -fPIC

all : native-code

clean ::
	rm -rf doc foo foo2

-include OCamlMakefile

