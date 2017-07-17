OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

#This is a normal makefile, but we specify ppx_interpret and ocamli-debugger to
#make debugging happen. We then annotate the file b.ml with [@interpret].
#These 3 changes are all that is required to debug a module
SOURCES = a.mli a.ml b.mli b.ml c.ml

#This will include the interpreter and C support code for it.
PACKS = bigarray ocamli

RESULT = example

OCAMLNCFLAGS = -ppx ../ppx_interpret
OCAMLBCFLAGS = -ppx ../ppx_interpret

CFLAGS = -fPIC

all : native-code

clean ::
	rm -rf doc foo foo2

-include OCamlMakefile

