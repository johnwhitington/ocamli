SOURCES = c.h c.c ocaml.mli ocaml.ml

RESULT = example

OCAMLNCFLAGS = -g -w -3 -w -40
OCAMLBCFLAGS = -g -w -3 -w -40
OCAMLLDFLAGS = -g
CFLAGS = -fPIC

all : native-code

clean ::
	rm -rf foo foo2 a.out

-include OCamlMakefile

