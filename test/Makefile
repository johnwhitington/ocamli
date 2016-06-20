MODS =

SOURCES = test.ml $(foreach x,$(MODS),$(x).ml $(x).mli)

PACKS = unix

RESULT = test

OCAMLNCFLAGS = -safe-string -g -w -3
OCAMLBCFLAGS = -safe-string -g -w -3
OCAMLLDFLAGS = -g

all : byte-code

clean ::
	rm -rf doc foo foo2 a.out test.top

-include OCamlMakefile

