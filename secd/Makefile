OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = secd

SOURCES = secd.ml

PACKS = bigarray ocamli

RESULT = secd

OCAMLNCFLAGS = -safe-string -g -w -3 -w -40
OCAMLBCFLAGS = -safe-string -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library 

clean ::
	rm -rf doc foo foo2 a.out eval.top

install : libinstall

-include OCamlMakefile

