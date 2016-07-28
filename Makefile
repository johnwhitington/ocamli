OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = ocamliutil tinyocaml tinyocamlutil pptinyocaml ocamliprim eval tinyocamlrw ocamlilib runeval ocamli

SOURCES = ocamlival.h ocamlival.c $(foreach x,$(MODS),$(x).ml $(x).mli)

PACKS = graphics unix bigarray compiler-libs.common

RESULT = ocamli

OCAMLNCFLAGS = -safe-string -g -w -3 -w -40
OCAMLBCFLAGS = -safe-string -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library 

clean ::
	rm -rf doc foo foo2 a.out eval.top

LIBINSTALL_FILES = ocamli.a ocamli.cma ocamli.cmxa libocamli_stubs.a \
dllocamli_stubs.* $(foreach x,$(MODS),$x.mli) \
$(foreach x,$(MODS),$x.cmi) $(foreach x,$(MODS),$x.cmx)

install : libinstall

-include OCamlMakefile

