OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = evalutils tinyocaml tinyocamlUtils pptinyocaml core environment eval ocamli

SOURCES = ocamlival.h ocamlival.c $(foreach x,$(MODS),$(x).ml $(x).mli)

PACKS = unix compiler-libs.common

RESULT = ocamli

OCAMLNCFLAGS = -safe-string -g -w -3
OCAMLBCFLAGS = -safe-string -g -w -3
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library 

clean ::
	rm -rf doc foo foo2 a.out eval.top

LIBINSTALL_FILES = ocamli.a ocamli.cma ocamli.cmxa libocamli_stubs.a \
dllocamli_stubs.* $(foreach x,$(MODS),$x.mli) \
$(foreach x,$(MODS),$x.cmi) $(foreach x,$(MODS),$x.cmx)

install : libinstall

-include OCamlMakefile

