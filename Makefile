OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = ocamliutil ocamlipat tinyocaml tinyexternal tinyocamlutil pptinyocaml ocamliprim eval tinyocamlrw ocamlilib runeval ocamli

SOURCES = ocamlival.h ocamlival.c $(foreach x,$(MODS),$(x).ml $(x).mli)

PACKS = str unix bigarray compiler-libs.common

RESULT = ocamli

OCAMLNCFLAGS = -g -w -3 -w -40 -ppx ppx_auto/ppx_auto
OCAMLBCFLAGS = -g -w -3 -w -40 -ppx ppx_auto/ppx_auto
OCAMLLDFLAGS = -g
CFLAGS = -fPIC

all : ppx_auto native-code native-code-library byte-code-library 

clean ::
	rm -rf doc foo foo2 a.out eval.top

LIBINSTALL_FILES = ocamli.a ocamli.cma ocamli.cmxa libocamli_stubs.a \
dllocamli_stubs.* $(foreach x,$(MODS),$x.mli) \
$(foreach x,$(MODS),$x.cmi) $(foreach x,$(MODS),$x.cmx)

install : libinstall

-include OCamlMakefile

