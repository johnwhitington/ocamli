OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

MODS = util type print read eval lib ocamli2

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli)

PACKS = compiler-libs.common

RESULT = ocamli2

OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library

clean ::
	rm -rf doc foo foo2 a.out foo.cmi foo.cmo

LIBINSTALL_FILES = ocamli2.a ocamli2.cma ocamli2.cmxa \
$(foreach x,$(MODS),$x.mli) \
$(foreach x,$(MODS),$x.cmi) $(foreach x,$(MODS),$x.cmx)

install : libinstall

-include OCamlMakefile

