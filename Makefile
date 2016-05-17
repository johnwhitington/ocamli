SOURCES = evalutils.mli evalutils.ml tinyocaml.mli tinyocaml.ml tinyocamlUtils.mli tinyocamlUtils.ml pptinyocaml.mli pptinyocaml.ml core.mli core.ml environment.mli environment.ml eval.ml ocamli.ml

PACKS = unix compiler-libs.common

RESULT = ocamli

OCAMLNCFLAGS = -safe-string -g -w -3
OCAMLBCFLAGS = -safe-string -g -w -3
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library 

clean ::
	rm -rf doc foo foo2 a.out eval.top

install : libinstall

-include OCamlMakefile

