SOURCES = evalutils.mli evalutils.ml tinyocaml.mli tinyocaml.ml tinyocamlUtils.mli tinyocamlUtils.ml pptinyocaml.mli pptinyocaml.ml core.mli core.ml tinyexamples.mli tinyexamples.ml naive.mli naive.ml naiveSimple.mli naiveSimple.ml naiveSimpleOneStep.mli naiveSimpleOneStep.ml environment.mli environment.ml eval.ml

PACKS = unix compiler-libs.common

RESULT = eval

OCAMLNCFLAGS = -safe-string -annot -g -w -3
OCAMLBCFLAGS = -safe-string -annot -g -w -3
OCAMLLDFLAGS = -g

all : native-code

clean ::
	rm -rf doc foo foo2 a.out

-include OCamlMakefile

