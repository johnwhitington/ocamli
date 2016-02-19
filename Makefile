SOURCES = evalutils.mli evalutils.ml tinyocaml.mli tinyocaml.ml tinyocamlUtils.mli tinyocamlUtils.ml pptinyocaml.mli pptinyocaml.ml tinyexamples.mli tinyexamples.ml naive.mli naive.ml naiveSimple.mli naiveSimple.ml naiveSimpleOneStep.mli naiveSimpleOneStep.ml environment.mli environment.ml cc.ml scc.ml ck.ml cek.ml secd.ml eval.ml

PACKS = compiler-libs.common

RESULT = eval

OCAMLNCFLAGS = -annot -g -w -3
OCAMLBCFLAGS = -annot -g -w -3
OCAMLLDFLAGS = -g

all : native-code

clean ::
	rm -rf doc foo foo2 a.out

-include OCamlMakefile

