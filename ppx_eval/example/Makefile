SOURCES = example.ml

#Uncomment this, and remove -ppx ../ppx_blob below to use the installed version
#PACKS = ppx_blob

RESULT = example

OCAMLNCFLAGS = -g -w -3 -ppx ../ppx_eval
OCAMLBCFLAGS = -g -w -3 -ppx ../ppx_eval
OCAMLLDFLAGS = -g

all : native-code

-include OCamlMakefile

