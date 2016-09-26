SOURCES = lexer.mll parser.mly main.ml

REAL_OCAMLFIND = ocamlfind

RESULT = main

OCAMLNCFLAGS = -safe-string -g -w -3 -w -40
OCAMLBCFLAGS = -safe-string -g -w -3 -w -40
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library 

-include OCamlMakefile

