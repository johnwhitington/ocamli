open Tinyocaml
open Parsetree
open Asttypes

external to_ocaml_value : 'a t' -> 'a = "to_ocaml_value"

