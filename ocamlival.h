/* Make an ocaml value from a Tinyocaml.t */
#include <caml/mlvalues.h>

value to_ocaml_value(value t);
value untyped_of_ocaml_value(value t);

