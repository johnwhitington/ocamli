/* Make an ocaml value from a Tinyocaml.t */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/*

type t =
  Unit          <-- Val_int(0)
| Int of int    <-- Block with tag 0

*/

/* For now, just int */
CAMLprim value to_ocaml_value(value t)
{
  CAMLparam1(t);
  value out = Field(t, 0);
  CAMLreturn(out);
}

/* For now, just knows how to do int */
CAMLprim value of_ocaml_value(value t)
{
  CAMLparam1(t);
  value out = caml_alloc(1, 0);
  Store_field(out, 0, t);
  CAMLreturn(out);
}
