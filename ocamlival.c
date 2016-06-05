/* Make an ocaml value from a Tinyocaml.t */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/*

type t =
  Unit          <-- Val_int(0)
| Int of int    <-- Block with tag 0

*/
CAMLprim value to_ocaml_value(value t)
{
  CAMLparam1(t);
  value out = Field(t, 0);
  CAMLreturn(out);
}
