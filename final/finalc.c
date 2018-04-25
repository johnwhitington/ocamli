/* Support code for new data type */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdio.h>

/* Take an OCaml value representing a float (always boxed here) and give it straight back. */
CAMLprim value magic (value x)
{
  CAMLparam1(x);
  CAMLreturn(x);
}

