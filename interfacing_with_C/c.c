/* The implementation file for the C portion */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <stdio.h>

CAMLprim value f(value x)
{
  CAMLparam1(x);
  caml_callback(*caml_named_value("write"), Val_unit);
  CAMLreturn(Val_int(Int_val(x) * 2));
}

