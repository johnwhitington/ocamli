/* The implementation file for the C portion */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <stdio.h>

CAMLprim value c_function(value x)
{
  CAMLparam1(x);
  CAMLlocal1(y);
  y = caml_callback(*caml_named_value("double"), x);
  CAMLreturn(y);
}

