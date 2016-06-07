/* Make an ocaml value from a Tinyocaml.t and vice versa */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdio.h>

/*

type t =
  Unit                            <-- Val_int(0).
| Int of int                      <-- Block with tag 0.
| Bool of bool                    <-- Block with tag 1.
| Float of float                  <-- Block with tag 2.
| String of string                <-- Block with tag 3.
| OutChannel of out_channel       <-- Block with tag 4.
| InChannel of in_channel         <-- Block with tag 5.
| Record of (string * t ref) list <-- Block with tag 6
| Tuple of t list                 <-- Block with tag 7
| Cons of (t * t)                 <-- Block with tag 8
| Nil                             <-- Val_int(1).
*/

value to_ocaml_value(value);

/* Read a list into the appropriately-sized block out */
void read_list(value list, value out)
{
  int p = 0;
  while (list != Val_int(0)) {
    Store_field(out, p, to_ocaml_value (Field(list, 0)));
    p++;
    list = Field(list, 1);
  }
}

/* Length of a list */
int length_of_list(value l)
{
   int len = 0;
   while (l != Val_int(0)) {len++; l = Field(l, 1);}
   return len;
}

/* Read a record */
void read_record(value record, value out)
{
   int p = 0;
   while (record != Val_int(0)) 
   {
     Store_field(out, p, to_ocaml_value (Field(Field(Field(record, 0), 1), 0)));
     p++;
     record = Field(record, 1);
   }
}

CAMLprim value to_ocaml_value(value t)
{
  CAMLparam1(t);
  value out;
  if (t == Val_int(0)) out = Val_unit; /* Unit */
  if (t == Val_int(1)) out = Val_unit; /* Nil */
  if (Is_block(t) && Tag_val(t) < 6) out = Field(t, 0); /* Int, Bool, Float, String, OutChannel, InChannel */
  /* Records */
  if (Is_block(t) && Tag_val(t) == 6)
  {
    out = caml_alloc_tuple(length_of_list (Field(t, 0)));
    read_record (Field(t, 0), out);
  }
  /* Tuples */
  if (Is_block(t) && Tag_val(t) == 7)
  {
    out = caml_alloc_tuple(length_of_list (Field(t, 0)));
    read_list (Field(t, 0), out);
  }
  /* Lists */
  if (Is_block(t) && Tag_val(t) == 8)
  {
    out = caml_alloc(2, 0);
    Store_field(out, 0, to_ocaml_value (Field(Field(t, 0), 0)));
    Store_field(out, 1, to_ocaml_value (Field(Field(t, 0), 1)));
  }
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
