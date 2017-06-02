/* Make an ocaml value from a Tinyocaml.t and vice versa */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdio.h>

/*
type t =
  Unit                                 Val_int(0)
| Int of int                           Block with tag 0
| Bool of bool                         Block with tag 1
| Float of float                       Block with tag 2
| String of string                     Block with tag 3
| OutChannel of out_channel            Block with tag 4
| InChannel of in_channel              Block with tag 5
| Record of (string * t ref) list      Block with tag 6
| Tuple of t list                      Block with tag 7
| Cons of (t * t)                      Block with tag 8
| Nil                                  Val_int(1)
| Int32 of Int32.t                     Block with tag 9
| Int64 of Int64.t                     Block with tag 10
| NativeInt of Nativeint.t             Block with tag 11
| Char of char                         Block with tag 12
| Array of t array                     Block with tag 13
| Constr of int * string * t option    Block with tag 14
| Fun of (label * pattern * t * env)   Block with tag 15
| Function of (case list * env)        Block with tag 16
*/

value to_ocaml_value(value);
value untyped_of_ocaml_value(value);

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

/* Read an array */
void read_array(value arr, value out)
{
  for (int p = 0; p < Wosize_val(out); p++)
  {
    Store_field(out, p, to_ocaml_value (Field(arr, p)));
  }
}

/* Read a float array */
void read_float_array(value arr, value out)
{
  for (int p = 0; p < Wosize_val(out); p++)
  {
    Store_double_field(out, p, Double_val(Field(Field(arr, p), 0)));
  }
}

CAMLprim value to_ocaml_value(value t)
{
  CAMLparam1(t);
  CAMLlocal1(out);
  int done = 0;
  if (t == Val_int(0))
  {out = Val_unit; /* Unit */
   done = 1;
  }
  if (t == Val_int(1))
  {out = Val_unit; /* Nil */
   done = 2;
  }
  /* Records */
  if (Is_block(t) && Tag_val(t) == 6)
  {
    out = caml_alloc_tuple(length_of_list (Field(t, 0)));
    read_record (Field(t, 0), out);
    done = 4;
  }
  /* Tuples */
  if (Is_block(t) && Tag_val(t) == 7)
  {
    out = caml_alloc_tuple(length_of_list (Field(t, 0)));
    read_list (Field(t, 0), out);
    done = 5;
  }
  /* Arrays */
  if (Is_block(t) && Tag_val(t) == 13)
  {
    /* Detect a float array - first elt has tag 2 (Float f) */
    if (Wosize_val(Field(t, 0)) > 0 && Tag_val(Field(Field(t, 0), 0)) == 2)
    {
      printf("this is A FLOAT ARRAY\n");
      fflush(stdout);
      out = caml_alloc_float_array(Wosize_val((Field(t, 0))));
      read_float_array(Field(t, 0), out);
      done = 9;
    }
    else
    {
      printf("this is NOT a float array\n");
      fflush(stdout);
      out = caml_alloc_tuple(Wosize_val((Field(t, 0))));
      read_array (Field(t, 0), out);
      done = 8;
    }
  }
  /* Lists */
  if (Is_block(t) && Tag_val(t) == 8)
  {
    out = caml_alloc(2, 0);
    Store_field(out, 0, to_ocaml_value (Field(Field(t, 0), 0)));
    Store_field(out, 1, to_ocaml_value (Field(Field(t, 0), 1)));
    done = 6;
  }
  /* Constructors. Just Some and None as examples for now. Later we will look up in the Constr to get the tag number. */
  if (Is_block(t) && Tag_val(t) == 14)
  {
    if (Field(t, 2) == Val_int(0))
    {
      out = Val_int(0); 
      done = 10;
    }
    else
    {
      out = caml_alloc(1, 0);
      Store_field(out, 0, to_ocaml_value (Field(Field(t, 2), 0)));
      done = 11;
    }
  }
  /* Int32, Int64, Nativeint, Char,Int, Bool, Float, String, OutChannel, InChannel */
  if (Is_block(t) && Tag_val(t) < 13 && done == 0)
  {out = Field(t, 0);
   done = 7;
  }
  if (done == 0)
  {
    printf("to_ocaml_value: not handled\n");
    fflush(stdout);
  }
  else
  {
    printf("to_ocaml_value: made %i\n", done);
    fflush(stdout);
  }
  CAMLreturn(out);
}

/*
  type untyped_ocaml_value =
  UInt of int                                <-- Block with tag 0
| UBlock of tag * untyped_ocaml_value array  <-- Block with tag 1
| UString of string                          <-- Block with tag 2
| UDouble of float                           <-- Block with tag 3
| UDoubleArray of float array                <-- Block with tag 4
*/

/* Read a Tinyocaml.untyped_ocaml_value from an ocaml heap value */
CAMLprim value untyped_of_ocaml_value(value t)
{
  int done = 0;
  setbuf(stdout, NULL);
  CAMLparam1(t);
  CAMLlocal2(out, arr);
  if (Is_long(t))
  {
    out = caml_alloc(1, 0);
    Store_field(out, 0, t);
    done = 1;
  }
  if (Is_block(t) && Tag_val(t) < No_scan_tag)
  {
    out = caml_alloc(2, 1);
    Store_field(out, 0, Tag_val(t));
    arr = caml_alloc_tuple(Wosize_val(t));
    Store_field(out, 1, arr);
    int p;
    for(p = 0; p < Wosize_val(t); p++)
      Store_field(arr, p, untyped_of_ocaml_value (Field(t, p)));
    done = 1;
  }
  if (Is_block(t) && Tag_val(t) == String_tag)
  {
    out = caml_alloc(1, 2);
    Store_field(out, 0, t);
    done = 1;
  }
  if (Is_block(t) && Tag_val(t) == Double_tag)
  {
    out = caml_alloc(1, 3);
    Store_field(out, 0, t);
    done = 1;
  }
  if (Is_block(t) && Tag_val(t) == Double_array_tag)
  {
    out = caml_alloc(1, 4);
    Store_field(out, 0, t);
    done = 1;
  }
  if (done == 0)
  {
    printf("untyped_of_ocaml_value: not handled\n");
    fflush(stdout);
  }
  CAMLreturn(out);
}
