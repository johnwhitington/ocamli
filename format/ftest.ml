(* Simple format module test *)
type t =
  I of int
| M of t * t

let example =
  M (I 1, M (I 2, M (I 3, M (I 4, M (I 5, M (I 6, M (I 7, M (I 8, M (I 9, I 10)))))))))

let rec pr f = function
  I i -> Format.fprintf f "%s" (string_of_int i);
| M (a, b) ->
    pr f a;
    Format.pp_print_text f " * ";
    pr f b

(* Works *)
let std () =
  let f = Format.std_formatter in
  Format.pp_set_margin f 40;
  Format.pp_open_box f 4;
  for _ = 1 to 10 do pr f example done;
  Format.pp_close_box f ();
  Format.pp_print_newline f ()

(* Does not work *)
let str () =
  let f = Format.str_formatter in
  Format.pp_set_margin f 40;
  Format.pp_open_box f 4;
  for _ = 1 to 10 do pr f example done;
  Format.pp_close_box f ();
  Format.pp_print_flush f ();
  print_string (Format.flush_str_formatter ());
  flush stdout

let _ = std (); str ()

