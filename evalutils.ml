(* For writing evaluators with *)
open Parsetree
open Asttypes

type 'a result =
    Next of 'a
  | IsValue
  | Malformed of string
  | Unimplemented of string

exception ExnUnimplemented of string
exception ExnMalformed of string

let unimp s = raise (ExnUnimplemented s)
let malformed s = raise (ExnMalformed s)

let to_string ast =
  let b = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer b in
    Pprintast.expression formatter ast;
    Format.pp_print_flush formatter ();
    Buffer.contents b

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}]
| [{pstr_desc =
     Pstr_value
       (Nonrecursive, [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}])}] -> e
| _ -> failwith "Not a single structure item"

let makestructure e =
  [{pstr_desc = Pstr_eval (e, []); pstr_loc = Location.none}]

let with_desc x =
  {pexp_desc = x;
   pexp_loc = Location.none;
   pexp_attributes = []}

let bool_of_bool_value e =
  match e.pexp_desc with
  | Pexp_construct
      ({txt = Longident.Lident (("true" | "false") as b)}, None) ->
        bool_of_string b
  | _ -> malformed __LOC__

let mkbool b =
  with_desc
    (Pexp_construct
      ({txt = Longident.Lident (string_of_bool b); loc = Location.none}, None))

