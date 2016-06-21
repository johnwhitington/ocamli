(* For writing evaluators with *)
open Parsetree
open Asttypes

let typecheck = ref true

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let env =
  Compmisc.init_path false;
  Compmisc.initial_env ()

let ast code =
  let ast = code |> Lexing.from_string |> Parse.implementation in
    if not !typecheck then ast else
      let _, _ = Typemod.type_implementation "example.ml" "" "example" env ast in
        ast

type 'a result =
    Next of 'a
  | IsValue
  | Malformed of string
  | Unimplemented of string

exception ExnUnimplemented of string
exception ExnMalformed of string

type last_op =
    Arith
  | Boolean
  | Comparison
  | IfBool
  | InsidePervasive 

let unimp s = raise (ExnUnimplemented s)
let malformed s = raise (ExnMalformed s)

let to_string ast =
  let b = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer b in
    Pprintast.expression formatter ast;
    Format.pp_print_flush formatter ();
    Buffer.contents b

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

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}]
| [{pstr_desc =
     Pstr_value
       (Nonrecursive, [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}])}] -> e
| _ -> failwith "Not a single structure item"

let namestarred s =
  String.length s > 7 && String.sub s 0 7 = "__PER__"

let unstar s =
  if String.length s > 7 && String.sub s 0 7 = "__PER__"
    then String.sub s 7 (String.length s - 7)
    else s

let rec option_map f = function
  | [] -> []
  | h::t ->
      match f h with
        None -> option_map f t
      | Some x -> x::option_map f t

