(* For writing evaluators with *)
open Parsetree
open Asttypes
open Tinyocaml

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

let ast_to_string ast =
  let b = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer b in
    Pprintast.expression formatter ast;
    Format.pp_print_flush formatter ();
    Buffer.contents b

let with_desc x =
  {pexp_desc = x;
   pexp_loc = Location.none;
   pexp_attributes = []}

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}]
| [{pstr_desc =
     Pstr_value
       (Nonrecursive, [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}])}] -> e
| _ -> failwith "Not a single structure item"

let isstarred s =
  String.length s > 1 && String.sub s 0 1 = "*"

let star s = "*" ^ s

let unstar s =
  if String.length s > 1 && String.sub s 0 1 = "*"
    then String.sub s 1 (String.length s - 1)
    else s

let rec option_map f = function
  | [] -> []
  | h::t ->
      match f h with
        None -> option_map f t
      | Some x -> x::option_map f t

(* Opening a module Find any items in the environment beginning with 'n', strip the name, and
duplicate them at the top of the environment. *)
let begins_with n s =
  String.length n <= String.length s &&
  n = String.sub s 0 (String.length n)

let rec pattern_begins_with n = function
  PatVar s when begins_with n s -> true
| PatTuple ts when List.for_all (pattern_begins_with n) ts -> true
| _ -> false

let binding_begins_with n (p, e) =
  pattern_begins_with n p

let bindings_beginning_with n env =
  option_map
    (fun (recflag, bindings) ->
      if List.for_all (binding_begins_with n) !bindings
        then Some (recflag, bindings)
        else None)
    env

let cut n s =
  String.sub s (String.length n + 1) (String.length s - String.length n - 1)

let rec strip_pattern n = function
  PatVar s -> PatVar (cut n s)
| PatTuple ts -> PatTuple (List.map (strip_pattern n) ts)
| _ -> failwith "implement Ocamliutil.strip_pattern"

let strip_binding n (p, e) = (strip_pattern n p, e)

let strip_bindings n (recflag, bs) =
  (recflag, ref (List.map (strip_binding n) !bs))

let open_module n env =
  List.map (strip_bindings n) (bindings_beginning_with n env) @ env

