open Ast_helper
open Ast_mapper
open Parsetree
open Longident
open Ppx_auto_util

(* Parse {|external word_size : unit -> int = "%word_size"|} returning:
  
  a) n = "word_size" b) t_in = "Unit" c) t_out = "Int" d) nstr = "%word_size" *)
let name = ref 'a'

let incrname () =
  name := char_of_int (int_of_char !name + 1)

let conv_type isin t =
  let addx s = if isin then s ^ " " ^ (string_of_char !name) else s in
    match t.ptyp_desc with
    Ptyp_constr ({txt = Lident "unit"}, _) -> "Tinyocaml.Unit"
  | Ptyp_constr ({txt = Lident "int"}, _) -> addx "Tinyocaml.Int"
  | Ptyp_constr ({txt = Lident "bool"}, _) -> addx "Tinyocaml.Bool"
  | Ptyp_constr ({txt = Lident "int32"}, _) -> addx "Tinyocaml.Int32"
  | Ptyp_constr ({txt = Lident "int64"}, _) -> addx "Tinyocaml.Int64"
  | Ptyp_constr ({txt = Lident "float"}, _) -> addx "Tinyocaml.Float"
  | Ptyp_constr ({txt = Lident "char"}, _) -> addx "Tinyocaml.Char"
  | Ptyp_constr ({txt = Lident "nativeint"}, _) -> addx "Tinyocaml.NativeInt"
  | Ptyp_constr ({txt = Lident "in_channel"}, _) -> addx "Tinyocaml.InChannel"
  | Ptyp_constr ({txt = Lident "out_channel"}, _) -> addx "Tinyocaml.OutChannel"
  | Ptyp_constr ({txt = Lident ("bytes")}, _) -> addx "Tinyocaml.String"
  | Ptyp_constr ({txt = Lident ("string")}, _) ->
      if isin
        then "Tinyocaml.String" ^ " " ^ (string_of_char !name)
        else "Tinyocaml.String (Bytes.of_string"
  | Ptyp_var "a" -> if isin then "a" else ""
  | _ -> failwith "conv_type"

let constr_of_type = function
  "Tinyocaml.Unit" -> "()"
| x when String.length x >= 16 && String.sub x 0 16 = "Tinyocaml.String" ->
    (*Printf.printf "NOTICED IT**************************\n";
    flush stdout;*)
    "(Bytes.to_string " ^ string_of_char !name ^ ")"
| x -> (*Printf.printf "constr_of_type: %s\n" x;*) string_of_char !name

(* Extract a -> b -> c -> d to ([a; b; c], d) etc. *)
let rec find_types t =
  match t.ptyp_desc with
   Ptyp_arrow (_, a, b) -> let (l, e) = find_types b in (a::l, e)
 | _ -> ([], t)

(* For an output of [find_types], produce the (t_in, t_in') pairs, and the t_out *)
let convert_types (l, e) =
  (List.fold_left (* Rather than map, to enforce left to right ordering on incrname calls *)
    (fun a i ->
      let r =
        let t_in = conv_type true i in
        let t_in' = constr_of_type t_in in
          (t_in, t_in')
      in
        incrname (); a @ [r])
    [] l,
   conv_type false e)

let parse_type typ =
  name := 'a';
  convert_types (find_types typ)

let parse_external e =
  match Parse.implementation (Lexing.from_string e) with
    [{pstr_desc =
        Pstr_primitive
          {pval_name = {txt = funname};
           pval_type;
           pval_prim = [primname]}} as structure_item] ->
      let tins, t_out = parse_type pval_type in
        (structure_item, funname, tins, t_out, primname)
  | _ -> failwith "parse_external"

let convert_nstr s =
  match explode s with
    '%'::t -> "percent_" ^ implode t
  | x -> s

(* Build the auto itself. *)
let rec mkvars = function
  0 -> ""
| n ->
    let l = {|Tinyocaml.Var "*|} ^ string_of_char !name ^ {|"; |} in
     incrname ();
     l ^ mkvars (n - 1)

let mkvars x =
  name := 'a';
  "[" ^ mkvars x ^ "]"

let rec make_centre oarity nstr = function
  0 -> {|Tinyocaml.CallBuiltIn (None, "|} ^ nstr ^ {|", |} ^ mkvars oarity ^ {|, f)|}
| n ->
    let l = 
      {|Tinyocaml.Fun (Tinyocaml.NoLabel, Tinyocaml.PatVar "*|} ^ string_of_char !name ^ {|", |}
    in
      incrname ();
      l ^ make_centre oarity nstr (n - 1) ^ {|, [])|}

let mkmk arity nstr =
  name := 'a';
  getexpr (ast ({|("|} ^ nstr ^ {|", |} ^ make_centre arity nstr arity ^ ")"))

let function_of_tins_tout tins t_out nstr n =
  let out =
    if t_out = "Tinyocaml.Unit"
      then n ^ " " ^ List.fold_left (fun a b -> a ^ " " ^ b) "" (List.map snd tins) ^ "; Tinyocaml.Unit"
      else if t_out = "Tinyocaml.String (Bytes.of_string"
      then t_out ^ " (" ^ n ^ " " ^ List.fold_left (fun a b -> a ^ " " ^ b) "" (List.map snd tins) ^ "))"
      else t_out ^ " (" ^ n ^ " " ^ List.fold_left (fun a b -> a ^ " " ^ b) "" (List.map snd tins) ^ ")"
  in
    let ins =
      List.fold_left (fun a b -> a ^ " " ^ b ^";") "" (List.map fst tins)
    in
        {|function env -> function [|} ^ ins ^ {|] -> |}
      ^ "begin try "
      ^ out
      ^ " with e -> exception_from_ocaml e end "
      ^ {|| _ -> raise (Ocamliutil.RuntimeTypeError ("|}
      ^ nstr
      ^ " expected argument types "
      ^ ins
      ^ {|"))|}

let build_auto tins t_out n nstr =
  let thetuple = mkmk (List.length tins) nstr in
  let f = function_of_tins_tout tins t_out nstr n in
    let theletfin =
      Exp.let_ Nonrecursive [Vb.mk (Pat.var {txt = "f"; loc = Location.none}) (getexpr (ast f))] thetuple
    in
      let binding =
        Vb.mk
          (Pat.var {txt = convert_nstr nstr; loc = Location.none})
          theletfin
      in
        Str.mk (Pstr_value (Nonrecursive, [binding]))

(* Two structure items. First, the external, then the let percent_word_size = *)
let build_all str =
  let parsed, n, tins, t_out, nstr = parse_external str in
    [parsed; build_auto tins t_out n nstr]

let auto_mapper argv =
  {default_mapper with
     structure =
       let rec f mapper structure =
         match structure with
           [] -> []
         | {pstr_desc = Pstr_extension (({txt = "auto"},
                        PStr [{pstr_desc =
                          Pstr_eval ({pexp_desc =
                            Pexp_constant (Pconst_string (s, _))}, _)}]), _)}::t ->
               build_all s @ f mapper t
         | h::t ->
             default_mapper.structure_item mapper h::f mapper t
       in
         f}

let _ = register "auto" auto_mapper

