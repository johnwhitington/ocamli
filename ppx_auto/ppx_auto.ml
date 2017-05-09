open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* Make a list of characters from a string, preserving order. *)
let explode s =
  let l = ref [] in
    for p = String.length s downto 1 do
      l := String.unsafe_get s (p - 1)::!l
    done;
    !l

(* Make a string from a list of characters, preserving order. *)
let implode l =
  let s = Bytes.create (List.length l) in
    let rec list_loop x = function
       [] -> ()
     | i::t -> Bytes.unsafe_set s x i; list_loop (x + 1) t
    in
      list_loop 0 l;
      Bytes.to_string s

(* Parse {|external word_size : unit -> int = "%word_size"|} returning:
  
  a) n = "word_size"
  b) t_in = "Unit"
  c) t_out = "Int"
  d) nstr = "%word_size" *)
let name = ref 'x'

let incrname () =
  name := char_of_int (int_of_char !name + 1)

let string_of_char c =
  let s = String.create 1 in
    String.unsafe_set s 0 c;
    s

let conv_type isin t =
  let addx s = if isin then s ^ " " ^ (string_of_char !name) else s in
    match t with
    {ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, _)} -> "Unit"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, _)} -> addx "Int"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "bool"}, _)} -> addx "Bool"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "int32"}, _)} -> addx "Int32"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "int64"}, _)} -> addx "Int64"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "float"}, _)} -> addx "Float"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "char"}, _)} -> addx "Char"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "nativeint"}, _)} -> addx "NativeInt"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "in_channel"}, _)} -> addx "InChannel"
  | {ptyp_desc = Ptyp_constr ({txt = Lident "out_channel"}, _)} -> addx "OutChannel"
  | {ptyp_desc = Ptyp_constr ({txt = Lident ("string" | "bytes")}, _)} -> addx "String"
  | {ptyp_desc = Ptyp_var "a"} -> "x"
  | _ -> failwith "conv_type"

let constr_of_type = function
  "Unit" -> "()"
| _ -> string_of_char !name

let parse_type = function
  {ptyp_desc = Ptyp_arrow (_, a, {ptyp_desc = Ptyp_arrow (_, b, c)})} ->
    let a_t = conv_type true a in
    let a_t' = constr_of_type a_t in
      incrname ();
    let b_t = conv_type true b in
    let c_t = conv_type false c in
    [(a_t, a_t'); (b_t, constr_of_type b_t)], c_t
| {ptyp_desc = Ptyp_arrow (_, a, b)} ->
    let a_t = conv_type true a in
    let b_t = conv_type false b in
    [(a_t, constr_of_type a_t)], b_t
| _ -> failwith "parse_type: unknown type"

let parse_type t =
  name := 'x';
  parse_type t

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

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}] -> e 
| _ -> failwith "getexpr: Not a single structure item"

let ast s =
  Parse.implementation (Lexing.from_string s)

let convert_nstr s =
  match explode s with
    '%'::t -> "percent_" ^ implode t
  | x -> s

(* Build the auto itself. *)
let mkmk arity nstr =
  let str = match arity with
    1 ->
      Printf.sprintf
         {|("%s", Fun (NoLabel, PatVar "*x", CallBuiltIn (None, "%s", [Var "*x"], f), []))|}
         nstr nstr
  | 2 ->
      Printf.sprintf
        {|("%s", Fun (NoLabel, PatVar "*x", Fun (NoLabel, PatVar "*y", CallBuiltIn (None, "%s", [Var "*x"; Var "*y"], f), []), []))|}
        nstr nstr
  | _ -> failwith "mkmk: unknown arity"
  in
    getexpr (ast str) 

let build_auto tins t_out n nstr =
  let thetuple = mkmk (List.length tins) nstr in
  let f =
    match tins with
      [] -> failwith "build_auto: no input types"
    | [(t_in, t_in')] ->
        let out =
          if t_out = "Unit"
            then Printf.sprintf "%s %s; Unit" n t_in'
            else Printf.sprintf "%s (%s %s)" t_out n t_in'
        in
          getexpr
            (ast
              (Printf.sprintf
                 {|function [%s] -> %s | _ -> failwith "%s"|}
                 t_in out nstr))
    | [(t_in, t_in'); (t_in2, t_in2')] ->
        let out =
          if t_out = "Unit"
            then Printf.sprintf "%s %s %s; Unit" n t_in' t_in2'
            else Printf.sprintf "%s (%s %s %s)" t_out n t_in' t_in2'
        in
          getexpr
            (ast
              (Printf.sprintf
                 {|function [%s; %s] -> %s | _ -> failwith "%s"|}
                 t_in t_in2 out nstr))
    | _ -> failwith "build_auto: too many input types"
  in
    let theletfin =
      Exp.let_ Nonrecursive [Vb.mk (Pat.var {txt = "f"; loc = Location.none}) f] thetuple
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

