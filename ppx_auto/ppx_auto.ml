open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* Parse {|external word_size : unit -> int = "%word_size"|} returning:
  
  a) n = "word_size"
  b) t_in = "Unit"
  c) t_out = "Int"
  d) nstr = "%word_size" *)
let conv_type = function
  {ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, _)} -> "Unit"
| {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, _)} -> "Int"
| _ -> failwith "conv_type"

let constr_of_type = function
  "Unit" -> "()"
| _ -> failwith "constr_of_type"

let parse_type = function
  {ptyp_desc = Ptyp_arrow (_, a, b)} ->
    let a_t = conv_type a in
    let b_t = conv_type b in
      a_t, constr_of_type a_t, b_t
| _ -> failwith "parse_type: unknown type"

let parse_external e =
  match Parse.implementation (Lexing.from_string e) with
    [{pstr_desc =
        Pstr_primitive
          {pval_name = {txt = funname};
           pval_type;
           pval_prim = [primname]}} as structure_item] ->
      let t_in, t_in', t_out = parse_type pval_type in
        (structure_item, funname, t_in, t_in', t_out, primname)
  | _ -> failwith "parse_external"

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}] -> e 
| _ -> failwith "getexpr: Not a single structure item"

let ast s =
  Parse.implementation (Lexing.from_string s)

(* Build the auto itself:

let percent_word_size =
  let f =
    (function [Unit] -> Int (word_size ())
     | _ -> failwith "%word_size")
  in
    ("%word_size", Fun (NoLabel, PatVar "*x", CallBuiltIn (None, "%word_size", [Var "*x"], f), []))
*)
let build_auto t_in t_in' t_out n nstr =
  let theletfin =
    let thetuple =
      getexpr
        (ast
          (Printf.sprintf
             {|("%s", Fun (NoLabel, PatVar "*x", CallBuiltIn (None, "%s", [Var "*x"], f), []))|}
             nstr nstr))
    in
      let f =
        getexpr
          (ast
            (Printf.sprintf
               {|function [%s] -> %s (%s %s) | _ -> failwith "%s"|}
               t_in t_out n t_in' nstr))
      in
        Exp.let_ Nonrecursive [Vb.mk (Pat.var {txt = "f"; loc = Location.none}) f] thetuple
  in
    let binding =
      Vb.mk
        (Pat.var {txt = "percent_word_size"; loc = Location.none})
        theletfin
    in
      Str.mk (Pstr_value (Nonrecursive, [binding]))

(* Two structure items. First, the external, then the let percent_word_size = *)
let build_all str =
  let parsed, n, t_in, t_in', t_out, nstr = parse_external str in
    [parsed; build_auto t_in t_in' t_out n nstr]

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

