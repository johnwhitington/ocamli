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
let name = ref 'a'

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
  | {ptyp_desc = Ptyp_var "a"} -> if isin then "a" else ""
  | _ -> failwith "conv_type"

let constr_of_type = function
  "Unit" -> "()"
| _ -> string_of_char !name

let parse_type = function
  {ptyp_desc = Ptyp_arrow (_, a, {ptyp_desc = Ptyp_arrow (_, b, {ptyp_desc = Ptyp_arrow (_, c, {ptyp_desc = Ptyp_arrow (_, d, {ptyp_desc = Ptyp_arrow (_, e, f)})})})})} ->
    let a_t = conv_type true a in
    let a_t' = constr_of_type a_t in
      incrname ();
    let b_t = conv_type true b in
    let b_t' = constr_of_type b_t in
      incrname ();
    let c_t = conv_type true c in
    let c_t' = constr_of_type c_t in
      incrname ();
    let d_t = conv_type true d in
    let d_t' = constr_of_type d_t in
      incrname ();
    let e_t = conv_type true e in
    let e_t' = constr_of_type e_t in
    let f_t = conv_type false f in
      [(a_t, a_t'); (b_t, b_t'); (c_t, c_t'); (d_t, d_t'); (e_t, e_t')], f_t
| {ptyp_desc = Ptyp_arrow (_, a, {ptyp_desc = Ptyp_arrow (_, b, {ptyp_desc = Ptyp_arrow (_, c, {ptyp_desc = Ptyp_arrow (_, d, e)})})})} ->
    let a_t = conv_type true a in
    let a_t' = constr_of_type a_t in
      incrname ();
    let b_t = conv_type true b in
    let b_t' = constr_of_type b_t in
      incrname ();
    let c_t = conv_type true c in
    let c_t' = constr_of_type c_t in
      incrname ();
    let d_t = conv_type true d in
    let d_t' = constr_of_type d_t in
    let e_t = conv_type false e in
      [(a_t, a_t'); (b_t, b_t'); (c_t, c_t'); (d_t, d_t')], e_t
| {ptyp_desc = Ptyp_arrow (_, a, {ptyp_desc = Ptyp_arrow (_, b, {ptyp_desc = Ptyp_arrow (_, c, d)})})} ->
    let a_t = conv_type true a in
    let a_t' = constr_of_type a_t in
      incrname ();
    let b_t = conv_type true b in
    let b_t' = constr_of_type b_t in
      incrname ();
    let c_t = conv_type true c in
    let c_t' = constr_of_type c_t in
    let d_t = conv_type false d in
      [(a_t, a_t'); (b_t, b_t'); (c_t, c_t')], d_t
| {ptyp_desc = Ptyp_arrow (_, a, {ptyp_desc = Ptyp_arrow (_, b, c)})} ->
    let a_t = conv_type true a in
    let a_t' = constr_of_type a_t in
      incrname ();
    let b_t = conv_type true b in
    let b_t' = constr_of_type b_t in
    let c_t = conv_type false c in
      [(a_t, a_t'); (b_t, b_t')], c_t
| {ptyp_desc = Ptyp_arrow (_, a, b)} ->
    let a_t = conv_type true a in
    let b_t = conv_type false b in
      [(a_t, constr_of_type a_t)], b_t
| _ -> failwith "parse_type: unknown type"

let parse_type t =
  name := 'a';
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
         {|("%s", Fun (NoLabel, PatVar "*a", CallBuiltIn (None, "%s", [Var "*a"], f), []))|}
         nstr nstr
  | 2 ->
      Printf.sprintf
        {|("%s", Fun (NoLabel, PatVar "*a", Fun (NoLabel, PatVar "*b", CallBuiltIn (None, "%s", [Var "*a"; Var "*b"], f), []), []))|}
        nstr nstr
  | 3 ->
      Printf.sprintf
        {|("%s", Fun (NoLabel, PatVar "*a", Fun (NoLabel, PatVar "*b", Fun (NoLabel, PatVar "*c", CallBuiltIn (None, "%s", [Var "*a"; Var "*b"; Var "*c"], f), []), []), []))|}
        nstr nstr
  | 4 ->
      Printf.sprintf
        {|("%s", Fun (NoLabel, PatVar "*a", Fun (NoLabel, PatVar "*b", Fun (NoLabel, PatVar "*c", Fun (NoLabel, PatVar "*d", CallBuiltIn (None, "%s", [Var "*a"; Var "*b"; Var "*c"; Var "*d"], f), []), []), []), []))|}
        nstr nstr
  | 5 ->
      Printf.sprintf
        {|("%s", Fun (NoLabel, PatVar "*a", Fun (NoLabel, PatVar "*b", Fun (NoLabel, PatVar "*c", Fun (NoLabel, PatVar "*d", Fun (NoLabel, PatVar "*e", CallBuiltIn (None, "%s", [Var "a"; Var "b"; Var "*c"; Var "*d"; Var "*e"], f), []), []), []), []), []))|}
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
    | [(t_in, t_in'); (t_in2, t_in2'); (t_in3, t_in3')] ->
        let out =
          if t_out = "Unit"
            then Printf.sprintf "%s %s %s %s; Unit" n t_in' t_in2' t_in3'
            else Printf.sprintf "%s (%s %s %s %s)" t_out n t_in' t_in2' t_in3'
        in
          getexpr
            (ast
              (Printf.sprintf
                 {|function [%s; %s; %s] -> %s | _ -> failwith "%s"|}
                 t_in t_in2 t_in3 out nstr))
    | [(t_in, t_in'); (t_in2, t_in2'); (t_in3, t_in3'); (t_in4, t_in4')] ->
        let out =
          if t_out = "Unit"
            then Printf.sprintf "%s %s %s %s %s; Unit" n t_in' t_in2' t_in3' t_in4'
            else Printf.sprintf "%s (%s %s %s %s %s)" t_out n t_in' t_in2' t_in3' t_in4'
        in
          getexpr
            (ast
              (Printf.sprintf
                 {|function [%s; %s; %s; %s] -> %s | _ -> failwith "%s"|}
                 t_in t_in2 t_in3 t_in4 out nstr))
    | [(t_in, t_in'); (t_in2, t_in2'); (t_in3, t_in3'); (t_in4, t_in4'); (t_in5, t_in5')] ->
        let out =
          if t_out = "Unit"
            then Printf.sprintf "%s %s %s %s %s %s; Unit" n t_in' t_in2' t_in3' t_in4' t_in5'
            else Printf.sprintf "%s (%s %s %s %s %s %s)" t_out n t_in' t_in2' t_in3' t_in4' t_in5'
        in
          getexpr
            (ast
              (Printf.sprintf
                 {|function [%s; %s; %s; %s; %s] -> %s | _ -> failwith "%s"|}
                 t_in t_in2 t_in3 t_in4 t_in5 out nstr))
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

