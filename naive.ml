open Parsetree
open Asttypes
open Evalutils

type t = Parsetree.expression

let init = getexpr

let rec is_value e =
  match e.pexp_desc with
    Pexp_constant (Pconst_integer _)
  | Pexp_construct ({txt = Longident.Lident ("true" | "false")}, None)
  | Pexp_fun _
  | Pexp_function _ -> true
  | _ -> false

let appears_in_value_bindings var value_bindings =
  List.exists
    (function
      {pvb_pat = {ppat_desc = Ppat_var {txt}}} when txt = var -> true
      | _ -> false)
    value_bindings

let rec substitute_expression_desc var value = function
  | Pexp_ident {txt} when txt = Longident.Lident var ->
      value.pexp_desc
  | Pexp_apply (expr, args) ->
      Pexp_apply
        (substitute_expression var value expr,
         List.map
           (function (arg_label, expr) ->
             (arg_label, substitute_expression var value expr))
           args)
  | Pexp_let (rec_flag, value_bindings, e) ->
      let e' =
        if appears_in_value_bindings var value_bindings
          then e
          else substitute_expression var value e
      in
        let value_bindings' =
          (* If recursive, name may shadow *)
          List.map
            (fun binding ->
               if rec_flag = Recursive && appears_in_value_bindings var [binding]
                 then binding
                 else substitute_value_binding var value binding)
            value_bindings
        in
          Pexp_let (rec_flag, value_bindings', e')
  | Pexp_fun (arg_label, label_expr, ({ppat_desc = Ppat_var {txt}} as pat), expr) ->
      if var = txt then
        Pexp_fun (arg_label, label_expr, pat, expr)
      else
        Pexp_fun
          (arg_label, label_expr, pat, substitute_expression var value expr)
  | Pexp_ifthenelse (e1, e2, Some e3) ->
      let r = substitute_expression var value in
        Pexp_ifthenelse (r e1, r e2, Some (r e3))
  | x -> x

and substitute_value_binding var value value_binding =
  {value_binding with
     pvb_expr = substitute_expression var value value_binding.pvb_expr}

and substitute_expression var value exp =
  {exp with
    pexp_desc = substitute_expression_desc var value exp.pexp_desc}

let int_op_of_string = function
  | "*" -> ( * ) | "+" -> ( + ) | "-" -> ( - ) | "/" -> ( / )
  | _ -> malformed __LOC__

let comparison a b op =
  let comparison_of_string = function
    | "=" -> ( = ) | "<>" -> ( <> ) | "<" -> ( < )
    | ">" -> ( > ) | "<=" -> ( <= ) | ">=" -> ( >= )
    | _ -> malformed __LOC__
  in
    match a.pexp_desc, b.pexp_desc with
      Pexp_constant (Pconst_integer (ai, _)), Pexp_constant (Pconst_integer (bi, _)) ->
        (comparison_of_string op) (int_of_string ai) (int_of_string bi)
    | _ -> malformed __LOC__

(* Evaluate an abstract syntax tree by one step *)
let rec eval_expression e = 
  if is_value e then e else
    begin match e.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident "&&"}} as op, args) ->
        begin match args with
        | [(l1, a1); (l2, a2)] ->
            if is_value a1 then 
              if bool_of_bool_value a1 then a2 else mkbool false
            else
              {e with pexp_desc = Pexp_apply (op, evaluate_first_args args)}
        | _ -> malformed __LOC__
        end
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident "||"}} as op, args) ->
        begin match args with
        | [(l1, a1); (l2, a2)] ->
            if is_value a1 then
              if bool_of_bool_value a1 then mkbool true else a2
            else
              {e with pexp_desc = Pexp_apply (op, evaluate_first_args args)}
        | _ -> malformed __LOC__
        end
    | Pexp_apply (expr, args) ->
        if List.for_all (fun (_, arg) -> is_value arg) args then
          begin match expr.pexp_desc with
            Pexp_ident
              {txt =
                Longident.Lident (("=" | "<>" | "<" | ">" | "<=" | ">=") as op)} ->
                begin match args with
                  [(_, a); (_, b)] when is_value a && is_value b ->
                     mkbool (comparison a b op)
                | _ -> malformed __LOC__
                end
          | Pexp_ident {txt = Longident.Lident (("*" | "/" | "+" | "-") as op)} ->
              begin match args with
                [(_, {pexp_desc = Pexp_constant (Pconst_integer (a, _))});
                 (_, {pexp_desc = Pexp_constant (Pconst_integer (b, _))})] ->
                  let result = (int_op_of_string op) (int_of_string a) (int_of_string b) in
                    {e with pexp_desc = Pexp_constant (Pconst_integer (string_of_int result, None))}
              | _ -> malformed __LOC__
              end
          | Pexp_fun (arg_label, opt, {ppat_desc = Ppat_var {txt}}, body) ->
              begin match args with
              | [] -> malformed __LOC__
              | (label, toapply)::more ->
                  if more = [] then
                    substitute_expression txt toapply body
                  else
                    {e with
                       pexp_desc =
                         Pexp_apply
                           (substitute_expression txt toapply body, more)}
              end
          | _ ->
              (* If it's not something we can apply, evaluate it one step. *)
              {e with pexp_desc = Pexp_apply (eval_expression expr, args)}
          end
        else
          (* Work on the arguments *)
          {e with pexp_desc = Pexp_apply (expr, evaluate_first_args args)}
    | Pexp_ifthenelse (e1, e2, Some e3) ->
        (* If e1 not a value already, evaluate it one step and return.
        Otherwise, pick e2 or e3, and replace the ifthenelse with it *)
        if is_value e1 then
          if bool_of_bool_value e1 then e2 else e3
        else
          {e with pexp_desc = Pexp_ifthenelse (eval_expression e1, e2, Some e3)}
    | Pexp_let (rec_flag, value_bindings, let_exp) ->
        begin match value_bindings with
        | [{pvb_expr} as binding] ->
          if is_value pvb_expr then
            begin match binding.pvb_pat.ppat_desc with
            | Ppat_var {txt} ->
                substitute_expression
                  txt
                  (if rec_flag = Recursive
                     then
                       {e with pexp_desc =
                         Pexp_let (rec_flag, value_bindings, binding.pvb_expr)}
                     else
                       binding.pvb_expr)
                  let_exp
            | _ -> unimp __LOC__
            end
          else
            {e with
              pexp_desc =
                Pexp_let
                  (rec_flag,
                   [{binding with pvb_expr = eval_expression pvb_expr}],
                   let_exp)}
        | _ -> unimp __LOC__
        end
    | x -> unimp __LOC__
    end

and evaluate_first_args args =
  let rec evaluate_first = function
  | [] -> []
  | h::t ->
    if is_value h
      then h::evaluate_first t
      else eval_expression h::t
  in
    let l, a = List.split args in
      List.combine l (evaluate_first a)

let next ast =
  if is_value ast then IsValue else
    try Next (eval_expression ast) with
      ExnUnimplemented s -> Unimplemented s
    | ExnMalformed s -> Malformed s

let tree ast = makestructure ast

let tiny ast = Tinyocaml.of_real_ocaml (makestructure ast)

let to_string = Evalutils.to_string

let last () = Unknown
let peek _ = Unknown

